#include "RcppArmadillo.h"
#ifdef _OPENMP
#include <omp.h>
#endif
#include <algorithm>
#include <vector>
#include <numeric>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// Helper: compute Euclidean distances from all training rows to one test point
static arma::vec euclidean_distances(const arma::mat & xTrain, const arma::rowvec & xTest) {
  int N = xTrain.n_rows;
  arma::vec dist(N);
  for (int i = 0; i < N; i++) {
    arma::rowvec diff = xTest - xTrain.row(i);
    dist(i) = std::sqrt(arma::dot(diff, diff));
  }
  return dist;
}

// Helper: 3-key argsort replicating R's order(distances, y_class_precedence, yTrain)
// Returns 0-based indices sorted in ascending order by (dist, precedence, class)
static std::vector<int> three_key_order(const arma::vec & distances,
                                        const arma::vec & y_class_precedence,
                                        const arma::vec & yTrain) {
  int N = distances.n_elem;
  std::vector<int> idx(N);
  std::iota(idx.begin(), idx.end(), 0);

  std::stable_sort(idx.begin(), idx.end(), [&](int a, int b) {
    if (distances(a) != distances(b)) return distances(a) < distances(b);
    if (y_class_precedence(a) != y_class_precedence(b)) return y_class_precedence(a) < y_class_precedence(b);
    return yTrain(a) < yTrain(b);
  });

  return idx;
}

// Helper: compute MODE (most frequent value; ties broken by smallest value)
static double compute_mode(const arma::vec & x) {
  // Sort unique values
  arma::vec sorted_unique = arma::sort(arma::unique(x));
  double best_val = sorted_unique(0);
  int best_count = 0;

  for (unsigned int i = 0; i < sorted_unique.n_elem; i++) {
    int count = 0;
    for (unsigned int j = 0; j < x.n_elem; j++) {
      if (x(j) == sorted_unique(i)) count++;
    }
    if (count > best_count) {
      best_count = count;
      best_val = sorted_unique(i);
    }
  }
  return best_val;
}

// Helper: compute class proportions for given values and all possible class values
static arma::rowvec compute_class_proportions(const arma::vec & x, const arma::vec & possible_values) {
  int n_classes = possible_values.n_elem;
  arma::rowvec props(n_classes);
  double total = x.n_elem;

  for (int c = 0; c < n_classes; c++) {
    int count = 0;
    for (unsigned int j = 0; j < x.n_elem; j++) {
      if (x(j) == possible_values(c)) count++;
    }
    props(c) = count / total;
  }
  return props;
}

//' @keywords internal
//' Full DANN prediction loop in C++ with OpenMP parallelization.
//' Replicates the behavior of the R dann_predict_base outer loop.
// [[Rcpp::export]]
Rcpp::List dann_predict_all_C(const arma::mat & xTrain,
                              const arma::vec & yTrain,
                              const arma::mat & xTest,
                              int k,
                              int neighborhood_size,
                              double epsilon,
                              const arma::vec & y_class_precedence,
                              const arma::vec & unique_classes,
                              bool probability) {

  int M = xTest.n_rows;    // number of test observations
  int N = xTrain.n_rows;   // number of training observations
  int P = xTrain.n_cols;   // number of features
  int n_classes = unique_classes.n_elem;

  // Pre-allocate output
  arma::vec pred_class;
  arma::mat pred_prob;
  if (!probability) {
    pred_class.set_size(M);
    pred_class.fill(-1);
  } else {
    pred_prob.set_size(M, n_classes);
    pred_prob.fill(0.0);
  }

  // Identity matrix (constant, shared across iterations)
  arma::mat I = arma::eye(P, P);

  // Set if any eigendecomposition fails. Checked after the parallel region;
  // throwing out of an OpenMP loop would call std::terminate.
  int decomp_failed = 0;

  #ifdef _OPENMP
  #pragma omp parallel for schedule(dynamic)
  #endif
  for (int i = 0; i < M; i++) {
    arma::rowvec xTest_i = xTest.row(i);

    // Step 1: Euclidean distances from test point to all training points
    arma::vec distances = euclidean_distances(xTrain, xTest_i);

    // Step 2: 3-key sort to find neighborhood
    std::vector<int> sorted_idx = three_key_order(distances, y_class_precedence, yTrain);

    // Extract neighborhood (top neighborhood_size indices)
    arma::mat neighborhood_xTrain(neighborhood_size, P);
    arma::vec neighborhood_y(neighborhood_size);
    for (int j = 0; j < neighborhood_size; j++) {
      neighborhood_xTrain.row(j) = xTrain.row(sorted_idx[j]);
      neighborhood_y(j) = yTrain(sorted_idx[j]);
    }

    arma::rowvec neighborhood_X_mean = arma::mean(neighborhood_xTrain, 0);

    // Find unique classes in the neighborhood
    arma::vec neighborhood_unique = arma::sort(arma::unique(neighborhood_y));

    // Step 3: Between and within class covariance matrices
    arma::mat within_class_cov(P, P, arma::fill::zeros);
    arma::mat between_class_cov(P, P, arma::fill::zeros);

    for (unsigned int c = 0; c < neighborhood_unique.n_elem; c++) {
      double target_class = neighborhood_unique(c);

      // Find indices of this class in neighborhood
      std::vector<int> class_idx;
      for (int j = 0; j < neighborhood_size; j++) {
        if (neighborhood_y(j) == target_class) {
          class_idx.push_back(j);
        }
      }

      double class_freq = (double)class_idx.size() / (double)neighborhood_size;

      // Extract class subset
      arma::mat class_data(class_idx.size(), P);
      for (unsigned int ci = 0; ci < class_idx.size(); ci++) {
        class_data.row(ci) = neighborhood_xTrain.row(class_idx[ci]);
      }

      arma::rowvec class_mean = arma::mean(class_data, 0);

      // Within class sum-of-squares, Hastie & Tibshirani (1996) eq. (8):
      // sum over classes of sum_{y_i = j} (x_i - xbar_j)(x_i - xbar_j)',
      // divided by the total weight. Observations are unweighted here, so
      // that total is neighborhood_size. Note there is no (n_j - 1) divisor,
      // so a class holding a single observation contributes zero on its own
      // and needs no special case.
      arma::mat centered = class_data;
      centered.each_row() -= class_mean;
      within_class_cov += (centered.t() * centered) / (double)neighborhood_size;

      // Between class sum-of-squares, eq. (8).
      arma::rowvec diff = class_mean - neighborhood_X_mean;
      between_class_cov += (diff.t() * diff) * class_freq;
    }

    // Step 4: Compute sigma. Hastie & Tibshirani (1996), equation (2), p. 608:
    //   sigma = W^(-1/2) [ W^(-1/2) B W^(-1/2) + epsilon I ] W^(-1/2)
    // W^(-1/2) is the symmetric matrix square root -- the transformation that
    // spheres the space with respect to W, so that W^(-1/2) W W^(-1/2) = I.
    // It is built from the eigendecomposition of the within-class covariance,
    // which is symmetric and positive semi-definite by construction.
    arma::mat W_sym = 0.5 * (within_class_cov + within_class_cov.t());

    arma::vec eigval;
    arma::mat eigvec;
    if (!arma::eig_sym(eigval, eigvec, W_sym)) {
      #ifdef _OPENMP
      #pragma omp atomic write
      #endif
      decomp_failed = 1;
      continue;
    }

    // W is PSD, so clamp any eigenvalue round-off has pushed below zero.
    double max_eigval = eigval.max();
    if (max_eigval < 0.0) {
      max_eigval = 0.0;
    }
    eigval = arma::clamp(eigval, 0.0, max_eigval);

    // Drop directions whose spread is numerically indistinguishable from zero
    // rather than inverting them. The relative sqrt(eps) cutoff is the one
    // MASS::ginv used here before this loop was ported to C++, and it is
    // applied on the eigenvalue scale: because sigma involves W^(-1/2) twice,
    // an eigenvalue at the cutoff is amplified by 1/eigval, so a looser rule
    // lets round-off directions dominate every DANN distance.
    double tol = std::sqrt(arma::datum::eps) * max_eigval;

    arma::vec inv_sv(P, arma::fill::zeros);
    for (int j = 0; j < P; j++) {
      if (eigval(j) > tol) {
        inv_sv(j) = 1.0 / std::sqrt(eigval(j));
      }
    }

    arma::mat W_star = eigvec * arma::diagmat(inv_sv) * eigvec.t();

    arma::mat B_star = W_star * between_class_cov * W_star;
    arma::mat sigma = W_star * (B_star + epsilon * I) * W_star;

    // Step 5: DANN distances using sigma
    arma::vec dann_distances(N);
    for (int j = 0; j < N; j++) {
      arma::rowvec diff = xTest_i - xTrain.row(j);
      arma::vec d = diff * sigma * diff.t();
      dann_distances(j) = d(0);
    }

    // Step 6: 3-key sort on DANN distances, take top k
    std::vector<int> dann_sorted = three_key_order(dann_distances, y_class_precedence, yTrain);

    arma::vec nearest_y(k);
    for (int j = 0; j < k; j++) {
      nearest_y(j) = yTrain(dann_sorted[j]);
    }

    // Step 7: Predict
    if (!probability) {
      pred_class(i) = compute_mode(nearest_y);
    } else {
      pred_prob.row(i) = compute_class_proportions(nearest_y, unique_classes);
    }
  }

  if (decomp_failed) {
    Rcpp::stop("Eigendecomposition of the within-class covariance matrix failed.");
  }

  // Return results as a list
  if (!probability) {
    return Rcpp::List::create(
      Rcpp::Named("type") = "class",
      Rcpp::Named("predictions") = Rcpp::wrap(pred_class)
    );
  } else {
    return Rcpp::List::create(
      Rcpp::Named("type") = "prob",
      Rcpp::Named("predictions") = Rcpp::wrap(pred_prob)
    );
  }
}
