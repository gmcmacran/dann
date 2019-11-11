#' Discriminant Adaptive Nearest Neighbor Classification
#'
#' @param xTrain Train features. Something easily converted to a numeric matrix.
#' @param yTrain Train classes. Something easily converted to a numeric matrix.
#' @param xTest Test features. Something easily converted to a numeric matrix.
#' @param k The number of data points used for final classificastion.
#' @param neighborhood_size The number of data points used to calcualate between and within class covariance.
#' @param epsilon Diaginal elemnts of a diagional matrix. 1 is the identity matirx.
#' @return  A numeric matrix containing class predictions.
#' @details
#' This is an implementation of Hastie and Tibshirani's
#' \href{https://web.stanford.edu/~hastie/Papers/dann_IEEE.pdf}{Discriminant Adaptive Nearest
#' Neighbor Classificastion publication.}.
#' The code is a port of Christopher Jenness's
#' python \href{https://github.com/christopherjenness/ML-lib}{implementation.}
#' @export
dann <- function(xTrain, yTrain, xTest, k = 5, neighborhood_size = min(floor(nrow(xTrain) / 5), 50), epsilon = 1) {
  ###################################
  # Input checking
  ###################################
  compiler::enableJIT(3)

  # Convert to matrices
  if (!is.matrix(xTrain)) {
    xTrain <- as.matrix(xTrain)
  }
  if (!is.matrix(yTrain)) {
    yTrain <- as.matrix(yTrain)
  }
  if (!is.matrix(xTest)) {
    yTrain <- as.matrix(yTrain)
  }

  # Confirm numeric
  if (!is.numeric(xTrain)) {
    stop("Argument xTrain should be numeric.")
  }
  if (!is.numeric(yTrain)) {
    stop("Argument yTrain should be numeric.")
  }
  if (!is.numeric(xTest)) {
    stop("Argument xTest should be numeric.")
  }

  # Missing values.
  if (any(is.na(xTrain))) {
    stop("Argument xTrain should not have any NA values.")
  }
  if (any(is.na(yTrain))) {
    stop("Argument yTrain should not have any NA values.")
  }
  if (any(is.na(xTest))) {
    stop("Argument xTest should not have any NA values.")
  }

  # Confirm structure looks right
  if (ncol(xTrain) != ncol(xTest)) {
    stop("Argument xTrain and xTest should have the same number of columns.")
  }
  if (nrow(xTrain) != nrow(yTrain)) {
    stop("Argument xTrain and yTrain should have the same number of rows.")
  }
  if (ncol(xTrain) < 1) {
    stop("Argument xTrain should have at least one column.")
  }
  if (ncol(yTrain) != 1) {
    stop("Argument yTrain should only have one column.")
  }
  if (ncol(xTest) < 1) {
    stop("Argument xTest should have at least one column.")
  }

  # k is valid
  if (length(k) != 1) {
    stop("Argument k be at length 1 vector.")
  }
  if (!is.numeric(k)) {
    stop("Argument k should be numeric.")
  }
  if (k > nrow(xTrain)) {
    stop("Argument k should be less than or equal to the numer of rows in xTrain.")
  }
  if (k <= 0) {
    stop("Argument k should be at least 1.")
  }

  # neighborhood_size is valid
  if (length(neighborhood_size) != 1) {
    stop("Argument neighborhood_size be at length 1 vector.")
  }
  if (!is.numeric(neighborhood_size)) {
    stop("Argument neighborhood_size should be numeric.")
  }
  if (neighborhood_size > nrow(xTrain)) {
    stop("Argument neighborhood_size should be less than or equal to the numer of rows in xTrain.")
  }
  if (neighborhood_size <= 1) {
    stop("Argument neighborhood_size should be at least 2.")
  }

  # epsilon is valid
  if (length(epsilon) != 1) {
    stop("Argument epsilon be at length 1 vector.")
  }
  if (!is.numeric(epsilon)) {
    stop("Argument epsilon should be numeric.")
  }
  if (epsilon < 0) {
    stop("Argument epsilon should be at 0.")
  }

  S <- diag(ncol(xTrain))

  ###################################
  # Calculate predictions
  ###################################
  predictions <- vector(mode = "numeric", length = nrow(xTest))
  for (i in seq_along(1:nrow(xTest))) {

    ###########
    # Find neighborhood for x[i,]
    ###########
    distances <- vector(mode = "numeric", length = nrow(xTrain))
    for (j in seq_along(1:nrow(xTrain))) {
      # distances[j] <- sum( (xTest[i, ]-xTrain[j,])^2) ^ .5
      distances[j] <- sum((xTrain[j, ] - xTest[i, ])^2)^.5
    }

    nearest_neighbors <- order(distances)[1:neighborhood_size]
    neighborhood_xTrain <- xTrain[nearest_neighbors, 1:ncol(xTrain), drop = FALSE]
    neighborhood_X_mean <- colMeans(neighborhood_xTrain)
    neighborhood_y <- yTrain[nearest_neighbors, 1, drop = FALSE]
    neighborhood_classes <- unique(neighborhood_y)

    ###########
    # Between and within matrices
    ###########
    class_frequencies <- vector(mode = "numeric", length = length(neighborhood_classes))
    within_class_cov <- matrix(0, nrow = ncol(xTrain), ncol = ncol(xTrain))
    between_class_cov <- matrix(0, nrow = ncol(xTrain), ncol = ncol(xTrain))

    for (kth in seq_along(1:length(neighborhood_classes))) {
      target_class <- neighborhood_classes[kth]
      class_indices <- which(neighborhood_y == target_class)
      class_frequencies[target_class] <- sum(neighborhood_y == target_class) / neighborhood_size

      class_covariance <- stats::var(neighborhood_xTrain[class_indices, 1:ncol(neighborhood_xTrain), drop = FALSE])
      # Deal with 1 row in class edge case
      if (all(is.na(class_covariance))) {
        class_covariance <- matrix(0, nrow = nrow(class_covariance), ncol = ncol(class_covariance))
      }

      within_class_cov <- class_covariance * class_frequencies[target_class] + within_class_cov
      class_mean <- colMeans(neighborhood_xTrain[class_indices, 1:ncol(neighborhood_xTrain), drop = FALSE])
      between_class_cov <- outer(class_mean - neighborhood_X_mean, class_mean - neighborhood_X_mean) *
        class_frequencies[target_class] + between_class_cov
    }

    # W* = W^-.5
    # B* = W*BW*
    W_star <- within_class_cov^.5
    # Deal with NA case
    for (kth in seq_along(1:ncol(W_star))) {
      W_star[which(is.na(W_star[, kth])), kth] <- 0
    }
    W_star <- MASS::ginv(W_star)
    B_star <- W_star %*% between_class_cov %*% W_star
    I <- diag(ncol(xTrain))

    sigma <- W_star %*% (B_star + epsilon * I) %*% W_star

    ###########
    # DANN distance using sigma
    ###########
    distances <- vector(mode = "numeric", length = nrow(xTrain))
    for (kth in seq_along(1:length(distances)))
      distances[kth] <- DANN_distance(xTest[i, 1:ncol(xTest), drop = FALSE], xTrain[kth, 1:ncol(xTrain), drop = FALSE ], sigma)
    nearest <- order(distances, length(distances):1)[1:k]
    predictions[i] <- MODE(yTrain[nearest])
  }

  predictions <- matrix(predictions, nrow = length(predictions), ncol = 1)
  return(predictions)
}

###################################
# Helper functions
###################################
#' Computes the distance between x0 and x1 using the DANN metric
#'
#' @param x1 A numeric matrix with training predictors as columns.
#' @param x2 A numeric matrix with training predictors as columns.
#' @param sigma A numeric matrix defined in Hastie's DANN publication.
#' @keywords internal
DANN_distance <- function(x0, x1, sigma) {
  difference <- x0 - x1
  distance <- difference %*% sigma %*% t(difference)
  return(distance)
}

#' Computes mode.
#' Code found at \href{https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode}{Stack Overflow}
#'
#' @param x A numeric vector.
#' @param na.rm Should na be removed?
#' @keywords internal
MODE <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  ux <- sort(unique(x))
  return(ux[which.max(tabulate(match(x, ux)))])
}
