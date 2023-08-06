#################
# constructor
#################
#' @keywords internal
new_dann <- function(X, Y, k, neighborhood_size, epsilon, levels, blueprint) {
  # X is valid.
  if (!is.numeric(X)) {
    stop("`X` should be a numeric matrix.", call. = FALSE)
  }

  if (!is.matrix(X)) {
    stop("`X` should be a numeric matrix.", call. = FALSE)
  }

  if (!ncol(X) >= 1) {
    stop("`X` should have at least one column.", call. = FALSE)
  }

  if (!nrow(X) >= 1) {
    stop("`X` should have at least one row.", call. = FALSE)
  }

  if (anyNA(X)) {
    stop("`X` should not contain NA.", call. = FALSE)
  }

  # Y is valid.
  if (!is.numeric(Y)) {
    stop("`Y` should be a numeric vector.", call. = FALSE)
  }

  if (!is.vector(Y)) {
    stop("`Y` should be a numeric vector.", call. = FALSE)
  }

  if (!length(Y) >= 1) {
    stop("`Y` should have positive length.", call. = FALSE)
  }

  if (anyNA(Y)) {
    stop("`Y` should not contain NA.", call. = FALSE)
  }

  if (nrow(X) != length(Y)) {
    stop("`Y` should have the same length as nrow('X').", call. = FALSE)
  }

  # levels is valid
  if (!length(levels) > 1) {
    stop("'Y should contain at least two classes.", call. = FALSE)
  }

  # k is valid.
  if (length(k) != 1) {
    stop("'k' should be at length 1 vector.", call. = FALSE)
  }
  if (!is.numeric(k)) {
    stop("'k' should be numeric.", call. = FALSE)
  }
  if (k > nrow(X)) {
    stop("'k' should be less than or equal to the numer of rows in 'X'", call. = FALSE)
  }
  if (k <= 0) {
    stop("'k' should be at least 1.", call. = FALSE)
  }
  if (k != round(k)) {
    stop("'k' should a positive whole number.", call. = FALSE)
  }

  # neighborhood_size is valid
  if (length(neighborhood_size) != 1) {
    stop("'neighborhood_size' should be at length 1 vector.", call. = FALSE)
  }
  if (!is.numeric(neighborhood_size)) {
    stop("'neighborhood_size' should be numeric.", call. = FALSE)
  }
  if (neighborhood_size > nrow(X)) {
    stop("'neighborhood_size' should be less than or equal to the numer of rows in 'X'.", call. = FALSE)
  }
  if (neighborhood_size <= 1) {
    stop("'neighborhood_size' should be at least 2.", call. = FALSE)
  }
  if (k > neighborhood_size) {
    stop("'k' should be less than 'neighborhood_size'.", call. = FALSE)
  }
  if (neighborhood_size != round(neighborhood_size)) {
    stop("'neighborhood_size' should a positive whole number.", call. = FALSE)
  }

  # epsilon is valid
  if (length(epsilon) != 1) {
    stop("'epsilon' be at length 1 vector.", call. = FALSE)
  }
  if (!is.numeric(epsilon)) {
    stop("'epsilon' should be numeric.", call. = FALSE)
  }
  if (!epsilon >= 0) {
    stop("'epsilon' should be at least 0.", call. = FALSE)
  }

  # epsilon is valid
  if (length(epsilon) != 1) {
    stop("'epsilon' be at length 1 vector.", call. = FALSE)
  }
  if (!is.numeric(epsilon)) {
    stop("'epsilon' should be numeric.", call. = FALSE)
  }
  if (!epsilon >= 0) {
    stop("'epsilon' should be at least 0.", call. = FALSE)
  }

  hardhat::new_model(
    X = X,
    Y = Y,
    k = k,
    neighborhood_size = neighborhood_size,
    epsilon = epsilon,
    levels = levels,
    blueprint = blueprint,
    class = "dann"
  )
}

#################
# training function
#################
#' @keywords internal
dann_impl <- function(predictors, outcomes, k, neighborhood_size, epsilon, levels) {
  list(
    X = predictors,
    Y = outcomes,
    k = k,
    neighborhood_size = neighborhood_size,
    epsilon = epsilon,
    levels = levels
  )
}

#################
# bridge
#################
#' @keywords internal
dann_bridge <- function(processed, k, neighborhood_size, epsilon) {
  predictors <- processed$predictors
  predictors <- as.matrix(predictors)
  hardhat::validate_predictors_are_numeric(predictors)

  outcomes <- processed$outcomes[[1]]
  hardhat::validate_outcomes_are_univariate(outcomes)
  if (!is.factor(outcomes)) {
    outcomes <- factor(outcomes)
  }
  levels <- levels(outcomes)
  outcomes <- as.numeric(as.vector(outcomes))


  fit <- dann_impl(predictors, outcomes, k, neighborhood_size, epsilon, levels)

  new_dann(
    X = fit$X,
    Y = fit$Y,
    k = fit$k,
    neighborhood_size = fit$neighborhood_size,
    epsilon = fit$epsilon,
    levels = fit$levels,
    blueprint = processed$blueprint
  )
}

#################
# User interface
#################
#' @title Discriminant Adaptive Nearest Neighbor Classification
#' @param k The number of data points used for final classification.
#' @param neighborhood_size The number of data points used to calculate between and within class covariance.
#' @param epsilon Diagonal elements of a diagonal matrix. 1 is the identity matrix.
#' @return  An S3 class of type dann.
#' @details
#' This is an implementation of Hastie and Tibshirani's
#' [Discriminant Adaptive Nearest
#' Neighbor Classification publication.](https://web.stanford.edu/~hastie/Papers/dann_IEEE.pdf).
#' @export
dann <- function(x, k = 5, neighborhood_size = max(floor(nrow(x) / 5), 50), epsilon = 1, ...) {
  UseMethod("dann")
}

# Default
#' @inherit dann title
#' @inheritParams dann
#' @param x A data frame.
#' @param y A vector.
#' @inherit dann return
#' @inherit dann details
#' @export
dann.default <- function(x, k = 5, neighborhood_size = max(floor(nrow(x) / 5), 50), epsilon = 1) {
  stop(
    "`dann()` is not defined for a '", class(x)[1], "'.",
    call. = FALSE
  )
}

# XY method - data frame
#' @inherit dann title
#' @inheritParams dann
#' @param x A data frame.
#' @param y A vector.
#' @inherit dann return
#' @inherit dann details
#' @examples
#' library(dann)
#' library(mlbench)
#' library(magrittr)
#' library(dplyr)
#'
#' set.seed(1)
#' train <- mlbench.circle(300, 2) %>%
#'   tibble::as_tibble()
#' colnames(train) <- c("X1", "X2", "Y")
#' y <- train$Y
#' x <- train[, 1:2]
#'
#' dann(x, y)
#' @export
dann.data.frame <- function(x, y, k = 5, neighborhood_size = max(floor(nrow(x) / 5), 50), epsilon = 1) {
  processed <- hardhat::mold(x, y)
  dann_bridge(processed, k, neighborhood_size, epsilon)
}

# XY method - matrix
#' @inherit dann title
#' @inheritParams dann
#' @param x A matrix.
#' @param y A vector.
#' @inherit dann return
#' @inherit dann details
#' @examples
#' library(dann)
#' library(mlbench)
#' library(magrittr)
#' library(dplyr)
#'
#' set.seed(1)
#' train <- mlbench.circle(300, 2) %>%
#'   tibble::as_tibble()
#' colnames(train) <- c("X1", "X2", "Y")
#' y <- train$Y
#' x <- cbind(train$X1, train$X2)
#'
#' dann(x, y)
#' @export
dann.matrix <- function(x, y, k = 5, neighborhood_size = max(floor(nrow(x) / 5), 50), epsilon = 1) {
  processed <- hardhat::mold(x, y)
  dann_bridge(processed, k, neighborhood_size, epsilon)
}

# Formula method
#' @inherit dann title
#' @inheritParams dann
#' @param formula A formula. Y ~ X1 + X1
#' @param data A data frame.
#' @inherit dann return
#' @inherit dann details
#' @examples
#' library(dann)
#' library(mlbench)
#' library(magrittr)
#' library(dplyr)
#'
#' set.seed(1)
#' train <- mlbench.circle(300, 2) %>%
#'   tibble::as_tibble()
#' colnames(train) <- c("X1", "X2", "Y")
#'
#' dann(Y ~ X1 + X2, train)
#' @export
dann.formula <- function(formula, data, k = 5, neighborhood_size = max(floor(nrow(data) / 5), 50), epsilon = 1) {
  hardhat::validate_no_formula_duplication(formula = formula, original = TRUE)
  processed <- hardhat::mold(formula, data)
  dann_bridge(processed, k, neighborhood_size, epsilon)
}

# Recipe method
#' @inherit dann title
#' @inheritParams dann
#' @param x A recipe from recipes library
#' @param data A data frame.
#' @inherit dann return
#' @inherit dann details
#' @examples
#' library(dann)
#' library(mlbench)
#' library(magrittr)
#' library(dplyr)
#' library(recipes)
#'
#' set.seed(1)
#' train <- mlbench.circle(300, 2) %>%
#'   tibble::as_tibble()
#' colnames(train) <- c("X1", "X2", "Y")
#'
#' rec_obj <- recipe(Y ~ X1 + X2, data = train)
#'
#' dann(rec_obj, train)
#' @export
dann.recipe <- function(x, data, k = 5, neighborhood_size = max(floor(nrow(data) / 5), 50), epsilon = 1) {
  processed <- hardhat::mold(x, data)
  dann_bridge(processed, k, neighborhood_size, epsilon)
}

#################
# prediction functions
#################
#' @keywords internal
dann_predict_base <- function(object, predictors, probability) {
  xTrain <- object$X
  yTrain <- object$Y
  k <- object$k
  neighborhood_size <- object$neighborhood_size
  epsilon <- object$epsilon

  xTest <- predictors

  ###################################
  # Shift classes if needed. Need min(yTrain) > 0
  ###################################
  if (min(yTrain) <= 0) {
    shiftedBy <- abs(min(yTrain)) + 1
    yTrain <- yTrain + shiftedBy
    shifted <- TRUE
  } else {
    shifted <- FALSE
  }


  ###################################
  # Calculate predictions
  ###################################

  if (!probability) {
    predictions <- rep(-1, nrow(xTest))
  } else {
    predictions <- matrix(0, nrow = nrow(xTest), ncol = length(unique(yTrain)))
    colnames(predictions) <- stringr::str_c("Class", as.character(sort(unique(yTrain))))
  }

  NCOLX <- ncol(xTrain)

  ###################################
  # Count number of rows per class
  ###################################
  # Used in dann distance sorting
  # If there is a tie in distance, break tie with most common class.
  Y_counts <- vector(mode = "numeric", length = length(unique(yTrain)))
  names(Y_counts) <- sort(unique(yTrain))
  for (i in seq_len(length(Y_counts))) {
    Y_counts[i] <- sum(yTrain == names(Y_counts)[i])
  }
  Y_counts <- sort(Y_counts, decreasing = TRUE)

  Y_class_presidence <- vector(mode = "numeric", length = length(yTrain))
  for (i in seq_len(length(Y_counts))) {
    Y_class_presidence[which(yTrain == names(Y_counts)[i])] <- i
  }

  for (i in seq_len(nrow(xTest))) {
    ###########
    # Find neighborhood for x[i,]
    ###########
    distances <- calc_distance_C(xTrain, xTest[i, ])

    nearest_neighbors <- order(distances, Y_class_presidence, yTrain)[1:neighborhood_size]
    neighborhood_xTrain <- xTrain[nearest_neighbors, 1:NCOLX, drop = FALSE]
    neighborhood_X_mean <- colMeans(neighborhood_xTrain)
    neighborhood_y <- yTrain[nearest_neighbors]
    neighborhood_classes <- unique(neighborhood_y)

    ###########
    # Between and within matrices
    ###########
    class_frequencies <- vector(mode = "numeric", length = length(neighborhood_classes))
    within_class_cov <- matrix(0, nrow = NCOLX, ncol = NCOLX)
    between_class_cov <- matrix(0, nrow = NCOLX, ncol = NCOLX)

    for (kth in seq_len(length(neighborhood_classes))) {
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
    W_star[which(is.na(W_star))] <- 0

    W_star <- MASS::ginv(W_star)
    B_star <- W_star %*% between_class_cov %*% W_star
    I <- diag(NCOLX)

    sigma <- W_star %*% (B_star + epsilon * I) %*% W_star

    ###########
    # DANN distance using sigma
    ###########
    distances <- vector(mode = "numeric", length = nrow(xTrain))
    for (kth in seq_len(length(distances))) {
      distances[kth] <- DANN_distance_C(xTest[i, 1:NCOLX, drop = FALSE], xTrain[kth, 1:NCOLX, drop = FALSE], sigma)
    }
    nearest <- order(distances, Y_class_presidence, yTrain)[1:k]
    if (!probability) {
      predictions[i] <- MODE(yTrain[nearest])
    } else {
      predictions[i, ] <- class_proportions(yTrain[nearest], sort(unique(yTrain)))
    }
  }

  ###################################
  # Shift classes back if needed.
  ###################################
  if (shifted && probability) {
    yTrain <- yTrain - shiftedBy
    colnames(predictions) <- stringr::str_c("Class", as.character(sort(unique(yTrain))))
  } else if (shifted && !probability) {
    predictions <- predictions - shiftedBy
  }

  return(predictions)
}

#' @keywords internal
dann_predict_class <- function(object, predictors) {
  obsLevels <- object$levels
  out <- dann_predict_base(object = object, predictors = predictors, probability = FALSE)
  out <- factor(x = out, levels = obsLevels)
  out <- hardhat::spruce_class(out)
  return(out)
}

#' @keywords internal
dann_predict_prop <- function(object, predictors) {
  obsLevels <- object$levels
  out <- dann_predict_base(object = object, predictors = predictors, probability = TRUE)
  out <- hardhat::spruce_prob(obsLevels, out)
  return(out)
}

#' @keywords internal
predict_dann_bridge <- function(type, object, predictors) {
  type <- rlang::arg_match(type, c("class", "prop"))

  predictors <- as.matrix(predictors)
  hardhat::validate_predictors_are_numeric(predictors)

  switch(type,
    class = dann_predict_class(object, predictors),
    prop = dann_predict_prop(object, predictors)
  )
}

#' @inherit dann title
#' @param object of class inheriting from "dann"
#' @param new_data A data frame.
#' @param type Type of prediction. (class, prob)
#' @return  A data frame containing either class or class probabilities. Adheres to tidy models standards.
#' @inherit dann details
#' @examples
#' library(dann)
#' library(mlbench)
#' library(magrittr)
#' library(dplyr)
#'
#' set.seed(1)
#' train <- mlbench.circle(300, 2) %>%
#'   tibble::as_tibble()
#' colnames(train) <- c("X1", "X2", "Y")
#'
#' test <- mlbench.circle(300, 2) %>%
#'   tibble::as_tibble()
#' colnames(test) <- c("X1", "X2", "Y")
#'
#' model <- dann(Y ~ X1 + X2, train)
#' predict(model, test, "class")
#'
#' predict(model, test, "prop")
#' @export
predict.dann <- function(object, new_data, type = "class") {
  processed <- hardhat::forge(new_data, object$blueprint)

  out <- predict_dann_bridge(type, object, processed$predictors)

  hardhat::validate_prediction_size(out, new_data)

  out
}
