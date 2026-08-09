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
    stop("'Y' should contain at least two classes.", call. = FALSE)
  }

  # k is valid.
  if (length(k) != 1) {
    stop("'k' should be a length 1 vector.", call. = FALSE)
  }
  if (!is.numeric(k)) {
    stop("'k' should be numeric.", call. = FALSE)
  }
  if (k > nrow(X)) {
    stop("'k' should be less than or equal to the number of rows in 'X'.", call. = FALSE)
  }
  if (k <= 0) {
    stop("'k' should be at least 1.", call. = FALSE)
  }
  if (k != round(k)) {
    stop("'k' should be a positive whole number.", call. = FALSE)
  }

  # neighborhood_size is valid
  if (length(neighborhood_size) != 1) {
    stop("'neighborhood_size' should be a length 1 vector.", call. = FALSE)
  }
  if (!is.numeric(neighborhood_size)) {
    stop("'neighborhood_size' should be numeric.", call. = FALSE)
  }
  if (neighborhood_size > nrow(X)) {
    stop("'neighborhood_size' should be less than or equal to the number of rows in 'X'.", call. = FALSE)
  }
  if (neighborhood_size <= 1) {
    stop("'neighborhood_size' should be at least 2.", call. = FALSE)
  }
  if (k > neighborhood_size) {
    stop("'k' should be less than or equal to 'neighborhood_size'.", call. = FALSE)
  }
  if (neighborhood_size != round(neighborhood_size)) {
    stop("'neighborhood_size' should be a positive whole number.", call. = FALSE)
  }

  # epsilon is valid
  if (length(epsilon) != 1) {
    stop("'epsilon' should be a length 1 vector.", call. = FALSE)
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
fix_dann_params <- function(k, neighborhood_size, epsilon, data) {
  # A valid neighborhood_size is at least 2 and at most nrow(data), so with
  # fewer than two rows there is nothing to clamp toward. Catch it here rather
  # than letting the constructor reject a corrected value further downstream.
  if (nrow(data) < 2) {
    stop("Training data should have at least two rows.", call. = FALSE)
  }

  if (k < 1) {
    k <- 1
    msg <- paste("k cannot be less than 1. Changing to ", k, ".", sep = "")
    message(msg)
  }
  if (k > nrow(data)) {
    k <- nrow(data)
    msg <- paste("k cannot be greater than nrow(data). Changing to ", k, ".", sep = "")
    message(msg)
  }
  if (k > neighborhood_size || neighborhood_size < 2) {
    neighborhood_size <- pmax(k, 2)
    msg <- paste("Changing neighborhood_size to ", neighborhood_size, ".", sep = "")
    message(msg)
  }
  if (neighborhood_size > nrow(data)) {
    neighborhood_size <- nrow(data)
    msg <- paste("neighborhood_size cannot be greater than nrow(data). Changing to ", neighborhood_size, ".", sep = "")
    message(msg)
  }
  if (epsilon < 0) {
    epsilon <- 0
    msg <- paste("epsilon cannot be less than zero. Changing to ", epsilon, ".", sep = "")
    message(msg)
  }
  betterParams <- list(k = k, neighborhood_size = neighborhood_size, epsilon = epsilon)
  return(betterParams)
}

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

  # Safely convert factor to  numeric
  temp <- rep(NA_real_, length(outcomes))
  for (i in seq(levels(outcomes))) {
    temp[outcomes == levels(outcomes)[i]] <- i
  }
  temp <- temp - 1
  outcomes <- temp

  betterParams <- fix_dann_params(k, neighborhood_size, epsilon, predictors)
  k <- betterParams$k
  neighborhood_size <- betterParams$neighborhood_size
  epsilon <- betterParams$epsilon
  rm(betterParams)

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
#' @param x A matrix, data frame, formula, or recipe.
#' @param ... Additional parameters passed to methods.
#' @param k The number of nearest neighbors used to classify a point. Identical to k in standard k nearest neighbors.
#' @param neighborhood_size The number of nearest neighbors used to estimate the between and within class covariance matrices that shape the neighborhood.
#' @param epsilon Softening parameter. Scales the identity matrix added to the between class covariance, which keeps the neighborhood from collapsing onto the class boundary. 1 matches the publication.
#' @return An S3 class of type dann.
#' @details
#' This is an implementation of Hastie and Tibshirani's
#' [Discriminant Adaptive Nearest Neighbor
#' Classification](https://web.stanford.edu/~hastie/Papers/dann_IEEE.pdf).
#' @export
dann <- function(x, ..., k = 5, neighborhood_size = max(floor(nrow(x) / 5), 50), epsilon = 1) {
  UseMethod("dann")
}

# Default
#' @inherit dann title
#' @inheritParams dann
#' @param x An object for which no `dann()` method exists.
#' @inherit dann return
#' @inherit dann details
#' @export
dann.default <- function(x, k = 5, neighborhood_size = max(floor(nrow(x) / 5), 50), epsilon = 1, ...) {
  stop(
    "`dann()` is not defined for a '", class(x)[1], "'.",
    call. = FALSE
  )
}

# XY method - data frame
#' @inherit dann title
#' @inheritParams dann
#' @param x A data frame.
#' @param y A vector of outcomes. Numeric, character, and factor are all accepted.
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
dann.data.frame <- function(x, y, k = 5, neighborhood_size = max(floor(nrow(x) / 5), 50), epsilon = 1, ...) {
  rlang::check_dots_empty()
  processed <- hardhat::mold(x, y)
  dann_bridge(processed, k, neighborhood_size, epsilon)
}

# XY method - matrix
#' @inherit dann title
#' @inheritParams dann
#' @param x A matrix.
#' @param y A vector of outcomes. Numeric, character, and factor are all accepted.
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
#' y <- as.numeric(train$Y)
#' x <- cbind(train$X1, train$X2)
#'
#' dann(x, y)
#' @export
dann.matrix <- function(x, y, k = 5, neighborhood_size = max(floor(nrow(x) / 5), 50), epsilon = 1, ...) {
  rlang::check_dots_empty()
  processed <- hardhat::mold(x, y)
  dann_bridge(processed, k, neighborhood_size, epsilon)
}

# Formula method
#' @inherit dann title
#' @inheritParams dann
#' @param formula A formula specifying the outcome and predictors. For example, Y ~ X1 + X2.
#' @param data A data frame containing the variables in `formula` or in the recipe.
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
dann.formula <- function(formula, data, k = 5, neighborhood_size = max(floor(nrow(data) / 5), 50), epsilon = 1, ...) {
  rlang::check_dots_empty()
  hardhat::validate_no_formula_duplication(formula = formula, original = TRUE)
  processed <- hardhat::mold(formula, data)
  dann_bridge(processed, k, neighborhood_size, epsilon)
}

# Recipe method
#' @inherit dann title
#' @inheritParams dann
#' @param x A recipe from the recipes package.
#' @param data A data frame containing the variables in `formula` or in the recipe.
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
dann.recipe <- function(x, data, k = 5, neighborhood_size = max(floor(nrow(data) / 5), 50), epsilon = 1, ...) {
  rlang::check_dots_empty()
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

  ###################################
  # Calculate predictions via C++ (with OpenMP parallelization)
  ###################################
  # One column per level, not per observed class. A level carrying no training
  # rows still needs a (zero) probability column, otherwise the result has
  # fewer columns than hardhat::spruce_prob expects.
  all_classes <- seq_along(object$levels) - 1
  if (shifted) {
    all_classes <- all_classes + shiftedBy
  }

  result <- dann_predict_all_C(
    xTrain = xTrain[, 1:NCOLX, drop = FALSE],
    yTrain = yTrain,
    xTest = xTest[, 1:NCOLX, drop = FALSE],
    k = k,
    neighborhood_size = neighborhood_size,
    epsilon = epsilon,
    y_class_precedence = Y_class_presidence,
    unique_classes = all_classes,
    probability = probability
  )

  if (!probability) {
    predictions <- as.vector(result$predictions)
  } else {
    predictions <- result$predictions
    colnames(predictions) <- stringr::str_c("Class", as.character(all_classes))
  }

  ###################################
  # Shift classes back if needed.
  ###################################
  if (shifted && probability) {
    colnames(predictions) <- stringr::str_c("Class", as.character(all_classes - shiftedBy))
  } else if (shifted && !probability) {
    predictions <- predictions - shiftedBy
  }

  return(predictions)
}

#' @keywords internal
dann_predict_class <- function(object, predictors) {
  obsLevels <- object$levels
  out <- dann_predict_base(object = object, predictors = predictors, probability = FALSE)
  temp <- rep(NA_character_, length(out))
  for (i in seq(obsLevels)) {
    temp[out == (i - 1)] <- obsLevels[i]
  }
  out <- factor(x = temp, levels = obsLevels)
  out <- hardhat::spruce_class(out)
  return(out)
}

#' @keywords internal
dann_predict_prob <- function(object, predictors) {
  obsLevels <- object$levels
  out <- dann_predict_base(object = object, predictors = predictors, probability = TRUE)
  out <- hardhat::spruce_prob(obsLevels, out)
  return(out)
}

#' @keywords internal
predict_dann_bridge <- function(type, object, predictors) {
  if (length(type) != 1) {
    stop("'type' should have length one.", call. = FALSE)
  }

  type <- rlang::arg_match(type, c("class", "prob"))

  predictors <- as.matrix(predictors)
  if (anyNA(predictors)) {
    stop("'new_data' must not contain missing values.")
  }
  hardhat::validate_predictors_are_numeric(predictors)

  switch(type,
    class = dann_predict_class(object, predictors),
    prob = dann_predict_prob(object, predictors)
  )
}

#' @inherit dann title
#' @param object A fitted model of class dann.
#' @param new_data A data frame of predictors to score.
#' @param type Type of prediction. One of "class" or "prob".
#' @param ... Not used.
#' @return A data frame of predicted classes or class probabilities. Adheres to tidymodels standards.
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
#' predict(model, test, "prob")
#' @importFrom stats predict
#' @export
predict.dann <- function(object, new_data, type = "class", ...) {
  rlang::check_dots_empty()

  processed <- hardhat::forge(new_data, object$blueprint)

  out <- predict_dann_bridge(type, object, processed$predictors)

  hardhat::validate_prediction_size(out, new_data)

  out
}
