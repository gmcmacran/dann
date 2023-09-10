#################
# constructor
#################
#' @keywords internal
new_sub_dann <- function(X, Y, k, neighborhood_size, epsilon, weighted, sphere, numDim, levels, subspace, blueprint) {
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

  # weighted is valid
  if (length(weighted) != 1) {
    stop("'weighted' should be a length 1 vector.", call. = FALSE)
  }
  if (!is.logical(weighted)) {
    stop("'weighted' should be logical.", call. = FALSE)
  }

  # sphere is valid
  if (length(sphere) != 1) {
    stop("'sphere' should be a length 1 vector.", call. = FALSE)
  }
  if (!is.character(sphere)) {
    stop("'sphere' should be a character vector.", call. = FALSE)
  }
  if (!(sphere %in% c("mve", "mcd", "classical", "none"))) {
    stop("'sphere' should be a one mve, mcd, classical or none.", call. = FALSE)
  }

  # numDim is valid
  if (length(numDim) != 1) {
    stop("'numDim' should be a length 1 vector.", call. = FALSE)
  }
  if (!is.numeric(numDim)) {
    stop("'numDim' should be numeric.", call. = FALSE)
  }
  if (numDim < 1) {
    stop("'numDim' should be at least 1.", call. = FALSE)
  }

  hardhat::new_model(
    X = X,
    Y = Y,
    k = k,
    neighborhood_size = neighborhood_size,
    epsilon = epsilon,
    weighted = weighted,
    sphere = sphere,
    numDim = numDim,
    levels = levels,
    subspace = subspace,
    blueprint = blueprint,
    class = "sub_dann"
  )
}

#################
# training function
#################
#' @keywords internal
sub_dann_impl <- function(predictors, outcomes, k, neighborhood_size, epsilon, weighted, sphere, numDim, levels) {
  # Find subspace
  subspace <- fpc::ncoord(
    xd = predictors, clvecd = outcomes,
    nn = neighborhood_size, weighted = weighted,
    sphere = sphere, countmode = 999999999999999
  )

  list(
    X = predictors,
    Y = outcomes,
    k = k,
    neighborhood_size = neighborhood_size,
    epsilon = epsilon,
    weighted = weighted,
    sphere = sphere,
    numDim = numDim,
    levels = levels,
    subspace = subspace
  )
}

#################
# bridge
#################
#' @keywords internal
fix_sub_dann_params <- function(k, neighborhood_size, epsilon, numDim, data) {
  betterParams <- fix_dann_params(k, neighborhood_size, epsilon, data)
  k <- betterParams$k
  neighborhood_size <- betterParams$neighborhood_size
  epsilon <- betterParams$epsilon
  rm(betterParams)

  if (numDim < 1) {
    numDim <- 1
    msg <- paste("numDim cannot be less than one. Changing to ", numDim, ".", sep = "")
    message(msg)
  }

  # be sure data does not contain Y
  if (numDim > ncol(data)) {
    numDim <- ncol(data)
    msg <- paste("numDim cannot be greater than the number of predictors. Changing to ", numDim, ".", sep = "")
    message(msg)
  }

  betterParams <- list(k = k, neighborhood_size = neighborhood_size, epsilon = epsilon, numDim = numDim)
  return(betterParams)
}

#' @keywords internal
sub_dann_bridge <- function(processed, k, neighborhood_size, epsilon, weighted, sphere, numDim) {
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

  betterParams <- fix_sub_dann_params(k, neighborhood_size, epsilon, numDim, predictors)
  k <- betterParams$k
  neighborhood_size <- betterParams$neighborhood_size
  epsilon <- betterParams$epsilon
  numDim <- betterParams$numDim
  rm(betterParams)

  fit <- sub_dann_impl(predictors, outcomes, k, neighborhood_size, epsilon, weighted, sphere, numDim, levels)

  new_sub_dann(
    X = fit$X,
    Y = fit$Y,
    k = fit$k,
    neighborhood_size = fit$neighborhood_size,
    epsilon = fit$epsilon,
    weighted = fit$weighted,
    sphere = fit$sphere,
    numDim = fit$numDim,
    levels = fit$levels,
    subspace = fit$subspace,
    blueprint = processed$blueprint
  )
}

#################
# User interface
#################
#' @title Discriminant Adaptive Nearest Neighbor With Subspace Reduction
#' @inheritParams dann
#' @param weighted weighted argument to ncoord. See [fpc::ncoord()] for details.
#' @param sphere One of "mcd", "mve", "classical", or "none" See [fpc::ncoord()] for details.
#' @param numDim Dimension of subspace used by dann. See [fpc::ncoord()] for details.
#' @return  An S3 class of type sub_dann
#' @details
#' An implementation of Hastie and Tibshirani's sub-dann in section 4.1 of
#' [Discriminant Adaptive Nearest
#' Neighbor Classification publication.](https://web.stanford.edu/~hastie/Papers/dann_IEEE.pdf).
#'
#' dann's performance suffers when noise variables are included in the model. Simulations show sub_dann
#' will generally be more performant in this scenario.
#' @export
sub_dann <- function(x, ..., k = 5, neighborhood_size = max(floor(nrow(x) / 5), 50), epsilon = 1, weighted = FALSE, sphere = "mcd", numDim = ceiling(ncol(x) / 2)) {
  UseMethod("sub_dann")
}

# Default
#' @inherit sub_dann title
#' @inheritParams sub_dann
#' @param x A data frame.
#' @inherit sub_dann return
#' @inherit sub_dann details
#' @export
sub_dann.default <- function(x, k = 5, neighborhood_size = max(floor(nrow(x) / 5), 50), epsilon = 1, weighted = FALSE, sphere = "mcd", numDim = ceiling(ncol(x) / 2), ...) {
  stop(
    "`sub_dann()` is not defined for a '", class(x)[1], "'.",
    call. = FALSE
  )
}

# XY method - data frame
#' @inherit sub_dann title
#' @inheritParams sub_dann
#' @param x A data frame.
#' @param y A vector.
#' @inherit sub_dann return
#' @inherit sub_dann details
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
#' sub_dann(x, y)
#' @export
sub_dann.data.frame <- function(x, y, k = 5, neighborhood_size = max(floor(nrow(x) / 5), 50), epsilon = 1, weighted = FALSE, sphere = "mcd", numDim = ceiling(ncol(x) / 2), ...) {
  ellipsis::check_dots_empty()
  processed <- hardhat::mold(x, y)
  sub_dann_bridge(processed, k, neighborhood_size, epsilon, weighted, sphere, numDim)
}

# XY method - matrix
#' @inherit sub_dann title
#' @inheritParams sub_dann
#' @param x A matrix.
#' @param y A vector.
#' @inherit sub_dann return
#' @inherit sub_dann details
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
#' sub_dann(x, y)
#' @export
sub_dann.matrix <- function(x, y, k = 5, neighborhood_size = max(floor(nrow(x) / 5), 50), epsilon = 1, weighted = FALSE, sphere = "mcd", numDim = ceiling(ncol(x) / 2), ...) {
  ellipsis::check_dots_empty()
  processed <- hardhat::mold(x, y)
  sub_dann_bridge(processed, k, neighborhood_size, epsilon, weighted, sphere, numDim)
}

# Formula method
#' @inherit sub_dann title
#' @inheritParams sub_dann
#' @param formula A formula. Y ~ X1 + X2
#' @param data A data frame.
#' @inherit sub_dann return
#' @inherit sub_dann details
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
#' sub_dann(Y ~ X1 + X2, train)
#' @export
sub_dann.formula <- function(formula, data, k = 5, neighborhood_size = max(floor(nrow(data) / 5), 50), epsilon = 1, weighted = FALSE, sphere = "mcd", numDim = ceiling(ncol(data) / 2), ...) {
  ellipsis::check_dots_empty()
  hardhat::validate_no_formula_duplication(formula = formula, original = TRUE)
  processed <- hardhat::mold(formula, data)
  sub_dann_bridge(processed, k, neighborhood_size, epsilon, weighted, sphere, numDim)
}

# Recipe method
#' @inherit sub_dann title
#' @inheritParams sub_dann
#' @param x A recipe from recipes library
#' @param data A data frame.
#' @inherit sub_dann return
#' @inherit sub_dann details
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
#' sub_dann(rec_obj, train)
#' @export
sub_dann.recipe <- function(x, data, k = 5, neighborhood_size = max(floor(nrow(data) / 5), 50), epsilon = 1, weighted = FALSE, sphere = "mcd", numDim = ceiling(ncol(data) / 2), ...) {
  ellipsis::check_dots_empty()
  processed <- hardhat::mold(x, data)
  sub_dann_bridge(processed, k, neighborhood_size, epsilon, weighted, sphere, numDim)
}

#################
# prediction functions
#################
#' @keywords internal
sub_dann_predict_base <- function(object, predictors, probability) {
  yTrain <- object$Y
  k <- object$k
  neighborhood_size <- object$neighborhood_size
  epsilon <- object$epsilon
  numDim <- object$numDim
  subspace <- object$subspace

  xTest <- predictors

  xTrain2 <- subspace$proj[, 1:numDim, drop = FALSE]
  xTest2 <- xTest %*% subspace$units[, 1:numDim, drop = FALSE]

  # Get predictions
  dannModel <- dann.matrix(x = xTrain2, y = yTrain, k = k, neighborhood_size = neighborhood_size, epsilon = epsilon)

  predictions <- dann_predict_base(object = dannModel, predictors = xTest2, probability = probability)

  return(predictions)
}

#' @keywords internal
sub_dann_predict_class <- function(object, predictors) {
  obsLevels <- object$levels
  out <- sub_dann_predict_base(object = object, predictors = predictors, probability = FALSE)
  temp <- rep(NA_character_, length(out))
  for (i in seq(obsLevels)) {
    temp[out == (i - 1)] <- obsLevels[i]
  }
  out <- factor(x = temp, levels = obsLevels)
  out <- hardhat::spruce_class(out)
  return(out)
}

#' @keywords internal
sub_dann_predict_prob <- function(object, predictors) {
  obsLevels <- object$levels
  out <- sub_dann_predict_base(object = object, predictors = predictors, probability = TRUE)
  out <- hardhat::spruce_prob(obsLevels, out)
  return(out)
}

#' @keywords internal
predict_sub_dann_bridge <- function(type, object, predictors) {
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
    class = sub_dann_predict_class(object, predictors),
    prob = sub_dann_predict_prob(object, predictors)
  )
}

#' @inherit sub_dann title
#' @param object of class inheriting from "sub_dann"
#' @param new_data A data frame.
#' @param type Type of prediction. (class, prob)
#' @return  A data frame containing either class or class probabilities. Adheres to tidy models standards.
#' @inherit sub_dann details
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
#' model <- sub_dann(Y ~ X1 + X2, train)
#' predict(model, test, "class")
#'
#' predict(model, test, "prob")
#' @export
predict.sub_dann <- function(object, new_data, type = "class") {
  processed <- hardhat::forge(new_data, object$blueprint)

  out <- predict_sub_dann_bridge(type, object, processed$predictors)

  hardhat::validate_prediction_size(out, new_data)

  out
}
