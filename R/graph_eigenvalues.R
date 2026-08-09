#################
# base
#################
#' @keywords internal
graph_eigenvalues_base <- function(xTrain, yTrain,
                                   neighborhood_size = max(floor(nrow(xTrain) / 5), 50),
                                   weighted = FALSE, sphere = "mcd") {
  ###################################
  # Input checking
  ###################################
  # Missing values.
  if (anyNA(xTrain)) {
    stop("Argument xTrain should not have any NA values.")
  }
  if (anyNA(yTrain)) {
    stop("Argument yTrain should not have any NA values.")
  }

  # Confirm structure looks right
  if (nrow(xTrain) != length(yTrain)) {
    stop("nrow(xTrain) should match length(yTrain).")
  }
  if (ncol(xTrain) < 1) {
    stop("Argument xTrain should have at least one column.")
  }
  if (nrow(xTrain) < 1) {
    stop("Argument xTrain should have at least one row.")
  }
  if (length(yTrain) < 1) {
    stop("Argument yTrain should have positive length.")
  }

  # neighborhood_size is valid
  if (length(neighborhood_size) != 1) {
    stop("Argument neighborhood_size should be a length 1 vector.")
  }
  if (!is.numeric(neighborhood_size)) {
    stop("Argument neighborhood_size should be numeric.")
  }
  if (neighborhood_size > nrow(xTrain)) {
    stop("Argument neighborhood_size should be less than or equal to the number of rows in xTrain.")
  }
  if (neighborhood_size <= 1) {
    stop("Argument neighborhood_size should be at least 2.")
  }

  # weighted is valid
  if (length(weighted) != 1) {
    stop("Argument weighted should be a length 1 vector.")
  }
  if (!is.logical(weighted)) {
    stop("Argument weighted should be logical.")
  }

  # sphere is valid
  if (length(sphere) != 1) {
    stop("Argument sphere should be a length 1 vector.")
  }
  if (!is.character(sphere)) {
    stop("Argument sphere should be a character vector.")
  }
  if (!(sphere %in% c("mve", "mcd", "classical", "none"))) {
    stop("Argument sphere should be one of mve, mcd, classical or none.")
  }

  # Find subspace
  subspace <- fpc::ncoord(
    xd = xTrain, clvecd = yTrain,
    nn = neighborhood_size, weighted = weighted,
    sphere = sphere, countmode = 999999999999999
  )

  eigen <- tibble::enframe(subspace$ev, value = "eigenValues", name = "order")

  graph <- ggplot2::ggplot(eigen, ggplot2::aes(x = .data$order, y = .data$eigenValues)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(breaks = 1:nrow(eigen)) +
    ggplot2::labs(x = "Rank Order", y = "Eigenvalues")

  return(graph)
}


#################
# bridge
#################
graph_eigenvalues_bridge <- function(processed, neighborhood_size, weighted, sphere) {
  predictors <- processed$predictors
  predictors <- as.matrix(predictors)
  hardhat::validate_predictors_are_numeric(predictors)

  outcomes <- processed$outcomes[[1]]
  hardhat::validate_outcomes_are_univariate(outcomes)
  if (!is.factor(outcomes)) {
    outcomes <- factor(outcomes)
  }
  levels <- levels(outcomes)
  outcomes <- as.vector(as.numeric(outcomes))

  graph_eigenvalues_base(
    xTrain = predictors,
    yTrain = outcomes,
    neighborhood_size = neighborhood_size,
    weighted = weighted,
    sphere = sphere
  )
}

#################
# User interface
#################
#' @title A helper for choosing sub_dann's numDim
#' @inheritParams sub_dann
#' @return A ggplot2 graph.
#' @details This function plots the eigenvalues found by [fpc::ncoord()] against their
#' rank order. Judge how many eigenvalues are large and set [sub_dann()]'s numDim to
#' that number. Keep neighborhood_size, weighted, and sphere consistent between this
#' function and [sub_dann()] so the two look at the same subspace.
#' @importFrom rlang .data
#' @export
graph_eigenvalues <- function(x, ..., neighborhood_size = max(floor(nrow(x) / 5), 50), weighted = FALSE, sphere = "mcd") {
  UseMethod("graph_eigenvalues")
}

# Default
#' @inherit graph_eigenvalues title
#' @inheritParams graph_eigenvalues
#' @param x An object for which no `graph_eigenvalues()` method exists.
#' @inherit graph_eigenvalues return
#' @inherit graph_eigenvalues details
#' @export
graph_eigenvalues.default <- function(x, neighborhood_size = max(floor(nrow(x) / 5), 50), weighted = FALSE, sphere = "mcd", ...) {
  stop(
    "`graph_eigenvalues()` is not defined for a '", class(x)[1], "'.",
    call. = FALSE
  )
}

# XY method - data frame
#' @inherit graph_eigenvalues title
#' @inheritParams graph_eigenvalues
#' @param x A data frame.
#' @param y A vector of outcomes. Numeric, character, and factor are all accepted.
#' @inherit graph_eigenvalues return
#' @inherit graph_eigenvalues details
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
#' # Add 5 unrelated variables
#' train <- train %>%
#'   mutate(
#'     U1 = runif(300, -1, 1),
#'     U2 = runif(300, -1, 1),
#'     U3 = runif(300, -1, 1),
#'     U4 = runif(300, -1, 1),
#'     U5 = runif(300, -1, 1)
#'   )
#'
#' y <- train$Y
#' x <- cbind(train[, 1:2], train[, 4:8])
#'
#' graph_eigenvalues(x, y)
#' @export
graph_eigenvalues.data.frame <- function(x, y, neighborhood_size = max(floor(nrow(x) / 5), 50), weighted = FALSE, sphere = "mcd", ...) {
  rlang::check_dots_empty()
  processed <- hardhat::mold(x, y)
  graph_eigenvalues_bridge(processed, neighborhood_size, weighted, sphere)
}

# XY method - matrix
#' @inherit graph_eigenvalues title
#' @inheritParams graph_eigenvalues
#' @param x A matrix.
#' @param y A vector of outcomes. Numeric, character, and factor are all accepted.
#' @inherit graph_eigenvalues return
#' @inherit graph_eigenvalues details
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
#' # Add 5 unrelated variables
#' train <- train %>%
#'   mutate(
#'     U1 = runif(300, -1, 1),
#'     U2 = runif(300, -1, 1),
#'     U3 = runif(300, -1, 1),
#'     U4 = runif(300, -1, 1),
#'     U5 = runif(300, -1, 1)
#'   )
#'
#' y <- as.numeric(train$Y)
#' x <- cbind(train$X1, train$X2, train$U1, train$U2, train$U3, train$U4, train$U5)
#'
#' graph_eigenvalues(x, y)
#' @export
graph_eigenvalues.matrix <- function(x, y, neighborhood_size = max(floor(nrow(x) / 5), 50), weighted = FALSE, sphere = "mcd", ...) {
  rlang::check_dots_empty()
  processed <- hardhat::mold(x, y)
  graph_eigenvalues_bridge(processed, neighborhood_size, weighted, sphere)
}

# Formula method
#' @inherit graph_eigenvalues title
#' @inheritParams graph_eigenvalues
#' @param formula A formula specifying the outcome and predictors. For example, Y ~ X1 + X2.
#' @param data A data frame containing the variables in `formula` or in the recipe.
#' @inherit graph_eigenvalues return
#' @inherit graph_eigenvalues details
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
#' # Add 5 unrelated variables
#' train <- train %>%
#'   mutate(
#'     U1 = runif(300, -1, 1),
#'     U2 = runif(300, -1, 1),
#'     U3 = runif(300, -1, 1),
#'     U4 = runif(300, -1, 1),
#'     U5 = runif(300, -1, 1)
#'   )
#'
#' graph_eigenvalues(Y ~ X1 + X2 + U1 + U2 + U3 + U4 + U5, train)
#' @export
graph_eigenvalues.formula <- function(formula, data, neighborhood_size = max(floor(nrow(data) / 5), 50), weighted = FALSE, sphere = "mcd", ...) {
  rlang::check_dots_empty()
  hardhat::validate_no_formula_duplication(formula = formula, original = TRUE)
  processed <- hardhat::mold(formula, data)
  graph_eigenvalues_bridge(processed, neighborhood_size, weighted, sphere)
}

# Recipe method
#' @inherit graph_eigenvalues title
#' @inheritParams graph_eigenvalues
#' @param x A recipe from the recipes package.
#' @param data A data frame containing the variables in `formula` or in the recipe.
#' @inherit graph_eigenvalues return
#' @inherit graph_eigenvalues details
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
#' # Add 5 unrelated variables
#' train <- train %>%
#'   mutate(
#'     U1 = runif(300, -1, 1),
#'     U2 = runif(300, -1, 1),
#'     U3 = runif(300, -1, 1),
#'     U4 = runif(300, -1, 1),
#'     U5 = runif(300, -1, 1)
#'   )
#'
#' rec_obj <- recipe(Y ~ X1 + X2 + U1 + U2 + U3 + U4 + U5, data = train)
#'
#' graph_eigenvalues(rec_obj, train)
#' @export
graph_eigenvalues.recipe <- function(x, data, neighborhood_size = max(floor(nrow(data) / 5), 50), weighted = FALSE, sphere = "mcd", ...) {
  rlang::check_dots_empty()
  processed <- hardhat::mold(x, data)
  graph_eigenvalues_bridge(processed, neighborhood_size, weighted, sphere)
}
