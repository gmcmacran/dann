#' Print dann model.
#'
#' @param x a dann model.
#' @param ... arguments passed to other methods.
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
#' model <- dann(Y ~ X1 + X2, train)
#' print(model)
#' @export
print.dann <- function(x, ...) {
  params <- c("k", "neighborhood_size", "epsilon", "levels")
  for (i in seq_len(length(params))) {
    msg <- paste(params[i][1], ":", paste(x[[params[i]]], collapse = ", "), collapse = " ")
    print(msg)
  }
  invisible(x)
}

#' Print dann model.
#'
#' @param x a dann model.
#' @param ... arguments passed to other methods.
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
#' model <- sub_dann(Y ~ X1 + X2, train)
#' print(model)
#' @export
print.sub_dann <- function(x, ...) {
  params <- c("k", "neighborhood_size", "epsilon", "weighted", "sphere", "numDim", "levels")
  for (i in seq_len(length(params))) {
    msg <- paste(params[i][1], ":", paste(x[[params[i]]], collapse = ", "), collapse = " ")
    print(msg)
  }
  invisible(x)
}
