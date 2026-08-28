#################
# thread control
#################
#' @title Control the number of threads dann uses
#' @param n The number of threads to use. A positive whole number, or NULL to
#'   restore the default.
#' @return `dann_set_threads` returns the previous setting invisibly: a positive
#'   whole number, or NULL if dann was using the default. `dann_get_threads`
#'   returns the number of threads the next prediction will use.
#' @details
#' The prediction loop inside [predict.dann()] and [predict.sub_dann()] is
#' parallelized with OpenMP. By default it uses every core the OpenMP runtime
#' makes available, which honors the OMP_NUM_THREADS environment variable.
#'
#' These functions change that count for dann alone. The count is applied to
#' dann's own parallel region, so no other package that uses OpenMP is
#' affected. This is different from calling something like
#' `omp_set_num_threads` in another package, which writes the thread count
#' shared by everything running in the session.
#'
#' The setting lasts for the R session. It is not saved between sessions and it
#' is not stored on model objects, so a model fit under one setting predicts
#' under whatever setting is in force at the time.
#'
#' Without OpenMP support, prediction runs on a single thread. `dann_get_threads`
#' then returns 1 no matter what was set.
#' @examples
#' library(dann)
#'
#' # Limit dann to two threads.
#' previous <- dann_set_threads(2)
#' dann_get_threads()
#'
#' # Put it back.
#' dann_set_threads(previous)
#' @export
dann_set_threads <- function(n = NULL) {
  if (is.null(n)) {
    n <- 0
  } else {
    if (length(n) != 1) {
      stop("Argument n should be a length 1 vector.", call. = FALSE)
    }
    if (!is.numeric(n)) {
      stop("Argument n should be numeric.", call. = FALSE)
    }
    if (is.na(n)) {
      stop("Argument n should not be NA.", call. = FALSE)
    }
    if (n != round(n)) {
      stop("Argument n should be a whole number.", call. = FALSE)
    }
    if (n < 1) {
      stop("Argument n should be at least 1.", call. = FALSE)
    }
  }

  previous <- dann_set_threads_C(as.integer(n))

  if (previous == 0) {
    invisible(NULL)
  } else {
    invisible(previous)
  }
}

#' @rdname dann_set_threads
#' @export
dann_get_threads <- function() {
  dann_get_threads_C()
}
