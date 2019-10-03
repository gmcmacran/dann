tri_cube <- function(x1, x2, s, h){

  rows <- 10
  cols <- 3

  x0 <- matrix(0, nrow = rows, ncol = cols)
  colnames(x0) <- c("A", "B", "C")
  x0 <- tibble::as_tibble(x0)

  x <- matrix(1:(rows*cols), nrow = rows, ncol = cols)
  colnames(x) <- c("A", "B", "C")

  s <- matrix(0, nrow = cols, ncol = cols)
  diag(s) <- 1

  h <- 3

  x0 <- x0 %>%
    dplyr::mutate(M = purrr::map(purrr::transpose(x0), function(...){
      as.matrix(..., nrow = 1)
      }))

calc_d <- function(m, X, S){

  m <- x0$M[[1]]
  m <- matrix(m, nrow = nrow(X), ncol = ncol(X))

  m <- matrix( c(x0$A[1], x0$B[1], x0$C[1]), nrow = 3, ncol = 10)
  X <- x

  d <- s^.5 %*% (m - X)
  d <- colSums(d)
  X0 <- matrix(c(...), nrow = length(c(...)), ncol = 1)
}
