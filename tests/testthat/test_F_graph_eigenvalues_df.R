library(mlbench, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

######################
# Circle data with 2 related variables and 5 unrelated variables
######################
set.seed(1)
train <- mlbench.circle(500, 2) %>%
  tibble::as_tibble()
colnames(train)[1:3] <- c("X1", "X2", "Y")
train <- train %>%
  mutate(Y = as.numeric(Y))

# Add 5 unrelated variables
train <- train %>%
  mutate(
    U1 = runif(500, -1, 1),
    U2 = runif(500, -1, 1),
    U3 = runif(500, -1, 1),
    U4 = runif(500, -1, 1),
    U5 = runif(500, -1, 1)
  )

# Data suggests a subspace with 2 dimensions. The correct answer.
graph <- graph_eigenvalues_df(
  formula = Y ~ X1 + X2 + U1 + U2 + U3 + U4 + U5, train = train
)

test_that("Validate structure", {
  expect_true(all(class(graph) == c("gg", "ggplot")))
})

rm(graph)

###############################################
# All legitimate values of weighted work
###############################################

test_that("Validate structure", {
  expect_true(all(class(graph_eigenvalues_df(Y ~ X1 + X2 + U1 + U2 + U3 + U4 + U5, train, 50, FALSE, "mcd")) == c("gg", "ggplot")))
})

test_that("Validate structure", {
  expect_true(all(class(graph_eigenvalues_df(Y ~ X1 + X2 + U1 + U2 + U3 + U4 + U5, train, 50, TRUE, "mcd")) == c("gg", "ggplot")))
})


###############################################
# All legitimate values of sphere work
###############################################
test_that("Validate structure", {
  expect_true(all(class(graph_eigenvalues_df(Y ~ X1 + X2 + U1 + U2 + U3 + U4 + U5, train, 50, FALSE, "mve")) == c("gg", "ggplot")))
})

test_that("Validate structure", {
  expect_true(all(class(graph_eigenvalues_df(Y ~ X1 + X2 + U1 + U2 + U3 + U4 + U5, train, 50, TRUE, "mcd")) == c("gg", "ggplot")))
})

test_that("Validate structure", {
  expect_true(all(class(graph_eigenvalues_df(Y ~ X1 + X2 + U1 + U2 + U3 + U4 + U5, train, 50, TRUE, "classical")) == c("gg", "ggplot")))
})

###############################################
# Input checking
###############################################
test_that("Formula inputs error", {
  expect_error(graph_eigenvalues_df("foo", train), NULL)
})

M <- as.matrix(train["X1"])
test_that("Type df", {
  expect_error(graph_eigenvalues_df(Y ~ X1 + X2, M), NULL)
})
rm(M)

emptyDF <- data.frame()
test_that("Empty df", {
  expect_error(graph_eigenvalues_df(Y ~ X1 + X2, emptyDF), NULL)
})
rm(emptyDF)

rm(train)
