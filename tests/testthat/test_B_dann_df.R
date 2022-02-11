library(mlbench, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

###############################################
# Easy problems
###############################################
set.seed(1)
train <- mlbench.2dnormals(1000, cl = 2, r = sqrt(2), sd = .2) %>%
  tibble::as_tibble()
colnames(train) <- c("X1", "X2", "Y")
train <- train %>%
  mutate(Y = as.numeric(Y))

test <- mlbench.2dnormals(1000, cl = 2, r = sqrt(2), sd = .2) %>%
  tibble::as_tibble()
colnames(test) <- c("X1", "X2", "Y")
test <- test %>%
  mutate(Y = as.numeric(Y))

dannPreds <- dann_df(Y ~ X1 + X2, train, test)

test_that("Validate structure", {
  expect_true(is.vector(dannPreds))
  expect_true(is.numeric(dannPreds))
  expect_true(length(dannPreds) == nrow(test))
})

test_that("Compare predictions to observed #1", {
  expect_true(all(dannPreds == test$Y))
})

rm(dannPreds)

######################
# probabilities
######################

dannPreds <- dann_df(Y ~ X1 + X2, train, test, 9, 60, 2, TRUE)

test_that("Validate structure", {
  expect_true(is.matrix(dannPreds))
  expect_true(is.numeric(dannPreds))
  expect_true(nrow(dannPreds) == nrow(test))
  expect_true(ncol(dannPreds) == 2)
  expect_true(all(colnames(dannPreds) == c("Class1", "Class2")))
})

rm(dannPreds)

######################
# default values match
######################
test_that("Defalut values match?", {
  expect_true(formals(dann_df)$k == formals(dann)$k)
  expect_true(formals(dann_df)$neighborhood_size == formals(dann)$neighborhood_size)
  expect_true(formals(dann_df)$epsilon == formals(dann)$epsilon)
  expect_true(formals(dann_df)$probability == formals(dann)$probability)
})

###############################################
# Input checking
###############################################
#######
# Data checks
#######
test_that("Formula inputs error", {
  expect_error(dann_df("foo", train, test), NULL)
})

M <- as.matrix(train["X1"])
test_that("Type df", {
  expect_error(dann_df(Y ~ X1 + X2, M, test), NULL)
  expect_error(dann_df(Y ~ X1 + X2, train, M), NULL)
})
rm(M)

emptyDF <- data.frame()
test_that("Empty df", {
  expect_error(dann_df(Y ~ X1 + X2, emptyDF, test), NULL)
  expect_error(dann_df(Y ~ X1 + X2, test, emptyDF), NULL)
})
rm(emptyDF)

rm(train, test)
