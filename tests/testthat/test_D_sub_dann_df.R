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

subDannPreds <- sub_dann_df(Y ~ X1 + X2, train, test)

test_that("Validate structure", {
  expect_true(is.vector(subDannPreds))
  expect_true(is.numeric(subDannPreds))
  expect_true(length(subDannPreds) == nrow(test))
})

test_that("Compare predictions to observed #1", {
  expect_true(mean(subDannPreds == test$Y) > .95)
})

rm(subDannPreds)

######################
# probabilities
######################

subDannPreds <- sub_dann_df(Y ~ X1 + X2, train, test, 9, 60, 2, TRUE)

test_that("Validate structure", {
  expect_true(is.matrix(subDannPreds))
  expect_true(is.numeric(subDannPreds))
  expect_true(nrow(subDannPreds) == nrow(test))
  expect_true(ncol(subDannPreds) == 2)
  expect_true(all(colnames(subDannPreds) == c("Class1", "Class2")))
})

rm(subDannPreds)

######################
# default values match
######################
test_that("Defalut values match?", {
  expect_true(formals(sub_dann_df)$k == formals(sub_dann)$k)
  expect_true(formals(sub_dann_df)$neighborhood_size == formals(sub_dann)$neighborhood_size)
  expect_true(formals(sub_dann_df)$epsilon == formals(sub_dann)$epsilon)
  expect_true(formals(sub_dann_df)$probability == formals(sub_dann)$probability)
  expect_true(formals(sub_dann_df)$weighted == formals(sub_dann)$weighted)
  expect_true(formals(sub_dann_df)$sphere == formals(sub_dann)$sphere)
  expect_true(formals(sub_dann_df)$numDim == formals(sub_dann)$numDim)
})

###############################################
# Input checking
###############################################
#######
# Data checks
#######
test_that("Formula inputs error", {
  expect_error(sub_dann_df("foo", train, test), NULL)
})

M <- as.matrix(train["X1"])
test_that("Type df", {
  expect_error(sub_dann_df(Y ~ X1 + X2, M, test), NULL)
  expect_error(sub_dann_df(Y ~ X1 + X2, train, M), NULL)
})
rm(M)

emptyDF <- data.frame()
test_that("Empty df", {
  expect_error(sub_dann_df(Y ~ X1 + X2, emptyDF, test), NULL)
  expect_error(sub_dann_df(Y ~ X1 + X2, test, emptyDF), NULL)
})
rm(emptyDF)

rm(train, test)
