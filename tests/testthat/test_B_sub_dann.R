library(mlbench, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(recipes, warn.conflicts = FALSE)

###############################################
# Test different interfaces
###############################################
# dann does not do much during fit.
# So data does not matter.
# Confirming all interfaces work and return the
# same results.

set.seed(1)
train <- mlbench.2dnormals(1000, cl = 2, r = sqrt(2), sd = .2) %>%
  tibble::as_tibble()
colnames(train) <- c("X1", "X2", "Y")

xTrain <- train %>%
  select(X1, X2) %>%
  as.matrix()

yTrain <- train %>%
  pull(Y) %>%
  as.numeric() %>%
  as.vector()

rec_obj <- recipe(Y ~ X1 + X2, data = train)

test_that("No errors?", {
  expect_no_error(sub_dann(train[, 1:2], yTrain))
  expect_no_error(sub_dann(Y ~ X1 + X2, train))
  expect_no_error(sub_dann(xTrain, yTrain))
  expect_no_error(sub_dann(rec_obj, train))
})

rm(train, xTrain, yTrain, rec_obj)

###############################################
# Create data for checking
###############################################
set.seed(1)
xTest <- matrix(0, nrow = 100, ncol = 2)
xTrain <- matrix(0, nrow = 100, ncol = 2)

xTrain[, 1] <- runif(100, -10, 1)
xTrain[, 2] <- runif(100, -1, 1)
yTrain <- c(rep(1, 50), rep(2, 50))

xTest[, 1] <- runif(100, -1, 1)
xTest[, 2] <- runif(100, -1, 1)

colnames(xTrain) <- c("X1", "X2")
colnames(xTest) <- c("X1", "X2")

###############################################
# All legitimate values of weighted work
###############################################
subDannPreds <- sub_dann(xTrain, yTrain, 2, 5, 1, FALSE, "mcd", 2)
test_that("No errors?", {
  expect_no_error(sub_dann(xTrain, yTrain, 2, 5, 1, FALSE, "mcd", 2))
  expect_no_error(sub_dann(xTrain, yTrain, 2, 5, 1, TRUE, "mcd", 2))
})

###############################################
# All legitimate values of sphere work
###############################################
test_that("Validate structure", {
  expect_no_error(sub_dann(xTrain, yTrain, 2, 50, 1, FALSE, "mve", 2))
  expect_no_error(sub_dann(xTrain, yTrain, 2, 50, 1, FALSE, "mcd", 2))
  expect_no_error(sub_dann(xTrain, yTrain, 2, 50, 1, FALSE, "classical", 2))
  expect_no_error(sub_dann(xTrain, yTrain, 2, 50, 1, FALSE, "none", 2))
})

###############################################
# default values match
###############################################
test_that("Defalut values match?", {
  expect_true(formals(dann)$k == formals(sub_dann)$k)
  expect_true(formals(dann)$neighborhood_size == formals(sub_dann)$neighborhood_size)
  expect_true(formals(dann)$epsilon == formals(sub_dann)$epsilon)
})

test_that("Defalut values match?", {
  expect_true(formals(sub_dann)$k == formals(sub_dann.data.frame)$k)
  expect_true(formals(sub_dann)$neighborhood_size == formals(sub_dann.data.frame)$neighborhood_size)
  expect_true(formals(sub_dann)$epsilon == formals(sub_dann.data.frame)$epsilon)
  expect_true(formals(sub_dann)$weighted == formals(sub_dann.data.frame)$weighted)
  expect_true(formals(sub_dann)$sphere == formals(sub_dann.data.frame)$sphere)
  expect_true(formals(sub_dann)$numDim == formals(sub_dann.data.frame)$numDim)
})

test_that("Defalut values match?", {
  expect_true(formals(sub_dann)$k == formals(sub_dann.default)$k)
  expect_true(formals(sub_dann)$neighborhood_size == formals(sub_dann.default)$neighborhood_size)
  expect_true(formals(sub_dann)$epsilon == formals(sub_dann.default)$epsilon)
  expect_true(formals(sub_dann)$weighted == formals(sub_dann.default)$weighted)
  expect_true(formals(sub_dann)$sphere == formals(sub_dann.default)$sphere)
  expect_true(formals(sub_dann)$numDim == formals(sub_dann.default)$numDim)
})

test_that("Defalut values match?", {
  expect_true(formals(sub_dann)$k == formals(sub_dann.formula)$k)
  expect_true(formals(sub_dann)$epsilon == formals(sub_dann.formula)$epsilon)
  expect_true(formals(sub_dann)$weighted == formals(sub_dann.formula)$weighted)
  expect_true(formals(sub_dann)$sphere == formals(sub_dann.formula)$sphere)
})

test_that("Defalut values match?", {
  expect_true(formals(sub_dann)$k == formals(sub_dann.matrix)$k)
  expect_true(formals(sub_dann)$neighborhood_size == formals(sub_dann.matrix)$neighborhood_size)
  expect_true(formals(sub_dann)$epsilon == formals(sub_dann.matrix)$epsilon)
  expect_true(formals(sub_dann)$weighted == formals(sub_dann.matrix)$weighted)
  expect_true(formals(sub_dann)$sphere == formals(sub_dann.matrix)$sphere)
  expect_true(formals(sub_dann)$numDim == formals(sub_dann.matrix)$numDim)
})

test_that("Defalut values match?", {
  expect_true(formals(sub_dann)$k == formals(sub_dann.recipe)$k)
  expect_true(formals(sub_dann.formula)$neighborhood_size == formals(sub_dann.recipe)$neighborhood_size)
  expect_true(formals(sub_dann)$epsilon == formals(sub_dann.recipe)$epsilon)
  expect_true(formals(sub_dann)$weighted == formals(sub_dann.recipe)$weighted)
  expect_true(formals(sub_dann)$sphere == formals(sub_dann.recipe)$sphere)
  expect_true(formals(sub_dann.formula)$numDim == formals(sub_dann.recipe)$numDim)
})


###############################################
# Input checking
###############################################
#######
# Data checks
#######
chars <- matrix("A", nrow = 5, ncol = 2)
colnames(chars) <- c("X1", "X2")
test_that("Nonnumeric inputs error", {
  expect_error(sub_dann(chars, yTrain, 3, 10, 1), NULL)
  expect_error(sub_dann(xTrain, chars, 3, 10, 1), NULL)
})
rm(chars)

missingValues <- xTrain
missingValues[1, 1] <- NA
test_that("Missing values in inputs error", {
  expect_error(sub_dann(missingValues, yTrain), NULL)
})
missingValues <- yTrain
missingValues[1] <- NA
test_that("Missing values in inputs error", {
  expect_error(sub_dann(xTrain, missingValues), NULL)
})
rm(missingValues)

xTrainrowMissing <- xTrain[1:(nrow(xTrain) - 1), ]
yTrainrowMissing <- yTrain[1:(length(yTrain) - 1)]
test_that("Differnet number of rows in xTrain and yTrain error.", {
  expect_error(sub_dann(xTrainrowMissing, yTrain), NULL)
  expect_error(sub_dann(xTrain, yTrainrowMissing), NULL)
})
rm(xTrainrowMissing, yTrainrowMissing)

noDataxTrain <- xTrain[0, ]
noDatayTrain <- yTrain[0]
test_that("No rows in inputs error", {
  expect_error(sub_dann(noDataxTrain, noDatayTrain, xTest), NULL)
})
rm(noDataxTrain, noDatayTrain)

#######
# non data checks
#######
test_that("k checks works", {
  expect_error(sub_dann(xTrain, yTrain, c(3, 2), 3, 1, FALSE, "mcd", 2), NULL)
  expect_error(sub_dann(xTrain, yTrain, "3", 3, 1, FALSE, "mcd", 2), NULL)
  expect_error(sub_dann(xTrain, yTrain, 100000, 3, 1, FALSE, "mcd", 2), NULL)
  expect_error(sub_dann(xTrain, yTrain, 0, 3, 1, FALSE, "mcd", 2), NULL)
})

test_that("neighborhood_size checks works", {
  expect_error(sub_dann(xTrain, yTrain, 2, "3", 1, FALSE, "mcd", 2), NULL)
  expect_error(sub_dann(xTrain, yTrain, 2, 100000, 1, FALSE, "mcd", 2), NULL)
  expect_error(sub_dann(xTrain, yTrain, 2, 0, 1, FALSE, "mcd", 2), NULL)
  expect_error(sub_dann(xTrain, yTrain, 4, 3, 1, FALSE, "mcd", 2), NULL)
})

test_that("epsilon checks works", {
  expect_error(sub_dann(xTrain, yTrain, 2, 2, c(2, 3), FALSE, "mcd", 2), NULL)
  expect_error(sub_dann(xTrain, yTrain, 2, 2, "1", FALSE, "mcd", 2), NULL)
  expect_error(sub_dann(xTrain, yTrain, 2, 2, -1, FALSE, "mcd", 2), NULL)
})

test_that("weighted checks works", {
  expect_error(sub_dann(xTrain, yTrain, 2, 2, 1, c(TRUE, FALSE), "mcd", 2), NULL)
  expect_error(sub_dann(xTrain, yTrain, 2, 2, 1, "TRUE", "mcd", 2), NULL)
})

test_that("sphere checks works", {
  expect_error(sub_dann(xTrain, yTrain, 2, 2, 1, FALSE, c("mcd", "mcd"), 2), NULL)
  expect_error(sub_dann(xTrain, yTrain, 2, 2, 1, FALSE, FALSE, 2), NULL)
  expect_error(sub_dann(xTrain, yTrain, 2, 2, 1, FALSE, "foo", 2), NULL)
})

test_that("numDim checks works", {
  expect_error(sub_dann(xTrain, yTrain, 2, 2, 1, FALSE, "mcd", c(1, 2)), NULL)
  expect_error(sub_dann(xTrain, yTrain, 2, 2, 1, FALSE, "mcd", "2"), NULL)
  expect_error(sub_dann(xTrain, yTrain, 2, 2, 1, FALSE, "mcd", 0), NULL)
})
