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
  expect_no_error(dann(train[, 1:2], yTrain))
  expect_no_error(dann(Y ~ X1 + X2, train))
  expect_no_error(dann(xTrain, yTrain))
  expect_no_error(dann(rec_obj, train))
})

rm(train, xTrain, yTrain, rec_obj)

###############################################
# default values match
###############################################
test_that("Defalut values match?", {
  expect_true(formals(dann)$k == formals(dann.data.frame)$k)
  expect_true(formals(dann)$neighborhood_size == formals(dann.data.frame)$neighborhood_size)
  expect_true(formals(dann)$epsilon == formals(dann.data.frame)$epsilon)
})

test_that("Defalut values match?", {
  expect_true(formals(dann)$k == formals(dann.formula)$k)
  expect_true(formals(dann)$epsilon == formals(dann.formula)$epsilon)
})

test_that("Defalut values match?", {
  expect_true(formals(dann)$k == formals(dann.default)$k)
  expect_true(formals(dann)$neighborhood_size == formals(dann.default)$neighborhood_size)
  expect_true(formals(dann)$epsilon == formals(dann.default)$epsilon)
})

test_that("Defalut values match?", {
  expect_true(formals(dann)$k == formals(dann.matrix)$k)
  expect_true(formals(dann)$neighborhood_size == formals(dann.matrix)$neighborhood_size)
  expect_true(formals(dann)$epsilon == formals(dann.matrix)$epsilon)
})

test_that("Defalut values match?", {
  expect_true(formals(dann)$k == formals(dann.recipe)$k)
  expect_true(formals(dann.formula)$neighborhood_size == formals(dann.recipe)$neighborhood_size)
  expect_true(formals(dann)$epsilon == formals(dann.recipe)$epsilon)
})

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

xTrainDF <- tibble::tibble(X1 = xTrain[, 1], X2 = xTrain[, 2])
colnames(xTrainDF) <- c("X1", "X2")
dat <- dplyr::mutate(xTrainDF, Y = yTrain)
rec_obj <- recipe(formula = Y ~ X1 + X2, data = dat)

###############################################
# Input checking
###############################################
#######
# Data checks
#######
chars <- matrix("A", nrow = 5, ncol = 2)
colnames(chars) <- c("X1", "X2")
test_that("Nonnumeric inputs error", {
  expect_error(dann(chars, yTrain, 3, 10, 1), NULL)
  expect_error(dann(xTrain, chars, 3, 10, 1), NULL)
})
rm(chars)

missingValues <- xTrain
missingValues[1, 1] <- NA
test_that("Missing values in inputs error", {
  expect_error(dann(missingValues, yTrain), NULL)
})
missingValues <- yTrain
missingValues[1] <- NA
test_that("Missing values in inputs error", {
  expect_error(dann(xTrain, missingValues), NULL)
})
rm(missingValues)

xTrainrowMissing <- xTrain[1:(nrow(xTrain) - 1), ]
yTrainrowMissing <- yTrain[1:(length(yTrain) - 1)]
test_that("Differnet number of rows in xTrain and yTrain error.", {
  expect_error(dann(xTrainrowMissing, yTrain), NULL)
  expect_error(dann(xTrain, yTrainrowMissing), NULL)
})
rm(xTrainrowMissing, yTrainrowMissing)

noDataxTrain <- xTrain[0, ]
noDatayTrain <- yTrain[0]
test_that("No rows in inputs error", {
  expect_error(dann(noDataxTrain, noDatayTrain), NULL)
  expect_error(dann(xTrain, noDatayTrain), NULL)
})
rm(noDataxTrain, noDatayTrain)

#######
# non data checks
#######
test_that("k checks works", {
  expect_error(dann(xTrain, yTrain, c(3, 2), 3, 1), NULL)
  expect_error(dann(xTrain, yTrain, "3", 3, 1), NULL)
  expect_error(dann(xTrain, yTrain, 100000, 3, 1), NULL)
  expect_error(dann(xTrain, yTrain, 0, 3, 1), NULL)
})

test_that("neighborhood_size checks works", {
  expect_error(dann(xTrain, yTrain, 2, c(2, 3), 1), NULL)
  expect_error(dann(xTrain, yTrain, 2, "3", 1), NULL)
  expect_error(dann(xTrain, yTrain, 2, 100000, 1), NULL)
  expect_error(dann(xTrain, yTrain, 2, 0, 1), NULL)
  expect_error(dann(xTrain, yTrain, 4, 3, 1), NULL)
})

test_that("epsilon checks works", {
  expect_error(dann(xTrain, yTrain, 2, 2, c(2, 3)), NULL)
  expect_error(dann(xTrain, yTrain, 2, 2, "1"), NULL)
  expect_error(dann(xTrain, yTrain, 2, 2, -1), NULL)
})

test_that("... checks works", {
  expect_error(dann(x = xTrainDF, y = yTrain, k = 2, neighborhood_size = 2, epsilonn = 1), NULL)
  expect_error(dann(formula = Y ~ X1 + X2, data = dat, k = 2, neighborhood_size = 2, epsilonn = 1), NULL)
  expect_error(dann(x = xTrain, y = yTrain, k = 2, neighborhood_size = 2, epsilonn = 1), NULL)
  expect_error(dann(x = rec_obj, data = dat, k = 2, neighborhood_size = 2, epsilonn = 1), NULL)
})
