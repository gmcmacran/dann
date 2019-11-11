context("dann")
library(dann)

###############################################
# Same Resutls as python version?
###############################################
# Problem 1
###################################
# xTest <- matrix(0, nrow = 5, ncol = 2)
# xTrain <- matrix(0, nrow = 5, ncol = 2)
#
# xTrain[, 1] <- c(1, 2, 3, 4, 5)
# xTrain[, 2] <- c(6, 7, 8, 9, 10)
# yTrain <- matrix(c(rep(1, 2), rep(2, 3)), nrow = 5, ncol = 1)
#
# xTest[, 1] <- c(5, 4, 3, 2, 1)
# xTest[, 2] <- c(10, 9, 8, 7, 6)
# danPreds <- dann(xTrain, yTrain, xTest, 3, 3, 1) == c(2, 2, 2, 1, 1)
#
# test_that("Validate structure", {
#   expect_true(is.matrix(danPreds))
#   expect_true(is.numeric(danPreds))
#   expect_true(nrow(danPreds) == nrow(xTest))
# })
#
# test_that("Compare results to python version. Problem #1 ", {
#   expect_true(all(dann(xTrain, yTrain, xTest, 3, 10, 1) == c(2, 2, 2, 1, 1)))
# })

###############################################
# Input checking
###############################################

xTest <- matrix(0, nrow = 5, ncol = 2)
xTrain <- matrix(0, nrow = 5, ncol = 2)

xTrain[, 1] <- c(1, 2, 3, 4, 5)
xTrain[, 2] <- c(6, 7, 8, 9, 10)
yTrain <- matrix(c(rep(1, 2), rep(2, 3)), nrow = 5, ncol = 1)

xTest[, 1] <- c(5, 4, 3, 2, 1)
xTest[, 2] <- c(10, 9, 8, 7, 6)

#######
# Data checks
#######
chars <- matrix("A", nrow = 5, ncol = 2)
test_that("Nonnumeric inputs error", {
  expect_error(dann(chars, yTrain, xTest, 3, 10, 1), NULL)
  expect_error(dann(xTrain, chars, xTest, 3, 10, 1), NULL)
  expect_error(dann(xTrain, yTrain, chars, 3, 10, 1), NULL)
})
rm(chars)

missingValues <- yTrain
missingValues[1, 1] <- NA
test_that("Missing values in inputs error", {
  expect_error(dann(missingValues, yTrain, xTest), NULL)
  expect_error(dann(xTrain, missingValues, xTest), NULL)
  expect_error(dann(xTrain, yTrain, missingValues), NULL)
})
rm(missingValues)

xTrainrowMissing <- xTrain[1:(nrow(xTrain) - 1), ]
yTrainrowMissing <- yTrain[1:(nrow(yTrain) - 1), ]
test_that("Differnet number of rows in xTrain and yTrain error.", {
  expect_error(dann(xTrainrowMissing, yTrain, xTest), NULL)
  expect_error(dann(xTrain, yTrainrowMissing, xTest), NULL)
})
rm(xTrainrowMissing, yTrainrowMissing)

noDataxTrain <- xTrain[0, ]
noDatayTrain <- yTrain[0, ]
noDataxTest <- xTest[0, ]
test_that("No rows in inputs error", {
  expect_error(dann(noDataxTrain, noDatayTrain, xTest), NULL)
  expect_error(dann(xTrain, yTrainrowMissing, noDataxTest), NULL)
})
rm(noDataxTrain, noDatayTrain, noDataxTest)

WrongVarTrain <- xTrain[0, 1:(ncol(xTrain) - 1)]
WrongVarTest <- xTest[0, 1:(ncol(xTest) - 1)]
test_that("Different number of columns in xTrain and xTest error.", {
  expect_error(dann(WrongVarTrain, yTrain, xTest), NULL)
  expect_error(dann(xTrain, yTrain, WrongVarTest), NULL)
})
rm(WrongVarTrain, WrongVarTest)

TooManyyTrain <- cbind(yTrain, yTrain)
test_that("Too many columns in yTrain error.", {
  expect_error(dann(xTrain, TooManyyTrain, xTest), NULL)
})
rm(TooManyyTrain)

#######
# non data checks
#######
test_that("k checks works", {
  expect_error(dann(xTrain, yTrain, xTest, c(3, 2), 3, 1), NULL)
  expect_error(dann(xTrain, yTrain, xTest, "3", 3, 1), NULL)
  expect_error(dann(xTrain, yTrain, xTest, 6, 3, 1), NULL)
  expect_error(dann(xTrain, yTrain, xTest, 0, 3, 1), NULL)
})

test_that("neighborhood_size checks works", {
  expect_error(dann(xTrain, yTrain, xTest, 2, c(2, 3), 1), NULL)
  expect_error(dann(xTrain, yTrain, xTest, 2, "3", 1), NULL)
  expect_error(dann(xTrain, yTrain, xTest, 2, 6, 1), NULL)
  expect_error(dann(xTrain, yTrain, xTest, 2, 0, 1), NULL)
})

test_that("epsilon checks works", {
  expect_error(dann(xTrain, yTrain, xTest, 2, 2, c(2, 3)), NULL)
  expect_error(dann(xTrain, yTrain, xTest, 2, 2, "1"), NULL)
  expect_error(dann(xTrain, yTrain, xTest, 2, 2, -1), NULL)
})
