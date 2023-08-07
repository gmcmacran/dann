library(mlbench, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(recipes, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)

###############################################
# Same results as python version?
###############################################
######################
# Problem 1
######################
xTest <- matrix(0, nrow = 5, ncol = 2)
xTrain <- matrix(0, nrow = 5, ncol = 2)

xTrain[, 1] <- c(1, 2, 3, 4, 5)
xTrain[, 2] <- c(6, 7, 8, 9, 10)
colnames(xTrain) <- c("X1", "X2")
yTrain <- c(rep(1, 2), rep(2, 3))

xTest[, 1] <- c(5, 4, 3, 2, 1)
xTest[, 2] <- c(10, 9, 8, 7, 6)
colnames(xTest) <- c("X1", "X2")

model <- dann(xTrain, yTrain, 3, 5, 1)
dannPreds <- predict(model, xTest, "class")

test_that("Validate structure", {
  expect_true(is_tibble(dannPreds))
  expect_true(is.factor(dannPreds$.pred_class))
  expect_true(nrow(dannPreds) == nrow(xTest))
})

correct <- tibble(.pred_class = factor(x = c(2, 2, 2, 1, 1), levels = c("1", "2")))
test_that("Compare results to python version. Problem #1", {
  expect_equal(dannPreds, correct)
})

rm(xTest, xTrain, yTrain, dannPreds, model)

######################
# Problem 2
######################
xTest <- matrix(0, nrow = 10, ncol = 3)
xTrain <- matrix(0, nrow = 10, ncol = 3)

xTrain[, 1] <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
xTrain[, 2] <- c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
xTrain[, 3] <- c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
yTrain <- c(rep(1, 2), rep(2, 2), rep(3, 2), rep(4, 2), rep(5, 2))

xTest[, 1] <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
xTest[, 2] <- c(7, 7, 8, 8, 10, 10, 12, 12, 14, 14)
xTest[, 3] <- c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

colnames(xTrain) <- c("X1", "X2", "X3")
colnames(xTest) <- c("X1", "X2", "X3")

model <- dann(xTrain, yTrain, 1, 4, 1)
dannPreds <- predict(model, xTest, "class")

test_that("Validate structure", {
  expect_true(is_tibble(dannPreds))
  expect_true(is.factor(dannPreds$.pred_class))
  expect_true(nrow(dannPreds) == nrow(xTest))
})

correct <- tibble(.pred_class = factor(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5), levels = c("1", "2", "3", "4", "5")))
test_that("Compare results to python version. Problem #1", {
  expect_equal(dannPreds, correct)
})

rm(xTest, xTrain, yTrain, model, dannPreds)

###############################################
# Easy problems
###############################################
######################
# Problem 1
######################
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

test <- mlbench.2dnormals(1000, cl = 2, r = sqrt(2), sd = .2) %>%
  tibble::as_tibble()
colnames(test) <- c("X1", "X2", "Y")

xTest <- test %>%
  select(X1, X2) %>%
  as.matrix()

yTest <- test %>%
  pull(Y) %>%
  as.numeric() %>%
  as.vector()

model <- dann(xTrain, yTrain)
dannPreds <- predict(model, xTest, "class")

test_that("Validate structure", {
  expect_true(is_tibble(dannPreds))
  expect_true(is.factor(dannPreds$.pred_class))
  expect_true(nrow(dannPreds) == nrow(xTest))
})

test_that("Compare predictions to observed #1", {
  expect_true(all(dannPreds == yTest))
})

rm(train, test)
rm(xTrain, yTrain)
rm(xTest, yTest)
rm(model, dannPreds)

######################
# Problem 2
######################
set.seed(1)
train <- mlbench.hypercube(n = 1000, d = 3, sides = rep(1, 3), sd = 0.1) %>%
  tibble::as_tibble()
colnames(train) <- c("X1", "X2", "X3", "Y")

xTrain <- train %>%
  select(X1, X2, X3) %>%
  as.matrix()

yTrain <- train %>%
  pull(Y) %>%
  as.numeric() %>%
  as.vector()

test <- mlbench.hypercube(n = 1000, d = 3, sides = rep(1, 3), sd = 0.1) %>%
  tibble::as_tibble()
colnames(test) <- c("X1", "X2", "X3", "Y")

xTest <- test %>%
  select(X1, X2, X3) %>%
  as.matrix()

yTest <- test %>%
  pull(Y) %>%
  as.numeric() %>%
  as.vector()

model <- dann(xTrain, yTrain, 7, 100, 1)
dannPreds <- predict(model, xTest, "class")

test_that("Validate structure", {
  expect_true(is_tibble(dannPreds))
  expect_true(is.factor(dannPreds$.pred_class))
  expect_true(nrow(dannPreds) == nrow(xTest))
})

test_that("Compare predictions to observed #2", {
  expect_true(all(dannPreds == yTest))
})

rm(train, test)
rm(xTrain, yTrain)
rm(xTest, yTest)
rm(model, dannPreds)

######################
# Ties are broken by most common class
######################
xTrain <- matrix(c(0, 0, 1), nrow = 3, ncol = 1)

yTrain <- matrix(c(0, 0, 1), nrow = 3, ncol = 1)

colnames(xTrain) <- c("X1")
colnames(yTrain) <- c("Y")

# Splits 0 and 1 evenly.
xTest <- matrix(.5, nrow = 1, ncol = 1)
colnames(xTest) <- c("X1")

model <- dann(xTrain, yTrain, 1, 3, 1)
dannPreds <- predict(model, xTest, "class")

test_that("Run multiple times to confirm results are consitant", {
  expect_true(dannPreds == 0)
  expect_true(dannPreds == 0)
  expect_true(dannPreds == 0)
  expect_true(dannPreds == 0)
  expect_true(dannPreds == 0)
})

rm(xTrain, yTrain, xTest)
rm(model, dannPreds)

###############################################
# Confirm class numeric value shifting works as expected
###############################################
######################
# Class
######################
xTest <- matrix(0, nrow = 5, ncol = 2)
xTrain <- matrix(0, nrow = 5, ncol = 2)

xTrain[, 1] <- c(1, 2, 3, 4, 5)
xTrain[, 2] <- c(6, 7, 8, 9, 10)
yTrain <- c(rep(-3, 2), rep(-2, 3))

xTest[, 1] <- c(5, 4, 3, 2, 1)
xTest[, 2] <- c(10, 9, 8, 7, 6)

colnames(xTrain) <- c("X1", "X2")
colnames(xTest) <- c("X1", "X2")

model <- dann(xTrain, yTrain, 3, 5, 1)
dannPreds <- predict(model, xTest)

test_that("Validate structure", {
  expect_true(is_tibble(dannPreds))
  expect_true(is.factor(dannPreds$.pred_class))
  expect_true(nrow(dannPreds) == nrow(xTest))
})

test_that("Shift logic test #1", {
  expect_true(all(dannPreds == c(-2, -2, -2, -3, -3)))
})

# Repeat with non sequental classes
yTrain <- c(rep(-3, 2), rep(2, 3))
model <- dann(xTrain, yTrain, 3, 5, 1)
dannPreds <- predict(model, xTest)

test_that("Validate structure", {
  expect_true(is_tibble(dannPreds))
  expect_true(is.factor(dannPreds$.pred_class))
  expect_true(nrow(dannPreds) == nrow(xTest))
})

test_that("Shift logic test #2", {
  expect_true(all(dannPreds == c(2, 2, 2, -3, -3)))
})

# Repeat normal use case. 0 and 1.
yTrain <- c(rep(0, 2), rep(1, 3))
model <- dann(xTrain, yTrain, 3, 5, 1)
dannPreds <- predict(model, xTest)
test_that("Validate structure", {
  expect_true(is_tibble(dannPreds))
  expect_true(is.factor(dannPreds$.pred_class))
  expect_true(nrow(dannPreds) == nrow(xTest))
})

test_that("Shift logic test #3", {
  expect_true(all(dannPreds == c(1, 1, 1, 0, 0)))
})

######################
# probabilities
######################

yTrain <- c(rep(0, 2), rep(1, 3))
model <- dann(xTrain, yTrain, 3, 5, 1)
dannPreds <- predict(model, xTest, "prob")

test_that("Validate structure", {
  expect_true(is_tibble(dannPreds))
  expect_true(is.numeric(dannPreds$.pred_0))
  expect_true(is.numeric(dannPreds$.pred_1))
  expect_true(nrow(dannPreds) == nrow(xTest))
  expect_true(ncol(dannPreds) == 2)
  expect_true(all(colnames(dannPreds) == c(".pred_0", ".pred_1")))
})

yTrain <- c(rep(-4, 2), rep(-1, 3))
model <- dann(xTrain, yTrain, 3, 5, 1)
dannPreds <- predict(model, xTest, "prob")
test_that("Validate structure", {
  expect_true(is_tibble(dannPreds))
  expect_true(is.numeric(dannPreds[[".pred_-4"]]))
  expect_true(is.numeric(dannPreds[[".pred_-1"]]))
  expect_true(nrow(dannPreds) == nrow(xTest))
  expect_true(ncol(dannPreds) == 2)
  expect_true(all(colnames(dannPreds) == c(".pred_-4", ".pred_-1")))
})

rm(xTest, xTrain, yTrain, model, dannPreds)

###############################################
# Confirm class probabilities look correct.
###############################################
######################
# Generate problem
######################
set.seed(1)
train <- mlbench.circle(20, 2) %>%
  tibble::as_tibble()
colnames(train) <- c("X1", "X2", "Y")

xTrain <- train %>%
  select(X1, X2) %>%
  as.matrix()

yTrain <- train %>%
  pull(Y) %>%
  as.numeric() %>%
  as.vector()

test <- mlbench.circle(20, 2) %>%
  tibble::as_tibble()
colnames(test) <- c("X1", "X2", "Y")

xTest <- test %>%
  select(X1, X2) %>%
  as.matrix()

yTest <- test %>%
  pull(Y) %>%
  as.numeric() %>%
  as.vector()

######################
# K
######################
ks <- c(1, 2, 5, 10)
for (i in seq_len(length(ks))) {
  k <- ks[i]

  model <- dann(xTrain, yTrain, k, 15, 1)
  dannPreds <- predict(model, xTest, "prob")

  test_that("Validate structure", {
    expect_true(is_tibble(dannPreds))
    expect_true(is.numeric(dannPreds$.pred_1))
    expect_true(is.numeric(dannPreds$.pred_2))
    expect_true(nrow(dannPreds) == nrow(xTest))
    expect_true(ncol(dannPreds) == 2)
    expect_true(all(colnames(dannPreds) == c(".pred_1", ".pred_2")))
  })

  test_that("Confirm class probabilities sum to 1", {
    expect_true(all(rowSums(dannPreds) == 1))
  })

  possibleProb <- 0:k / k
  dannPreds <- as.matrix(dannPreds)
  test_that("Confirm class probabilities are divisible by k", {
    expect_true(all(dannPreds %in% possibleProb))
  })
}

rm(train, test)
rm(xTrain, yTrain)
rm(xTest, yTest)
rm(k, ks)

###############################################
# predict works regardless of interface
###############################################

set.seed(1)
train <- mlbench.2dnormals(50, cl = 2, r = sqrt(2), sd = .2) %>%
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

model <- dann(train[, 1:2], yTrain)
test_that("No errors?", {
  expect_no_error(predict(model, train[, 1:2]))
})

model <- dann(Y ~ X1 + X2, train)
test_that("No errors?", {
  expect_no_error(predict(model, train))
})

model <- dann(xTrain, yTrain)
test_that("No errors?", {
  expect_no_error(predict(model, xTrain))
})

model <- dann(rec_obj, train)
test_that("No errors?", {
  expect_no_error(predict(model, train))
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

model <- dann(xTrain, yTrain, 2, 5, 1)

###############################################
# All legitimate values of type work
###############################################
test_that("No errors?", {
  expect_no_error(predict(model, xTest, "class"))
  expect_no_error(predict(model, xTest, "prob"))
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
  expect_error(predict(model, chars, "class"), NULL)
  expect_error(predict(model, chars, "prob"), NULL)
})
rm(chars)

missingValues <- xTrain
missingValues[1] <- NA
test_that("Missing values in inputs error", {
  expect_error(predict(model, missingValues, "class"), NULL)
})
rm(missingValues)

noDataxTrain <- xTrain[0, ]
test_that("No rows in inputs error", {
  expect_error(predict(model, missingValues, "class"), NULL)
})
rm(noDataxTrain)

#######
# non data checks
#######
test_that("probability checks works", {
  expect_error(predict(model, xTest, "foo"), NULL)
  expect_error(predict(model, xTest, c("class", "prob")), NULL)
})
