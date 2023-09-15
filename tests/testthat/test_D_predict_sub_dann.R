library(mlbench, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(recipes, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)

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

model <- sub_dann(xTrain, yTrain, 5, 50, 1, FALSE, "mcd", 2)
sub_dannPreds <- predict(model, xTest, "class")

test_that("Validate structure", {
  expect_true(is_tibble(sub_dannPreds))
  expect_true(is.factor(sub_dannPreds$.pred_class))
  expect_true(nrow(sub_dannPreds) == nrow(xTest))
})

test_that("Compare predictions to observed #1", {
  expect_true(all(sub_dannPreds == yTest))
})

rm(train, test)
rm(xTrain, yTrain)
rm(xTest, yTest)
rm(model, sub_dannPreds)

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

model <- sub_dann(xTrain, yTrain, 5, 60, 1, FALSE, "mcd", 3)
sub_dannPreds <- predict(model, xTest, "class")

test_that("Validate structure", {
  expect_true(is_tibble(sub_dannPreds))
  expect_true(is.factor(sub_dannPreds$.pred_class))
  expect_true(nrow(sub_dannPreds) == nrow(xTest))
})

test_that("Compare predictions to observed #2", {
  expect_true(mean(sub_dannPreds == yTest) >= .90)
})

######################
# probabilities
######################
xTest <- matrix(0, nrow = 100, ncol = 2)
xTrain <- matrix(0, nrow = 100, ncol = 2)

xTrain[, 1] <- runif(100)
xTrain[, 2] <- runif(100)
yTrain <- c(rep(-3, 2), rep(-2, 3))

xTest[, 1] <- runif(100)
xTest[, 2] <- runif(100)

colnames(xTrain) <- c("X1", "X2")
colnames(xTest) <- c("X1", "X2")

yTrain <- c(rep(0, 50), rep(1, 50))
model <- sub_dann(xTrain, yTrain, 3, 5, 1)
sub_dannPreds <- predict(model, xTest, "prob")

test_that("Validate structure", {
  expect_true(is_tibble(sub_dannPreds))
  expect_true(is.numeric(sub_dannPreds$.pred_0))
  expect_true(is.numeric(sub_dannPreds$.pred_1))
  expect_true(nrow(sub_dannPreds) == nrow(xTest))
  expect_true(ncol(sub_dannPreds) == 2)
  expect_true(all(colnames(sub_dannPreds) == c(".pred_0", ".pred_1")))
})

yTrain <- c(rep(-4, 50), rep(-1, 50))
model <- sub_dann(xTrain, yTrain, 3, 5, 1)
sub_dannPreds <- predict(model, xTest, "prob")
test_that("Validate structure", {
  expect_true(is_tibble(sub_dannPreds))
  expect_true(is.numeric(sub_dannPreds[[".pred_-4"]]))
  expect_true(is.numeric(sub_dannPreds[[".pred_-1"]]))
  expect_true(nrow(sub_dannPreds) == nrow(xTest))
  expect_true(ncol(sub_dannPreds) == 2)
  expect_true(all(colnames(sub_dannPreds) == c(".pred_-4", ".pred_-1")))
})

rm(train, test)
rm(xTest, yTest)
rm(model, sub_dannPreds)

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

  model <- sub_dann(xTrain, yTrain, k, 15, 1, FALSE, "mcd", 2)
  sub_dannPreds <- predict(model, xTest, "prob")

  test_that("Validate structure", {
    expect_true(is_tibble(sub_dannPreds))
    expect_true(is.numeric(sub_dannPreds$.pred_1))
    expect_true(is.numeric(sub_dannPreds$.pred_2))
    expect_true(nrow(sub_dannPreds) == nrow(xTest))
    expect_true(ncol(sub_dannPreds) == 2)
    expect_true(all(colnames(sub_dannPreds) == c(".pred_1", ".pred_2")))
  })

  test_that("Confirm class probabilities sum to 1", {
    expect_true(all(rowSums(sub_dannPreds) == 1))
  })

  possibleProb <- 0:k / k
  sub_dannPreds <- as.matrix(sub_dannPreds)
  test_that("Confirm class probabilities are divisible by k", {
    expect_true(all(sub_dannPreds %in% possibleProb))
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

model <- sub_dann(train[, 1:2], yTrain)
test_that("No errors?", {
  expect_no_error(predict(model, train[, 1:2]))
})

model <- sub_dann(Y ~ X1 + X2, train)
test_that("No errors?", {
  expect_no_error(predict(model, train))
})

model <- sub_dann(xTrain, yTrain)
test_that("No errors?", {
  expect_no_error(predict(model, xTrain))
})

model <- sub_dann(rec_obj, train)
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

model <- sub_dann(xTrain, yTrain, 2, 5, 1)

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
