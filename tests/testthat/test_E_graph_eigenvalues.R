library(mlbench, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(recipes, warn.conflicts = FALSE)

######################
# Circle data with 2 related variables and 5 unrelated variables
######################
set.seed(1)
train <- mlbench.circle(500, 2) %>%
  tibble::as_tibble()
colnames(train)[1:3] <- c("X1", "X2", "Y")

# Add 5 unrelated variables
train <- train %>%
  mutate(
    U1 = runif(500, -1, 1),
    U2 = runif(500, -1, 1),
    U3 = runif(500, -1, 1),
    U4 = runif(500, -1, 1),
    U5 = runif(500, -1, 1)
  )

rec_obj <- recipe(Y ~ X1 + X2 + U1 + U2 + U3 + U4 + U5, data = train)

xTrain <- train %>%
  select(X1, X2, U1, U2, U3, U4, U5) %>%
  as.matrix()

yTrain <- train %>%
  pull(Y) %>%
  as.numeric() %>%
  as.vector()

test_that("No errors?", {
  expect_no_error(graph_eigenvalues(train[, 1:2], yTrain))
  expect_no_error(graph_eigenvalues(Y ~ X1 + X2, train))
  expect_no_error(graph_eigenvalues(xTrain, yTrain))
  expect_no_error(graph_eigenvalues(rec_obj, train))
})

###############################################
# All legitimate values of weighted work
###############################################

test_that("No errors?", {
  expect_no_error(graph_eigenvalues(xTrain, yTrain, 50, FALSE, "mcd"))
  expect_no_error(graph_eigenvalues(xTrain, yTrain, 50, TRUE, "mcd"))
})


###############################################
# All legitimate values of sphere work
###############################################
test_that("No errors?", {
  expect_no_error(graph_eigenvalues(xTrain, yTrain, 50, FALSE, "mve"))
  expect_no_error(graph_eigenvalues(xTrain, yTrain, 50, FALSE, "mcd"))
  expect_no_error(graph_eigenvalues(xTrain, yTrain, 50, FALSE, "classical"))
  expect_no_error(graph_eigenvalues(xTrain, yTrain, 50, FALSE, "none"))
})

###############################################
# default values match
###############################################
test_that("Defalut values match?", {
  expect_true(formals(graph_eigenvalues)$neighborhood_size == formals(sub_dann)$neighborhood_size)
  expect_true(formals(graph_eigenvalues)$weighted == formals(sub_dann)$weighted)
  expect_true(formals(graph_eigenvalues)$sphere == formals(sub_dann)$sphere)
})

test_that("Defalut values match?", {
  expect_true(formals(graph_eigenvalues)$neighborhood_size == formals(graph_eigenvalues.default)$neighborhood_size)
  expect_true(formals(graph_eigenvalues)$weighted == formals(graph_eigenvalues.default)$weighted)
  expect_true(formals(graph_eigenvalues)$sphere == formals(graph_eigenvalues.default)$sphere)
})

test_that("Defalut values match?", {
  expect_true(formals(graph_eigenvalues)$weighted == formals(graph_eigenvalues.formula)$weighted)
  expect_true(formals(graph_eigenvalues)$sphere == formals(graph_eigenvalues.formula)$sphere)
})

test_that("Defalut values match?", {
  expect_true(formals(graph_eigenvalues)$neighborhood_size == formals(graph_eigenvalues.matrix)$neighborhood_size)
  expect_true(formals(graph_eigenvalues)$weighted == formals(graph_eigenvalues.matrix)$weighted)
  expect_true(formals(graph_eigenvalues)$sphere == formals(graph_eigenvalues.matrix)$sphere)
})

test_that("Defalut values match?", {
  expect_true(formals(graph_eigenvalues.formula)$neighborhood_size == formals(graph_eigenvalues.recipe)$neighborhood_size)
  expect_true(formals(graph_eigenvalues)$weighted == formals(graph_eigenvalues.recipe)$weighted)
  expect_true(formals(graph_eigenvalues)$sphere == formals(graph_eigenvalues.recipe)$sphere)
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
  expect_error(graph_eigenvalues(chars, yTrain, 50, FALSE, "mcd"), NULL)
  expect_error(graph_eigenvalues(xTrain, chars, 50, FALSE, "mcd"), NULL)
})
rm(chars)

missingValues <- yTrain
missingValues[1] <- NA
test_that("Nonnumeric inputs error", {
  expect_error(graph_eigenvalues(missingValues, yTrain, 50, FALSE, "mcd"), NULL)
  expect_error(graph_eigenvalues(xTrain, missingValues, 50, FALSE, "mcd"), NULL)
})
rm(missingValues)

xTrainrowMissing <- xTrain[1:(nrow(xTrain) - 1), ]
yTrainrowMissing <- yTrain[1:(length(yTrain) - 1)]
test_that("Differnet number of rows in xTrain and yTrain error.", {
  expect_error(graph_eigenvalues(xTrainrowMissing, yTrain), NULL)
  expect_error(graph_eigenvalues(xTrain, yTrainrowMissing), NULL)
})
rm(xTrainrowMissing, yTrainrowMissing)

noDataxTrain <- xTrain[0, ]
noDatayTrain <- yTrain[0]
test_that("No rows in inputs error", {
  expect_error(graph_eigenvalues(noDataxTrain, noDatayTrain), NULL)
})
rm(noDataxTrain, noDatayTrain)

#######
# non data checks
#######

test_that("neighborhood_size checks works", {
  expect_error(graph_eigenvalues(xTrain, yTrain, c(2, 3), FALSE, "mcd"), NULL)
  expect_error(graph_eigenvalues(xTrain, yTrain, "3", FALSE, "mcd"), NULL)
  expect_error(graph_eigenvalues(xTrain, yTrain, 100000, FALSE, "mcd"), NULL)
  expect_error(graph_eigenvalues(xTrain, yTrain, 0, FALSE, "mcd"), NULL)
})

test_that("weighted checks works", {
  expect_error(graph_eigenvalues(xTrain, yTrain, 3, c(TRUE, FALSE), "mcd"), NULL)
  expect_error(graph_eigenvalues(xTrain, yTrain, 3, "FALSE", "mcd"), NULL)
})

test_that("sphere checks works", {
  expect_error(graph_eigenvalues(xTrain, yTrain, 3, FALSE, c("mcd", "mcd")), NULL)
  expect_error(graph_eigenvalues(xTrain, yTrain, 3, FALSE, FALSE), NULL)
  expect_error(graph_eigenvalues(xTrain, yTrain, 3, FALSE, "foo"), NULL)
})

set.seed(1)
xTrain <- matrix(0, nrow = 100, ncol = 2)

xTrain[, 1] <- runif(100, -10, 1)
xTrain[, 2] <- runif(100, -1, 1)
yTrain <- c(rep(1, 50), rep(2, 50))

colnames(xTrain) <- c("X1", "X2")

xTrainDF <- tibble::tibble(X1 = xTrain[, 1], X2 = xTrain[, 2])
colnames(xTrainDF) <- c("X1", "X2")
dat <- dplyr::mutate(xTrainDF, Y = yTrain)
rec_obj <- recipe(formula = Y ~ X1 + X2, data = dat)

test_that("... checks works", {
  expect_error(graph_eigenvalues(x = xTrainDF, y = yTrain, neighborhood_sizee = 2), NULL)
  expect_error(graph_eigenvalues(formula = Y ~ X1 + X2, data = dat, neighborhood_sizee = 2), NULL)
  expect_error(graph_eigenvalues(x = xTrain, y = yTrain, neighborhood_sizee = 2), NULL)
  expect_error(graph_eigenvalues(x = rec_obj, data = dat, neighborhood_sizee = 2), NULL)
})

rm(train)
rm(xTrain, yTrain)
