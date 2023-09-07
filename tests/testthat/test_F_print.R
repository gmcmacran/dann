library(mlbench, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)

###############################################
# Make data
###############################################

set.seed(1)
train <- mlbench.2dnormals(1000, cl = 2, r = sqrt(2), sd = .2) %>%
  tibble::as_tibble()
colnames(train) <- c("X1", "X2", "Y")

###############################################
# Test dann's print
###############################################
model <- dann(Y ~ X1 + X2, train)
test_that("works for one sample.", {
  expect_output(print(model))
})

###############################################
# Test sub_dann's print
###############################################
model <- sub_dann(Y ~ X1 + X2, train)
test_that("works for one sample.", {
  expect_output(print(model))
})
