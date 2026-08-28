###############################################
# Setting and reading the thread count
###############################################
# setup.R caps the suite at 2 threads. Each test here puts back whatever it
# found so the rest of the suite is unaffected.

test_that("setup.R caps the suite at two threads", {
  expect_equal(dann_get_threads(), 2)
})

test_that("dann_get_threads reports the current setting", {
  previous <- dann_set_threads(4)
  on.exit(dann_set_threads(previous))

  expect_equal(dann_get_threads(), 4)
})

test_that("dann_set_threads returns the previous setting", {
  previous <- dann_set_threads(3)
  on.exit(dann_set_threads(previous))

  expect_equal(dann_set_threads(5), 3)
  expect_equal(dann_get_threads(), 5)
})

test_that("NULL restores the default", {
  previous <- dann_set_threads(1)
  on.exit(dann_set_threads(previous))

  # The default is every core OpenMP offers, so all that can be asserted
  # portably is that it is a positive whole number.
  expect_equal(dann_set_threads(NULL), 1)
  expect_true(dann_get_threads() >= 1)
})

test_that("the previous setting round trips through NULL", {
  dann_set_threads(NULL)
  previous <- dann_set_threads(2)

  expect_null(previous)
  expect_equal(dann_get_threads(), 2)
})

###############################################
# Bad input
###############################################
test_that("bad input is rejected", {
  previous <- dann_set_threads(2)
  on.exit(dann_set_threads(previous))

  expect_error(dann_set_threads(c(1, 2)))
  expect_error(dann_set_threads("2"))
  expect_error(dann_set_threads(NA))
  expect_error(dann_set_threads(2.5))
  expect_error(dann_set_threads(0))
  expect_error(dann_set_threads(-1))

  # A rejected call leaves the setting alone.
  expect_equal(dann_get_threads(), 2)
})

###############################################
# Predictions do not depend on the thread count
###############################################
test_that("thread count does not change predictions", {
  set.seed(1)
  train <- mlbench::mlbench.circle(300, 2) |>
    tibble::as_tibble()
  colnames(train) <- c("X1", "X2", "Y")

  previous <- dann_set_threads(1)
  on.exit(dann_set_threads(previous))

  model <- dann(Y ~ X1 + X2, train)
  serial <- predict(model, train, "class")

  dann_set_threads(2)
  parallel <- predict(model, train, "class")

  expect_equal(serial, parallel)
})
