###############################################
# Setting and reading the thread count
###############################################
# setup.R caps the suite at 2 threads. Each test here puts back whatever it
# found so the rest of the suite is unaffected. No test asks for more than 2,
# even where the count is only read back and never used to run anything, so
# that nothing in the package ever requests more than CRAN allows.
#
# Without OpenMP the count is always 1, so tests asserting a specific count
# above 1 are skipped rather than made to fail on such a build.

test_that("setup.R caps the suite at two threads", {
  skip_if_not(dann_has_openmp(), "built without OpenMP")

  expect_equal(dann_get_threads(), 2)
})

test_that("dann_get_threads reports the current setting", {
  previous <- dann_set_threads(1)
  on.exit(dann_set_threads(previous))

  expect_equal(dann_get_threads(), 1)
})

test_that("dann_set_threads returns the previous setting", {
  skip_if_not(dann_has_openmp(), "built without OpenMP")

  previous <- dann_set_threads(1)
  on.exit(dann_set_threads(previous))

  expect_equal(dann_set_threads(2), 1)
  expect_equal(dann_get_threads(), 2)
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
  skip_if_not(dann_has_openmp(), "built without OpenMP")

  dann_set_threads(NULL)
  previous <- dann_set_threads(2)

  expect_null(previous)
  expect_equal(dann_get_threads(), 2)
})

test_that("dann_has_openmp reports a single logical", {
  expect_true(is.logical(dann_has_openmp()))
  expect_length(dann_has_openmp(), 1)
  expect_false(is.na(dann_has_openmp()))
})

###############################################
# Bad input
###############################################
test_that("bad input is rejected", {
  previous <- dann_set_threads(1)
  on.exit(dann_set_threads(previous))

  expect_error(dann_set_threads(c(1, 2)))
  expect_error(dann_set_threads("2"))
  expect_error(dann_set_threads(NA))
  expect_error(dann_set_threads(2.5))
  expect_error(dann_set_threads(0))
  expect_error(dann_set_threads(-1))

  # Non finite values pass a round() test but cannot be coerced to an integer.
  expect_error(dann_set_threads(NaN))
  expect_error(dann_set_threads(Inf))
  expect_error(dann_set_threads(-Inf))

  # A rejected call leaves the setting alone.
  expect_equal(dann_get_threads(), 1)
})

test_that("state survives a rejected call", {
  skip_if_not(dann_has_openmp(), "built without OpenMP")

  previous <- dann_set_threads(2)
  on.exit(dann_set_threads(previous))

  expect_error(dann_set_threads(Inf))

  # Left unguarded, Inf coerces to NA_integer_, reaches C++ as INT_MIN and
  # makes the next call fail. The save and restore idiom the docs recommend
  # has to keep working after a rejected call.
  expect_equal(dann_set_threads(1), 2)
  expect_equal(dann_get_threads(), 1)
})

###############################################
# Clamping
###############################################
test_that("a count above the available threads is clamped with a message", {
  previous <- dann_set_threads(1)
  on.exit(dann_set_threads(previous))

  available <- dann_max_threads_C()

  expect_message(dann_set_threads(available + 1))
  expect_equal(dann_get_threads(), available)
})

test_that("values beyond integer range are clamped, not overflowed", {
  previous <- dann_set_threads(1)
  on.exit(dann_set_threads(previous))

  available <- dann_max_threads_C()

  # as.integer() would turn these into NA_integer_.
  suppressMessages(dann_set_threads(.Machine$integer.max + 1))
  expect_equal(dann_get_threads(), available)

  suppressMessages(dann_set_threads(2^40))
  expect_equal(dann_get_threads(), available)
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
