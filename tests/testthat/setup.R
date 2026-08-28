# CRAN checks must not use more than two cores. dann defaults to every core the
# OpenMP runtime offers, so cap it for the duration of the test run.
dann_set_threads(2)
