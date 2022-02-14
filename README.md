
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DANN

<!-- badges: start -->

[![R build
status](https://github.com/gmcmacran/dann/workflows/R-CMD-check/badge.svg)](https://github.com/gmcmacran/dann/actions)
[![Codecov test
coverage](https://codecov.io/gh/gmcmacran/dann/branch/master/graph/badge.svg)](https://app.codecov.io/gh/gmcmacran/dann?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/dann)](https://cran.r-project.org/package=dann)
<!-- badges: end -->

An implementation of Hastie and Tibshirani’s Discriminant Adaptive
Nearest Neighbor Classification in R.

## Installation

You can install the released version of dann from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dann")
```

## Package Introduction

In k nearest neighbors, the shape of the neighborhood is always
circular. Discriminant Adaptive Nearest Neighbor (dann) is a variation
of k nearest neighbors where the shape of the neighborhood is data
driven. The neighborhood is elongated along class boundaries and shrunk
in the orthogonal direction. See [Discriminate Adaptive Nearest Neighbor
Classification](https://web.stanford.edu/~hastie/Papers/dann_IEEE.pdf)
by Hastie and Tibshirani. This package implements DANN and sub-DANN in
section 4.1 of the publication and is based on Christopher Jenness’s
python [implementation.](https://github.com/christopherjenness/ML-lib)

## Example: Circle Problem

In this example, simulated data is made. The overall trend is a circle
inside a square.

``` r
library(dann)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(mlbench)

set.seed(1) 

#Create training data
train <- mlbench.circle(500, 2) %>%
  tibble::as_tibble()
colnames(train) <- c("X1", "X2", "Y")
train <- train %>%
  mutate(Y = as.numeric(Y))

ggplot(train, aes(x = X1, y = X2, colour = as.factor(Y))) + 
  geom_point() + 
  labs(title = "Train Data", colour = "Y")
```

<img src="man/figures/README-Circle-1.png" width="100%" />

``` r
#Create test data
test <- mlbench.circle(500, 2) %>%
  tibble::as_tibble()
colnames(test) <- c("X1", "X2", "Y")
test <- test %>%
  mutate(Y = as.numeric(Y))

ggplot(test, aes(x = X1, y = X2, colour = as.factor(Y))) + 
  geom_point() + 
  labs(title = "Test Data", colour = "Y")
```

<img src="man/figures/README-Circle-2.png" width="100%" />

To train a model, the data and a few parameters are passed into dann.
Neighborhood_size is the number of data points used to estimate a good
shape of the neighborhood. K is the number of data points used in the
final classification. Overall, dann is a highly accurate model for this
data set.

``` r
dannPreds <- dann_df(formula = Y ~ X1 + X2, train = train, test = test, k = 7, neighborhood_size = 50)
mean(dannPreds == test$Y)
#> [1] 0.964
```
