# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`dann` is an R package implementing Discriminant Adaptive Nearest Neighbor (DANN) classification from [Hastie & Tibshirani (1996)](https://web.stanford.edu/~hastie/Papers/dann_IEEE.pdf). It exports three user-facing functions: `dann`, `sub_dann`, and `graph_eigenvalues`. The computationally intensive prediction loop is implemented in C++ (RcppArmadillo) with OpenMP parallelization.

## Commands

### Build and install
```r
R CMD INSTALL .
# or
devtools::install()
```

### Regenerate Rcpp glue (after editing .cpp files)
```r
Rcpp::compileAttributes()
```
This regenerates `R/RcppExports.R` and `src/RcppExports.cpp` — do not edit those files by hand.

### Tests
```r
devtools::test()                        # all tests
devtools::test(filter = "dann")         # matches test_A_dann.R
devtools::test(filter = "sub_dann")     # matches test_B_sub_dann.R
```

### CRAN check
```r
devtools::check()
```

### Lint
```r
lintr::lint_package()
```
The `.lintr` config disables `line_length_linter` and `object_name_linter` but enables several extra linters.

### Rebuild documentation
```r
devtools::document()
```

## Architecture

### Hardhat pattern

All three exported functions follow the same [hardhat](https://hardhat.tidymodels.org/) model-fitting stack:

```
Generic (UseMethod)
  └─ Methods (.data.frame / .matrix / .formula / .recipe)
       └─ Bridge  (hardhat::mold → validate → fix params → call impl)
            └─ Impl  (pure logic, returns a plain list)
                 └─ Constructor  (strict field validation → hardhat::new_model)
```

Prediction mirrors this: `predict.dann` → `predict_dann_bridge` → `dann_predict_class`/`dann_predict_prob` → `dann_predict_base`.

### C++ layer (`src/internal_armadillo_helpers.cpp`)

The main entry point is `dann_predict_all_C`. Per test point it:

1. Computes Euclidean distances to all training points
2. Picks the `neighborhood_size` nearest neighbors via a 3-key sort (distance, class precedence, class value)
3. Computes within-class and between-class covariance matrices on the neighborhood
4. Derives the DANN sigma matrix: `W*(B* + εI)W*` where `W* = pinv(sqrt(W))`
5. Re-ranks all training points by DANN distance, takes the top `k`, and returns the mode or class proportions

The outer loop over test observations is parallelized with `#pragma omp parallel for`. OpenMP flags are set in `src/Makevars`.

`src/internal_helpers_C.cpp` contains `calc_distance_C` (plain Euclidean, no Armadillo); it is currently unused in the main prediction path.

### `sub_dann` vs `dann`

`sub_dann` first reduces the feature space via `fpc::ncoord()` (discriminant coordinates), then delegates to a `dann.matrix` model trained on the projected data. `numDim` controls how many dimensions to retain. `graph_eigenvalues` plots the eigenvalue scree from `fpc::ncoord()` to help choose `numDim`.

### Factor/numeric encoding

Outcomes are stored internally as 0-indexed integers (factor level index minus 1). The `levels` field on the model object maps them back to original labels during prediction.

### Parameter auto-correction

`fix_dann_params` and `fix_sub_dann_params` silently clamp out-of-range `k`, `neighborhood_size`, `epsilon`, and `numDim` values with `message()` at fit time. The constructors (`new_dann`, `new_sub_dann`) then enforce strict validation on the corrected values.
