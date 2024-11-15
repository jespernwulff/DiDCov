
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DiDCov

<!-- badges: start -->
<!-- badges: end -->

**DiDCov** is an R package designed to assist researchers and analysts
working with Difference-in-Differences (DiD) estimations. It provides
tools for constructing variance-covariance matrices for DiD estimates,
accommodating both constant and time-decaying correlations between
periods.

## Installation

You can install the development version of DiDCov from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jespernwulff/DiDCov")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(DiDCov)

#Example 1: Using a specified decay parameter (exponential decay)
years <- c(2008, 2009, 2010, 2011)
variances <- c(0.0001, 0.0002, 0.00015, 0.00012)
decay_param <- 0.1
result_exp <- construct_cov_matrix_decay(
   years = years,
   variances = variances,
   decay_type = "exponential",
   decay_param = decay_param
)
print(result_exp$cov_matrix)
#>              [,1]         [,2]         [,3]         [,4]
#> [1,] 1.000000e-04 0.0001279633 0.0001002736 8.115257e-05
#> [2,] 1.279633e-04 0.0002000000 0.0001567224 1.268372e-04
#> [3,] 1.002736e-04 0.0001567224 0.0001500000 1.213967e-04
#> [4,] 8.115257e-05 0.0001268372 0.0001213967 1.200000e-04
print(result_exp$decay_param)
#> [1] 0.1
```
