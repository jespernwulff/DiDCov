
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DiDCov

<!-- badges: start -->
<!-- badges: end -->

**DiDCov** is an R package designed to assist researchers and analysts
working with Difference-in-Differences (DiD) estimations. It provides
tools for constructing variance-covariance matrices for DiD estimates,
accommodating both constant and time-decaying correlations between
periods.

The package is currently experimental and under development.

## Installation

You can install the development version of DiDCov from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jespernwulff/DiDCov")
```

## Example

**Example 1: Constructing a Covariance Matrix with a Specified Rho**

The `construct_cov_matrix` function creates a covariance matrix using a
constant correlation coefficient (`rho`) for all off-diagonal elements.

``` r
library(DiDCov)

years <- c(2008, 2009, 2010, 2011, 2012, 2014, 2015)
variances <- c(0.000104, 0.000067, 0.00008, 0.000067, 0.000059, 0.000063, 0.000063)

# Specify the correlation coefficient
rho_value <- 0.8

# Construct the covariance matrix
cov_matrix <- construct_cov_matrix(years, variances, rho = rho_value)

# Display the covariance matrix
print(cov_matrix)
#> $cov_matrix
#>              [,1]         [,2]         [,3]         [,4]         [,5]
#> [1,] 1.040000e-04 6.677964e-05 7.297123e-05 6.677964e-05 6.266610e-05
#> [2,] 6.677964e-05 6.700000e-05 5.856962e-05 5.360000e-05 5.029831e-05
#> [3,] 7.297123e-05 5.856962e-05 8.000000e-05 5.856962e-05 5.496180e-05
#> [4,] 6.677964e-05 5.360000e-05 5.856962e-05 6.700000e-05 5.029831e-05
#> [5,] 6.266610e-05 5.029831e-05 5.496180e-05 5.029831e-05 5.900000e-05
#> [6,] 6.475554e-05 5.197538e-05 5.679437e-05 5.197538e-05 4.877376e-05
#> [7,] 6.475554e-05 5.197538e-05 5.679437e-05 5.197538e-05 4.877376e-05
#>              [,6]         [,7]
#> [1,] 6.475554e-05 6.475554e-05
#> [2,] 5.197538e-05 5.197538e-05
#> [3,] 5.679437e-05 5.679437e-05
#> [4,] 5.197538e-05 5.197538e-05
#> [5,] 4.877376e-05 4.877376e-05
#> [6,] 6.300000e-05 5.040000e-05
#> [7,] 5.040000e-05 6.300000e-05
#> 
#> $rho
#> [1] 0.8
```

**Explanation**:

- The diagonal elements are the variances.
- The off-diagonal elements are calculated using the specified rho and
  the standard deviations of the variances.
- This covariance matrix assumes a constant correlation of 0.8 between
  all pairs of years.

**Example 2: Constructing a Covariance Matrix with Exponential Decay**

The `construct_cov_matrix_decay` function creates a covariance matrix
where the correlation between years decays exponentially based on the
time difference and a decay parameter `lambda`.

``` r
# Specify the decay parameter
lambda_exp <- 0.5  # Controls the rate of exponential decay

# Construct the covariance matrix with exponential decay
cov_matrix_exp <- construct_cov_matrix_decay(years, variances, 
                                             decay_type = "exponential", 
                                             lambda = lambda_exp)

# Display the covariance matrix
print(cov_matrix_exp)
#>              1            2            3            4            5            6
#> 1 1.040000e-04 5.062987e-05 3.355577e-05 1.862569e-05 1.060117e-05 4.029986e-06
#> 2 5.062987e-05 6.700000e-05 4.440534e-05 2.464792e-05 1.402884e-05 5.332999e-06
#> 3 3.355577e-05 4.440534e-05 8.000000e-05 4.440534e-05 2.527415e-05 9.607852e-06
#> 4 1.862569e-05 2.464792e-05 4.440534e-05 6.700000e-05 3.813433e-05 1.449659e-05
#> 5 1.060117e-05 1.402884e-05 2.527415e-05 3.813433e-05 5.900000e-05 2.242858e-05
#> 6 4.029986e-06 5.332999e-06 9.607852e-06 1.449659e-05 2.242858e-05 6.300000e-05
#> 7 2.444310e-06 3.234627e-06 5.827457e-06 8.792628e-06 1.360362e-05 3.821143e-05
#>              7
#> 1 2.444310e-06
#> 2 3.234627e-06
#> 3 5.827457e-06
#> 4 8.792628e-06
#> 5 1.360362e-05
#> 6 3.821143e-05
#> 7 6.300000e-05
```

**Explanation**:

- The correlation between years decreases exponentially as the time
  difference increases.
- The lambda parameter controls how quickly the correlation decays. A
  larger lambda results in a faster decay.
- This covariance matrix reflects the assumption that observations
  closer in time are more strongly correlated.

**Example 3: Constructing a Covariance Matrix with Linear Decay**

Alternatively, you can use linear decay where the correlation decreases
linearly with time difference until it reaches zero.

``` r
# Specify the decay parameter
lambda_lin <- 0.2  # Controls the rate of linear decay

# Construct the covariance matrix with linear decay
cov_matrix_lin <- construct_cov_matrix_decay(years, variances, 
                                             decay_type = "linear", 
                                             lambda = lambda_lin)

# Display the covariance matrix
print(cov_matrix_lin)
#>              1            2            3            4            5            6
#> 1 1.040000e-04 6.677964e-05 5.472842e-05 3.338982e-05 1.566652e-05 0.000000e+00
#> 2 6.677964e-05 6.700000e-05 5.856962e-05 4.020000e-05 2.514916e-05 0.000000e+00
#> 3 5.472842e-05 5.856962e-05 8.000000e-05 5.856962e-05 4.122135e-05 1.419859e-05
#> 4 3.338982e-05 4.020000e-05 5.856962e-05 6.700000e-05 5.029831e-05 2.598769e-05
#> 5 1.566652e-05 2.514916e-05 4.122135e-05 5.029831e-05 5.900000e-05 3.658032e-05
#> 6 0.000000e+00 0.000000e+00 1.419859e-05 2.598769e-05 3.658032e-05 6.300000e-05
#> 7 0.000000e+00 0.000000e+00 0.000000e+00 1.299384e-05 2.438688e-05 5.040000e-05
#>              7
#> 1 0.000000e+00
#> 2 0.000000e+00
#> 3 0.000000e+00
#> 4 1.299384e-05
#> 5 2.438688e-05
#> 6 5.040000e-05
#> 7 6.300000e-05
```

**Explanation**:

- The correlation decreases linearly with the time difference.
- The lambda parameter controls the rate of decay. Here, a lambda of 0.2
  causes the correlation to reach zero for time differences greater than
  5 units.
- Notice that correlations are zero for years that are sufficiently far
  apart.
