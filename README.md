
<!-- README.md is generated from README.Rmd. Please edit that file -->

# exponential (Package Description)

<!-- badges: start -->
<!-- badges: end -->

The `exponential()` function in this eponymously named package computes
the exponential of a number, vector of numbers, or a (numeric) variable
in a tibble. This is done recursively, via a truncated approximation of
the Taylor-series definition of the exponential function. This function
can be used to reverse logarithmic transformations of numerical
variables, as shown in the last example below.

## Installation

You can install the development version of exponential like so:

``` r
# install.packages("devtools") # only if not already installed
devtools::install_github("jsa378/exponential", ref = "0.1.0")
```

## Examples

These are basic examples of the use of the `exponential()` function. We
apply `exponential()` to a single number, a vector of numbers, a vector
of numbers and `NA` values, and a numerical variable in a tibble.

``` r
library(exponential)

exponential(1) # exponentiate a number
#> [1] 2.718282

exponential(c(1, 2)) # exponentiate a vector of numbers
#> [1] 2.718282 7.389056

exponential(c(3, NA)) # the function accepts NA inputs
#> [1] 20.08554       NA

# Next, we apply the function to a numerical variable
# in a tibble

# We use a dataset of trees from the datateachr package,
# and apply exponential to the logarithm of the tree diameters
library(datateachr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# Filter out observations with 0 diameter,
# arrange by diameter (decreasing),
# select diameter column,
# create log_diam and exp_log_diam
log_diam_dataset <- vancouver_trees |>
  filter(diameter > 0) |>
  arrange(desc(diameter)) |>
  select(diameter) |>
  mutate(
    log_diam = log(diameter),
    exp_log_diam = exponential(log_diam)
  )

# View log_diam_dataset
log_diam_dataset
#> # A tibble: 146,519 × 3
#>    diameter log_diam exp_log_diam
#>       <dbl>    <dbl>        <dbl>
#>  1      435     6.08         435.
#>  2      317     5.76         317.
#>  3      305     5.72         305.
#>  4      182     5.20         182.
#>  5      161     5.08         161.
#>  6      156     5.05         156.
#>  7      151     5.02         151.
#>  8      144     4.97         144.
#>  9      141     4.95         141.
#> 10      131     4.88         131.
#> # ℹ 146,509 more rows

# Check that the diameter
# and exp_log_diameter columns
# are equal
tibble_1 <- log_diam_dataset |> select(diameter)
tibble_2 <- log_diam_dataset |> select(exp_log_diam)
all.equal(tibble_1, tibble_2, check.attributes = FALSE)
#> [1] TRUE
```

For further guidance regarding how to use the `exponential()` function,
please install the package and see the `exponential()` function
documentation.
