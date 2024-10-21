#' Exponentiate numbers
#'
#' @title Exponentiate real numbers
#'
#' @description This function computes the exponential of a number,
#'   vector of numbers, or a (numeric) variable in a tibble.
#'   This is done recursively, via a truncated approximation
#'   of the Taylor-series definition of the exponential function.
#'   This function can be used to reverse logarithmic transformations
#'   of numerical variables, as shown in the last example below.
#'
#' @param x The number(s) to exponentiate. This can be a number,
#'   a vector of numbers, or a (numeric) variable in a tibble.
#'   The name was chosen because it is standard for real numbers.
#' @param n The number of terms to include in the series approximation.
#'   This must be a whole number at least zero.
#'   The default is 20; smaller numbers may lead to poor accuracy,
#'   while numbers over 100 may lead to numerical problems.
#'   The name was chosen because it is standard for integers.
#'
#' @return The number, vector of numbers or tibble column,
#'   where each number has been exponentiated.
#' @export
#'
#' @examples
#' # Exponential of a single number
#' exponential(1)
#' exponential(2)
#'
#' # If the `n` parameter is set too low, the function is inaccurate
#' exponential(1, n = 5)
#'
#' # Exponential of a vector of numbers
#' exponential(c(1, 2))
#'
#' # NA values are handled without error
#' exponential(c(3, NA))
#'
#' # Exponential of a numerical variable in a tibble
#' # (First we load necessary libraries)
#' library(dplyr)
#' library(datateachr)
#'
#' # Take the logarithm of the diameter variable,
#' # then take the exponential of log_diam variable
#' log_diam_dataset <- vancouver_trees |>
#'   filter(diameter > 0) |>
#'   arrange(desc(diameter)) |>
#'   select(diameter) |>
#'   mutate(
#'     log_diam = log(diameter),
#'     exp_log_diam = exponential(log_diam)
#'   )
#'
#' # View log_diam_dataset
#' log_diam_dataset
#'
#' # Check that the diameter
#' # and exp_log_diameter columns
#' # are equal
#' tibble_1 <- log_diam_dataset |> select(diameter)
#' tibble_2 <- log_diam_dataset |> select(exp_log_diam)
#' all.equal(tibble_1, tibble_2, check.attributes = FALSE)
exponential <- function(x, n = 20) {
  # We check that the inputs to the function
  # are appropriate

  # First we check that the x input
  # is either numeric or NA
  # (anything else will throw an error)
  if (!(is.numeric(x) || any(is.na(x)))) {
    # stop("`x` must include only numerical and NA values.")
    # rlang::abort("`x` must include only numerical and NA values.")
    cli::cli_abort("`x` must include only numerical and NA values.")
  }

  # Next, we check that n
  # is a whole number
  # and non-negative
  if (n < 0 || as.integer(n) != n) {
    # stop("`n` must be a whole number at least zero.")
    # rlang::abort("`n` must be a whole number at least zero.")
    cli::cli_abort("`n` must be a whole number at least zero.")
  }

  # Lastly, we warn the user
  # that large n values
  # can cause numerical problems
  if (n > 100 && n <= 101) {
    # warning("`n` is very large; check output carefully for NaN values.")
    # rlang::warn("`n` is very large; check output carefully for NaN values.")
    cli::cli_warn("`n` is very large; check output carefully for NaN values.")
  }

  # Having passed the above,
  # we are ready to compute exponential(x, n)

  # First we define the base case,
  # then the recursive step
  if (n == 0) {
    return(1)
  } else {
    nth_term <- x^n / factorial(n)
    return(nth_term + exponential(x, n - 1))
  }
}
