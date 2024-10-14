test_that(
  "exponential() throws an error when given a string",
  {
    expect_error(exponential("1"))
  }
)

test_that(
  "exponential() given a warning when given a large n value",
  {
    expect_warning(exponential(3, n = 500))
  }
)

test_that(
  "exponential() produces equal output to exp()",
  {
    expect_equal(exponential(c(1, 2)), exp(c(1, 2)))
  }
)

test_that(
  "exponential() handles NA values equivalently to exp()",
  {
    expect_equal(exponential(c(3, NA)), exp(c(3, NA)))
  }
)

test_that(
  "exponential() throws an error when given a data frame",
  {
    test_df <- data.frame(rbind(c(1, 2), c(3, 4)))
    expect_error(exponential(test_df))
  }
)
