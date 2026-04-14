test_that("col mean works", {
  df <- data.frame(a = 1:3, b = 4:6, c = 6:8)
  expect_equal(col_summary(df, "mean"), c("a" = 2, "b" = 5, "c" = 7))
})

test_that("col min works", {
  df <- data.frame(a = 1:3, b = 4:6, c = 6:8)
  expect_equal(col_summary(df, "min"), c("a" = 1, "b" = 4, "c" = 6))
})

# TODO Insert test for col max & sd here


# Tests for errors
test_that("invalid df input errors", {
  expect_error(col_summary(1), regexp = "df needs to be")
  expect_error(col_summary(c(1, 2)))
  expect_error(col_summary(Inf))
  expect_error(col_summary(NA))
  expect_error(col_summary(NULL))
  expect_error(col_summary(data.frame(a = c(1, 2), b = c(NA, 30))), regexp = "Please remove NAs")
})


test_that("sd not computed if any column has one unique value only", {
  df <- data.frame(c(1, 2), c(1, 1))
  expect_error(col_summary(df, "sd"))
})
