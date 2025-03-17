library(testthat)

test_that("apply_link_transform returns the correct inverse transformation", {
  # Assume apply_link_transform is implemented to support "log" and "logit"
  original_values <- c(1, 2, 3)
  # For log link, inverse is exp()
  transformed <- apply_link_transform(original_values, link_function = "log", inverse = TRUE)
  expect_equal(transformed, exp(original_values))
})
