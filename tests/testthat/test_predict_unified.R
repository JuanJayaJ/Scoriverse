test_that("predict_unified.lm returns expected columns", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  preds <- predict_unified(model, newdata = mtcars, level = 0.95)
  expected_cols <- c("estimate", "std_error", "lower", "upper")
  expect_true(all(expected_cols %in% names(preds)))
})
