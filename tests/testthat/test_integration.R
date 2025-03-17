library(testthat)
library(ggplot2)

test_that("complete workflow runs without error", {
  model <- lm(mpg ~ wt + hp, data = mtcars)

  # Predict using the unified function
  preds <- predict_unified(model, newdata = mtcars, level = 0.95)
  expect_true(is.data.frame(preds))
  expect_true(all(c("estimate", "std_error", "lower", "upper") %in% names(preds)))

  # Score the model
  scores <- score_model(model, newdata = mtcars, truth = mtcars$mpg, metrics = c("crps"))
  expect_true(is.data.frame(scores))
  expect_true(nrow(scores) > 0)

  # Generate a visualization and check the output
  p <- plot_model_scores(scores, type = "bar")
  expect_s3_class(p, "ggplot")
})
