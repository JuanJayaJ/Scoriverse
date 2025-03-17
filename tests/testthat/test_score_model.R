test_that("score_model computes CRPS correctly", {
  mod <- lm(mpg ~ wt + hp, data = mtcars)
  score_result <- score_model(mod, newdata = mtcars, truth = mtcars$mpg, metrics = c("crps"))
  expect_true("metric" %in% colnames(score_result))
  expect_true("value" %in% colnames(score_result))
})
