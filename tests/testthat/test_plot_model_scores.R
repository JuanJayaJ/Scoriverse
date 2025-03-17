library(ggplot2)
library(testthat)

test_that("plot_model_scores returns a ggplot object for bar plot", {
  # Create dummy score results for testing
  score_results <- data.frame(
    model = c("lm", "glm"),
    metric = c("CRPS", "CRPS"),
    value = c(0.5, 0.6)
  )

  # Generate the plot using a bar chart
  p <- plot_model_scores(score_results, type = "bar")
  expect_s3_class(p, "ggplot")
})

test_that("plot_model_scores returns a ggplot object for heatmap", {
  score_results <- data.frame(
    model = c("lm", "glm"),
    metric = c("CRPS", "CRPS"),
    value = c(0.5, 0.6)
  )

  # Generate the plot using a heatmap
  p <- plot_model_scores(score_results, type = "heatmap")
  expect_s3_class(p, "ggplot")
})

test_that("plot_model_scores throws an error for unsupported type", {
  score_results <- data.frame(
    model = c("lm", "glm"),
    metric = c("CRPS", "CRPS"),
    value = c(0.5, 0.6)
  )

  # Expect an error if an unsupported plot type is provided
  expect_error(plot_model_scores(score_results, type = "unsupported"))
})
