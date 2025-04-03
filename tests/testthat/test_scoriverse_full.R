library(testthat)
library(DraftScoriverse)

# -------------------------------------------------------------------
# Test Prediction Extraction and Model Preparation
# -------------------------------------------------------------------

test_that("wrap_predict returns numeric predictions for an lm model", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)
  preds <- wrap_predict(model, new_data = mtcars)
  
  expect_true(is.numeric(preds))
  expect_equal(length(preds), nrow(mtcars))
  
  # Check that additional parameters (like sigma) were attached
  additional_params <- attr(preds, "additional_params")
  expect_true(is.list(additional_params))
  expect_true(!is.null(additional_params$sigma))
})

test_that("prepare_model_for_prediction behaves the same as wrap_predict", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)
  preds_direct <- wrap_predict(model, new_data = mtcars)
  preds_prepared <- prepare_model_for_prediction(model, new_data = mtcars)
  
  expect_equal(preds_direct, preds_prepared)
})

# -------------------------------------------------------------------
# Test Visualization Functions
# -------------------------------------------------------------------

test_that("visualize_predictions returns a ggplot object", {
  p <- visualize_predictions(y_true = mtcars$mpg, predictions = mtcars$mpg)
  expect_true(inherits(p, "ggplot"))
})

test_that("visualize_residuals returns a ggplot object", {
  p <- visualize_residuals(y_true = mtcars$mpg, predictions = mtcars$mpg)
  expect_true(inherits(p, "ggplot"))
})

# -------------------------------------------------------------------
# Tests for Scoring Functions - Directly Testing Each Metric
# -------------------------------------------------------------------

# Prepare sample data for scoring tests
y_sc       <- c(2, 4, 6)
pred_mean  <- c(2.1, 3.9, 6.2)
pred_sd    <- c(1, 1, 1)
# Create a sample matrix with 3 rows and 100 samples per row
set.seed(123)
pred_matrix <- matrix(rnorm(300, mean = 5, sd = 1), nrow = 3)

test_that("compute_crps works with pred_mean and pred_sd", {
  crps_vals <- compute_crps(y_sc, pred_mean = pred_mean, pred_sd = pred_sd)
  expect_true(is.numeric(crps_vals))
  expect_equal(length(crps_vals), length(y_sc))
})

test_that("compute_crps works with a pred_matrix", {
  crps_vals <- compute_crps(y_sc, pred_matrix = pred_matrix)
  expect_true(is.numeric(crps_vals))
  expect_equal(length(crps_vals), length(y_sc))
})

test_that("compute_log_score returns numeric values", {
  log_scores <- compute_log_score(y_sc, pred_mean, pred_sd)
  expect_true(is.numeric(log_scores))
  expect_equal(length(log_scores), length(y_sc))
})

test_that("compute_brier_score returns correct squared differences", {
  # Binary classification example.
  y_bin      <- c(0, 1, 0, 1)
  pred_prob  <- c(0.2, 0.8, 0.3, 0.9)
  brier_vals <- compute_brier_score(y_bin, pred_prob)
  expected   <- (pred_prob - y_bin)^2
  expect_equal(brier_vals, expected)
})

test_that("compute_interval_score calculates correct scores", {
  y_int    <- c(10, 20, 30)
  lower_int <- c(9, 19, 29)
  upper_int <- c(11, 21, 31)
  alpha    <- 0.05
  
  # Calculate expected interval width and penalties manually.
  interval_width <- upper_int - lower_int
  penalty <- 2 / alpha * (lower_int - y_int) * (y_int < lower_int) +
    2 / alpha * (y_int - upper_int) * (y_int > upper_int)
  expected_scores <- interval_width + penalty
  
  computed_scores <- compute_interval_score(y_int, lower_int, upper_int, alpha)
  expect_equal(computed_scores, expected_scores)
})

test_that("compute_dss returns numeric values", {
  dss_vals <- compute_dss(y_sc, pred_mean, pred_sd)
  expect_true(is.numeric(dss_vals))
  expect_equal(length(dss_vals), length(y_sc))
})

# -------------------------------------------------------------------
# Tests for wrap_scoring Function Across All Metrics
# -------------------------------------------------------------------

test_that("wrap_scoring errors when required parameters missing", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)
  preds <- wrap_predict(model)
  
  # For CRPS, pred_sd is mandatory.
  expect_error(
    wrap_scoring("crps", y_true = mtcars$mpg, predictions = preds),
    regexp = "Standard deviation \\('pred_sd'\\) must be provided for this score computation."
  )
})

test_that("wrap_scoring returns correct outputs for each metric", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)
  preds <- wrap_predict(model)
  additional_params <- attr(preds, "additional_params")
  
  # For CRPS, log score, and DSS, use the sigma from additional parameters.
  if (!is.null(additional_params$sigma)) {
    crps_out <- wrap_scoring("crps", y_true = mtcars$mpg, predictions = preds,
                             pred_sd = additional_params$sigma)
    expect_true(is.numeric(crps_out))
    
    log_score_out <- wrap_scoring("log_score", y_true = mtcars$mpg, predictions = preds,
                                  pred_sd = additional_params$sigma)
    expect_true(is.numeric(log_score_out))
    
    dss_out <- wrap_scoring("dss", y_true = mtcars$mpg, predictions = preds,
                            pred_sd = additional_params$sigma)
    expect_true(is.numeric(dss_out))
  }
  
  # For Brier: Create dummy binary outcomes and normalized predicted probabilities.
  pred_prob <- preds
  # Normalize to [0,1] (this is only for testing)
  pred_prob <- (pred_prob - min(pred_prob)) / (max(pred_prob) - min(pred_prob))
  y_bin <- ifelse(mtcars$mpg > median(mtcars$mpg), 1, 0)
  brier_out <- wrap_scoring("brier", y_true = y_bin, predictions = preds, pred_prob = pred_prob)
  expect_true(is.numeric(brier_out))
  
  # For Interval score: use dummy lower and upper bounds.
  lower_bound <- preds - 1
  upper_bound <- preds + 1
  interval_out <- wrap_scoring("interval", y_true = mtcars$mpg, predictions = preds,
                               lower = lower_bound, upper = upper_bound)
  expect_true(is.numeric(interval_out))
})
