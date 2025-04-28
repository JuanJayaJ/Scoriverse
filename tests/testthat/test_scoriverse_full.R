library(testthat)
library(DraftScoriverse)
library(MASS)        # Needed for glm.nb
library(mgcv)        # Needed for GAM models

# -------------------------------------------------------------------
# Test: extract_additional_params
# -------------------------------------------------------------------

test_that("extract_additional_params extracts sigma for GLM Gaussian", {
  model <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian)
  params <- extract_additional_params(model)
  expect_true(is.list(params))
  expect_true(!is.null(params$sigma))
})

test_that("extract_additional_params extracts phi for GLM Negative Binomial", {
  set.seed(123)
  count_data <- data.frame(count = rnbinom(50, size = 2, mu = 10), x = rnorm(50))
  model_nb <- MASS::glm.nb(count ~ x, data = count_data)
  params <- extract_additional_params(model_nb)
  expect_true(is.list(params))
  expect_true(!is.null(params$phi))
})

test_that("extract_additional_params returns empty list for unsupported model", {
  dummy_model <- list()
  expect_equal(extract_additional_params(dummy_model), list())
})

# -------------------------------------------------------------------
# Test: wrap_predict and prepare_model_for_prediction
# -------------------------------------------------------------------

test_that("wrap_predict and prepare_model_for_prediction return correct predictions", {
  model <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian)
  preds_wrap <- wrap_predict(model, new_data = mtcars)
  preds_prepare <- prepare_model_for_prediction(model, new_data = mtcars)
  expect_true(is.numeric(preds_wrap))
  expect_equal(preds_wrap, preds_prepare)
})

# -------------------------------------------------------------------
# Test: Outcome-Scale Sampling (return_samples = TRUE)
# -------------------------------------------------------------------

test_that("extract_predictions returns outcome-scale samples for GLM Poisson", {
  set.seed(123)
  count_data <- data.frame(count = rpois(50, lambda = 10), x = rnorm(50))
  model <- glm(count ~ x, data = count_data, family = "poisson")

  draws <- extract_predictions(model, new_data = count_data, return_samples = TRUE, n_samples = 100)
  expect_true(is.matrix(draws))
  expect_equal(ncol(draws), nrow(count_data))
  expect_equal(nrow(draws), 100)
})

test_that("extract_predictions returns outcome-scale samples for GLM Negative Binomial", {
  set.seed(123)
  count_data <- data.frame(count = rnbinom(50, size = 2, mu = 10), x = rnorm(50))
  model_nb <- MASS::glm.nb(count ~ x, data = count_data)

  draws <- extract_predictions(model_nb, new_data = count_data, return_samples = TRUE, n_samples = 100)
  expect_true(is.matrix(draws))
  expect_equal(ncol(draws), nrow(count_data))
  expect_equal(nrow(draws), 100)
})

test_that("extract_predictions returns outcome-scale samples for GLM Gaussian", {
  set.seed(123)
  model_gaussian <- glm(mpg ~ wt + hp, family = gaussian, data = mtcars)

  draws <- extract_predictions(model_gaussian, new_data = mtcars, return_samples = TRUE, n_samples = 100)
  expect_true(is.matrix(draws))
  expect_equal(ncol(draws), nrow(mtcars))
  expect_equal(nrow(draws), 100)
})

test_that("extract_predictions returns outcome-scale samples for GAM Poisson", {
  set.seed(123)
  count_data <- data.frame(count = rpois(50, lambda = 10), x = rnorm(50))
  model_gam <- mgcv::gam(count ~ s(x), family = poisson, data = count_data)

  draws <- extract_predictions(model_gam, new_data = count_data, return_samples = TRUE, n_samples = 100)
  expect_true(is.matrix(draws))
  expect_equal(ncol(draws), nrow(count_data))
  expect_equal(nrow(draws), 100)
})

test_that("extract_predictions returns outcome-scale samples for GAM Gaussian", {
  set.seed(123)
  model_gam_gaussian <- mgcv::gam(mpg ~ s(wt), family = gaussian, data = mtcars)

  draws <- extract_predictions(model_gam_gaussian, new_data = mtcars, return_samples = TRUE, n_samples = 100)
  expect_true(is.matrix(draws))
  expect_equal(ncol(draws), nrow(mtcars))
  expect_equal(nrow(draws), 100)
})

# -------------------------------------------------------------------
# Test: Scoring Functions (Direct Metric Calculation)
# -------------------------------------------------------------------

test_that("scoring functions compute correctly", {
  y_true <- c(2, 4, 6)
  pred_mean <- c(2.1, 3.9, 6.2)
  pred_sd <- c(1, 1, 1)
  pred_matrix <- matrix(rnorm(300, mean = 5, sd = 1), nrow = 3)

  expect_equal(length(compute_crps(y_true, pred_mean = pred_mean, pred_sd = pred_sd)), length(y_true))
  expect_equal(length(compute_crps(y_true, pred_matrix = pred_matrix)), length(y_true))
  expect_equal(length(compute_log_score(y_true, pred_mean, pred_sd)), length(y_true))
  expect_equal(length(compute_dss(y_true, pred_mean, pred_sd)), length(y_true))

  y_bin <- c(0, 1, 0, 1)
  pred_prob <- c(0.2, 0.8, 0.3, 0.9)
  expect_equal(compute_brier_score(y_bin, pred_prob), (pred_prob - y_bin)^2)

  y_int <- c(10, 20, 30)
  lower <- c(9, 19, 29)
  upper <- c(11, 21, 31)
  alpha <- 0.05
  interval_scores <- compute_interval_score(y_int, lower, upper, alpha)
  expect_equal(length(interval_scores), length(y_int))
})

# -------------------------------------------------------------------
# Test: wrap_scoring Error Handling and Correct Outputs
# -------------------------------------------------------------------

test_that("wrap_scoring handles missing arguments and computes scores correctly", {
  model <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian)
  preds <- wrap_predict(model, new_data = mtcars)
  additional_params <- attr(preds, "additional_params")

  expect_error(wrap_scoring("crps", y_true = mtcars$mpg, predictions = preds),
               regexp = "Standard deviation \\('pred_sd'\\) is required")

  if (!is.null(additional_params$sigma)) {
    expect_true(is.numeric(wrap_scoring("crps", y_true = mtcars$mpg, predictions = preds, pred_sd = additional_params$sigma)))
    expect_true(is.numeric(wrap_scoring("log_score", y_true = mtcars$mpg, predictions = preds, pred_sd = additional_params$sigma)))
    expect_true(is.numeric(wrap_scoring("dss", y_true = mtcars$mpg, predictions = preds, pred_sd = additional_params$sigma)))
  }

  pred_prob <- (preds - min(preds)) / (max(preds) - min(preds))
  y_bin <- ifelse(mtcars$mpg > median(mtcars$mpg), 1, 0)
  expect_true(is.numeric(wrap_scoring("brier", y_true = y_bin, predictions = preds, pred_prob = pred_prob)))
  expect_true(is.numeric(wrap_scoring("interval", y_true = mtcars$mpg, predictions = preds,
                                      lower = preds - 1, upper = preds + 1)))
})
