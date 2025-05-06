library(testthat)
library(DraftScoriverse)
library(MASS)
library(mgcv)

# ────────────────────────────────────────────────
# Test: extract_additional_params
# ────────────────────────────────────────────────
test_that("extract_additional_params returns sigma/phi for GLM and NB models", {
  gaussian_model <- glm(mpg ~ wt, data = mtcars, family = gaussian)
  nb_model <- MASS::glm.nb(count ~ x, data = data.frame(count = rnbinom(100, mu = 10, size = 2), x = rnorm(100)))
  dummy_model <- list()

  expect_true(!is.null(extract_additional_params(gaussian_model)$sigma))
  expect_true(!is.null(extract_additional_params(nb_model)$phi))
  expect_equal(extract_additional_params(dummy_model), list())
})

# ────────────────────────────────────────────────
# Test: wrap_predict and prepare_model_for_prediction
# ────────────────────────────────────────────────
test_that("wrap_predict and prepare_model_for_prediction give consistent output", {
  model <- glm(mpg ~ wt + hp, data = mtcars)
  preds1 <- wrap_predict(model, new_data = mtcars)
  preds2 <- prepare_model_for_prediction(model, new_data = mtcars)

  expect_equal(preds1, preds2)
  expect_true(is.numeric(preds1))
})

# ────────────────────────────────────────────────
# Test: extract_predictions (point vs sample) for GLM
# ────────────────────────────────────────────────
test_that("extract_predictions returns numeric point estimates for GLM", {
  model <- glm(mpg ~ wt, data = mtcars)
  preds <- extract_predictions(model, new_data = mtcars, return_samples = FALSE)
  expect_true(is.numeric(preds))
  expect_equal(length(preds), nrow(mtcars))
})

test_that("extract_predictions returns matrix of outcome-scale samples for Poisson GLM", {
  set.seed(42)
  pois_model <- glm(count ~ x, data = data.frame(count = rpois(100, 10), x = rnorm(100)), family = poisson)
  draws <- extract_predictions(pois_model, new_data = data.frame(x = rnorm(100)), return_samples = TRUE, n_samples = 50)
  expect_true(is.matrix(draws))
  expect_equal(nrow(draws), 50)
})

# ────────────────────────────────────────────────
# Test: extract_predictions with GAM
# ────────────────────────────────────────────────
test_that("extract_predictions returns draws matrix for GAM (gratia)", {
  gam_model <- mgcv::gam(mpg ~ s(wt), data = mtcars, family = gaussian)
  draws <- extract_predictions(gam_model, new_data = mtcars, return_samples = TRUE, n_samples = 20)
  expect_true(is.matrix(draws))
  expect_equal(ncol(draws), nrow(mtcars))
})

# ────────────────────────────────────────────────
# Test: wrap_scoring across different metrics
# ────────────────────────────────────────────────
test_that("wrap_scoring computes all supported metrics correctly", {
  model <- glm(mpg ~ wt + hp, data = mtcars)
  preds <- wrap_predict(model, new_data = mtcars)
  sigma <- attr(preds, "additional_params")$sigma

  expect_error(wrap_scoring("crps", mtcars$mpg, preds))  # missing SD
  expect_true(is.numeric(wrap_scoring("crps", mtcars$mpg, preds, pred_sd = sigma)))
  expect_true(is.numeric(wrap_scoring("log_score", mtcars$mpg, preds, pred_sd = sigma)))
  expect_true(is.numeric(wrap_scoring("dss", mtcars$mpg, preds, pred_sd = sigma)))

  y_bin <- ifelse(mtcars$mpg > median(mtcars$mpg), 1, 0)
  prob_pred <- (preds - min(preds)) / (max(preds) - min(preds))
  expect_true(is.numeric(wrap_scoring("brier", y_bin, preds, pred_prob = prob_pred)))
  expect_true(is.numeric(wrap_scoring("interval", mtcars$mpg, preds, lower = preds - 1, upper = preds + 1)))
})

# ────────────────────────────────────────────────
# Test: run_scoriverse full pipeline
# ────────────────────────────────────────────────
test_that("run_scoriverse executes full pipeline with all arguments", {
  model <- glm(mpg ~ wt + hp, data = mtcars)
  preds <- wrap_predict(model, new_data = mtcars)
  params <- attr(preds, "additional_params")
  sigma <- rep(params$sigma, length(preds))  # ensure vector format

  prob_pred <- (preds - min(preds)) / (max(preds) - min(preds))

  results <- run_scoriverse(
    model,
    new_data = mtcars,
    y_true = mtcars$mpg,
    visualize = TRUE,
    pred_sd = sigma,
    pred_prob = prob_pred,
    lower = preds - 1,
    upper = preds + 1
  )

  expect_true(is.list(results))
  expect_true("scores" %in% names(results))
  expect_length(results$scores, 5)
})

# ────────────────────────────────────────────────
# Test: fallback sampling for GLM and LM
# ────────────────────────────────────────────────
test_that("extract_predictions returns samples for GLM and LM using manual RNG", {
  glm_model <- glm(mpg ~ wt, data = mtcars)
  lm_model  <- lm(mpg ~ hp, data = mtcars)

  glm_draws <- extract_predictions(glm_model, new_data = mtcars, return_samples = TRUE, n_samples = 10)
  lm_draws  <- extract_predictions(lm_model, new_data = mtcars, return_samples = TRUE, n_samples = 10)

  expect_true(is.matrix(glm_draws))
  expect_true(is.matrix(lm_draws))
  expect_equal(ncol(glm_draws), nrow(mtcars))
  expect_equal(ncol(lm_draws), nrow(mtcars))
})
