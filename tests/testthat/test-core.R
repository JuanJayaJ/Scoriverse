library(testthat)
library(Scoriverse)
library(mgcv)        # for gam
library(MASS)        # for glm.nb

# Test for wrap_predict() with lm (Gaussian)
test_that("wrap_predict extracts predictions from lm model", {
  model <- lm(mpg ~ wt, data = mtcars)
  preds <- wrap_predict(model, new_data = mtcars, type = "response")
  expect_type(preds, "double")
  expect_equal(length(preds), nrow(mtcars))
})

test_that("wrap_predict fallback works with generic predict", {
  model <- lm(mpg ~ wt, data = mtcars)
  preds <- wrap_predict(model, type = "response")
  expect_type(preds, "double")
  expect_equal(length(preds), nrow(mtcars))
})

# Test for wrap_scoring() with CRPS
test_that("wrap_scoring computes CRPS correctly", {
  model <- lm(mpg ~ wt, data = mtcars)
  preds <- wrap_predict(model, new_data = mtcars, type = "response")
  y_true <- mtcars$mpg
  pred_sd <- rep(sd(residuals(model)), length(preds))
  crps_vals <- wrap_scoring("crps", y_true, preds, pred_sd = pred_sd)
  expect_type(crps_vals, "double")
  expect_equal(length(crps_vals), length(y_true))
})

# Test for wrap_scoring errors when required arguments are missing
test_that("wrap_scoring errors when required arguments are missing", {
  model <- lm(mpg ~ wt, data = mtcars)
  preds <- wrap_predict(model, new_data = mtcars, type = "response")
  y_true <- mtcars$mpg
  expect_error(wrap_scoring("crps", y_true, preds))
  expect_error(wrap_scoring("brier", y_true, preds))
})

# Test for run_scoriverse() workflow
test_that("run_scoriverse returns a list with predictions, scores, and plot", {
  model <- lm(mpg ~ wt, data = mtcars)
  y_true <- mtcars$mpg
  pred_sd <- rep(sd(residuals(model)), nrow(mtcars))
  result <- run_scoriverse(
    model = model,
    new_data = mtcars,
    y_true = y_true,
    score_metrics = c("crps", "log_score", "dss"),
    pred_sd = pred_sd,
    visualize = TRUE,
    type = "response"
  )
  expect_true(is.list(result))
  expect_true(all(c("predictions", "scores", "plot") %in% names(result)))
  expect_type(result$predictions, "double")
  expect_true(is.list(result$scores))
  expect_true(inherits(result$plot, "ggplot"))
})

# Test for visualization functions
test_that("visualize_predictions returns a ggplot object", {
  y_true <- mtcars$mpg
  preds <- y_true + rnorm(nrow(mtcars))
  p <- visualize_predictions(new_data = mtcars, y_true = y_true, predictions = preds)
  expect_true(inherits(p, "ggplot"))
})

test_that("visualize_residuals returns a ggplot object", {
  y_true <- mtcars$mpg
  preds <- y_true + rnorm(nrow(mtcars))
  p <- visualize_residuals(y_true = y_true, predictions = preds)
  expect_true(inherits(p, "ggplot"))
})

# --- Extended tests for convert_to_outcome_scale() functionality ---

# Test for GAM with Poisson family conversion
test_that("convert_to_outcome_scale converts GAM Poisson predictions", {
  # Fit a GAM with Poisson family
  gam_model <- mgcv::gam(count ~ s(age), family = poisson,
                         data = data.frame(count = rpois(100, 5), age = runif(100, 1, 10)))
  # Get predictions (on the response scale)
  pred_exp <- predict(gam_model, type = "response")
  # Convert to outcome scale by explicitly passing "poisson"
  pred_outcome <- convert_to_outcome_scale(pred_exp, "poisson")
  expect_true(all(pred_outcome >= 0))
  expect_length(pred_outcome, length(pred_exp))
})

# Test for GLM with Negative Binomial conversion using glm.nb from MASS
test_that("convert_to_outcome_scale converts NB predictions", {
  # Fit a negative binomial model
  nb_model <- MASS::glm.nb(count ~ age,
                           data = data.frame(count = rnbinom(100, mu = 5, size = 2),
                                             age = runif(100, 1, 10)))
  pred_exp <- predict(nb_model, type = "response")
  # Convert to outcome scale by explicitly passing "negbin" and extra argument theta
  pred_outcome <- convert_to_outcome_scale(pred_exp, "negbin", theta = nb_model$theta)
  expect_true(all(pred_outcome >= 0))
  expect_length(pred_outcome, length(pred_exp))
})

# Test for GLM with Binomial conversion
test_that("convert_to_outcome_scale converts Binomial predictions", {
  # Fit a binomial glm model
  bin_model <- glm(cbind(success, trials - success) ~ age,
                   data = data.frame(success = rbinom(100, 1, 0.5),
                                     trials = rep(1, 100),
                                     age = runif(100, 1, 10)),
                   family = binomial)
  pred_prob <- predict(bin_model, type = "response")
  # Convert to outcome scale by explicitly passing "binomial"
  pred_outcome <- convert_to_outcome_scale(pred_prob, "binomial")
  # Since the model already returns probabilities, the output should equal the input.
  expect_equal(pred_outcome, pred_prob)
  expect_length(pred_outcome, length(pred_prob))
})
