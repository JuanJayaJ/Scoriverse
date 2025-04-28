#' Extract Posterior Predictive Draws from a Model
#'
#' Retrieves posterior predictive draws from a Bayesian model.
#'
#' @param model A fitted Bayesian model (e.g., brmsfit).
#' @param new_data A data frame of new observations.
#' @param draws Number of posterior draws to return (default: 1000).
#'
#' @return A matrix of posterior predictive samples. Rows = draws, columns = observations.
#' @export
get_posterior_draws <- function(model, new_data, draws = 1000) {
  if (!inherits(model, "brmsfit")) {
    stop("Posterior draw extraction is only supported for 'brmsfit' models.")
  }
  brms::posterior_predict(model, newdata = new_data, draws = draws)
}

# ----------------------------
# Internal Helper: Validate Inputs
# ----------------------------
.validate_extract_inputs <- function(model, new_data, return_samples, n_samples) {
  if (is.null(model)) stop("`model` must be provided.")
  if (!is.logical(return_samples)) stop("`return_samples` must be TRUE or FALSE.")
  if (!is.numeric(n_samples) || n_samples <= 0) stop("`n_samples` must be a positive integer.")
}

# ----------------------------
# Extract Predictions from Diverse Model Objects
# ----------------------------
#' Extract Predictions from Diverse Model Objects
#'
#' Extracts prediction estimates from a fitted model object using appropriate methods.
#' Supports outcome-scale sampling using random number generators when `return_samples = TRUE`.
#'
#' @param model A fitted model object.
#' @param new_data Optional data frame with new observations.
#' @param return_samples Logical. If TRUE, return posterior samples or draws; if FALSE, return point estimates (default).
#' @param n_samples Number of posterior samples to draw (if return_samples = TRUE).
#' @param ... Additional arguments for underlying prediction functions.
#'
#' @return A numeric vector of predictions or matrix of posterior samples (depending on `return_samples`).
#' @export
extract_predictions <- function(model, new_data = NULL, return_samples = FALSE, n_samples = 1000, ...) {
  .validate_extract_inputs(model, new_data, return_samples, n_samples)
  dots <- list(...)
  scoring_args <- c("pred_sd", "pred_prob", "lower", "upper")
  filtered_args <- dots[!names(dots) %in% scoring_args]
  model_classes <- class(model)

  # ---------------------------
  # brmsfit Handling
  # ---------------------------
  if ("brmsfit" %in% model_classes) {
    preds <- if (return_samples) {
      get_posterior_draws(model, new_data = new_data, draws = n_samples)
    } else {
      colMeans(get_posterior_draws(model, new_data = new_data, draws = n_samples))
    }
    return(preds)
  }

  # ---------------------------
  # GAM Handling (Manual Outcome-Scale Sampling)
  # ---------------------------
  if ("gam" %in% model_classes) {
    fam <- family(model)$family
    pred_mean <- predict(model, newdata = new_data, type = "response")

    if (return_samples) {
      sampled_values <- if (grepl("poisson", fam, ignore.case = TRUE)) {
        rpois(n = n_samples * length(pred_mean), lambda = rep(pred_mean, each = n_samples))
      } else if (grepl("gaussian", fam, ignore.case = TRUE)) {
        sigma <- sqrt(sum(residuals(model)^2) / df.residual(model))
        rnorm(n = n_samples * length(pred_mean), mean = rep(pred_mean, each = n_samples), sd = sigma)
      } else if (grepl("Negative Binomial", fam, ignore.case = TRUE)) {
        params <- extract_additional_params(model)
        if (is.null(params$phi)) stop("Negative Binomial: overdispersion parameter 'phi' not available.")
        rnbinom(n = n_samples * length(pred_mean), size = params$phi, mu = rep(pred_mean, each = n_samples))
      } else {
        stop("Sampling not implemented for GAM family: ", fam)
      }

      outcome_draws <- matrix(sampled_values, nrow = n_samples, byrow = TRUE)
      return(outcome_draws)
    } else {
      return(pred_mean)
    }
  }

  # ---------------------------
  # GLM Handling (Fixed Gaussian logic, Negative Binomial detection)
  # ---------------------------
  if (inherits(model, "glm")) {
    fam <- family(model)$family
    pred_mean <- predict(model, new_data = new_data, type = "response")

    if (return_samples) {
      sampled_values <- if (grepl("poisson", fam, ignore.case = TRUE)) {
        rpois(n = n_samples * length(pred_mean), lambda = rep(pred_mean, each = n_samples))
      } else if (grepl("gaussian", fam, ignore.case = TRUE)) {
        # Robust sigma extraction logic
        dispersion <- summary(model)$dispersion
        sigma <- if (!is.null(dispersion)) {
          sqrt(dispersion)
        } else {
          sqrt(mean(residuals(model)^2))  # fallback if dispersion is missing
        }
        rnorm(n = n_samples * length(pred_mean), mean = rep(pred_mean, each = n_samples), sd = sigma)
      } else if (grepl("Negative Binomial", fam, ignore.case = TRUE)) {
        params <- extract_additional_params(model)
        if (is.null(params$phi)) stop("Negative Binomial: overdispersion parameter 'phi' not available.")
        rnbinom(n = n_samples * length(pred_mean), size = params$phi, mu = rep(pred_mean, each = n_samples))
      } else {
        stop("Sampling not implemented for GLM family: ", fam)
      }

      outcome_draws <- matrix(sampled_values, nrow = n_samples, ncol = length(pred_mean), byrow = TRUE)
      return(outcome_draws)
    } else {
      return(pred_mean)
    }
  }

  # ---------------------------
  # Tidymodels Workflow / ML Models (no sampling)
  # ---------------------------
  if (any(model_classes %in% c("workflow", "randomForest", "gbm", "xgb.Booster", "glmnet"))) {
    preds_generic <- tryCatch({
      if ("workflow" %in% model_classes) {
        if (!requireNamespace("parsnip", quietly = TRUE)) stop("The 'parsnip' package is required for tidymodels workflows.")
        pred_result <- predict(model, new_data = new_data)
        if (".pred" %in% names(pred_result)) {
          as.numeric(pred_result$.pred)
        } else {
          as.numeric(pred_result[[1]])
        }
      } else {
        if (!is.null(new_data)) {
          predict(model, newdata = new_data, ...)
        } else {
          predict(model, ...)
        }
      }
    }, error = function(e) {
      stop("Generic predict() failed for machine learning models: ", e$message)
    })
    n_expected <- if (!is.null(new_data)) nrow(new_data) else length(preds_generic)
    return(standardize_predictions(preds_generic, n_expected = n_expected))
  }

  # ---------------------------
  # Fallback Prediction
  # ---------------------------
  predictions <- tryCatch({
    preds_generic <- if (!is.null(new_data)) {
      predict(model, newdata = new_data, ...)
    } else {
      predict(model, ...)
    }
    n_expected <- if (!is.null(new_data)) nrow(new_data) else length(preds_generic)
    standardize_predictions(preds_generic, n_expected = n_expected)
  }, error = function(e) {
    stop("Generic predict() failed: ", e$message)
  })

  return(predictions)
}
