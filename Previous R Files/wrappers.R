#' Wrapper Functions for Model Prediction and Scoring
#'
#' Provides wrapper functions to standardize prediction extraction, scoring,
#' and model preparation. These functions handle different model types
#' (such as GLM, GAM, brmsfit, and tidymodels workflows) through a unified API.
#'
#' @details
#' The key functions are:
#' \describe{
#'   \item{\code{wrap_predict()}}{
#'     Standardizes prediction extraction across supported models.
#'   }
#'   \item{\code{wrap_scoring()}}{
#'     Applies scoring functions (e.g., CRPS, log score, Brier score) to predictions.
#'   }
#'   \item{\code{prepare_model_for_prediction()}}{
#'     Validates and prepares the model object before extracting predictions.
#'   }
#'   \item{\code{extract_additional_params()}}{
#'     Extracts model-specific parameters like sigma (for Gaussian) or phi (for Negative Binomial).
#'   }
#' }
#'
#' @import marginaleffects
#' @import ggplot2
#' @importFrom stats rpois rnbinom rnorm predict
#' @importFrom gratia fitted_samples
#' @import scoringRules
#' @name wrappers
NULL

# ----------------------------
# Standardize Predictions Output
# ----------------------------
standardize_predictions <- function(preds, n_expected = NULL) {
  if (is.data.frame(preds) || inherits(preds, "tbl_df")) {
    pred_vec <- if ("estimate" %in% names(preds)) as.numeric(preds$estimate) else as.numeric(preds)
  } else {
    pred_vec <- as.numeric(preds)
  }
  if (!is.null(n_expected) && length(pred_vec) != n_expected) {
    warning(sprintf("Prediction length (%d) does not match expected length (%d).", length(pred_vec), n_expected))
  }
  return(pred_vec)
}

# ----------------------------
# Validate Model Object
# ----------------------------
validate_model_object <- function(model) {
  if (is.null(model)) stop("Provided model is NULL.")
  model
}

# ----------------------------
#' Extract Additional Parameters from Model
#'
#' Retrieves additional parameters required for scoring, such as sigma
#' for Gaussian models or phi for Negative Binomial models.
#'
#' @param model A fitted model object.
#'
#' @return A list of extracted parameters (e.g., sigma, phi).
#' @export

extract_additional_params <- function(model) {
  params <- list()
  if (inherits(model, c("lm", "glm"))) {
    fam <- tryCatch(stats::family(model)$family, error = function(e) NULL)
    if (!is.null(fam)) {
      if (fam == "gaussian") {
        dispersion <- tryCatch(summary(model)$dispersion, error = function(e) NULL)
        params$sigma <- if (!is.null(dispersion)) sqrt(dispersion) else sqrt(mean(residuals(model)^2))
      } else if (grepl("Negative Binomial", fam, ignore.case = TRUE)) {
        if (!is.null(model$theta)) {
          params$phi <- model$theta
        }
      }
    }
  }
  return(params)
}

# ----------------------------
# Prediction Dispatch Logic (Internal)
# ----------------------------
#' @keywords internal
.predict_model_dispatch <- function(model, new_data = NULL, filtered_args = list(), ...) {
  model_classes <- class(model)

  # Tidymodels Workflow
  if ("workflow" %in% model_classes) {
    if (!requireNamespace("parsnip", quietly = TRUE)) {
      stop("The 'parsnip' package is required for tidymodels workflows.")
    }
    if (is.null(new_data)) stop("`new_data` must be provided for tidymodels workflows.")
    pred_result <- predict(model, new_data = new_data)
    pred_vec <- if (".pred" %in% names(pred_result)) as.numeric(pred_result$.pred) else as.numeric(pred_result[[1]])
    attr(pred_vec, "additional_params") <- list()
    return(pred_vec)
  }

  # lm / glm
  if ("lm" %in% model_classes) {
    preds_raw <- predict(model, newdata = new_data, ...)
    pred_vec <- as.numeric(preds_raw)
    attr(pred_vec, "additional_params") <- extract_additional_params(model)
    return(pred_vec)
  }

  # brmsfit
  if ("brmsfit" %in% model_classes) {
    if (!is.null(filtered_args$type) && filtered_args$type == "posterior_predict") {
      n_draws <- filtered_args$draws %||% 1000
      draws <- brms::posterior_predict(model, newdata = new_data, draws = n_draws)
      attr(draws, "additional_params") <- extract_additional_params(model)
      return(draws)
    } else {
      preds_raw <- do.call(marginaleffects::predictions, c(list(model = model, newdata = new_data), filtered_args))
      pred_vec <- standardize_predictions(preds_raw)
      attr(pred_vec, "additional_params") <- extract_additional_params(model)
      return(pred_vec)
    }
  }

  # gam
  if ("gam" %in% model_classes) {
    fam <- tryCatch(stats::family(model)$family, error = function(e) NULL)
    mu <- predict(model, newdata = new_data, type = "response")
    if (!is.null(fam)) {
      if (grepl("poisson", fam, ignore.case = TRUE)) {
        pred_vec <- sapply(mu, function(lam) rpois(1, lambda = lam))
      } else if (grepl("negbinom", fam, ignore.case = TRUE)) {
        params <- extract_additional_params(model)
        if (is.null(params$phi)) stop("Negative Binomial: overdispersion parameter 'phi' not available.")
        pred_vec <- sapply(mu, function(lam) rnbinom(1, size = params$phi, mu = lam))
      } else {
        pred_vec <- mu
      }
    } else {
      pred_vec <- mu
    }
    attr(pred_vec, "additional_params") <- extract_additional_params(model)
    return(pred_vec)
  }

  # Fallback
  preds_generic <- predict(model, newdata = new_data, ...)
  preds_generic <- standardize_predictions(preds_generic, n_expected = if (!is.null(new_data)) nrow(new_data) else length(preds_generic))
  attr(preds_generic, "additional_params") <- extract_additional_params(model)
  return(preds_generic)
}

# ----------------------------
# wrap_predict (Public)
# ----------------------------
#' Prediction Extraction Wrapper
#'
#' Extracts predictions from a fitted model object, supporting GLM, GAM, brmsfit,
#' and tidymodels workflows. Handles appropriate extraction logic for each model type.
#'
#' @param model A fitted model object.
#' @param new_data A data frame of new observations.
#' @param ... Additional arguments passed to the prediction functions.
#'
#' @return A numeric vector of predictions or a matrix of posterior samples.
#' @export
wrap_predict <- function(model, new_data = NULL, ...) {
  model <- validate_model_object(model)
  dots <- list(...)
  scoring_args <- c("pred_sd", "pred_prob", "lower", "upper")
  filtered_args <- dots[!names(dots) %in% scoring_args]
  tryCatch({
    .predict_model_dispatch(model, new_data = new_data, filtered_args = filtered_args, ...)
  }, error = function(e) {
    stop("wrap_predict failed: ", e$message)
  })
}

# ----------------------------
# wrap_scoring (Public)
# ----------------------------
#' Scoring Wrapper
#'
#' Computes probabilistic scores (CRPS, log score, Brier score, interval score, DSS)
#' for model predictions against observed outcomes.
#'
#' @param score_type A character string specifying the score type ('crps', 'log_score', 'brier', 'interval', 'dss').
#' @param y_true A numeric vector of observed outcome values.
#' @param predictions A numeric vector or matrix of predictions.
#' @param ... Additional arguments such as 'pred_sd', 'pred_prob', 'lower', or 'upper' as required by the scoring function.
#'
#' @return A numeric vector of computed scores.
#' @export
wrap_scoring <- function(score_type, y_true, predictions, ...) {
  if (!is.character(score_type)) stop("`score_type` must be a character string.")
  if (!is.numeric(y_true)) stop("`y_true` must be numeric.")
  if (!(is.numeric(predictions) || is.matrix(predictions))) stop("`predictions` must be numeric or a matrix.")
  extra_args <- list(...)
  if (is.matrix(predictions)) {
    return(compute_score_from_samples(y = y_true, pred_samples = predictions,
                                      score_function = switch(score_type,
                                                              "crps" = compute_crps,
                                                              stop("Sample-based scoring not yet supported for this score type."))))
  }
  if (score_type %in% c("crps", "log_score", "dss") && is.null(extra_args$pred_sd)) {
    stop("Standard deviation ('pred_sd') is required for this score computation.")
  }
  if (score_type == "brier" && is.null(extra_args$pred_prob)) {
    stop("Predicted probabilities ('pred_prob') are required for Brier score computation.")
  }
  if (score_type == "interval" && (!all(c("lower", "upper") %in% names(extra_args)))) {
    stop("Both 'lower' and 'upper' bounds must be provided for interval score computation.")
  }
  switch(score_type,
         "crps" = compute_crps(y_true, pred_mean = predictions, pred_sd = extra_args$pred_sd),
         "log_score" = compute_log_score(y_true, pred_mean = predictions, pred_sd = extra_args$pred_sd),
         "brier" = compute_brier_score(y_true, pred_prob = extra_args$pred_prob),
         "interval" = compute_interval_score(y_true, lower = extra_args$lower, upper = extra_args$upper),
         "dss" = compute_dss(y_true, pred_mean = predictions, pred_sd = extra_args$pred_sd),
         stop("Unsupported score_type. Choose from: 'crps', 'log_score', 'brier', 'interval', 'dss'."))
}

# ----------------------------
# prepare_model_for_prediction (Public)
# ----------------------------
#' Prepare Model for Prediction
#'
#' Validates the input model and extracts predictions using \code{wrap_predict()}.
#'
#' @param model A fitted model object.
#' @param new_data A data frame of new observations.
#' @param ... Additional arguments passed to prediction functions.
#'
#' @return A numeric vector of predictions or a matrix of posterior samples.
#' @export
prepare_model_for_prediction <- function(model, new_data, ...) {
  validated_model <- validate_model_object(model)
  wrap_predict(validated_model, new_data = new_data, ...)
}
