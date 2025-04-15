#' Wrapper Functions for Model Prediction and Scoring
#'
#' Provides functions to standardize prediction extraction, scoring, and model preparation.
#' It handles different model types with varying interfaces through a unified API.
#'
#' @details
#' The functions included in this file are:
#' \describe{
#'   \item{\code{wrap_predict()}}{
#'     Standardizes prediction extraction by checking the model's class and applying the
#'     appropriate extraction method. Falls back to the generic \code{predict()} if no
#'     specialized method is found.
#'   }
#'   \item{\code{wrap_scoring()}}{
#'     Provides a consistent interface to apply scoring functions to predictions.
#'   }
#'   \item{\code{prepare_model_for_prediction()}}{
#'     Validates and pre-processes the model object before prediction extraction.
#'   }
#' }
#'
#' @name wrappers
#' @import marginaleffects
#' @import ggplot2
#' @importFrom stats rpois
#' @import scoringRules
NULL

# Helper: Standardize Predictions Output
standardize_predictions <- function(preds, n_expected = NULL) {
  if (is.data.frame(preds) || inherits(preds, "tbl_df")) {
    if ("estimate" %in% names(preds)) {
      pred_vec <- as.numeric(preds$estimate)
    } else {
      pred_vec <- as.numeric(preds)
    }
  } else {
    pred_vec <- as.numeric(preds)
  }

  # Check if prediction length matches expected length
  if (!is.null(n_expected) && length(pred_vec) != n_expected) {
    message(sprintf("Note: standardized prediction length (%d) does not match expected length (%d).",
                    length(pred_vec), n_expected))
  }

  return(pred_vec)
}

# Validate a Model Object
validate_model_object <- function(model) {
  if (is.null(model)) {
    stop("Provided model is NULL.")
  }
  model
}

#' Extract Additional Model Parameters for Scoring
#'
#' Extracts additional parameters from the model object required for scoring,
#' such as sigma for Gaussian models or phi for Negative Binomial models.
#'
#' @param model A fitted model object.
#'
#' @return A list of additional parameters.
#' @export
extract_additional_params <- function(model) {
  params <- list()
  if (inherits(model, "lm") || inherits(model, "glm")) {
    fam <- tryCatch(stats::family(model)$family, error = function(e) NULL)
    if (!is.null(fam)) {
      if (fam == "gaussian") {
        params$sigma <- summary(model)$sigma
      } else if (grepl("Negative Binomial", fam, ignore.case = TRUE)) {
        if (!is.null(model$theta)) {
          params$phi <- model$theta
        }
      }
    }
  }
  # Additional extraction logic for other model types can be added here
  return(params)
}

#' Wrap Prediction Extraction for Various Models
#'
#' Standardizes prediction extraction for models that might not be directly supported by
#' \code{marginaleffects} or the generic \code{predict()} function.
#'
#' @param model A fitted model object.
#' @param new_data Optional data frame with new observations.
#' @param ... Additional arguments for the underlying prediction functions.
#'
#' @return A numeric vector of predictions.
#' @export
wrap_predict <- function(model, new_data = NULL, ...) {
  model <- validate_model_object(model)
  dots <- list(...)
  scoring_args <- c("pred_sd", "pred_prob", "lower", "upper")
  filtered_args <- dots[!names(dots) %in% scoring_args]
  predictions <- NULL
  model_classes <- class(model)

  # Special handling for lm models (linear models)
  if ("lm" %in% model_classes) {
    predictions <- tryCatch({
      # Using predict() for lm models
      preds_raw <- predict(model, newdata = new_data, ...)
      # Ensure the prediction is in a numeric vector format
      pred_vec <- as.numeric(preds_raw)

      # Extract additional parameters (sigma for lm)
      additional_params <- list()
      additional_params$sigma <- summary(model)$sigma

      # Ensure that additional_params is a list and contains sigma for lm
      if (!is.list(additional_params)) {
        stop("additional_params should be a list for lm model.")
      }

      # Return predictions and additional parameters
      attr(pred_vec, "additional_params") <- additional_params
      return(pred_vec)

    }, error = function(e) {
      warning("Prediction extraction failed for lm model: ", e$message)
      return(NULL)
    })
  }

  # Handling for other model types (brmsfit, gam, etc.)
  if (any(model_classes %in% c("lm", "glm", "gam", "merMod", "brmsfit"))) {
    predictions <- tryCatch({
      args <- list(model)
      if (!is.null(new_data)) {
        args$newdata <- new_data
      }
      if ("brmsfit" %in% model_classes) {
        if (!is.null(filtered_args$type) && filtered_args$type == "posterior_predict") {
          n_draws <- filtered_args$draws %||% 1000
          draws <- brms::posterior_predict(model, newdata = new_data, draws = n_draws)
          attr(draws, "additional_params") <- extract_additional_params(model)
          return(draws)
        } else {
          preds_raw <- do.call(marginaleffects::predictions, c(args, filtered_args))
          pred_vec <- standardize_predictions(preds_raw)
          additional_params <- extract_additional_params(model)
          attr(pred_vec, "additional_params") <- additional_params
          return(pred_vec)
        }
      }

      if ("gam" %in% model_classes) {
        fam <- tryCatch(stats::family(model)$family, error = function(e) NULL)
        if (!is.null(fam)) {
          if (grepl("poisson", fam, ignore.case = TRUE)) {
            message("Detected gam model with Poisson family. Generating Poisson draws.")
            pred_vec <- sapply(pred_vec, function(mu) rpois(n = 1, lambda = mu))
          } else if (grepl("negbinom", fam, ignore.case = TRUE)) {
            message("Detected gam model with Negative Binomial family. Generating NB draws.")
            params <- extract_additional_params(model)
            if (is.null(params$phi)) {
              stop("Negative Binomial: overdispersion parameter 'phi' not available.")
            }
            size <- params$phi
            pred_vec <- sapply(pred_vec, function(mu) rnbinom(n = 1, size = size, mu = mu))
          }
        }
      }
      pred_vec
    }, error = function(e) {
      warning("Prediction extraction failed: ", e$message)
      return(NULL)  # Ensures that NULL is returned on error
    })
  }

  # Generic prediction for other models (e.g., randomForest, glmnet)
  if (is.null(predictions)) {
    predictions <- tryCatch({
      if (!is.null(new_data)) {
        preds_generic <- predict(model, newdata = new_data, ...)
        if (length(preds_generic) != nrow(new_data)) {
          preds_generic <- preds_generic[seq_len(nrow(new_data))]
        }
        preds_generic
      } else {
        predict(model, ...)
      }
    }, error = function(e) {
      stop("Generic predict() failed: ", e$message)
    })
  }

  # Standardize predictions and return
  n_expected <- if (!is.null(new_data)) nrow(new_data) else length(predictions)
  predictions <- standardize_predictions(predictions, n_expected = n_expected)
  additional_params <- extract_additional_params(model)
  attr(predictions, "additional_params") <- additional_params
  predictions
}

#' Wrap Scoring Functions for Predictions
#'
#' Standardizes the process of applying scoring functions to predictions.
#'
#' @param score_type A character string indicating the type of scoring metric.
#' @param y_true A numeric vector of observed values.
#' @param predictions A numeric vector (or matrix) of predictions.
#' @param ... Additional arguments required by the scoring function.
#'
#' @return A numeric vector of computed score values.
#' @export
wrap_scoring <- function(score_type, y_true, predictions, ...) {
  if (is.matrix(predictions)) {
    return(compute_score_from_samples(y_true, predictions, score_function = switch(
      score_type,
      "crps" = compute_crps,
      # Add more sample-based scoring functions here later
      stop("Sample-based scoring not yet supported for this score type.")
    )))
  }
  extra_args <- list(...)
  if (score_type %in% c("crps", "log_score", "dss") && !("pred_sd" %in% names(extra_args))) {
    stop("Standard deviation ('pred_sd') must be provided for this score computation.")
  }
  if (score_type == "brier" && !("pred_prob" %in% names(extra_args))) {
    stop("Predicted probabilities ('pred_prob') must be provided for Brier score computation.")
  }
  if (score_type == "interval" && !all(c("lower", "upper") %in% names(extra_args))) {
    stop("Both 'lower' and 'upper' bounds must be provided for interval score computation.")
  }

  if (score_type == "crps") {
    return(compute_crps(y_true, pred_mean = predictions, pred_sd = extra_args[["pred_sd"]]))
  } else if (score_type == "log_score") {
    return(compute_log_score(y_true, predictions, extra_args[["pred_sd"]]))
  } else if (score_type == "brier") {
    return(compute_brier_score(y_true, extra_args[["pred_prob"]]))
  } else if (score_type == "interval") {
    return(compute_interval_score(y_true, extra_args[["lower"]], extra_args[["upper"]]))
  } else if (score_type == "dss") {
    return(compute_dss(y_true, predictions, extra_args[["pred_sd"]]))
  } else {
    stop("Unsupported score_type. Choose from: 'crps', 'log_score', 'brier', 'interval', 'dss'.")
  }
}

#' Prepare Model for Prediction
#'
#' Validates and pre-processes a model object before extracting predictions.
#'
#' @param model A fitted model object.
#' @param new_data A data frame containing new data for predictions.
#' @param ... Additional arguments for the prediction function.
#'
#' @return A numeric vector of predictions.
#' @export
prepare_model_for_prediction <- function(model, new_data, ...) {
  validated_model <- validate_model_object(model)
  wrap_predict(validated_model, new_data = new_data, ...)
}

