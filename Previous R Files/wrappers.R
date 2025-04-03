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
#' @import scoringRules
NULL

# Helper: Standardize Predictions Output
#' Standardize Predictions
#'
#' Converts the raw prediction output into a numeric vector. Optionally checks that
#' the output length matches an expected value.
#'
#' @param preds The raw predictions output.
#' @param n_expected Optional. An expected length.
#'
#' @return A numeric vector of predictions.
standardize_predictions <- function(preds, n_expected = NULL) {
  # Extract predictions from data frames or tibbles
  if (is.data.frame(preds) || inherits(preds, "tbl_df")) {
    if ("estimate" %in% names(preds)) {
      pred_vec <- as.numeric(preds$estimate)
    } else {
      pred_vec <- as.numeric(preds)
    }
  } else {
    pred_vec <- as.numeric(preds)
  }
  
  if (!is.null(n_expected) && length(pred_vec) != n_expected) {
    message(sprintf("Note: standardized prediction length (%d) does not match expected length (%d).",
                    length(pred_vec), n_expected))
  }
  
  return(pred_vec)
}

#' Validate a Model Object
#'
#' Performs basic validation checks on the model object.
#'
#' @param model A fitted model object.
#'
#' @return The original model if valid; otherwise, stops with an error.
validate_model_object <- function(model) {
  if (is.null(model)) {
    stop("Provided model is NULL.")
  }
  model
}

#' Convert Predictions to a Matrix
#'
#' Converts point predictions into a matrix format if necessary.
#'
#' @param predictions A numeric vector or data frame of predictions.
#'
#' @return A matrix of predictions.
convert_predictions_to_matrix <- function(predictions) {
  if (is.vector(predictions)) {
    matrix(predictions, ncol = 1)
  } else if (is.data.frame(predictions)) {
    as.matrix(predictions)
  } else if (is.matrix(predictions)) {
    predictions
  } else {
    stop("Unrecognized format for predictions; cannot convert to matrix.")
  }
}

#' Wrap Prediction Extraction for Various Models
#'
#' Standardizes prediction extraction for models that might not be directly supported by
#' \code{marginaleffects} or the generic \code{predict()} function.
#'
#' @param model A fitted model object.
#' @param new_data Optional data frame with new observations.
#' @param ... Additional arguments to pass to the underlying prediction functions.
#'
#' @return A numeric vector of predictions.
#' @export
wrap_predict <- function(model, new_data = NULL, ...) {
  model <- validate_model_object(model)
  
  # Filter out scoring-specific arguments.
  dots <- list(...)
  scoring_args <- c("pred_sd", "pred_prob", "lower", "upper")
  filtered_args <- dots[!names(dots) %in% scoring_args]
  
  predictions <- NULL
  model_classes <- class(model)
  
  # Handle models compatible with marginaleffects::predictions()
  if (any(model_classes %in% c("lm", "glm", "gam", "merMod", "brmsfit"))) {
    predictions <- tryCatch({
      args <- list(model)
      if (!is.null(new_data)) {
        args$newdata <- new_data
      }
      do.call(marginaleffects::predictions, c(args, filtered_args))
    }, error = function(e) {
      warning("marginaleffects::predictions() failed: ", e$message)
      NULL
    })
    
    if (!is.null(predictions)) {
      pred_vec <- standardize_predictions(predictions)
      if (!is.null(new_data) && length(pred_vec) != nrow(new_data)) {
        pred_vec <- pred_vec[seq_len(nrow(new_data))]
      }
      return(pred_vec)
    }
  }
  
  # Handle tree-based models (randomForest, gbm, xgb.Booster, glmnet)
  if (any(model_classes %in% c("randomForest", "gbm", "xgb.Booster", "glmnet"))) {
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
      warning("Model-specific predict() failed: ", e$message)
      NULL
    })
    if (!is.null(predictions)) {
      n_expected <- if (!is.null(new_data)) nrow(new_data) else length(predictions)
      return(standardize_predictions(predictions, n_expected = n_expected))
    }
  }
  
  # Use the generic prediction method as a fallback
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
  
  n_expected <- if (!is.null(new_data)) nrow(new_data) else length(predictions)
  standardize_predictions(predictions, n_expected = n_expected)
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
  extra_args <- list(...)
  
  # Validate that necessary extra arguments are provided.
  if (score_type %in% c("crps", "log_score", "dss") && !("pred_sd" %in% names(extra_args))) {
    stop("Standard deviation ('pred_sd') must be provided for this score computation.")
  }
  if (score_type == "brier" && !("pred_prob" %in% names(extra_args))) {
    stop("Predicted probabilities ('pred_prob') must be provided for Brier score computation.")
  }
  if (score_type == "interval" && !all(c("lower", "upper") %in% names(extra_args))) {
    stop("Both 'lower' and 'upper' bounds must be provided for interval score computation.")
  }
  
  # Apply the specific scoring function based on score_type.
  if (score_type == "crps") {
    compute_crps(y_true, pred_mean = predictions, pred_sd = extra_args[["pred_sd"]])
  } else if (score_type == "log_score") {
    compute_log_score(y_true, predictions, extra_args[["pred_sd"]])
  } else if (score_type == "brier") {
    compute_brier_score(y_true, extra_args[["pred_prob"]])
  } else if (score_type == "interval") {
    compute_interval_score(y_true, extra_args[["lower"]], extra_args[["upper"]])
  } else if (score_type == "dss") {
    compute_dss(y_true, predictions, extra_args[["pred_sd"]])
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
