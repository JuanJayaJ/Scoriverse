#' Wrapper Functions for Model Prediction and Scoring
#'
#' This file provides functions to standardize prediction extraction, scoring,
#' and model preparation. It ensures that different model types with varying interfaces
#' can be handled through a unified API.
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
#'     Provides a consistent interface to apply scoring functions to predictions. It checks
#'     for necessary inputs and applies any pre-processing if needed.
#'   }
#'   \item{\code{prepare_model_for_prediction()}}{
#'     Acts as a high-level function to validate and pre-process the model object before
#'     prediction extraction. This function can be extended to include additional model
#'     harmonization logic.
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
#' This helper function converts the raw prediction output (from marginaleffects::predictions()
#' or predict()) into a numeric vector. Optionally, it can check that the output length matches
#' an expected value.
#'
#' @param preds The raw predictions output.
#' @param n_expected (Optional) An expected length.
#'
#' @return A numeric vector of predictions.
standardize_predictions <- function(preds, n_expected = NULL) {
  # Handle different types of prediction outputs
  if (is.data.frame(preds) || inherits(preds, "tbl_df")) {
    if ("estimate" %in% names(preds)) {
      pred_vec <- as.numeric(preds$estimate)
    } else {
      pred_vec <- as.numeric(unlist(preds[1]))  # Take first column if no estimate column
    }
  } else if (is.matrix(preds)) {
    pred_vec <- as.numeric(preds)
  } else if (is.list(preds) && !is.data.frame(preds)) {
    # Handle list outputs from some prediction functions
    if (length(preds) > 0 && is.numeric(preds[[1]])) {
      pred_vec <- as.numeric(unlist(preds))
    } else {
      pred_vec <- as.numeric(unlist(preds))
    }
  } else {
    # Vector or other type
    pred_vec <- as.numeric(preds)
  }

  # Handle NAs by replacing with mean or 0
  if (any(is.na(pred_vec))) {
    warning("NA values found in predictions, replacing with mean")
    if (all(is.na(pred_vec))) {
      pred_vec[is.na(pred_vec)] <- 0
    } else {
      pred_vec[is.na(pred_vec)] <- mean(pred_vec, na.rm = TRUE)
    }
  }

  # Adjust length if needed
  if (!is.null(n_expected) && length(pred_vec) != n_expected) {
    if (length(pred_vec) > n_expected) {
      warning(sprintf("Prediction length (%d) exceeds expected length (%d), truncating.",
                      length(pred_vec), n_expected))
      pred_vec <- pred_vec[seq_len(n_expected)]
    } else {
      warning(sprintf("Prediction length (%d) less than expected length (%d), padding.",
                      length(pred_vec), n_expected))
      # Pad by repeating the last value
      pred_vec <- c(pred_vec, rep(pred_vec[length(pred_vec)], n_expected - length(pred_vec)))
    }
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
  return(model)
}

#' Convert Predictions to a Matrix
#'
#' Converts point predictions into a matrix format if necessary.
#'
#' @param predictions A numeric vector or a data frame of predictions.
#'
#' @return A matrix of predictions.
convert_predictions_to_matrix <- function(predictions) {
  if (is.vector(predictions)) {
    return(matrix(predictions, ncol = 1))
  } else if (is.data.frame(predictions)) {
    return(as.matrix(predictions))
  } else if (is.matrix(predictions)) {
    return(predictions)
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
#' @param new_data Optional. A data frame containing new data for prediction.
#' @param ... Additional arguments to pass to the underlying prediction functions.
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

  # Handle linear models (lm, glm, gam, merMod, brmsfit)
  if (any(model_classes %in% c("lm", "glm", "gam", "merMod", "brmsfit"))) {
    predictions <- tryCatch({
      if (!is.null(new_data)) {
        do.call(marginaleffects::predictions, c(list(model, newdata = new_data), filtered_args))
      } else {
        do.call(marginaleffects::predictions, c(list(model), filtered_args))
      }
    }, error = function(e) {
      warning("marginaleffects::predictions() failed: ", e$message)
      NULL
    })

    if (!is.null(predictions)) {
      # Ensure predictions match expected length
      return(standardize_predictions(predictions,
                                     n_expected = if (!is.null(new_data)) nrow(new_data) else NULL))
    }
  }

  # Handle tree-based models (randomForest, gbm, xgb.Booster, glmnet)
  if (any(model_classes %in% c("randomForest", "gbm", "xgb.Booster", "glmnet"))) {
    predictions <- tryCatch({
      if (!is.null(new_data)) {
        predict(model, newdata = new_data, ...)
      } else {
        predict(model, ...)
      }
    }, error = function(e) {
      warning("Model-specific predict() failed: ", e$message)
      NULL
    })

    if (!is.null(predictions)) {
      return(standardize_predictions(predictions,
                                     n_expected = if (!is.null(new_data)) nrow(new_data) else NULL))
    }
  }

  # Handle other model types with the generic prediction method
  predictions <- tryCatch({
    if (!is.null(new_data)) {
      predict(model, newdata = new_data, ...)
    } else {
      predict(model, ...)
    }
  }, error = function(e) {
    stop("Generic predict() failed: ", e$message)
  })

  return(standardize_predictions(predictions,
                                 n_expected = if (!is.null(new_data)) nrow(new_data) else NULL))
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

  # Validate inputs
  y_true <- as.numeric(y_true)
  predictions <- as.numeric(predictions)

  # Ensure lengths match
  n <- length(y_true)
  if (length(predictions) != n) {
    if (length(predictions) > n) {
      predictions <- predictions[seq_len(n)]
    } else {
      predictions <- c(predictions, rep(predictions[length(predictions)], n - length(predictions)))
    }
  }

  # Validate required arguments for scoring
  if (score_type == "crps" || score_type == "log_score" || score_type == "dss") {
    if (!("pred_sd" %in% names(extra_args))) {
      stop("Standard deviation ('pred_sd') must be provided for this score computation.")
    }
    # Ensure pred_sd has correct length
    pred_sd <- extra_args[["pred_sd"]]
    if (length(pred_sd) != n) {
      if (length(pred_sd) > n) {
        extra_args[["pred_sd"]] <- pred_sd[seq_len(n)]
      } else {
        extra_args[["pred_sd"]] <- c(pred_sd, rep(pred_sd[length(pred_sd)], n - length(pred_sd)))
      }
    }
  }
  if (score_type == "brier") {
    if (!("pred_prob" %in% names(extra_args))) {
      stop("Predicted probabilities ('pred_prob') must be provided for Brier score computation.")
    }
    # Ensure pred_prob has correct length
    pred_prob <- extra_args[["pred_prob"]]
    if (length(pred_prob) != n) {
      if (length(pred_prob) > n) {
        extra_args[["pred_prob"]] <- pred_prob[seq_len(n)]
      } else {
        extra_args[["pred_prob"]] <- c(pred_prob, rep(pred_prob[length(pred_prob)], n - length(pred_prob)))
      }
    }
  }
  if (score_type == "interval") {
    if (!all(c("lower", "upper") %in% names(extra_args))) {
      stop("Both 'lower' and 'upper' bounds must be provided for interval score computation.")
    }
    # Ensure bounds have correct lengths
    lower <- extra_args[["lower"]]
    upper <- extra_args[["upper"]]
    if (length(lower) != n) {
      if (length(lower) > n) {
        extra_args[["lower"]] <- lower[seq_len(n)]
      } else {
        extra_args[["lower"]] <- c(lower, rep(lower[length(lower)], n - length(lower)))
      }
    }
    if (length(upper) != n) {
      if (length(upper) > n) {
        extra_args[["upper"]] <- upper[seq_len(n)]
      } else {
        extra_args[["upper"]] <- c(upper, rep(upper[length(upper)], n - length(upper)))
      }
    }
  }

  # Handle specific scoring functions
  result <- NULL
  if (score_type == "crps") {
    result <- compute_crps(y_true, pred_mean = predictions, pred_sd = extra_args[["pred_sd"]])
  } else if (score_type == "log_score") {
    result <- compute_log_score(y_true, predictions, extra_args[["pred_sd"]])
  } else if (score_type == "brier") {
    result <- compute_brier_score(y_true, extra_args[["pred_prob"]])
  } else if (score_type == "interval") {
    result <- compute_interval_score(y_true, extra_args[["lower"]], extra_args[["upper"]])
  } else if (score_type == "dss") {
    result <- compute_dss(y_true, predictions, extra_args[["pred_sd"]])
  } else {
    stop("Unsupported score_type provided. Please choose one of: 'crps', 'log_score', 'brier', 'interval', 'dss'.")
  }

  # Ensure output length matches input length
  if (length(result) != n) {
    if (length(result) > n) {
      result <- result[seq_len(n)]
    } else {
      result <- c(result, rep(result[length(result)], n - length(result)))
    }
  }

  return(result)
}

#' Prepare Model for Prediction
#'
#' Acts as a high-level interface to validate and pre-process a model object
#' before performing prediction extraction.
#'
#' @param model A fitted model object.
#' @param new_data A data frame containing new data to make predictions on.
#' @param ... Additional arguments passed to the underlying functions.
#'
#' @return A numeric vector of predictions.
#' @export
prepare_model_for_prediction <- function(model, new_data, ...) {
  validated_model <- validate_model_object(model)
  return(wrap_predict(validated_model, new_data = new_data, ...))
}

#' Convert predictions to the outcome scale
#'
#' This function converts predictions from the linear predictor scale to the outcome scale
#' for different model types like Poisson, Negative Binomial, or Binomial.
#'
#' @param predictions A numeric vector of predictions on the linear predictor scale (or already on the outcome scale).
#' @param model_type A character string indicating the model type: "poisson", "negbin", or "binomial".
#' @param ... Additional arguments (currently ignored).
#'
#' @return A numeric vector of predictions on the outcome scale.
#' @export
convert_to_outcome_scale <- function(predictions, model_type, ...) {
  # Ensure model_type is a single character string
  model_type <- as.character(model_type)[1]
  
  if (model_type == "poisson") {
    pred_outcome <- exp(predictions)
    pred_outcome[pred_outcome < 0] <- 0
    return(pred_outcome)
  } else if (model_type == "negbin") {
    pred_outcome <- exp(predictions)
    pred_outcome[pred_outcome < 0] <- 0
    return(pred_outcome)
  } else if (model_type == "binomial") {
    # If predictions are already between 0 and 1, assume they are on the outcome scale.
    if (all(predictions >= 0 & predictions <= 1)) {
      return(predictions)
    } else {
      pred_outcome <- plogis(predictions)
      pred_outcome[pred_outcome < 0] <- 0
      pred_outcome[pred_outcome > 1] <- 1
      return(pred_outcome)
    }
  } else {
    stop("Unsupported model_type. Please use one of: 'poisson', 'negbin', 'binomial'.")
  }
}


