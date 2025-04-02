#' Extract Predictions from Diverse Model Objects
#'
#' This function extracts prediction estimates from a fitted model object.
#'
#' @param model A fitted model object.
#' @param new_data Optional. A data frame containing new data for prediction.
#' @param ... Additional arguments passed to the underlying prediction functions.
#'
#' @return A numeric vector of predictions.
#'
#' @examples
#' \dontrun{
#'   mod <- lm(mpg ~ wt + hp, data = mtcars)
#'   preds <- extract_predictions(mod)
#'   library(randomForest)
#'   rf_mod <- randomForest(mpg ~ wt + hp, data = mtcars)
#'   preds_rf <- extract_predictions(rf_mod, new_data = mtcars)
#' }
#'
#' @export
extract_predictions <- function(model, new_data = NULL, ...) {
  dots <- list(...)
  scoring_args <- c("pred_sd", "pred_prob", "lower", "upper")
  filtered_args <- dots[!names(dots) %in% scoring_args]

  # First approach: Handle linear models and those supported by marginaleffects
  if (inherits(model, "lm") ||
      inherits(model, "glm") ||
      inherits(model, "gam") ||
      inherits(model, "merMod") ||
      inherits(model, "brmsfit")) {
    preds <- tryCatch({
      if (!is.null(new_data)) {
        do.call(marginaleffects::predictions, c(list(model, newdata = new_data), filtered_args))
      } else {
        do.call(marginaleffects::predictions, c(list(model), filtered_args))
      }
    }, error = function(e) {
      warning("marginaleffects::predictions() failed: ", e$message)
      NULL
    })

    if (!is.null(preds)) {
      pred_vec <- standardize_predictions(preds)
      # Ensure length matches the number of rows in new_data
      if (!is.null(new_data) && length(pred_vec) != nrow(new_data)) {
        if (length(pred_vec) > nrow(new_data)) {
          pred_vec <- pred_vec[seq_len(nrow(new_data))]
        } else {
          # If we have fewer predictions than expected, repeat the last value
          warning("Fewer predictions than expected rows, padding to match length")
          pred_vec <- c(pred_vec, rep(pred_vec[length(pred_vec)], nrow(new_data) - length(pred_vec)))
        }
      }
      return(pred_vec)
    }
  }

  # Second approach: Handle ML models with specific predict methods
  if (inherits(model, "randomForest") ||
      inherits(model, "gbm") ||
      inherits(model, "xgb.Booster") ||
      inherits(model, "glmnet")) {
    if (!is.null(new_data)) {
      preds_generic <- predict(model, newdata = new_data, ...)
      # Ensure length matches
      if (!is.vector(preds_generic)) {
        preds_generic <- as.vector(preds_generic)
      }
      if (length(preds_generic) != nrow(new_data)) {
        if (length(preds_generic) > nrow(new_data)) {
          preds_generic <- preds_generic[seq_len(nrow(new_data))]
        } else {
          warning("Fewer predictions than expected rows, padding to match length")
          preds_generic <- c(preds_generic, rep(preds_generic[length(preds_generic)],
                                                nrow(new_data) - length(preds_generic)))
        }
      }
      return(as.numeric(preds_generic))
    } else {
      return(as.numeric(predict(model, ...)))
    }
  }

  # Fallback: Generic predict method
  predictions <- tryCatch({
    if (!is.null(new_data)) {
      preds_generic <- predict(model, newdata = new_data, ...)
      if (!is.vector(preds_generic)) {
        preds_generic <- as.vector(preds_generic)
      }
      if (length(preds_generic) != nrow(new_data)) {
        if (length(preds_generic) > nrow(new_data)) {
          preds_generic <- preds_generic[seq_len(nrow(new_data))]
        } else {
          warning("Fewer predictions than expected rows, padding to match length")
          preds_generic <- c(preds_generic, rep(preds_generic[length(preds_generic)],
                                                nrow(new_data) - length(preds_generic)))
        }
      }
      preds_generic
    } else {
      predict(model, ...)
    }
  }, error = function(e) {
    stop("Generic predict() failed: ", e$message)
  })

  # Ensure we return a numeric vector of appropriate length
  pred_vec <- standardize_predictions(predictions)
  if (!is.null(new_data) && length(pred_vec) != nrow(new_data)) {
    if (length(pred_vec) > nrow(new_data)) {
      pred_vec <- pred_vec[seq_len(nrow(new_data))]
    } else {
      warning("Fewer predictions than expected rows, padding to match length")
      pred_vec <- c(pred_vec, rep(pred_vec[length(pred_vec)], nrow(new_data) - length(pred_vec)))
    }
  }
  return(pred_vec)
}
