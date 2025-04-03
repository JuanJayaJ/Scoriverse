#' Extract Predictions from Diverse Model Objects
#'
#' Extracts prediction estimates from a fitted model object using appropriate methods.
#'
#' @param model A fitted model object.
#' @param new_data Optional data frame with new observations.
#' @param ... Additional arguments for underlying prediction functions.
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
#' @export
extract_predictions <- function(model, new_data = NULL, ...) {
  # Exclude arguments that are specific to scoring.
  dots <- list(...)
  scoring_args <- c("pred_sd", "pred_prob", "lower", "upper")
  filtered_args <- dots[!names(dots) %in% scoring_args]
  
  # For model classes that work with marginaleffects::predictions()
  if (inherits(model, "lm") || inherits(model, "glm") ||
      inherits(model, "gam") || inherits(model, "merMod") ||
      inherits(model, "brmsfit")) {
    preds <- tryCatch({
      args <- list(model)
      if (!is.null(new_data)) {
        args$newdata <- new_data
      }
      do.call(marginaleffects::predictions, c(args, filtered_args))
    }, error = function(e) {
      warning("marginaleffects::predictions() failed: ", e$message)
      NULL
    })
    
    if (!is.null(preds)) {
      pred_vec <- standardize_predictions(preds)
      if (!is.null(new_data) && length(pred_vec) != nrow(new_data)) {
        pred_vec <- pred_vec[seq_len(nrow(new_data))]
      }
      return(pred_vec)
    }
  }
  
  # For models from randomForest, gbm, xgb.Booster, glmnet
  if (inherits(model, "randomForest") || inherits(model, "gbm") ||
      inherits(model, "xgb.Booster") || inherits(model, "glmnet")) {
    if (!is.null(new_data)) {
      preds_generic <- predict(model, newdata = new_data, ...)
      if (length(preds_generic) != nrow(new_data)) {
        preds_generic <- preds_generic[seq_len(nrow(new_data))]
      }
      return(standardize_predictions(preds_generic, n_expected = nrow(new_data)))
    } else {
      return(standardize_predictions(predict(model, ...)))
    }
  }
  
  # Fallback to the generic predict() method
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
