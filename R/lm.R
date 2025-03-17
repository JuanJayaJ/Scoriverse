#' @export
#' @method predict_unified lm
predict_unified.lm <- function(model, newdata, type = "response", level = 0.95, ...) {
  # Check for missing required variables in newdata
  model_vars <- all.vars(stats::terms(model))[-1]  # exclude response
  if (!all(model_vars %in% names(newdata))) {
    stop("newdata is missing some required variables: ",
         paste(setdiff(model_vars, names(newdata)), collapse = ", "))
  }

  # Get predictions with standard errors
  preds <- try(predict(model, newdata = newdata, se.fit = TRUE, ...), silent = TRUE)
  if (inherits(preds, "try-error")) {
    stop("Error in prediction: ", attr(preds, "condition")$message)
  }

  # Calculate critical value
  crit_val <- stats::qnorm((1 + level) / 2)

  tibble::tibble(
    estimate  = preds$fit,
    std_error = preds$se.fit,
    lower     = preds$fit - crit_val * preds$se.fit,
    upper     = preds$fit + crit_val * preds$se.fit
  )
}
