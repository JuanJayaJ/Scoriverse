#' @export
#' @method predict_unified gam
predict_unified.gam <- function(model, newdata, type = "response", level = 0.95, ...) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required for GAM predictions")
  }

  preds <- try(mgcv::predict.gam(model, newdata = newdata, se.fit = TRUE, type = type, ...), silent = TRUE)
  if (inherits(preds, "try-error")) {
    stop("Error in prediction: ", attr(preds, "condition")$message)
  }

  crit_val <- stats::qnorm((1 + level) / 2)

  tibble::tibble(
    estimate  = preds$fit,
    std_error = preds$se.fit,
    lower     = preds$fit - crit_val * preds$se.fit,
    upper     = preds$fit + crit_val * preds$se.fit
  )
}

