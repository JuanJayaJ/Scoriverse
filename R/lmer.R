#' @export
#' @method predict_unified lmerMod
predict_unified.lmerMod <- function(model, newdata, type = "response", level = 0.95, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for mixed model predictions")
  }

  preds <- try(predict(model, newdata = newdata, re.form = NA, se.fit = TRUE, ...), silent = TRUE)
  if (inherits(preds, "try-error")) {
    stop("Error in prediction: ", attr(preds, "condition")$message)
  }

  crit_val <- stats::qnorm((1 + level) / 2)

  vc <- as.data.frame(lme4::VarCorr(model))
  rand_var <- sum(vc$vcov)

  total_se <- sqrt(preds$se.fit^2 + rand_var)

  tibble::tibble(
    estimate    = preds$fit,
    std_error   = total_se,
    lower       = preds$fit - crit_val * total_se,
    upper       = preds$fit + crit_val * total_se,
    re_variance = rand_var
  )
}
