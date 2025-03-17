#' @export
#' @method predict_unified glm
predict_unified.glm <- function(model, newdata, type = "response", level = 0.95, ...) {
  if (!inherits(model$family, "family")) {
    stop("Invalid GLM object: family not properly specified")
  }

  # Get predictions on the link scale
  preds_link <- try(predict(model, newdata = newdata, se.fit = TRUE, type = "link", ...), silent = TRUE)
  if (inherits(preds_link, "try-error")) {
    stop("Error in prediction: ", attr(preds_link, "condition")$message)
  }

  # Calculate critical value
  crit_val <- stats::qnorm((1 + level) / 2)

  if (type == "response") {
    linkinv  <- model$family$linkinv
    estimate <- linkinv(preds_link$fit)
    # Delta method for standard errors on response scale
    deriv    <- model$family$mu.eta(preds_link$fit)
    std_error <- abs(deriv * preds_link$se.fit)

    tibble::tibble(
      estimate  = estimate,
      std_error = std_error,
      lower     = linkinv(preds_link$fit - crit_val * preds_link$se.fit),
      upper     = linkinv(preds_link$fit + crit_val * preds_link$se.fit)
    )
  } else {
    tibble::tibble(
      estimate  = preds_link$fit,
      std_error = preds_link$se.fit,
      lower     = preds_link$fit - crit_val * preds_link$se.fit,
      upper     = preds_link$fit + crit_val * preds_link$se.fit
    )
  }
}

