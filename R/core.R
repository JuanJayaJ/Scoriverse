#' Unified Prediction Extraction
#'
#' Extracts standardized predictions (point estimates, standard errors, and confidence
#' intervals) from various statistical models. This function provides a consistent
#' interface across different model types, handling the necessary transformations
#' and uncertainty calculations appropriately for each model class.
#'
#' @param model A fitted model object (e.g., lm, glm, gam, lmerMod)
#' @param newdata A data frame containing the predictor variables used in the model
#' @param type Character string specifying the type of prediction:
#'   \itemize{
#'     \item \code{"response"} (default) - predictions on the response scale
#'     \item \code{"link"} - predictions on the link scale (for GLMs)
#'   }
#' @param level The confidence level for intervals (default: 0.95)
#' @param ... Additional arguments passed to the underlying predict methods
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item \code{estimate}: Point predictions
#'     \item \code{std_error}: Standard errors of the predictions
#'     \item \code{lower}: Lower bound of the confidence interval
#'     \item \code{upper}: Upper bound of the confidence interval
#'     \item \code{re_variance}: (Only for mixed models) The random effects variance
#'   }
#'
#' @examples
#' # Linear model example
#' mod_lm <- lm(mpg ~ wt + hp, data = mtcars)
#' predict_unified(mod_lm, newdata = mtcars[1:5, ])
#'
#' # GLM example
#' mod_glm <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' predict_unified(mod_glm, newdata = mtcars[1:5, ])
#'
#' @export
predict_unified <- function(model, newdata, type = "response", level = 0.95, ...) {
  # Input validation common to all model types
  if (!inherits(model, c("lm", "glm", "gam", "lmerMod"))) {
    stop("Model type not supported. Must be one of: lm, glm, gam, or lmerMod")
  }
  if (!is.data.frame(newdata)) {
    stop("newdata must be a data frame")
  }
  if (!is.numeric(level) || level <= 0 || level >= 1) {
    stop("level must be a number between 0 and 1")
  }
  if (!type %in% c("response", "link")) {
    stop("type must be either 'response' or 'link'")
  }
  UseMethod("predict_unified", model)
}
