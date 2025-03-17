#' Validate Model, Data, and Truth Inputs
#'
#' Checks that the model is of a supported type, that newdata is a data frame,
#' and that the truth vector has the correct length and no missing values.
#'
#' @param model A fitted model object.
#' @param newdata A data frame of predictor variables.
#' @param truth A vector of observed values.
#'
#' @export
#' @importFrom stats formula
validate_inputs <- function(model, newdata, truth) {
  supported_classes <- c("lm", "glm", "gam", "lmerMod")
  if (!any(class(model) %in% supported_classes)) {
    stop(sprintf("Model class '%s' not supported. Supported classes: %s",
                 class(model)[1], paste(supported_classes, collapse = ", ")))
  }

  if (!is.data.frame(newdata)) {
    stop("newdata must be a data frame.")
  }

  if (length(truth) != nrow(newdata)) {
    stop(sprintf("Length of truth (%d) does not match number of rows in newdata (%d).",
                 length(truth), nrow(newdata)))
  }

  if (any(is.na(truth))) {
    stop("Truth values contain missing data.")
  }

  if (any(sapply(newdata, function(x) all(is.na(x))))) {
    stop("newdata contains columns with all missing values.")
  }

  model_terms <- all.vars(formula(model))
  missing_terms <- setdiff(model_terms, names(newdata))
  if (length(missing_terms) > 0) {
    stop(sprintf("Missing required variables in newdata: %s", paste(missing_terms, collapse = ", ")))
  }
}

