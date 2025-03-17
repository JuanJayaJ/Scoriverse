#' Compute Scoring Metrics for a Model
#'
#' Computes a set of proper scoring metrics for a model using standardized predictions.
#'
#' @param model A fitted model.
#' @param newdata A data frame containing new observations.
#' @param truth The observed outcomes.
#' @param metrics A vector of metric names.
#' @param alpha Significance level used in interval calculations (default: 0.05).
#' @param ... Additional arguments passed to prediction methods.
#'
#' @return A tibble with scoring results, including the metric name, value, type, direction,
#' model type, timestamp, and number of observations.
#'
#' @export
#' @importFrom dplyr bind_rows mutate
#' @importFrom stats formula model.matrix predict
score_model <- function(model, newdata, truth,
                        metrics = c("logarithmic", "brier", "crps", "energy",
                                    "dawid_sebastiani", "spherical", "ranked_probability",
                                    "interval", "multiclass_brier", "zero_one"),
                        alpha = 0.05,
                        ...) {
  if (!is.data.frame(newdata)) stop("newdata must be a data frame.")
  if (length(truth) != nrow(newdata)) stop("Length of truth must match number of rows in newdata.")
  if (!is.numeric(truth) && !is.factor(truth)) stop("truth must be numeric or factor.")
  if (!all(metrics %in% available_metrics())) {
    invalid_metrics <- setdiff(metrics, available_metrics())
    stop("Invalid metrics specified: ", paste(invalid_metrics, collapse = ", "))
  }

  preds <- tryCatch({
    predict_unified(model, newdata, ...)
  }, error = function(e) {
    stop("Error in prediction: ", e$message)
  })

  scores <- list()
  for (metric in metrics) {
    scores[[metric]] <- tryCatch({
      compute_score(metric, preds, truth, alpha)
    }, error = function(e) {
      warning("Skipping ", metric, ": ", e$message)
      NULL
    })
  }
  scores <- scores[!sapply(scores, is.null)]

  score_result <- dplyr::bind_rows(scores)

  # Ensure the required columns exist
  if (!"metric" %in% colnames(score_result)) {
    score_result$metric <- NA
  }
  if (!"value" %in% colnames(score_result)) {
    score_result$value <- NA
  }

  score_result %>%
    dplyr::mutate(
      model_type = class(model)[1],
      timestamp = Sys.time(),
      n_observations = length(truth)
    )
}
