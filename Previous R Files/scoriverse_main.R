#' Execute the Scoriverse Workflow
#'
#' This function streamlines the process of generating predictions from a trained model,
#' evaluating them using scoring rules, and optionally visualizing the results.
#'
#' @param model A trained model object.
#' @param new_data (Optional) A data frame containing new observations for prediction.
#' @param y_true A numeric vector representing observed values.
#' @param score_metrics A character vector specifying which metrics to compute.
#'   Supported options: "crps", "log_score", "brier", "interval", "dss".
#' @param visualize Logical. If TRUE, a plot comparing observed vs. predicted values is created.
#' @param ... Additional parameters passed to underlying functions.
#'
#' @return A list containing:
#'   \describe{
#'     \item{predictions}{Numeric vector of predicted values.}
#'     \item{scores}{List of computed metrics based on user selection.}
#'     \item{plot}{(Optional) A ggplot2 visualization if visualize = TRUE.}
#'   }
#'
#' @examples
#' \dontrun{
#'   model_fit <- lm(mpg ~ wt + hp, data = mtcars)
#'   result <- run_scoriverse(
#'     model = model_fit,
#'     new_data = mtcars,
#'     y_true = mtcars$mpg,
#'     score_metrics = c("crps", "log_score", "dss"),
#'     visualize = TRUE
#'   )
#'   print(result$scores)
#'   print(result$plot)
#' }
#'
#' @export
run_scoriverse <- function(model, new_data = NULL, y_true,
                           score_metrics = c("crps", "log_score", "brier", "interval", "dss"),
                           visualize = FALSE, ...) {
  message("Generating predictions...")
  predictions <- extract_predictions(model, new_data, ...)

  if (is.null(predictions)) {
    stop("Prediction extraction failed. Check model compatibility.")
  }

  message("Calculating evaluation metrics...")
  scores <- list()
  params <- list(...)

  if ("crps" %in% score_metrics) {
    if (!("pred_sd" %in% names(params))) {
      warning("CRPS calculation skipped - missing standard deviation.")
    } else {
      scores$crps <- compute_crps(y_true, pred_mean = predictions, pred_sd = params[["pred_sd"]])
    }
  }

  if ("log_score" %in% score_metrics) {
    if (!("pred_sd" %in% names(params))) {
      warning("Skipping log score - standard deviation not provided.")
    } else {
      scores$log_score <- compute_log_score(y_true, predictions, params[["pred_sd"]])
    }
  }

  if ("brier" %in% score_metrics) {
    if (!("pred_prob" %in% names(params))) {
      warning("Brier score skipped - predicted probabilities missing.")
    } else {
      scores$brier <- compute_brier_score(y_true, params[["pred_prob"]])
    }
  }

  if ("interval" %in% score_metrics) {
    if (!all(c("lower", "upper") %in% names(params))) {
      warning("Interval score requires both lower and upper bounds - skipping.")
    } else {
      scores$interval <- compute_interval_score(y_true, params[["lower"]], params[["upper"]])
    }
  }

  if ("dss" %in% score_metrics) {
    if (!("pred_sd" %in% names(params))) {
      warning("Skipping DSS calculation - missing standard deviation.")
    } else {
      scores$dss <- compute_dss(y_true, predictions, params[["pred_sd"]])
    }
  }

  plot_obj <- NULL
  if (visualize) {
    message("Creating visualization...")
    plot_obj <- visualize_predictions(new_data, y_true, predictions)
  }

  list(
    predictions = predictions,
    scores = scores,
    plot = plot_obj
  )
}
