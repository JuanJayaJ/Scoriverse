#' Execute the Scoriverse Workflow
#'
#' This function streamlines the process of generating predictions from a trained model,
#' evaluating them using scoring rules, and visualizing the results.
#'
#' @param model A trained model object.
#' @param new_data Optional data frame with new observations for prediction.
#' @param y_true A numeric vector representing observed outcome values.
#' @param score_metrics A character vector specifying which metrics to compute.
#'   Supported options: "crps", "log_score", "brier", "interval", "dss".
#' @param visualize Logical. If TRUE, a plot comparing observed vs. predicted values is created.
#' @param ... Additional parameters passed to the underlying prediction extraction.
#'
#' @return A list containing:
#'   \describe{
#'     \item{predictions}{Numeric vector of predicted values.}
#'     \item{scores}{List of computed metrics based on user selection.}
#'     \item{plot}{(Optional) A ggplot2 visualization if visualize = TRUE.}
#'   }
#' @export
run_scoriverse <- function(model, new_data = NULL, y_true,
                                 score_metrics = c("crps", "log_score", "brier", "interval", "dss"),
                                 visualize = FALSE, ...) {
  message("Generating predictions...")
  preds <- extract_predictions(model, new_data = new_data, ...)
  if (is.null(preds)) {
    stop("Prediction extraction failed. Check model compatibility.")
  }

  message("Calculating evaluation metrics...")
  scores <- list()
  params <- list(...)

  if ("crps" %in% score_metrics) {
    if (!("pred_sd" %in% names(params))) {
      warning("CRPS calculation skipped - missing standard deviation.")
    } else {
      scores$crps <- compute_crps(y_true, pred_mean = preds, pred_sd = params[["pred_sd"]])
    }
  }

  if ("log_score" %in% score_metrics) {
    if (!("pred_sd" %in% names(params))) {
      warning("Log score calculation skipped - missing standard deviation.")
    } else {
      scores$log_score <- compute_log_score(y_true, preds, params[["pred_sd"]])
    }
  }

  if ("brier" %in% score_metrics) {
    if (!("pred_prob" %in% names(params))) {
      warning("Brier score calculation skipped - missing predicted probabilities.")
    } else {
      scores$brier <- compute_brier_score(y_true, params[["pred_prob"]])
    }
  }

  if ("interval" %in% score_metrics) {
    if (!all(c("lower", "upper") %in% names(params))) {
      warning("Interval score calculation skipped - missing bounds.")
    } else {
      scores$interval <- compute_interval_score(y_true, params[["lower"]], params[["upper"]])
    }
  }

  if ("dss" %in% score_metrics) {
    if (!("pred_sd" %in% names(params))) {
      warning("DSS calculation skipped - missing standard deviation.")
    } else {
      scores$dss <- compute_dss(y_true, preds, params[["pred_sd"]])
    }
  }

  plot_obj <- NULL
  if (visualize) {
    plot_obj <- visualize_predictions(new_data, y_true, preds)
  }

  return(list(predictions = preds, scores = scores, plot = plot_obj))
}
