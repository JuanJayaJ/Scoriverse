#' Execute the Scoring Pipeline
#'
#' Streamlines the process of generating predictions from a trained model,
#' evaluating them using scoring rules, and optionally visualizing the results.
#'
#' @param model A trained model object.
#' @param new_data Optional data frame with new observations for prediction.
#' @param y_true A numeric vector representing observed outcome values.
#' @param score_metrics A character vector specifying which metrics to compute.
#'   Supported options: "crps", "log_score", "brier", "interval", "dss".
#' @param visualize Logical. If TRUE, a plot comparing observed vs. predicted values is created.
#' @param ... Additional parameters passed to the underlying prediction extraction and scoring functions.
#'
#' @return A list containing:
#'   \describe{
#'     \item{predictions}{Numeric vector of predicted values.}
#'     \item{scores}{List of computed metrics based on user selection.}
#'     \item{plot}{(Optional) A ggplot2 visualization if visualize = TRUE.}
#'     \item{meta}{Optional metadata about the model and scoring process.}
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
  params <- list(...)
  scores <- list()

  # Loop over each requested score metric and apply scoring via wrap_scoring
  for (metric in score_metrics) {
    score_result <- tryCatch({
      do.call(wrap_scoring, c(
        list(score_type = metric, y_true = y_true, predictions = preds),
        params
      ))
    }, error = function(e) {
      warning(sprintf("Skipping %s calculation: %s", metric, e$message))
      NULL
    })
    scores[[metric]] <- score_result
  }

  # Visualization
  plot_obj <- NULL
  if (visualize) {
    plot_obj <- visualize_predictions(new_data, y_true, preds)
  }

  # Add metadata (useful for reproducibility)
  meta_info <- list(
    model_class = class(model),
    n_observations = length(preds),
    timestamp = Sys.time()
  )

  return(list(predictions = preds, scores = scores, plot = plot_obj, meta = meta_info))
}
