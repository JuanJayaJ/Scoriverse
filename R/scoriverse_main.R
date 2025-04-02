#' Run the Scoriverse Workflow
#'
#' This function provides a unified workflow to extract predictions from a fitted model,
#' compute evaluation scores using proper scoring rules, and optionally visualize the results.
#'
#' @param model A fitted model object.
#' @param new_data Optional. A data frame with new data for prediction extraction.
#' @param y_true A numeric vector of observed values.
#' @param score_metrics A character vector indicating which scoring metrics to compute.
#'   Options include "crps", "log_score", "brier", "interval", and "dss".
#' @param visualize Logical. If TRUE, generates plots comparing observed and predicted values.
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return A list containing:
#'   \describe{
#'     \item{predictions}{A numeric vector of model predictions.}
#'     \item{scores}{A list of computed scores based on the specified metrics.}
#'     \item{plot}{(Optional) A ggplot object if \code{visualize = TRUE}.}
#'   }
#'
#' @examples
#' \dontrun{
#'   lm_mod <- lm(mpg ~ wt + hp, data = mtcars)
#'   result <- run_scoriverse(
#'     model = lm_mod,
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
  message("Extracting predictions...")
  predictions <- extract_predictions(model, new_data, ...)

  if (is.null(predictions)) {
    stop("Failed to extract predictions from the provided model.")
  }

  # Ensure y_true is a numeric vector
  y_true <- as.numeric(y_true)

  # Ensure predictions length matches y_true length
  n <- length(y_true)
  if (length(predictions) != n) {
    if (length(predictions) > n) {
      predictions <- predictions[seq_len(n)]
    } else {
      predictions <- c(predictions, rep(predictions[length(predictions)], n - length(predictions)))
    }
  }

  message("Computing evaluation scores...")
  scores <- list()
  dots <- list(...)

  if ("crps" %in% score_metrics) {
    if (!("pred_sd" %in% names(dots))) {
      warning("No standard deviation provided for CRPS computation. Skipping CRPS.")
    } else {
      pred_sd <- dots[["pred_sd"]]
      # Ensure pred_sd has correct length
      if (length(pred_sd) != n) {
        if (length(pred_sd) > n) {
          pred_sd <- pred_sd[seq_len(n)]
        } else {
          pred_sd <- c(pred_sd, rep(pred_sd[length(pred_sd)], n - length(pred_sd)))
        }
      }
      scores$crps <- compute_crps(y_true, pred_mean = predictions, pred_sd = pred_sd)
    }
  }

  if ("log_score" %in% score_metrics) {
    if (!("pred_sd" %in% names(dots))) {
      warning("No standard deviation provided for log score computation. Skipping log score.")
    } else {
      pred_sd <- dots[["pred_sd"]]
      # Ensure pred_sd has correct length
      if (length(pred_sd) != n) {
        if (length(pred_sd) > n) {
          pred_sd <- pred_sd[seq_len(n)]
        } else {
          pred_sd <- c(pred_sd, rep(pred_sd[length(pred_sd)], n - length(pred_sd)))
        }
      }
      scores$log_score <- compute_log_score(y_true, predictions, pred_sd)
    }
  }

  if ("brier" %in% score_metrics) {
    if (!("pred_prob" %in% names(dots))) {
      warning("No predicted probabilities provided for Brier score computation. Skipping Brier score.")
    } else {
      pred_prob <- dots[["pred_prob"]]
      # Ensure pred_prob has correct length
      if (length(pred_prob) != n) {
        if (length(pred_prob) > n) {
          pred_prob <- pred_prob[seq_len(n)]
        } else {
          pred_prob <- c(pred_prob, rep(pred_prob[length(pred_prob)], n - length(pred_prob)))
        }
      }
      scores$brier <- compute_brier_score(y_true, pred_prob)
    }
  }

  if ("interval" %in% score_metrics) {
    if (!all(c("lower", "upper") %in% names(dots))) {
      warning("Lower and upper bounds not provided for interval score computation. Skipping interval score.")
    } else {
      lower <- dots[["lower"]]
      upper <- dots[["upper"]]
      # Ensure bounds have correct lengths
      if (length(lower) != n) {
        if (length(lower) > n) {
          lower <- lower[seq_len(n)]
        } else {
          lower <- c(lower, rep(lower[length(lower)], n - length(lower)))
        }
      }
      if (length(upper) != n) {
        if (length(upper) > n) {
          upper <- upper[seq_len(n)]
        } else {
          upper <- c(upper, rep(upper[length(upper)], n - length(upper)))
        }
      }
      scores$interval <- compute_interval_score(y_true, lower, upper)
    }
  }

  if ("dss" %in% score_metrics) {
    if (!("pred_sd" %in% names(dots))) {
      warning("No standard deviation provided for DSS computation. Skipping DSS.")
    } else {
      pred_sd <- dots[["pred_sd"]]
      # Ensure pred_sd has correct length
      if (length(pred_sd) != n) {
        if (length(pred_sd) > n) {
          pred_sd <- pred_sd[seq_len(n)]
        } else {
          pred_sd <- c(pred_sd, rep(pred_sd[length(pred_sd)], n - length(pred_sd)))
        }
      }
      scores$dss <- compute_dss(y_true, predictions, pred_sd)
    }
  }

  plot_obj <- NULL
  if (visualize) {
    message("Generating visualization...")
    # Get prediction intervals if available
    lower <- if ("lower" %in% names(dots)) dots[["lower"]] else NULL
    upper <- if ("upper" %in% names(dots)) dots[["upper"]] else NULL

    # Create visualization
    plot_obj <- visualize_predictions(new_data, y_true, predictions, lower, upper)
  }

  # Ensure we return properly structured results
  result <- list(
    predictions = as.numeric(predictions),
    scores = scores,
    plot = plot_obj
  )

  return(result)
}
