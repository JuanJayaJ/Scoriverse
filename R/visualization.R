#' Visualize Predictions vs. Observed Values
#'
#' Creates a ggplot2 scatter plot of observed values versus predicted values.
#' Optionally includes error bars or a shaded ribbon if lower and upper prediction
#' intervals are provided. A reference line (y = x) is added to help assess prediction quality.
#'
#' @param new_data A data frame or NULL. If provided and xvar is specified, its xvar column is used for the x-axis.
#' @param y_true A numeric vector of observed values.
#' @param predictions A numeric vector of predicted values.
#' @param lower Optional numeric vector of lower bounds for the prediction intervals.
#' @param upper Optional numeric vector of upper bounds for the prediction intervals.
#' @param xvar Optional. A character string specifying the column name in new_data to be used as the x-axis variable.
#' If NULL, y_true is used for the x-axis.
#' @param title Character string for the plot title.
#' @param xlab Character string for the x-axis label.
#' @param ylab Character string for the y-axis label.
#' @param point_color Color for the data points.
#' @param point_size Numeric value indicating the point size.
#' @param theme_custom A ggplot2 theme object to customize the appearance (default is theme_minimal()).
#' @param add_metrics Logical. If TRUE, adds performance metrics annotation to the plot.
#'
#' @return A ggplot2 object.
#' @export
visualize_predictions <- function(new_data = NULL,
                                  y_true,
                                  predictions,
                                  lower = NULL,
                                  upper = NULL,
                                  xvar = NULL,
                                  title = "Observed vs. Predicted",
                                  xlab = "Observed",
                                  ylab = "Predicted",
                                  point_color = "blue",
                                  point_size = 2,
                                  theme_custom = ggplot2::theme_minimal(),
                                  add_metrics = TRUE) {

  # Input validation
  if (!is.numeric(y_true)) {
    stop("y_true must be a numeric vector")
  }

  if (!is.numeric(predictions)) {
    stop("predictions must be a numeric vector")
  }

  if (length(y_true) != length(predictions)) {
    warning("Length of y_true and predictions differ. Using the minimum length.")
    min_length <- min(length(y_true), length(predictions))
    y_true <- y_true[seq_len(min_length)]
    predictions <- predictions[seq_len(min_length)]
  }

  # If new_data and xvar are provided, use that column for the x-axis; otherwise, use y_true
  if (!is.null(new_data) && !is.null(xvar) && xvar %in% names(new_data)) {
    x_values <- new_data[[xvar]]
    if (length(x_values) != length(y_true)) {
      warning("Length of x_values and y_true differ. Using the minimum length.")
      min_length <- min(length(x_values), length(y_true))
      x_values <- x_values[seq_len(min_length)]
      y_true <- y_true[seq_len(min_length)]
      predictions <- predictions[seq_len(min_length)]
    }
  } else {
    x_values <- y_true
  }

  df <- data.frame(
    x = x_values,
    observed = y_true,
    predicted = predictions
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = predicted)) +
    ggplot2::geom_point(color = point_color, size = point_size, alpha = 0.7) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    theme_custom

  if (!is.null(lower) && !is.null(upper)) {
    # Ensure lengths match
    if (length(lower) != length(y_true) || length(upper) != length(y_true)) {
      warning("Length of lower/upper bounds and y_true differ. Using the minimum length.")
      min_length <- min(length(lower), length(upper), length(y_true))
      lower <- lower[seq_len(min_length)]
      upper <- upper[seq_len(min_length)]
      df <- df[seq_len(min_length), ]
    }

    df$lower <- lower
    df$upper <- upper

    # If an x variable is provided, use a ribbon to show the uncertainty region
    if (!is.null(xvar) && !is.null(new_data) && xvar %in% names(new_data)) {
      # Sort by x for proper ribbon drawing
      df <- df[order(df$x), ]
      p <- p + ggplot2::geom_ribbon(data = df,
                                    ggplot2::aes(x = x, y = predicted, ymin = lower, ymax = upper),
                                    fill = point_color, alpha = 0.2)
    } else {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                      width = 0.2, color = point_color, alpha = 0.5)
    }
  }

  # Add performance metrics
  if (add_metrics) {
    rmse <- sqrt(mean((y_true - predictions)^2))
    mae <- mean(abs(y_true - predictions))
    r_squared <- cor(y_true, predictions)^2

    metrics_text <- sprintf(
      "RMSE: %.3f\nMAE: %.3f\nR²: %.3f",
      rmse, mae, r_squared
    )

    # Position the annotation in the top left
    x_pos <- min(x_values) + 0.05 * (max(x_values) - min(x_values))
    y_pos <- max(predictions) - 0.05 * (max(predictions) - min(predictions))

    p <- p + ggplot2::annotate(
      "text", x = x_pos, y = y_pos,
      label = metrics_text, hjust = 0, vjust = 1,
      size = 3.5, color = "black"
    )
  }

  return(p)
}

#' Visualize Residuals
#'
#' Creates a plot of residuals (observed minus predicted) to help diagnose model fit.
#' This function plots residuals versus predicted values and includes a horizontal line at zero.
#' Optionally, a smoothing line (loess) can be added.
#'
#' @param y_true A numeric vector of observed values.
#' @param predictions A numeric vector of predicted values.
#' @param title Character string for the plot title.
#' @param xlab Character string for the x-axis label.
#' @param ylab Character string for the y-axis label.
#' @param point_color Color for the residual points.
#' @param point_size Numeric value for the point size.
#' @param smooth Logical. If TRUE, adds a loess smoothing line.
#' @param theme_custom A ggplot2 theme object to customize the appearance (default is theme_minimal()).
#' @param standardize Logical. If TRUE, standardizes residuals.
#'
#' @return A ggplot2 object.
#' @export
visualize_residuals <- function(y_true,
                                predictions,
                                title = "Residuals vs. Predicted",
                                xlab = "Predicted",
                                ylab = "Residuals",
                                point_color = "darkgreen",
                                point_size = 2,
                                smooth = FALSE,
                                theme_custom = ggplot2::theme_minimal(),
                                standardize = FALSE) {

  # Input validation
  if (!is.numeric(y_true)) {
    stop("y_true must be a numeric vector")
  }

  if (!is.numeric(predictions)) {
    stop("predictions must be a numeric vector")
  }

  if (length(y_true) != length(predictions)) {
    warning("Length of y_true and predictions differ. Using the minimum length.")
    min_length <- min(length(y_true), length(predictions))
    y_true <- y_true[seq_len(min_length)]
    predictions <- predictions[seq_len(min_length)]
  }

  residuals <- y_true - predictions

  if (standardize) {
    # Calculate standardized residuals
    sigma <- sqrt(mean(residuals^2))
    if (sigma > 0) {
      residuals <- residuals / sigma
      ylab <- "Standardized Residuals"
    } else {
      warning("Cannot standardize residuals (sigma = 0). Using raw residuals.")
    }
  }

  df <- data.frame(
    predicted = predictions,
    residuals = residuals
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = predicted, y = residuals)) +
    ggplot2::geom_point(color = point_color, size = point_size, alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    theme_custom

  if (smooth) {
    p <- p + ggplot2::geom_smooth(method = "loess", se = TRUE, color = "black", fill = "lightgray")
  }

  # Add horizontal reference lines at +/- 2 for standardized residuals
  if (standardize) {
    p <- p + ggplot2::geom_hline(yintercept = c(-2, 2), linetype = "dotted", color = "blue")
  }

  return(p)
}

#' Create Diagnostic Plots Panel
#'
#' Creates a panel of diagnostic plots for model evaluation.
#'
#' @param y_true A numeric vector of observed values.
#' @param predictions A numeric vector of predicted values.
#' @param lower Optional numeric vector of lower bounds for the prediction intervals.
#' @param upper Optional numeric vector of upper bounds for the prediction intervals.
#' @param title Character string for the overall plot title.
#' @param theme_custom A ggplot2 theme object to customize the appearance.
#'
#' @return A ggplot2 object arranged in a grid.
#' @export
#' @importFrom patchwork plot_layout
create_diagnostic_panel <- function(y_true,
                                    predictions,
                                    lower = NULL,
                                    upper = NULL,
                                    title = "Model Diagnostic Plots",
                                    theme_custom = ggplot2::theme_minimal()) {

  # Observed vs Predicted plot
  p1 <- visualize_predictions(
    y_true = y_true,
    predictions = predictions,
    lower = lower,
    upper = upper,
    title = "Observed vs. Predicted",
    theme_custom = theme_custom
  )

  # Residuals plot
  p2 <- visualize_residuals(
    y_true = y_true,
    predictions = predictions,
    title = "Residuals vs. Predicted",
    smooth = TRUE,
    theme_custom = theme_custom
  )

  # Q-Q plot of residuals
  residuals <- y_true - predictions
  qq_data <- data.frame(
    theoretical = qqnorm(residuals, plot.it = FALSE)$x,
    sample = qqnorm(residuals, plot.it = FALSE)$y
  )
  p3 <- ggplot2::ggplot(qq_data, ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_point(color = "purple", alpha = 0.7) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Q-Q Plot of Residuals",
                  x = "Theoretical Quantiles",
                  y = "Sample Quantiles") +
    theme_custom

  # Histogram of residuals
  p4 <- ggplot2::ggplot(data.frame(residuals = residuals), ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
    theme_custom

  # Combine plots
  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined_plot <- (p1 + p2) / (p3 + p4) +
      patchwork::plot_layout(guides = "collect") +
      patchwork::plot_annotation(
        title = title,
        theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold"))
      )
    return(combined_plot)
  } else {
    warning("Package 'patchwork' is required for panel layout. Returning only observed vs predicted plot.")
    return(p1)
  }
}

#' Plot Scoring Metric Results
#'
#' Creates visualizations for one or more scoring metrics across observations.
#'
#' @param scores A list of scoring metric results as returned by run_scoriverse().
#' @param metric_names Character vector of metric names to plot (default is all available).
#' @param y_true Optional numeric vector of observed values for reference.
#' @param predictions Optional numeric vector of predicted values for reference.
#' @param arrange_plots Logical. If TRUE, arranges plots in a grid.
#' @param theme_custom A ggplot2 theme object to customize the appearance.
#'
#' @return A list of ggplot2 objects or a combined patchwork object if arrange_plots is TRUE.
#' @export
plot_scoring_metrics <- function(scores,
                                 metric_names = NULL,
                                 y_true = NULL,
                                 predictions = NULL,
                                 arrange_plots = TRUE,
                                 theme_custom = ggplot2::theme_minimal()) {

  # If no specific metrics requested, use all available
  if (is.null(metric_names)) {
    metric_names <- names(scores)
  } else {
    # Validate requested metrics exist
    invalid_metrics <- setdiff(metric_names, names(scores))
    if (length(invalid_metrics) > 0) {
      warning("The following metrics were not found and will be ignored: ",
              paste(invalid_metrics, collapse = ", "))
      metric_names <- intersect(metric_names, names(scores))
    }
  }

  if (length(metric_names) == 0) {
    stop("No valid scoring metrics found to plot")
  }

  plot_list <- list()

  # Create a plot for each metric
  for (metric in metric_names) {
    df <- data.frame(
      index = seq_along(scores[[metric]]),
      score = scores[[metric]]
    )

    # Add observed values and predictions if available
    if (!is.null(y_true) && !is.null(predictions)) {
      min_length <- min(nrow(df), length(y_true), length(predictions))
      df$observed <- y_true[seq_len(min_length)]
      df$predicted <- predictions[seq_len(min_length)]
      df <- df[seq_len(min_length), ]
    }

    # Basic plot for the metric
    p <- ggplot2::ggplot(df, ggplot2::aes(x = index, y = score)) +
      ggplot2::geom_line(color = "steelblue") +
      ggplot2::geom_point(color = "steelblue", size = 1) +
      ggplot2::labs(
        title = paste0(toupper(metric), " Score"),
        x = "Observation Index",
        y = paste0(toupper(metric), " Value")
      ) +
      theme_custom

    # Add overall metric mean
    mean_score <- mean(scores[[metric]], na.rm = TRUE)
    p <- p + ggplot2::geom_hline(
      yintercept = mean_score,
      linetype = "dashed",
      color = "red"
    ) +
      ggplot2::annotate(
        "text",
        x = max(df$index) * 0.9,
        y = mean_score * 1.1,
        label = sprintf("Mean: %.3f", mean_score),
        color = "red"
      )

    plot_list[[metric]] <- p
  }

  # Either return the list of plots or arrange them in a grid
  if (arrange_plots && length(plot_list) > 1 && requireNamespace("patchwork", quietly = TRUE)) {
    n_plots <- length(plot_list)
    ncol <- min(2, n_plots)
    nrow <- ceiling(n_plots / ncol)

    combined_plot <- patchwork::wrap_plots(plot_list, ncol = ncol) +
      patchwork::plot_annotation(
        title = "Scoring Metrics Evaluation",
        theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold"))
      )

    return(combined_plot)
  } else {
    return(plot_list)
  }
}
