#' Visualize Predictions vs. Observed Values
#'
#' Creates a ggplot2 scatter plot of observed values versus predicted values.
#' Optionally includes error bars for prediction intervals if `lower` and `upper` bounds are provided.
#' Adds a reference line (y = x) to help assess prediction quality.
#'
#' @param new_data A data frame or NULL. If provided, used to extract an x-axis variable.
#'   Otherwise, the observed values are used on both axes.
#' @param y_true A numeric vector of observed values.
#' @param predictions A numeric vector of predicted values.
#' @param lower Optional numeric vector of lower bounds for the prediction intervals.
#' @param upper Optional numeric vector of upper bounds for the prediction intervals.
#' @param title Character string for the plot title.
#' @param xlab Character string for the x-axis label (default is "Observed").
#' @param ylab Character string for the y-axis label (default is "Predicted").
#' @param point_color Color for the data points.
#' @param point_size Numeric value indicating the point size.
#' @param errorbar_width Numeric value controlling the width of error bars.
#' @param theme_custom A ggplot2 theme object to customize the appearance (default is theme_minimal()).
#'
#' @return A ggplot2 object.
#' @export
visualize_predictions <- function(new_data = NULL,
                                  y_true,
                                  predictions,
                                  lower = NULL,
                                  upper = NULL,
                                  title = "Observed vs. Predicted",
                                  xlab = "Observed",
                                  ylab = "Predicted",
                                  point_color = "blue",
                                  point_size = 2,
                                  errorbar_width = 0.2,
                                  theme_custom = ggplot2::theme_minimal()) {
  # Argument validation
  if (!is.numeric(y_true)) stop("`y_true` must be numeric.")
  if (!is.numeric(predictions)) stop("`predictions` must be numeric.")
  if (length(y_true) != length(predictions)) stop("`y_true` and `predictions` must have the same length.")

  df <- data.frame(
    observed = y_true,
    predicted = predictions
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = observed, y = predicted)) +
    ggplot2::geom_point(color = point_color, size = point_size) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    theme_custom

  # Add error bars if provided
  if (!is.null(lower) && !is.null(upper)) {
    if (!is.numeric(lower) || !is.numeric(upper)) {
      stop("`lower` and `upper` must be numeric vectors.")
    }
    if (length(lower) != length(y_true) || length(upper) != length(y_true)) {
      stop("`lower` and `upper` must have the same length as `y_true`.")
    }
    df$lower <- lower
    df$upper <- upper
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = errorbar_width, color = point_color)
  }

  return(p)
}

#' Visualize Residuals
#'
#' Creates a plot of residuals (observed minus predicted) to help diagnose model fit.
#' Includes a horizontal reference line at zero.
#'
#' @param y_true A numeric vector of observed values.
#' @param predictions A numeric vector of predicted values.
#' @param title Character string for the plot title.
#' @param xlab Character string for the x-axis label (default is "Predicted").
#' @param ylab Character string for the y-axis label (default is "Residuals").
#' @param point_color Color for the residual points.
#' @param point_size Numeric value for the point size.
#' @param theme_custom A ggplot2 theme object to customize the appearance (default is theme_minimal()).
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
                                theme_custom = ggplot2::theme_minimal()) {
  # Argument validation
  if (!is.numeric(y_true)) stop("`y_true` must be numeric.")
  if (!is.numeric(predictions)) stop("`predictions` must be numeric.")
  if (length(y_true) != length(predictions)) stop("`y_true` and `predictions` must have the same length.")

  residuals <- y_true - predictions
  df <- data.frame(
    predicted = predictions,
    residuals = residuals
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = predicted, y = residuals)) +
    ggplot2::geom_point(color = point_color, size = point_size) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    theme_custom

  return(p)
}
