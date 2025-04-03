#' Visualize Predictions vs. Observed Values
#'
#' Creates a ggplot2 scatter plot of observed versus predicted values.
#' Optionally includes error bars or a shaded region if lower and upper prediction
#' intervals are provided. A reference line (y = x) is added to assess prediction quality.
#'
#' @param new_data A data frame or NULL. If provided, used to extract an x-axis variable;
#'   otherwise, the observed values are used on both axes.
#' @param y_true A numeric vector of observed values.
#' @param predictions A numeric vector of predicted values.
#' @param lower Optional numeric vector of lower bounds for the prediction intervals.
#' @param upper Optional numeric vector of upper bounds for the prediction intervals.
#' @param title Plot title.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param point_color Color for the data points.
#' @param point_size Numeric value for the point size.
#' @param theme_custom A ggplot2 theme object for customizing the appearance (default is theme_minimal()).
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
                                  theme_custom = ggplot2::theme_minimal()) {
  df <- data.frame(
    observed = y_true,
    predicted = predictions
  )
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = observed, y = predicted)) +
    ggplot2::geom_point(color = point_color, size = point_size) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    theme_custom
  
  if (!is.null(lower) && !is.null(upper)) {
    df$lower <- lower
    df$upper <- upper
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.2, color = point_color)
  }
  
  return(p)
}

#' Visualize Residuals
#'
#' Creates a scatter plot of residuals (observed minus predicted) versus predicted values,
#' including a horizontal line at zero for reference.
#'
#' @param y_true A numeric vector of observed values.
#' @param predictions A numeric vector of predicted values.
#' @param title Plot title.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param point_color Color for the residual points.
#' @param point_size Numeric value for the point size.
#' @param theme_custom A ggplot2 theme object for customizing the appearance (default is theme_minimal()).
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
