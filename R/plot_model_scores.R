#' Plot Model Scoring Outcomes
#'
#' Visualizes the scoring results using various plot types.
#'
#' @param score_results A tibble of scoring results (e.g., returned by score_model).
#' @param type The type of plot to create. Options include "bar", "density", "calibration", "radar", "heatmap".
#' @param facet_by A variable to facet the plot by (default is "metric").
#' @param theme A ggplot2 theme to apply ("minimal", "classic", "dark").
#' @param color_palette Color palette to use (default: "viridis").
#' @param ... Additional arguments passed to ggplot2 functions.
#'
#' @return A ggplot object.
#'
#' @export
#' @importFrom ggplot2 theme_minimal theme_classic theme_dark ggplot aes geom_bar scale_fill_viridis_d labs geom_tile scale_fill_viridis_c theme element_text facet_wrap ggplot
#' @importFrom tidyr pivot_wider
#' @importFrom ggradar ggradar
#' @importFrom magrittr %>%
#' @importFrom stats as.formula
plot_model_scores <- function(score_results,
                              type = c("bar", "density", "calibration", "radar", "heatmap"),
                              facet_by = "metric",
                              theme = "minimal",
                              color_palette = "viridis",
                              ...) {
  type <- match.arg(type)

  theme_func <- switch(theme,
                       "minimal" = ggplot2::theme_minimal(),
                       "classic" = ggplot2::theme_classic(),
                       "dark" = ggplot2::theme_dark(),
                       ggplot2::theme_minimal())

  base_plot <- ggplot2::ggplot(score_results)

  plot <- switch(type,
                 "bar" = {
                   base_plot +
                     ggplot2::aes(x = model, y = value, fill = metric) +
                     ggplot2::geom_bar(stat = "identity", position = "dodge") +
                     ggplot2::scale_fill_viridis_d() +
                     theme_func +
                     ggplot2::labs(y = "Score Value", x = "Model")
                 },
                 "heatmap" = {
                   base_plot +
                     ggplot2::aes(x = model, y = metric, fill = value) +
                     ggplot2::geom_tile() +
                     ggplot2::scale_fill_viridis_c() +
                     theme_func +
                     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
                 },
                 "radar" = {
                   if (!requireNamespace("ggradar", quietly = TRUE)) {
                     stop("Package 'ggradar' is needed for radar plots. Please install it.")
                   }
                   score_results %>%
                     tidyr::pivot_wider(names_from = metric, values_from = value) %>%
                     ggradar::ggradar(...)
                 },
                 stop("Selected plot type is not supported.")
  )

  if (facet_by != "none" && type != "radar") {
    plot <- plot + ggplot2::facet_wrap(as.formula(paste("~", facet_by)), scales = "free_y")
  }

  return(plot)
}
