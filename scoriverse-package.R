#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_abline
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom patchwork plot_layout
#' @importFrom patchwork plot_annotation
#' @importFrom patchwork wrap_plots
#' @importFrom stats predict
## usethis namespace: end
NULL

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("observed", "predicted", "lower", "upper", "x", "y_true"))
}
