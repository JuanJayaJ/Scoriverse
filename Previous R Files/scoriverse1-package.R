#' Internal Package Metadata
#'
#' This file handles necessary imports and global variable declarations
#' to prevent R CMD check warnings.
#'
#' @keywords internal
"_PACKAGE"

## Required Imports
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_abline
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom stats predict

NULL

# Suppress R CMD check notes about global variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("observed", "predicted"))
}
