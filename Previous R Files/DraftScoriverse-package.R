#' DraftScoriverse: Probabilistic Prediction Scoring and Evaluation Framework
#'
#' DraftScoriverse provides a unified framework for generating probabilistic model predictions,
#' applying proper scoring rules, and visualizing model performance. It supports a variety of model
#' types, including:
#' \itemize{
#'   \item Bayesian models via \code{brms}.
#'   \item Generalized Additive Models (GAMs).
#'   \item Classical models like \code{lm} and \code{glm}.
#'   \item Machine learning models such as \code{randomForest}, \code{xgboost}, and \code{tidymodels} workflows.
#' }
#'
#' Key features:
#' \itemize{
#'   \item Standardized prediction extraction across model types.
#'   \item Scoring rule implementations: CRPS, Logarithmic Score, Brier Score, Interval Score, Dawid–Sebastiani Score (DSS).
#'   \item A scoring pipeline via \code{\link{run_scoriverse}} for streamlined evaluation.
#'   \item Posterior predictive scoring for sample-based approaches.
#'   \item Visualization tools for predictions and residuals.
#' }
#'
#' @docType package
#' @name DraftScoriverse
#' @import ggplot2
#' @importFrom stats predict family residuals df.residual
#' @importFrom scoringRules crps_sample crps_norm logs_norm
#' @importFrom utils globalVariables
"_PACKAGE"

# ----------------------------
# Global Variables for R CMD Check
# ----------------------------
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("observed", "predicted", "lower", "upper", "residuals"))
}

