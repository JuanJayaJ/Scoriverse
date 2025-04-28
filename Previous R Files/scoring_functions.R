# ----------------------------
# Argument Checking Helpers
# ----------------------------
.check_numeric <- function(x, name) {
  if (!is.numeric(x)) stop(sprintf("`%s` must be numeric.", name), call. = FALSE)
}

.check_length <- function(x, y, name_x, name_y) {
  if (length(x) != length(y)) {
    stop(sprintf("`%s` and `%s` must have the same length.", name_x, name_y), call. = FALSE)
  }
}

# ----------------------------
# CRPS Scoring
# ----------------------------
#' Compute Continuous Ranked Probability Score (CRPS)
#'
#' Computes CRPS for observed values with either:
#' - Predictive samples (`pred_matrix`), or
#' - Normal distribution using `pred_mean` and `pred_sd`.
#'
#' @param y A numeric vector of observed values.
#' @param pred_mean Predicted means (required if `pred_matrix` is NULL).
#' @param pred_sd Predicted standard deviations (required if `pred_matrix` is NULL).
#' @param pred_matrix Optional: Matrix of predictive samples (rows = observations).
#' @param verbose Logical, if TRUE prints debug info.
#'
#' @return Numeric vector of CRPS values.
#' @export
compute_crps <- function(y, pred_mean = NULL, pred_sd = NULL, pred_matrix = NULL, verbose = FALSE) {
  .check_numeric(y, "y")
  if (!is.null(pred_matrix)) {
    if (verbose) message("Using sample-based CRPS.")
    return(scoringRules::crps_sample(y = y, dat = pred_matrix))
  }
  if (!is.null(pred_mean) && !is.null(pred_sd)) {
    .check_numeric(pred_mean, "pred_mean")
    .check_numeric(pred_sd, "pred_sd")
    .check_length(y, pred_mean, "y", "pred_mean")
    if (verbose) message("Using normal-based CRPS.")
    return(scoringRules::crps_norm(y = y, mean = pred_mean, sd = pred_sd))
  }
  stop("Either `pred_matrix` OR both `pred_mean` and `pred_sd` must be provided.", call. = FALSE)
}

# ----------------------------
# Logarithmic Score
# ----------------------------
#' Compute Logarithmic Score (Log Score)
#'
#' Assumes a normal predictive distribution.
#'
#' @param y A numeric vector of observed values.
#' @param pred_mean Predicted means.
#' @param pred_sd Predicted standard deviations.
#'
#' @return Numeric vector of log score values.
#' @export
compute_log_score <- function(y, pred_mean, pred_sd) {
  .check_numeric(y, "y")
  .check_numeric(pred_mean, "pred_mean")
  .check_numeric(pred_sd, "pred_sd")
  .check_length(y, pred_mean, "y", "pred_mean")
  scoringRules::logs_norm(y = y, mean = pred_mean, sd = pred_sd)
}

# ----------------------------
# Brier Score
# ----------------------------
#' Compute Brier Score
#'
#' Squared difference between predicted probabilities and binary observed outcomes.
#'
#' @param y Binary numeric vector (0 or 1).
#' @param pred_prob Predicted probabilities for outcome 1.
#'
#' @return Numeric vector of Brier score values.
#' @export
compute_brier_score <- function(y, pred_prob) {
  .check_numeric(y, "y")
  .check_numeric(pred_prob, "pred_prob")
  .check_length(y, pred_prob, "y", "pred_prob")
  (pred_prob - y)^2
}

# ----------------------------
# Interval Score
# ----------------------------
#' Compute Interval Score
#'
#' Penalizes observations outside prediction intervals, rewards narrow intervals.
#'
#' @param y A numeric vector of observed values.
#' @param lower Lower bounds of prediction intervals.
#' @param upper Upper bounds of prediction intervals.
#' @param alpha Nominal significance level (default = 0.05 for 95% intervals).
#'
#' @return Numeric vector of interval scores.
#' @export
compute_interval_score <- function(y, lower, upper, alpha = 0.05) {
  .check_numeric(y, "y")
  .check_numeric(lower, "lower")
  .check_numeric(upper, "upper")
  .check_length(y, lower, "y", "lower")
  .check_length(y, upper, "y", "upper")
  interval_width <- upper - lower
  penalty <- 2 / alpha * (lower - y) * (y < lower) + 2 / alpha * (y - upper) * (y > upper)
  interval_width + penalty
}

# ----------------------------
# Dawid–Sebastiani Score (DSS)
# ----------------------------
#' Compute Dawid–Sebastiani Score (DSS)
#'
#' DSS = ((y - mu)^2 / sigma^2) + 2 * log(sigma)
#'
#' @param y A numeric vector of observed values.
#' @param pred_mean Predicted means.
#' @param pred_sd Predicted standard deviations.
#'
#' @return Numeric vector of DSS values.
#' @export
compute_dss <- function(y, pred_mean, pred_sd) {
  .check_numeric(y, "y")
  .check_numeric(pred_mean, "pred_mean")
  .check_numeric(pred_sd, "pred_sd")
  .check_length(y, pred_mean, "y", "pred_mean")
  ((y - pred_mean)^2 / (pred_sd^2)) + 2 * log(pred_sd)
}

# ----------------------------
# Score Computation from Samples
# ----------------------------
#' Compute Scores from Samples
#'
#' Computes a requested score (e.g., CRPS) for posterior samples.
#'
#' @param y A numeric vector of observed values.
#' @param pred_samples A matrix of posterior predictive samples.
#' @param score_function A function to compute the score (e.g., `compute_crps`).
#' @param ... Additional arguments for the scoring function.
#'
#' @return Numeric vector of score values.
#' @export
compute_score_from_samples <- function(y, pred_samples, score_function, ...) {
  if (is.null(pred_samples)) {
    stop("`pred_samples` is required for sample-based score computation.", call. = FALSE)
  }
  score_function(y = y, pred_matrix = pred_samples, ...)
}
