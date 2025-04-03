#' Compute Continuous Ranked Probability Score (CRPS)
#'
#' Computes the CRPS for a set of observations given a predictive distribution.
#' Two modes are supported:
#' - Sample-based CRPS when a sample matrix is provided.
#' - Normal-based CRPS using predicted means and standard deviations.
#'
#' @param y A numeric vector of observed values.
#' @param pred_mean A numeric vector of predicted means (required if pred_matrix is NULL).
#' @param pred_sd A numeric vector of predicted standard deviations (required if pred_matrix is NULL).
#' @param pred_matrix Optional. A matrix with each row representing samples from the predictive distribution.
#' @param verbose Logical. If TRUE, prints debugging messages.
#'
#' @return A numeric vector of CRPS values.
#' @export
compute_crps <- function(y, pred_mean = NULL, pred_sd = NULL, pred_matrix = NULL, verbose = FALSE) {
  if (verbose) {
    message("Computing CRPS: y length = ", length(y))
  }
  
  if (!is.null(pred_matrix)) {
    crps_vals <- scoringRules::crps_sample(y = y, dat = pred_matrix)
  } else if (!is.null(pred_mean) && !is.null(pred_sd)) {
    crps_vals <- scoringRules::crps_norm(y = y, mean = pred_mean, sd = pred_sd)
  } else {
    stop("Either provide pred_matrix or both pred_mean and pred_sd.")
  }
  
  if (verbose) {
    message("CRPS computed, output length = ", length(crps_vals))
  }
  
  return(crps_vals)
}

#' Compute Logarithmic Score (Log Score)
#'
#' Computes the logarithmic score for continuous outcomes assuming a normal predictive distribution.
#'
#' @param y A numeric vector of observed values.
#' @param pred_mean A numeric vector of predicted means.
#' @param pred_sd A numeric vector of predicted standard deviations.
#'
#' @return A numeric vector of log score values.
#' @export
compute_log_score <- function(y, pred_mean, pred_sd) {
  scoringRules::logs_norm(y = y, mean = pred_mean, sd = pred_sd)
}

#' Compute Brier Score
#'
#' Computes the Brier score for binary outcomes. The score is the squared difference
#' between the predicted probability and the observed outcome (0 or 1).
#'
#' @param y A binary numeric vector of observed outcomes (0 or 1).
#' @param pred_prob A numeric vector of predicted probabilities for outcome 1.
#'
#' @return A numeric vector of Brier score values.
#' @export
compute_brier_score <- function(y, pred_prob) {
  (pred_prob - y)^2
}

#' Compute Interval Score
#'
#' Computes the interval score for prediction intervals, rewarding narrow intervals
#' while penalizing observations that fall outside the interval.
#'
#' @param y A numeric vector of observed values.
#' @param lower A numeric vector of lower bounds of the prediction intervals.
#' @param upper A numeric vector of upper bounds of the prediction intervals.
#' @param alpha Nominal significance level (default is 0.05 for 95% intervals).
#'
#' @return A numeric vector of interval score values.
#' @export
compute_interval_score <- function(y, lower, upper, alpha = 0.05) {
  interval_width <- upper - lower
  penalty <- 2 / alpha * (lower - y) * (y < lower) +
    2 / alpha * (y - upper) * (y > upper)
  interval_width + penalty
}

#' Compute Dawid–Sebastiani Score (DSS)
#'
#' Computes the Dawid–Sebastiani Score (DSS) for observations given a normal predictive distribution.
#' The DSS accounts for both the accuracy and sharpness of the predictions.
#'
#' DSS is calculated as:
#'   DSS = ((y - mu)^2 / sigma^2) + 2 * log(sigma)
#'
#' @param y A numeric vector of observed values.
#' @param pred_mean A numeric vector of predicted means.
#' @param pred_sd A numeric vector of predicted standard deviations.
#'
#' @return A numeric vector of DSS values.
#' @export
compute_dss <- function(y, pred_mean, pred_sd) {
  ((y - pred_mean)^2 / (pred_sd^2)) + 2 * log(pred_sd)
}
