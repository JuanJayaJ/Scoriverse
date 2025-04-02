#' Compute Continuous Ranked Probability Score (CRPS)
#'
#' This function computes the CRPS for a set of observations given a predictive
#' distribution. It supports two modes:
#' - If a sample-based predictive distribution is provided (as a matrix), the
#'   sample-based CRPS is computed.
#' - If the predictive distribution is assumed to be normal, you can supply
#'   the predicted means and standard deviations.
#'
#' @param y A numeric vector of observed values.
#' @param pred_mean A numeric vector of predicted means. (Required if pred_matrix is NULL)
#' @param pred_sd A numeric vector of predicted standard deviations. (Required if pred_matrix is NULL)
#' @param pred_matrix Optional. A matrix where each row contains samples from the
#'   predictive distribution for the corresponding observation.
#' @param verbose Logical. If TRUE, prints debugging messages.
#'
#' @return A numeric vector of CRPS values.
#' @export
compute_crps <- function(y, pred_mean = NULL, pred_sd = NULL, pred_matrix = NULL, verbose = FALSE) {
  if (verbose) {
    message("Computing CRPS: y length = ", length(y))
  }

  if (!is.null(pred_matrix)) {
    # Check that pred_matrix has as many rows as observations
    if (nrow(pred_matrix) != length(y)) {
      stop("The number of rows in pred_matrix must match the length of y.")
    }
    crps_vals <- scoringRules::crps_sample(y = y, dat = pred_matrix)
  } else if (!is.null(pred_mean) && !is.null(pred_sd)) {
    # Validate lengths and positivity of pred_sd
    if (length(pred_mean) != length(y) || length(pred_sd) != length(y)) {
      stop("Length of pred_mean and pred_sd must match the length of y.")
    }
    if (any(pred_sd <= 0)) {
      stop("All predicted standard deviations must be positive.")
    }
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
#' This function computes the logarithmic score for continuous outcomes.
#' For Gaussian predictions it uses a normal density, while for Poisson models it
#' uses the Poisson density on the link scale.
#'
#' @param y A numeric vector of observed values.
#' @param pred_mean A numeric vector of predicted means.
#' @param pred_sd A numeric vector of predicted standard deviations (for Gaussian).
#' @param family A character string indicating the family ("gaussian" or "poisson").
#'
#' @return A numeric vector of log score values.
#' @export
compute_log_score <- function(y, pred_mean, pred_sd, family = "gaussian") {
  if (family == "poisson") {
    # For Poisson, ensure pred_mean is non-negative
    if (any(pred_mean < 0)) {
      stop("For Poisson models, predicted means must be non-negative.")
    }
    log_scores <- dpois(y, lambda = pred_mean, log = TRUE)
  } else if (family == "gaussian") {
    # Validate input lengths and positivity of pred_sd
    if (length(pred_mean) != length(y) || length(pred_sd) != length(y)) {
      stop("Length of pred_mean and pred_sd must match the length of y.")
    }
    if (any(pred_sd <= 0)) {
      stop("All predicted standard deviations must be positive.")
    }
    log_scores <- scoringRules::logs_norm(y = y, mean = pred_mean, sd = pred_sd)
  } else {
    stop("Unsupported family for log score computation. Choose 'gaussian' or 'poisson'.")
  }
  return(log_scores)
}

#' Compute Brier Score
#'
#' This function computes the Brier score for binary outcomes. The Brier score
#' is defined as the squared difference between the predicted probability and the
#' observed outcome (0 or 1).
#'
#' @param y A binary numeric vector of observed outcomes (0 or 1).
#' @param pred_prob A numeric vector of predicted probabilities for outcome 1.
#'
#' @return A numeric vector of Brier score values.
#' @export
compute_brier_score <- function(y, pred_prob) {
  # Validate that y is binary (0 or 1)
  if (!all(y %in% c(0, 1))) {
    stop("Observed values (y) must be binary (0 or 1) for the Brier score.")
  }
  if (length(y) != length(pred_prob)) {
    stop("Length of y and pred_prob must match.")
  }
  brier_vals <- (pred_prob - y)^2
  return(brier_vals)
}

#' Compute Interval Score
#'
#' This function computes the interval score for a set of prediction intervals.
#' The interval score rewards narrow intervals but imposes a penalty if the
#' observed value falls outside the interval.
#'
#' @param y A numeric vector of observed values.
#' @param lower A numeric vector of lower bounds of the prediction intervals.
#' @param upper A numeric vector of upper bounds of the prediction intervals.
#' @param alpha The nominal significance level (default is 0.05 for 95% prediction intervals).
#'
#' @return A numeric vector of interval score values.
#' @export
compute_interval_score <- function(y, lower, upper, alpha = 0.05) {
  # Validate that lengths match
  if (length(y) != length(lower) || length(y) != length(upper)) {
    stop("Lengths of y, lower, and upper must all match.")
  }
  if (any(upper < lower)) {
    stop("Each upper bound must be greater than or equal to its corresponding lower bound.")
  }
  interval_width <- upper - lower
  penalty <- (2 / alpha) * (lower - y) * (y < lower) + (2 / alpha) * (y - upper) * (y > upper)
  interval_score <- interval_width + penalty
  return(interval_score)
}

#' Compute Dawid–Sebastiani Score (DSS)
#'
#' This function computes the Dawid–Sebastiani Score (DSS) for a set of observations,
#' given a predictive distribution assumed to be normal. The DSS takes into account
#' both the accuracy (difference between observed and predicted means) and the sharpness
#' (through the predicted standard deviation) of the predictive distribution.
#'
#' The DSS is computed as:
#'   DSS = ((y - mu)^2 / sigma^2) + 2 * log(sigma)
#'
#' @param y A numeric vector of observed values.
#' @param pred_mean A numeric vector of predicted means.
#' @param pred_sd A numeric vector of predicted standard deviations.
#'
#' @return A numeric vector of DSS values.
#' @export
compute_dss <- function(y, pred_mean, pred_sd) {
  # Validate input lengths and ensure pred_sd is positive
  if (length(y) != length(pred_mean) || length(y) != length(pred_sd)) {
    stop("Lengths of y, pred_mean, and pred_sd must all match.")
  }
  if (any(pred_sd <= 0)) {
    stop("All predicted standard deviations must be positive for DSS computation.")
  }
  dss_vals <- ((y - pred_mean)^2 / (pred_sd^2)) + 2 * log(pred_sd)
  return(dss_vals)
}
