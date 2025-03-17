#' Available Scoring Metrics
#'
#' @keywords internal
available_metrics <- function() {
  c("logarithmic", "brier", "crps", "energy", "dawid_sebastiani",
    "spherical", "ranked_probability", "interval", "multiclass_brier", "zero_one")
}

#' Compute an Individual Scoring Metric
#'
#' @keywords internal
compute_score <- function(metric, preds, truth, alpha = 0.05) {
  standardize_score <- function(value, metric_name, type, lower_better = TRUE) {
    tibble::tibble(
      metric = metric_name,
      value  = value,
      type   = type,
      direction = if (lower_better) "lower is better" else "higher is better"
    )
  }

  switch(metric,
         "logarithmic" = {
           if (!requireNamespace("scoringRules", quietly = TRUE)) {
             warning("Skipping Logarithmic Score: 'scoringRules' package not installed.")
             return(NULL)
           }
           value <- scoringRules::logs_sample(y = truth, dat = preds$estimate)
           standardize_score(value, "Logarithmic Score", "probabilistic")
         },
         "brier" = {
           truth_numeric <- as.numeric(truth)
           if (!all(truth_numeric %in% c(0, 1))) {
             warning("Skipping Brier Score: Requires binary (0/1) truth values.")
             return(NULL)
           }
           value <- mean((preds$estimate - truth_numeric)^2)
           standardize_score(value, "Brier Score", "probabilistic")
         },
         "crps" = {
           if (!requireNamespace("scoringRules", quietly = TRUE)) {
             warning("Skipping CRPS: 'scoringRules' package not installed.")
             return(NULL)
           }
           if (!is.matrix(preds$estimate)) {
             preds$estimate <- matrix(preds$estimate, ncol = 1)  # Ensure it is a matrix
           }
           value <- scoringRules::crps_sample(y = truth, dat = preds$estimate)
           standardize_score(value, "CRPS", "probabilistic")
         },
         "energy" = {
           if (!requireNamespace("scoringRules", quietly = TRUE)) {
             warning("Skipping Energy Score: 'scoringRules' package not installed.")
             return(NULL)
           }
           value <- scoringRules::es_sample(y = truth, dat = preds$estimate)
           standardize_score(value, "Energy Score", "multivariate")
         },
         "dawid_sebastiani" = {
           if (any(preds$std_error <= 0)) {
             warning("Skipping Dawid-Sebastiani Score: Requires positive standard errors.")
             return(NULL)
           }
           sigma2 <- preds$std_error^2
           value <- mean(log(sigma2) + (truth - preds$estimate)^2 / sigma2)
           standardize_score(value, "Dawid-Sebastiani Score", "probabilistic")
         },
         "spherical" = {
           if (any(preds$estimate < 0)) {
             warning("Skipping Spherical Score: Requires non-negative predictions.")
             return(NULL)
           }
           value <- mean(preds$estimate / sqrt(sum(preds$estimate^2)))
           standardize_score(value, "Spherical Score", "probabilistic", lower_better = FALSE)
         },
         "ranked_probability" = {
           if (!is.ordered(truth) && !is.numeric(truth)) {
             warning("Skipping Ranked Probability Score: Requires ordered or numeric truth values.")
             return(NULL)
           }
           value <- mean((cumsum(preds$estimate) - cumsum(as.numeric(truth)))^2)
           standardize_score(value, "Ranked Probability Score", "ordinal")
         },
         "interval" = {
           value <- mean((preds$upper - preds$lower) +
                           (2/alpha) * (preds$lower - truth) * (truth < preds$lower) +
                           (2/alpha) * (truth - preds$upper) * (truth > preds$upper))
           standardize_score(value, "Interval Score", "interval")
         },
         "multiclass_brier" = {
           if (!is.matrix(preds$estimate)) {
             warning("Skipping Multiclass Brier Score: Requires a matrix of class probabilities.")
             return(NULL)
           }
           if (ncol(preds$estimate) != length(unique(truth))) {
             warning("Skipping Multiclass Brier Score: Mismatch between predictions and class levels.")
             return(NULL)
           }
           value <- mean(rowSums((preds$estimate - model.matrix(~ truth - 1))^2))
           standardize_score(value, "Multiclass Brier Score", "multiclass")
         },
         "zero_one" = {
           value <- mean(round(preds$estimate) != truth)
           standardize_score(value, "Zero-One Loss", "classification")
         },
         {
           warning("Unknown metric: ", metric)
           return(NULL)
         }
  )
}

