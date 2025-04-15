#' Extract Posterior Predictive Draws from a Model
#'
#' Retrieves posterior predictive draws from a Bayesian model.
#'
#' @param model A fitted Bayesian model (e.g., brmsfit).
#' @param new_data A data frame of new observations.
#' @param draws Number of posterior draws to return (default: 1000).
#'
#' @return A matrix of posterior predictive samples. Rows = draws, columns = observations.
#' @export
get_posterior_draws <- function(model, new_data, draws = 1000) {
  if (inherits(model, "brmsfit")) {
    return(brms::posterior_predict(model, newdata = new_data, draws = draws))
  } else {
    stop("Posterior draw extraction not supported for this model type yet.")
  }
}

#' Extract Predictions from Diverse Model Objects
#'
#' Extracts prediction estimates from a fitted model object using appropriate methods.
#'
#' @param model A fitted model object.
#' @param new_data Optional data frame with new observations.
#' @param return_samples Logical. If TRUE, return posterior samples or draws; if FALSE, return point estimates (default).
#' @param n_samples Number of posterior samples to draw (if return_samples = TRUE).
#' @param ... Additional arguments for underlying prediction functions.
#'
#' @return A numeric vector of predictions or matrix of posterior samples (depending on `return_samples`).
#' @export
extract_predictions <- function(model, new_data = NULL, return_samples = FALSE, n_samples = 1000, ...) {
  dots <- list(...)
  scoring_args <- c("pred_sd", "pred_prob", "lower", "upper")
  filtered_args <- dots[!names(dots) %in% scoring_args]

  if (inherits(model, "brmsfit")) {
    if (return_samples) {
      return(get_posterior_draws(model, new_data = new_data, draws = n_samples))
    } else {
      return(colMeans(get_posterior_draws(model, new_data = new_data, draws = n_samples)))
    }
  }

  if (inherits(model, "gam")) {
    if (return_samples) {
      if (!requireNamespace("gratia", quietly = TRUE) ||
          !requireNamespace("tidybayes", quietly = TRUE)) {
        stop("Packages 'gratia' and 'tidybayes' must be installed to return samples for GAMs.")
      }
      draws <- tidybayes::fitted_draws(model, newdata = new_data, n = n_samples, value = "lambda")
      fam <- family(model)$family
      lambda_mat <- tidyr::pivot_wider(draws, names_from = ".row", values_from = "lambda")
      lambda_mat <- as.matrix(lambda_mat[,-1])
      if (grepl("poisson", fam, ignore.case = TRUE)) {
        outcome_draws <- apply(lambda_mat, 2, function(lam) rpois(n_samples, lam))
      } else if (grepl("gaussian", fam, ignore.case = TRUE)) {
        sigma <- sqrt(sum(residuals(model)^2) / df.residual(model))
        outcome_draws <- apply(lambda_mat, 2, function(mu) rnorm(n_samples, mu, sigma))
      } else if (grepl("negbinom", fam, ignore.case = TRUE)) {
        params <- extract_additional_params(model)
        if (is.null(params$phi)) {
          stop("Negative Binomial: overdispersion parameter 'phi' not available.")
        }
        size <- params$phi
        outcome_draws <- apply(lambda_mat, 2, function(mu) rnbinom(n_samples, size = size, mu = mu))
      } else {
        stop("Sampling for GAM family not yet implemented: ", fam)
      }
      return(outcome_draws)
    } else {
      mu <- predict(model, newdata = new_data, type = "response")
      return(mu)
    }
  }

  if (any(class(model) %in% c("randomForest", "gbm", "xgb.Booster", "glmnet"))) {
    if (!is.null(new_data)) {
      preds_generic <- predict(model, newdata = new_data, ...)
      if (length(preds_generic) != nrow(new_data)) {
        preds_generic <- preds_generic[seq_len(nrow(new_data))]
      }
      return(standardize_predictions(preds_generic, n_expected = nrow(new_data)))
    } else {
      return(standardize_predictions(predict(model, ...)))
    }
  }

  predictions <- tryCatch({
    if (!is.null(new_data)) {
      preds_generic <- predict(model, newdata = new_data, ...)
      if (length(preds_generic) != nrow(new_data)) {
        preds_generic <- preds_generic[seq_len(nrow(new_data))]
      }
      preds_generic
    } else {
      predict(model, ...)
    }
  }, error = function(e) {
    stop("Generic predict() failed: ", e$message)
  })

  n_expected <- if (!is.null(new_data)) nrow(new_data) else length(predictions)
  standardize_predictions(predictions, n_expected = n_expected)
}

