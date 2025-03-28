% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_model.R
\name{score_model}
\alias{score_model}
\title{Compute Scoring Metrics for a Model}
\usage{
score_model(
  model,
  newdata,
  truth,
  metrics = c("logarithmic", "brier", "crps", "energy", "dawid_sebastiani", "spherical",
    "ranked_probability", "interval", "multiclass_brier", "zero_one"),
  alpha = 0.05,
  ...
)
}
\arguments{
\item{model}{A fitted model.}

\item{newdata}{A data frame containing new observations.}

\item{truth}{The observed outcomes.}

\item{metrics}{A vector of metric names.}

\item{alpha}{Significance level used in interval calculations (default: 0.05).}

\item{...}{Additional arguments passed to prediction methods.}
}
\value{
A tibble with scoring results, including the metric name, value, type, direction,
model type, timestamp, and number of observations.
}
\description{
Computes a set of proper scoring metrics for a model using standardized predictions.
}
