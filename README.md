# DraftScoriverse

**Probabilistic Prediction Scoring and Evaluation Framework**

*Version: 0.0.1*

## Overview

`DraftScoriverse` provides a flexible and unified framework for probabilistic prediction generation and model evaluation using proper scoring rules. The package supports outcome-scale sampling and evaluation across a variety of statistical modeling approaches, including:

- Generalized Linear Models (GLM)
- Generalized Additive Models (GAM)
- Bayesian models fitted via `brms`
- Tidymodels workflows

This framework facilitates robust uncertainty quantification and scoring through a consistent API, making it easier to compare model performance using well-established probabilistic metrics.

## ✨ Key Features

- **Outcome-scale sampling** for GLM, GAM, and Bayesian models
- **Posterior predictive draws** using proper random number generation
- **Support for multiple model classes** (GLM, GAM, brmsfit, tidymodels workflows)
- **Proper scoring rules**:
  - Continuous Ranked Probability Score (CRPS)
  - Logarithmic Score (Log Score)
  - Brier Score
  - Interval Score
  - Dawid-Sebastiani Score (DSS)
- **Diagnostic visualizations** for prediction assessment
- **Unified wrapper functions** for prediction extraction and scoring

## 🚀 Installation

To install the development version of the package from your local directory:

```r
install.packages("path_to_your_downloaded/DraftScoriverse_0.0.1.tar.gz", repos = NULL, type = "source")
```

## 📦 Package Usage

### Prediction and Sampling Example:

```r
library(DraftScoriverse)
library(MASS)

data <- data.frame(count = rnbinom(100, size = 2, mu = 5), x = rnorm(100))
model <- glm.nb(count ~ x, data = data)

samples <- extract_predictions(model, new_data = data, return_samples = TRUE, n_samples = 100)

scores <- wrap_scoring(score_type = "crps", y_true = data$count, predictions = samples)
```

## 📊 Scoring Functions

| Score Type  | Required Inputs                | Description                                 |
|-------------|---------------------------------|---------------------------------------------|
| `crps`      | `pred_mean`, `pred_sd`          | Continuous Ranked Probability Score        |
| `log_score` | `pred_mean`, `pred_sd`          | Logarithmic Score for density functions    |
| `brier`     | `pred_prob`                     | Brier Score for binary classification      |
| `interval`  | `lower`, `upper`                | Interval Score for prediction intervals    |
| `dss`       | `pred_mean`, `pred_sd`          | Dawid-Sebastiani Score                     |

## 📂 Supported Models

| Model Type           | Supported Sampling                        |
|----------------------|--------------------------------------------|
| GLM                  | Outcome-scale sampling with appropriate RNGs |
| GAM (`mgcv`)         | Outcome-scale sampling, posterior uncertainty handling |
| brmsfit (`brms`)     | Posterior predictive draws via `posterior_predict()` |
| Tidymodels workflows | Standard prediction extraction (no posterior sampling) |

## ✅ Testing

Run all tests locally with:

```r
devtools::test()
```

Or check the package with:

```r
devtools::check()
```

## 📋 License

This package is licensed under the **MIT License**.  
See the `LICENSE` file for details.

## 🙌 Acknowledgements
This package was developed by Juan Jauanda as part of data science capstone project on model evaluation using probabilistic scoring rules.
Special thanks to the maintainers of the brms, mgcv, scoringRules, and marginaleffects packages, which are integrated into this framework.
