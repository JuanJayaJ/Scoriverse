# DraftScoriverse

**Probabilistic Prediction Scoring and Evaluation Framework**  
📦 Version: 0.0.1  
🧪 License: MIT

---

## 🔍 Overview

**DraftScoriverse** provides a flexible and unified framework for generating probabilistic predictions and evaluating models using **proper scoring rules**. It supports **outcome-scale sampling**, **posterior predictive draws**, and **modular scoring tools** across a range of statistical modeling approaches, including:

- ✅ Generalized Linear Models (GLM)
- ✅ Generalized Additive Models (GAM)
- ✅ Bayesian models via `brms`
- ✅ Tidymodels workflows

This framework enhances uncertainty-aware evaluation by offering a consistent API for comparing model performance using well-established probabilistic metrics.

---

## ✨ Key Features

- 🔁 Outcome-scale sampling for GLM, GAM, and Bayesian models  
- 🎯 Posterior predictive draws using proper random number generation  
- 🧠 Support for multiple model classes: `glm`, `gam`, `brmsfit`, and `workflow`  
- 📈 Proper scoring rules:
  - Continuous Ranked Probability Score (CRPS)
  - Logarithmic Score (Log Score)
  - Dawid-Sebastiani Score (DSS)
  - Interval Score
  - Brier Score
- 📊 Diagnostic visualizations for prediction and residual analysis  
- 🧰 Unified wrapper functions for predictions and scoring  

---

## 🚀 Installation

To install the development version from your local tarball:

```r
install.packages("path/to/DraftScoriverse_0.0.1.tar.gz", repos = NULL, type = "source")
```

---

## 📦 Usage Example

```r
library(DraftScoriverse)
library(MASS)

# Fit a Negative Binomial GLM
data <- data.frame(count = rnbinom(100, size = 2, mu = 5), x = rnorm(100))
model <- glm.nb(count ~ x, data = data)

# Generate samples
samples <- extract_predictions(model, new_data = data, return_samples = TRUE, n_samples = 100)

# Evaluate CRPS
scores <- wrap_scoring("crps", y_true = data$count, predictions = samples)
```

---

## 📊 Scoring Functions

| Score Type    | Required Inputs             | Description                                   |
|---------------|-----------------------------|-----------------------------------------------|
| `crps`        | `pred_mean`, `pred_sd`      | Continuous Ranked Probability Score           |
| `log_score`   | `pred_mean`, `pred_sd`      | Logarithmic Score (density-based)             |
| `brier`       | `pred_prob`                 | Brier Score for binary classification         |
| `interval`    | `lower`, `upper`            | Interval Score for prediction intervals       |
| `dss`         | `pred_mean`, `pred_sd`      | Dawid-Sebastiani Score                        |

---

## 📂 Supported Models

| Model Type            | Sampling Support                                          |
|------------------------|----------------------------------------------------------|
| **GLM** (`glm`)        | Outcome-scale sampling with `rnorm()`, `rpois()`, etc.   |
| **GAM** (`gam`)        | Posterior predictive sampling via `gratia::fitted_samples()` |
| **Bayesian** (`brms`)  | Posterior predictive draws via `posterior_predict()`     |
| **Tidymodels** (`workflow`) | Point prediction only; no posterior samples           |

---

## ✅ Testing

Run all package tests locally:

```r
devtools::test()
```

Check the full package build:

```r
devtools::check()
```

---

## 📄 License

This package is released under the **MIT License**.  
See the `LICENSE` file for full details.

---

## 🙌 Acknowledgements

Developed by **Juan Jauanda** as part of a data science capstone project on **model evaluation with probabilistic scoring rules**.

Special thanks to the authors of:
- [`brms`](https://cran.r-project.org/package=brms)
- [`mgcv`](https://cran.r-project.org/package=mgcv)
- [`scoringRules`](https://cran.r-project.org/package=scoringRules)
- [`marginaleffects`](https://cran.r-project.org/package=marginaleffects)

for building the foundational tools integrated into this package.

---
