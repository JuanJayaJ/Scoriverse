# DraftScoriverse News

## DraftScoriverse 0.0.1 (Development Release)

### 🎉 New Features
- Initial implementation of `DraftScoriverse`: a probabilistic prediction scoring and evaluation framework.
- Support for **Generalized Linear Models (GLM)** with outcome-scale sampling.
- Support for **Generalized Additive Models (GAM)** with outcome-scale sampling.
- Integrated support for **Bayesian models** via `brms`, with posterior predictive draws.
- Added support for **tidymodels workflows** for standard prediction extraction.

### 📏 Scoring Functions
- Implemented proper scoring rules:
  - Continuous Ranked Probability Score (CRPS)
  - Logarithmic Score (Log Score)
  - Brier Score
  - Interval Score
  - Dawid-Sebastiani Score (DSS)
- Added `wrap_scoring()` function to provide a unified scoring interface.

### 📌 Prediction Utilities
- Developed `wrap_predict()` to standardize prediction extraction across supported model types.
- Developed `extract_predictions()` to handle posterior sampling and outcome-scale sampling using proper random number generators.
- Created `extract_additional_params()` to safely retrieve sigma (for Gaussian) or phi (for Negative Binomial) from models.
- Implemented `prepare_model_for_prediction()` for model validation and pre-prediction checks.

### 📊 Visualization
- Added basic diagnostic visualizations for residuals and prediction assessment.

### ✅ Testing and Validation
- Extensive unit testing with `testthat` for:
  - Sampling correctness
  - Prediction extraction
  - Scoring accuracy
  - Input validation and error handling

### 📄 Documentation
- Added initial documentation with `roxygen2` comments.
- Completed function-level documentation for all public interfaces.
- Created README.md with usage examples.
- Package passes `devtools::check()` with zero errors, warnings, and notes.

---

Stay tuned for future releases with extended model support and more advanced scoring options!
