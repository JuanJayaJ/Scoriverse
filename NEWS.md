# DraftScoriverse News

## DraftScoriverse 0.0.1 (Development Release)

### 🎉 New Features
- Initial release of **DraftScoriverse**: a probabilistic prediction scoring and evaluation framework.
- Support for:
  - ✅ **Generalized Linear Models (GLM)** with outcome-scale sampling
  - ✅ **Generalized Additive Models (GAM)** with `gratia::fitted_samples()`
  - ✅ **Bayesian models** via `brms::posterior_predict()`
  - ✅ **Tidymodels workflows** with standard prediction extraction

### 📏 Scoring Functions
- Implemented proper scoring rules:
  - CRPS (Continuous Ranked Probability Score)
  - Log Score (Logarithmic Score)
  - Brier Score
  - Interval Score
  - Dawid–Sebastiani Score (DSS)
- Introduced `wrap_scoring()` for unified scoring workflows.

### 🔁 Prediction Utilities
- `wrap_predict()` for standardized prediction across models.
- `extract_predictions()` for outcome-scale and posterior sampling.
- `extract_additional_params()` for extracting model-specific variance/dispersion parameters.
- `prepare_model_for_prediction()` for pre-prediction validation.

### 📊 Visualization
- Diagnostic plots for residuals and predictions via:
  - `visualize_residuals()`
  - `visualize_predictions()`

### ✅ Testing and Validation
- 26 unit tests using `{testthat}` v3 covering:
  - Sampling logic
  - Prediction correctness
  - Scoring accuracy
  - Input validation and error catching
- All tests passing with zero warnings, skips, or failures.

### 📚 Documentation
- Full documentation via `{roxygen2}`.
- Completed `README.md` with usage examples and installation guide.
- Package passes `devtools::check()` cleanly with no errors, warnings, or notes.

---

Stay tuned for future versions featuring:
- 🔄 Survival and time-series model support
- 🧪 Calibration and sharpness scoring metrics
- 📈 Enhanced visual diagnostics
