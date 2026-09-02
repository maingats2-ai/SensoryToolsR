# SensoryToolsR 0.1.0

## Initial release

SensoryToolsR provides tools for importing, validating, summarizing,
analysing, and visualizing sensory evaluation data, with particular
support for replicated trained-panel and QDA studies.

### Main features

* Import and clean sensory datasets with `sensory_import()` and
  `sensory_clean()`.
* Validate sensory experimental designs with `sensory_validate()`.
* Calculate descriptive sensory statistics with `sensory_summary()`.
* Perform simple additive Product + Assessor ANOVA with
  `sensory_anova()` and Tukey post-hoc comparisons with
  `sensory_posthoc()`.
* Analyse replicated trained-panel data with `sensory_panel_anova()`,
  including Product x Assessor interaction and Product testing against
  the Product x Assessor error term.
* Require complete balanced Assessor x Product x Session designs for
  classical panel ANOVA.
* Evaluate assessor performance with `sensory_panel_performance()`,
  including:
  - product discrimination;
  - session effects;
  - repeatability RMSE;
  - residual MSE;
  - agreement correlation;
  - mean level bias;
  - scale-use diagnostics;
  - missing and duplicated Product x Session design checks.
* Run multi-attribute panel analysis with `sensory_panel_multi()`.
* Perform PCA and PCA diagnostics with `sensory_pca()`,
  `sensory_pca_diagnostics()`, and `sensory_pca_plot()`.
* Run an integrated QDA workflow with `sensory_qda()`.
* Visualize assessor performance with `sensory_panel_plot()`.
* Include the simulated `qda_example` dataset for examples,
  demonstrations, and testing.

### Statistical and robustness improvements

* Product significance in replicated panel ANOVA uses the
  Product x Assessor mean square as the error term.
* Incomplete and duplicated assessor design cells are detected and
  flagged appropriately.
* Pearson agreement is calculated against product means from the
  remaining panel, excluding the assessor being evaluated.
* `mean_level_bias` provides a separate diagnostic for systematic
  score-level shifts that may not be detected by correlation.
* Repeatability RMSE retains session-to-session shifts so that
  systematic replicate drift remains visible.
* Screening thresholds are configurable and are documented as
  diagnostic rules rather than universal ISO pass/fail criteria.
* Non-finite values are rejected for `alpha`,
  `agreement_threshold`, and `repeatability_multiplier`.
