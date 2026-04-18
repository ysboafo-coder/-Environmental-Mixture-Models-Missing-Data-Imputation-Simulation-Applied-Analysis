# Evaluating Missing Data Imputation Strategies for Environmental Mixture Models: A Simulation Study and Applied Analysis

This repository provides a configurable R framework for running simulation studies on environmental mixture methods under multiple missing-data scenarios, then analyzing results with a focus on **variable importance** and **variable selection**.

The framework replaces a one-script-per-scenario workflow with a modular pipeline that supports:

- complete data
- available-case analysis
- mean imputation
- median imputation
- KNN imputation
- MICE imputation
- Amelia imputation
- continuous and binary outcomes
- linear and nonlinear relationships
- multivariate normal (`mvnorm`) and multivariate t (`mvt`) exposure distributions
- MAR and MNAR missingness
- exposure, outcome, and exposure-outcome missingness
- six fitted models:
  - WQS
  - BWQS
  - Q-gcomp
  - BKMR
  - Elastic Net
  - Lasso

## Purpose

This repository is designed for reproducible simulation work comparing how different missing-data handling strategies affect variable-importance and variable-selection results across mixture models.

The comparison layer is intentionally centered on:

- variable importance
- variable selection frequency
- pooled or aggregated variable-level summaries
- sensitivity
- specificity
- false discovery rate (FDR)
- precision

**AIC is intentionally excluded from the main comparison layer** because it is not directly comparable across all fitted model classes used here.

## Repository structure

```text
sim_repo_public_clean/
├── R/
│   ├── generate_data.R
│   ├── induce_missingness.R
│   ├── handle_missing_data.R
│   ├── fit_models.R
│   ├── run_one_iteration.R
│   ├── utils_parallel.R
│   ├── run_simulation_scenario.R
│   ├── scenario_grid.R
│   ├── extract_variable_importance.R
│   ├── pool_variable_importance.R
│   ├── rubin_pool.R
│   ├── truth_objects.R
│   ├── evaluate_variable_selection.R
│   └── analyze_simulation_results.R
├── scripts/
│   ├── run_all_scenarios.R
│   └── analyze_saved_result.R
├── docs/
│   └── scenario_catalog.md
├── results/
├── .gitignore
├── LICENSE
├── CITATION.cff
└── README.md
```

## Core workflow

### 1. Simulation layer

The simulation layer:

1. generates data
2. induces missingness according to the scenario definition
3. applies the requested handling method
4. fits all six models
5. stores results in a standardized structure

### 2. Variable-importance / selection layer

The post-processing layer:

1. extracts model-specific variable-importance outputs
2. pools or aggregates imputation-specific results where needed
3. creates intermediate variable-level selected/not-selected outputs within each simulation
4. summarizes results **overall across simulations**

## Standard simulation output

Each scenario returns the same structure:

```r
list(
  meta = config,
  runtime = list(...),
  results = list(
    qws.pos.out = [simulation][imputation],
    bqws.out = [simulation][imputation],
    gcomp.out = [simulation][imputation],
    bkmr.out = [simulation][imputation],
    cvfit_elasticnet.out = [simulation][imputation],
    cvfit_lasso.out = [simulation][imputation]
  )
)
```

For single-dataset methods (`complete`, `available`, `mean`, `median`, `knn`), each simulation still uses an imputation index of `[[1]]`, so downstream code can use the same indexing convention for all methods.

## Variable-importance conventions

The post-processing layer follows these model-specific conventions:

- **WQS**: uses `final_weights`; variable selection is based on interval exclusion of zero when available
- **BWQS**: uses weight summaries; interval-based selection logic is used when available
- **Q-gcomp**: uses exposure-specific coefficients; Rubin pooling is used for multiply imputed scenarios where appropriate
- **BKMR**: uses `ExtractPIPs()`; the default selection threshold is `PIP >= 0.5`
- **Elastic Net / Lasso**: uses coefficients at `lambda.min`; nonzero coefficients are treated as selected

## Post-processing outputs

The analysis layer returns:

```r
list(
  raw_importance = ...,
  pooled_importance = ...,
  simulation_variable_results = ...,
  simulation_confusion_counts = ...,
  overall_variable_summary = ...,
  overall_selection_metrics = ...
)
```

Interpretation:

- `simulation_variable_results`: intermediate variable-level selected/not-selected results for each simulation
- `simulation_confusion_counts`: intermediate TP/FN/FP/TN counts for each simulation
- `overall_variable_summary`: final variable-level summaries aggregated across simulations
- `overall_selection_metrics`: final sensitivity, specificity, FDR, and precision aggregated across simulations

## Example: run one scenario

```r
source("R/generate_data.R")
source("R/induce_missingness.R")
source("R/handle_missing_data.R")
source("R/fit_models.R")
source("R/run_one_iteration.R")
source("R/utils_parallel.R")
source("R/run_simulation_scenario.R")

cfg <- list(
  scenario_name = "continuous_linear_mvnorm_MNAR_exposure_mice",
  outcome_type = "continuous",
  relationship = "linear",
  distribution = "mvnorm",
  missing_mechanism = "MNAR",
  missing_target = "exposure",
  method = "mice",
  nsim = 100,
  m = 5,
  parallel = TRUE,
  n_cores = 4,
  output_dir = "results"
)

res <- run_simulation_scenario(cfg)
save_simulation_result(res)
```

## Example: analyze a saved result

```r
library(dplyr)

source("R/truth_objects.R")
source("R/rubin_pool.R")
source("R/extract_variable_importance.R")
source("R/pool_variable_importance.R")
source("R/evaluate_variable_selection.R")
source("R/analyze_simulation_results.R")

res <- readRDS("results/result_continuous_linear_mvnorm_MAR_exposure_knn.rds")
analysis <- analyze_simulation_results(res, truth = build_truth())

analysis$overall_selection_metrics
analysis$overall_variable_summary
```

## Reproducibility notes

- Windows parallel execution uses a PSOCK cluster.
- Mac/Linux parallel execution uses `mclapply()`.
- BKMR for binary outcomes includes extra guards and knot handling similar to the original binary KNN analysis workflow.
- MICE and Amelia follow the same output contract as the single-imputation methods.

## Recommended use before publishing results

Before relying on the repository for a full study run:

1. test one small scenario with a reduced `nsim`
2. confirm package versions in your local R environment
3. verify one saved `.rds` file can be analyzed end to end
4. inspect one model object from each family if package versions differ from the original analysis environment

## Citation

If you use this repository in academic work, please cite the repository using the information in `CITATION.cff` and also cite the relevant manuscript or dissertation chapter that accompanies the simulation study.

## License

This repository is released under the MIT License.
