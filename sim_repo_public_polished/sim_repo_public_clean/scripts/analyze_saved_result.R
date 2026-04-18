# Example post-processing script for a saved scenario result

library(dplyr)
source("R/truth_objects.R")
source("R/rubin_pool.R")
source("R/extract_variable_importance.R")
source("R/pool_variable_importance.R")
source("R/evaluate_variable_selection.R")
source("R/analyze_simulation_results.R")

result_path <- "results/result_continuous_linear_mvnorm_MAR_exposure_knn.rds"
out_dir <- "results/postprocessed"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

res <- readRDS(result_path)
analysis <- analyze_simulation_results(res, truth = build_truth())

write.csv(analysis$raw_importance, file.path(out_dir, "raw_importance.csv"), row.names = FALSE)
write.csv(analysis$pooled_importance, file.path(out_dir, "pooled_importance.csv"), row.names = FALSE)
write.csv(analysis$simulation_variable_results, file.path(out_dir, "simulation_variable_results.csv"), row.names = FALSE)
write.csv(analysis$simulation_confusion_counts, file.path(out_dir, "simulation_confusion_counts.csv"), row.names = FALSE)
write.csv(analysis$overall_variable_summary, file.path(out_dir, "overall_variable_summary.csv"), row.names = FALSE)
write.csv(analysis$overall_selection_metrics, file.path(out_dir, "overall_selection_metrics.csv"), row.names = FALSE)
