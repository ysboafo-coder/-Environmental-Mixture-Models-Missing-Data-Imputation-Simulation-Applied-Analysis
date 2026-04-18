# End-to-end post-processing for variable importance / selection

analyze_simulation_results <- function(result_object, truth = build_truth()) {
  raw_importance <- extract_variable_importance(result_object)
  pooled_importance <- pool_variable_importance(raw_importance)
  simulation_variable_results <- pooled_importance |>
    apply_selection_rules() |>
    classify_selection(truth = truth)
  summaries <- summarize_selection_metrics(simulation_variable_results)

  list(
    raw_importance = raw_importance,
    pooled_importance = pooled_importance,
    simulation_variable_results = summaries$simulation_variable_results,
    simulation_confusion_counts = summaries$simulation_confusion_counts,
    overall_variable_summary = summaries$overall_variable_summary,
    overall_selection_metrics = summaries$overall_selection_metrics
  )
}
