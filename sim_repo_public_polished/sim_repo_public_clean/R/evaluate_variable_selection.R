# Variable selection rules and overall metrics

apply_selection_rules <- function(df) {
  if (!nrow(df)) return(df)

  df$selected <- FALSE

  is_wqs <- df$model %in% c("wqs", "bwqs")
  df$selected[is_wqs] <- with(df[is_wqs, , drop = FALSE],
    (!is.na(conf.low) & !is.na(conf.high) & (conf.low > 0 | conf.high < 0)) |
      (!is.na(importance) & importance > 0)
  )

  is_qg <- df$model == "qgcomp"
  df$selected[is_qg] <- with(df[is_qg, , drop = FALSE],
    (!is.na(conf.low) & !is.na(conf.high) & (conf.low > 0 | conf.high < 0)) |
      (!is.na(p.value) & p.value < 0.05)
  )

  is_bkmr <- df$model == "bkmr"
  df$selected[is_bkmr] <- !is.na(df$importance[is_bkmr]) & df$importance[is_bkmr] >= 0.5

  is_glmnet <- df$model %in% c("elasticnet", "lasso")
  df$selected[is_glmnet] <- !is.na(df$estimate[is_glmnet]) & abs(df$estimate[is_glmnet]) > 0

  df
}

classify_selection <- function(df, truth = build_truth()) {
  if (!nrow(df)) return(df)
  df$truth <- ifelse(df$variable %in% truth$active, 1L, 0L)
  df$class <- dplyr::case_when(
    df$truth == 1L & df$selected ~ "TP",
    df$truth == 1L & !df$selected ~ "FN",
    df$truth == 0L & df$selected ~ "FP",
    df$truth == 0L & !df$selected ~ "TN",
    TRUE ~ NA_character_
  )
  df
}

summarize_selection_metrics <- function(df) {
  if (!nrow(df)) {
    return(list(
      overall_selection_metrics = data.frame(),
      overall_variable_summary = data.frame(),
      simulation_variable_results = data.frame(),
      simulation_confusion_counts = data.frame()
    ))
  }

  simulation_variable_results <- df |>
    dplyr::select(
      model, simulation, variable, importance, estimate, std.error,
      conf.low, conf.high, p.value, selected, truth, class
    ) |>
    dplyr::arrange(model, simulation, variable)

  simulation_confusion_counts <- df |>
    dplyr::group_by(model, simulation) |>
    dplyr::summarise(
      TP = sum(class == "TP", na.rm = TRUE),
      FN = sum(class == "FN", na.rm = TRUE),
      FP = sum(class == "FP", na.rm = TRUE),
      TN = sum(class == "TN", na.rm = TRUE),
      .groups = "drop"
    )

  overall_selection_metrics <- simulation_confusion_counts |>
    dplyr::group_by(model) |>
    dplyr::summarise(
      TP = sum(TP, na.rm = TRUE),
      FN = sum(FN, na.rm = TRUE),
      FP = sum(FP, na.rm = TRUE),
      TN = sum(TN, na.rm = TRUE),
      sensitivity = ifelse((TP + FN) > 0, TP / (TP + FN), NA_real_),
      specificity = ifelse((TN + FP) > 0, TN / (TN + FP), NA_real_),
      fdr = ifelse((TP + FP) > 0, FP / (TP + FP), NA_real_),
      precision = ifelse((TP + FP) > 0, TP / (TP + FP), NA_real_),
      n_simulations = dplyr::n(),
      .groups = "drop"
    )

  overall_variable_summary <- df |>
    dplyr::group_by(model, variable) |>
    dplyr::summarise(
      mean_importance = mean(importance, na.rm = TRUE),
      sd_importance = stats::sd(importance, na.rm = TRUE),
      selection_frequency = mean(selected, na.rm = TRUE),
      truth = dplyr::first(truth),
      .groups = "drop"
    )

  list(
    overall_selection_metrics = overall_selection_metrics,
    overall_variable_summary = overall_variable_summary,
    simulation_variable_results = simulation_variable_results,
    simulation_confusion_counts = simulation_confusion_counts
  )
}
