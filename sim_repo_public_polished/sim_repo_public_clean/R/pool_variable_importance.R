# Pool or aggregate variable importance across imputations

aggregate_importance_within_sim <- function(df) {
  if (!nrow(df)) return(df)

  out <- dplyr::group_by(df, model, simulation, variable) |>
    dplyr::summarise(
      importance = mean(importance, na.rm = TRUE),
      estimate = mean(estimate, na.rm = TRUE),
      std.error = if (all(is.na(std.error))) NA_real_ else mean(std.error, na.rm = TRUE),
      conf.low = if (all(is.na(conf.low))) NA_real_ else mean(conf.low, na.rm = TRUE),
      conf.high = if (all(is.na(conf.high))) NA_real_ else mean(conf.high, na.rm = TRUE),
      p.value = if (all(is.na(p.value))) NA_real_ else mean(p.value, na.rm = TRUE),
      n_imputations = dplyr::n(),
      .groups = "drop"
    )

  out
}

pool_qgcomp_with_rubin <- function(df) {
  qg <- df[df$model == "qgcomp", , drop = FALSE]
  other <- df[df$model != "qgcomp", , drop = FALSE]
  if (!nrow(qg)) return(aggregate_importance_within_sim(other))

  pooled_list <- qg |>
    dplyr::group_by(model, simulation, variable) |>
    dplyr::group_split()

  pooled_df <- lapply(pooled_list, function(chunk) {
    if (all(is.na(chunk$std.error))) {
      out <- aggregate_importance_within_sim(chunk)
      return(out)
    }
    pooled <- rubin_pool(chunk$estimate, chunk$std.error)
    data.frame(
      model = unique(chunk$model),
      simulation = unique(chunk$simulation),
      variable = unique(chunk$variable),
      importance = abs(pooled$estimate),
      estimate = pooled$estimate,
      std.error = pooled$std.error,
      conf.low = pooled$estimate - 1.96 * pooled$std.error,
      conf.high = pooled$estimate + 1.96 * pooled$std.error,
      p.value = NA_real_,
      n_imputations = pooled$m,
      stringsAsFactors = FALSE
    )
  }) |>
    dplyr::bind_rows()

  dplyr::bind_rows(aggregate_importance_within_sim(other), pooled_df)
}

pool_variable_importance <- function(importance_df) {
  if (!nrow(importance_df)) return(importance_df)
  pool_qgcomp_with_rubin(importance_df)
}
