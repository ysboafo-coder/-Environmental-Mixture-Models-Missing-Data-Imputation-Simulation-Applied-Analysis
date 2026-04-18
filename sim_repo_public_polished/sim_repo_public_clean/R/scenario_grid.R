# Scenario grid builder

build_scenario_grid <- function(include_complete = TRUE) {
  complete_grid <- if (include_complete) {
    expand.grid(
      outcome_type = c("continuous", "binary"),
      relationship = c("linear", "nonlinear"),
      distribution = c("mvnorm", "mvt"),
      missing_mechanism = "none",
      missing_target = "complete",
      method = "complete",
      stringsAsFactors = FALSE
    )
  } else NULL

  missing_grid <- expand.grid(
    outcome_type = c("continuous", "binary"),
    relationship = c("linear", "nonlinear"),
    distribution = c("mvnorm", "mvt"),
    missing_mechanism = c("MAR", "MNAR"),
    missing_target = c("exposure", "outcome", "exposure_outcome"),
    method = c("available", "mean", "median", "knn", "mice", "amelia"),
    stringsAsFactors = FALSE
  )

  out <- rbind(complete_grid, missing_grid)
  out$scenario_name <- paste(
    out$outcome_type, out$relationship, out$distribution,
    out$missing_mechanism, out$missing_target, out$method,
    sep = "_"
  )
  out
}
