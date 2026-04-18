# Scenario-level simulation runner

build_default_config <- function() {
  list(
    scenario_name = NULL,
    outcome_type = "continuous",
    relationship = "linear",
    distribution = "mvnorm",
    missing_mechanism = "none",
    missing_target = "complete",
    method = "complete",
    n = 1000,
    n_exposures = 10,
    rho = 0.2,
    e_sd = 0.2,
    missingness_rate = 0.25,
    m = 5,
    k = 5,
    nsim = 100,
    seed = 123,
    parallel = TRUE,
    n_cores = 4,
    exposure_names = paste0("X", 1:10),
    covariate_names = c("age", "gender", "income"),
    wqs_q = 10,
    wqs_validation = 0.6,
    wqs_b = 2,
    wqs_rh = 5,
    wqs_seed = 2016,
    bwqs_iter = 100,
    bwqs_q = 4,
    bkmr_iter = 100,
    output_dir = "results"
  )
}

merge_config <- function(config) utils::modifyList(build_default_config(), config)

scenario_id <- function(config) {
  if (!is.null(config$scenario_name) && nzchar(config$scenario_name)) return(config$scenario_name)
  paste(
    c(config$outcome_type, config$relationship, config$distribution,
      config$missing_mechanism, config$missing_target, config$method),
    collapse = "_"
  )
}

standardize_results <- function(raw_results) {
  model_names <- c("qws.pos.out", "bqws.out", "gcomp.out", "bkmr.out", "cvfit_elasticnet.out", "cvfit_lasso.out")
  out <- setNames(vector("list", length(model_names)), model_names)
  for (m in model_names) {
    out[[m]] <- lapply(raw_results, function(sim_result) {
      lapply(sim_result, function(imp_result) imp_result[[m]])
    })
  }
  out
}

run_simulation_scenario <- function(config) {
  config <- merge_config(config)
  start_time <- Sys.time()
  raw_results <- run_iterations_parallel(config$nsim, config)
  end_time <- Sys.time()
  elapsed_seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))

  list(
    meta = config,
    runtime = list(
      start_time = start_time,
      end_time = end_time,
      total_seconds = elapsed_seconds,
      average_seconds_per_sim = elapsed_seconds / config$nsim
    ),
    results = standardize_results(raw_results)
  )
}

save_simulation_result <- function(result, file_prefix = NULL) {
  config <- result$meta
  out_dir <- config$output_dir
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  prefix <- if (is.null(file_prefix)) scenario_id(config) else file_prefix
  runtime_file <- file.path(out_dir, paste0("runtime_", prefix, ".txt"))
  result_file <- file.path(out_dir, paste0("result_", prefix, ".rds"))

  writeLines(
    c(
      paste("Scenario:", prefix),
      paste("Total Runtime (seconds):", round(result$runtime$total_seconds, 2)),
      paste("Average Runtime per Simulation (seconds):", round(result$runtime$average_seconds_per_sim, 2))
    ),
    runtime_file
  )
  saveRDS(result, result_file)
  invisible(list(runtime_file = runtime_file, result_file = result_file))
}
