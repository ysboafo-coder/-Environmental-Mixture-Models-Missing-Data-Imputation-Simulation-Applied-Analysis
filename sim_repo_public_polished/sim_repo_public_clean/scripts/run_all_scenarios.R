# Example driver script

source("R/generate_data.R")
source("R/induce_missingness.R")
source("R/handle_missing_data.R")
source("R/fit_models.R")
source("R/run_one_iteration.R")
source("R/utils_parallel.R")
source("R/run_simulation_scenario.R")
source("R/scenario_grid.R")

grid <- build_scenario_grid(include_complete = TRUE)

for (i in seq_len(min(3, nrow(grid)))) {
  cfg <- as.list(grid[i, ])
  cfg$nsim <- 5
  cfg$parallel <- TRUE
  cfg$n_cores <- 2
  cfg$output_dir <- "results"

  message("Running scenario: ", cfg$scenario_name)
  res <- run_simulation_scenario(cfg)
  save_simulation_result(res)
}
