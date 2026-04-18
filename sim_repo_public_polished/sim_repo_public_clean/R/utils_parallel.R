# Parallel helpers

run_iterations_parallel <- function(nsim, config) {
  worker_fun <- function(i) run_one_iteration(i, config)

  if (!isTRUE(config$parallel)) return(lapply(seq_len(nsim), worker_fun))

  n_cores <- min(config$n_cores, max(1, parallel::detectCores() - 1))

  if (.Platform$OS.type == "windows") {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterSetRNGStream(cl, iseed = config$seed)

    parallel::clusterEvalQ(cl, {
      library(gWQS); library(BWQS); library(qgcomp); library(bkmr); library(glmnet)
      library(MASS); library(mvtnorm)
      suppressWarnings(try(library(VIM), silent = TRUE))
      suppressWarnings(try(library(mice), silent = TRUE))
      suppressWarnings(try(library(Amelia), silent = TRUE))
      suppressWarnings(try(library(fields), silent = TRUE))
      NULL
    })

    parallel::clusterExport(
      cl,
      varlist = c(
        "logit",
        "simulate_data_continuous", "simulate_data_binary", "generate_data",
        "induce_missing_exposure_mar", "induce_missing_outcome_mar", "induce_missing_exposure_outcome_mar",
        "induce_missing_exposure_mnar", "induce_missing_outcome_mnar", "induce_missing_exposure_outcome_mnar",
        "apply_missingness", "handle_missing_data", "fit_all_models", "run_one_iteration"
      ),
      envir = environment()
    )

    return(parallel::parLapply(cl, seq_len(nsim), worker_fun))
  }

  parallel::mclapply(seq_len(nsim), worker_fun, mc.cores = n_cores)
}
