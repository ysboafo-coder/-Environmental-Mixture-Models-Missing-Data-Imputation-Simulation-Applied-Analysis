# One-iteration wrapper

run_one_iteration <- function(i, config) {
  set.seed(config$seed + i)
  dat_complete <- generate_data(config)
  dat_missing <- apply_missingness(dat_complete, config)
  handled <- handle_missing_data(dat_missing, config)

  out <- vector("list", length = length(handled$data_list))
  for (j in seq_along(handled$data_list)) {
    out[[j]] <- fit_all_models(handled$data_list[[j]], config)
  }
  if (length(out) == 0) out <- list(list())
  out
}
