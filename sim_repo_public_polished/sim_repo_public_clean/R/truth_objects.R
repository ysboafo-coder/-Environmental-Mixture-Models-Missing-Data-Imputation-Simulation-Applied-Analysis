# Truth objects for variable-selection evaluation

build_truth <- function(exposure_names = paste0("X", 1:10), active_count = 5) {
  stopifnot(active_count >= 0, active_count <= length(exposure_names))
  active <- exposure_names[seq_len(active_count)]
  inactive <- setdiff(exposure_names, active)

  list(
    active = active,
    inactive = inactive,
    indicator = stats::setNames(
      c(rep(1L, length(active)), rep(0L, length(inactive))),
      c(active, inactive)
    ),
    qgcomp_names = paste0(exposure_names, "_q"),
    wqs_names = exposure_names,
    bwqs_names = paste0("W_", exposure_names),
    bkmr_names = exposure_names,
    glmnet_names = exposure_names
  )
}
