# Missingness helpers for MAR and MNAR scenarios

logit <- function(x) 1 / (1 + exp(-x))

induce_missing_exposure_mar <- function(data, n_exposures, missingness_rate) {
  out <- data
  n <- nrow(out)
  for (j in seq_len(n_exposures)) {
    prob <- logit(0.1 * out$age + 0.2 * out$gender - 0.00001 * out$income)
    idx <- which(stats::runif(n) < prob * missingness_rate)
    out[idx, j] <- NA
  }
  out
}

induce_missing_outcome_mar <- function(data, missingness_rate) {
  out <- data
  n <- nrow(out)
  prob <- logit(0.1 * out$age + 0.2 * out$gender - 0.00001 * out$income)
  idx <- which(stats::runif(n) < prob * missingness_rate)
  out$outcome[idx] <- NA
  out
}

induce_missing_exposure_outcome_mar <- function(data, n_exposures, missingness_rate) {
  induce_missing_outcome_mar(
    induce_missing_exposure_mar(data, n_exposures, missingness_rate),
    missingness_rate
  )
}

induce_missing_exposure_mnar <- function(data, n_exposures, missingness_rate) {
  out <- data
  n <- nrow(out)
  for (j in seq_len(n_exposures)) {
    exposure_z <- as.numeric(scale(out[[j]]))
    prob <- logit(0.8 * exposure_z + 0.1 * out$age + 0.2 * out$gender - 0.00001 * out$income)
    idx <- which(stats::runif(n) < prob * missingness_rate)
    out[idx, j] <- NA
  }
  out
}

induce_missing_outcome_mnar <- function(data, missingness_rate) {
  out <- data
  n <- nrow(out)
  outcome_z <- as.numeric(scale(out$outcome))
  prob <- logit(0.8 * outcome_z + 0.1 * out$age + 0.2 * out$gender - 0.00001 * out$income)
  idx <- which(stats::runif(n) < prob * missingness_rate)
  out$outcome[idx] <- NA
  out
}

induce_missing_exposure_outcome_mnar <- function(data, n_exposures, missingness_rate) {
  induce_missing_outcome_mnar(
    induce_missing_exposure_mnar(data, n_exposures, missingness_rate),
    missingness_rate
  )
}

apply_missingness <- function(data, config) {
  mechanism <- config$missing_mechanism
  target <- config$missing_target

  if (is.null(mechanism) || mechanism == "none" || target == "complete") return(data)

  key <- paste(mechanism, target, sep = "_")
  switch(
    key,
    MAR_exposure = induce_missing_exposure_mar(data, config$n_exposures, config$missingness_rate),
    MAR_outcome = induce_missing_outcome_mar(data, config$missingness_rate),
    MAR_exposure_outcome = induce_missing_exposure_outcome_mar(data, config$n_exposures, config$missingness_rate),
    MNAR_exposure = induce_missing_exposure_mnar(data, config$n_exposures, config$missingness_rate),
    MNAR_outcome = induce_missing_outcome_mnar(data, config$missingness_rate),
    MNAR_exposure_outcome = induce_missing_exposure_outcome_mnar(data, config$n_exposures, config$missingness_rate),
    stop("Unsupported missingness combination: ", key)
  )
}
