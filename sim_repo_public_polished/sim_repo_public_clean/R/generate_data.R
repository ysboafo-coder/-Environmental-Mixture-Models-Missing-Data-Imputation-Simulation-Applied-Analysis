# Data generation helpers for the simulation study

simulate_data_continuous <- function(n,
                                     n_exposures = 10,
                                     rho = 0.2,
                                     relationship = c("linear", "nonlinear"),
                                     distribution = c("mvnorm", "mvt"),
                                     e_sd = 0.2) {
  relationship <- match.arg(relationship)
  distribution <- match.arg(distribution)

  mean_exposures <- rep(0, n_exposures)
  cov_exposures <- matrix(rho, nrow = n_exposures, ncol = n_exposures)
  diag(cov_exposures) <- 1

  age <- rnorm(n, mean = 50, sd = 10)
  gender <- sample(c(0, 1), n, replace = TRUE)
  income <- rnorm(n, mean = 50000, sd = 10000)

  exposures <- switch(
    distribution,
    mvnorm = MASS::mvrnorm(n, mean_exposures, cov_exposures),
    mvt    = mvtnorm::rmvt(n, sigma = cov_exposures, df = 10, delta = mean_exposures)
  )

  exposures <- exposures + rnorm(n * n_exposures, mean = 0, sd = 1e-6)
  colnames(exposures) <- paste0("X", seq_len(n_exposures))

  if (relationship == "linear") {
    outcome <- 0.5 * exposures[, 1] + 0.3 * exposures[, 2] - 0.2 * exposures[, 3] +
      0.1 * exposures[, 4] - 0.05 * exposures[, 5] +
      rnorm(n, mean = 0, sd = e_sd)
  } else {
    outcome <- exposures[, 1] * (exposures[, 1] > 0) +
      exp(exposures[, 2]) +
      abs(exposures[, 3]) +
      exposures[, 4]^2 +
      (exposures[, 5] + 1)^2 +
      rnorm(n, mean = 0, sd = e_sd)
  }

  data.frame(exposures, age = age, gender = gender, income = income, outcome = outcome)
}

simulate_data_binary <- function(n,
                                 n_exposures = 10,
                                 rho = 0.2,
                                 relationship = c("linear", "nonlinear"),
                                 distribution = c("mvnorm", "mvt")) {
  relationship <- match.arg(relationship)
  distribution <- match.arg(distribution)

  mean_exposures <- rep(0, n_exposures)
  cov_exposures <- matrix(rho, nrow = n_exposures, ncol = n_exposures)
  diag(cov_exposures) <- 1

  age <- rnorm(n, mean = 50, sd = 10)
  gender <- sample(c(0, 1), n, replace = TRUE)
  income <- rnorm(n, mean = 50000, sd = 10000)

  exposures <- switch(
    distribution,
    mvnorm = MASS::mvrnorm(n, mean_exposures, cov_exposures),
    mvt    = mvtnorm::rmvt(n, sigma = cov_exposures, df = 10, delta = mean_exposures)
  )

  exposures <- exposures + rnorm(n * n_exposures, mean = 0, sd = 1e-6)
  colnames(exposures) <- paste0("X", seq_len(n_exposures))

  x <- exposures
  mu <- if (relationship == "linear") {
    0.5 * x[, 1] + 0.3 * x[, 2] - 0.2 * x[, 3] + 0.1 * x[, 4] - 0.05 * x[, 5]
  } else {
    x[, 1] * (x[, 1] > 0) +
      exp(x[, 2]) +
      abs(x[, 3]) +
      x[, 4]^2 +
      (x[, 5] + 1)^2 +
      x[, 1] * exp(x[, 2]) +
      x[, 3] * (x[, 1] > 0) +
      x[, 2] * (x[, 1]^2) / (x[, 1] + 1)^2 +
      exp(x[, 2]) * x[, 3] +
      x[, 3] * ((x[, 5]^2) + (x[, 3]^2) + (x[, 5] + 1)^2)
  }

  p <- 1 / (1 + exp(-mu))
  outcome <- stats::rbinom(n, size = 1, prob = p)

  data.frame(exposures, age = age, gender = gender, income = income, outcome = outcome)
}

generate_data <- function(config) {
  if (config$outcome_type == "continuous") {
    return(simulate_data_continuous(
      n = config$n,
      n_exposures = config$n_exposures,
      rho = config$rho,
      relationship = config$relationship,
      distribution = config$distribution,
      e_sd = config$e_sd
    ))
  }

  if (config$outcome_type == "binary") {
    return(simulate_data_binary(
      n = config$n,
      n_exposures = config$n_exposures,
      rho = config$rho,
      relationship = config$relationship,
      distribution = config$distribution
    ))
  }

  stop("Unsupported outcome_type: ", config$outcome_type)
}
