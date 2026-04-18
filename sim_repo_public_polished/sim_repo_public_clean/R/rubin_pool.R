# Rubin pooling helpers

rubin_pool <- function(estimates, ses) {
  estimates <- as.numeric(estimates)
  ses <- as.numeric(ses)

  keep <- !is.na(estimates) & !is.na(ses)
  estimates <- estimates[keep]
  ses <- ses[keep]

  m <- length(estimates)

  if (m == 0) {
    return(data.frame(
      estimate = NA_real_,
      std.error = NA_real_,
      within_var = NA_real_,
      between_var = NA_real_,
      total_var = NA_real_,
      m = 0,
      stringsAsFactors = FALSE
    ))
  }

  qbar <- mean(estimates)
  ubar <- mean(ses^2)
  b <- if (m > 1) stats::var(estimates) else 0
  tvar <- ubar + (1 + 1 / m) * b
  se_pool <- sqrt(tvar)

  data.frame(
    estimate = qbar,
    std.error = se_pool,
    within_var = ubar,
    between_var = b,
    total_var = tvar,
    m = m,
    stringsAsFactors = FALSE
  )
}

pool_scalar_df <- function(df, estimate_col = "estimate", se_col = "std.error") {
  if (!all(c(estimate_col, se_col) %in% names(df))) {
    stop("Required columns missing for Rubin pooling")
  }
  rubin_pool(df[[estimate_col]], df[[se_col]])
}
