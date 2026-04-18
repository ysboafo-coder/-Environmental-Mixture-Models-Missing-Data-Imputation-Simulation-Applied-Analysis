# Model-specific extractors for variable importance / selection

safe_col <- function(df, candidates, default = NA) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(default, nrow(df)))
  df[[hit[1]]]
}

normalize_exposure_name <- function(x) {
  x <- as.character(x)
  x <- gsub("^W_", "", x)
  x <- gsub("_q$", "", x)
  x
}

extract_wqs_importance <- function(model, simulation, imputation = 1L, model_name = "wqs") {
  if (is.null(model) || (length(model) == 1 && is.na(model))) return(NULL)
  fw <- tryCatch(model$final_weights, error = function(e) NULL)
  if (is.null(fw) || !is.data.frame(fw) || nrow(fw) == 0) return(NULL)

  variable_raw <- safe_col(fw, c("mix_name", "variable_name", "variable"), default = NA)
  estimate <- as.numeric(safe_col(fw, c("Estimate", "estimate", "mean", "Mean"), default = NA))
  ll <- suppressWarnings(as.numeric(safe_col(fw, c("LL", "lower", "Lower", "CI.lower", "conf.low"), default = NA)))
  ul <- suppressWarnings(as.numeric(safe_col(fw, c("UL", "upper", "Upper", "CI.upper", "conf.high"), default = NA)))

  data.frame(
    simulation = simulation,
    imputation = imputation,
    model = model_name,
    variable_raw = variable_raw,
    variable = normalize_exposure_name(variable_raw),
    importance = estimate,
    estimate = estimate,
    std.error = NA_real_,
    conf.low = ll,
    conf.high = ul,
    p.value = NA_real_,
    stringsAsFactors = FALSE
  )
}

extract_bwqs_importance <- function(model, simulation, imputation = 1L, model_name = "bwqs") {
  if (is.null(model) || (length(model) == 1 && is.na(model))) return(NULL)

  candidate_tables <- list(
    tryCatch(model$final_weights, error = function(e) NULL),
    tryCatch(model$weights, error = function(e) NULL),
    tryCatch(model$summary_fit$Weights, error = function(e) NULL),
    tryCatch(model$summary_fit$weights, error = function(e) NULL)
  )
  fw <- candidate_tables[[which(vapply(candidate_tables, function(x) is.data.frame(x) && nrow(x) > 0, logical(1)))[1]]]
  if (is.null(fw)) return(NULL)

  variable_raw <- safe_col(fw, c("variable_name", "mix_name", "variable", "Parameter", "parameter"), default = NA)
  estimate <- as.numeric(safe_col(fw, c("Estimate", "estimate", "mean", "Mean", "postmean", "post_mean"), default = NA))
  ll <- suppressWarnings(as.numeric(safe_col(fw, c("LL", "lower", "Lower", "lcl", "CI.lower", "conf.low"), default = NA)))
  ul <- suppressWarnings(as.numeric(safe_col(fw, c("UL", "upper", "Upper", "ucl", "CI.upper", "conf.high"), default = NA)))

  data.frame(
    simulation = simulation,
    imputation = imputation,
    model = model_name,
    variable_raw = variable_raw,
    variable = normalize_exposure_name(variable_raw),
    importance = estimate,
    estimate = estimate,
    std.error = NA_real_,
    conf.low = ll,
    conf.high = ul,
    p.value = NA_real_,
    stringsAsFactors = FALSE
  )
}

extract_qgcomp_importance <- function(model, simulation, imputation = 1L, model_name = "qgcomp") {
  if (is.null(model) || (length(model) == 1 && is.na(model))) return(NULL)
  sm <- tryCatch(summary(model), error = function(e) NULL)
  if (is.null(sm)) return(NULL)

  coef_tab <- tryCatch(sm$coefficients, error = function(e) NULL)
  if (is.null(coef_tab)) coef_tab <- tryCatch(coef(sm), error = function(e) NULL)
  if (is.null(coef_tab)) return(NULL)

  coef_tab <- as.data.frame(coef_tab)
  coef_tab$term <- rownames(coef_tab)
  coef_tab <- coef_tab[grepl("^X\\d+(_q)?$", coef_tab$term), , drop = FALSE]
  if (nrow(coef_tab) == 0) return(NULL)

  estimate_col <- intersect(c("Estimate", "estimate"), names(coef_tab))[1]
  se_col <- intersect(c("Std. Error", "Std.Error", "std.error"), names(coef_tab))[1]
  p_col <- intersect(c("Pr(>|t|)", "Pr(>|z|)", "p.value", "pvalue"), names(coef_tab))[1]

  data.frame(
    simulation = simulation,
    imputation = imputation,
    model = model_name,
    variable_raw = coef_tab$term,
    variable = normalize_exposure_name(coef_tab$term),
    importance = abs(as.numeric(coef_tab[[estimate_col]])),
    estimate = as.numeric(coef_tab[[estimate_col]]),
    std.error = if (!is.na(se_col)) as.numeric(coef_tab[[se_col]]) else NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    p.value = if (!is.na(p_col)) as.numeric(coef_tab[[p_col]]) else NA_real_,
    stringsAsFactors = FALSE
  )
}

extract_bkmr_importance <- function(model, simulation, imputation = 1L, model_name = "bkmr") {
  if (is.null(model) || (length(model) == 1 && is.na(model))) return(NULL)
  model_obj <- if (is.list(model) && !is.null(model$model)) model$model else model
  pips <- tryCatch(ExtractPIPs(model_obj), error = function(e) NULL)
  if (is.null(pips) || !is.data.frame(pips) || nrow(pips) == 0) return(NULL)

  variable_raw <- safe_col(pips, c("variable", "Variable"), default = NA)
  pip <- as.numeric(safe_col(pips, c("PIP", "pip"), default = NA))

  data.frame(
    simulation = simulation,
    imputation = imputation,
    model = model_name,
    variable_raw = variable_raw,
    variable = normalize_exposure_name(variable_raw),
    importance = pip,
    estimate = pip,
    std.error = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    p.value = NA_real_,
    stringsAsFactors = FALSE
  )
}

extract_glmnet_importance <- function(model, simulation, imputation = 1L, model_name = "glmnet") {
  if (is.null(model) || (length(model) == 1 && is.na(model))) return(NULL)
  beta <- tryCatch(as.matrix(stats::coef(model, s = "lambda.min")), error = function(e) NULL)
  if (is.null(beta)) return(NULL)

  beta_df <- data.frame(
    variable_raw = rownames(beta),
    estimate = as.numeric(beta[, 1]),
    stringsAsFactors = FALSE
  )
  beta_df <- beta_df[beta_df$variable_raw != "(Intercept)", , drop = FALSE]
  beta_df <- beta_df[grepl("^X\\d+$", beta_df$variable_raw), , drop = FALSE]
  if (nrow(beta_df) == 0) return(NULL)

  data.frame(
    simulation = simulation,
    imputation = imputation,
    model = model_name,
    variable_raw = beta_df$variable_raw,
    variable = normalize_exposure_name(beta_df$variable_raw),
    importance = abs(beta_df$estimate),
    estimate = beta_df$estimate,
    std.error = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    p.value = NA_real_,
    stringsAsFactors = FALSE
  )
}

extract_model_importance <- function(model, model_name, simulation, imputation = 1L) {
  switch(
    model_name,
    "qws.pos.out" = extract_wqs_importance(model, simulation, imputation, model_name = "wqs"),
    "bqws.out" = extract_bwqs_importance(model, simulation, imputation, model_name = "bwqs"),
    "gcomp.out" = extract_qgcomp_importance(model, simulation, imputation, model_name = "qgcomp"),
    "bkmr.out" = extract_bkmr_importance(model, simulation, imputation, model_name = "bkmr"),
    "cvfit_elasticnet.out" = extract_glmnet_importance(model, simulation, imputation, model_name = "elasticnet"),
    "cvfit_lasso.out" = extract_glmnet_importance(model, simulation, imputation, model_name = "lasso"),
    stop("Unsupported model name: ", model_name)
  )
}

extract_variable_importance <- function(result_object) {
  model_names <- names(result_object$results)
  out <- list()
  idx <- 1L

  for (model_name in model_names) {
    sims <- result_object$results[[model_name]]
    for (sim in seq_along(sims)) {
      imps <- sims[[sim]]
      for (imp in seq_along(imps)) {
        tmp <- extract_model_importance(imps[[imp]], model_name, simulation = sim, imputation = imp)
        if (!is.null(tmp) && nrow(tmp) > 0) {
          out[[idx]] <- tmp
          idx <- idx + 1L
        }
      }
    }
  }

  if (length(out) == 0) return(data.frame())
  dplyr::bind_rows(out)
}
