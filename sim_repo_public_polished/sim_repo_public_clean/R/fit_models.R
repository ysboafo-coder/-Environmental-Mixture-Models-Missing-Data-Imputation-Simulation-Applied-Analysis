# Model-fitting helpers

fit_all_models <- function(data, config) {
  exposure_names <- config$exposure_names
  covariate_names <- config$covariate_names
  outcome_type <- config$outcome_type

  family_name <- if (outcome_type == "binary") "binomial" else "gaussian"
  family_obj <- if (outcome_type == "binary") stats::binomial() else stats::gaussian()

  Y <- data$outcome
  Z <- as.matrix(data[, exposure_names, drop = FALSE])
  X <- as.matrix(data[, covariate_names, drop = FALSE])

  qgcomp_formula <- stats::reformulate(
    termlabels = c(exposure_names, covariate_names),
    response = "outcome"
  )

  bkmr_fit <- tryCatch({
    Zs <- scale(Z)
    if (outcome_type == "binary" && length(unique(Y)) < 2) return(NA)

    if (outcome_type == "binary") {
      Zs <- apply(Zs, 2, function(x) pmin(pmax(x, -10), 10))
      rownames(Zs) <- NULL; colnames(Zs) <- NULL
      rownames(X) <- NULL; colnames(X) <- NULL
      knots_matrix <- try(fields::cover.design(Zs, nd = 30)$design, silent = TRUE)
      if (inherits(knots_matrix, "try-error")) knots_matrix <- NULL

      list(
        model = bkmr::kmbayes(
          y = Y, Z = Zs, X = X,
          iter = config$bkmr_iter,
          verbose = FALSE,
          varsel = TRUE,
          family = family_name,
          knots = knots_matrix
        ),
        binary_Y = Y
      )
    } else {
      bkmr::kmbayes(
        y = Y, Z = Zs, X = X,
        iter = config$bkmr_iter,
        verbose = FALSE,
        varsel = TRUE,
        family = family_name
      )
    }
  }, error = function(e) NA)

  list(
    qws.pos.out = tryCatch(
      gWQS::gwqs(
        stats::as.formula("outcome ~ wqs + age + gender + income"),
        mix_name = exposure_names,
        data = data,
        q = config$wqs_q,
        validation = config$wqs_validation,
        b = config$wqs_b,
        b1_pos = TRUE,
        rh = config$wqs_rh,
        b_constr = FALSE,
        family = family_name,
        seed = config$wqs_seed
      ),
      error = function(e) NA
    ),
    bqws.out = tryCatch(
      BWQS::bwqs(
        stats::as.formula("outcome ~ age + gender + income"),
        mix_name = exposure_names,
        iter = config$bwqs_iter,
        data = data,
        q = config$bwqs_q,
        family = family_name
      ),
      error = function(e) NA
    ),
    gcomp.out = tryCatch(
      qgcomp::qgcomp.glm.noboot(
        qgcomp_formula,
        data = data[, c(exposure_names, covariate_names, "outcome")],
        family = family_obj,
        bayes = TRUE
      ),
      error = function(e) NA
    ),
    bkmr.out = bkmr_fit,
    cvfit_elasticnet.out = tryCatch(
      glmnet::cv.glmnet(
        x = as.matrix(data[, c(exposure_names, covariate_names)]),
        y = Y,
        alpha = 0.5,
        family = family_name
      ),
      error = function(e) NA
    ),
    cvfit_lasso.out = tryCatch(
      glmnet::cv.glmnet(
        x = as.matrix(data[, c(exposure_names, covariate_names)]),
        y = Y,
        alpha = 1,
        family = family_name
      ),
      error = function(e) NA
    )
  )
}
