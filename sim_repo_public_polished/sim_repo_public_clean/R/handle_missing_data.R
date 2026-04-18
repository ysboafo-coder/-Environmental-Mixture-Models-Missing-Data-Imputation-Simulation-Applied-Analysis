# Missing-data handling helpers

handle_missing_data <- function(data, config) {
  method <- config$method
  exposure_names <- config$exposure_names
  covariate_names <- config$covariate_names
  outcome_type <- config$outcome_type
  m <- config$m
  k <- config$k

  if (method == "complete") return(list(type = "single", data_list = list(data)))
  if (method == "available") return(list(type = "single", data_list = list(stats::na.omit(data))))

  if (method == "mean") {
    dat <- data
    for (nm in exposure_names) dat[[nm]][is.na(dat[[nm]])] <- mean(dat[[nm]], na.rm = TRUE)
    return(list(type = "single", data_list = list(dat)))
  }

  if (method == "median") {
    dat <- data
    for (nm in exposure_names) dat[[nm]][is.na(dat[[nm]])] <- stats::median(dat[[nm]], na.rm = TRUE)
    return(list(type = "single", data_list = list(dat)))
  }

  if (method == "knn") {
    vars_to_impute <- character(0)
    if (anyNA(data[exposure_names])) vars_to_impute <- c(vars_to_impute, exposure_names)
    if (anyNA(data$outcome)) vars_to_impute <- c(vars_to_impute, "outcome")
    vars_to_impute <- unique(vars_to_impute)

    dat <- if (length(vars_to_impute) == 0) {
      data
    } else {
      VIM::kNN(
        data,
        variable = vars_to_impute,
        dist_var = unique(c(exposure_names, covariate_names, "outcome")),
        k = k,
        imp_var = FALSE
      )
    }

    if (outcome_type == "binary") {
      dat$outcome <- as.integer(round(dat$outcome))
      dat$outcome[dat$outcome < 0] <- 0L
      dat$outcome[dat$outcome > 1] <- 1L
    }

    return(list(type = "single", data_list = list(dat)))
  }

  if (method == "mice") {
    imp <- mice::mice(data, m = m, printFlag = FALSE)
    return(list(type = "multiple", data_list = lapply(seq_len(m), function(j) mice::complete(imp, action = j))))
  }

  if (method == "amelia") {
    imp <- Amelia::amelia(data, m = m)
    return(list(type = "multiple", data_list = imp$imputations))
  }

  stop("Unsupported method: ", method)
}
