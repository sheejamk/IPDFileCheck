
#######################################################################
#' Plotting and return the residuals after survival model
#' @param param_to_be_estimated  parameter to be estimated
#' @param indep_var independent variable
#' @param covariates  covariates
#' @param fit  fit object from survreg method
#' @return plot and the residuals
#' @examples
#' \donttest{
#' data_for_survival <- survival::lung
#' surv_estimated <- use_parametric_survival("status", data_for_survival,
#' "sex",
#' info_distribution = "weibull",covariates = c("ph.ecog"), "time")
#' plot_return_residual_survival("status", "sex",
#' covariates = c("ph.ecog"),surv_estimated$fit)
#' }
#' @export
plot_return_residual_survival <- function(param_to_be_estimated, indep_var,
                                          covariates, fit) {
  if (!("survreg" %in% class(fit)))
    stop("Error- Fit object should be of type survreg")
  # checking if parameter to be estimated is NULL or NA
  check_list <- list(param_to_be_estimated, indep_var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Error - some of the required parameters are NULL or NA")

  name_file_plot <- paste0("Survival_residuals_", param_to_be_estimated,
                           "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  oldpar <- graphics::par(no.readonly = TRUE)
  graphics::par(mar = c(4, 4, 2, 2))

  residuals_response <- stats::residuals(fit, type = "response")
  residuals_deviance <- stats::residuals(fit, type = "deviance")
  residuals_working <- stats::residuals(fit, type = "working")
  residuals_ldshape <- stats::residuals(fit, type = "ldshape")
  residuals_ldresp <- stats::residuals(fit, type = "ldresp")
  residuals_ldcase <- stats::residuals(fit, type = "ldcase")

  plot(residuals_response, type = "p", main = "Response", ylab =
         "Residuals", lwd = 2)
  plot(residuals_deviance, type = "p", main = "Deviance", ylab =
         "Residuals", lwd = 2)
  if (sum(is.na(covariates)) == 0) {
    nos <- 1
  } else {
    nos <- ceiling(3 + length(covariates)) / 2
  }
  residuals_matrix <- stats::residuals(fit, type = "matrix")
  residuals_dfbeta <- stats::residuals(fit, type = "dfbeta")
  residuals_dfbetas <- stats::residuals(fit, type = "dfbetas")

  graphics::par(mfrow = c(2, 3))
  for (i in seq_len(dim(residuals_matrix)[2])) {
    main_name <- paste("Matrix", colnames(residuals_matrix)[i], sep = ":")
    plot(residuals_matrix[, i], type = "p", main = main_name, ylab =
           "Residuals", lwd = 2)
  }
   graphics::par(mfrow = c(2, nos))
  for (i in seq_len(dim(residuals_dfbeta)[2]))
    plot(residuals_dfbeta[, i], type = "p", main = "Dfbeta", ylab =
           "Residuals", lwd = 2)
  for (i in seq_len(dim(residuals_dfbetas)[2]))
    plot(residuals_dfbetas[, i], type = "p", main = "Dfbetas", ylab =
           "Residuals", lwd = 2)
  on.exit(graphics::par(oldpar))
  grDevices::dev.off()

  results_residuals <- structure(list(
    Response_residual = residuals_response,
    Deviance_residual = residuals_deviance,
    Working_residual = residuals_working,
    Ldshape_residual = residuals_ldshape,
    Ldresp_residual = residuals_ldresp,
    Ldcase_residual = residuals_ldcase,
    Matrix_residual = residuals_matrix,
    Dfbeta_residual = residuals_dfbeta,
    Dfbetas_residual = residuals_dfbetas
  ))
  return(results_residuals)
}
#######################################################################
#' Plot the predicted survival curves for covariates keeping the others fixed
#' @param param_to_be_estimated   parameter to be estimated
#' @param indep_var  variable for which the levels have to be identified
#' @param covariates the  covariates
#' @param dataset  the dataset where these variables contain
#' @param fit the fit result survreg
#' @param timevar_survival  time variable from the dataset
#' @return plot
#' @examples
#' \donttest{
#' data_for_survival <- survival::lung
#' surv_estimated <- use_parametric_survival("status", data_for_survival,
#' "sex",
#' info_distribution = "weibull",covariates = c("ph.ecog"),"time")
#' plot_prediction_parametric_survival("status", "sex",
#' covariates = c("ph.ecog"),data_for_survival, surv_estimated$fit, "time")
#' }
#' @export
plot_prediction_parametric_survival <- function(param_to_be_estimated,
                                                indep_var, covariates, dataset,
                                                fit, timevar_survival) {
  if (!("survreg" %in% class(fit)))
    stop("Error- Fit object should be of type survreg")

  check_list <- list(param_to_be_estimated, indep_var, timevar_survival)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Error - some of the required parameters are NULL or NA")

  # dataset should not be null
  if (is.null(dataset))
    stop("Error - dataset can not be null")

  if (sum(is.na(covariates)) != 0) {
    no_var <- 1
  }else{
    no_var <- 1 + length(covariates)
  }
  name_file_plot <- paste0("Survival_covariates_", param_to_be_estimated,
                           "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  for (i in 1:no_var) {
    if (i == 1) {
      var <- indep_var
      other_fixed <- covariates
    }else{
      var <- covariates[i - 1]
      other_fixed <- c(indep_var, covariates[- (i - 1)])
    }
    categorical <- list()
    if (sum(!is.na(other_fixed)) == length(other_fixed)) {
      for (j in seq_len(length(other_fixed))) {
        variable <- dataset[[other_fixed[j]]]
        result <- suppressWarnings(as.numeric(levels(factor(variable))))
        if (sum(is.na(result)) == 0) catego <- FALSE else catego <- TRUE
        categorical <- c(categorical, catego)
      }
    }
    categorical <- unlist(categorical)
    newdata <- as.data.frame(create_new_dataset(var, other_fixed, dataset,
                                                categorical))
    pct <- 1:98 / 100
    # The 100th percentile of predicted survival is at +infinity
    prediction_value <- stats::predict(fit, newdata = newdata, type =
                                         "quantile", p = pct, se = TRUE)
    indep <- dataset[[var]]
    names <- var
    result <- suppressWarnings(as.numeric(levels(factor(indep))))
    if (sum(is.na(result)) == 0) {
      indep_lvl <- result
    } else {
      levels <- as.numeric(as.factor(indep))
      levels <- levels[is.na(levels)]
      indep_lvl <- unique(levels)
    }
    for (m in seq_len(length(indep_lvl))) {
      graphics::matplot(cbind(prediction_value$fit[m, ],
                              prediction_value$fit[m, ] +
                    2 * prediction_value$se.fit[m, ],
                    prediction_value$fit[m, ] - 2 *
                      prediction_value$se.fit[m, ]) / 30.5,
                    1 - pct, xlab = timevar_survival, ylab = "Survival",
                    type = "l",
                    lty = c(1, 2, 2), col = 3)
      graphics::legend("topright", legend = c(paste(names, "=", indep_lvl[m],
              sep = ""), "upper ci", "lower ci"), lty = c(1, 2, 2), lwd = 2,
              col = 3)
    }

  }
  grDevices::dev.off()
}
#######################################################################
#' create new dataset while keeping
#' cox regression results and returned coefficients
#' @param var   variable for which the levels have to be identified
#' usually indep variable
#' @param covar the other covariates
#' @param dataset  the dataset where these variables contain
#' @param categorical  are these variables categorical? True of false
#' @return new data frame
#' @examples
#' dataset <- survival::lung
#' new = create_new_dataset("status", c("age"), dataset, c(FALSE))
#' @export
create_new_dataset <- function(var, covar, dataset, categorical) {
  # checking if independent variable  is NULL or NA
  check_list <- list(var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Error - var is NULL or NA")
  #Error if data set is null
  if (is.null(dataset)) {
    stop("Error -  dataset can not be null")
  }
  #Error if categorical variable is null
  if (is.null(categorical)) {
    categorical <- c(FALSE)
  }
  check <- IPDFileCheck::check_column_exists(var, dataset)
  if (check != 0) {
    stop("Column of variable not in the dataset")
  }
  indep <- dataset[[var]]
  names <- var
  result <- suppressWarnings(as.numeric(levels(factor(indep))))
  if (sum(is.na(result)) == 0) {
    # means it is numerical
    indep_lvl <- result
  } else {
    indep_lvl <- levels(factor(indep))
  }
  #if the var is independent variable loop over the covariates else include
  # independent variable as fixed variable. If categorical take the
  # minimum value, else the mean for the fixed variables
  df <- as.data.frame(indep_lvl)
  if (sum(is.na(covar)) == 0) {
    for (i in seq_len(length(covar))) {
      this_var <- covar[i]
      check <- IPDFileCheck::check_column_exists(this_var, dataset)
      if (check != 0) {
        stop("Column of covariate not in the dataset")
      }
      fixed <- dataset[[covar[i]]]
      if (categorical[i]) {
        fixed_covar <- levels(factor(fixed))[1]
      } else {
        fixed_covar <- mean(fixed, na.rm = TRUE)
      }
      df <- cbind(df, fixed_covar)
      names <- c(names, covar[i])
    }
  }
  colnames(df) <- names
  return(df)
}

################################################################
#' Plotting survival function for all covariates using survfit
#' @param param_to_be_estimated  parameter to be estimated
#' @param dataset param describing the methods
#' @param indep_var independent variable
#' @param covariates  covariates
#' @param timevar_survival  time variable for survival analysis
#' @return plot and the survival function values
#' @examples
#' \donttest{
#'  data_for_survival <- survival::lung
#'  plot_return_survival_curve(param_to_be_estimated = "status",
#'  dataset = data_for_survival,indep_var = "sex",covariates = c("ph.ecog"),
#'  timevar_survival = "time")
#'  }
#' @export
plot_return_survival_curve <- function(param_to_be_estimated, dataset,
                                       indep_var,
                                       covariates, timevar_survival) {

  check_list <- list(param_to_be_estimated, indep_var, timevar_survival)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Error - some of the required parameters are NULL or NA")
  # dataset need not be null
  if (is.null(dataset))
    stop("Error - dataset can not be null")

  surv_object <- paste("survival::Surv(", timevar_survival, ",",
                       param_to_be_estimated, ")", sep = "")

  covariates_list <- make_string_covariates(covariates)

  if (sum(is.na(covariates_list)) != 0) {
    expression_recreated_forsurfit <- paste0("survival::survfit", "(",
                                          surv_object, " ~ ", indep_var, ", ",
                                             "data = dataset)",
                                             sep = ""
    )
  } else {
    expression_recreated_forsurfit <- paste0("survival::survfit", "(",
                                             surv_object, " ~ ",
                                             covariates_list, " + ",
                                        indep_var, ", ", "data = dataset)",
                                             sep = ""
    )
  }
  # estimate survival function using survival::survfit
  fit_survfit <- eval(parse(text = expression_recreated_forsurfit))


  name_file_plot <- paste0("Survival_function_default_", param_to_be_estimated,
                           "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  graphics::plot(fit_survfit)
  grDevices::dev.off()

  summary <- summary(fit_survfit)
  fac <- levels(factor(summary$strata))
  total_no <- length(fac)
  survival_fn <- list()

  name_file_plot <- paste0("Survival_function_", param_to_be_estimated, "_",
                           indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  if (sum(is.na(covariates)) != 0) {
    subplots <- 1
    no_levels <- length(levels(factor(dataset[[indep_var]])))
  } else {
    no_levels <- 4
    subplots <- ceiling(total_no / no_levels)
  }
  for (i in 1:subplots) {
    colours <- legend_here <- list()
    for (j in 1:no_levels) {
      k <- i * j
      times <- summary$time[which(summary$strata == fac[k])]
      survs <- summary$surv[which(summary$strata == fac[k])]
      survs_lb <- summary$lower[which(summary$strata == fac[k])]
      survs_ub <- summary$upper[which(summary$strata == fac[k])]
      if (j == 1) {
        graphics::plot(times, survs,
                       type = "l", xlab = "Time", ylab = "Survival function",
                       col = k, lwd = 3, ylim = c(0, 1)
        )
      } else {
        graphics::lines(times, survs,
                        type = "l", xlab = "Time", ylab = "Survival function",
                        col = k, lwd = 3, ylim = c(0, 1)
        )
      }
      graphics::lines(times, survs_lb, type = "l", col = k, lwd = 1, lty = 2)
      graphics::lines(times, survs_ub, type = "l", col = k, lwd = 1, lty = 2)
      this <- data.frame(time = times, survfun = survs, upper_ci = survs_lb,
                         lower_ci = survs_ub)
      colnames(this) <- c(
        paste("times for", fac[k]), paste("survival for", fac[k]),
        paste("upper ci for", fac[k]), paste("lower ci for", fac[k])
      )
      survival_fn <- append(survival_fn, this)
      legend_here <- append(legend_here, c(paste(fac[k], "Mean"), paste(fac[k],
                                                                        "CIs")))
      this_col <- c(k, k)
      colours <- append(colours, this_col)
      line_types <- rep(c(1, 2), no_levels)
    }
    graphics::legend("topright",
                     legend = legend_here, bty = "n", col = unlist(colours),
                     lty = line_types, cex = 0.5
    )
  }
  grDevices::dev.off()
  return(survival_fn)
}

#######################################################################
#' Plotting and return the residuals after cox proportional hazard model
#' @param param_to_be_estimated  parameter to be estimated
#' @param indep_var independent variable
#' @param covariates  covariates
#' @param fit  fit object from coxph method
#' @param dataset data used for cox ph model
#' @return plot and the residuals
#' @examples
#' \donttest{
#'   data_for_survival <- survival::lung
#'   surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
#'   covariates = c("ph.ecog"), "time")
#'   plot_return_residual_cox("status", "sex", covariates = c("ph.ecog"),
#'   surv_estimated$fit,data_for_survival )
#'   }
#' @export
#' @importFrom stats residuals
plot_return_residual_cox <- function(param_to_be_estimated, indep_var,
                                     covariates, fit, dataset) {
  if (!("coxph" %in% class(fit)))
    stop("Error- Fit object should be of type  coxph")

  check_list <- list(param_to_be_estimated, indep_var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Error - some of the required parameters are NULL or NA")

  name_file_plot <- paste0("Cox_residuals_", param_to_be_estimated, "_",
                           indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)

  residuals_martingale <- stats::residuals(fit, type = "martingale")
  cox_snell <- residuals_martingale - dataset[[param_to_be_estimated]]
  residuals_deviance <- stats::residuals(fit, type = "deviance")
  residuals_score <- stats::residuals(fit, type = "score")
  residuals_schoenfeld <- stats::residuals(fit, type = "schoenfeld")
  residuals_dfbeta <- stats::residuals(fit, type = "dfbeta")
  residuals_dfbetas <- stats::residuals(fit, type = "dfbetas")
  residuals_scaledsch <- stats::residuals(fit, type = "scaledsch")
  residuals_partial <- stats::residuals(fit, type = "partial")
  if (is.na(covariates)) {
    oldpar <- graphics::par(no.readonly = TRUE)
    graphics::par(mar = c(4, 4, 2, 2))

    plot(residuals_martingale, type = "p", main = "Martingale",
         ylab = "Residuals", lwd = 2)
    plot(residuals_deviance, type = "p", main = "Deviance",
         ylab = "Residuals", lwd = 2)
    plot(residuals_score, type = "p",
         main = paste("Score_", indep_var, sep = ""),
         ylab = "Residuals", lwd = 2)
    plot(residuals_schoenfeld, type = "p", main =
           paste("Schoenfeld_", indep_var, sep = ""),
         ylab = "Residuals", lwd = 2)
    plot(residuals_dfbeta, type = "p", main =
           paste("Dfbeta_", indep_var, sep = ""),
         ylab = "Residuals", lwd = 2)
    plot(residuals_dfbetas, type = "p", main =
           paste("Dfbetas_", indep_var, sep = ""),
         ylab = "Residuals", lwd = 2)
    plot(residuals_scaledsch, type = "p", main =
           paste("Scaledsch_", indep_var, sep = ""),
         ylab = "Residuals", lwd = 2)
    plot(residuals_partial, type = "p", main =
           paste("Partial_", indep_var, sep = ""),
         ylab = "Residuals", lwd = 2)
    on.exit(graphics::par(oldpar))
    grDevices::dev.off()
  }else{
    total <- 1 + length(covariates)
    oldpar <- graphics::par(no.readonly = TRUE)
    graphics::par(mar = c(4, 4, 2, 2))
    nos <- ceiling(total / 2)
    graphics::par(mfrow = c(2, nos))
    plot(residuals_martingale, type = "p", main = "Martingale",
         ylab = "Residuals", lwd = 2)
    plot(residuals_deviance, type = "p", main = "Deviance",
         ylab = "Residuals", lwd = 2)
    for (i in 1:total) {
      if (i == 1) name <- indep_var else  name  <-  covariates[i - 1]
      plot(residuals_score[, i], type = "p", main =
             paste("Score_", name, sep = ""), ylab = "Residuals", lwd = 2)
    }
    for (i in 1:total) {
      if (i == 1) name <- indep_var else  name <-  covariates[i - 1]
      plot(residuals_schoenfeld[, i], type = "p", main = paste("Schoenfeld_",
                              name, sep = ""), ylab = "Residuals", lwd = 2)
    }
    for (i in 1:total) {
      if (i == 1) name <- indep_var else  name <- covariates[i - 1]
      plot(residuals_dfbeta[, i], type = "p", main = paste("Dfbeta_", name,
                            sep = ""), ylab = "Residuals", lwd = 2)
    }

    for (i in 1:total) {
      if (i == 1) name <- indep_var else  name <-  covariates[i - 1]
      plot(residuals_dfbetas[, i], type = "p", main = paste("Dfbetas_", name,
                           sep = ""), ylab = "Residuals", lwd = 2)
    }
    for (i in 1:total) {
      if (i == 1) name <- indep_var else  name <- covariates[i - 1]
      plot(residuals_scaledsch[, i], type = "p",
           main = paste("Scaledsch_", name,
                            sep = ""), ylab = "Residuals", lwd = 2)
    }

    for (i in 1:total) {
      if (i == 1) name <- indep_var else  name <-  covariates[i - 1]
      plot(residuals_partial[, i], type = "p",
           main = paste("Partial_", name,
                               sep = ""), ylab = "Residuals", lwd = 2)
    }
    on.exit(graphics::par(oldpar))
    grDevices::dev.off()
 }
  results_residuals <- structure(list(
    Coxsnell_residual = cox_snell,
    Martingale_residual = residuals_martingale,
    Deviance_residual = residuals_deviance,
    Score_residual = residuals_score,
    Schoenfeld_residual = residuals_schoenfeld,
    Dfbeta_residual = residuals_dfbeta,
    Dfbetas_residual = residuals_dfbetas,
    Scaledsch_residual = residuals_scaledsch,
    Partial_residual = residuals_partial
  ))
}
#######################################################################
#' Predict risk/hazard function for  cox ph regression
#' @param coxfit  cox regression fit result
#' @param dataset param describing the methods
#' @param param_to_be_estimated  parameter to be estimated
#' @param covariates  covariates
#' @param indep_var independent variable
#' @param timevar_survival time variable
#' @return plot and the survival function values
#' @examples
#' \donttest{
#' data_for_survival <- survival::lung
#' surv_estimated <- use_coxph_survival("status", data_for_survival,
#' "sex",covariates = c("ph.ecog"), "time")
#' predict_coxph(surv_estimated$fit,data_for_survival, "status","sex",
#' covariates = c("ph.ecog"), "time")
#' }
#' @export
#' @details
#' "risk" option for "type" returns the hazard ratio relative to mean
#' e.g given below For lung data with
#' data_for_survival <- survival::lung
#' fit <- use_coxph_survival("status", data_for_survival, "sex",
#' covariates = c("ph.ecog"), "time")
#' coeffit = fit$coefficients
#' r1234 <- exp(coeffit("sex")lung$sex+ coeffit("ph.ecog")lung$ph.ecog)
#' rMean <- exp(sum(coef(fit) * fit$means, na.rm=TRUE))
#' rr <- r1234/rMean
predict_coxph <- function(coxfit, dataset, param_to_be_estimated,
                          covariates, indep_var, timevar_survival) {

  if (!("coxph" %in% class(coxfit)))
    stop("Error- Fit object should be of type  coxph")
  # dataset need not be null
  if (is.null(dataset))
    stop("Error - dataset can not be null")
  check_list <- list(param_to_be_estimated, indep_var, timevar_survival)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Error - some of the required parameters are NULL or NA")

  baseline_hazard <- survival::basehaz(coxfit)
  name_file_plot <- paste0("Cox_Cumhazard_", param_to_be_estimated,
                           "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  graphics::plot(baseline_hazard[, 2], baseline_hazard[, 1], main =
                   "Cumulative hazard", xlab = "Time", ylab = "H0(t)")
  grDevices::dev.off()

  risk_relative_mean <- stats::predict(coxfit, type = "risk")
  times_used <- dataset[[timevar_survival]]
  times <- times_used
  relative_risk_mean <- risk_relative_mean

  predict_results <- structure(list(
    cumulative_hazard = baseline_hazard,
    times = times,
    relative_risk_to_mean = relative_risk_mean
  ))
  return(predict_results)
}
#######################################################################
#' Plotting survival function for all covariates  calculated from
#' cox regression results and returned coefficients
#' @param coxfit  cox regression fit result
#' @param dataset param describing the methods
#' @param param_to_be_estimated  parameter to be estimated
#' @param covariates  covariates
#' @param indep_var independent variable
#' @return plot and the survival function values
#' @examples
#' \donttest{
#'  data_for_survival <- survival::lung
#'  surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
#'  covariates = c("ph.ecog"), "time")
#'  plot_survival_cox_covariates(surv_estimated$fit,data_for_survival,
#'  "status", covariates = c("ph.ecog"), "sex")
#'  }
#' @export
plot_survival_cox_covariates <- function(coxfit, dataset,
                                         param_to_be_estimated,
                                         covariates, indep_var) {
  if (!("coxph" %in% class(coxfit)))
    stop("Error- Fit object should be of type  coxph")
  # dataset need not be null
  if (is.null(dataset))
    stop("Error - dataset can not be null")

  check_list <- list(param_to_be_estimated, indep_var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Error - some of the required parameters are NULL or NA")

  baseline_haz <- survival::basehaz(coxfit)
  name_file_plot <- paste0("Cox_Survivial function_", param_to_be_estimated,
                           "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  if (sum(is.na(covariates)) == 0) {
    total <- length(covariates) + 1
  } else {
    total <- 1
  }
  for (j in 1:total) {
    if (total == 1) {
      var <- names(coxfit$coefficients)[1]
      var2 <- indep_var
      fixed <- NA
    } else {
      if (j == 1) {
        var <- names(coxfit$coefficients)[1]
        var2 <- indep_var
        fixed <- covariates
      } else {
        var <- covariates[j - 1]
        var2 <- var
        fixed <- c(names(coxfit$coefficients)[1], covariates[- (j - 1)])
      }
    }
    result <- suppressWarnings(as.numeric(levels(factor(dataset[[var2]]))))
    if (sum(is.na(result)) == 0) {
      lvls <- result
    } else {
      lvls <- unique(as.numeric(as.factor(dataset[[var2]])))
    }
    terms <- 0
    lbterms <- 0
    ubterms <- 0
    if (sum(is.na(fixed) == 0)) {
      for (m in seq_len(length(fixed))) {
        this_fixed <- fixed[m]
        value <- mean(dataset[[this_fixed]], na.rm = TRUE)
        this_coeff <- coxfit$coef[[this_fixed]]
        this_term <- this_coeff * value
        terms <- terms + this_term
        se_coeff <- summary(coxfit)$coefficients[this_fixed, 3]
        LB <- this_coeff - stats::qnorm(1 - 0.95 / 2) * se_coeff
        UB <- this_coeff + stats::qnorm(1 - 0.95 / 2) * se_coeff
        lbterms <- lbterms + LB * value
        ubterms <- ubterms + UB * value
      }
    }
    for (i in seq_len(length(lvls))) {
      this_coeff <- coxfit$coef[[var]]
      this_coeff_all <- terms + this_coeff * lvls[i]
      exp_coef <- exp(this_coeff_all)

      se_coeff <- summary(coxfit)$coefficients[var, 3]
      LB <- this_coeff - stats::qnorm(1 - 0.95 / 2) * se_coeff
      UB <- this_coeff + stats::qnorm(1 - 0.95 / 2) * se_coeff
      this_lb <- lbterms + LB * lvls[i]
      this_ub <- ubterms + UB * lvls[i]

      exp_coef_ci1 <- exp(this_lb)
      exp_coef_ci2 <- exp(this_ub)
      if (i == 1) {
        graphics::plot(baseline_haz[, 2], exp(-baseline_haz[, 1]) ^ (exp_coef),
                       col = i, main = paste("Survival curves", "-", var2),
                       xlab = "Time", ylab = "S(t|X)"
        )
      } else {
        graphics::plot(baseline_haz[, 2], exp(-baseline_haz[, 1]) ^ (exp_coef),
                       col = i, xaxt = "n", yaxt = "n", ann = FALSE)
      }
      graphics::lines(baseline_haz[, 2],
                      exp(-baseline_haz[, 1]) ^ (exp_coef_ci1),
                      col = i, lty = 2)
      graphics::lines(baseline_haz[, 2],
                      exp(-baseline_haz[, 1]) ^ (exp_coef_ci2),
                      col = i, lty = 2)
      graphics::par(new = TRUE)
    }
    graphics::legend("topright", legend = paste(var2, "-", lvls),
                     lty = rep(length(lvls), 1), col = seq_len(length(lvls)))
  }
  grDevices::dev.off()
}
