#' ############################################################################
#' Get the parameter values using the survival analysis
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param info_get_method additional information on methods e.g
#' Kaplan-Meier ot hazard
#' @param info_distribution distribution name  eg. for logistic
#' regression -binomial
#' @param covariates list of covariates - calculations to be done
#' before passing
#' @param timevar_survival time variable for survival analysis,
#' default is NA
#' @param cluster_var cluster variable for survival analysis
#' @return the results of the regression analysis
#' @examples
#' \donttest{
#' data_for_survival <- survival::aml
#' surv_estimated_aml <- use_survival_analysis("status", data_for_survival,
#'   "x",
#'   info_get_method = "parametric", info_distribution = "weibull",
#'   covariates = NA, "time")
#' }
#' @export
#' @details
#' This function helps to get the parameter values after the survival analysis
#' Takes into account many different methods like KM.FH, Cox proportional etc.
#' and then calls appropriate functions to do the survival analysis
use_survival_analysis <- function(param_to_be_estimated, dataset,
                                  indep_var, info_get_method,
                                  info_distribution,
                                  covariates, timevar_survival,
                                  cluster_var = NA) {
  # checking if parameters are NULL or NA
  check_list <- list(param_to_be_estimated, indep_var, info_get_method)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Error - some parameters are NULL or NA")

  if (is.null(dataset)) {
    stop("Need to provide a data set or file to lookup the data")
  }
  if (is.character(dataset)) {
    dataset <- load_trial_data(dataset)
  } else {
    dataset <- dataset
  }

  data_details <- c(param_to_be_estimated, indep_var, covariates,
                    timevar_survival)
  data_details <- data_details[!is.na(data_details)]
  check_cols_exist <- unlist(lapply(data_details,
                                    IPDFileCheck::check_column_exists, dataset))
  if (sum(check_cols_exist) != 0) {
    stop("Given column(s) can not be found !!!")
  }
  caps_info_method <- toupper(info_get_method)
  if (caps_info_method == "PARAMETRIC SURVIVAL" |
      caps_info_method == "PARAMETRIC") {
    results <- use_parametric_survival(
      param_to_be_estimated, dataset, indep_var,
      info_distribution, covariates, timevar_survival, cluster_var
    )
  }
  if (caps_info_method == "KAPLAN-MEIER" | caps_info_method == "KM") {
    results <- use_km_survival(
      param_to_be_estimated, dataset, indep_var, covariates,
      timevar_survival
    )
  }
  if (caps_info_method == "FLEMING-HARRINGTON" | caps_info_method == "FH") {
    results <- use_fh_survival(
      param_to_be_estimated, dataset, indep_var, covariates,
      timevar_survival
    )
  }
  if (caps_info_method == "FH2") {
    results <- use_fh2_survival(
      param_to_be_estimated, dataset, indep_var, covariates,
      timevar_survival
    )
  }
  if (caps_info_method %in% c(
    "COX-PROPORTIONAL-HAZARD", "COX PROPORTIONAL HAZARD", "COX-PH",
    "COX PH", "COXPH"
  )) {
    results <- use_coxph_survival(
      param_to_be_estimated, dataset, indep_var, covariates,
      timevar_survival
    )
  }
  return(results)
}
#' ###########################################################################
#' Get the parameter values using the survival analysis parametric survival
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param info_distribution distribution name  eg. for logistic
#' regression -binomial
#' @param covariates list of covariates
#' @param timevar_survival time variable for survival analysis,
#' default is NA
#' @param cluster_var cluster variable for survival analysis
#' @return the results of the regression analysis
#' @examples
#' \donttest{
#' data_for_survival <- survival::lung
#' surv_estimated <- use_parametric_survival("status",
#' data_for_survival, "sex", info_distribution = "weibull",
#' covariates = c("ph.ecog"), "time")
#' }
#' @export
#' @importFrom SurvRegCensCov ConvertWeibull
#' @details
#' This function is the last in the layer of function for parametric
#' survival analysis. This then returns the parameters of interest, plots
#' the results etc if the distribution is weibull it uses the package
#' SurvRegCensCov for easy interpretation of results
#' Returns the fit result, summary of regression, variance-covariance
#' matrix of coeff, cholesky decomposition, the parameters that define the
#' assumed distribution and the plot of model prediction
#' Using survfit from survival package to plot the survival curve
#' R's weibull distribution is defined as std weibull in terms of a and b
#' as (a/b) (x/b)^ (a-1) exp((-x/b)^a) where a is the shape and b is the scale
#' In HE the weibull distribution is parameterised as bit different
#' it is like gamma.lambda. t^(gamma-1) .exp(-lambda*t^gamma) where
#' gamma is the shape and lambda is the scale. The relationship is as below.
#' HE_shape = rweibull_shape
#' HE_scale = rweibull_scale ^(-rweibull_shape)
#' The survreg shape and scale are again bit different and they are
#' rweibull's shape and scale as below.
#' rweibull_shape = 1/fit$scale
#' rweibull_scale = exp(fit intercept)= exp(fit$coefficients)
#' remember to use 1st of coefficients
#' This has been utilised in SurvRegCensCov::ConvertWeibull
#' predict() for survreg object with type =quantile will provide
#' the failure times as survival function is 1-CDF of failure time.
use_parametric_survival <- function(param_to_be_estimated, dataset,
                                    indep_var, info_distribution, covariates,
                                    timevar_survival, cluster_var = NA) {

  # checking if parameters are NULL or NA
  check_list <- list(param_to_be_estimated, indep_var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Error - some parameters are NULL or NA")

  if (is.null(dataset)) {
    stop("Need to provide a data set or file to lookup the data")
  }
  if (is.character(dataset)) {
    dataset <- load_trial_data(dataset)
  } else {
    dataset <- dataset
  }

  covariates_list <- make_string_covariates(covariates)

  if (is.na(timevar_survival)) {
    stop("For survival analysis, please provide the variable to use as time ")
  } else {
    surv_object <- paste("survival::Surv(", timevar_survival, ",",
                         param_to_be_estimated, ")", sep = "")
  }
  if (is.na(info_distribution)) {
    stop("Error - information on distribution is missing")
  } else {
    this_dist <- find_survreg_distribution(info_distribution)
  }
  if (sum(is.na(covariates_list)) != 0) {
    if (is.na(cluster_var)) {

    expression_recreated <- paste0("survival::survreg", "(", surv_object, " ~ ",
                                   indep_var, ", ",
                                   "data = dataset,  dist = \"",
                                   this_dist, "\" ) ",
                                   sep = "")
    } else {
      check <- IPDFileCheck::check_column_exists(cluster_var, dataset)
      if (check == 0) {
        expression_recreated <- paste0("survival::survreg", "(", surv_object, " ~ ", indep_var, "+ cluster(", cluster_var, ")", ", ", "data = dataset,  dist = \"", this_dist, "\" ) ", sep = "")

      } else {
        stop("no variable found in the dataset")
      }

    }
  } else {
    if (is.na(cluster_var)) {
      expression_recreated <- paste0("survival::survreg", "(", surv_object, " ~ ", covariates_list, " + ", indep_var, ", ", "data = dataset,  dist = \"", this_dist, "\" ) ", sep = "")

    } else {
      check <- IPDFileCheck::check_column_exists(cluster_var, dataset)
      if (check == 0) {
        expression_recreated <- paste0("survival::survreg", "(", surv_object, " ~ ", covariates_list, " + ", indep_var, "+ cluster(", cluster_var, ")", ", ", "data = dataset,  dist = \"", this_dist, "\" ) ", sep = "")
      } else {
        stop("no variable found in the dataset")

      }
    }
  }
  fit <- eval(parse(text = expression_recreated))
  summary <- summary(fit)

  model_coeff <- as.data.frame(summary$table[, 1])
  se_coeff <- summary$table[, 2]
  model_coeff <- cbind(model_coeff, se_coeff)
  LB <- model_coeff - stats::qnorm(1 - 0.95 / 2) * se_coeff
  UB <- model_coeff + stats::qnorm(1 - 0.95 / 2) * se_coeff
  ci_coeff <- as.data.frame(list(LB = LB, UB = UB))


  if (toupper(this_dist) == "WEIBULL") {
    distribution_parameters <- SurvRegCensCov::ConvertWeibull(fit,
                                                        conf.level = 0.95)
    model_coeff <- distribution_parameters$vars
    LB <- model_coeff[, 1] - stats::qnorm(1 - 0.95 / 2) * model_coeff[, 2]
    UB <- model_coeff[, 1] + stats::qnorm(1 - 0.95 / 2) * model_coeff[, 2]
    ci_coeff <- list(LB = LB, UB = UB)
    hazard_ratios <- distribution_parameters$HR[, 1]
    ci_HR <- distribution_parameters$HR[, -1]
    event_time_ratio <- distribution_parameters$ETR
  }
  colnames(model_coeff) <- c("model coefficient", "SE")
  variance_covariance_coeff <- stats::vcov(fit)
  chol_decomp_matrix <- chol(variance_covariance_coeff)


  AIC <- stats::extractAIC(fit)[2]
  LLf <- fit$loglik[2]
  LL0 <- fit$loglik[1]
  N <- nrow(dataset)
  McFadden_r2 <- as.vector(1 - (LLf / LL0))
  CoxSnaell_r2 <- as.vector(1 - exp((2 / N) * (LL0 - LLf)))
  Nagelkerke_r2 <- as.vector((1 - exp((2 / N) * (LL0 - LLf))) /
                               (1 - exp(LL0) ^ (2 / N)))

  residuals_result <- plot_return_residual_survival(param_to_be_estimated,
                                                    indep_var, covariates, fit)

  model_fit_assessment <- structure(list(
    AIC = AIC,
    McFadden_R2 = McFadden_r2,
    CoxSnaell_R2 = CoxSnaell_r2,
    Nagelkerke_R2 = Nagelkerke_r2,
    residuals_result = residuals_result
  ))
  prediction_using_param_estimated <- stats::predict(fit)

  plot_prediction <- plot_prediction_parametric_survival(param_to_be_estimated,
                indep_var, covariates, dataset, fit, timevar_survival)

  results <- structure(list(
    fit = fit,
    summary = summary,
    model_coeff = model_coeff,
    ci_coeff = ci_coeff,
    hazard_ratios = hazard_ratios,
    ci_hazard_ratios = ci_HR,
    event_time_ratio = event_time_ratio,
    variance_covariance_coeff = variance_covariance_coeff,
    cholesky_decomp_matrix = chol_decomp_matrix,
    residuals = residuals_result,
    model_fit_assessment = model_fit_assessment,
    # model_diagnostics = model_diagnostics,
    prediction_results = prediction_using_param_estimated
  ))
  return(results)
}

#' ###########################################################################
#' Get the parameter values using the Kaplan-Meier survival analysis
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates
#' @param timevar_survival time variable for survival analysis, default is NA
#' @return the results of the regression analysis, fit results, summary and plot
#' @examples
#' \donttest{
#' data_for_survival <- survival::aml
#' surv_estimated <- use_km_survival("status", data_for_survival, "x",
#'   covariates = NA, "time")
#'   }
#' \donttest{
#' data_for_survival <- survival::lung
#' surv_estimated <- use_km_survival("status", data_for_survival, "sex",
#'   covariates = c("ph.ecog"), "time")
#' }
#' @export
#' @details
#' This function is for survival analysis using Kaplan Meier.
#' This plots the cumulative survival function for each combination of covariate
#' If the covariate is numeric, R takes it as different levels.
#' The plot uses the returned list of survfit and extracts the time
#' and the strata from summary of the fit (implemented in
#' plot_return_survival_curve function)
use_km_survival <- function(param_to_be_estimated, dataset,
                            indep_var, covariates, timevar_survival) {
  # checking if parameters are NULL or NA
  check_list <- list(param_to_be_estimated, indep_var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Error - some parameters are NULL or NA")

  if (is.null(dataset)) {
    stop("Need to provide a data set or file to lookup the data")
  }
  if (is.character(dataset)) {
    dataset <- load_trial_data(dataset)
  } else {
    dataset <- dataset
  }

  covariates_list <- make_string_covariates(covariates)

  if (is.na(timevar_survival)) {
    stop("For survival analysis, please provide the variable to use as time ")
  } else {
    surv_object <- paste("survival::Surv(", timevar_survival, ",",
                         param_to_be_estimated, ")", sep = ""
    )
  }
  # create the expression  for km fit
  if (sum(is.na(covariates_list)) != 0) {
    expression_recreated <- paste0("survival::survfit", "(",
                                   surv_object, " ~ ", indep_var, ",type =",
                                   "\"kaplan-meier\"", ", ", "data = dataset) ",
                                   sep = ""
    )
  } else {
    expression_recreated <- paste0("survival::survfit", "(", surv_object,
                                   " ~ ", covariates_list, " + ",
      indep_var, ", type =", "\"kaplan-meier\"", ", ",
      "data = dataset) ", sep = ""
    )
  }
  # fit result
  fit <- eval(parse(text = expression_recreated))
  # summary of the fir
  summary <- summary(fit)
  # Plot survival curve
  survival_func_covariates_plot <- plot_return_survival_curve(
    param_to_be_estimated, dataset, indep_var,
    covariates, timevar_survival
  )
  # results
  results <- structure(list(
    fit = fit,
    summary = summary,
    survival_func_covariates_plot = survival_func_covariates_plot
  ))
  return(results)
}
#' ###########################################################################
#' Get the parameter values using the survival analysis method FH
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates
#' @param timevar_survival time variable for survival analysis, default is NA
#' @return the results of the regression analysis
#' @examples
#' \donttest{
#' data_for_survival <- survival::aml
#' surv_estimated <- use_fh_survival("status", data_for_survival, "x",
#'   covariates = NA, "time"
#' )
#' }
#' @details
#' This function is for survival analysis using FH.
#' This plots the cumulative survival function for each combination of covariate
#' If the covariate is numeric, R takes it as different levels.
#' The plot uses the returned list of survfit and extracts the time and the
#' strata from summary of the fit
#' (implemented in plot_return_survival_curve function)
#' @export
use_fh_survival <- function(param_to_be_estimated, dataset,
                            indep_var, covariates, timevar_survival) {
  # checking if parameters are NULL or NA
  check_list <- list(param_to_be_estimated, indep_var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Error - some parameters are NULL or NA")

  if (is.null(dataset)) {
    stop("Need to provide a data set or file to lookup the data")
  }
  if (is.character(dataset)) {
    dataset <- load_trial_data(dataset)
  } else {
    dataset <- dataset
  }

  covariates_list <- make_string_covariates(covariates)

  if (is.na(timevar_survival)) {
    stop("For survival analysis, please provide the variable to use as time ")
  } else {
    surv_object <- paste("survival::Surv(", timevar_survival, ",",
                         param_to_be_estimated, ")", sep = ""
    )
  }
  # generate the expression
  if (sum(is.na(covariates_list)) != 0) {
    expression_recreated <- paste0("survival::survfit", "(", surv_object, " ~ ",
    indep_var, ", type =", "\"fleming-harrington\"", ", ", "data = dataset) ",
      sep = ""
    )
  } else {
    expression_recreated <- paste0("survival::survfit", "(", surv_object, " ~ ",
                                   covariates_list, " + ", indep_var,
                                   ", type =", "\"fleming-harrington\"", ", ",
      "data = dataset) ", sep = ""
    )
  }
  # fit result
  fit <- eval(parse(text = expression_recreated))
  # summary of fit result
  summary <- summary(fit)
  # plot the survival curve
  survival_func_covariates_plot <- plot_return_survival_curve(
    param_to_be_estimated, dataset, indep_var,
    covariates, timevar_survival
  )
  # result
  results <- structure(list(
    fit = fit,
    summary = summary,
    survival_func_covariates_plot = survival_func_covariates_plot
  ))
  return(results)
}
#' ############################################################################
#' Get the parameter values using the survival analysis using FH2 method
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates
#' @param timevar_survival time variable for survival analysis, default is NA
#' false by default
#' @return the results of the regression analysis
#' @examples
#' \donttest{
#' data_for_survival <- survival::aml
#' surv_estimated <- use_fh2_survival("status", data_for_survival, "x",
#'   covariates = NA, "time")
#' }
#' @export
#' @details
#' This function is for survival analysis using FH2.
#' This plots the cumulative survival function for each combination of covariate
#' If the covariate is numeric, R takes it as different levels.
#' The plot uses the returned list of survfit and extracts the time and
#' the strata from summary of the fit
#' (implemented in plot_return_survival_curve function)
use_fh2_survival <- function(param_to_be_estimated, dataset,
                             indep_var, covariates, timevar_survival) {
  # checking if parameters are NULL or NA
  check_list <- list(param_to_be_estimated, indep_var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Error - some parameters are NULL or NA")

  if (is.null(dataset)) {
    stop("Need to provide a data set or file to lookup the data")
  }
  if (is.character(dataset)) {
    dataset <- load_trial_data(dataset)
  } else {
    dataset <- dataset
  }
  covariates_list <- make_string_covariates(covariates)
  if (is.na(timevar_survival)) {
    stop("For survival analysis, please provide the variable to use as time ")
  } else {
    surv_object <- paste("survival::Surv(", timevar_survival, ",",
                         param_to_be_estimated, ")",
      sep = ""
    )
  }
  # generate the expression
  if (sum(is.na(covariates_list)) != 0) {
    expression_recreated <- paste0("survival::survfit", "(", surv_object,
                                   " ~ ", indep_var, ", type =", "\"fh2\"",
                                   ", ", "data = dataset) ", sep = "")
  } else {
    expression_recreated <- paste0("survival::survfit", "(",
                                   surv_object, " ~ ", covariates_list,
      " + ", indep_var, ", type =", "\"fh2\"", ", ", "data = dataset) ",
      sep = ""
    )
  }
  # fit result
  fit <- eval(parse(text = expression_recreated))
  # summary of fit result
  summary <- summary(fit)
  # plot the survival curve
  survival_func_covariates_plot <- plot_return_survival_curve(
    param_to_be_estimated, dataset, indep_var,
    covariates, timevar_survival
  )
  # results
  results <- structure(list(
    fit = fit,
    summary = summary,
    survival_func_covariates_plot = survival_func_covariates_plot
  ))
  return(results)
}
#' ###########################################################################
#' Get the parameter values using the survival analysis using cox
#' proportional hazard
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates - calculations to be done before passing
#' @param timevar_survival time variable for survival analysis, default is NA
#' false by default
#' @return the results of the regression analysis
#' @examples
#' \donttest{
#' data_for_survival <- survival::aml
#' surv_estimated <- use_coxph_survival("status", data_for_survival, "x",
#'   covariates = NA, "time")
#' }
#' \donttest{
#' data_for_survival <- survival::lung
#' surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
#'   covariates = c("ph.ecog"), "time")
#' }
#' @export
#' @importFrom survminer ggcoxzph
#' @details
#' plots baseline cumulative hazard function, survival function for
#' each covariate
#' while keeping the other fixed at the mean value
#' (using plot_survival_cox_covariates),
#' survival function for each combination of covariate using survfit (using
#' plot_return_survival_curve) and test for cox regression results
#' It also returns  risk relative to mean (predicted at mean value
#' of each covariate) along with the fit results coefficients,
#' SE of coefficients, summary, and analysis of deviance
use_coxph_survival <- function(param_to_be_estimated, dataset, indep_var,
                               covariates, timevar_survival) {
  # checking if parameters are NULL or NA
  check_list <- list(param_to_be_estimated, indep_var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Error - some parameters are NULL or NA")

  if (is.null(dataset)) {
    stop("Need to provide a data set or file to lookup the data")
  }
  if (is.character(dataset)) {
    dataset <- load_trial_data(dataset)
  } else {
    dataset <- dataset
  }

  covariates_list <- make_string_covariates(covariates)

  if (is.na(timevar_survival)) {
    stop("For survival analysis, please provide the variable to use as time ")
  } else {
    surv_object <- paste("survival::Surv(", timevar_survival, ",",
                         param_to_be_estimated, ")", sep = ""
    )
  }
  if (sum(is.na(covariates_list)) != 0) {
    expression_recreated <- paste0("survival::coxph", "(", surv_object, " ~ ",
                                   indep_var, ", ", "data = dataset) ",
                                   sep = ""
    )
    expression_recreated_forsurfit <- paste0("survival::survfit", "(",
                                             surv_object, " ~ ",
                                             indep_var, ", ", "data = dataset)",
                                             sep = ""
    )
  } else {
    expression_recreated <- paste0("survival::coxph", "(", surv_object, " ~ ",
                                   covariates_list, " + ", indep_var, ", ",
                                   "data = dataset) ", sep = ""
    )
    expression_recreated_forsurfit <- paste0("survival::survfit", "(",
                                             surv_object, " ~ ",
                                             covariates_list, " + ",
                                             indep_var, ", ", "data = dataset)",
                                             sep = ""
    )
  }
  # fit result
  fit <- eval(parse(text = expression_recreated))
  # summary of fit
  summary <- summary(fit)
  #  exp(coef) gives the multiplicative effect on hazard
  # model coefficients
  model_coeff <- as.data.frame(summary$coefficients[, 1])
  colnames(model_coeff) <- "model coefficient"
  # se on model cofficients
  se_coeff <- summary$coefficients[, 3]
  LB <- model_coeff - stats::qnorm(1 - 0.95 / 2) * se_coeff
  UB <- model_coeff + stats::qnorm(1 - 0.95 / 2) * se_coeff
  # confidence level on model coefficients
  ci_coeff <- as.data.frame(list(LB = LB, UB = UB))
  colnames(ci_coeff) <- c("LB", "UB")
  # Hazard ratio and their confidence intervals
  hazard_ratios <- as.data.frame(summary$conf.int[, 1])
  colnames(hazard_ratios) <- "hazard ratios"

  LB <- summary$conf.int[, 3]
  UB <- summary$conf.int[, 4]
  ci_HR <- as.data.frame(list(LB = LB, UB = UB))
  colnames(ci_HR) <- c("LB", "UB")
  # akiake informaiton criteria, log likelihood and differenr R2 values
  AIC <- stats::extractAIC(fit)[2]
  LLf <- fit$loglik[2]
  LL0 <- fit$loglik[1]
  N <- nrow(dataset)
  McFadden_r2 <- as.vector(1 - (LLf / LL0))
  CoxSnaell_r2 <- as.vector(1 - exp((2 / N) * (LL0 - LLf)))
  Nagelkerke_r2 <- as.vector((1 - exp((2 / N) * (LL0 - LLf))) /
                               (1 - exp(LL0) ^ (2 / N)))
  # wald test for model diagnostics
  wald_test <- as.data.frame(cbind(summary$coefficients[, 4],
                                   summary$coefficients[, 5]))
  colnames(wald_test) <- c("wald.test", "p.value")
  # analysis of deviance
  analysis_deviance <- car::Anova(fit, test.statistic = "LR")
  # plot of residuals to check themodel fit
  residuals_result <- plot_return_residual_cox(param_to_be_estimated,
                                        indep_var, covariates, fit, dataset)

  model_fit_assessment <- structure(list(
    AIC = AIC,
    McFadden_R2 = McFadden_r2,
    CoxSnaell_R2 = CoxSnaell_r2,
    Nagelkerke_R2 = Nagelkerke_r2,
    wald_test = wald_test,
    analysis_deviance = analysis_deviance,
    residuals_result = residuals_result
    ))
  # survival cox curves
  plot_survival_cox_covariates(fit, dataset, param_to_be_estimated,
                               covariates, indep_var)
  # model diagnostics and the plor
  model_diagnostics <- survival::cox.zph(fit)
  name_file_plot <- paste0("Coxph_survival_diganostic_",
                           param_to_be_estimated, "_", indep_var,
    ".pdf",
    sep = ""
  )
  grDevices::pdf(name_file_plot)
  plot_prediction <- survminer::ggcoxzph(model_diagnostics)
  grDevices::dev.off()
  # prediction after cox fit
  prediction_coxph <- predict_coxph(fit, dataset, param_to_be_estimated,
                                    covariates, indep_var, timevar_survival)
  # results
  results <- structure(list(
    fit = fit,
    summary = summary,
    model_coeff = model_coeff,
    ci_coeff = ci_coeff,
    hazard_ratios = hazard_ratios,
    ci_hazard_ratios = ci_HR,
    residuals = residuals_result,
    model_fit_assessment = model_fit_assessment,
    model_diagnostics = model_diagnostics,
    prediction_results = prediction_coxph,
    plot_prediction = plot_prediction
  ))
  return(results)
}
