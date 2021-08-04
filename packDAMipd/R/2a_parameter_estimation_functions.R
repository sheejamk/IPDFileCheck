
#######################################################################
#' Get the parameter values from reading a file
#' @param parameter  parameter of interest
#' @param paramvalue parameter value to be assigned
#' @return the paramvalue
#' @examples
#' a <- get_parameter_direct("cost_IT", paramvalue = 100)
#' @export
#' @details
#' Basic function to assign a parameter directly
get_parameter_direct <- function(parameter, paramvalue) {
  if (!is.null(paramvalue)) {
    assigned_value <- assign(parameter, paramvalue)
    return(assigned_value)
  } else {
    stop("Need to provide a parameter value")
  }
}
#######################################################################
#' Get the parameter values from reading a file
#' @param parameter  parameter of interest
#' @param paramfile parameter file to be provided
#' @param strategycol treatment strategy
#' @param strategyname treatment strategy name in the column strategycol
#' @return the paramvalue
#' @examples
#' a <- get_parameter_read("cost_IT", paramfile = system.file("extdata",
#' "table_param.csv", package = "packDAMipd"))
#' @export
#' @details
#' This function read the parameter from a file given that
#' the file has these column names (at least) Parameter and Value
#' Strategy col and name are optional. Check if the data file contains
#' column names parameter and value and then get the results.
get_parameter_read <- function(parameter, paramfile, strategycol = NA,
                               strategyname = NA) {
  # check if the paramfile is null
  if (is.null(paramfile)) {
    stop("Need to provide a parameter file to lookup")
  }
  # file exists and readable?
  if (IPDFileCheck::test_file_exist_read(paramfile) != 0) {
    stop("File doesnt exist or not able to access")
  }
  dataset <- data.frame(read.csv(paramfile, header = TRUE, sep = ",",
                                 stringsAsFactors = FALSE))
  # is a column named value exists?
  result <- IPDFileCheck::check_column_exists("value", dataset)
  if (result != 0) {
    stop(paste("Parameter file should contain the term value in column name",
               sep = ""))
  }
  value_colno <- IPDFileCheck::get_columnno_fornames(dataset, "value")
  # is a column named parameter exists?
  result <- IPDFileCheck::check_column_exists("parameter", dataset)
  if (result != 0) {
    stop(paste("Parameter file should have the term parameter as column name",
               sep = ""))
  }
  param_colno <- IPDFileCheck::get_columnno_fornames(dataset, "parameter")
  # if you say strategycol is needed, then it should have that column
  if (!is.na(strategycol)) {
    if (IPDFileCheck::check_column_exists(strategycol, dataset) == 0) {
      dataset <- dataset[dataset[[strategycol]] == strategyname, ]
      answer <- dataset[dataset[[param_colno]] == parameter, ][[value_colno]]
    } else {
      stop(paste("Parameter file should contain strategy in column name",
                 sep = ""))
    }
  } else {
    # get the value of the parameter if everyhting is correct
    answer <- dataset[dataset[[param_colno]] == parameter, ][[value_colno]]
    if (is.na(answer)) {
      stop("Error- Parameter value is NA - Did you use the wrong function
              to get the parameter?")
    }
  }
  answer <- unlist(answer)
  return(answer)
}
#######################################################################
#' Get the mortality rate  values from reading a file
#' @param paramfile  parameter file to get the mortality eg.national life
#' table data
#' @param age age to get the age specific data
#' @param mortality_colname column name with the mortality rates if it is not
#' gender specific
#' @param gender gender details to get the gender specific mortality data
#' @return the paramvalue
#' @examples
#' paramfile <- system.file("extdata", "LifeTable_USA_Mx_2015.csv",
#'   package = "packDAMipd"
#' )
#' a <- get_mortality_from_file(paramfile, age = 10, mortality_colname =
#' "total", gender = NULL)
#' @export
#' @details
#' Provides the mortality rates as age and gender dependent
#' Assumes the data contains mortality rate for single year and once
#' it extracted
#' per gender will retrieve single value
#' Age column can consists of range of values, or a particular value
#' also assumes that the mortality rate for each gender is listed under
#' the gender column for gender specific values. if the mortality
#' is not gender specific, the column name should be passed on to the function
#' if gender is not null, mortality_name will be ignored
get_mortality_from_file <- function(paramfile, age, mortality_colname,
                                    gender = NULL) {
  # check for the parameter file
  if (is.null(paramfile)) {
    stop("Need to provide a parameter file to lookup")
  }
  # check for the file existence and readability
  if (IPDFileCheck::test_file_exist_read(paramfile) != 0) {
    stop("File doesnt exists or not able to access")
  }
  dataset <- data.frame(utils::read.csv(paramfile, header = TRUE,
                                        sep = ",", stringsAsFactors = FALSE))
  # check if the column age exists
  result <- IPDFileCheck::check_column_exists("age", dataset)
  if (result != 0) {
    stop("Expecting the life tables to contain an age column")
  }
  age_columnno <- IPDFileCheck::get_columnno_fornames(dataset, "age")
  # find the row corresponding to the specific age
  this_row <- which(dataset[age_columnno] == age)
  # if not found, find the row with age ranges that specific age will fall into
  if (length(this_row) == 0) {
    i <- 1
    while (i <= nrow(dataset)) {
      the_string <- dataset[[age_columnno]][i]
      # find the position of "-" assuming the age range given as 10-20
      pos <- stringr::str_locate(the_string, "-")
      if (sum(is.na(pos)) < 2) {
        minage <- as.numeric(substr(the_string, 1, pos[1] - 1))
        maxage <- as.numeric(substr(the_string, pos[1] + 1,
                                    nchar(the_string)))
      } else {
        # for some it could be 0 and over - then find that
        pos <- stringr::str_locate(the_string, "and over")
        if (sum(is.na(pos)) < 2) {
          minage <- as.numeric(substr(the_string, 1, pos[1] - 1))
          maxage <- 120
        }else{
          minage <- 0
          maxage <- 120
        }
      }
      if (minage <= age & maxage >= age) {
        # find the row containing the age
        this_row <- i
        i <- nrow(dataset) + 1
      } else {
        i <- i + 1
      }
    }
  }
  # if gender is null get the column containing the rate
  if (is.null(gender)) {
    if (is.null(mortality_colname)) {
      stop("column name for mortality rate should not be null")
    }
    result <- IPDFileCheck::check_column_exists(mortality_colname, dataset)
    if (result != 0) {
      stop("Life tables should contain given column name for mortality rate")
    }
    mortality_columnno <- IPDFileCheck::get_columnno_fornames(dataset,
                                                              mortality_colname)
    # find
    mortality <- dataset[[mortality_columnno]][this_row]
  } else {
    result <- IPDFileCheck::check_column_exists(gender, dataset)
    if (result != 0) {
      stop("Life tables should contain column with gender name for mortality")
    }
    # when the gender column is given the mortality rate is under that
    # gender column
    mortality_columnno <- IPDFileCheck::get_columnno_fornames(dataset, gender)
    mortality <- dataset[[mortality_columnno]][this_row]
  }
  return(mortality)
}
#######################################################################
#' Get the definition of given parameter distribution defined in a file
#' @param parameter  parameter of interest
#' @param paramfile data file to be provided
#' @param colnames_paramdistr list of column names for the parameters that
#' define the distribution
#' @param strategycol treatment strategy column name
#' @param strategyname treatment strategy name in the column strategycol
#' @return the definition of parameter from the given distribution
#' @examples
#' paramfile <- system.file("extdata", "table_param.csv",
#' package = "packDAMipd")
#' a <- get_parameter_def_distribution("rr", paramfile, c("Param1_name",
#' "Param1_value"))
#' @export
#' @details
#' This function reads the parameter distribution from a file and return the
#' parameter obtained
#' This assumes that the file contains parameter, distribution
#' colnames for parameter values for the distribution are passed on
#' to the function assumes the name of each parameter and value are given
#' in the consecutive columns. Once the expression is created using the
#' parameters given in the file, it gets checked for correctness of
#' specifying the distribution in R context using the function
#' check_estimate_substitute_proper_params and then evaluated.
get_parameter_def_distribution <- function(parameter, paramfile,
                                        colnames_paramdistr, strategycol = NA,
                                        strategyname = NA) {
  # check for paramfile given
  if (is.null(paramfile)) {
    stop("Need to provide a parameter file to lookup")
  }
  # check for file existence
  if (IPDFileCheck::test_file_exist_read(paramfile) != 0) {
    stop("File doesnt exists or not able to access")
  }
  dataset <- data.frame(read.csv(paramfile, header = TRUE, sep = ",",
                                 stringsAsFactors = FALSE))
  # if the strategy col is provided, it has to exist in the dataset
  if (!is.na(strategycol)) {
    if (is.null(strategyname))
      stop("Error - Need to provide the strategy name")
    if (IPDFileCheck::check_column_exists(strategycol, dataset) == 0) {
      dataset <- dataset[dataset[[strategycol]] == strategyname, ]
    }
  }
  # parameter column should exist
  result <- IPDFileCheck::check_column_exists("parameter", dataset)
  if (result != 0) {
    stop(paste("Parameter file should have the term parameter as column name",
               sep = ""))
  }
  param_colno <- IPDFileCheck::get_columnno_fornames(dataset, "parameter")

  # column with name distribution should exist
  result <- IPDFileCheck::check_column_exists("distribution", dataset)
  if (result != 0) {
    stop(paste("Parameter file should contain distribution in column name",
               sep = ""))
  }
  distr_colno <- IPDFileCheck::get_columnno_fornames(dataset, "distribution")
  required_row <- dataset[dataset[[param_colno]] == parameter, ]
  if (nrow(required_row) < 1) {
        stop("Parameter not found")
  } else {
        this_val <-
          dataset[dataset[[param_colno]] == parameter, ][[distr_colno]]
  }
  # if the value from the distribution col is not existing throw an error
  if (sum(is.na(this_val)) > 0)
    stop("The distribution can not be NA")
  if (identical(this_val, character(0)) |
      length(this_val) == 0 | this_val == "")
        stop("This parameter may not be estimated from a distribution")

  # the parameter name and value that define the distribution should exist
  if (is.null(colnames_paramdistr) | sum(is.na(colnames_paramdistr) != 0)) {
    stop("Column names of the parameters should not be null or NA")
  } else {
    result <- unlist(lapply(colnames_paramdistr,
                            IPDFileCheck::check_column_exists, dataset))
    if (sum(result) != 0) {
      stop("File should have parameters for the distribtution as column name")
    }
    this_param <- dataset[dataset[param_colno] == parameter, ][[param_colno]]
    this_distr_name <-
      dataset[dataset[param_colno] == parameter, ][[distr_colno]]
    expression_created <- paste(this_distr_name, "(", sep = "")
    i <- 1
    while (i <= length(colnames_paramdistr)) {
      # expect the column name for parameter name should have "name" in it
      result <- grep("name", colnames_paramdistr[i])
      if (length(result) != 0) {
        name_col <- IPDFileCheck::get_columnno_fornames(dataset,
                                                  colnames_paramdistr[i])
        param1 <-
         dataset[dataset[param_colno] == parameter, ][[name_col]]
        val_col <- IPDFileCheck::get_columnno_fornames(dataset,
                                                colnames_paramdistr[i + 1])
        param1_value <-
         dataset[dataset[param_colno] == parameter, ][[val_col]]
        if (!is.numeric(param1_value)) {
          stop("Parameter values should be numeric for evaluation")
        }
        # try creating the expression
        if (i >= length(colnames_paramdistr) / 2) {
          expression_created <- paste(expression_created, param1, " = ",
                                      param1_value, ")", sep = "")
        } else {
          expression_created <- paste(expression_created, param1, " = ",
                                      param1_value, ", ", sep = "")
        }
      } else {
        stop("Column name for parameter should contain the keyword \"name\" ")
      }
      i <- i + 2
    }
  }
  # substitute proper parameters that R can understand
  expression_recreated <-
    check_estimate_substitute_proper_params(expression_created)
  param_with_expression <-
    paste(this_param, " = ", expression_recreated, sep = "")
  # evaluate that expression and return the value obtained.
  param_obtained <- eval(parse(text = param_with_expression))
  return(param_obtained)
}
#######################################################################
#' Get the parameter values using the provided statistical regression methods
#' @param param_to_be_estimated  parameter of interest
#' @param data data to be provided or the data file containing dataset
#' @param method method of estimation (for example, linear,
#' logistic regression etc)
#' @param indep_var the independent variable (column name in data file)
#' @param info_get_method additional information on methods
#' e.g Kaplan-Meier ot hazard
#' @param info_distribution distribution name  eg.
#' for logistic regression -binomial
#' @param covariates list of covariates-calculations to be done before passing
#' @param timevar_survival time variable for survival analysis
#' @param interaction boolean value to indicate interaction in the case of
#' linear regression
#' @param fix_eff boolean value to indicate interaction in the case of
#' linear regression
#' @param fix_eff_interact_vars boolean value to indicate interaction in the
#' case of linear regression
#' @param random_intercept_vars boolean value to indicate interaction in the
#' case of linear regression
#' @param nested_intercept_vars_pairs boolean value to indicate interaction in
#' the case of linear regression
#' @param cross_intercept_vars_pairs boolean value to indicate interaction in
#' the case of linear regression
#' @param uncorrel_slope_intercept_pairs boolean value to indicate interaction
#' in the case of linear regression
#' @param random_slope_intercept_pairs boolean value to indicate interaction
#' in the case of linear regression
#' @param naaction what action to be taken for the missing values, default
#' is a missing value.
#' @param param2_to_be_estimated  parameter of interest for equation 2 in
#' bivariate regression
#' @param covariates2 list of covariates - for equation 2 in
#' bivariate regression
#' @param interaction2 boolean value to indicate interaction for equation
#' 2 in bivariate regression
#' @param link link function to be provided if not using the default link
#' for each of the info_distribution
#' @param cluster_var cluster variable if any
#' @param package_mixed_model package to be used for mixed model
#' ie nlme or lme4
#' @return results the results of the regression analysis
#' @examples
#'\donttest{
#' result <- get_parameter_estimated_regression(
#'   param_to_be_estimated = "Direction",
#'   data = ISLR::Smarket, method = "logistic", indep_var = "Lag1",
#'   info_get_method = NA, info_distribution = "binomial",
#'   covariates = c("Lag2", "Lag3"), interaction = FALSE,
#'   naaction = "na.omit", link = NA)
#'   }
#' @export
#' @details
#' This function is the top in the layer of functions used for regression
#' analysis Thus it contains many parameters to be passed on
#' The required ones are parameter to be estimated, data that contains
#' the observation, the method of regression to be used, the independent
#' variable and the information for the distribution and method.
#' if the data is given as a file name. it will load the data in that file
#' Then it calls the appropriate functions depending on the regression method
#' that specified. The methods that are considered : Survival analysis,
#' linear regression, logistic regression,generalised linear model,
#' linear multilevel or mixed model, and seemingly unrelated regression
get_parameter_estimated_regression <- function(param_to_be_estimated, data,
                                          method, indep_var,
                                          info_get_method = NA,
                                          info_distribution = NA,
                                          covariates = NA,
                                          timevar_survival = NA,
                                          interaction = FALSE,
                                          fix_eff = NA,
                                          fix_eff_interact_vars = NA,
                                          random_intercept_vars = NA,
                                          nested_intercept_vars_pairs = NA,
                                          cross_intercept_vars_pairs = NA,
                                          uncorrel_slope_intercept_pairs = NA,
                                          random_slope_intercept_pairs = NA,
                                          naaction = "stats::na.omit",
                                          param2_to_be_estimated = NA,
                                          covariates2 = NA,
                                          interaction2 = FALSE,
                                          link = NA, cluster_var = NA,
                                          package_mixed_model = NA) {
  # regular checks to see the required parameters ar given
  check_list <- c(param_to_be_estimated, method)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Some of parameters are null or NA")

  if (is.null(data)) {
    stop("Need to provide a data set or file to lookup the data")
  }
  # if the given data is a file name , load the data in that
  if (is.character(data)) {
    dataset <- load_trial_data(data)
  } else {
    dataset <- data
  }

  # check the required column exists in the dataset
  data_details <- c(param_to_be_estimated, indep_var, covariates,
                    timevar_survival, param2_to_be_estimated, covariates2)
  data_details <- data_details[!is.na(data_details)]
  check_cols_exist <- unlist(lapply(data_details,
                              IPDFileCheck::check_column_exists, dataset))
  if (sum(check_cols_exist) != 0) {
    stop("Given column(s) can not be found !!!")
  }
  # now  depending on the method, call the corresponding regression methods
  caps_method <- toupper(method)
  # linear regression
  if (caps_method == "LINEAR REGRESSION" | caps_method == "LINEAR_REGRESSION"
      | caps_method == "LINEAR") {
    results <- use_linear_regression(param_to_be_estimated, data, indep_var,
                                     covariates, interaction)
  }
  # logistic regression
  if (caps_method == "LOGISTIC REGRESSION" |
      caps_method == "LOGISTIC_REGRESSION"
      | caps_method == "LOGISTIC") {
    results <- use_generalised_linear_model(param_to_be_estimated, dataset,
                                            indep_var,
                                            family = info_distribution,
                                            covariates, interaction, naaction,
                                            link = "logit"
    )
  }
  # linear mixed effect model or multilevel model
  if (caps_method == "LINEAR MULTILEVEL MODELLING" |
      caps_method == "LINEAR_MULTILEVEL_MODELLING" |
      caps_method == "LINEAR MULTILEVEL" |
      caps_method == "LINEAR_MULTILEVEL" |
      caps_method == "LINEAR MIXED EFFECT" |
      caps_method == "LINEAR_MIXED_EFFECT" |
      caps_method == "LINEAR MIXED" |
      caps_method == "LINEAR_MIXED") {
    results <- use_linear_mixed_model(param_to_be_estimated,
                                      dataset,
                                      fix_eff, fix_eff_interact_vars,
                                      random_intercept_vars,
                                      nested_intercept_vars_pairs,
                                      cross_intercept_vars_pairs,
                                      uncorrel_slope_intercept_pairs,
                                      random_slope_intercept_pairs,
                                      package_mixed_model

    )
  }
  # generalised linear model
  if (caps_method == "GENERALISED LINEAR MODEL" |
      caps_method == "GENERALISED_LINEAR_MODEL"
      | caps_method == "GLM" | caps_method == "GENERALISED LINEAR") {
    results <- use_generalised_linear_model(param_to_be_estimated,
                                            dataset, indep_var,
                                            family = info_distribution,
                                            covariates, interaction,
                                            naaction, link)
  }
  #generalised linear multlevel mode  or generlised mixed effect model
  if (caps_method == "GENERALISED MULTILEVEL MODELLING" |
      caps_method == "GENERALISED_MULTILEVEL_MODELLING" |
      caps_method == "GENERALISED_MULTILEVEL" |
      caps_method == "GENERALISED MULTILEVEL" |
      caps_method == "GENERALISED MIXED EFFECT" |
      caps_method == "GENERALISED_MIXED_EFFECT" |
      caps_method == "GENERALISED MIXED" |
      caps_method == "GENERALISED_MIXED") {
    results <- use_generalised_linear_mixed_model(param_to_be_estimated,
                                              dataset, fix_eff,
                                              fix_eff_interact_vars,
                                              random_intercept_vars,
                                              nested_intercept_vars_pairs,
                                              cross_intercept_vars_pairs,
                                              uncorrel_slope_intercept_pairs,
                                              random_slope_intercept_pairs,
                                              family = info_distribution,
                                              link, package_mixed_model)
  }
  # seemingly unrelated regression or bivariate regression
  if (caps_method == "SEEMINGLY UNRELATED REGRESSION" |
      caps_method == "SEEMINGLY_UNRELATED_REGRESSION" |
      caps_method == "SEEMINGLY_UNRELATED" |
      caps_method == "SEEMINGLY UNRELATED" |
      caps_method == "BIVARIATE_REGRESSION" |
      caps_method == "BIVARIATE REGRESSION" |
      caps_method == "BIVARIATE") {
    results <- use_seemingly_unrelated_regression(
      param1_to_be_estimated = param_to_be_estimated,
      param2_to_be_estimated, dataset,
      indep_var, covariates1 = covariates, covariates2,
      interaction1 = interaction, interaction2
    )
  }
  # top function for all survival models
  if (caps_method == "SURVIVAL" | caps_method == "SURVIVAL ANALYSIS") {
    results <- use_survival_analysis(
      param_to_be_estimated, dataset, indep_var, info_get_method,
      info_distribution, covariates, timevar_survival, cluster_var
    )
  }
  if (is.null(results))
    stop("No appropriate methods found, please check")
  return(results)
}
#' ###########################################################################
#' Get the parameter values using the linear regression
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates-calculations to be done before passing
#' @param interaction boolean value to indicate interaction in the case of
#' linear regression, false by default
#' @return the results of the regression analysis
#' @examples
#' \donttest{
#' results_lm <- use_linear_regression("dist",
#'   dataset = cars,
#'   indep_var = "speed", covariates = NA, interaction = FALSE)
#' }
#' \donttest{
#' library(car)
#' results_lm <- use_linear_regression("mpg",
#'   dataset = mtcars,
#'   indep_var = "disp", covariates = c("hp", "wt", "drat"),
#'   interaction = FALSE)
#' }
#' @export
#' @importFrom relaimpo boot.relimp
#' @importFrom broom tidy
#' @importFrom MASS stepAIC
#' @details
#' This function returns the results and plots after doing linear regression
#' Requires param to be estimated, dataset, independent variables and
#' information on covariates, and interaction variables if there are
#' Uses form_expression_lm to create the expression as per R standard
#' for e.g lm(y ~ x ). Returns the fit result,s summary results as returned by
#' summary(), confidence interval for fit coefficients (ci_coeff), variance
#' covariance matrix, cholesky decomposition matrix, Results from correlation
#' test, plot of diagnostic tests and model fit  assumptions, plot of model
#' prediction diagnostic include AIC, R2, and BIC. The results of the
#' prediction ie predicted values when each of covariate is fixed will be
#' returned in prediction matrix predicted values will provide the mean value
#' of param_to_to_estimated as calculated by the linear regression formula.
#' ref:https://www.statmethods.net/stats/regression.html
use_linear_regression <- function(param_to_be_estimated, dataset, indep_var,
                                  covariates, interaction) {

  # regular checks to see the required parameters ar given
  check_list <- c(param_to_be_estimated, indep_var, interaction)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Some of parameters are null or NA")

  if (is.null(dataset)) {
    stop("Need to provide a data set or file to lookup the data")
  }
  if (is.character(dataset)) {
    dataset <- load_trial_data(dataset)
  } else {
    dataset <- dataset
  }
  # create the expression for linear regression in R using the keyword "lm"
  expression_recreated <- form_expression_lm(param_to_be_estimated, indep_var,
                                             covariates, interaction)
  # Fit results
  fit <- eval(parse(text = expression_recreated$formula))
  # summary of fit
  summary <- summary(fit)
  # model coefficients
  #Betas (coefficients) in a linear regression represents how much the
  #dependent variable increases when that predictor is increased 1 unit
  #and the other predictors are held constant.
  model_coeff <- stats::coefficients(fit)
  # confidence limits on model coefficients
  ci_coeff <- stats::confint(fit)
  # predicted parameters from the fir
  predicted_param_estimated <- stats::fitted(fit)
  # residuals from the fit
  residuals <- stats::residuals(fit)
  # variance and covraince of the model coefficients
  variance_covariance_coeff <- stats::vcov(fit)
  # cholesky decomposition matrix of the varcovar matrix
  chol_decomp_matrix <- chol(variance_covariance_coeff)

  # Odds ratio and their confidence levels, in linear regession this is not imp
  table_this <- broom::tidy(fit)
  OR <- exp(table_this$estimate)
  LCI <- exp(table_this$estimate - stats::qnorm(0.975, 0, 1) *
               table_this$std.error)
  UCI <- exp(table_this$estimate + stats::qnorm(0.975, 0, 1) *
               table_this$std.error)
  or_from_lm <- data.frame(cbind(term = table_this$term, LCI, OR, UCI))

  # stepwise regression?
  if (sum(is.na(covariates)) == 0) {
    step <- MASS::stepAIC(fit, direction = "both")
    stepwise_regression_results <- step$anova # display results
  }
  # Mode diagnostic results for linear regression
  diganostic_results <- do_diagnostic_linear_regression(
    "lm", fit, expression_recreated,
    param_to_be_estimated,
    dataset, indep_var, covariates, interaction
  )
  # predict regression results
  prediction_regression_results <- prediction_regression(
    "lm", fit, expression_recreated,
    param_to_be_estimated, indep_var,
    covariates, interaction
  )
  if (sum(is.na(covariates)) == 0) {
    # all results in  a list and return this
    results <- list(
      param_to_be_estimated = param_to_be_estimated,
      expression_recreated = expression_recreated,
      fit = fit,
      summary = summary,
      model_coeff = model_coeff,
      ci_coeff = ci_coeff,
      predicted_values = predicted_param_estimated,
      residuals = residuals,
      variance_covariance_coeff = variance_covariance_coeff,
      cholesky_decomp_matrix = chol_decomp_matrix,
      odds_ratio_ci = or_from_lm,
      diganostic_results = diganostic_results,
      prediction_regression_results = prediction_regression_results,
      stepwise_regression_results = stepwise_regression_results
    )

  } else {
    # all results in  a list and return this
    results <- list(
      param_to_be_estimated = param_to_be_estimated,
      expression_recreated = expression_recreated,
      fit = fit,
      summary = summary,
      model_coeff = model_coeff,
      ci_coeff = ci_coeff,
      predicted_values = predicted_param_estimated,
      residuals = residuals,
      variance_covariance_coeff = variance_covariance_coeff,
      cholesky_decomp_matrix = chol_decomp_matrix,
      odds_ratio_ci = or_from_lm,
      diganostic_results = diganostic_results,
      prediction_regression_results = prediction_regression_results
    )

  }
  return(results)
}
#' ############################################################################
#' Get the parameter values using logistic regression
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param family distribution name  eg. for logistic regression -binomial
#' @param covariates list of covariates-calculations to be done before passing
#' @param interaction boolean value to indicate interaction in the case of
#' linear regression
#' @param naaction action to be taken with the missing values
#' @param link link function if not the default for each family
#' @return the results of the regression analysis
#' @examples
#' \donttest{
#' gm_result <- use_generalised_linear_model(
#'   param_to_be_estimated = "Direction",
#'   dataset = ISLR::Smarket, indep_var = "Lag1", family = "binomial",
#'   covariates = c("Lag2", "Lag3"),
#'   interaction = FALSE, naaction = "na.omit", link = NA)
#' }
#' @export
#' @details
#' This function returns the results and plots after doing linear regression
#' Requires param to be estimated, dataset, independent variables and
#' information on covariates, and interaction variables if there are
#' Uses form_expression_glm to create the expression as per R standard for
#' e.g glm(y ~ x ). Returns the fit result,s summary results as returned by
#' summary(), confidence interval for
#' fit coefficients (ci_coeff), variance covariance matrix, cholesky
#' decomposition matrix, results from correlation test, plot of diagnostic
#' tests and model fit  assumptions, plot of model prediction
#' diagnostic include AIC, R2, and BIC. The results of the prediction
#' ie predicted values for fixed other variables will be returned in
#' prediction matrix
use_generalised_linear_model <- function(param_to_be_estimated, dataset,
                                         indep_var, family, covariates,
                                         interaction, naaction, link = NA) {

  # regular checks to see the required parameters ar given
  check_list <- c(param_to_be_estimated, indep_var, interaction, family)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Some of parameters are null or NA")

  if (is.null(dataset)) {
    stop("Need to provide a data set or file to lookup the data")
  }
  if (is.character(dataset)) {
    dataset <- load_trial_data(dataset)
  } else {
    dataset <- dataset
  }
  # generate expression for logistic regression glm in R
  expression_recreated <- form_expression_glm(param_to_be_estimated, indep_var,
                            family, covariates, interaction, naaction, link)
  # fit result
  fit <- eval(parse(text = expression_recreated$formula))
  # summary fo fit
  summary <- summary(fit)
  # model coefficients
  model_coeff <- stats::coefficients(fit)
  # confidence intervals for the model coefficients
  ci_coeff <- stats::confint(fit)
  # predicted parameters
  predicted_param_estimated <- stats::fitted(fit)
  #residuals from the fit
  residuals <- stats::residuals(fit)
  # variance and covariance matrix of the model coefficients
  variance_covariance_coeff <- stats::vcov(fit)
  # cholesky decomposition matrix from vcov matrix
  chol_decomp_matrix <- chol(variance_covariance_coeff)
  # Odds ratio and the cis for logistic regression
  table_this <- broom::tidy(fit)
  OR <- exp(table_this$estimate)
  LCI <- exp(table_this$estimate - stats::qnorm(0.975, 0, 1) *
               table_this$std.error)
  UCI <- exp(table_this$estimate + stats::qnorm(0.975, 0, 1) *
               table_this$std.error)
  or <- data.frame(cbind(term = table_this$term, LCI, OR, UCI))

  # stepwise regression?
  if (sum(is.na(covariates)) == 0) {
    step <- MASS::stepAIC(fit, direction = "both")
    stepwise_regression_results <- step$anova # display results
  } else {
    stepwise_regression_results <- NA
  }
  # model diagnostic results - how does the model do
  diganostic_results <- do_diagnostic_glm(
    "glm", fit, expression_recreated,
    param_to_be_estimated,
    dataset, indep_var, covariates, interaction
  )
  # prediction of regression results
  prediction_regression_results <- prediction_regression(
    "glm", fit, expression_recreated,
    param_to_be_estimated, indep_var,
    covariates, interaction
  )
  results <- list(
    param_to_be_estimated = param_to_be_estimated,
    expression_recreated = expression_recreated,
    fit = fit,
    summary = summary,
    model_coeff = model_coeff,
    ci_coeff = ci_coeff,
    predicted_values = predicted_param_estimated,
    residuals = residuals,
    variance_covariance_coeff = variance_covariance_coeff,
    cholesky_decomp_matrix = chol_decomp_matrix,
    odds_ratio_ci = or,
    diganostic_results = diganostic_results,
    prediction_regression_results = prediction_regression_results,
    stepwise_regression_results = stepwise_regression_results
  )
  return(results)
}
###############################################################################
#' Function for mixed effect regression
#' @param package_mixed_model package to be used for mixed model
#' @param param_to_be_estimated column name of dependent variable
#' @param dataset a dataframe
#' @param fix_eff names of variables as fixed effect predictors
#' @param fix_eff_interact_vars, those of the fixed effect predictors that
#' show interaction
#' @param random_intercept_vars, names of variables for random intercept
#' @param nested_intercept_vars_pairs, those of the random intercept variables
#' with nested effect
#' @param cross_intercept_vars_pairs, those of the random intercept variables
#' with crossed effect
#' @param uncorrel_slope_intercept_pairs, variables with no correlated
#' intercepts
#' @param random_slope_intercept_pairs, random slopes intercept pairs - this
#' is a list of paired variables
#' @return result regression result with plot if success and -1, if failure
#' @examples
#' \donttest{
#' datafile <- system.file("extdata", "data_linear_mixed_model.csv",
#' package = "packDAMipd")
#' dataset = utils::read.table(datafile, header = TRUE, sep = ",",
#' na.strings = "NA",
#' dec = ".", strip.white = TRUE)
#' result <- use_linear_mixed_model("extro",
#'   dataset = dataset,
#'   fix_eff = c("open", "agree", "social"), fix_eff_interact_vars = NULL,
#'   random_intercept_vars = c("school", "class"),
#'   nested_intercept_vars_pairs = list(c("school", "class")),
#'   cross_intercept_vars_pairs = NULL, uncorrel_slope_intercept_pairs = NULL,
#'   random_slope_intercept_pairs = NULL, package_mixed_model = NA)
#' }
#' @export
#' @importFrom utils read.csv
#' @importFrom utils read.table
use_linear_mixed_model <- function(param_to_be_estimated, dataset,
                                   fix_eff,
                                   fix_eff_interact_vars,
                                   random_intercept_vars,
                                   nested_intercept_vars_pairs,
                                   cross_intercept_vars_pairs,
                                   uncorrel_slope_intercept_pairs,
                                   random_slope_intercept_pairs,
                                   package_mixed_model) {
  # regular checks to see the required parameters ar given
  check_list <- c(param_to_be_estimated, fix_eff, random_intercept_vars)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Some of parameters are null or NA")
  if (is.null(dataset)) {
    stop("Need to provide a data set or file to lookup the data")
  }
  if (is.character(dataset)) {
    dataset <- load_trial_data(dataset)
  } else {
    dataset <- dataset
  }
  if (sum(is.na(nested_intercept_vars_pairs)) != 0)
    nested_intercept_vars_pairs <- NULL
  if (sum(is.na(cross_intercept_vars_pairs)) != 0)
    cross_intercept_vars_pairs <- NULL
  if (sum(is.na(uncorrel_slope_intercept_pairs)) != 0)
    uncorrel_slope_intercept_pairs <- NULL
  if (sum(is.na(random_slope_intercept_pairs)) != 0)
    random_slope_intercept_pairs <- NULL
  if (sum(is.na(fix_eff_interact_vars)) != 0)
    fix_eff_interact_vars <- NULL

  # create expression for mixed model using the variables fixed effect and
  # random intercept
  if (!is.null(package_mixed_model)) {
    if (is.na(package_mixed_model) | package_mixed_model == "lme4") {
      expression_recreated <-
        form_expression_mixed_model_lme4(param_to_be_estimated, dataset,
                                       fix_eff, fix_eff_interact_vars,
                                       random_intercept_vars,
                                       nested_intercept_vars_pairs,
                                       cross_intercept_vars_pairs,
                                       uncorrel_slope_intercept_pairs,
                                       random_slope_intercept_pairs,
                                       family = NA, link = NA)
    } else {
      if (package_mixed_model == "nlme") {
        expression_recreated <-
          form_expression_mixed_model_lme4(param_to_be_estimated, dataset,
                                           fix_eff, fix_eff_interact_vars,
                                           random_intercept_vars,
                                           nested_intercept_vars_pairs,
                                           cross_intercept_vars_pairs,
                                           uncorrel_slope_intercept_pairs,
                                           random_slope_intercept_pairs,
                                           family = NA, link = NA)
      }
    }
  } else {
    expression_recreated <-
      form_expression_mixed_model_lme4(param_to_be_estimated, dataset,
                                       fix_eff, fix_eff_interact_vars,
                                       random_intercept_vars,
                                       nested_intercept_vars_pairs,
                                       cross_intercept_vars_pairs,
                                       uncorrel_slope_intercept_pairs,
                                       random_slope_intercept_pairs,
                                       family = NA, link = NA)
  }

  # fit result
  fit <- eval(parse(text = expression_recreated))
  # summary of fit
  summary <- summary(fit)
  model_coeff <- stats::coefficients(fit)
  #variance covariance matrix for the fixed effect
  vcov_fixed_eff <- stats::vcov(fit)
  #variance covariance matrix for the random effect
  varcorr_random_eff <- as.data.frame(lme4::VarCorr(fit))
  # cholsky decompisiton matrix for random effect
  to_extract <- 2 * length(random_slope_intercept_pairs)


  if (is.null(nested_intercept_vars_pairs) &
      is.null(uncorrel_slope_intercept_pairs)
      & !is.null(random_slope_intercept_pairs)) {
    to_extract_corr <- length(random_slope_intercept_pairs)
    variances <- varcorr_random_eff[1:to_extract, "vcov"]
    correlation <- varcorr_random_eff[to_extract:to_extract + to_extract_corr,
                                      "sdcor"]
    vcov_random_eff <- matrix(rep(0, to_extract * to_extract), byrow = TRUE,
                              ncol = to_extract)
    diag(vcov_random_eff) <- variances
    for (i in 1:to_extract) {
      j <- i + 1
      if (j <= to_extract) {
        covariance <- correlation[i] * sqrt(variances[i] * variances[j])
        vcov_random_eff[i, j] <- vcov_random_eff[j, i] <- covariance
      }
    }
    cholesky_decomp_matrix_random_eff <- chol(vcov_random_eff)
  }
  # cholsky decomposition matrix for fixed effect
  cholesky_decomp_matrix_fixed_eff <- chol(vcov_fixed_eff)
  # fixed-effect parameter estimates
  fixed_coef <- summary$coef[, 1, drop = FALSE]
  # fixed-effect parameter standard errors
  fixed_coef_se <- summary$coef[, 2, drop = FALSE]
  # confidence intervals for fixed effect
  upperCI <- fixed_coef + 1.96 * fixed_coef_se
  lowerCI <- fixed_coef - 1.96 * fixed_coef_se
  ci_coeff <- cbind(fixed_coef, upperCI, lowerCI)
  colnames(ci_coeff) <- c("fixed-effect_coeff", "upper-ci", "lower-ci")

  # residuals plot
  name_file_plot <- paste0("lmer_residuals_", param_to_be_estimated, ".pdf",
                           sep = "")
  grDevices::pdf(name_file_plot)
  oldpar <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(3, 1), mar = c(4, 4, 1, 1))
  plot_diagnostics <- graphics::plot(stats::fitted(fit), stats::resid(fit),
    type = "p", xlab =
      paste("Fitted values", param_to_be_estimated), ylab = "Residuals",
        main = "Residuals vs Fitted"
  )
  # Fitted plot
  graphics::plot(stats::fitted(fit), sqrt(abs(stats::resid(fit))),
    type = "p", xlab =
      paste("Fitted values", param_to_be_estimated), ylab = "Sqrt(residuals)",
    main = "Scale  vs Location"
  )
  # Normal Q-Q plot
  stats::qqnorm(stats::resid(fit), main = "Normal Q-Q plot")
  stats::qqline(stats::resid(fit))
  on.exit(graphics::par(oldpar))
  grDevices::dev.off()

  # Predicted and simulated values after regression
  predicted <- stats::predict(fit, newdata = dataset)
  # deterministic and takes only fixed effect
  simulated <- stats::simulate(fit, seed = 1, re.form = NA,
                               allow.new.levels = T, newdata = dataset)[, 1]
  # stochastic and takes both fixed effect and random effect
  # source  https://gist.github.com/tmalsburg/df66e6c2ab494fad83ee
  no_fixed_eff <- length(fix_eff)
  if (!is.null(fix_eff)) {
    name_file_plot <- paste0("lmer_fixed_eff_predicted and simulated_",
                             param_to_be_estimated, ".pdf", sep = "")
    grDevices::pdf(name_file_plot)
    oldpar <- graphics::par(no.readonly = TRUE)
    graphics::par(mfrow = c(no_fixed_eff, 3), mar = c(4, 4, 1, 1))
    for (i in seq_len(length(fix_eff))) {
      xvar <- fix_eff[i]
      with(dataset, graphics::plot(dataset[[param_to_be_estimated]],
                                   dataset[[xvar]], main = "Original data",
                                   ylab = param_to_be_estimated, xlab = xvar
      ))
      with(dataset, graphics::plot(predicted, dataset[[xvar]],
        main = "Predicted data", xlab = xvar, ylab = ""
      ))
      with(dataset, graphics::plot(simulated, dataset[[xvar]],
        main = "Simulated data", xlab = xvar, ylab = ""
      ))
      graphics::grid()
    }
    on.exit(graphics::par(oldpar))
    grDevices::dev.off()
  }

  no_random_eff <- length(random_intercept_vars)
  name_file_plot <- paste0("lmer_random_eff_predicted and simulated_",
                           param_to_be_estimated, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  oldpar <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(no_random_eff, 3), mar = c(4, 4, 1, 1))
  for (i in seq_len(length(random_intercept_vars))) {
    xvar <- random_intercept_vars[i]
    with(dataset, graphics::plot(tapply(dataset[[param_to_be_estimated]],
                                        dataset[[xvar]], mean),
      main = "Original data - mean", ylab = param_to_be_estimated, xlab = xvar
    ))
    with(dataset, graphics::plot(tapply(predicted, dataset[[xvar]], mean),
      main = "Predicted data  - mean", xlab = xvar, ylab = ""
    ))
    with(dataset, graphics::plot(tapply(simulated, dataset[[xvar]], mean),
      main = "Simulated data  - mean", xlab = xvar, ylab = ""
    ))
    graphics::grid()
  }
  on.exit(graphics::par(oldpar))
  grDevices::dev.off()
  if (!is.null(fix_eff)) {
    name_file_plot <- paste0("lmer_prediction_",
                             param_to_be_estimated, ".pdf", sep = "")
    grDevices::pdf(name_file_plot)
    for (i in 1:no_fixed_eff) {
      for (j in 1:no_random_eff) {
        print(paste("i=", i, "and j = ", j))
        xvar <- dataset[[fix_eff[i]]]
        yvar <- dataset[[param_to_be_estimated]]
        group <- factor(dataset[[random_intercept_vars[j]]])
        this_plot <- ggplot2::ggplot(dataset, ggplot2::aes(x = xvar,
                                              y = yvar, colour = group)) +
          ggplot2::geom_point(size = 3) +
          ggplot2::geom_line(ggplot2::aes(y = predicted), size = 2) +
          ggplot2::labs(color = random_intercept_vars[j]) +
          ggplot2::xlab(fix_eff[i]) +
          ggplot2::ylab(param_to_be_estimated)
        print(this_plot)
      }
    }
    grDevices::dev.off()
  }
  # Model fit diagnostics
  fit_diagnostics <- summary$AICtab
  # send the results
  if (is.null(nested_intercept_vars_pairs) &
      is.null(uncorrel_slope_intercept_pairs) &
      !is.null(random_slope_intercept_pairs)) {
    results <- (list(
      param_to_be_estimated = param_to_be_estimated,
      expression_recreated = expression_recreated,
      fit = fit,
      summary = summary,
      model_coeff = model_coeff,
      ci_coeff = ci_coeff,
      variance_covariance_coeff_fixed = vcov_fixed_eff,
      variance_covariance_coeff_random = vcov_random_eff,
      cholesky_decomp_matrix_fixed_eff = cholesky_decomp_matrix_fixed_eff,
      cholesky_decomp_matrix_random_eff = cholesky_decomp_matrix_random_eff,
      fit_diagnostics = fit_diagnostics
    ))
  } else {
    results <- (list(
      param_to_be_estimated = param_to_be_estimated,
      expression_recreated = expression_recreated,
      fit = fit,
      summary = summary,
      model_coeff = model_coeff,
      ci_coeff = ci_coeff,
      variance_covariance_coeff_fixed = vcov_fixed_eff,
      cholesky_decomp_matrix_fixed_eff = cholesky_decomp_matrix_fixed_eff,
      fit_diagnostics = fit_diagnostics
    ))
  }
  return(results)
}
##############################################################################
#' Function for generalised linear mixed model
#' @param param_to_be_estimated column name of dependent variable
#' @param dataset a dataframe
#' @param fix_eff names of variables as fixed effect predictors
#' @param fix_eff_interact_vars, those of the fixed effect predictors that
#' show interaction
#' @param random_intercept_vars, names of variables for random intercept
#' @param nested_intercept_vars_pairs, those of the random intercept variables
#' with nested effect
#' @param cross_intercept_vars_pairs, those of the random intercept variables
#' with crossed effect
#' @param uncorrel_slope_intercept_pairs, variables with no
#' correlated intercepts
#' @param random_slope_intercept_pairs, random slopes intercept pairs -
#' this is a list of paired variables
#' @param family, family of distributions for the response variable
#' @param link, link function for the variances
#' @param package_mixed_model package to be used for mixed model
#' @return result regression result with plot if success and -1, if failure
#' @examples
#' \donttest{
#' datafile <- system.file("extdata", "culcita_data.csv",
#' package = "packDAMipd")
#' dataset <- read.csv(datafile)
#' results1 = use_generalised_linear_mixed_model("predation",
#' dataset = datafile,fix_eff = c("ttt"), family = "binomial",
#' fix_eff_interact_vars = NULL, random_intercept_vars = c("block"),
#' nested_intercept_vars_pairs = NULL, cross_intercept_vars_pairs = NULL,
#' uncorrel_slope_intercept_pairs = NULL, random_slope_intercept_pairs = NULL,
#'  link = NA, package_mixed_model = NA)
#'  }
#' @export
use_generalised_linear_mixed_model <- function(param_to_be_estimated, dataset,
                                          fix_eff, fix_eff_interact_vars,
                                          random_intercept_vars,
                                          nested_intercept_vars_pairs,
                                          cross_intercept_vars_pairs,
                                          uncorrel_slope_intercept_pairs,
                                          random_slope_intercept_pairs,
                                          family, link, package_mixed_model) {
  # regular checks to see the required parameters ar given
  check_list <- list(param_to_be_estimated, random_intercept_vars, family)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Some of parameters are null or NA")

  if (is.null(dataset)) {
    stop("Need to provide a data set or file to lookup the data")
  }
  if (is.character(dataset)) {
    dataset <- load_trial_data(dataset)
  } else {
    dataset <- dataset
  }
  if (sum(is.na(nested_intercept_vars_pairs)) != 0)
    nested_intercept_vars_pairs <- NULL
  if (sum(is.na(cross_intercept_vars_pairs)) != 0)
    cross_intercept_vars_pairs <- NULL
  if (sum(is.na(uncorrel_slope_intercept_pairs)) != 0)
    uncorrel_slope_intercept_pairs <- NULL
  if (sum(is.na(random_slope_intercept_pairs)) != 0)
    random_slope_intercept_pairs <- NULL
  if (sum(is.na(fix_eff_interact_vars)) != 0)
    fix_eff_interact_vars <- NULL
  if (is.na(package_mixed_model) | is.null(package_mixed_model) |
          package_mixed_model == "lme4") {
    expression_recreated <- form_expression_mixed_model_lme4(
      param_to_be_estimated, dataset, fix_eff, fix_eff_interact_vars,
      random_intercept_vars, nested_intercept_vars_pairs,
      cross_intercept_vars_pairs,
      uncorrel_slope_intercept_pairs, random_slope_intercept_pairs, family,
      link
    )
  } else {
    if (package_mixed_model == "nlme") {
      expression_recreated <- form_expression_mixed_model_lme4(
        param_to_be_estimated, dataset, fix_eff, fix_eff_interact_vars,
        random_intercept_vars, nested_intercept_vars_pairs,
        cross_intercept_vars_pairs,
        uncorrel_slope_intercept_pairs, random_slope_intercept_pairs, family,
        link
      )
    }
  }
  # fit result
  fit <- eval(parse(text = expression_recreated))
  # summary of fit
  summary <- summary(fit)
  # variance covariance of model coefficients -fixed effect
  vcov_fixed_eff <- stats::vcov(fit)
  # variance covariance of random effect
  varcorr_random_eff <- as.data.frame(lme4::VarCorr(fit))

  #Cholesky for random effect
  to_extract <- 2 * length(random_slope_intercept_pairs)
  if (is.null(nested_intercept_vars_pairs)
      & is.null(uncorrel_slope_intercept_pairs)
      & !is.null(random_slope_intercept_pairs)) {
    to_extract_corr <- length(random_slope_intercept_pairs)
    variances <- varcorr_random_eff[1:to_extract, "vcov"]
    correlation <- varcorr_random_eff[to_extract:to_extract + to_extract_corr,
                                      "sdcor"]
    vcov_random_eff <- matrix(rep(0, to_extract * to_extract), byrow = TRUE,
                              ncol = to_extract)
    diag(vcov_random_eff) <- variances
    for (i in 1:to_extract) {
      j <- i + 1
      if (j <= to_extract) {
        covariance <- correlation[i] * sqrt(variances[i] * variances[j])
        vcov_random_eff[i, j] <- vcov_random_eff[j, i] <- covariance
      }
      cholesky_decomp_matrix_random_eff <- chol(vcov_random_eff)
    }
  }
  # cholesky for fixed effect
  cholesky_decomp_matrix_fixed_eff <- chol(vcov_fixed_eff)

  # fixed-effect parameter estimates
  fixed_coef <- summary$coef[, 1, drop = FALSE]
  # fixed-effect parameter standard errors
  fixed_coef_se <- summary$coef[, 2, drop = FALSE]
  upperCI <- fixed_coef + 1.96 * fixed_coef_se
  lowerCI <- fixed_coef - 1.96 * fixed_coef_se
  ci_coeff <- cbind(fixed_coef, upperCI, lowerCI)
  colnames(ci_coeff) <- c("fixed-effect_coeff", "upper-ci", "lower-ci")

  # residuals of the fit
  name_file_plot <- paste0("glmer_residuals_", param_to_be_estimated,
                           ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  oldpar <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(3, 1), mar = c(4, 4, 1, 1))
  plot_diagnostics <- graphics::plot(stats::fitted(fit), stats::resid(fit),
    type = "p", xlab =
      paste("Fitted values", param_to_be_estimated), ylab = "Residuals",
    main = "Residuals vs Fitted"
  )
  graphics::plot(stats::fitted(fit), sqrt(abs(stats::resid(fit))),
    type = "p", xlab =
      paste("Fitted values", param_to_be_estimated), ylab = "Sqrt(residuals)",
    main = "Scale  vs Location"
  )
  stats::qqnorm(stats::resid(fit), main = "Normal Q-Q plot")
  stats::qqline(stats::resid(fit))
  on.exit(graphics::par(oldpar))
  grDevices::dev.off()

 # predicted and simulated regression results
  predicted <- stats::predict(fit, newdata = dataset)
  # deterministic and takes only fixed effect
  simulated <- stats::simulate(fit, newdata = dataset)[, 1]
  # stochastic and takes both fixed effect and random effect
  # source  https://gist.github.com/tmalsburg/df66e6c2ab494fad83ee
  no_fixed_eff <- length(fix_eff)

  if (!is.null(fix_eff)) {
    name_file_plot <- paste0("glmer_fixed_eff_predicted and simulated_",
                             param_to_be_estimated, ".pdf", sep = "")
    grDevices::pdf(name_file_plot)
    oldpar <- graphics::par(no.readonly = TRUE)
    graphics::par(mfrow = c(no_fixed_eff, 3), mar = c(4, 4, 1, 1))
    for (i in seq_len(length(fix_eff))) {
      xvar <- fix_eff[i]
      xvalues <- dataset[[xvar]]
      if (!is.numeric(xvalues))
            xvalues <- unclass(as.factor(xvalues))
      with(dataset, graphics::plot(dataset[[param_to_be_estimated]],
                                   xvalues,
                                   main = "Original data",
                                   ylab = param_to_be_estimated, xlab = xvar
      ))
      with(dataset, graphics::plot(predicted, xvalues,
                            main = "Predicted data", xlab = xvar, ylab = ""
      ))
      with(dataset, graphics::plot(simulated, xvalues,
                            main = "Simulated data", xlab = xvar, ylab = ""
      ))
      graphics::grid()
    }
    on.exit(graphics::par(oldpar))
    grDevices::dev.off()
  }
  no_random_eff <- length(random_intercept_vars)
  name_file_plot <- paste0("glmer_random_eff_predicted and simulated_",
                           param_to_be_estimated, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  oldpar <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(no_random_eff, 3), mar = c(4, 4, 1, 1))
  for (i in seq_len(length(random_intercept_vars))) {
    xvar <- random_intercept_vars[i]
    with(dataset, graphics::plot(tapply(dataset[[param_to_be_estimated]],
                                        dataset[[xvar]], mean),
                                 main = "Original data - mean",
                                 ylab = param_to_be_estimated, xlab = xvar
    ))
    with(dataset, graphics::plot(tapply(predicted, dataset[[xvar]], mean),
      main = "Predicted data  - mean", xlab = xvar, ylab = ""
    ))
    with(dataset, graphics::plot(tapply(simulated, dataset[[xvar]], mean),
      main = "Simulated data  - mean", xlab = xvar, ylab = ""
    ))
    graphics::grid()
  }
  on.exit(graphics::par(oldpar))
  grDevices::dev.off()
  if (!is.null(fix_eff)) {
    name_file_plot <- paste0("glmer_prediction_", param_to_be_estimated,
                             ".pdf", sep = "")
    grDevices::pdf(name_file_plot)
    for (i in 1:no_fixed_eff) {
      for (j in 1:no_random_eff) {
          xvar <- dataset[[fix_eff[i]]]
          yvar <- dataset[[param_to_be_estimated]]
          group <- factor(dataset[[random_intercept_vars[j]]])
          this_plot <- ggplot2::ggplot(dataset, ggplot2::aes(x = xvar,
                                                y = yvar, colour = group)) +
            ggplot2::geom_point(size = 3) +
            ggplot2::geom_line(ggplot2::aes(y = predicted), size = 2) +
            ggplot2::labs(color = random_intercept_vars[j]) +
            ggplot2::xlab(fix_eff[i]) +
            ggplot2::ylab(param_to_be_estimated)
        print(this_plot)
      }
    }
    grDevices::dev.off()
  }
  # Model fit diagnostics
  fit_diagnostics <- summary$AICtab
  # results
  if (is.null(nested_intercept_vars_pairs) &
      is.null(uncorrel_slope_intercept_pairs) &
      !is.null(random_slope_intercept_pairs)) {
    results <- (list(
      param_to_be_estimated = param_to_be_estimated,
      expression_recreated = expression_recreated,
      fit = fit,
      summary = summary,
      ci_coeff = ci_coeff,
      variance_covariance_coeff_fixed = vcov_fixed_eff,
      variance_covariance_coeff_random = vcov_random_eff,
      cholesky_decomp_matrix_fixed_eff = cholesky_decomp_matrix_fixed_eff,
      cholesky_decomp_matrix_random_eff = cholesky_decomp_matrix_random_eff,
      fit_diagnostics = fit_diagnostics
    ))
  } else {
    results <- (list(
      param_to_be_estimated = param_to_be_estimated,
      expression_recreated = expression_recreated,
      fit = fit,
      summary = summary,
      ci_coeff = ci_coeff,
      variance_covariance_coeff_fixed = vcov_fixed_eff,
      cholesky_decomp_matrix_fixed_eff = cholesky_decomp_matrix_fixed_eff,
      fit_diagnostics = fit_diagnostics
    ))
  }
  return(results)
}
##############################################################################
#' Bivariate regression for correlated observations
#' @param param1_to_be_estimated  parameter of interest
#' @param param2_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates1 list of covariates - for equation 1
#' @param covariates2 list of covariates - for equation 2
#' @param interaction1 boolean value to indicate interaction - for equation 1
#' @param interaction2 boolean value to indicate interaction - for equation 2
#' false by default
#' @return the results of the regression analysis
#' @examples
#' \donttest{
#' datafile <- system.file("extdata", "sureg_data.csv", package = "packDAMipd")
#' dataset <- read.csv(datafile, stringsAsFactors = TRUE)
#' results_sureg <- use_seemingly_unrelated_regression("read", "math",
#'   dataset = dataset,
#'   indep_var = "female", covariates1 = c("as.numeric(ses)", "socst"),
#'   covariates2 = c("as.numeric(ses)", "science"), interaction1 = FALSE,
#'   interaction2 = FALSE
#' )
#' }
#' @export
#' @importFrom systemfit systemfit
use_seemingly_unrelated_regression <- function(param1_to_be_estimated,
                                               param2_to_be_estimated,
                                               dataset, indep_var,
                                               covariates1, covariates2,
                                               interaction1, interaction2) {
  # regular checks to see the required parameters ar given
  check_list <- c(param1_to_be_estimated, param2_to_be_estimated, indep_var,
                 interaction1, interaction2)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop("Some of parameters are null or NA")

    if (is.null(dataset)) {
      stop("Need to provide a data set or file to lookup the data")
    }
    if (is.character(dataset)) {
      dataset <- load_trial_data(dataset)
    } else {
      dataset <- dataset
    }
  #there has to be two formula nad generate using lm expression
  formula1 <- form_expression_lm(param1_to_be_estimated, indep_var,
                                 covariates1, interaction1)
  formula2 <- form_expression_lm(param2_to_be_estimated, indep_var,
                                 covariates2, interaction2)
  pattern <- "lm"
  # get the formulas to be sent to systemfit extract from the above formulas
  pos1 <- stringr::str_locate(formula1$formula, pattern)
  end_pattern <- ", data"
  pos1_end <- stringr::str_locate(formula1$formula, end_pattern)
  pos2 <- stringr::str_locate(formula2$formula, pattern)
  pos2_end <- stringr::str_locate(formula2$formula, end_pattern)
  string1 <- substr(formula1$formula, pos1[2] + 2, pos1_end - 1)
  string2 <- substr(formula2$formula, pos2[2] + 2, pos2_end - 1)
  formula_1 <- stats::as.formula(string1)
  formula_2 <- stats::as.formula(string2)

  # do the seemingly unrelated regression
  fit <- systemfit::systemfit(list(formula_1, formula_2), data = dataset)
  # summary of fit
  summary <- summary(fit)
  # model coefficients
  model_coeff <- stats::coefficients(fit)
  # confidence interval for model coefficients
  ci_coeff <- stats::confint(fit)
  # variance covariance for model coefficients
  variance_covariance_coeff <- summary$coefCov
  # cholesky decompisiton matrix
  chol_decomp_matrix <- chol(variance_covariance_coeff)
  std_error <- stats::coef(summary)

  # fitted values
  fitted_values <- stats::fitted(fit)
  # residuals
  residuals <- stats::residuals(fit)
  sqrt_residuals <- sqrt(abs(residuals))

  # plot residuals
  name_file_plot <- paste0("sureg_residuals_", param1_to_be_estimated, "_",
                           param1_to_be_estimated, "_",
                           indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)

  # plot fitted values
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  graphics::par(mfrow = c(2, 4))
  plot_diagnostics <- graphics::plot(fitted_values$eq1, residuals$eq1,
                                     type = "p", xlab = paste("Fitted values",
                                     param1_to_be_estimated),
                                     ylab = "residuals")
  graphics::plot(fitted_values$eq2, residuals$eq2, type = "p",
                 xlab = paste("Fitted values", param2_to_be_estimated),
                 ylab = "Residuals")
  graphics::plot(fitted_values$eq1, sqrt_residuals$eq1, type = "p",
                 xlab = paste("Fitted values", param1_to_be_estimated),
                 ylab = "Sqrt(residuals)")
  graphics::plot(fitted_values$eq2, sqrt_residuals$eq2, type = "p",
                 xlab = paste("Fitted values", param1_to_be_estimated),
                 ylab = "Sqrt(residuals)")
  stats::qqnorm(residuals$eq1, main = "Normal Q-Q plot - Eq 1")
  stats::qqline(residuals$eq1)
  stats::qqnorm(residuals$eq2, main = "Normal Q-Q plot - Eq 2")
  stats::qqline(residuals$eq2)
  on.exit(graphics::par(oldpar))
  grDevices::dev.off()

  name_file_plot <- paste0(param1_to_be_estimated, "_",
                           param2_to_be_estimated, "_sureg_",
                           indep_var, ".pdf", sep = "")
  sur_predict <- stats::predict(fit, newdata = dataset, type = "response",
                                se.fit = TRUE)
  grDevices::pdf(name_file_plot)
  oldpar <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(1, 2))
  predict1 <- sur_predict$eq1.pred
  predict1_lci <- (sur_predict$eq1.pred - 2 * sur_predict$eq1.se.fit)
  predict1_uci <- (sur_predict$eq1.pred + 2 * sur_predict$eq1.se.fit)
  predict2 <- sur_predict$eq2.pred
  predict2_lci <- (sur_predict$eq2.pred - 2 * sur_predict$eq2.se.fit)
  predict2_uci <- (sur_predict$eq2.pred + 2 * sur_predict$eq2.se.fit)

  new_df <- data.frame(
    x = dataset[[indep_var]], prediction1 = predict1, lci1 = predict1_lci,
    uci1 = predict1_uci,
    prediction2 = predict2, lci2 = predict2_lci, uci2 = predict2_uci
  )
  if (is.factor(new_df$x)) {
    plot_prediction <- graphics::plot(new_df$x, new_df$prediction1,
                                type = "p", ylab = param1_to_be_estimated)
    graphics::plot(new_df$x, new_df$prediction2, type = "p",
                   ylab = param2_to_be_estimated)
    print(plot_prediction)
  } else {
    plot_prediction <- graphics::plot(new_df$x, new_df$prediction1,
                                type = "p", ylab = param1_to_be_estimated)
    graphics::lines(new_df$x, new_df$lci1)
    graphics::lines(new_df$x, new_df$uci1)
    graphics::plot(new_df$x, new_df$prediction2, type = "p",
                   ylab = param2_to_be_estimated)
    graphics::lines(new_df$x, new_df$lci2)
    graphics::lines(new_df$x, new_df$uci2)
    print(plot_prediction)
  }
  on.exit(graphics::par(oldpar))
  grDevices::dev.off()

  # fit diagnostics
  correlation_matrix <- summary$residCor
  OLS_R2 <- summary$ols.r.squared
  AIC <- stats::AIC(fit)
  BIC <- stats::BIC(fit)
  fit_diagnostics <- data.frame(cbind(OLS_R2, AIC, BIC))
  colnames(fit_diagnostics) <- c("OLS_R2", "AIC", "BIC")

  results <- (list(
    param1_to_be_estimated = param1_to_be_estimated,
    param2_to_be_estimated = param2_to_be_estimated,
    formula_1 = formula_1,
    formula_2 = formula_2,
    fit = fit,
    summary = summary,
    model_coeff = model_coeff,
    ci_coeff = ci_coeff,
    std_error = std_error,
    correlation_matrix = correlation_matrix,
    variance_covariance_coeff = variance_covariance_coeff,
    cholesky_decomp_matrix = chol_decomp_matrix,
    plot_diagnostics = plot_diagnostics,
    fit_diagnostics = fit_diagnostics,
    plot_prediction = plot_prediction
  ))
  return(results)
}
