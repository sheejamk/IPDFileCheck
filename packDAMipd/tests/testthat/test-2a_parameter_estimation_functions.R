###############################################################################
context("testing assigning parameter directly")
test_that("testing  assigning parameter directly", {
  # assigning a to vale 0.2
  expect_equal(get_parameter_direct("a", 0.2), 0.2)
  # assigning null will cause error
  expect_error(get_parameter_direct("a", NULL))
})
###############################################################################
test_that("testing reading parameter from file", {
  file <- system.file("extdata", "table_param.csv", package = "packDAMipd")
  nofile <- system.file("extdata", "nofile.csv", package = "packDAMipd")
  diff_file <- system.file("extdata", "blank.csv", package = "packDAMipd")
  nocol_parameter <- system.file("extdata", "nocol_parameter.csv",
                                 package = "packDAMipd")
  nostrategy <- system.file("extdata", "nostrategy.csv",
                                 package = "packDAMipd")
  value_na <- system.file("extdata", "value_na.csv",
                            package = "packDAMipd")
  # read the value of cost_IT from the file table_param.csv
  expect_equal(get_parameter_read("cost_IT", file, "Strategy", "Intervention"),
               2000)
  # read the value of cost_IT from the file table_param.csv
  expect_equal(get_parameter_read("cost_IT", file), 2000)
  # error while reading from a file that is not existing
  expect_error(get_parameter_read("cost_IT", nofile))
  # error while reading from a file where column value do not exist
  expect_error(get_parameter_read("cost_IT", diff_file))
  # error while reading from a file where column Parameter do not exist
  expect_error(get_parameter_read("cost_IT", nocol_parameter))
  # error while reading from a file where column value exists but
  # with Na/missing

  expect_error(get_parameter_read("cost_IT", value_na))
  # error while reading from a file where column Strategy do not exist
  expect_error(get_parameter_read("cost_IT", nostrategy, "Strategy",
                                  "intervention"))

  # error while reading a non existing parameter from the file
  expect_error(get_parameter_read("a", file))
  # paramfile null
  expect_error(get_parameter_read("a", NULL))


})
###############################################################################
test_that("testing reading parameter from mortality file", {
  paramfile <- system.file("extdata", "LifeTable_USA_Mx_2015.csv",
                           package = "packDAMipd")
  paramfile_missingage <- system.file("extdata",
                                      "LifeTable_USA_Mx_2015_missing.csv",
                           package = "packDAMipd")
  nofile <- system.file("extdata", "nofile.csv", package = "packDAMipd")
  diff_file <- system.file("extdata", "blank.csv", package = "packDAMipd")
  # File doesn't exist error
  expect_error(get_mortality_from_file(nofile, 16, "female"))
  # null file error
  expect_error(get_mortality_from_file(NULL, 16,  "female"))

  # age column doesn't exist error
  expect_error(get_mortality_from_file(diff_file, 16, "female"))
  # missing colname
  expect_error(get_mortality_from_file(paramfile_missingage, 12, NULL))
  # missing mortality column name
  expect_error(get_mortality_from_file(paramfile, 12, NULL))
  #  if you dont care about gender, the column for mortality should be given
  expect_equal(get_mortality_from_file(paramfile, 16, "Total"), 0.000351)
  #  if you want mortality for the particular gender, the column for mortality
  # need not  be given
  expect_equal(get_mortality_from_file(paramfile, 12, NULL, "female"),
               0.000112)
  #  if you want mortality for the particular gender, the column for mortality
  # need not  be given, but then gender column should be given appropriately
  expect_error(get_mortality_from_file(paramfile, 12, NULL, "hello"))

  paramfile <- system.file("extdata", "LifeTable_USA_Mx_2015_agerange.csv",
                           package = "packDAMipd")
  expect_equal(get_mortality_from_file(paramfile, 12, "female"),
               0.000142, tol = 1e-4)
  expect_equal(get_mortality_from_file(paramfile, 105, "male"),
               0.000481, tol = 1e-4)
  expect_error(get_mortality_from_file(paramfile, 105, "tot"))

 })
###############################################################################
test_that("testing parameter using distribution read from file", {
  file <- system.file("extdata", "table_param.csv", package = "packDAMipd")
  nofile <- system.file("extdata", "nofile.csv", package = "packDAMipd")
  #
  get_parameter_def_distribution("b", file,
          c("Param1_name", "Param1_value", "Param2_name", "Param2_value"),
          "Strategy", "Intervention")
  #strategy name NA

  # the parameter is not obtained from a distribution
  expect_error(get_parameter_def_distribution("cost_IT", file,
                                          c("Param1_name", "Param1_value")))
  #strategy name null
  expect_error(get_parameter_def_distribution("b", file,
                                        c("Param1_name", "Param1_value"),
                                        "Strategy", NULL))
  #strategy name NA
  expect_error(get_parameter_def_distribution("or", file,
                  c("Param1_name", "Param1_value"),
                  "Strategy", "Intervention"))


  # error while reading a parameter where file is not given
  expect_error(get_parameter_def_distribution("rr", NULL, NULL))

  # error while reading a parameter where no file exists
  expect_error(get_parameter_def_distribution("rr2", nofile, NULL))

  diff_file <- system.file("extdata", "blank.csv", package = "packDAMipd")

  # error while reading a parameter where distribution do not exists
  expect_error(get_parameter_def_distribution("rr2", diff_file, NULL))

  nocol_parameter <- system.file("extdata", "nocol_parameter.csv",
                                 package = "packDAMipd")

  # error while reading a parameter where no Parameter column exists
  expect_error(get_parameter_def_distribution("rr", nocol_parameter, NULL))


  table_param_distrb_no <- system.file("extdata", "table_param_distrb_no.csv",
                          package = "packDAMipd")
  # error while reading a parameter where no distribution column exists
  # but no value for it
  expect_error(get_parameter_def_distribution("rr", table_param_distrb_no,
                                              NULL))
  expect_error(get_parameter_def_distribution("rr3", table_param_distrb_no,
                                              NULL))
  expect_error(get_parameter_def_distribution("rr4", table_param_distrb_no,
                                              NULL))

  # error column names for distribution parameters not given or can not be null
  expect_error(get_parameter_def_distribution("rr", file, NULL))
  expect_error(get_parameter_def_distribution("rr", file, NULL, NULL))
  # error column names for distribution parameters not same as in the file
  expect_error(get_parameter_def_distribution("rr", file, c("Param")))
  # Now the column of parameter for distribution is ok, but but column name for
  # value is missing
  expect_error(get_parameter_def_distribution("rr", file, c("Param1_name")))
  # Now the column of parameter for distribution is ok, but column name for
  # value is wrong
  expect_error(get_parameter_def_distribution("rr", file, c("Param1_name"),
                                              c("paaram2")))
  # both column names for parameter name and value are given no error here
  get_parameter_def_distribution("rr", file, c("Param1_name", "Param1_value"))

  value_error <- system.file("extdata", "table_param_value_error.csv",
                             package = "packDAMipd")
  # both column names for parameter name and value are given, but value is
  # not numeric
  expect_error(get_parameter_def_distribution("rr", value_error,
                                      c("Param1_name", "Param1_value")))


  table_param_name_error <- system.file("extdata",
                                        "table_param_name_error.csv",
                                        package = "packDAMipd")
  # both column name for param name should have name
  expect_error(get_parameter_def_distribution("rr", table_param_name_error,
                                              c("Param1", "Param1_value")))

  file <- system.file("extdata", "table_param_nostrategycol.csv",
                      package = "packDAMipd")
  # strategy col not in the data file
  expect_error(get_parameter_def_distribution("b", file,
                        c("Param1_name", "Param1_value"), "Strategy", NULL))
})
###############################################################################
test_that("get parameter from estimated regression", {
  datafile <- system.file("extdata", "binary.csv", package = "packDAMipd")
  mydata <- read.csv(datafile)
  # No parameter to be estimated given - error
  expect_error(get_parameter_estimated_regression(
    NA, mydata,
    "logistic regression", "gre", NA, "binomial", c("gpa", "rank")
  ))
  # No data is sent -error
  expect_error(get_parameter_estimated_regression(
    "admit", NA,
    "logistic regression", "gre", NA, "binomial", c("gpa", "rank")
  ))
  expect_error(get_parameter_estimated_regression(
    "admit", NULL,
    "logistic regression", "gre", NA, "binomial", c("gpa", "rank")
  ))
  # No independent variable is sent -error
  expect_error(get_parameter_estimated_regression(
    "admit", mydata,
    "logistic regression", NA, NA, "binomial", c("gpa", "rank")
  ))
  # No information on method for regression - error
  expect_error(get_parameter_estimated_regression(
    "admit", mydata,
    NA, "gre", NA, "binomial", c("gpa", "rank")
  ))
  # No columns for the variables needed - error
  file <- system.file("extdata", "table_param.csv", package = "packDAMipd")
  expect_error(get_parameter_estimated_regression(
    "admit", file,
    "logistic regression", "gre", NA, "binomial", c("gpa", "rank")
  ))

  # testing for linear regression
  datafile <- system.file("extdata", "binary.csv", package = "packDAMipd")
  mydata <- read.csv(datafile)
  results <- get_parameter_estimated_regression("gre", data = mydata,
                                    method = "linear",
                                    info_get_method = NULL, indep_var = "gpa",
                                    covariates = NA,
                                    interaction = FALSE)
  expect_equal(results$summary$coefficients[1], 192.3042, tol = 1e-4)
  # testing logistic regression
  results <- get_parameter_estimated_regression("admit", data = mydata,
                                  method = "logistic",
                                  info_get_method = "NULL", indep_var = "gre",
                                  info_distribution = "binomial",
                                  covariates = NA,
                                  interaction = FALSE, naaction = "na.omit",
                                  link = NA)
  expect_equal(results$summary$coefficients[1], -2.9013, tol = 1e-4)
  # testing glm model generalised linear model
  results <- get_parameter_estimated_regression("admit", data = mydata,
                                               method = "generalised linear",
                                               info_get_method = "NULL",
                                               indep_var = "gre",
                                               info_distribution = "binomial",
                                               covariates = NA,
                                               interaction = FALSE,
                                               naaction = "na.omit",
                                               link = NA, )
  expect_equal(results$summary$coefficients[1], -2.9013, tol = 1e-4)

  # testing linear mixed model
  datafile <- system.file("extdata", "lmmdata.csv", package = "packDAMipd")
  dataset <- read.csv(datafile)
  results1 <- get_parameter_estimated_regression("extro", data = dataset,
                                               method = "linear mixed effect",
                                               indep_var = NULL,
                                               info_get_method = NULL,
                                               fix_eff =
                                                 c("open", "agree", "social"),
                                               fix_eff_interact_vars = NULL,
                                               random_intercept_vars =
                                                 c("school", "class"),
                                               nested_intercept_vars_pairs =
                                                 list(c("school", "class")),
                                               cross_intercept_vars_pairs =
                                                 NULL,
                                               uncorrel_slope_intercept_pairs =
                                                 NULL,
                                               random_slope_intercept_pairs =
                                                 NULL, cluster_var = NA,
                                               package_mixed_model = NA
                                               )

 expect_equal(results1$summary$coefficients[1], 60.2400412163, tol = 1e-4)

 datafile <- system.file("extdata", "glmm_data.csv", package = "packDAMipd")
 dataset <- read.csv(datafile)

 results1 <- get_parameter_estimated_regression("remission", data = dataset,
                                            method = "generalised mixed effect",
                                            indep_var = NULL,
                                            info_get_method = NULL,
                                            info_distribution = "binomial",
                                            fix_eff = NULL,
                                            fix_eff_interact_vars = NULL,
                                            random_intercept_vars = c("DID"),
                                            nested_intercept_vars_pairs =
                                                NULL,
                                            cross_intercept_vars_pairs =
                                                NULL,
                                            uncorrel_slope_intercept_pairs =
                                                NULL,
                                              random_slope_intercept_pairs =
                                                NULL, cluster_var = NA,
                                            package_mixed_model = NA)

 expect_equal(results1$summary$coefficients[1], -1.412, tol = 1e-3)

 datafile <- system.file("extdata", "sureg_data.csv", package = "packDAMipd")
 dataset <- read.csv(datafile, stringsAsFactors = TRUE)
 results <- get_parameter_estimated_regression("read", data = dataset,
                                               indep_var = "female",
                                               method = "bivariate",
                                               info_get_method = NULL,
                                               covariates = c("ses", "socst"),
                                               covariates2 =
                                                 c("ses", "science"),
                                               param2_to_be_estimated = "math",
                                               interaction = FALSE,
                                               interaction2 = FALSE)
 expect_equal(results$summary$coefficients[1], 23.146, tol = 1e-4)

 data_for_survival <- survival::aml
 data_for_survival <- na.omit(data_for_survival)
 re <- get_parameter_estimated_regression("status", data = data_for_survival,
                                    method = "survival",
                                    indep_var = "x",
                                    info_get_method = "parametric",
                                    info_distribution = "weibull",
                                    covariates = NA, timevar_survival = "time")

 expect_equal(unname(re$summary$coefficients)[1], 4.109, tol = 1e-3)

})
###############################################################################
test_that("get parameter using linear regression", {
  datafile <- system.file("extdata", "binary.csv", package = "packDAMipd")
  mydata <- read.csv(datafile)
  result <- use_linear_regression("admit", dataset = datafile,
                                  indep_var = "gpa",
                                     covariates = NA, interaction = FALSE)
  expect_equal(unname(result$model_coeff[1]), -0.4223, tol = 1e-3)


  datafile <- system.file("extdata", "lm_dat.csv", package = "packDAMipd")
  mydata <- read.csv(datafile, stringsAsFactors = TRUE)
  result <- use_linear_regression("expression", dataset = mydata,
                                 indep_var = "treatment",
                                 covariates = "temperature",
                                 interaction = FALSE)
  expect_equal(unname(result$model_coeff[1]), -8.0714, tol = 1e-3)

  # Error - no  parameter to be estimated
  expect_error(use_linear_regression(NA, dataset = mydata, indep_var = "gpa",
                                     covariates = NA, interaction = FALSE
  ))
  # Error - no dataset
  expect_error(use_linear_regression("gre", dataset = NULL, indep_var = "gpa",
                                     covariates = NA, interaction = FALSE
  ))
  # Error - no independent variable provided
  expect_error(use_linear_regression("gre",
                                     dataset = mydata, covariates = NA,
                                     indep_var = NA,
                                     interaction = FALSE
  ))
})
###############################################################################
test_that("get parameter using generalised linear model", {
  datafile <- system.file("extdata", "binary.csv", package = "packDAMipd")
  mydata <- read.csv(datafile)
  results_logit <- use_generalised_linear_model("admit", dataset = mydata,
                                indep_var = "gre", family = "binomial",
                                covariates = NA, interaction = FALSE,
                                naaction = "na.omit", link = NA
  )
  expect_equal(unname(results_logit$model_coeff[1]), -2.9013, tol = 1e-4)

  results_logit <- use_generalised_linear_model("admit", dataset = datafile,
                                                indep_var = "gre",
                                                family = "binomial",
                                                covariates = NA,
                                                interaction = FALSE,
                                                naaction = "na.omit",
                                                link = NA
  )
  expect_equal(unname(results_logit$model_coeff[1]), -2.9013, tol = 1e-4)

  #
  # Error - no parameter given for estimation
  expect_error(use_generalised_linear_model(NA, mydata,
                                            indep_var = "gre",
                                            family = "binomial",
                                            covariates = NA,
                                            interaction = FALSE,
                                            naaction = "na.omit",
                                            link = NA
  ))
  # Error - no dataset given
  expect_error(use_generalised_linear_model("admit", dataset = NULL,
                              indep_var = "gre", family = "binomial",
                              covariates = NA,
                              interaction = FALSE, naaction = "na.omit",
                              link = NA
  ))
  # Error - no independent variable given
  expect_error(use_generalised_linear_model("admit", dataset = mydata,
                              indep_var = NA, family = "binomial",
                              covariates = NA,
                              interaction = FALSE,
                              naaction = "na.omit", link = NA
  ))
  # Error - no family given for the distribution
  expect_error(use_generalised_linear_model("admit",
                                            dataset = mydata,
                                            indep_var = "gre", family = NA,
                                            covariates = NA,
                                            interaction = FALSE,
                                            naaction = "na.omit", link = NA
  ))
  # Error - given link is not sutiable for the given family
  expect_error(use_generalised_linear_model("admit",
                                            dataset = mydata,
                                            indep_var = "gre",
                                            family = "binomial",
                                            covariates = NA,
                                            interaction = FALSE,
                                            naaction = "na.omit",
                                            link = "identity"
  ))
  results <- use_generalised_linear_model("admit",
                               dataset = mydata,
                               indep_var = "gre", family = "binomial",
                               covariates = "gpa",
                               interaction = FALSE,
                               naaction = "na.omit", link = "logit"
  )
  expect_equal(unname(results$model_coeff[1]), -4.9493, tol = 1e-3)
})
###############################################################################
test_that("get parameter using mixed effect regression", {
  datafile <- system.file("extdata", "lmmdata.csv", package = "packDAMipd")
  dataset <- read.csv(datafile)
  result <- use_linear_mixed_model("extro",
                         dataset = dataset,
                         fix_eff = c("open", "agree", "social"),
                         fix_eff_interact_vars = NULL,
                         random_intercept_vars = c("school", "class"),
                         nested_intercept_vars_pairs =
                           list(c("school", "class")),
                         cross_intercept_vars_pairs = NULL,
                         uncorrel_slope_intercept_pairs = NULL,
                         random_slope_intercept_pairs = NULL,
                         package_mixed_model = "lme4"
  )
  expect_equal((result$summary$coefficients[1]), 60.240, tol = 1e-4)
  result <- use_linear_mixed_model("extro",
                                   dataset = dataset,
                                   fix_eff = c("open", "agree", "social"),
                                   fix_eff_interact_vars = NA,
                                   random_intercept_vars = c("school", "class"),
                                   nested_intercept_vars_pairs = NA,
                                   cross_intercept_vars_pairs = NA,
                                   uncorrel_slope_intercept_pairs = NA,
                                   random_slope_intercept_pairs = NA,
                                   package_mixed_model = "lme4"
  )
  expect_equal((result$summary$coefficients[1]), 60.2, tol = 1e-2)
  result <- use_linear_mixed_model("extro",
                                   dataset = dataset,
                                   fix_eff = c("open", "agree", "social"),
                                   fix_eff_interact_vars = NA,
                                   random_intercept_vars = c("school", "class"),
                                   nested_intercept_vars_pairs = NA,
                                   cross_intercept_vars_pairs = NA,
                                   uncorrel_slope_intercept_pairs = NA,
                                   random_slope_intercept_pairs = NA,
                                   package_mixed_model = "nlme"
  )
  expect_equal((result$summary$coefficients[1]), 60.2, tol = 1e-2)
  result <- use_linear_mixed_model("extro",
                                  dataset = datafile,
                                  fix_eff = c("open", "agree", "social"),
                                  fix_eff_interact_vars = NULL,
                                  random_intercept_vars =
                                    c("school", "class"),
                                  nested_intercept_vars_pairs =
                                    list(c("school", "class")),
                                  cross_intercept_vars_pairs = NULL,
                                  uncorrel_slope_intercept_pairs = NULL,
                                  random_slope_intercept_pairs = NULL,
                                  package_mixed_model = "lme4"
  )
  expect_equal(unname(result$summary$coefficients[1]), 60.240, tol = 1e-4)

  # Error - parameter to be estimated not found
  expect_error(use_linear_mixed_model(NA,
                                    dataset = dataset,
                                    fix_eff = c("open", "agree", "social"),
                                    fix_eff_interact_vars = NULL,
                                    random_intercept_vars =
                                      c("school", "class"),
                                    nested_intercept_vars_pairs =
                                      list(c("school", "class")),
                                    cross_intercept_vars_pairs = NULL,
                                    uncorrel_slope_intercept_pairs = NULL,
                                    random_slope_intercept_pairs = NULL,
                                    package_mixed_model = "lme4"
  ))
 # Error -dataset should not be null
  expect_error(use_linear_mixed_model("extro",
                                    dataset = NULL,
                                    fix_eff = c("open", "agree", "social"),
                                    fix_eff_interact_vars = NULL,
                                    random_intercept_vars =
                                      c("school", "class"),
                                    nested_intercept_vars_pairs =
                                      list(c("school", "class")),
                                    cross_intercept_vars_pairs = NULL,
                                    uncorrel_slope_intercept_pairs = NULL,
                                    random_slope_intercept_pairs = NULL,
                                    package_mixed_model = "lme4"
  ))
  result <- use_linear_mixed_model("extro",
                                    dataset = dataset,
                                    fix_eff = NULL,
                                   fix_eff_interact_vars = NULL,
                                    random_intercept_vars =
                                     c("school", "class"),
                                    nested_intercept_vars_pairs =
                                     list(c("school", "class")),
                                    cross_intercept_vars_pairs = NULL,
                                    uncorrel_slope_intercept_pairs = NULL,
                                    random_slope_intercept_pairs = NULL,
                                   package_mixed_model = "lme4"
 )
 expect_equal(result$summary$coefficients[1], 60.26744, tol = 1e-4)

 result <- use_linear_mixed_model("extro",
                                 dataset = dataset,
                                 fix_eff = NULL,
                                 fix_eff_interact_vars = NULL,
                                 random_intercept_vars =
                                   c("school", "class"),
                                 nested_intercept_vars_pairs = NULL,
                                 cross_intercept_vars_pairs =
                                   list(c("school", "class")),
                                 uncorrel_slope_intercept_pairs = NULL,
                                 random_slope_intercept_pairs = NULL,
                                 package_mixed_model = "lme4"
 )
 expect_equal(result$summary$coefficients[1], 60.26744, tol = 1e-4)

 # Error - Random intercept  variable should be provided
 expect_error(use_linear_mixed_model("extro",
                                    dataset = dataset,
                                    fix_eff = c("open"),
                                    fix_eff_interact_vars = NULL,
                                    random_intercept_vars = NULL,
                                    nested_intercept_vars_pairs =
                                      list(c("school", "class")),
                                    cross_intercept_vars_pairs = NULL,
                                    uncorrel_slope_intercept_pairs = NULL,
                                    random_slope_intercept_pairs = NULL,
                                    package_mixed_model = "lme4"
))
 datafile <- system.file("extdata", "glmm_data.csv", package = "packDAMipd")
 dataset <- read.csv(datafile)


 result <- use_linear_mixed_model("remission", dataset = datafile,
                                     fix_eff = "Age",
                                     fix_eff_interact_vars = NULL,
                                     random_intercept_vars =
                                    c("FamilyHx", "Married"),
                                     nested_intercept_vars_pairs = NULL,
                                    cross_intercept_vars_pairs =
                                      list(c("FamilyHx", "Married")),
                                    uncorrel_slope_intercept_pairs = NULL,
                                    random_slope_intercept_pairs =
                                      list(c("Age", "Married")),
                                  package_mixed_model = "lme4")
 expect_equal(result$summary$coefficients[1], 0.62, tol = 1e-2)

})
###############################################################################
test_that("get parameter using generalised linear mixed model", {
  datafile <- system.file("extdata", "culcita_data.csv",
                          package = "packDAMipd")
  dataset <- read.csv(datafile)
  results1 <- use_generalised_linear_mixed_model("predation", dataset = dataset,
                                                fix_eff = c("ttt"),
                                                family = "binomial",
                                                fix_eff_interact_vars = NULL,
                                                random_intercept_vars =
                                                  c("block"),
                                                nested_intercept_vars_pairs =
                                                  NULL,
                                                cross_intercept_vars_pairs =
                                                  NULL,
                                                uncorrel_slope_intercept_pairs =
                                                  NULL,
                                                random_slope_intercept_pairs =
                                                  NULL,
                                                link = NA,
                                                package_mixed_model = "lme4")

  expect_equal(unname(results1$fit_diagnostics[1]), 70.3, tol = 1e-3)

  results1 <- use_generalised_linear_mixed_model("predation", dataset = dataset,
                                                 fix_eff = c("ttt"),
                                                 family = "binomial",
                                                 fix_eff_interact_vars = NA,
                                                 random_intercept_vars =
                                                   c("block"),
                                                 nested_intercept_vars_pairs =
                                                   NA,
                                                 cross_intercept_vars_pairs =
                                                   NA,
                                            uncorrel_slope_intercept_pairs =
                                                   NA,
                                            random_slope_intercept_pairs =
                                                   NA,
                                                 link = NA,
                                                 package_mixed_model = "lme4")

  expect_equal(unname(results1$fit_diagnostics[1]), 70.3, tol = 1e-3)

  results1 <- use_generalised_linear_mixed_model("predation",
                                                 dataset = datafile,
                                          fix_eff = c("ttt"),
                                                family = "binomial",
                                                fix_eff_interact_vars = NULL,
                                                random_intercept_vars =
                                                  c("block"),
                                                nested_intercept_vars_pairs =
                                                  NULL,
                                         cross_intercept_vars_pairs =
                                                  NULL,
                                          uncorrel_slope_intercept_pairs =
                                                  NULL,
                                                random_slope_intercept_pairs =
                                                  NULL,
                                                link = NA,
                                         package_mixed_model = "lme4")

  expect_equal(unname(results1$fit_diagnostics[1]), 70.3, tol = 1e-3)
  results1 <- use_generalised_linear_mixed_model("predation",
                                                 dataset = datafile,
                                                 fix_eff = c("ttt"),
                                                 family = "binomial",
                                                 fix_eff_interact_vars = NULL,
                                                 random_intercept_vars =
                                                   c("block"),
                                                 nested_intercept_vars_pairs =
                                                   NULL,
                                                 cross_intercept_vars_pairs =
                                                   NULL,
                                              uncorrel_slope_intercept_pairs =
                                                   NULL,
                                              random_slope_intercept_pairs =
                                                   NULL,
                                                 link = NA,
                                                 package_mixed_model = "nlme")

  expect_equal(unname(results1$fit_diagnostics[1]), 70.3, tol = 1e-3)

  datafile <- system.file("extdata", "glmm_data.csv", package = "packDAMipd")
  dataset <- read.csv(datafile)

  # Error - parameter to be estimated not found
  expect_error(use_generalised_linear_mixed_model(NA, dataset = datafile,
                                     fix_eff = "Age",
                                     family = "binomial",
                                     fix_eff_interact_vars = NULL,
                                     random_intercept_vars = c("DID"),
                                     nested_intercept_vars_pairs = NULL,
                                     cross_intercept_vars_pairs = c("DID"),
                                     uncorrel_slope_intercept_pairs = NULL,
                                     random_slope_intercept_pairs = NULL,
                                     link = NA, package_mixed_model = "lme4"))
  # Error -dataset should not be null
  expect_error(use_generalised_linear_mixed_model("remission", dataset = NULL,
                                     fix_eff = "Age",
                                     family = "binomial",
                                     fix_eff_interact_vars = NULL,
                                     random_intercept_vars = c("DID"),
                                     nested_intercept_vars_pairs = NULL,
                                     cross_intercept_vars_pairs = c("DID"),
                                     uncorrel_slope_intercept_pairs = NULL,
                                     random_slope_intercept_pairs = NULL,
                                     link = NA, package_mixed_model = "lme4"))

  # Error - Random intercept  variable should be provided
  expect_error(use_generalised_linear_mixed_model("remission",
                                                  dataset = dataset,
                                     fix_eff = "Age",
                                     family = "binomial",
                                     fix_eff_interact_vars = NULL,
                                     random_intercept_vars = NULL,
                                     nested_intercept_vars_pairs = NULL,
                                     cross_intercept_vars_pairs = c("DID"),
                                     uncorrel_slope_intercept_pairs = NULL,
                                     random_slope_intercept_pairs = NULL,
                                     link = NA, package_mixed_model = "lme4"))

})

###############################################################################
test_that("Bivaraite regression for correlated values", {
  datafile <- system.file("extdata", "hsb2.dta", package = "packDAMipd")
  mydata <- haven::read_dta(datafile)
  results_sureg <- use_seemingly_unrelated_regression("read", "math",
    dataset = mydata,
    indep_var = "female", covariates1 = c("as.numeric(ses)", "socst"),
    covariates2 = c("as.numeric(ses)", "science"), interaction1 = FALSE,
    interaction2 = FALSE
  )
  results_sureg <- use_seemingly_unrelated_regression("read", "math",
                                                      dataset = datafile,
                                                      indep_var = "female",
                                                      covariates1 =
                                                        c("as.numeric(ses)",
                                                          "socst"),
                                                      covariates2 =
                                                        c("as.numeric(ses)",
                                                          "science"),
                                                      interaction1 = FALSE,
                                                      interaction2 = FALSE
  )
  # Error - need parameter 1
  expect_error(use_seemingly_unrelated_regression(NA, "math",
    dataset = mydata,
    indep_var = "female", covariates1 = c("as.numeric(ses)", "socst"),
    covariates2 = c("as.numeric(ses)", "science"), interaction1 = FALSE,
    interaction2 = FALSE
  ))
  # Error - need parameter 2
  expect_error(use_seemingly_unrelated_regression("read", NA,
    dataset = mydata,
    indep_var = "female", covariates1 = c("as.numeric(ses)", "socst"),
    covariates2 = c("as.numeric(ses)", "science"), interaction1 = FALSE,
    interaction2 = FALSE
  ))
  # Error - need dataset
  expect_error(use_seemingly_unrelated_regression("read", "math",
    dataset = NULL,
    indep_var = "female", covariates1 = c("as.numeric(ses)", "socst"),
    covariates2 = c("as.numeric(ses)", "science"), interaction1 = FALSE,
    interaction2 = FALSE
  ))
  # Error - need independent variable
  expect_error(use_seemingly_unrelated_regression("read", "math",
    dataset = mydata,
    indep_var = NA, covariates1 = c("as.numeric(ses)", "socst"),
    covariates2 = c("as.numeric(ses)", "science"), interaction1 = FALSE,
    interaction2 = FALSE
  ))
})
