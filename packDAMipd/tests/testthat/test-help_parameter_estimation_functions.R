###############################################################################
test_that("find the keyword for random number generation", {
  expect_equal(find_keyword_rand_generation("gamma"), "rgamma")
  expect_equal(find_keyword_rand_generation("expo"), "rexp")
  expect_equal(find_keyword_rand_generation("normal"), "rnorm")
  expect_equal(find_keyword_rand_generation("lognormal"), "rlnorm")
  expect_equal(find_keyword_rand_generation("binomial"), "rbinom")
  expect_equal(find_keyword_rand_generation("beta"), "rbeta")
  expect_equal(find_keyword_rand_generation("uniform"), "runif")
  expect_equal(find_keyword_rand_generation("weibull"), "rweibull")
  expect_equal(find_keyword_rand_generation("poisson"), "rpois")
  # error multi normal is not a keyword
  expect_error(find_keyword_rand_generation("multi normal"))
  # text can not be empty
  expect_error(find_keyword_rand_generation(" "))
  # text can not be null or NA
  expect_error(find_keyword_rand_generation(NA))
  expect_error(find_keyword_rand_generation(NULL))
})
###############################################################################
test_that("find the required parameters for given distribution", {
  expect_equal(find_required_parameter_combs("gamma"), c("shape", "rate"))
  expect_equal(find_required_parameter_combs("binomial"), c("size", "prob"))
  this_list <- list(c("meanlog", "sdlog"), c("mean", "sd"), c("mean", "sdlog"),
                   c("meanlog", "sd"))
  expect_equal(find_required_parameter_combs("lognormal"), this_list)
  expect_equal(find_required_parameter_combs("beta"), c("shape1", "shape2"))
  expect_equal(find_required_parameter_combs("uniform"), c("min", "max"))
  expect_equal(find_required_parameter_combs("gaussian"), c("mean", "sd"))
  expect_equal(find_required_parameter_combs("weibull"), c("shape", "scale"))
  expect_equal(find_required_parameter_combs("expo"), c("rate"))
  expect_equal(find_required_parameter_combs("poisson"), c("lambda"))

  # error no suitable prob distribution found
  expect_error(find_required_parameter_combs("multi normal"))

  # name of the distribuion can not be null or NA
  expect_error(find_required_parameter_combs(NA))
  expect_error(find_required_parameter_combs(NULL))
})
###############################################################################
test_that("get the name and value of parameters in the prob distribution", {
  ans <- get_name_value_probdistrb_def("gamma(mean = 10, sd =1)")
  expect_equal(ans$params, c("mean", "sd"))
  expect_equal(ans$values, c(10, 1))
  ans <- get_name_value_probdistrb_def("gamma(mean = sqrt(2), b =17)")
  expect_equal(ans$params, c("mean", "b"))
  expect_equal(ans$values, c(sqrt(2), 17))
  # error as it is not separated by comma-
  expect_error(get_name_value_probdistrb_def("gamma(shape =1 & scale =1)"))
  # eror the expression has no commas
  expect_error(get_name_value_probdistrb_def("sheeja"))
  # error more than one equal sign
  expect_error(
    get_name_value_probdistrb_def("gamma(shape = 1 & scale =1, r=2)"))
  # expression can not be null or NA
  expect_error(get_name_value_probdistrb_def(NA))
  expect_error(get_name_value_probdistrb_def(NULL))

  ans <- get_name_value_probdistrb_def(
    "gamma(mean = 10, sd =1, error = 1)")
  expect_equal(ans$params[1], "mean")
  expect_error(get_name_value_probdistrb_def("gamma(shape = 1 ,
                                             scale = sqrt(one))"))
})

###############################################################################
test_that("find keyword regression", {
  expect_error(find_keyword_regression_method(NULL))
  expect_error(find_keyword_regression_method(NA))
  expect_equal(find_keyword_regression_method("linear"), "lm")
  expect_equal(find_keyword_regression_method("logistic"), "glm")
  expect_equal(find_keyword_regression_method("multilevel modelling"), "lmer")
  expect_error(find_keyword_regression_method("survival", NULL))
  expect_error(find_keyword_regression_method("survival", NA))
  expect_equal(find_keyword_regression_method("survival", "km"),
               "survival::survfit")
  expect_equal(find_keyword_regression_method("survival", "coxph"),
               "survival::coxph")
  expect_equal(find_keyword_regression_method("survival", "parametric"),
               "flexsurv::flexsurvreg")
  expect_error(find_keyword_regression_method("poisson"))
})
###############################################################################
test_that("get check estimate required params", {
  ans <- check_estimate_required_params("gamma(mean = 10 ,sd=1)", "gamma")
  expect_equal(ans$shape, 100)
  expect_equal(ans$rate, 10)
  # Error - gamma distribution but parameters are not mean and sd
  expect_error(check_estimate_required_params("gamma(mean = sqrt(2), b = 17)",
                                              "gamma"))
  # Error - distribution can not be null or NA
  expect_error(check_estimate_required_params("gamma(mean = sqrt(2), sd = 17)",
                                              NA))
  # Error - expression can not be NA or NULL
  expect_error(check_estimate_required_params(NULL, "gamma"))
  expect_error(check_estimate_required_params(NA, "gamma"))
  expect_error(check_estimate_required_params("gamma(mean = 10 ,sd=1)", NULL))
  answer <-  check_estimate_required_params("beta(mean = 10 ,sd=1)", "beta")
  expect_equal(answer, NA)
})
###############################################################################
test_that("check estimate and substitute proper params", {
  expect_error(check_estimate_substitute_proper_params("poisson(mean = 10,
                                                       sd=1)"))
  ans <- check_estimate_substitute_proper_params("gamma(mean = 10, sd=1)")
  expect_equal(ans, "rgamma(1, shape = 100, rate = 10)")
  ans <- check_estimate_substitute_proper_params("gamma(shape = 10, rate = 1)")
  expect_equal(ans, "rgamma(1, shape = 10, rate = 1)")
  # Error - expression can not be NA or NULL
  expect_error(check_estimate_substitute_proper_params(NULL))
  expect_error(check_estimate_substitute_proper_params(NA))
  # Error - expression should have bracket
  expect_error(check_estimate_substitute_proper_params("sheeja"))
  # Error - gamma distribution but parameters are not mean and sd
  expect_error(check_estimate_substitute_proper_params("gamma(mean = sqrt(2),
                                                       b = 17)"))
  # Error - gamma distribution but parameters are not mean and sd
  expect_error(check_estimate_substitute_proper_params("gamma(mean = sqrt(2),
                                                       sd = e)"))
})
###############################################################################
test_that("get the extension of a filename", {
  expect_equal(get_extension_file("data.txt"), "txt")
  expect_equal(get_extension_file("data.jpg"), "jpg")
  # error no extension given after the "." sign
  expect_error(get_extension_file("data"))
})
###############################################################################
test_that("loading a datafile", {
  load_trial_data(system.file("extdata", "trial_data.csv",
                              package = "packDAMipd"))
  #  file name given as NA (Null is the default and it will upload
  # default trial data)
  expect_error(load_trial_data(NA))
  #  Error - file doesn't exists
  expect_error(load_trial_data("blank"))
  df <- load_trial_data(system.file("extdata", "data.txt",
                                    package = "packDAMipd"))
  expect_equal(df$age, c(44, 30, 21, 43))
  df <- load_trial_data(system.file("extdata", "hsb2.dta",
                                    package = "packDAMipd"))
  expect_equal(mean(df$id), 100.5, tol = 1e-2)
  df <- load_trial_data(NULL)
  expect_equal(mean(df$age), 57.5, tol = 1e-3)

  ref_cost_data_file <- system.file("extdata", "test.xlsx",
                                    package = "packDAMipd")

  df <- load_trial_data(ref_cost_data_file, "EL")
  expect_equal(df$Currency_Code[1], "AA22C")

  file <- system.file("extdata", "eq5d.RDS", package = "packDAMipd")
  df1 <- load_trial_data(file)

  expect_equal(length(df1$ID), 13)
  expect_error(load_trial_data(system.file("extdata", "eq5d.rda",
                                    package = "packDAMipd")))
})
###############################################################################
test_that("making a string of covariates", {
  expect_equal(make_string_covariates(c("open", "grade")), "open + grade")
  expect_error(make_string_covariates(NULL))
  expect_error(make_string_covariates(NA), NA)
})
###############################################################################
context("testing  form expression for glm")
test_that("testing form expression for glm", {
  formula <- form_expression_glm("admit",
                                 indep_var = "gre", family = "binomial",
                                 covariates = c("gpa", "rank"),
                                 interaction = FALSE, naaction = "na.omit",
                                 link = "logit")
  # Error - parameter to be estimated should not be NULL or NA
  expect_error(form_expression_glm(NA, "gre", "binomial", covariates = NA,
                                   interaction = FALSE,
                                   naaction = "na.omit", link = NA))
  expect_error(form_expression_glm(NULL, "gre", "binomial", covariates = NA,
                                   interaction = FALSE, naaction = "na.omit",
                                   link = NA))
  # Error - independent variable should not be NULL or NA
  expect_error(form_expression_glm("admit", NA, "binomial", covariates = NA,
                                   interaction = FALSE, naaction = "na.omit",
                                   link = NA))
  expect_error(form_expression_glm("admit", NULL, "binomial", covariates = NA,
                                   interaction = FALSE, naaction = "na.omit",
                                   link = NA))
  # Error - distribution should not be NULL or NA
  expect_error(form_expression_glm("admit", "gre", NA, covariates = NA,
                                   interaction = FALSE, naaction = "na.omit",
                                   link = NA))
  # Error - interaction should not be NULL or NA
  expect_error(form_expression_glm("admit", "gre", "binomial", covariates = NA,
                                   interaction = NA, naaction = "na.omit",
                                   link = NA))
  # Error - interaction should not be NULL or NA
  expect_error(form_expression_glm("admit", "gre", "binomial", covariates = NA,
                                   interaction = NULL, naaction = "na.omit",
                                   link = NA))
  expect_error(form_expression_glm("admit",
                                   indep_var = "gre", family = "binomial",
                                   covariates = c("gpa", "rank"),
                                   interaction = FALSE, naaction = "na.omit",
                                   link = "identity"))
  epxr <- form_expression_glm("admit",
                             indep_var = "gre", family = "binomial",
                             covariates = c("gpa", "rank"), interaction = TRUE,
                             naaction = "na.omit",
                             link = "logit")
  expect_identical(epxr$short_formula, " ~ gpa * rank + gre")
})


###############################################################################
context("testing  getting family of distribution for glm")
test_that("testing  getting family of distribution for glm", {
  expect_equal(find_glm_distribution("normal"), "gaussian")
  expect_equal(find_glm_distribution("poisson"), "poisson")
  expect_error(find_glm_distribution("logical"))
  # Error the parameter can not be null
  expect_error(find_glm_distribution(NULL))
  expect_error(find_glm_distribution(NA))
  expect_equal(find_glm_distribution("gamma"), "Gamma")
  expect_equal(find_glm_distribution("inverse gaussian"), "inverse.gaussian")
  expect_equal(find_glm_distribution("quasi binomial"), "quasibinomial")
  expect_equal(find_glm_distribution("quasi poisson"), "quasipoisson")
  expect_equal(find_glm_distribution("quasi"), "quasi")
})


###############################################################################
context("testing  getting link function for the family of distribution for glm")
test_that("testing  getting link function for distribution for glm", {
  expect_equal(check_link_glm("gaussian", "log"), "log")
  expect_equal(check_link_glm("poisson", "identity"), "identity")
  expect_error(check_link_glm("logical", NULL))
  expect_error(check_link_glm("poisson", "probit"))
  # Error the parameter can not be Na or NULL
  expect_error(check_link_glm(NULL, NULL))
  # Error the parameter can not be Na or NULL
  expect_error(check_link_glm("gaussian", NULL))
  #family not na
  expect_error(check_link_glm(NA, "identity"))
  # Error the parameter can not be Na or NULL
  expect_error(check_link_glm("gaussian", NA))

  expect_equal(check_link_glm("Gamma", "identity"), "identity")
  expect_equal(check_link_glm("inverse.gaussian", "identity"), "identity")
  expect_equal(check_link_glm("quasi", "identity"), "identity")
  expect_equal(check_link_glm("quasibinomial", "logit"), "logit")
  expect_equal(check_link_glm("quasipoisson", "logit"), "logit")

})

###############################################################################
context("testing  diagnosis for glm fit")
test_that("testing  diagnosis for glm fit", {
  datafile <- system.file("extdata", "binary.csv", package = "packDAMipd")
  mydata <- read.csv(datafile)
  results_logit <- use_generalised_linear_model("admit", dataset = mydata,
                                                indep_var = "gre",
                                                family = "binomial",
                                                covariates = NA,
                                                interaction = FALSE,
                                                naaction = "na.omit",
                                                link = NA)
  #Error method should be glm
  expect_error(do_diagnostic_glm(NULL, results_logit$fit,
                                 results_logit$expression_recreated,
                                 results_logit$param_to_be_estimated,
                                 mydata, "gre", covariates = NA,
                                 interaction = FALSE))
  #Error method is not glm
  expect_error(do_diagnostic_glm("lm", "fit",
                                 results_logit$expression_recreated,
                                 results_logit$param_to_be_estimated,
                                 mydata, "gre", covariates = NA,
                                 interaction = FALSE))

  #Error "fit" is not glm fit object
  expect_error(do_diagnostic_glm("glm", "fit",
                                 results_logit$expression_recreated,
                                 results_logit$param_to_be_estimated,
                                 mydata, "gre", covariates = NA,
                                 interaction = FALSE))
  # expression created can not be null
  expect_error(do_diagnostic_glm("glm", results_logit$fit, NULL,
                                 results_logit$param_to_be_estimated,
                                 mydata, "gre", covariates = NA,
                                 interaction = FALSE))

  # param_to_be_estimated can not be null
  expect_error(do_diagnostic_glm("glm", results_logit$fit,
                                 results_logit$expression_recreated, NULL,
                                 mydata, "gre", covariates = NA,
                                 interaction = FALSE))

  # dataset can not be null
  expect_error(do_diagnostic_glm("glm", results_logit$fit,
                                 results_logit$expression_recreated, "gre",
                                 NULL, "gre", covariates = NA,
                                 interaction = FALSE))
  # indep_var can not be null
  expect_error(do_diagnostic_glm("glm", results_logit$fit,
                                 results_logit$expression_recreated, "gre",
                                 mydata, NULL, covariates = NA,
                                 interaction = FALSE))

  # interaction can not be null
  expect_error(do_diagnostic_glm("glm", results_logit$fit,
                                 results_logit$expression_recreated, "gre",
                                 mydata, "gre", covariates = NA, NULL))
})
###############################################################################
context("testing find distribution for survreg")
test_that("testing  diagnosis for glm fit", {
  expect_equal(find_survreg_distribution("weibull"), "weibull")
  expect_equal(find_survreg_distribution("expo"), "exponential")
  # Error - text can not be NULL or NA
  expect_error(find_survreg_distribution(NULL))
  expect_error(find_survreg_distribution(NA))
  # Error - distribution not recognised
  expect_error(find_survreg_distribution("NULL"))
  expect_equal(find_survreg_distribution("gaussian"), "gaussian")
  expect_equal(find_survreg_distribution("log gaussian"), "loggaussian")
  expect_equal(find_survreg_distribution("rayleigh"), "rayleigh")
  expect_equal(find_survreg_distribution("logistic"), "logistic")
  expect_equal(find_survreg_distribution("log normal"), "lognormal")
  expect_equal(find_survreg_distribution("log logistic"), "loglogistic")
  expect_equal(find_survreg_distribution("log normal"), "lognormal")
})

###############################################################################
context("testing creating expression for linear regression ")
test_that("testing creating expression for linear regression", {
  formula <- form_expression_lm("gre", indep_var = "gpa", covariates = NA,
                                interaction = FALSE)
  #Error -parameter to be estimated is NULL
  expect_error(form_expression_lm(NULL, indep_var = "gpa", covariates = NA,
                                  interaction = FALSE))
  # Error- independent variable  null
  expect_error(form_expression_lm("gre", indep_var = NULL, covariates = NA,
                                  interaction = FALSE))
  formula <- form_expression_lm("gre", indep_var = "gpa",
                                covariates = c("age", "sex"),
                                interaction = FALSE)
  expect_equal(formula$short_formula, " ~ age + sex + gpa")
  formula <- form_expression_lm("gre", indep_var = "gpa",
                                covariates = c("age", "sex"),
                                interaction = TRUE)
  expect_equal(formula$short_formula, " ~ age * sex + gpa")
})

###############################################################################
context("testing  diagnosis for lm fit")
test_that("testing  diagnosis for lm fit", {
  datafile <- system.file("extdata", "binary.csv", package = "packDAMipd")
  mydata <- read.csv(datafile)
  results_logit <- use_linear_regression("admit", dataset = mydata,
                                         indep_var = "gre", covariates = NA,
                                         interaction = FALSE)
  #fir should be of type lm
  expect_error(
    do_diagnostic_linear_regression("lm", results_logit,
                                          results_logit$expression_recreated,
                                          results_logit$param_to_be_estimated,
                                          mydata, "gre", covariates = NA,
                                          interaction = FALSE))

  #Error method should be lm
  expect_error(do_diagnostic_linear_regression("glm", results_logit$fit,
                                          results_logit$expression_recreated,
                                          results_logit$param_to_be_estimated,
                                          mydata,
                                          "gre", covariates = NA,
                                          interaction = FALSE))
  #Error method should not be null
  expect_error(do_diagnostic_linear_regression(NULL, results_logit$fit,
                                          results_logit$expression_recreated,
                                          results_logit$param_to_be_estimated,
                                          mydata,
                                          "gre", covariates = NA,
                                          interaction = FALSE))
  #Error "fit" is not lm fit object
  expect_error(do_diagnostic_linear_regression("lm", "fit",
                                          results_logit$expression_recreated,
                                          results_logit$param_to_be_estimated,
                                           mydata,
                                          "gre", covariates = NA,
                                          interaction = FALSE))
  # expression created can not be null
  expect_error(do_diagnostic_linear_regression("lm", results_logit$fit, NULL,
                                            results_logit$param_to_be_estimated,
                                             mydata,
                                            "gre", covariates = NA,
                                            interaction = FALSE))

  # param_to_be_estimated can not be null
  expect_error(do_diagnostic_linear_regression("lm", results_logit$fit,
                                               results_logit$fit$call, NULL,
                                               mydata, "gre", covariates = NA,
                                               interaction = FALSE))

  # dataset can not be null
  expect_error(do_diagnostic_linear_regression("lm", results_logit$fit,
                                               results_logit$fit$call, "gre",
                                               NULL, "gre", covariates = NA,
                                               interaction = FALSE))
  # indep_var can not be null
  expect_error(do_diagnostic_linear_regression("lm", results_logit$fit,
                                               results_logit$fit$call, "gre",
                                               mydata, NULL, covariates = NA,
                                               interaction = FALSE))

  # interaction can not be null
  expect_error(do_diagnostic_linear_regression("lm", results_logit$fit,
                                               results_logit$fit$call, "gre",
                                               mydata, "gre", covariates = NA,
                                               interaction = NULL))
})


###############################################################################
context("testing predicting regression")
test_that("testing predicting regression", {
  datafile <- system.file("extdata", "binary.csv", package = "packDAMipd")
  mydata <- read.csv(datafile)
  results_logit <- use_linear_regression("admit", dataset = mydata,
                                         indep_var = "gre", covariates = NA,
                                         interaction = FALSE)
  predict <- prediction_regression("lm", results_logit$fit,
                                   results_logit$fit$call, "admit",
                                   covariates = NA, "gre", FALSE)
  #Error method should be glm or lm
  expect_error(prediction_regression(NULL, results_logit$fit,
                                     results_logit$fit$call, "admit",
                                     covariates = NA, "gre", FALSE))
  expect_error(prediction_regression("fit", results_logit$fit,
                                     results_logit$fit$call, "admit",
                                     covariates = NA, "gre", FALSE))

  #Error "fit" is not lm fit object
  expect_error(prediction_regression("lm", "fit",
                                     results_logit$fit$call, "admit",
                                     covariates = NA, "gre", FALSE))
  # expression created can not be null
  expect_error(prediction_regression("lm",
                                     results_logit$fit, NULL, "admit",
                                     covariates = NA, "gre", FALSE))
  # param_to_be_estimated can not be null
  expect_error(prediction_regression("lm",
                                     results_logit$fit,
                                     results_logit$expression_recreated,
                                     NULL,
                                     covariates = NA, "gre", FALSE))
  # indep_var can not be null
  expect_error(prediction_regression("lm", results_logit$fit,
                                     results_logit$expression_recreated,
                                     "admit",
                                     covariates = NA, NULL, FALSE))

  # interaction can not be null
  expect_error(prediction_regression("lm", results_logit$fit,
                                     results_logit$expression_recreated,
                                     "admit",
                                     covariates = NA, "gre", NULL))
  results_lm <- use_linear_regression("mpg",
                                      dataset = mtcars,
                                      indep_var = "disp",
                                      covariates = c("hp", "wt", "drat"),
                                      interaction = FALSE)

  pred <- prediction_regression("lm", results_lm$fit,
                                results_lm$expression_recreated, "admit",
                                covariates = c("hp", "wt", "drat"),
                                "gre", FALSE)


})

###############################################################################
context("testing forming expression for mixed model")
test_that("testing forming expression for mixed model", {
  datafile <- system.file("extdata", "binary.csv", package = "packDAMipd")
  mydata <- read.csv(datafile)
  # nested intercept
  formula <- form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         cross_intercept_vars_pairs = NULL,
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs = NULL,
                                         family = "binomial", link = "logit"
  )
  this_formula <- "lme4::lmer(extro ~ open +  agree +  social +   ( 1 | school / class ) , family = binomial(link = logit), data = dataset)"
  expect_equal(formula, this_formula)

  expect_error(form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars = NULL,
                                         nested_intercept_vars_pairs = NULL,
                                         cross_intercept_vars_pairs = NULL,
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs = NULL,
                                         family = "binomial", link = "logit"
  ))

  formula <- form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         cross_intercept_vars_pairs = NULL,
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs = NULL,
                                         family = "binomial", link = NA
  )
  this_formula <- "lme4::lmer(extro ~ open +  agree +  social +   ( 1 | school / class ) , family = binomial, data = dataset)"
  expect_equal(formula, this_formula)

  formula <- form_expression_mixed_model_lme4("extro",
                                              dataset = mydata,
                                fix_eff = c("open", "agree", "social"),
                                fix_eff_interact_vars = c("agree", "social"),
                                random_intercept_vars = c("class", "school"),
                                nested_intercept_vars_pairs =
                                  list(c("class", "school")),
                                cross_intercept_vars_pairs = NULL,
                                uncorrel_slope_intercept_pairs = NULL,
                                random_slope_intercept_pairs = NULL,
                              family = "binomial", link = NA
  )
  this_formula <- "lme4::lmer(extro ~ open +  agree *  social +   ( 1 | school / class ) , family = binomial, data = dataset)"
  expect_equal(formula, this_formula)

  # cross intercept variables
  formula <- form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs = NULL,
                                         cross_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs = NULL,
                                         family = "binomial", link = "logit"
  )
  this_formula <- "lme4::lmer(extro ~ open +  agree +  social +   ( 1 | class ) +  ( 1 | school ) , family = binomial(link = logit), data = dataset)"
  expect_equal(formula, this_formula)
  form_expression_mixed_model_lme4("extro",
                              dataset = mydata,
                              fix_eff = NULL,
                              fix_eff_interact_vars = NULL,
                              random_intercept_vars = c("class", "school"),
                              nested_intercept_vars_pairs =
                                list(c("class", "school")),
                              cross_intercept_vars_pairs = NULL,
                              uncorrel_slope_intercept_pairs = NULL,
                              random_slope_intercept_pairs = NULL,
                              family = "binomial", link = "logit"
  )
  #Error - parameter to be estimated can not be null
  expect_error(form_expression_mixed_model_lme4(NULL,
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs = NULL,
                                         cross_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs = NULL,
                                         family = "binomial", link = "logit"
  ), "Error - param to be estimated or random intercepts is NULL or NA")
  # Error - dataset can not be null
  expect_error(form_expression_mixed_model_lme4("extro",
                                         dataset = NULL,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs = NULL,
                                         cross_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs = NULL,
                                         family = "binomial", link = "logit"
  ))
  # both nested and cross - error
  expect_error(form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         cross_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs = NULL,
                                         family = "binomial", link = "logit"
  ), "Random intercepts should not be in both nested or crossed")
  # both nested and cross - error
  expect_error(form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars =
                                           c("on", "class", "school"),
                                         nested_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         cross_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs = NULL,
                                         family = "binomial", link = "logit"
  ), "Random intercepts should not be in both nested or crossed")

  # Error - nested variables should be in pairs
  expect_error(form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs =
                                           c("class", "school"),
                                         cross_intercept_vars_pairs = NULL,
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs = NULL,
                                         family = "binomial", link = "logit"
  ), "Nested intercepts have to be given as a list of pairs")

  # Error - cross intercept variables should be in pairs
  expect_error(form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs = NULL,
                                         cross_intercept_vars_pairs =
                                           c("class", "school"),
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs = NULL,
                                         family = "binomial", link = "logit"
  ), "Cross intercepts have to be given as a list of pairs")

  # an extra variable in random effect, that is not termed as cross or nested.
  formula <- form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars =
                                           c("open", "class", "school"),
                                         nested_intercept_vars_pairs = NULL,
                                         cross_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs = NULL,
                                         family = "binomial", link = "logit"
  )
  this_formula <- "lme4::lmer(extro ~ open +  agree +  social +   ( 1 | open ) +  ( 1 | class ) +  ( 1 | school ) , family = binomial(link = logit), data = dataset)"
  expect_equal(formula, this_formula)

  # more than one slope intercept (crossed) pairs
  formula <- form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs = NULL,
                                         cross_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs =
                                           list(c("open", "school"),
                                             c("agree", "class")),
                                         family = "binomial", link = "logit"
  )
  this_formula <- "lme4::lmer(extro ~ open +  agree +  social +   ( 1 +  open | school ) +  ( 1 +  agree | class ) , family = binomial(link = logit), data = dataset)"
  expect_equal(formula, this_formula)

  # more than one slope intercept(nested) pairs
  formula <- form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         cross_intercept_vars_pairs = NULL,
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs =
                                           list(c("open", "school"),
                                            c("agree", "class")),
                                         family = "binomial", link = "logit"
  )
  this_formula <- "lme4::lmer(extro ~ open +  agree +  social +   ( 1 +  open | school / class ) +  ( 1 +  agree | school / class ), family = binomial(link = logit), data = dataset)"
  expect_equal(formula, this_formula)

  formula <- form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars =
                                           c("agree", "social"),
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs = NULL,
                                         cross_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs =
                                           list(c("open", "school")),
                                         family = "binomial", link = "logit"
  )
  this_formula <- "lme4::lmer(extro ~ open +  agree *  social +   ( 1 | class ) +  ( 1 +  open | school ) , family = binomial(link = logit), data = dataset)"
  expect_equal(formula, this_formula)

  formula <- form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars =
                                           c("agree", "social"),
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs = NULL,
                                         cross_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         uncorrel_slope_intercept_pairs =
                                           list(c("open", "school")),
                                         random_slope_intercept_pairs =
                                           list(c("open", "school")),
                                         family = "binomial", link = "logit"
  )
  this_formula <- "lme4::lmer(extro ~ open +  agree *  social +   ( 1 | class ) +  ( 0 +  open | school ) , family = binomial(link = logit), data = dataset)"
  expect_equal(formula, this_formula)

  # random intercept variables can not be null
  expect_error(form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars =
                                           c("agree", "social"),
                                         random_intercept_vars = NULL,
                                         nested_intercept_vars_pairs = NULL,
                                         cross_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         uncorrel_slope_intercept_pairs =
                                           list(c("open", "school")),
                                         random_slope_intercept_pairs =
                                           list(c("open", "school")),
                                         family = "binomial", link = "logit"
  ), "Error - param to be estimated or random intercepts is NULL or NA")

  # Error - family can not be NULL
  expect_error(form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars =
                                           c("agree", "social"),
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs = NULL,
                                         cross_intercept_vars_pairs =
                                           list(c("class", "school")),
                                         uncorrel_slope_intercept_pairs =
                                           list(c("open", "school")),
                                         random_slope_intercept_pairs =
                                           list(c("open", "school")),
                                         family = NULL, link = "logit"
  ), "Error - family can not be null")

  formula <- form_expression_mixed_model_lme4("extro",
                                         dataset = mydata,
                                         fix_eff = c("open", "agree", "social"),
                                         fix_eff_interact_vars = NULL,
                                         random_intercept_vars =
                                           c("class", "school"),
                                         nested_intercept_vars_pairs = NULL,
                                         cross_intercept_vars_pairs = NULL,
                                         uncorrel_slope_intercept_pairs = NULL,
                                         random_slope_intercept_pairs =
                                           list(c("open", "school")),
                                         family = "binomial", link = "logit"
  )
  this_formula <- "lme4::lmer(extro ~ open +  agree +  social +   ( 1 | class ) +  ( 1 +  open | school ) , family = binomial(link = logit), data = dataset)"
  expect_equal(formula, this_formula)

})
###############################################################################
context("testing forming slope and intercept ")
test_that("testing forming slope and intercept ", {
  # not nested or cross intercept
  formula <- get_slope_intercept("extro ~ open +",
                                 random_intercept_vars = c("class", "school"),
                                 random_slope_intercept_pairs = NULL,
                                 uncorrel_slope_intercept_pairs = NULL
  )
  this_formula <- "extro ~ open +  ( 1 | class ) +  ( 1 | school ) "
  expect_equal(formula, this_formula)

  expect_error(get_slope_intercept(NULL,
                                 random_intercept_vars = c("class", "school"),
                                 random_slope_intercept_pairs = NULL,
                                 uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept(NA,
                                   random_intercept_vars = c("class", "school"),
                                   random_slope_intercept_pairs = NULL,
                                   uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept("extro ~ open +",
                      random_intercept_vars = NULL,
                      random_slope_intercept_pairs = NULL,
                      uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept("extro ~ open +",
                                   random_intercept_vars = NA,
                                   random_slope_intercept_pairs = NULL,
                                   uncorrel_slope_intercept_pairs = NULL
  ))

  formula <- get_slope_intercept("extro ~ open +",
                      random_intercept_vars = c("class", "school"),
                      random_slope_intercept_pairs = list(c("open", "school")),
                      uncorrel_slope_intercept_pairs = list(c("open", "school"))
  )
  this_formula <- "extro ~ open +  ( 1 | class ) +  ( 0 +  open | school ) "
  expect_equal(this_formula, formula)

  formula <- get_slope_intercept("extro ~ open +",
                              random_intercept_vars = c("class", "school"),
                              random_slope_intercept_pairs =
                                list(c("open", "school"), c("agree", "school")),
                              uncorrel_slope_intercept_pairs =
                                list(c("open", "school"), c("agree", "school"))
  )
  this_formula <-  "extro ~ open +  ( 1 | class ) +  ( 0 +  open | school ) +  ( 0 +  agree | school ) "
  expect_equal(this_formula, formula)
})
###############################################################################
context("testing forming slope and intercept  for nested intercepts")
test_that("testing forming slope and for nested intercepts ", {
  # not nested or cross intercept
  formula <- get_slope_intercept_nested("extro ~ open +",
                                 random_intercept_vars = c("class", "school"),
                                 intercept_vars_pairs =
                                   list(c("class", "school")),
                                 random_slope_intercept_pairs = NULL,
                                 uncorrel_slope_intercept_pairs = NULL
  )
  this_formula <- "extro ~ open +  ( 1 | school / class ) "
  expect_equal(formula, this_formula)

  expect_error(get_slope_intercept_nested(NULL,
                                          random_intercept_vars =
                                            c("class", "school"),
                                          intercept_vars_pairs =
                                            list(c("class", "school")),
                                          random_slope_intercept_pairs = NULL,
                                        uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept_nested(NA,
                                          random_intercept_vars =
                                            c("class", "school"),
                                          intercept_vars_pairs =
                                            list(c("class", "school")),
                                          random_slope_intercept_pairs = NULL,
                                        uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept_nested("extro ~ open +",
                                          random_intercept_vars = NULL,
                                          intercept_vars_pairs =
                                            list(c("class", "school")),
                                          random_slope_intercept_pairs = NULL,
                                          uncorrel_slope_intercept_pairs = NULL
  ))

  expect_error(get_slope_intercept_nested("extro ~ open +",
                                          random_intercept_vars = NA,
                                          intercept_vars_pairs =
                                            list(c("class", "school")),
                                          random_slope_intercept_pairs = NULL,
                                          uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept_nested("extro ~ open +",
                                          random_intercept_vars =
                                            c("class", "school"),
                                          intercept_vars_pairs = NULL,
                                          random_slope_intercept_pairs = NULL,
                                          uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept_nested("extro ~ open +",
                                          random_intercept_vars =
                                            c("class", "school"),
                                          intercept_vars_pairs = NA,
                                          random_slope_intercept_pairs = NULL,
                                          uncorrel_slope_intercept_pairs = NULL
  ))

  formula <- get_slope_intercept_nested("extro ~ open +",
                             random_intercept_vars =
                               c("open", "class", "school"),
                             intercept_vars_pairs = list(c("class", "school")),
                             random_slope_intercept_pairs = NULL,
                             uncorrel_slope_intercept_pairs = NULL
  )
  this_formula <- "extro ~ open +  ( 1 | open ) +  ( 1 | school / class ) "
  expect_equal(formula, this_formula)


  expect_error(get_slope_intercept_nested("extro ~ open +",
                                      random_intercept_vars =
                                        c("class", "school"),
                                      intercept_vars_pairs =
                                        list(c("open", "school")),
                                      random_slope_intercept_pairs = NULL,
                                      uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept_nested("extro ~ open +",
                           random_intercept_vars = c("class", "school"),
                           intercept_vars_pairs = list(c("class", "school")),
                           random_slope_intercept_pairs =
                             list(c("open", "agree")),
                           uncorrel_slope_intercept_pairs = NULL
  ))

  formula <- get_slope_intercept_nested("extro ~ open +",
                                        random_intercept_vars =
                                          c("class", "school"),
                                        intercept_vars_pairs =
                                          list(c("class", "school")),
                                        random_slope_intercept_pairs =
                                          list(c("open", "school")),
                                        uncorrel_slope_intercept_pairs = NULL
)
  this_formula <- "extro ~ open +  ( 1 +  open | school / class )"
  expect_equal(this_formula, formula)
  formula <- get_slope_intercept_nested("extro ~ open +",
                                        random_intercept_vars =
                                          c("class", "school"),
                                        intercept_vars_pairs =
                                          list(c("class", "school")),
                                        random_slope_intercept_pairs =
                                          list(c("open", "school")),
                                        uncorrel_slope_intercept_pairs =
                                          list(c("open", "school"))
  )
  this_formula <- "extro ~ open +  ( 0 +  open | school / class )"
  expect_equal(this_formula, formula)

  formula <- get_slope_intercept_nested("extro ~ open +",
                                        random_intercept_vars =
                                          c("class", "school", "town",
                                            "state"),
                                        intercept_vars_pairs =
                                          list(c("class", "school"),
                                               c("town", "state")),
                                        random_slope_intercept_pairs =
                                          list(c("open", "school")),
                                        uncorrel_slope_intercept_pairs =
                                          list(c("open", "school"))
  )
  this_formula <- "extro ~ open +  ( 1 | state / town ) +  ( 0 +  open | school / class )"
  expect_equal(this_formula, formula)

  formula <- get_slope_intercept_nested("extro ~ open +",
                                        random_intercept_vars =
                                          c("class", "school",
                                            "town", "state"),
                                        intercept_vars_pairs =
                                          list(c("class", "school"),
                                               c("town", "state")),
                                        random_slope_intercept_pairs = NULL,
                                        uncorrel_slope_intercept_pairs = NULL
  )
  this_formula <- "extro ~ open +  ( 1 | school / class ) +  ( 1 | state / town ) "
  expect_equal(this_formula, formula)

  expect_error(get_slope_intercept_nested("extro ~ open +",
                                        random_intercept_vars =
                                          c("class", "school", "town"),
                                        intercept_vars_pairs =
                                       list(c("class", "school"), c("town")),
                                        random_slope_intercept_pairs = NULL,
                                        uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept_nested("extro ~ open +",
                                        random_intercept_vars =
                                          c("class", "school", "town"),
                                        intercept_vars_pairs =
                                       list(c("class", "school"), c("town")),
                                        random_slope_intercept_pairs =
                                          list(c("open", "school")),
                                        uncorrel_slope_intercept_pairs = NULL
  ))
  formula <- get_slope_intercept_nested("extro ~ open +",
                                        random_intercept_vars =
                                      c("class", "school", "town", "state"),
                                        intercept_vars_pairs =
                                          list(c("class", "school"),
                                               c("town", "state")),
                                        random_slope_intercept_pairs = NULL,
                                        uncorrel_slope_intercept_pairs = NULL
  )
  this_formula <- "extro ~ open +  ( 1 | school / class ) +  ( 1 | state / town ) "
  expect_equal(this_formula, formula)
})
###############################################################################
context("testing forming slope and intercept  for cross intercepts")
test_that("testing forming slope and for cross intercepts ", {
  # not nested or cross intercept
  formula <- get_slope_intercept_cross("extro ~ open +",
                                        random_intercept_vars =
                                         c("class", "school"),
                                        intercept_vars_pairs =
                                         list(c("class", "school")),
                                        random_slope_intercept_pairs = NULL,
                                        uncorrel_slope_intercept_pairs = NULL
  )
  this_formula <- "extro ~ open +  ( 1 | class ) +  ( 1 | school ) "
  expect_equal(formula, this_formula)

  expect_error(get_slope_intercept_cross(NULL,
                                          random_intercept_vars =
                                           c("class", "school"),
                                          intercept_vars_pairs =
                                           list(c("class", "school")),
                                          random_slope_intercept_pairs = NULL,
                                          uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept_cross(NA,
                                          random_intercept_vars =
                                           c("class", "school"),
                                          intercept_vars_pairs =
                                           list(c("class", "school")),
                                          random_slope_intercept_pairs = NULL,
                                          uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept_cross("extro ~ open +",
                                          random_intercept_vars = NULL,
                                          intercept_vars_pairs =
                                           list(c("class", "school")),
                                          random_slope_intercept_pairs = NULL,
                                          uncorrel_slope_intercept_pairs = NULL
  ))

  expect_error(get_slope_intercept_cross("extro ~ open +",
                                          random_intercept_vars = NA,
                                          intercept_vars_pairs =
                                           list(c("class", "school")),
                                          random_slope_intercept_pairs = NULL,
                                      uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept_cross("extro ~ open +",
                                          random_intercept_vars =
                                           c("class", "school"),
                                          intercept_vars_pairs = NULL,
                                          random_slope_intercept_pairs = NULL,
                                      uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept_cross("extro ~ open +",
                                          random_intercept_vars =
                                           c("class", "school"),
                                          intercept_vars_pairs = NA,
                                          random_slope_intercept_pairs = NULL,
                                      uncorrel_slope_intercept_pairs = NULL
  ))
  expect_error(get_slope_intercept_cross("extro ~ open +",
                                         random_intercept_vars =
                                           c("class", "school"),
                                         intercept_vars_pairs =
                                           list(c("open", "school")),
                                         random_slope_intercept_pairs = NULL,
                                         uncorrel_slope_intercept_pairs = NULL
  ))
  formula <- get_slope_intercept_cross("extro ~ open +",
                                        random_intercept_vars =
                                         c("class", "school", "town", "state"),
                                        intercept_vars_pairs =
                                         list(c("class", "school"),
                                              c("town", "state")),
                                        random_slope_intercept_pairs =
                                         list(c("open", "school")),
                                        uncorrel_slope_intercept_pairs =
                                         list(c("open", "school"))
  )
  this_formula <- "extro ~ open +  ( 1 | class ) +  ( 1 | town ) +  ( 1 | state ) +  ( 0 +  open | school ) "
  expect_equal(this_formula, formula)

  formula <- get_slope_intercept_cross("extro ~ open +",
                                        random_intercept_vars =
                                      c("class", "school", "town", "state"),
                                        intercept_vars_pairs =
                                         list(c("class", "school"),
                                              c("town", "state")),
                                        random_slope_intercept_pairs = NULL,
                                      uncorrel_slope_intercept_pairs = NULL
  )
  this_formula <- "extro ~ open +  ( 1 | class ) +  ( 1 | school ) +  ( 1 | town ) +  ( 1 | state ) "
  expect_equal(this_formula, formula)
})
