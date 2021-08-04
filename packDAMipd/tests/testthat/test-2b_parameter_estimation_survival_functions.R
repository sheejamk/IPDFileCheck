###############################################################################
test_that("get parameter using survival analysis", {
  datafile <- system.file("extdata", "survival_aml.csv", package = "packDAMipd")
  data_for_survival <- read.csv(datafile)
  surv_results <- use_survival_analysis("status", datafile,
                                        "x", info_get_method = "parametric",
                                        info_distribution
                                        = "weibull", covariates = NA, "time"
  )
  expect_equal(surv_results$model_coeff[1], 0.00554, tol = 1e-5)
  data_for_survival <- na.omit(data_for_survival)
  surv_results <- use_survival_analysis("status", data_for_survival,
                        "x",
                        info_get_method = "parametric", info_distribution
                        = "weibull", covariates = NA, "time"
  )
  expect_equal(surv_results$model_coeff[1], 0.00554, tol = 1e-5)

  surv_results <- use_survival_analysis("status", data_for_survival,
                                        "x",
                                    info_get_method = "km",
                                    info_distribution = "weibull",
                                    covariates = NA, "time"
  )
  expect_equal(surv_results$fit$n, c(11, 12))
  surv_results <- use_survival_analysis("status", data_for_survival,
                                        "x",
                                    info_get_method = "fh",
                                    info_distribution
                                    = "weibull", covariates = NA, "time"
  )
  expect_equal(surv_results$fit$n, c(11, 12))
  surv_results <- use_survival_analysis("status", data_for_survival,
                                        "x",
                                    info_get_method = "fh2",
                                    info_distribution = "weibull",
                                    covariates = NA, "time"
  )
  expect_equal(surv_results$fit$n, c(11, 12))

  surv_results <- use_survival_analysis("status", data_for_survival,
                                        "x",
                                    info_get_method = "coxph",
                                    info_distribution = "weibull",
                                    covariates = NA, "time"
  )
  expect_equal(unname(surv_results$fit$coefficients), 0.9155, tol = 1e-4)
  # columns cant be found
  expect_error(use_survival_analysis("status", data_for_survival,
                                     "sex",
                                     info_get_method = "parametric",
                                     info_distribution = "weibull",
                                     covariates = NA, "time"
  ))
  # Error - parameter to be estimated is not found
  expect_error(use_survival_analysis(NULL, data_for_survival,
                                     "x",
                                     info_get_method = "parametric",
                                     info_distribution = "weibull",
                                     covariates = NA, "time"
  ))
  # Error - dataset is not found
  expect_error(use_survival_analysis("status", NULL, "x",
                                     info_get_method = "parametric",
                                     info_distribution = "weibull",
                                     covariates = NA, "time"))
  # Error - independent variable is not found
  expect_error(use_survival_analysis("status", data_for_survival,
                                     NA,
                                     info_get_method = "parametric",
                                     info_distribution = "weibull",
                                     covariates = NA, "time"
  ))
  # Error - method is missing
  expect_error(use_survival_analysis("status", data_for_survival, "x", NA,
                                     info_distribution = "weibull",
                                     covariates = NA,
                                     "time"
  ))
})
###############################################################################
test_that("get parameter using parametric regression survival analysis", {
  datafile <- system.file("extdata", "survival_lung.csv",
                          package = "packDAMipd")
  data_for_survival <- read.csv(datafile)
  surv_estimated <- use_parametric_survival("status", data_for_survival, "sex",
                                            info_distribution = "weibull",
                                            covariates = c("ph.ecog"), "time")
  expect_equal(surv_estimated$model_coeff[1], 0.00035, tol = 1e-4)

  surv_estimated <- use_parametric_survival("status", datafile, "sex",
                                            info_distribution = "weibull",
                                            covariates = c("ph.ecog"), "time")
  expect_equal(surv_estimated$model_coeff[1], 0.00035, tol = 1e-4)

  # error - param to be estimated not valid
  expect_error(use_parametric_survival(NA, data_for_survival, "sex",
                              info_distribution = "weibull", covariates = NA,
                               "time"
  ))
  #error - no dataset
  expect_error(use_parametric_survival("status", NULL, "sex",
                              info_distribution = "weibull", covariates = NA,
                              "time"
  ))
  # error - no independent variable
  expect_error(use_parametric_survival("status", data_for_survival, NA,
                              info_distribution = "weibull", covariates = NA,
                              "time"
  ))
  # Error - no information on distribution
  expect_error(use_parametric_survival("status", data_for_survival,
                                       "sex",
                                       NA,
                                       covariates = NA, "time"
  ))
  # Error - no information on the time variable
  expect_error(use_parametric_survival("status", data_for_survival, "sex",
                info_distribution = "weibull", covariates = NA, NA
  ))
  # Error - no cluster variable in dataset
  expect_error(use_parametric_survival("status", data_for_survival, "sex",
                                       info_distribution = "weibull",
                                       covariates = NA, "time",
                                       cluster_var = "open"
  ))
  # Error - no cluster variable in dataset
  expect_error(use_parametric_survival("status", data_for_survival, "sex",
                                       info_distribution = "weibull",
                                       covariates = "ph.ecog", "time",
                                       cluster_var = "open"
  ))
  data_for_survival <- data_for_survival[!is.na(data_for_survival$meal.cal), ]
  surv_estimated <- use_parametric_survival("status", data_for_survival, "sex",
                                            info_distribution = "weibull",
                                            covariates = c("ph.ecog"), "time",
                                            cluster_var = "meal.cal"
  )
  expect_equal(surv_estimated$model_coeff[1], 0.00035, tol = 1e-4)

  surv_estimated <- use_parametric_survival("status", data_for_survival, "sex",
                                            info_distribution = "weibull",
                                            covariates = NA, "time",
                                            cluster_var = "meal.cal"
  )
  expect_equal(surv_estimated$model_coeff[1], 0.000816, tol = 1e-4)

})
###############################################################################

test_that("get parameter using kaplan meier survival analysis", {

  datafile <- system.file("extdata", "survival_lung.csv",
                          package = "packDAMipd")
  data_for_survival <- read.csv(datafile)

  surv_estimated_aml <- use_km_survival("status", data_for_survival, "sex",
                                        covariates = c("ph.ecog"), "time")
  expect_equal(surv_estimated_aml$fit$n, c(36, 27, 71, 42, 29, 21,  1))

  surv_estimated_aml <- use_km_survival("status", datafile, "sex",
                                        covariates = c("ph.ecog"), "time")
  expect_equal(surv_estimated_aml$fit$n, c(36, 27, 71, 42, 29, 21,  1))
  datafile <- system.file("extdata", "survival_aml.csv",
                          package = "packDAMipd")
  data_for_survival <- read.csv(datafile)
  surv_estimated_aml <- use_km_survival("status", data_for_survival, "x",
                                        covariates = NA, "time")
  expect_equal(surv_estimated_aml$fit$n, c(11, 12))
  surv_estimated_aml <- use_km_survival("status", datafile, "x",
                                        covariates = NA, "time")
  expect_equal(surv_estimated_aml$fit$n, c(11, 12))


  # Error - parameter to be estimated is not found
  expect_error(use_km_survival(NA, data_for_survival, "x",
                               covariates = NA, "time"))
  # Error - dataset not provided
  expect_error(use_km_survival("status", NULL, "x", covariates = NA, "time"))
  # Error - independent variable not given
  expect_error(use_km_survival("status", data_for_survival, NA,
                               covariates = NA, "time"))
  # Error - time variable not given
  expect_error(use_km_survival("status", data_for_survival, "x",
                               covariates = NA, NA))
})
###############################################################################

test_that("get parameter using FH survival analysis", {
  datafile <- system.file("extdata", "survival_lung.csv",
                          package = "packDAMipd")
  data_for_survival <- read.csv(datafile)
  surv_estimated_aml <- use_fh_survival("status", data_for_survival, "sex",
                                        covariates = c("ph.ecog"), "time")
  expect_equal(surv_estimated_aml$fit$n, c(36, 27, 71, 42, 29, 21,  1))
  surv_estimated_aml <- use_fh_survival("status", datafile, "sex",
                                        covariates = c("ph.ecog"), "time")
  expect_equal(surv_estimated_aml$fit$n, c(36, 27, 71, 42, 29, 21,  1))

  data_for_survival <- survival::aml
  data_for_survival <- na.omit(data_for_survival)
  surv_estimated_aml <- use_fh_survival("status", data_for_survival, "x",
                                        covariates = NA, "time")
  # Error - parameter to be estimated is not found
  expect_error(use_fh_survival(NA, data_for_survival, "x", covariates = NA,
                               "time"))
  # Error - dataset not provided
  expect_error(use_fh_survival("status", NULL, "x", covariates = NA, "time"))
  # Error - independent variable not given
  expect_error(use_fh_survival("status", data_for_survival, NA,
                               covariates = NA, "time"))
  # Error - time variable not given
  expect_error(use_fh_survival("status", data_for_survival, "x",
                               covariates = NA, NA))
})
###############################################################################

test_that("get parameter using FH2 survival analysis", {
  datafile <- system.file("extdata", "survival_lung.csv",
                          package = "packDAMipd")
  data_for_survival <- read.csv(datafile)
  surv_estimated_aml <- use_fh2_survival("status", data_for_survival, "sex",
                                        covariates = c("ph.ecog"), "time")
  expect_equal(surv_estimated_aml$fit$n, c(36, 27, 71, 42, 29, 21,  1))
  surv_estimated_aml <- use_fh2_survival("status", datafile, "sex",
                                        covariates = c("ph.ecog"), "time")
  expect_equal(surv_estimated_aml$fit$n, c(36, 27, 71, 42, 29, 21,  1))

  data_for_survival <- survival::aml
  data_for_survival <- na.omit(data_for_survival)
  surv_estimated_aml <- use_fh2_survival("status", data_for_survival, "x",
                                         covariates = NA, "time")
  # Error - parameter to be estimated is not found
  expect_error(use_fh2_survival(NA, data_for_survival, "x",
                                covariates = NA, "time"))
  # Error - dataset not provided
  expect_error(use_fh2_survival("status", NULL, "x", covariates = NA, "time"))
  # Error - independent variable not given
  expect_error(use_fh2_survival("status", data_for_survival, NA,
                                covariates = NA, "time"))
  # Error - time variable not given
  expect_error(use_fh2_survival("status", data_for_survival, "x",
                                covariates = NA, NA))
})
###############################################################################

test_that("get parameter using cox ph survival analysis", {
  datafile <- system.file("extdata", "survival_lung.csv",
                          package = "packDAMipd")
  data_for_survival <- read.csv(datafile)
  myvars <- c("status", "time", "sex", "ph.ecog")
  data_for_survival <- data_for_survival[myvars]
  data_for_survival <- na.omit(data_for_survival)
  surv_estimated_aml <- use_coxph_survival("status", data_for_survival, "sex",
                                        covariates = c("ph.ecog"), "time")
  expect_equal(surv_estimated_aml$fit$n, c(227))

   surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
    covariates = c("ph.ecog"), "time"
  )
  # Error - parameter to be estimated is not found
  expect_error(use_coxph_survival(NA, data_for_survival, "sex",
                                  covariates = NA, "time"))
  # Error - dataset not provided
  expect_error(use_coxph_survival("status", NULL, "sex", covariates = NA,
                                  "time"))
  # Error - independent variable not given
  expect_error(use_coxph_survival("status", data_for_survival, NA,
                                  covariates = NA, "time"))
  # Error - time variable not given
  expect_error(use_coxph_survival("status", data_for_survival, "sex",
                                  covariates = NA, NA))
})
