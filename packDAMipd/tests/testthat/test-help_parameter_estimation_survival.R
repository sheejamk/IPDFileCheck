
###############################################################################
context("testing returning residual for survival")
test_that("testing  returning residual for survival", {
  data_for_survival <- survival::lung
  surv_estimated <- use_parametric_survival("status", data_for_survival, "sex",
                                            info_distribution = "weibull",
                                            covariates = c("ph.ecog"), "time"
  )

  plot_return_residual_survival("status", "sex",
                    covariates = c("ph.ecog"), surv_estimated$fit)

  data_for_survival <- data_for_survival[!is.na(data_for_survival$meal.cal), ]
  surv_estimated <- use_parametric_survival("status", data_for_survival, "sex",
                                            info_distribution = "weibull",
                                            covariates = c("ph.ecog"), "time",
                                            cluster_var = "meal.cal"
  )
  plot_return_residual_survival("status", "sex",
                                covariates = c("ph.ecog"),
                                surv_estimated$fit)
  #Error - parameter to be estimated can not be NA
  expect_error(plot_return_residual_survival(NA, "sex",
                                covariates = c("ph.ecog"), surv_estimated$fit))
  #Error - independent variable  can not be NULL
  expect_error(plot_return_residual_survival("status", NULL,
                                covariates = c("ph.ecog"), surv_estimated$fit))
  #Error - fit should be of survreg object
  expect_error(plot_return_residual_survival("status", "sex",
                            covariates = c("ph.ecog"), "fit"))
})
###############################################################################
context("testing plotting prediction parameteric survival")
test_that("testing  plotting prediction parameteric survival", {
  data_for_survival <- survival::lung
  surv_estimated <- use_parametric_survival("status", data_for_survival, "sex",
                                            info_distribution = "weibull",
                                            covariates = c("ph.ecog"), "time")

  newdata <- data_for_survival
  newdata[newdata$sex == 2, ]$sex <- "male"
  newdata[newdata$sex == 1, ]$sex <- "female"
  surv_estimated <- use_parametric_survival("status", newdata, "sex",
                                            info_distribution = "weibull",
                                            covariates = NA, "time")
  plot_prediction_parametric_survival("status", "sex", covariates = NA,
                                      newdata, surv_estimated$fit,
                                      "time")

  surv_estimated <- use_parametric_survival("status", data_for_survival, "sex",
                                    info_distribution = "weibull",
                                    covariates = NA, "time")

  plot_prediction_parametric_survival("status", "sex", covariates = NA,
                                      data_for_survival, surv_estimated$fit,
                                      "time")

  surv_estimated <- use_parametric_survival("status", data_for_survival, "sex",
                                            info_distribution = "weibull",
                                            covariates = c("ph.ecog"),
                                            "time")

  plot_prediction_parametric_survival("status", "sex",
            covariates = c("ph.ecog"), data_for_survival,
            surv_estimated$fit,
            "time")

  #Error - parameter to be estimated can not be NA
  expect_error(plot_prediction_parametric_survival(NA, "sex",
          covariates = c("ph.ecog"), data_for_survival, surv_estimated$fit,
          "time"))
  #Error - independent variable  can not be NULL
  expect_error(plot_prediction_parametric_survival("status", NULL,
            covariates = c("ph.ecog"), data_for_survival,
            surv_estimated$fit, "time"))
  #Error - dataset should not be NULL
  expect_error(plot_prediction_parametric_survival("status", "sex",
              covariates = c("ph.ecog"), NULL, surv_estimated$fit, "time"))
  #Error - fit object should be of survreg type
  expect_error(plot_prediction_parametric_survival("status", "sex",
          covariates = c("ph.ecog"), data_for_survival, "fit", "time"))
  #Error - time variable can not be NULL
  expect_error(plot_prediction_parametric_survival("status", "sex",
        covariates = c("ph.ecog"), data_for_survival, surv_estimated$fit, NULL))
})
###############################################################################
context("testing creating a new dataset based on given one")
test_that("testing creating a new dataset based on given one", {
  dataset <- survival::lung
  temp <- sample(c("no", "yes"), nrow(dataset), replace = T)
  dataset[["check"]] <- temp
  new <- create_new_dataset("status", c("check"), dataset, c(TRUE))
  expect_equal(unique(new$check), "no")
  new <- create_new_dataset("status", c("age"), dataset, c(FALSE))
  expect_equal(unique(new$age), 62.4437, tol = 1e-3)
  #Error -var null or Na
  expect_error(create_new_dataset(NULL, c("age"), dataset, c(FALSE)))
  expect_error(create_new_dataset(NA, c("age"), dataset, c(FALSE)))
  # Error- dataset  null
  expect_error(create_new_dataset("status", c("age"), NULL, c(FALSE)))
  # Error- column not in dataset
  expect_error(create_new_dataset("status", c("age1"), dataset, c(FALSE)))
  expect_error(create_new_dataset("status1", c("age"), dataset, c(FALSE)))
  df <- data.frame(status = c(1, 2),
                  age = c(62.44737, 62.44737))
  expect_equal(create_new_dataset("status", c("age"), dataset,
                                  c(FALSE)), df, tol = 1e-3)
})
###############################################################################
context("testing plotting survival function")
test_that("testingplotting survival function", {
  data_for_survival <- survival::lung
  data_for_survival <- na.omit(data_for_survival)
  plot_return_survival_curve(param_to_be_estimated = "status",
                             dataset = data_for_survival,
                             indep_var = "sex",
                             covariates = c("ph.ecog"),
                             timevar_survival = "time")

  plot_return_survival_curve(param_to_be_estimated = "status",
                             dataset = data_for_survival,
                             indep_var = "sex", covariates = NA,
                             timevar_survival = "time")

  #Error - parameter to be estimated can not be NA or NULL
  expect_error(plot_return_survival_curve(param_to_be_estimated = NULL,
                                          dataset = data_for_survival,
                                          indep_var = "sex",
                                          covariates = c("ph.ecog"),
                                          timevar_survival = "time"))
  expect_error(plot_return_survival_curve(param_to_be_estimated = NA,
                                          dataset = data_for_survival,
                                          indep_var = "sex",
                                          covariates = c("ph.ecog"),
                                          timevar_survival = "time"))

  #Error -data set can not be NULL
  expect_error(plot_return_survival_curve(param_to_be_estimated =  "status",
                                          dataset = NULL, indep_var = "sex",
                                          covariates = c("ph.ecog"),
                                          timevar_survival = "time"))

  #Error -independent variable can not be NULL or NA
  expect_error(plot_return_survival_curve(param_to_be_estimated = "status",
                                          dataset = data_for_survival,
                                          indep_var = NULL,
                                          covariates = c("ph.ecog"),
                                          timevar_survival = "time"))

  expect_error(plot_return_survival_curve(param_to_be_estimated = "status",
                                          dataset = data_for_survival,
                      indep_var = NA, covariates = c("ph.ecog"),
                      timevar_survival = "time"))

  #Error -time variable can not be NULL or NA
  expect_error(plot_return_survival_curve(param_to_be_estimated = "status",
                                          dataset = data_for_survival,
                                          indep_var = "sex",
                                          covariates = c("ph.ecog"),
                                          timevar_survival = NULL))
  expect_error(plot_return_survival_curve(param_to_be_estimated = "status",
                                          dataset = data_for_survival,
                                          indep_var = "sex",
                                          covariates = c("ph.ecog"),
                                          timevar_survival = NA))

})
###############################################################################
context("testing plotting residual for cox ph models")
test_that("testing plotting residual for cox ph models", {

  data_for_survival <- survival::lung
  data_for_survival <- na.omit(data_for_survival)
  surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
    covariates = c("ph.ecog"), "time")
  plot_return_residual_cox("status", "sex", covariates = NA,
                           surv_estimated$fit,  data_for_survival)
  plot_return_residual_cox("status", "sex", covariates = c("ph.ecog"),
                           surv_estimated$fit,  data_for_survival)
  #Error - param to be estimated can not be null  or NA
  expect_error(plot_return_residual_cox(NULL, "sex",
                                        covariates = c("ph.ecog"),
                                        surv_estimated$fit,
                                        data_for_survival))
  expect_error(plot_return_residual_cox(NA, "sex",
                                        covariates = c("ph.ecog"),
                                        surv_estimated$fit,
                                        data_for_survival))

  #Error - independent variable  can not be null or na
  expect_error(plot_return_residual_cox("status", NULL,
                                        covariates = c("ph.ecog"),
                                        surv_estimated$fit,
                                        data_for_survival))
  expect_error(plot_return_residual_cox("status", NA,
                                        covariates = c("ph.ecog"),
                                        surv_estimated$fit,
                                        data_for_survival))

  # Error - fit object is not of type coxph
  expect_error(plot_return_residual_cox("status", "sex",
                                        covariates = c("ph.ecog"),
                                        "fit", data_for_survival))
})
###############################################################################
context("testing plotting residual for cox ph models")
test_that("testing plotting residual for cox ph models", {

  data_for_survival <- survival::lung
  data_for_survival <- na.omit(data_for_survival)
  surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
                                       covariates = c("ph.ecog"), "time")
  predict_coxph(surv_estimated$fit, data_for_survival, "status", "sex",
                covariates = c("ph.ecog"), "time")
  #Error - fit object is not of coxph type
  expect_error(predict_coxph("fit", data_for_survival, "status", "sex",
                covariates = c("ph.ecog"), "time"))
  #Error - dataset should not be NULL
  expect_error(predict_coxph(surv_estimated$fit, NULL, "status", "sex",
                             covariates = c("ph.ecog"), "time"))

  #Error - param to be estimated should not be NULL or NA
  expect_error(predict_coxph(surv_estimated$fit, data_for_survival, NULL, "sex",
                             covariates = c("ph.ecog"), "time"))
  expect_error(predict_coxph(surv_estimated$fit, data_for_survival, NA, "sex",
                             covariates = c("ph.ecog"), "time"))

  #Error - independent varaible should not be NULL or NA
  expect_error(predict_coxph(surv_estimated$fit, data_for_survival,
                             "status", NULL,
                             covariates = c("ph.ecog"), "time"))
  expect_error(predict_coxph(surv_estimated$fit, data_for_survival,
                             "status", NA,
                             covariates = c("ph.ecog"), "time"))

  #Error -time variable should not be NULL or NA
  expect_error(predict_coxph(surv_estimated$fit, data_for_survival, "status",
                             "sex", covariates = c("ph.ecog"), NULL))
  expect_error(predict_coxph(surv_estimated$fit, data_for_survival, "status",
                             "sex", covariates = c("ph.ecog"), NA))

})
###############################################################################
context("testing plotting residual for cox ph models")
test_that("testing plotting residual for cox ph models", {
  data_for_survival <- survival::lung
  data_for_survival <- na.omit(data_for_survival)

  surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
                                       covariates = c("ph.ecog"), "time")
  plot_survival_cox_covariates(surv_estimated$fit, data_for_survival, "status",
                covariates = c("ph.ecog"), "sex")

  new_data <- data_for_survival
  new_data[new_data$sex == 1, ]$sex <- "F"
  new_data[new_data$sex == 2, ]$sex <- "M"
  surv_estimated <- use_coxph_survival("status", new_data, "sex",
                                       covariates = c("ph.ecog"), "time")

  plot_survival_cox_covariates(surv_estimated$fit, new_data, "status",
                               covariates = c("ph.ecog"), "age")

  surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
                                       covariates = NA, "time")
  plot_survival_cox_covariates(surv_estimated$fit, data_for_survival, "status",
                               covariates = NA, "sex")

  #Error - fit object is not of coxph type
  expect_error(plot_survival_cox_covariates("fit", data_for_survival,
                                            "status",
                             covariates = c("ph.ecog"), "sex"))
  #Error - dataset should not be NULL
  expect_error(plot_survival_cox_covariates(surv_estimated$fit,
                                            NULL, "status",
                                            covariates = c("ph.ecog"), "sex"))

  #Error - param to be estimated should not be NULL or NA
  expect_error(plot_survival_cox_covariates(surv_estimated$fit,
                                            data_for_survival, NULL,
                             covariates = c("ph.ecog"), "sex"))
  expect_error(predict_coxph(surv_estimated$fit, data_for_survival, NA,
                             covariates = c("ph.ecog"), "sex", NA))

  #Error - independent varaible should not be NULL or NA
  expect_error(plot_survival_cox_covariates(surv_estimated$fit,
                                            data_for_survival, "status",
                             covariates = c("ph.ecog"), NULL))
  expect_error(plot_survival_cox_covariates(surv_estimated$fit,
                                            data_for_survival, "status",
                             covariates = c("ph.ecog"), NA))


})
