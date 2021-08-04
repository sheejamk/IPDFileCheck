###############################################################################
context("testing mean and sd of age from data")
test_that("testing mean and sd of age from data", {
  this_data <- data.table::data.table("Age" = c(21, 15), "sex" = c("m", "f"))
  results <- get_mean_sd_age(this_data, NA)
  expect_equal(results$mean, 18.00)
  expect_equal(results$sd, 4.242641, tolerance = 1e-4)
  this_data <- data.table::data.table("Age" = c("k", 15), "sex" = c("m", "f"))
  # Error - format wrong
  expect_error(get_mean_sd_age(this_data, NA))
  # Error - data should not be NULL
  expect_error(get_mean_sd_age(NULL, NA))

  this_data <- data.table::data.table("Age" = c(21, 15, -1),
                                      "sex" = c("m", "f", "f"))
  results <- get_mean_sd_age(this_data, -1)
  expect_equal(results$mean, 18.00)
})
###############################################################################
context("testing adding EQ5D5L values to the data")
test_that("testing adding EQ5D5L values to the data", {
  trial_data <- data.frame(
    "qol.MO" = c(1, 2), "qol.SC" = c(1, 5), "qol.UA" = c(1, 2),
    "qol.PD" = c(1, 2), "qol.AD" = c(1, 2)
  )
  results <- value_eq5d5L_IPD(trial_data, NA)
  expect_equal(results$EQ5D5LIndex, c(1, 0.548))
  this_data <- data.table::data.table("Age" = c("k", 15), "sex" = c("m", "f"))
  expect_error(value_eq5d5L_IPD(this_data, NA))
  # Error - invalid EQ5D responses
  trial_data <- data.frame(
    "qol.MO" = c(1, 2), "qol.SC" = c(1, 8), "qol.UA" = c(1, 2),
    "qol.PD" = c(1, 2), "qol.AD" = c(1, 2)
  )
  expect_error(value_eq5d5L_IPD(trial_data, NA))
  # Error - data should not be NULL
  expect_error(value_eq5d5L_IPD(NULL, NA))
  trialdatafile <- system.file("extdata", "trial_data_sampleEq5d.csv",
                               package = "packDAMipd")
  trial_data <- read.csv(trialdatafile)
  results <- value_eq5d5L_IPD(trial_data, NA)
  expect_equal(results$EQ5D5LIndex[1], 0.535)

  trial_data <- data.frame(
    "qol.MO" = c(1, 3, NA), "qol.SC" = c(1, 3, 1), "qol.UA" = c(1, 1, 1),
    "qol.PD" = c(3, 2, 1), "qol.AD" = c(2, 2, 1)
  )
  results <- value_eq5d5L_IPD(trial_data, NA)
  expect_equal(results$EQ5D5LIndex, c(0.838, 0.703, NA))

  trial_data <- data.frame(
    "qol.MO" = c(1, 3, 99), "qol.SC" = c(1, 3, 1), "qol.UA" = c(1, 1, 1),
    "qol.PD" = c(3, 2, 1), "qol.AD" = c(2, 2, 1)
  )
  results <- value_eq5d5L_IPD(trial_data, 99)
  expect_equal(results$EQ5D5LIndex, c(0.838, 0.703, 99))

})
###############################################################################
context("testing adding EQ5D3L values to the data")
test_that("testing adding EQ5D3L values to the data", {
  library(valueEQ5D)

  trial_data <- data.frame(
    "qol.MO" = c(1, 3), "qol.SC" = c(1, 3), "qol.UA" = c(1, 1),
    "qol.PD" = c(3, 2), "qol.AD" = c(2, 2)
  )
  results <- value_eq5d3L_IPD(trial_data, NA)
  expect_equal(results$EQ5D3LIndex, c(0.193, -0.072))
  this_data <- data.table::data.table("Age" = c("k", 15), "sex" = c("m", "f"))
  expect_error(value_eq5d3L_IPD(this_data, NA))
  # Error - invalid EQ5D responses
  trial_data <- data.frame(
    "qol.MO" = c(1, 2), "qol.SC" = c(1, 5), "qol.UA" = c(1, 2),
    "qol.PD" = c(1, 2), "qol.AD" = c(1, 2)
  )
  expect_error(value_eq5d3L_IPD(trial_data, NA))
  # Error - data should not be NULL
  expect_error(value_eq5d3L_IPD(NULL, NA))
  trialdatafile <- system.file("extdata", "trial_data_sampleEq5d3l.csv",
                               package = "packDAMipd")
  trial_data <- read.csv(trialdatafile)
  results <- value_eq5d3L_IPD(trial_data, NA)
  expect_equal(results$EQ5D3LIndex[1], 0.014)
  trial_data <- data.frame(
    "qol.MO" = c(1, 3, NA), "qol.SC" = c(1, 3, 1), "qol.UA" = c(1, 1, 1),
    "qol.PD" = c(3, 2, 1), "qol.AD" = c(2, 2, 1)
  )
  results <- value_eq5d3L_IPD(trial_data, NA)
  expect_equal(results$EQ5D3LIndex, c(0.193, -0.072, NA))

  trial_data <- data.frame(
    "qol.MO" = c(1, 3, 99), "qol.SC" = c(1, 3, 1), "qol.UA" = c(1, 1, 1),
    "qol.PD" = c(3, 2, 1), "qol.AD" = c(2, 2, 1)
  )
  results <- value_eq5d3L_IPD(trial_data, 99)
  expect_equal(results$EQ5D3LIndex, c(0.193, -0.072, 99))

})
###############################################################################
context("testing mapping EQ5D5L values to 3L ")
test_that("testing adding EQ5D5L values to the data", {
  trial_data <- data.frame(
    "qol.MO" = c(1, 2), "qol.SC" = c(1, 2), "qol.UA" = c(1, 2),
    "qol.PD" = c(1, 2), "qol.AD" = c(1, 2)
  )
  library(valueEQ5D)
  results <- map_eq5d5Lto3L_VanHout(trial_data, NA)
  expect_equal(results$EQ5D3L_from5L, c(1, 0.5919117))
  this_data <- data.table::data.table("Age" = c("k", 15), "sex" = c("m", "f"))
  expect_error(map_eq5d5Lto3L_VanHout(this_data, NA))
  trial_data <- data.frame(
    "qol.MO" = c(1, 2), "qol.SC" = c(1, 8), "qol.UA" = c(1, 2),
    "qol.PD" = c(1, 2), "qol.AD" = c(1, 2)
  )
  expect_error(map_eq5d5Lto3L_VanHout(trial_data, NA))
  #Error - data should not be NULL
  expect_error(map_eq5d5Lto3L_VanHout(NULL, NA))
  trialdatafile <- system.file("extdata", "trial_data_sampleEq5d.csv",
                               package = "packDAMipd")
  trial_data <- read.csv(trialdatafile)
  results <- map_eq5d5Lto3L_VanHout(trial_data, NA)
  expect_equal(results$EQ5D3L_from5L[1], 0.1311, tol = 1e-4)

  trial_data <- data.frame(
    "qol.MO" = c(1, 2, NA), "qol.SC" = c(1, 2, 3), "qol.UA" = c(1, 2, 3),
    "qol.PD" = c(1, 2, 3), "qol.AD" = c(1, 2, 3)
  )
  results <- map_eq5d5Lto3L_VanHout(trial_data, NA)
  expect_equal(results$EQ5D3L_from5L, c(1, 0.5919117, NA))

  trial_data <- data.frame(
    "qol.MO" = c(1, 2, -99), "qol.SC" = c(1, 2, 3), "qol.UA" = c(1, 2, 3),
    "qol.PD" = c(1, 2, 3), "qol.AD" = c(1, 2, 3)
  )
  results <- map_eq5d5Lto3L_VanHout(trial_data, -99)
  expect_equal(results$EQ5D3L_from5L, c(1, 0.5919117, -99))
})
##############################################################################

context("testing conversion of ADL responses to scores")
test_that("testing conversion of ADL responses to scores", {
  trial_data <- data.frame(
    "tpi.q1" = c(1, 2), "tpi.q2" = c(1, 2), "tpi.q3" = c(1, 2),
    "tpi.q4" = c(1, 2),
    "tpi.q5" = c(1, 2), "tpi.q6" = c(1, 2), "tpi.q7" = c(1, 2),
    "tpi.q8" = c(1, 2)
  )

  results <- value_ADL_scores_IPD(trial_data, c("tpi"), NA, adl_scoring)
  expect_equal(results$ADLTscore, c(40.7, 55.8))

  trial_data <- data.frame(
    "tpi.q1" = c(1, 2, NA), "tpi.q2" = c(1, 2, 3), "tpi.q3" = c(1, 2, 3),
    "tpi.q4" = c(1, 2, 3),
    "tpi.q5" = c(1, 2, 3), "tpi.q6" = c(1, 2, 3), "tpi.q7" = c(1, 2, 3),
    "tpi.q8" = c(1, 2, 3)
  )
  results <- value_ADL_scores_IPD(trial_data, c("tpi"), NA, adl_scoring)
  expect_equal(results$ADLTscore, c(40.7, 55.8, NA))

  trial_data <- data.frame(
    "tpi.q1" = c(1, 2, 99), "tpi.q2" = c(1, 2, 3), "tpi.q3" = c(1, 2, 3),
    "tpi.q4" = c(1, 2, 3),
    "tpi.q5" = c(1, 2, 3), "tpi.q6" = c(1, 2, 3), "tpi.q7" = c(1, 2, 3),
    "tpi.q8" = c(1, 2, 3)
  )
  results <- value_ADL_scores_IPD(trial_data, c("tpi"), 99, adl_scoring)
  expect_equal(results$ADLTscore, c(40.7, 55.8, 99))

  this_data <- data.table::data.table("Age" = c("k", 15), "sex" = c("m", "f"))
  expect_error(value_ADL_scores_IPD(this_data, NA, NA, NA))
  trial_data <- data.frame(
    "qol.MO" = c(1, 2), "qol.SC" = c(1, 8), "qol.UA" = c(1, 2),
    "qol.PD" = c(1, 2), "qol.AD" = c(1, 2)
  )
  expect_error(value_ADL_scores_IPD(trial_data, NA, NA))
  #Error data should not be NULL
  expect_error(value_ADL_scores_IPD(NULL, c("tpi"), NA, adl_scoring))

  #Error no matching columns
  expect_error(value_ADL_scores_IPD(trial_data, NULL, NA, adl_scoring))

  trialdatafile <- system.file("extdata", "trial_data_sampleEq5d.csv",
                               package = "packDAMipd")
  trial_data <- read.csv(trialdatafile)

  results <- value_ADL_scores_IPD(trial_data, c("tpi"), NA, NULL)
  expect_equal(results$ADLTscore[1], 60.2, tol = 1e-4)

  trialdatafile <- system.file("extdata", "trial_data_sample_notenoughcol.csv",
                               package = "packDAMipd")
  trial_data <- read.csv(trialdatafile)
  # Error - ADL should have  columns
  expect_error(value_ADL_scores_IPD(trial_data, c("tpi"), NA, adl_scoring))

  trialdatafile <- system.file("extdata", "trial_data_sample_error.csv",
                               package = "packDAMipd")
  trial_data <- read.csv(trialdatafile)
  # Error - ADL responses do not seem right
  expect_error(value_ADL_scores_IPD(trial_data, c("tpi"), NA, adl_scoring))

  trial_data <- data.frame(
    "tpi.q1" = c(1, 2, 3), "tpi.q2" = c(1, 2, 3), "tpi.q3" = c(1, 2, 3),
    "tpi.q4" = c(1, 2, 3),
    "tpi.q5" = c(1, 2, 3), "tpi.q6" = c(1, 2, NA), "tpi.q7" = c(1, 2, 3),
    "tpi.q8" = c(1, 2, NA)
  )
  results <- value_ADL_scores_IPD(trial_data, c("tpi"), NA, NULL)
  expect_equal(results$ADLTscore, c(40.7, 55.8, NA))

  trial_data <- data.frame(
    "tpi.q1" = c(1, NA, 3, 4), "tpi.q2" = c(1, 2, 3, 4), "tpi.q3" = c(1, 2, 3, 4),
    "tpi.q4" = c(1, 2, 3, 4),
    "tpi.q5" = c(1, 2, 3, 4), "tpi.q6" = c(1, 2, NA, 4), "tpi.q7" = c(1, 2, 3, 4),
    "tpi.q8" = c(1, 2, 3, 4)
  )
   results <- value_ADL_scores_IPD(trial_data, c("tpi"), NA, NULL)
  expect_equal(results$ADLTscore, c(40.7, NA, NA, 66.9))


  trial_data <- data.frame(
    "tpi.q1" = c(1, -99, 3, 4), "tpi.q2" = c(1, 2, 3, 4), "tpi.q3" = c(1, 2, 3, 4),
    "tpi.q4" = c(1, 2, 3, 4),
    "tpi.q5" = c(1, 2, 3, 4), "tpi.q6" = c(1, 2, -99, 4), "tpi.q7" = c(1, 2, 3, 4),
    "tpi.q8" = c(1, 2, 3, 4)
  )
  results <- value_ADL_scores_IPD(trial_data, c("tpi"), -99, NULL)
  expect_equal(results$ADLTscore, c(40.7, -99, -99, 66.9))
})
###############################################################################

context("testing conversion of ADL responses to scores ")
test_that("testing adding EQ5D5L values to the data", {
  trial_data <- data.frame(
    "shows.q1" = c(1, 2), "shows.q2" = c(1, 2), "shows.q3" = c(1, 2),
    "shows.q4" = c(1, 2), "shows.q5" = c(1, 2), "shows.q6" = c(1, 2),
    "shows.q7" = c(1, 2), "shows.q8" = c(1, 2), "shows.q9" = c(1, 2),
    "shows.q10" = c(1, 2)
  )
  results <- value_Shows_IPD(trial_data, c("shows"), NA)
  expect_equal(results$ShOWSscore, c(0, 10))
  this_data <- data.table::data.table("Age" = c("k", 15), "sex" = c("m", "f"))
  expect_error(value_Shows_IPD(this_data, NA, NA))
  trial_data <- data.frame(
    "qol.MO" = c(1, 2), "qol.SC" = c(1, 8), "qol.UA" = c(1, 2),
    "qol.PD" = c(1, 2), "qol.AD" = c(1, 2)
  )
  expect_error(value_Shows_IPD(trial_data, NA, NA))
  #Error data should not be NULL
  expect_error(value_Shows_IPD(NULL, NA, NA))

  trialdatafile <- system.file("extdata", "trial_data_sampleEq5d.csv",
                               package = "packDAMipd")
  trial_data <- read.csv(trialdatafile)
  results <- value_Shows_IPD(trial_data, c("qsy"), NA)
  expect_equal(results$ShOWSscore[1], 16, tol = 1e-4)

  trialdatafile <- system.file("extdata", "trial_data_sample_notenoughcol.csv",
                               package = "packDAMipd")
  trial_data <- read.csv(trialdatafile)
  # Error - Shows should have 10  columns
  expect_error(value_Shows_IPD(trial_data, c("qsy"), NA))

  trialdatafile <- system.file("extdata", "trial_data_sample_error.csv",
                               package = "packDAMipd")
  trial_data <- read.csv(trialdatafile)
  # Error - shows responses do not seem right
  expect_error(value_Shows_IPD(trial_data, c("qsy"), NA))
  trial_data <- data.frame(
    "shows.q1" = c(1, 2, 3), "shows.q2" = c(1, 2, NA), "shows.q3" = c(1, 2, 3),
    "shows.q4" = c(1, 2, 3), "shows.q5" = c(1, 2, 3), "shows.q6" = c(1, 2, 3),
    "shows.q7" = c(1, 2, 3), "shows.q8" = c(1, 2, 3), "shows.q9" = c(1, 2, 3),
    "shows.q10" = c(1, 2, 3)
  )
  results <- value_Shows_IPD(trial_data, c("shows"), NA)
  expect_equal(results$ShOWSscore, c(0, 10, NA))

  trial_data <- data.frame(
    "shows.q1" = c(1, 2, 3), "shows.q2" = c(1, 2, -99), "shows.q3" = c(1, 2, 3),
    "shows.q4" = c(1, 2, 3), "shows.q5" = c(1, 2, 3), "shows.q6" = c(1, 2, 3),
    "shows.q7" = c(1, 2, 3), "shows.q8" = c(1, 2, 3), "shows.q9" = c(1, 2, 3),
    "shows.q10" = c(1, 2, 3)
  )
  results <- value_Shows_IPD(trial_data, c("shows"), -99)
  expect_equal(results$ShOWSscore, c(0, 10, -99))

})
#####################################################
context("testing conversion of promis3a responses to scores")
test_that("testing conversion of promis3a responses to scores", {
  trial_data <- data.frame(
    "tpi.q1" = c(1, 2), "tpi.q2" = c(1, 2), "tpi.q3" = c(1, 2)
  )
  results <- value_promis3a_scores_IPD(trial_data, c("tpi"), NA, NULL)
  expect_equal(results$promis3aTscore, c(36.3, 51.4))
  this_data <- data.table::data.table("Age" = c("k", 15),
                                      "sex" = c("m", "f"))
  expect_error(value_promis3a_scores_IPD(this_data, NA, NA, NA))
  trial_data <- data.frame(
    "qol.MO" = c(1, 2), "qol.SC" = c(1, 8), "qol.UA" = c(1, 2),
    "qol.PD" = c(1, 2), "qol.AD" = c(1, 2)
  )
  expect_error(value_promis3a_scores_IPD(trial_data, NA, NA))
  #Error data should not be NULL
  expect_error(value_promis3a_scores_IPD(NULL, c("tpi"), NA, NULL))

  #Error no matching columns
  expect_error(value_promis3a_scores_IPD(trial_data, NULL, NA, NULL))

  trial_data <- data.frame(
    "tpi.q1" = c(1, 2, NA), "tpi.q2" = c(1, 2, 3), "tpi.q3" = c(1, 2, 2)
  )
  results <- value_promis3a_scores_IPD(trial_data, c("tpi"), NA, NULL)
  expect_equal(results$promis3aTscore, c(36.3, 51.4, NA))

  trial_data <- data.frame(
    "tpi.q1" = c(1, 2, 99), "tpi.q2" = c(1, 2, 3), "tpi.q3" = c(1, 2, 2)
  )
  results <- value_promis3a_scores_IPD(trial_data, c("tpi"), 99, NULL)
  expect_equal(results$promis3aTscore, c(36.3, 51.4, 99))

  trial_data <- data.frame(
    "tpi.q1" = c(1, 2), "tpi.q2" = c(1, 2), "tpi.q3" = c(1, 9)
  )
  expect_error(value_promis3a_scores_IPD(trial_data, c("tpi"), NA, NULL))
  trial_data <- data.frame(
    "tpi.q1" = c(1, 2), "tpi.q2" = c(1, 2),
    "tpi.q3" = c(1, 3), "tpi.q3" = c(1, 3)
  )
  expect_error(value_promis3a_scores_IPD(trial_data, c("tpi"), NA, NULL))

  trial_data <- data.frame(
    timepoint = c(1, 1, 2, 2), "tpi.q1" = c(1, 2, 2, 99),
    "tpi.q2" = c(1, 2, 2, 3), "tpi.q3" = c(1, 2, 2, 3)
  )
  results <- value_promis3a_scores_IPD(trial_data, c("tpi"), 99, NULL)
  expect_equal(results$promis3aTscore, c(36.3, 51.4, 51.4, 99))

})
