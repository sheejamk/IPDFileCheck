###############################################################################
context("testing getting the trial arm details")
test_that("testing getting the trial arm details", {
  details <- get_trial_arm_details(data.table::data.table("Age" = c(21, 15),
                                  "arm" = c("control", "intervention")))
  expect_equal(details$name, "arm")
  expect_equal(details$codes, c("control", "intervention"))
  # error - no column names that tell infomraiton on arm
  expect_error(get_trial_arm_details(data.table::data.table("Age" = c(21, 15),
                                "control" = c("control", "intervention"))))
  details <- get_trial_arm_details(data.table::data.table("Age" = c(21, 15),
                                  "trial" = c("control", "intervention")))
  expect_equal(details$name, "trial")
  details <- get_trial_arm_details(data.table::data.table("Age" = c(21, 15),
                            "trial" = c("control", "intervention"),
                            "arm" = c(1, 2)))
  expect_equal(details$name, "arm")
  #Error - trial data should not be NULL
  expect_error(get_trial_arm_details(NULL))
})
##############################################################################
context("testing getting the gender details")
test_that("testing getting the gender details", {
  details <- get_gender_details(data.table::data.table("Age" = c(21, 15),
                                                      "sex" = c("m", "f")))
  expect_equal(details$name, "sex")
  expect_equal(details$codes, c("m", "f"))
  details <- get_gender_details(data.table::data.table("Age" = c(21, 15),
                                            "gender" = c("male", "female")))
  expect_equal(details$name, "gender")
  expect_equal(details$codes, c("male", "female"))
  # Error- no matching column found
  expect_error(get_gender_details(data.table::data.table("Age" = c(21, 15),
                              "control" = c("control", "intervention"))))
  # Two matches gets the first match
  details <- get_gender_details(data.table::data.table("Age" = c(21, 15),
                             "sex" = c("m", "f"), "gender" = c("m", "f")))
  expect_equal(details$name, "sex")
  #Error - trial data should not be NULL
  expect_error(get_gender_details(NULL))
})
##############################################################################
context("testing getting the age details")
test_that("testing getting the age details", {
  details <- get_age_details(data.table::data.table("Age" = c(21, 15),
                                                    "sex" = c("m", "f")))
  expect_equal(details$name, "Age")
  expect_equal(details$codes, c(21, 15))
  details <- get_age_details(data.table::data.table("dob" =
                                        c("1997-02-12", "1978-04-01"),
                                        "gender" = c("male", "female")))
  expect_equal(details$name, "dob")
  expect_equal(details$codes, c("1997-02-12", "1978-04-01"))
  # Error - no matching column
  expect_error(get_age_details(data.table::data.table("no" = c(21, 15),
                                "control" = c("control", "intervention"))))

  #Error - trial data should not be NULL
  expect_error(get_age_details(NULL))

  details <- get_age_details(data.table::data.table("Age" = c(20, 30),
                                                    yob = c(2000, 1990),
                                                    "sex" = c("m", "f")))
  expect_equal(details$name, "Age")

})
##############################################################################
context("testing getting the timepoint details")
test_that("testing getting the timepoint details", {
  details <- get_timepoint_details(data.table::data.table("time" = c(21, 15),
                                                     "sex" = c("m", "f")))
  expect_equal(details$name, "time")
  expect_equal(details$codes, c(21, 15))
  details <- get_timepoint_details(data.table::data.table("timepoint" =
                                                            c(21, 15),
                                                      "sex" = c("m", "f")))
  expect_equal(details$name, "timepoint")
  expect_equal(details$codes, c(21, 15))
  details <- get_timepoint_details(data.table::data.table("point" = c(21, 15),
                                                  "control" = c("control",
                                                          "intervention")))
  # no time point in column- returning NA
  expect_identical(details, NA)
  #Error - trial data should not be NULL
  expect_error(get_timepoint_details(NULL))
})
##############################################################################
context("testing getting the outcome details")
test_that("testing getting the outcome details", {
  details <- get_outcome_details(data.table::data.table("qol.MO" = c(1, 2),
                  "qol.PD" = c(1, 2), "qol.AD" = c(1, 2)), "eq5d", "qol", TRUE)
  expect_equal(details$name, c("qol.MO", "qol.PD", "qol.AD"))
  expect_equal(details$codes, c(1, 2))
  details <- get_outcome_details(data.table::data.table("qol1" = c(1, 2),
                                                        "qol2" = c(1, 2)),
                                 "eq5d", "qol", TRUE)
  expect_equal(details$name, c("qol1", "qol2"))
  expect_equal(details$codes, c(1, 2))
  # Error no atching columns
  expect_error(get_outcome_details(data.table::data.table("point" = c(21, 15)),
                                  "eq5d", "qol", TRUE))
  details <- get_outcome_details(data.table::data.table("qol1" = c(1, 2),
                                    "qol2" = c(4, 3)), "eq5d", "qol", TRUE)
  expect_equal(details$name, c("qol1", "qol2"))
  expect_equal(details$codes, c(1, 2, 3, 4))
  #Error - name should not be NULL
  expect_error(get_outcome_details(data.table::data.table("qol.MO" = c(1, 2),
        "qol.PD" = c(1, 2), "qol.AD" = c(1, 2)), NULL, "qol", TRUE))
  #Error - trial data should not be NULL
  expect_error(get_outcome_details(NULL, "eq5d", "qol", TRUE))

  details <- get_outcome_details(data.table::data.table("qol" = c(1, 2)),
                                 "eq5d", "qol", FALSE)
  expect_equal(details$name, c("qol"))


  details <- get_outcome_details(data.table::data.table("qol.MO" = c(1, 2),
                                                        "qol.UA" = c(1, 2)),
                                 "eq5d", "qol", FALSE)
  expect_equal(details$name, c("qol.MO"))
})
###############################################################################
context("testing getting the outcome details")
test_that("testing getting the outcome details", {
  details <- get_eq5d_details(data.table::data.table("MO" = c(1, 2),
                                                     "SC" = c(1, 2),
                                                     "UA" = c(1, 2),
                                                     "PD" = c(1, 2),
                                                     "AD" = c(1, 2)))
  expect_equal(details$name, c("MO", "SC", "UA", "PD", "AD"))
  expect_equal(details$codes, c(1, 2))
  # Error - need to match 5 columns
  expect_error(get_eq5d_details(data.table::data.table("qol1" = c(1, 2),
                                                       "qol2" = c(1, 2))))
  # Error - no matching columns
  expect_error(get_eq5d_details(data.table::data.table("point" = c(21, 15))))

  #Error - trial data should not be NULL
  expect_error(get_eq5d_details(NULL))

  this_data <- data.table::data.table("Mobility" = c(1, 2),
                         "SelfCare" = c(1, 2),
                         "UsualActivity" = c(1, 2),
                         "PainDepression" = c(1, 2),
                         "Anxiety" = c(1, 2))
  details <- get_eq5d_details(this_data)
  expect_equal(details$codes, c(1, 2))
})

###############################################################################
context("testing get the coding values of a column in data")
test_that("testing get the coding values of a column in data", {
  ans <- get_colnames_codedvalues("arm", "pat_trial_arm", c("Y", "N"), 999)
  expect_equal(ans$variable, "arm")
  expect_equal(ans$nonrescode, "999")
  # Error - the name or variable can not be NULL or NA
  expect_error(get_colnames_codedvalues("arm", NULL, c("Y", "N"), 999))
  expect_error(get_colnames_codedvalues(NA, "pat_trial_arm", c("Y", "N"), 999))

  ans <- get_colnames_codedvalues("arm", "pat_trial_arm", NULL)
  expect_equal(ans$variable, "arm")
})
###############################################################################
context("testing check treatment arm")
test_that("testing check treatment arm", {
  expect_equal(check_treatment_arm("control"), 0)
  expect_equal(check_treatment_arm("intervention"), 0)
  expect_error(check_treatment_arm("inter"))
  expect_error(check_treatment_arm("cntrl"))
})
