###############################################################################
context("testing costing resource use")
test_that("testing costing resource use", {
  costs_file <- system.file("extdata", "costs_resource_use.csv",
                            package = "packDAMipd")
  datafile <- system.file("extdata", "resource_use_hc_2.csv",
                          package = "packDAMipd")
  ind_part_data <- load_trial_data(datafile)
  unit_cost_data <- load_trial_data(costs_file)
  # Someone admitted to in patient hospital for 1 day (nhs hospital)
  # another day not in NHS hospital
  # unit_cost_data shows inpatient hospital admission cost £20
  # another patient admitted to in patient hospital for 2 days first time,
  # then 2 days another time- so total 4 times -£80

  part_data <-  ind_part_data[1, ]

  res <- costing_resource_use(part_data, "hospital_admission_1",
                              list("length_1", "length_2"),
                              list("nhs_1", "nhs_2"), "day", unit_cost_data,
                              "Inpatient hospital admissions",
                              "UnitCost", "UnitUsed", NULL, NULL)
  expect_equal(res$totcost_hospital_admission_1, 20, tolerance = 1e-3)

  res <- costing_resource_use(
    ind_part_data[2, ], "hospital_admission_1",
    list("length_1", "length_2"), list("nhs_1", "nhs_2"), "day",
    unit_cost_data, "Inpatient hospital admissions", "UnitCost",
    "UnitUsed", NULL, NULL)
  expect_equal(res$totcost_hospital_admission_1, 80, tolerance = 1e-3)

  expect_error(costing_resource_use(
    ind_part_data[2, ], "hospital_admission_1", list("length_1", "length_2"),
    list("nhs_1"), "day", unit_cost_data, "Inpatient hospital admissions",
    "UnitCost", "UnitUsed", NULL, NULL))

  datafile <- system.file("extdata", "resource_use_hc_2_codes.csv",
                          package = "packDAMipd")
  ind_part_data <- load_trial_data(datafile)
  # Someone admitted to in patient hospital for 1 day (nhs hospital)
  # another day not in NHS hospital
  # unit_cost_data shows inpatient hospital admission cost £20
  # another patient admitted to in patient hospital for 2 days first time,
  # then 2 days another time- so total 4 times -£80
  # testing if we use all codes
  res <- costing_resource_use(
    ind_part_data[1, ],
    "hospital_admission_1",
    list("length_1", "length_2"),
    list("nhs_1", "nhs_2"),
    "day",
    unit_cost_data, "Inpatient hospital admissions", "UnitCost",
    "UnitUsed",
    list(c("yes", "no"), c(1, 2)), list(c("yes", "no"), c(1, 2))
  )
  expect_equal(res$totcost_hospital_admission_1, 20, tolerance = 1e-3)

  res <- costing_resource_use(
    ind_part_data[1, ],
    "daycare",
    list("number"),
    list("nhs_visit"),
    "visit",
    unit_cost_data, "Day care admission", "UnitCost",
    "UnitUsed",
    NULL, list(c("yes", "no"), c(1, 2))
  )
  expect_equal(res$totcost_daycare, 45, tolerance = 1e-3)
  expect_error(costing_resource_use(
    ind_part_data[1, ],
    "daycare",
    list("number"),
    list("visit"),
    "visit",
    unit_cost_data, "Day care admission", "UnitCost",
    "UnitUsed",
    NULL, list(c("yes", "no"), c(1, 2))
  ))
  res <- costing_resource_use(
    ind_part_data[1, ],
    "other_contact",
    list("number_of_contacts"),
    NULL, "visit",
    unit_cost_data, NULL, "UnitCost",
    "UnitUsed",
    NULL, NULL
  )
  expect_equal(res$totcost_other_contact, 39.23, tolerance = 1e-3)

  res <- costing_resource_use(
    ind_part_data[2, ],
    "other_contact",
    list("number_of_contacts"),
    NULL, "hour",
    unit_cost_data, NULL, "UnitCost",
    "UnitUsed",
    NULL, NULL
  )
  # 3 contacts of practice nurse- unit is per hour.
  expect_equal(res$totcost_other_contact, 126, tolerance = 1e-3)
  # data should not be null
  expect_error(costing_resource_use(
    NULL,
    "other_contact",
    list("number_of_contacts"),
    NULL, "hour",
    unit_cost_data, NULL, "UnitCost",
    "UnitUsed",
    NULL, NULL
  ))
  # name should not be null
  expect_error(costing_resource_use(
    ind_part_data[2, ],
    NULL,
    list("number_of_contacts"),
    NULL, "hour",
    unit_cost_data, NULL, "UnitCost",
    "UnitUsed",
    NULL, NULL
  ))
  # unit cost column should not be null
  expect_error(costing_resource_use(
    ind_part_data[2, ],
    "other_contact",
    list("number_of_contacts"),
    NULL, "hour",
    unit_cost_data, NULL, NULL,
    "UnitUsed",
    NULL, NULL
  ))
  # name of resource use not in patient data
  datafile <- system.file("extdata", "resource_use_hc_2_noname.csv",
                          package = "packDAMipd")
  ind_part_data <- load_trial_data(datafile)
  expect_error(costing_resource_use(
    ind_part_data[2, ],
    "other_contact",
    list("number_of_contacts"),
    NULL, "hour",
    unit_cost_data, NULL, "UnitCost",
    "UnitUsed",
    NULL, NULL
  ))
  costs_file <- system.file("extdata", "costs_resource_use_nounitcostcol.csv",
                            package = "packDAMipd")
  datafile <- system.file("extdata", "resource_use_hc_2.csv",
                          package = "packDAMipd")
  ind_part_data <- load_trial_data(datafile)
  unit_cost_data <- load_trial_data(costs_file)
  expect_error(costing_resource_use(
    ind_part_data[2, ],
    "hospital_admission_1",
    list("number_of_contacts"),
    NULL, "hour",
    unit_cost_data, NULL, "UnitCost",
    "UnitUsed",
    NULL, NULL
  ))

  costs_file <- system.file("extdata", "costs_resource_use_nonameres.csv",
                            package = "packDAMipd")
  datafile <- system.file("extdata", "resource_use_hc_2.csv",
                          package = "packDAMipd")
  ind_part_data <- load_trial_data(datafile)
  unit_cost_data <- load_trial_data(costs_file)
  expect_error(costing_resource_use(
    ind_part_data[2, ],
    "hospital_admission_1",
    list("number_of_contacts"),
    NULL, "hour",
    unit_cost_data, NULL, "UnitCost",
    "UnitUsed",
    NULL, NULL
  ))
  costs_file <- system.file("extdata", "costs_resource_use_nohos.csv",
                            package = "packDAMipd")
  datafile <- system.file("extdata", "resource_use_hc_2.csv",
                          package = "packDAMipd")
  ind_part_data <- load_trial_data(datafile)
  unit_cost_data <- load_trial_data(costs_file)
  expect_error(costing_resource_use(
    ind_part_data[2, ],
    "hospital_admission_1",
    list("number_of_contacts"),
    NULL, "hour",
    unit_cost_data, NULL, "UnitCost",
    "UnitUsed",
    NULL, NULL
  ))

  costs_file <- system.file("extdata", "costs_resource_use.csv",
                            package = "packDAMipd")
  datafile <- system.file("extdata", "resource_use_hc_2_unitlength.csv",
                          package = "packDAMipd")
  ind_part_data <- load_trial_data(datafile)
  unit_cost_data <- load_trial_data(costs_file)
  # unit length is in individual data
  res <- costing_resource_use(
    ind_part_data,
    "hospital_admission_1",
    list("length_1", "length_2"),
    list("nhs_1", "nhs_2"),
    "unit_length",
    unit_cost_data, "Inpatient hospital admissions", "UnitCost",
    "UnitUsed",
    NULL, NULL
  )
  expect_equal(res$totcost_hospital_admission_1, c(20, 80, 0, 0, 0))
  #units of resource use expressed and calculated are different
  costs_file <- system.file("extdata",
                            "costs_resource_use_unitexpressed_notright.csv",
                            package = "packDAMipd")
  datafile <- system.file("extdata", "resource_use_hc_2_unitlength.csv",
                          package = "packDAMipd")
  ind_part_data <- load_trial_data(datafile)
  unit_cost_data <- load_trial_data(costs_file)
  # unit length is in individual data
  expect_error(costing_resource_use(
    ind_part_data,
    "hospital_admission_1",
    list("length_1", "length_2"),
    list("nhs_1", "nhs_2"),
    "unit_length",
    unit_cost_data, "Inpatient hospital admissions", "UnitCost",
    "UnitUsed",
    NULL, NULL
  ))
})
###############################################################################
context("testing extracting unit cost matching hrg code")
test_that("testing extracting unit cost matching hrg code", {
  ref_cost_data_file <- system.file("extdata",
                                    "National_schedule_of_NHS_costs_2019.csv",
                                    package = "packDAMipd")
  result <- get_cost_ip_dc_hrg("AA22C", ref_cost_data_file, "Currency_Code",
                                   "National_Average_Unit_Cost")

  expect_equal(result, 5053, tol = 1e-1)
  expect_error(get_cost_ip_dc_hrg(NULL, ref_cost_data_file,
                                      "Currency_Code",
                                      "National_Average_Unit_Cost", "EL"))
  expect_error(get_cost_ip_dc_hrg("AA22C", NULL, "Currency_Code",
                                      "National_Average_Unit_Cost", "EL"))
  expect_error(get_cost_ip_dc_hrg("AA22C", ref_cost_data_file, NULL,
                                      "National_Average_Unit_Cost", "EL"))
  expect_error(get_cost_ip_dc_hrg("AA22C", ref_cost_data_file,
                                      "Currency_Code",
                                      NULL, "EL"))
  result <- get_cost_ip_dc_hrg("AA22C", ref_cost_data_file,
                         "Currency_Code",
                         "National_Average_Unit_Cost", NULL)
  expect_equal(result, 5053, tol = 1e-1)
  expect_error(get_cost_ip_dc_hrg("AA", ref_cost_data_file,
                                      "Currency_Code",
                                      "National_Average_Unit_Cost", "EL"))

})
###############################################################################
context("testing extracting unit cost matching description")
test_that("testing extracting unit cost matching description", {
  ref_cost_data_file <- system.file("extdata",
                                    "National_schedule_of_NHS_costs_2019.csv",
                                    package = "packDAMipd")
  result <- get_cost_ip_dc_description("Cerebrovascular Accident",
                                           ref_cost_data_file,
                                           "Currency_Description",
                                           "National_Average_Unit_Cost", "EL")
  expect_equal(result, 3530, tol = 1e-1)
  expect_error(get_cost_ip_dc_description(NULL, ref_cost_data_file,
                                              "Currency_Description",
                                              "National_Average_Unit_Cost",
                                              "EL"))
  expect_error(get_cost_ip_dc_description("Cerebrovascular Accident", NULL,
                                              "Currency_Description",
                                              "National_Average_Unit_Cost",
                                              "EL"))
  expect_error(get_cost_ip_dc_description("Cerebrovascular Accident",
                                              ref_cost_data_file, NULL,
                                              "National_Average_Unit_Cost",
                                              "EL"))
  expect_error(get_cost_ip_dc_description("Cerebrovascular Accident",
                                              ref_cost_data_file,
                                              "Currency_Description",
                                              NULL, "EL"))
  result <- get_cost_ip_dc_description("Cerebrovascular Accident",
                                 ref_cost_data_file,
                                 "Currency_Description",
                                 "National_Average_Unit_Cost", NULL)
  expect_equal(result, 3530, tol = 1e-1)
  expect_error(get_cost_ip_dc_description("hello",
                                              ref_cost_data_file,
                                              "Currency_Description",
                                              "National_Average_Unit_Cost",
                                              "EL"))
})
# # ###############################################################################
context("testing costing inpatient admission")
test_that("testing costing inpatient admission", {
  costs_file <- system.file("extdata", "patient_adm_cost_EL.xlsx",
                            package = "packDAMipd")
  datafile <- system.file("extdata", "resource_use_hc_ip.csv",
                          package = "packDAMipd")
  ind_part_data <- packDAMipd::load_trial_data(datafile)
  unit_cost_data <- packDAMipd::load_trial_data(costs_file)
  result <- costing_inpatient_daycase_admission(ind_part_data,
                                  hrg_code_ip_admi = "HRGcode",
                                  descrip_ip_admi = NULL,
                                  number_use_ip_admi = "number_use",
                                  elective_col = "EL",
                                  unit_cost_data,
                                  hrg_code_col = "Currency_Code",
                                  description_col = NULL,
                                  unit_cost_col = "National_Average_Unit_Cost",
                                  cost_calculated_in = "admission")

  expect_equal(result$totcost_ip_admission[1], 5053, tol = 1e-2)

  expect_error(costing_inpatient_daycase_admission(NULL,
                                  hrg_code_ip_admi = "HRGcode",
                                  descrip_ip_admi = NULL,
                                  number_use_ip_admi = "number_use",
                                  elective_col = "EL",
                                  unit_cost_data,
                                  hrg_code_col = "Currency_Code",
                                  description_col = NULL,
                                  unit_cost_col = "National_Average_Unit_Cost",
                                  cost_calculated_in = "admission"))

  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                      hrg_code_ip_admi = "HRGcode",
                                      descrip_ip_admi = NULL,
                                      number_use_ip_admi = "number_use",
                                      elective_col = "EL",
                                      unit_cost_data,
                                      hrg_code_col = NULL,
                                      description_col = NULL,
                                      unit_cost_col =
                                        "National_Average_Unit_Cost",
                                      cost_calculated_in = "admission"))

  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                 hrg_code_ip_admi = NULL,
                                 descrip_ip_admi = NULL,
                                 number_use_ip_admi = "number_use",
                                 elective_col = "EL",
                                 unit_cost_data,
                                 hrg_code_col = "Currency_Code",
                                 description_col = NULL,
                                  unit_cost_col = "National_Average_Unit_Cost",
                                  cost_calculated_in = "admission"))

  result <- costing_inpatient_daycase_admission(ind_part_data,
                                        hrg_code_ip_admi = "HRGcode",
                                        descrip_ip_admi = NULL,
                                        number_use_ip_admi = NULL,
                                        elective_col = "EL",
                                        unit_cost_data,
                                        hrg_code_col = "Currency_Code",
                                        description_col = NULL,
                                        unit_cost_col =
                                        "National_Average_Unit_Cost",
                                        cost_calculated_in = "admission")
  expect_equal(result$totcost_ip_admission[1], 5053, tol = 1e-2)

  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                           hrg_code_ip_admi = "HRGcode",
                                           descrip_ip_admi = NULL,
                                           number_use_ip_admi = "number_use",
                                           elective_col = "EL",
                                           NULL,
                                           hrg_code_col = "Currency_Code",
                                           description_col = NULL,
                                           unit_cost_col =
                                             "National_Average_Unit_Cost",
                                           cost_calculated_in = "admission"))

  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                           hrg_code_ip_admi = "HRGcode",
                                           descrip_ip_admi = NULL,
                                           number_use_ip_admi = "number_use",
                                           elective_col = NULL,
                                           unit_cost_data,
                                           hrg_code_col = "Currency_Code",
                                           description_col = NULL,
                                           unit_cost_col =
                                             "National_Average_Unit_Cost",
                                           cost_calculated_in = "admission"))


  result <- costing_inpatient_daycase_admission(ind_part_data,
                                        hrg_code_ip_admi = "HRGcode",
                                        descrip_ip_admi = NULL,
                                        number_use_ip_admi = "number_use",
                                        elective_col = "EL",
                                        unit_cost_data,
                                        hrg_code_col = "Currency_Code",
                                        description_col = NULL,
                                        unit_cost_col =
                                          "National_Average_Unit_Cost",
                                        cost_calculated_in = "admission")
  expect_equal(result$totcost_ip_admission[1], 5053, tol = 1e-2)
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                           hrg_code_ip_admi = "HRGcode",
                                           descrip_ip_admi = NULL,
                                           number_use_ip_admi = "number_use",
                                           elective_col = "EL",
                                           unit_cost_data,
                                           hrg_code_col = "Currency_Code",
                                           description_col = NULL,
                                           unit_cost_col = NULL,
                                           cost_calculated_in = "admission"))
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                           hrg_code_ip_admi = "HRGcode",
                                           descrip_ip_admi = NULL,
                                           number_use_ip_admi = "number_use",
                                           elective_col = "EL",
                                           unit_cost_data,
                                           hrg_code_col = "Currency_Code",
                                           description_col = NULL,
                                           unit_cost_col =
                                             "National_Average_Unit_Cost",
                                           cost_calculated_in = NULL))
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                           hrg_code_ip_admi = "HRGcode",
                                           descrip_ip_admi = NULL,
                                           number_use_ip_admi = "number_use",
                                           elective_col = "EL",
                                           unit_cost_data,
                                           hrg_code_col = "Currency_Code",
                                           description_col = NULL,
                                           unit_cost_col =
                                             "National_Average_Unit_Cost",
                                           cost_calculated_in = "ad"))
  result <- costing_inpatient_daycase_admission(ind_part_data,
                                        hrg_code_ip_admi = NULL,
                                        descrip_ip_admi = "Description",
                                        number_use_ip_admi = "number_use",
                                        elective_col = "EL",
                                        unit_cost_data,
                                        hrg_code_col = NULL,
                                        description_col =
                                          "Currency_Description",
                                        unit_cost_col =
                                          "National_Average_Unit_Cost",
                                        cost_calculated_in = "admission")
  expect_equal(result$totcost_ip_admission[1], 3530, tol = 1e-2)
  datafile <- system.file("extdata", "resource_use_hc_ip_nocol.csv",
                          package = "packDAMipd")

  ind_part_data <- packDAMipd::load_trial_data(datafile)
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                           hrg_code_ip_admi = NULL,
                                           descrip_ip_admi = "Description",
                                           number_use_ip_admi = "number_use",
                                           elective_col = "EL",
                                           unit_cost_data,
                                           hrg_code_col = "Currency_Code",
                                           description_col =
                                             "Currency_Description",
                                           unit_cost_col =
                                             "National_Average_Unit_Cost",
                                           cost_calculated_in = "admission"))
  datafile <- system.file("extdata", "resource_use_hc_ip_nonumuse.csv",
                          package = "packDAMipd")

  ind_part_data <- packDAMipd::load_trial_data(datafile)
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                           hrg_code_ip_admi = NULL,
                                           descrip_ip_admi = "Description",
                                           number_use_ip_admi = "number_use",
                                           elective_col = "EL",
                                           unit_cost_data,
                                           hrg_code_col = "Currency_Code",
                                           description_col =
                                             "Currency_Description",
                                           unit_cost_col =
                                             "National_Average_Unit_Cost",
                                           cost_calculated_in = "admission"))

  datafile <- system.file("extdata", "resource_use_hc_ip_nocols.csv",
                          package = "packDAMipd")

  ind_part_data <- packDAMipd::load_trial_data(datafile)
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                           hrg_code_ip_admi = NULL,
                                           descrip_ip_admi = "Description",
                                           number_use_ip_admi = "number_use",
                                           elective_col = "EL",
                                           unit_cost_data,
                                           hrg_code_col = "Currency_Code",
                                           description_col =
                                             "Currency_Description",
                                           unit_cost_col =
                                             "National_Average_Unit_Cost",
                                           cost_calculated_in = "admission"))

  costs_file <- system.file("extdata",
                            "National_schedule_of_NHS_costs_2019_error.xlsx",
                            package = "packDAMipd")
  datafile <- system.file("extdata", "resource_use_hc_ip.csv",
                          package = "packDAMipd")

  ind_part_data <- packDAMipd::load_trial_data(datafile)
  unit_cost_data <- packDAMipd::load_trial_data(costs_file, sheet = "EL")
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                           hrg_code_ip_admi = NULL,
                                           descrip_ip_admi = "Description",
                                           number_use_ip_admi = "number_use",
                                           elective_col = "EL",
                                           unit_cost_data,
                                           hrg_code_col = "Currency_Code",
                                           description_col =
                                             "Currency_Description",
                                           unit_cost_col =
                                             "National_Average_Unit_Cost",
                                           cost_calculated_in = "admission"))

  costs_file <- system.file("extdata",
                            "National_schedule_of_NHS_costs_2019.csv",
                            package = "packDAMipd")
  datafile <- system.file("extdata", "resource_use_hc_ip.csv",
                          package = "packDAMipd")
  ind_part_data <- packDAMipd::load_trial_data(datafile)
  unit_cost_data <- packDAMipd::load_trial_data(costs_file, sheet = "EL")
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                      hrg_code_ip_admi = "HRGcode",
                                      descrip_ip_admi = "Description",
                                      number_use_ip_admi = "number_use",
                                      elective_col = "EL",
                                      unit_cost_data,
                                      hrg_code_col = NULL,
                                      description_col = "Currency_Description",
                                      unit_cost_col =
                                      "National_Average_Unit_Cost",
                                      cost_calculated_in = "admission"))
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                           hrg_code_ip_admi = NULL,
                                           descrip_ip_admi = "Description",
                                           number_use_ip_admi = "number_use",
                                           elective_col = "EL",
                                           unit_cost_data,
                                           hrg_code_col = "Currency_Code",
                                           description_col = NULL,
                                           unit_cost_col =
                                             "National_Average_Unit_Cost",
                                           cost_calculated_in = "admission"))

  datafile <- system.file("extdata", "resource_use_hc_ip_numuse_wrong.csv",
                          package = "packDAMipd")
  ind_part_data <- packDAMipd::load_trial_data(datafile)
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                           hrg_code_ip_admi = "HRGcode",
                                           descrip_ip_admi = "Description",
                                           number_use_ip_admi = "number_use",
                                           elective_col = "EL",
                                           unit_cost_data,
                                           hrg_code_col = "Currency_Code",
                                           description_col = NULL,
                                           unit_cost_col =
                                             "National_Average_Unit_Cost",
                                           cost_calculated_in = "admission"))
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                      hrg_code_ip_admi = NULL,
                                      descrip_ip_admi = "Description",
                                      number_use_ip_admi = "number_use",
                                      elective_col = "EL",
                                      unit_cost_data,
                                      hrg_code_col = "Currency_Code",
                                      description_col = "Currency_Description",
                                      unit_cost_col =
                                      "National_Average_Unit_Cost",
                                      cost_calculated_in = "admission"))

  datafile <- system.file("extdata", "resource_use_hc_ip_numuse_missing.csv",
                          package = "packDAMipd")
  ind_part_data <- packDAMipd::load_trial_data(datafile)

  result <- costing_inpatient_daycase_admission(ind_part_data,
                                      hrg_code_ip_admi = NULL,
                                      descrip_ip_admi = "Description",
                                      number_use_ip_admi = "number_use",
                                      elective_col = "EL",
                                      unit_cost_data,
                                      hrg_code_col = "Currency_Code",
                                      description_col = "Currency_Description",
                                      unit_cost_col =
                                      "National_Average_Unit_Cost",
                                      cost_calculated_in = "admission")
  expect_equal(result$totcost_ip_admission[1], 3530, tol = 1e-2)
  result <- costing_inpatient_daycase_admission(ind_part_data,
                                        hrg_code_ip_admi = "HRGcode",
                                        descrip_ip_admi = "Description",
                                        number_use_ip_admi = "number_use",
                                        elective_col = "EL",
                                        unit_cost_data,
                                        hrg_code_col = "Currency_Code",
                                        description_col = NULL,
                                        unit_cost_col =
                                          "National_Average_Unit_Cost",
                                        cost_calculated_in = "admission")
  expect_equal(result$totcost_ip_admission[1], 5053, tol = 1e-2)

  datafile <- system.file("extdata", "resource_use_hc_ip_el_missing.csv",
                          package = "packDAMipd")
  ind_part_data <- packDAMipd::load_trial_data(datafile)
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                    hrg_code_ip_admi = NULL,
                                    descrip_ip_admi = "Description",
                                    number_use_ip_admi = NULL,
                                    elective_col = "EL",
                                    unit_cost_data,
                                    hrg_code_col = "Currency_Code",
                                    description_col = "Currency_Description",
                                    unit_cost_col =
                                            "National_Average_Unit_Cost",
                                     cost_calculated_in = "admission"))
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                            hrg_code_ip_admi = "HRGcode",
                                            descrip_ip_admi = "Description",
                                            number_use_ip_admi = NULL,
                                            elective_col = "EL",
                                            unit_cost_data,
                                            hrg_code_col = "Currency_Code",
                                            description_col = NULL,
                                            unit_cost_col =
                                               "National_Average_Unit_Cost",
                                            cost_calculated_in = "admission"))

  datafile <- system.file("extdata", "resource_use_hc_ip_noelcol.csv",
                          package = "packDAMipd")
  ind_part_data <- packDAMipd::load_trial_data(datafile)
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                                   hrg_code_ip_admi = NULL,
                                              descrip_ip_admi = "Description",
                                                   number_use_ip_admi = NULL,
                                                   elective_col = "EL",
                                                   unit_cost_data,
                                            hrg_code_col = "Currency_Code",
                                      description_col = "Currency_Description",
                                                   unit_cost_col =
                                              "National_Average_Unit_Cost",
                                            cost_calculated_in = "admission"))
  expect_error(costing_inpatient_daycase_admission(ind_part_data,
                                            hrg_code_ip_admi = "HRGcode",
                                            descrip_ip_admi = "Description",
                                                   number_use_ip_admi = NULL,
                                                   elective_col = "EL",
                                                   unit_cost_data,
                                            hrg_code_col = "Currency_Code",
                                                   description_col = NULL,
                                                   unit_cost_col =
                                              "National_Average_Unit_Cost",
                                            cost_calculated_in = "admission"))


})
###############################################################################
context("testing extracting unit cost for A&E matching code")
test_that("testing extracting unit cost for A&E matching  code", {
  ref_cost_data_file <- system.file("extdata",
                          "National_schedule_of_NHS_costs_2019_AandE.csv",
                          package = "packDAMipd")
  re <- get_cost_AandE_code("VB02Z", "T01A", ref_cost_data_file,
                            "Currency_Code",
                    "National_Average_Unit_Cost", "Service_Code")
  expect_equal(re, 452, tol = 1e-2)
  ref_cost_data_file <- system.file("extdata",
                          "NHS_costs_2019_AandE.xlsx",
                           package = "packDAMipd")
  re <- get_cost_AandE_code("VB02Z", "T01A", ref_cost_data_file,
                            "Currency_Code",
                            "National_Average_Unit_Cost",
                            "Service_Code", sheet = "AE")
  expect_equal(re, 452, tol = 1e-2)
  expect_error(get_cost_AandE_code("VB02Z", "T00A", ref_cost_data_file,
                      "Currency_Code",
                      "National_Average_Unit_Cost",
                      "Service_Code", sheet = "AE"))
  expect_error(get_cost_AandE_code(NULL, "T01A", ref_cost_data_file,
                                   "Currency_Code",
                                   "National_Average_Unit_Cost",
                                   "Service_Code"))
  expect_error(get_cost_AandE_code("VB02Z", NA, ref_cost_data_file,
                                   "Currency_Code",
                                   "National_Average_Unit_Cost",
                                   "Service_Code"))
  expect_error(get_cost_AandE_code("VB02Z", "T01A", NULL,
                                   "Currency_Code",
                                   "National_Average_Unit_Cost",
                                   "Service_Code"))
  expect_error(get_cost_AandE_code("VB02Z", "T01A", ref_cost_data_file,
                                   NA, "National_Average_Unit_Cost",
                                   "Service_Code"))
  expect_error(get_cost_AandE_code("VB02Z", "T01A", ref_cost_data_file,
                                   "Currency_Code", NA, "Service_Code"))
  expect_error(get_cost_AandE_code("VB02Z", "T01A", ref_cost_data_file,
                                   "Currency_Code",
                                   "National_Average_Unit_Cost", NULL))
})
###############################################################################

context("testing extracting unit cost matching description")
test_that("testing extracting unit cost matching description", {
  ref_cost_data_file <- system.file("extdata",
                              "National_schedule_of_NHS_costs_2019_AandE.csv",
                               package = "packDAMipd")
  re <- get_cost_AandE_description("Emergency Medicine", "T01A",
                                  ref_cost_data_file,
                                "Currency_Description",
                                "National_Average_Unit_Cost", "Service_Code")
  expect_equal(re, 265.33, tol = 1e-2)
  ref_cost_data_file <- system.file("extdata",
                                    "NHS_costs_2019_AandE.xlsx",
                                    package = "packDAMipd")
  re <- get_cost_AandE_description("Emergency Medicine", "T01A",
                            ref_cost_data_file,
                            "Currency_Description",
                            "National_Average_Unit_Cost", "Service_Code",
                            sheet = "AE")
  expect_equal(re, 265.33, tol = 1e-2)
  expect_error(get_cost_AandE_description("Emergency Medicine", "T00A",
                             ref_cost_data_file,
                             "Currency_Description",
                             "National_Average_Unit_Cost", "Service_Code",
                             sheet = "AE"))
  expect_error(get_cost_AandE_description(NULL, "T01A",
                                          ref_cost_data_file,
                                          "Currency_Description",
                                          "National_Average_Unit_Cost",
                                          "Service_Code"))
  expect_error(get_cost_AandE_description("Emergency Medicine", NULL,
                                          ref_cost_data_file,
                                          "Currency_Description",
                                          "National_Average_Unit_Cost",
                                          "Service_Code"))
  expect_error(get_cost_AandE_description("Emergency Medicine", "T01A",
                                          NULL,
                                          "Currency_Description",
                                          "National_Average_Unit_Cost",
                                          "Service_Code"))
  expect_error(get_cost_AandE_description("Emergency Medicine", "T01A",
                                          ref_cost_data_file,
                                          NULL,
                                          "National_Average_Unit_Cost",
                                          "Service_Code"))
  expect_error(get_cost_AandE_description("Emergency Medicine", "T01A",
                                          ref_cost_data_file,
                                          "Currency_Description",
                                          NULL,
                                          "Service_Code"))
  expect_error(get_cost_AandE_description("Emergency Medicine", "T01A",
                                          ref_cost_data_file,
                                          "Currency_Description",
                                          "National_Average_Unit_Cost",
                                          NULL))


})
############################################################################

context("testing extracting unit cost matching description")
test_that("testing extracting unit cost matching description", {
  costs_file <- system.file("extdata",
                          "National_schedule_of_NHS_costs_2019_AandE.csv",
                          package = "packDAMipd")
  datafile <- system.file("extdata", "resource_use_ae_ip.csv",
                          package = "packDAMipd")
  ind_part_data <- packDAMipd::load_trial_data(datafile)
  unit_cost_data <- packDAMipd::load_trial_data(costs_file)


 result <- costing_AandE_admission(ind_part_data = ind_part_data,
                                   code_ae = "code", descrip_ae = NULL,
                                   number_use_ae = "number_use",
                                   type_admit_ae = "type_admit",
                                   unit_cost_data = unit_cost_data,
                                   code_col = "Currency_Code",
                                   type_admit_col = "Service_Code",
                                   description_col = NULL,
                                unit_cost_col = "National_Average_Unit_Cost",
                                   cost_calculated_in = "attendance")

 expect_equal(result$totcost_AE_Admission[1], 672, tol = 1e-2)

 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                         code_ae = "code", descrip_ae = NULL,
                         number_use_ae = "number_use",
                         type_admit_ae = "type_admit",
                         unit_cost_data = unit_cost_data,
                         code_col = NULL,
                         type_admit_col = "Service_Code",
                         description_col = "Currency_Description",
                         unit_cost_col = "National_Average_Unit_Cost",
                         cost_calculated_in = "attendance"))
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                                      code_ae = NULL, descrip_ae = "desc",
                                      number_use_ae = "number_use",
                                      type_admit_ae = "type_admit",
                                      unit_cost_data = unit_cost_data,
                                      code_col =  "Currency_Code",
                                      type_admit_col = "Service_Code",
                                      description_col = NULL,
                                unit_cost_col = "National_Average_Unit_Cost",
                                      cost_calculated_in = "attendance"))

 expect_error(costing_AandE_admission(ind_part_data = NULL,
                                   code_ae = "code", descrip_ae = NULL,
                                   number_use_ae = "number_use",
                                   type_admit_ae = "type_admit",
                                   unit_cost_data = unit_cost_data,
                                   code_col = "Currency_Code",
                                   type_admit_col = "Service_Code",
                                   description_col = NULL,
                              unit_cost_col = "National_Average_Unit_Cost",
                                   cost_calculated_in = "attendance"))

 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                                   code_ae = NULL, descrip_ae = NULL,
                                   number_use_ae = "number_use",
                                   type_admit_ae = "type_admit",
                                   unit_cost_data = unit_cost_data,
                                   code_col = "Currency_Code",
                                   type_admit_col = "Service_Code",
                                   description_col = NULL,
                            unit_cost_col = "National_Average_Unit_Cost",
                                   cost_calculated_in = "attendance"))

 result <- costing_AandE_admission(ind_part_data = ind_part_data,
                                   code_ae = "code", descrip_ae = NULL,
                                   number_use_ae = NULL,
                                   type_admit_ae = "type_admit",
                                   unit_cost_data = unit_cost_data,
                                   code_col = "Currency_Code",
                                   type_admit_col = "Service_Code",
                                   description_col = NULL,
                            unit_cost_col = "National_Average_Unit_Cost",
                                   cost_calculated_in = "attendance")
 expect_equal(result$totcost_AE_Admission[1], 672, tol = 1e-2)
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                                   code_ae = "code", descrip_ae = NULL,
                                   number_use_ae = "number_use",
                                   type_admit_ae = NULL,
                                   unit_cost_data = unit_cost_data,
                                   code_col = "Currency_Code",
                                   type_admit_col = "Service_Code",
                                   description_col = NULL,
                            unit_cost_col = "National_Average_Unit_Cost",
                                   cost_calculated_in = "attendance"))
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                                   code_ae = "code", descrip_ae = NULL,
                                   number_use_ae = "number_use",
                                   type_admit_ae = "type_admit",
                                   unit_cost_data = NULL,
                                   code_col = "Currency_Code",
                                   type_admit_col = "Service_Code",
                                   description_col = NULL,
                              unit_cost_col = "National_Average_Unit_Cost",
                                   cost_calculated_in = "attendance"))
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                                   code_ae = "code", descrip_ae = NULL,
                                   number_use_ae = "number_use",
                                   type_admit_ae = "type_admit",
                                   unit_cost_data = unit_cost_data,
                                   code_col = "Currency_Code",
                                   type_admit_col = NULL,
                                   description_col = NULL,
                              unit_cost_col = "National_Average_Unit_Cost",
                                   cost_calculated_in = "attendance"))
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                                   code_ae = "code", descrip_ae = NULL,
                                   number_use_ae = "number_use",
                                   type_admit_ae = "type_admit",
                                   unit_cost_data = unit_cost_data,
                                   code_col = NULL,
                                   type_admit_col = "Service_Code",
                                   description_col = NULL,
                            unit_cost_col = "National_Average_Unit_Cost",
                                   cost_calculated_in = "attendance"))
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                                   code_ae = "code", descrip_ae = NULL,
                                   number_use_ae = "number_use",
                                   type_admit_ae = "type_admit",
                                   unit_cost_data = unit_cost_data,
                                   code_col = "Currency_Code",
                                   type_admit_col = "Service_Code",
                                   description_col = NULL,
                                   unit_cost_col = NULL,
                                   cost_calculated_in = "attendance"))
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                                   code_ae = "code", descrip_ae = NULL,
                                   number_use_ae = "number_use",
                                   type_admit_ae = "type_admit",
                                   unit_cost_data = unit_cost_data,
                                   code_col = "Currency_Code",
                                   type_admit_col = "Service_Code",
                                   description_col = NULL,
                              unit_cost_col = "National_Average_Unit_Cost",
                                   cost_calculated_in = "ss"))

 result <- costing_AandE_admission(ind_part_data = ind_part_data,
                                   code_ae = NULL, descrip_ae = "desc",
                                   number_use_ae = "number_use",
                                   type_admit_ae = "type_admit",
                                   unit_cost_data = unit_cost_data,
                                   code_col = "Currency_Code",
                                   type_admit_col = "Service_Code",
                                   description_col = "Currency_Description",
                            unit_cost_col = "National_Average_Unit_Cost",
                                   cost_calculated_in = "attendance")
 expect_equal(result$totcost_AE_Admission[1], 754, tol = 1e-2)

 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                         code_ae = NULL, descrip_ae = "dd",
                         number_use_ae = "number_use",
                         type_admit_ae = "type_admit",
                         unit_cost_data = unit_cost_data,
                         code_col = "Currency_Code",
                         type_admit_col = "Service_Code",
                         description_col = "Currency_Description",
                         unit_cost_col = "National_Average_Unit_Cost",
                         cost_calculated_in = "attendance"))
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                         code_ae = "dd", descrip_ae = NULL,
                         number_use_ae = NULL,
                         type_admit_ae = "type_admit",
                         unit_cost_data = unit_cost_data,
                         code_col = "Currency_Code",
                         type_admit_col = "Service_Code",
                         description_col = NULL,
                         unit_cost_col = "National_Average_Unit_Cost",
                         cost_calculated_in = "attendance"))

 datafile <- system.file("extdata", "resource_use_ae_ip_notypecol.csv",
                         package = "packDAMipd")
 ind_part_data <- packDAMipd::load_trial_data(datafile)
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                         code_ae = "code", descrip_ae = NULL,
                         number_use_ae = "number_use",
                         type_admit_ae = "type_admit",
                         unit_cost_data = unit_cost_data,
                         code_col = "Currency_Code",
                         type_admit_col = "Service_Code",
                         description_col = NULL,
                         unit_cost_col = "National_Average_Unit_Cost",
                         cost_calculated_in = "attendance"))
 datafile <- system.file("extdata", "resource_use_ae_ip_nonumusecol.csv",
                         package = "packDAMipd")
 ind_part_data <- packDAMipd::load_trial_data(datafile)
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                              code_ae = "code", descrip_ae = NULL,
                              number_use_ae = "number_use",
                              type_admit_ae = "type_admit",
                              unit_cost_data = unit_cost_data,
                              code_col = "Currency_Code",
                              type_admit_col = "Service_Code",
                              description_col = NULL,
                              unit_cost_col = "National_Average_Unit_Cost",
                              cost_calculated_in = "attendance"))
 costs_file <- system.file("extdata",
                           "NHS_costs_2019_AandE_nocode.csv",
                           package = "packDAMipd")
 datafile <- system.file("extdata", "resource_use_ae_ip.csv",
                         package = "packDAMipd")
 ind_part_data <- packDAMipd::load_trial_data(datafile)
 unit_cost_data <- packDAMipd::load_trial_data(costs_file)
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                                   code_ae = "code", descrip_ae = NULL,
                                   number_use_ae = "number_use",
                                   type_admit_ae = "type_admit",
                                   unit_cost_data = unit_cost_data,
                                   code_col = "Currency_Code",
                                   type_admit_col = "Service_Code",
                                   description_col = NULL,
                              unit_cost_col = "National_Average_Unit_Cost",
                                   cost_calculated_in = "attendance"))
 costs_file <- system.file("extdata",
                           "National_schedule_of_NHS_costs_2019_AandE.csv",
                           package = "packDAMipd")
 unit_cost_data <- packDAMipd::load_trial_data(costs_file)
 datafile <- system.file("extdata",
                         "resource_use_ae_ip_lengthtypecodediff.csv",
                         package = "packDAMipd")
 ind_part_data <- packDAMipd::load_trial_data(datafile)
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                         code_ae = "code", descrip_ae = NULL,
                         number_use_ae = "number_use",
                         type_admit_ae = "type_admit",
                         unit_cost_data = unit_cost_data,
                         code_col = "Currency_Code",
                         type_admit_col = "Service_Code",
                         description_col = NULL,
                         unit_cost_col = "National_Average_Unit_Cost",
                         cost_calculated_in = "attendance"))
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                                      code_ae = NULL, descrip_ae =  "desc",
                                      number_use_ae = "number_use",
                                      type_admit_ae = "type_admit",
                                      unit_cost_data = unit_cost_data,
                                      code_col = "Currency_Code",
                                      type_admit_col = "Service_Code",
                              description_col = "Currency_Description",
                              unit_cost_col = "National_Average_Unit_Cost",
                                      cost_calculated_in = "attendance"))

 datafile <- system.file("extdata", "resource_use_ae_ip_extranumuse.csv",
                         package = "packDAMipd")
 ind_part_data <- packDAMipd::load_trial_data(datafile)
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                         code_ae = "code", descrip_ae = NULL,
                         number_use_ae = "number_use",
                         type_admit_ae = "type_admit",
                         unit_cost_data = unit_cost_data,
                         code_col = "Currency_Code",
                         type_admit_col = "Service_Code",
                         description_col = NULL,
                         unit_cost_col = "National_Average_Unit_Cost",
                         cost_calculated_in = "attendance"))
 expect_error(costing_AandE_admission(ind_part_data = ind_part_data,
                         code_ae = NULL, descrip_ae =  "desc",
                         number_use_ae = "number_use",
                         type_admit_ae = "type_admit",
                         unit_cost_data = unit_cost_data,
                         code_col = "Currency_Code",
                         type_admit_col = "Service_Code",
                         description_col = "Currency_Description",
                         unit_cost_col = "National_Average_Unit_Cost",
                         cost_calculated_in = "attendance"))
 datafile <- system.file("extdata", "resource_use_ae_ip_lessnumuse.csv",
                         package = "packDAMipd")
 ind_part_data <- packDAMipd::load_trial_data(datafile)
 res <- costing_AandE_admission(ind_part_data = ind_part_data,
                         code_ae = "code", descrip_ae = NULL,
                         number_use_ae = "number_use",
                         type_admit_ae = "type_admit",
                         unit_cost_data = unit_cost_data,
                         code_col = "Currency_Code",
                         type_admit_col = "Service_Code",
                         description_col = NULL,
                         unit_cost_col = "National_Average_Unit_Cost",
                         cost_calculated_in = "attendance")
 expect_equal(res$totcost_AE_Admission[1], 671, tol = 1e-2)
 res <- costing_AandE_admission(ind_part_data = ind_part_data,
                                code_ae = NULL, descrip_ae =  "desc",
                                number_use_ae = "number_use",
                                type_admit_ae = "type_admit",
                                unit_cost_data = unit_cost_data,
                                code_col = "Currency_Code",
                                type_admit_col = "Service_Code",
                                description_col = "Currency_Description",
                                unit_cost_col = "National_Average_Unit_Cost",
                                cost_calculated_in = "attendance")
 expect_equal(res$totcost_AE_Admission[1], 754, tol = 1e-2)

 datafile <- system.file("extdata", "resource_use_ae_ip.csv",
                         package = "packDAMipd")
 ind_part_data <- packDAMipd::load_trial_data(datafile)
 costs_file <- system.file("extdata",
                           "National_schedule_of_NHS_costs_2019_AandE.csv",
                           package = "packDAMipd")
 unit_cost_data <- packDAMipd::load_trial_data(costs_file)
 res <- costing_AandE_admission(ind_part_data = ind_part_data,
                         code_ae = "code", descrip_ae = NULL,
                         number_use_ae = NULL,
                         type_admit_ae = "type_admit",
                         unit_cost_data = unit_cost_data,
                         code_col = "Currency_Code",
                         type_admit_col = "Service_Code",
                         description_col = "Currency_Description",
                         unit_cost_col = "National_Average_Unit_Cost",
                         cost_calculated_in = "attendance")
 expect_equal(res$totcost_AE_Admission[1], 671, tol = 1e-2)
 res <- costing_AandE_admission(ind_part_data = ind_part_data,
                         code_ae = NULL, descrip_ae =  "desc",
                         number_use_ae = NULL,
                         type_admit_ae = "type_admit",
                         unit_cost_data = unit_cost_data,
                         code_col = "Currency_Code",
                         type_admit_col = "Service_Code",
                         description_col = "Currency_Description",
                         unit_cost_col = "National_Average_Unit_Cost",
                         cost_calculated_in = "attendance")
 expect_equal(res$totcost_AE_Admission[1], 754, tol = 1e-2)
})

