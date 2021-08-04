#######################################################################
context("testing microcosting tablets")
test_that("testing microcosting tablets", {
  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_all_brandNull.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                   name_med = "tab_name",
                                   brand_med = "tab_brand",
                                   dose_med = "tab_str",
                                   unit_med = "tab_unit",
                                   no_taken = "tab_no_taken",
                                   freq_taken = "tab_frequency",
                                   timeperiod = "one day",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "mg")
  expect_equal(res$totmed_period_tablets_mg, c(10.1, 0), tolerance = 1e-2)

  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_all_strengthnotgiven.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                   name_med = "tab_name",
                                   brand_med = "tab_brand",
                                   dose_med = "tab_str",
                                   unit_med = NULL,
                                   no_taken = "tab_no_taken",
                                   freq_taken = "tab_frequency",
                                   timeperiod = "one day",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "mg")
  expect_equal(res$totmed_period_tablets_mg, c(110, 8), tolerance = 1e-2)

  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_all_test.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                   name_med = "tab_name",
                                   brand_med = "tab_brand",
                                   dose_med = "tab_strength",
                                   unit_med = "tab_str_unit",
                                   no_taken = "tab_no_taken",
                                   freq_taken = "tab_frequency",
                                   timeperiod = "one day",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "mg")
expect_equal(res$totmed_period_tablets_mg, c(40.1, 8), tolerance = 1e-2)



med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_all_unitnotseparate.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                 name_med = "tab_name",
                                 brand_med = "tab_brand",
                                 dose_med = "tab_strength",
                                 unit_med = NULL,
                                 no_taken = "tab_no_taken",
                                 freq_taken = "tab_frequency",
                                 timeperiod = "one day",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per  = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_freq = NULL,
                                 list_of_code_dose_unit = NULL,
                                 eqdose_cov_tab = table,
                                 basis_strength_unit = "mg")

expect_equal(res$totmed_period_tablets_mg, c(40.1, 8), tolerance = 1e-2)


med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_all_test_error.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                 name_med = "tab_name",
                                 brand_med = "tab_brand",
                                 dose_med = "tab_strength",
                                 unit_med = "tab_str_unit",
                                 no_taken = "tab_no_taken",
                                 freq_taken = "tab_frequency",
                                 timeperiod = "one day",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per  = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_freq = NULL,
                                 list_of_code_dose_unit = NULL,
                                 eqdose_cov_tab = table,
                                 basis_strength_unit = "mg"))


med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_all_2.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                 name_med = "tab_name",
                                 brand_med = "tab_brand",
                                 dose_med = "tab_strength",
                                 unit_med = NULL,
                                 no_taken = "tab_no_taken",
                                 freq_taken = "tab_frequency",
                                 timeperiod = "one day",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per  = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_freq = NULL,
                                 list_of_code_dose_unit = NULL,
                                 eqdose_cov_tab = table,
                                 basis_strength_unit = "mg")

expect_equal(res$totmed_period_tablets_mg, c(10.14, 0), tolerance = 1e-2)

med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_all.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the medication is known

res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                 name_med = "tab_name",
                                 brand_med = "tab_brand",
                                 dose_med = "tab_strength",
                                 unit_med = "tab_str_unit",
                                 no_taken = "tab_no_taken",
                                 freq_taken = "tab_frequency",
                                 timeperiod = "2 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per  = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_freq = NULL,
                                 list_of_code_dose_unit = NULL,
                                 eqdose_cov_tab = table,
                                 basis_strength_unit = "mg")
expect_equal(res$totmed_period_tablets_mg, c(617.50, 0), tolerance = 1e-2)

expect_error(microcosting_tablets_wide(ind_part_data = NULL,
                                       name_med = "tab_name",
                                       brand_med = "tab_brand",
                                       dose_med = "tab_strength",
                                       unit_med = "tab_str_unit",
                                       no_taken = "tab_no_taken",
                                       freq_taken = "tab_frequency",
                                       timeperiod = "2 months",
                                       unit_cost_data = med_costs,
                                       unit_cost_column = "UnitCost",
                                       cost_calculated_per  = "Basis",
                                       strength_column = "Strength",
                                       list_of_code_names = NULL,
                                       list_of_code_freq = NULL,
                                       list_of_code_dose_unit = NULL,
                                       eqdose_cov_tab = table,
                                       basis_strength_unit = "mg"))

expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                       name_med = NULL,
                                       brand_med = "tab_brand",
                                       dose_med = "tab_strength",
                                       unit_med = "tab_str_unit",
                                       no_taken = "tab_no_taken",
                                       freq_taken = "tab_frequency",
                                       timeperiod = "2 months",
                                       unit_cost_data = med_costs,
                                       unit_cost_column = "UnitCost",
                                       cost_calculated_per  = "Basis",
                                       strength_column = "Strength",
                                       list_of_code_names = NULL,
                                       list_of_code_freq = NULL,
                                       list_of_code_dose_unit = NULL,
                                       eqdose_cov_tab = table,
                                       basis_strength_unit = "mg"))

med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_all_no_tabname.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the medication is known

expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                       name_med = "tab_name",
                                       brand_med = "tab_brand",
                                       dose_med = "tab_strength",
                                       unit_med = "tab_str_unit",
                                       no_taken = "tab_no_taken",
                                       freq_taken = "tab_frequency",
                                       timeperiod = "2 months",
                                       unit_cost_data = med_costs,
                                       unit_cost_column = "UnitCost",
                                       cost_calculated_per  = "Basis",
                                       strength_column = "Strength",
                                       list_of_code_names = NULL,
                                       list_of_code_freq = NULL,
                                       list_of_code_dose_unit = NULL,
                                       eqdose_cov_tab = table,
                                       basis_strength_unit = "mg"))

med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the medication is known
res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                 name_med = "tab_name",
                                 brand_med = "tab_brand",
                                 dose_med = "tab_strength",
                                 unit_med = NULL,
                                 no_taken = "tab_no_taken",
                                 freq_taken = "tab_frequency",
                                 timeperiod = "2 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per  = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_freq = NULL,
                                 list_of_code_dose_unit = NULL,
                                 eqdose_cov_tab = table,
                                 basis_strength_unit = "mg")

expect_equal(res$totmed_period_tablets_mg, c(617.50, 48), tolerance = 1e-2)

res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                 name_med = "tab_name",
                                 brand_med = "tab_brand",
                                 dose_med = "tab_strength",
                                 unit_med = NULL,
                                 no_taken = "tab_no_taken",
                                 freq_taken = "tab_frequency",
                                 timeperiod = "2 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per  = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_freq = NULL,
                                 list_of_code_dose_unit = NULL,
                                 eqdose_cov_tab = table,
                                 basis_strength_unit = NULL)
expect_equal(res$totmed_period_tablets_mg, c(617.50, 48), tolerance = 1e-2)

res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                 name_med = "tab_name",
                                 brand_med = "tab_brand",
                                 dose_med = "tab_strength",
                                 unit_med = NULL,
                                 no_taken = "tab_no_taken",
                                 freq_taken = "tab_frequency",
                                 timeperiod = "2 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per  = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_freq = NULL,
                                 list_of_code_dose_unit = NULL,
                                 eqdose_cov_tab = table,
                                 basis_strength_unit = NA)
expect_equal(res$totmed_period_tablets_mg, c(617.50, 48), tolerance = 1e-2)


expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                       name_med = "tab_name",
                                       brand_med = "tab_brand",
                                       dose_med = "tab_strength",
                                       unit_med = NULL,
                                       no_taken = "tab_no_taken",
                                       freq_taken = "tab_frequency",
                                       timeperiod = "2 months",
                                       unit_cost_data = med_costs,
                                       unit_cost_column = "UnitCost",
                                       cost_calculated_per  = "Basis",
                                       strength_column = "Strength",
                                       list_of_code_names = NULL,
                                       list_of_code_freq = NULL,
                                       list_of_code_dose_unit = NULL,
                                       eqdose_cov_tab = table,
                                       basis_strength_unit = "NA"))
med_costs_file <- system.file("extdata",
                              "medicaton_costs_all_brandcolname_error.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)

expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                       name_med = "tab_name",
                                       brand_med = "tab_brand",
                                       dose_med = "tab_strength",
                                       unit_med = NULL,
                                       no_taken = "tab_no_taken",
                                       freq_taken = "tab_frequency",
                                       timeperiod = "2 months",
                                       unit_cost_data = med_costs,
                                       unit_cost_column = "UnitCost",
                                       cost_calculated_per  = "Basis",
                                       strength_column = "Strength",
                                       list_of_code_names = NULL,
                                       list_of_code_freq = NULL,
                                       list_of_code_dose_unit = NULL,
                                       eqdose_cov_tab = table,
                                       basis_strength_unit = NA))


med_costs_file <- system.file("extdata",
                              "medicaton_costs_all_sizecol_error.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)

expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                       name_med = "tab_name",
                                       brand_med = "tab_brand",
                                       dose_med = "tab_strength",
                                       unit_med = NULL,
                                       no_taken = "tab_no_taken",
                                       freq_taken = "tab_frequency",
                                       timeperiod = "2 months",
                                       unit_cost_data = med_costs,
                                       unit_cost_column = "UnitCost",
                                       cost_calculated_per  = "Basis",
                                       strength_column = "Strength",
                                       list_of_code_names = NULL,
                                       list_of_code_freq = NULL,
                                       list_of_code_dose_unit = NULL,
                                       eqdose_cov_tab = table,
                                       basis_strength_unit = NA))
#no brand name given in ipd data or we dont care about it for tablets
med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
med_costs <- load_trial_data(med_costs_file)
res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                 name_med = "tab_name",
                                 brand_med = NULL,
                                 dose_med = "tab_strength",
                                 unit_med = NULL,
                                 no_taken = "tab_no_taken",
                                 freq_taken = "tab_frequency",
                                 timeperiod = "2 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per  = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names = NULL,
                                 list_of_code_freq = NULL,
                                 list_of_code_dose_unit = NULL,
                                 eqdose_cov_tab = table,
                                 basis_strength_unit = "mg")
expect_equal(res$totmed_period_tablets_mg, c(617.50, 48), tolerance = 1e-2)



med_costs_file <- system.file("extdata",
                              "medicaton_costs_all_nounitcost.xlsx",
                              package = "packDAMipd")
med_costs <- load_trial_data(med_costs_file)

expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                       name_med = "tab_name",
                                       brand_med = NULL,
                                       dose_med = "tab_strength",
                                       unit_med = NULL,
                                       no_taken = "tab_no_taken",
                                       freq_taken = "tab_frequency",
                                       timeperiod = "2 months",
                                       unit_cost_data = med_costs,
                                       unit_cost_column = "UnitCost",
                                       cost_calculated_per  = "Basis",
                                       strength_column = "Strength",
                                       list_of_code_names = NULL,
                                       list_of_code_freq = NULL,
                                       list_of_code_dose_unit = NULL,
                                       eqdose_cov_tab = table,
                                       basis_strength_unit = "mg"))


med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
med_costs <- load_trial_data(med_costs_file)
data_file <- system.file("extdata", "medication_namemissing.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                       name_med = "tab_name",
                                       brand_med = NULL,
                                       dose_med = "tab_strength",
                                       unit_med = "tab_str_unit",
                                       no_taken = "tab_no_taken",
                                       freq_taken = "tab_frequency",
                                       timeperiod = "2 months",
                                       unit_cost_data = med_costs,
                                       unit_cost_column = "UnitCost",
                                       cost_calculated_per  = "Basis",
                                       strength_column = "Strength",
                                       list_of_code_names = NULL,
                                       list_of_code_freq = NULL,
                                       list_of_code_dose_unit = NULL,
                                       eqdose_cov_tab = table,
                                       basis_strength_unit = "mg"))

med_costs_file <- system.file("extdata",
                              "medicaton_costs_all_namecol_nameerror.xlsx",
                              package = "packDAMipd")
med_costs <- load_trial_data(med_costs_file)
data_file <- system.file("extdata", "medication.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                       name_med = "tab_name",
                                       brand_med = NULL,
                                       dose_med = "tab_strength",
                                       unit_med = NULL,
                                       no_taken = "tab_no_taken",
                                       freq_taken = "tab_frequency",
                                       timeperiod = "2 months",
                                       unit_cost_data = med_costs,
                                       unit_cost_column = "UnitCost",
                                       cost_calculated_per  = "Basis",
                                       strength_column = "Strength",
                                       list_of_code_names = NULL,
                                       list_of_code_freq = NULL,
                                       list_of_code_dose_unit = NULL,
                                       eqdose_cov_tab = table,
                                       basis_strength_unit = "mg"))

med_costs_file <- system.file("extdata",
                              "medicaton_costs_all_formcol_nameerror.xlsx",
                              package = "packDAMipd")
med_costs <- load_trial_data(med_costs_file)
data_file <- system.file("extdata", "medication.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
expect_error(microcosting_tablets_wide(ind_part_data = ind_part_data,
                                       name_med = "tab_name",
                                       brand_med = NULL,
                                       dose_med = "tab_strength",
                                       unit_med = NULL,
                                       no_taken = "tab_no_taken",
                                       freq_taken = "tab_frequency",
                                       timeperiod = "2 months",
                                       unit_cost_data = med_costs,
                                       unit_cost_column = "UnitCost",
                                       cost_calculated_per  = "Basis",
                                       strength_column = "Strength",
                                       list_of_code_names = NULL,
                                       list_of_code_freq = NULL,
                                       list_of_code_dose_unit = NULL,
                                       eqdose_cov_tab = table,
                                       basis_strength_unit = "mg"))

## Information is coded  in the data file
med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_with_codes.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                 name_med = "tab_name",
                                 brand_med = "tab_brand",
                                 dose_med = "tab_strength",
                                 unit_med = "tab_unit",
                                 no_taken = "tab_no_taken",
                                 freq_taken = "tab_frequency",
                                 timeperiod = "4 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per  = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names =
                                   list(c(1, 2), c("Buprenorphine", "Fentanyl")),
                                 list_of_code_freq =
                                   list(c(1, 2, 3),
                                        c("once a day", "twice a day", "once a week")),
                                 list_of_code_dose_unit =
                                   list(c(1, 2), c("mcg", "mg")),
                                 list_of_code_brand = list(c(1, 2),
                                                           c("Buprenorphine",
                                                                      "Temgesic")),
                                 eqdose_cov_tab = table,
                                 basis_strength_unit = "mg")


expect_equal(res$totmed_period_tablets_mg, c(1217, 96), tolerance = 1e-1)

res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                 name_med = "tab_name",
                                 brand_med = "tab_brand",
                                 dose_med = "tab_strength",
                                 unit_med = "tab_unit",
                                 no_taken = "tab_no_taken",
                                 freq_taken = "tab_frequency",
                                 timeperiod = "4 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per  = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names =
                                   list(c(1, 2), c("Buprenorphine", "Fentanyl")),
                                 list_of_code_freq =
                                   list(c(1, 2, 3),
                                        c("once a day", "twice a day",
                                          "once a week")),
                                 list_of_code_dose_unit =
                                   list(c(1, 2), c("mcg", "mg")),
                                 list_of_code_brand = list(c(1, 2),
                                                           c("Buprenorphine", "Temgesic")),
                                 eqdose_cov_tab = NULL,
                                 basis_strength_unit = "mg")

res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
                                 name_med = "tab_name",
                                 brand_med = "tab_brand",
                                 dose_med = "tab_strength",
                                 unit_med = "tab_unit",
                                 no_taken = "tab_no_taken",
                                 freq_taken = "tab_frequency",
                                 timeperiod = "4 months",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per  = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names =
                                   list(c(1, 2), c("Buprenorphine", "Fentanyl")),
                                 list_of_code_freq =
                                   list(c(1, 2, 3),
                                        c("once a day", "twice a day", "once a week")),
                                 list_of_code_dose_unit =
                                   list(c(1, 2), c("mcg", "mg")),
                                 list_of_code_brand = list(c(1, 2),
                                                           c("Buprenorphine", "Temgesic")),
                                 eqdose_cov_tab = NA,
                                 basis_strength_unit = "mg")

})
###############################################################################
###############################################################################

context("testing microcosting tablets when data being long format")
test_that("testing microcosting tablets when data being long format", {
  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_all.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  names <- colnames(ind_part_data)
  ending <- length(names)
  ind_part_data_long <- tidyr::gather(ind_part_data, measurement, value,
                                      names[2]:names[ending], factor_key = TRUE)
  the_columns <- c("measurement", "value")
  res <- microcosting_tablets_long(the_columns,
                                   ind_part_data_long = ind_part_data_long,
                                   name_med = "tab_name",
                                   brand_med = "tab_brand",
                                   dose_med = "tab_strength",
                                   unit_med = "tab_str_unit",
                                   no_taken = "tab_no_taken",
                                   freq_taken = "tab_frequency",
                                   timeperiod = "2 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "mg")

  index <-  which(res$measurement == "totmed_period_tablets_mg")[1]

  expect_equal(as.numeric(res$value[index]), 617.50, tolerance = 1e-3)

  expect_error(microcosting_tablets_long(the_columns,
                                         ind_part_data_long = NULL,
                                         name_med = "tab_name",
                                         brand_med = "tab_brand",
                                         dose_med = "tab_strength",
                                         unit_med = "tab_str_unit",
                                         no_taken = "tab_no_taken",
                                         freq_taken = "tab_frequency",
                                         timeperiod = "2 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mg"))
})
