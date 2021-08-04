#######################################################################
context("testing costing opioid tablets using average MED tablets")
test_that("testing costing opioid tablets using average MED tablets", {
  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_all_brandNull.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  res <- costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
  res <- costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
  expect_true(sum(is.na(res$totmed_period_tablets_mg)) ==
              length(res$totmed_period_tablets_mg))

  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_all_test.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  res <- costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
res <- costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
expect_error(costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
res <- costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
conv_file <- system.file("extdata", "Med_calc_tabletunitwrong.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the medication is known

expect_error(costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
data_file <- system.file("extdata", "medication_all.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the medication is known

res <- costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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

expect_error(costing_opioid_tablets_averageMED_wide(ind_part_data = NULL,
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

expect_error(costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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

expect_error(costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
res <- costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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

res <- costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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

res <- costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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


expect_error(costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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

expect_error(costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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

expect_error(costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
res <- costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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

expect_error(costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
expect_error(costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
expect_error(costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
expect_error(costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
res <- costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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

res <- costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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

res <- costing_opioid_tablets_averageMED_wide(ind_part_data = ind_part_data,
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
data_file <- system.file("extdata", "med_tablets_timepoints.csv",
                         package = "packDAMipd")
med_tablets_timepoints <- load_trial_data(data_file)

opioids_tablets_timepoints <- med_tablets_timepoints[med_tablets_timepoints$tabdrugname != 97, ]

list_code_names <- list(seq(1:21), c("Buprenorphine", "Co-codamol (strength not given)", "Codeine phosphate", "Co-dydramol (strength not given)", "Dihydrocodeine", "Fentanyl", "Hydromorphone", "Meptazinol", "Morphine", "Oxycodone", "Pethidin", "Tapentadol", "Tramadol", "Methadone", "Co-codamol 8/500", "Co-codamol 15/500", "Co-codamol 30/500",  "Co-dydramol 7.46/500",  "Co-dydramol 10/500", "Co-dydramol 20/500", "Co-dydramol 30/500"))

list_code_freq <- list(seq(1:20), c("Once a day", "Twice a day",
                                    "Three times a day", "Four times a day",
                                    "Five times a day", "Six times a day",
                                    "Seven times a day", "Eight times a day",
                                    "Nine times a day", "Ten times a day",
                                    "Once every 2 days", "Once every 3 days",
                                    "Once every 4 days", "Once every 5 days",
                                    "Once every 6 days", "Once a week",
                                    "Twice a week", "Thrice a week",
                                    "Four times a week", "Not stated"))

list_code_dose_unit <- list(seq(1:2), c("mcg", "mg"))

med_costs_file <- system.file("extdata",
                              "Medications_unit_costs_all.xlsx",
                              package = "packDAMipd")
med_costs <- load_trial_data(med_costs_file)

equiv_dose_calc <- load_trial_data(system.file("extdata",
                               "Med_calc.xlsx",
                               package = "packDAMipd"))

index <- which(colnames(opioids_tablets_timepoints) == "tabdrugnameother")
colnames(opioids_tablets_timepoints)[index] <- "tabdrugother"

# Morphine brand MST is replaced with MST Continus
indices <- which(opioids_tablets_timepoints$tabdrugname  == 9 &
                   opioids_tablets_timepoints$tabbrandname == "MST")
opioids_tablets_timepoints$tabbrandname[indices] <- "MST Continus"

# co-codamol no brand mame called Ef-fervescent replaced with ""
indices <- which(opioids_tablets_timepoints$tabdrugname  == 17 &
                   opioids_tablets_timepoints$tabbrandname == "EF-Fervescent")
opioids_tablets_timepoints$tabbrandname[indices] <- ""

# Morphine  brand mame called Zomproh replaced with "Zomorph"
indices <- which(opioids_tablets_timepoints$tabdrugname  == 9
                 & opioids_tablets_timepoints$tabbrandname == "Zomproh")
opioids_tablets_timepoints$tabbrandname[indices] <- "Zomorph"

# Tramadol  brand mame called "Mabron Slow Release" replaced with "Mabron"
indices <- which(opioids_tablets_timepoints$tabdrugname  == 13 &
                   opioids_tablets_timepoints$tabbrandname == "Mabron Slow Release")
opioids_tablets_timepoints$tabbrandname[indices] <- "Mabron"

# Tramadol  brand mame called "Marbon" replaced with "Mabron"
indices <- which(opioids_tablets_timepoints$tabdrugname  == 13 &
                   opioids_tablets_timepoints$tabbrandname == "Marbon")
opioids_tablets_timepoints$tabbrandname[indices] <- "Mabron"

# Oxycodone  brand name called "Longtech" replaced with "Longtec"
indices <- which(opioids_tablets_timepoints$tabdrugname  == 10 &
                   opioids_tablets_timepoints$tabbrandname == "Longtech")
opioids_tablets_timepoints$tabbrandname[indices] <- "Longtec"



opioids_tablets_timepoints[["tab_actual"]] <- ""
for (i in 1:nrow(opioids_tablets_timepoints)) {
  m <- opioids_tablets_timepoints$tabdrugname[i]
  if (m == 97) {
    opioids_tablets_timepoints$tab_actual[i] <- opioids_tablets_timepoints$tabdrugother[i]
  } else {
    opioids_tablets_timepoints$tab_actual[i] <- list_code_names[[2]][m]
  }
}

tmp <- opioids_tablets_timepoints[1:2, ]
tmp1 <- costing_opioid_tablets_averageMED_wide(tmp, "tab_actual",
                                               "tabbrandname", "tabmedicationstrength", "tabmedicationunit",
                                               "tabletscount", "tabletsfrequency",
                                               "1 day", med_costs,
                                               "PackageRate", "Basis", "Strength",
                                               list_of_code_names = NULL,
                                               list_of_code_freq = list_code_freq,
                                               list_of_code_dose_unit =
                                                 list_code_dose_unit,
                                               list_of_code_brand = NULL,
                                               eqdose_cov_tab = equiv_dose_calc)

expect_equal(tmp1$totmed_equiv_period_tablets, c(12.5, 12.5))



###############################################################################

context("testing costing opioid tablets using average MED tablets when data being long format")
test_that("testing costing opioid tablets using average MED tablets when data being long format", {
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
  res <- costing_opioid_tablets_averageMED_long(the_columns,
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

  expect_error(costing_opioid_tablets_averageMED_long(the_columns,
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
