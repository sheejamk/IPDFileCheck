###############################################################################
context("testing costing patches")
test_that("testing costing patches", {

  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_brandmissing_ipd.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  #using the package price when the brand name of the medication is known
  res <- costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                   name_med = "patch_name",
                                   brand_med = "patch_brand",
                                   dose_med = "patch_strength",
                                   unit_med = NULL,
                                   no_taken = "patch_no_taken",
                                   freq_taken = "patch_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   list_of_code_brand = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "mcg/hr")

  expect_equal(res$totalmed_equiv_dose_period_patches, c(180, 57.6),
               tolerance = 1e-3)


  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  conv_file <- system.file("extdata", "Med_calc_patchunitwrong.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  #using the package price when the brand name of the medication is known
  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                                name_med = "patch_name",
                                                brand_med = "patch_brand",
                                                dose_med = "patch_strength",
                                                unit_med = NULL,
                                                no_taken = "patch_no_taken",
                                                freq_taken = "patch_frequency",
                                                timeperiod = "4 months",
                                                unit_cost_data = med_costs,
                                                unit_cost_column = "UnitCost",
                                                cost_calculated_per  = "Basis",
                                                strength_column = "Strength",
                                                list_of_code_names = NULL,
                                                list_of_code_freq = NULL,
                                                list_of_code_dose_unit = NULL,
                                                list_of_code_brand = NULL,
                                                eqdose_cov_tab = table,
                                                basis_strength_unit = "mcg/hr"))




  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_incorrect_brand.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                                name_med = "patch_name",
                                                brand_med = "patch_brand",
                                                dose_med = "patch_strength",
                                                unit_med = NULL,
                                                no_taken = "patch_no_taken",
                                                freq_taken = "patch_frequency",
                                                timeperiod = "4 months",
                                                unit_cost_data = med_costs,
                                                unit_cost_column = "UnitCost",
                                                cost_calculated_per  = "Basis",
                                                strength_column = "Strength",
                                                list_of_code_names = NULL,
                                                list_of_code_freq = NULL,
                                                list_of_code_dose_unit = NULL,
                                                list_of_code_brand = NULL,
                                                eqdose_cov_tab = table,
                                                basis_strength_unit = "mcg/hr"))



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
  res <- costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                   name_med = "patch_name",
                                   brand_med = "patch_brand",
                                   dose_med = "patch_strength",
                                   unit_med = NULL,
                                   no_taken = "patch_no_taken",
                                   freq_taken = "patch_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   list_of_code_brand = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "mcg/hr")

  expect_equal(res$totalmed_equiv_dose_period_patches, c(180, 57.6),
               tolerance = 1e-3)


  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "jg/hr"))

  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mg/b"))

  res <- costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                   name_med = "patch_name",
                                   brand_med = "patch_brand",
                                   dose_med = "patch_strength",
                                   unit_med = NA,
                                   no_taken = "patch_no_taken",
                                   freq_taken = "patch_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   list_of_code_brand = NULL,
                                   eqdose_cov_tab = NULL,
                                   basis_strength_unit = "mcg/hr")

  expect_equal(res$totalmed_equiv_dose_period_patches, c(75, 24),
               tolerance = 1e-3)
  res <- costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                   name_med = "patch_name",
                                   brand_med = "patch_brand",
                                   dose_med = "patch_strength",
                                   unit_med = NA,
                                   no_taken = "patch_no_taken",
                                   freq_taken = "patch_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   list_of_code_brand = NULL,
                                   eqdose_cov_tab = NA,
                                   basis_strength_unit = "mcg/hr")

  expect_equal(res$totalmed_equiv_dose_period_patches, c(75, 24),
               tolerance = 1e-3)

  # no cols with drug names
  conv_file <- system.file("extdata", "Med_calc_nodrugcol.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NA,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))

  # no column for form in conversion table
  conv_file <- system.file("extdata", "Med_calc_noformcol.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NA,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))
  # no unit column in conversion table
  conv_file <- system.file("extdata", "Med_calc_nounitcol.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NA,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))
  #no conversion factor column in conversion table
  conv_file <- system.file("extdata", "Med_calc_noconvfactorcol.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NA,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))


  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  res <- costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                   name_med = "patch_name",
                                   brand_med = "patch_brand",
                                   dose_med = "patch_strength",
                                   unit_med = NA,
                                   no_taken = "patch_no_taken",
                                   freq_taken = "patch_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   list_of_code_brand = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "mcg/hr")

  expect_equal(res$totalmed_equiv_dose_period_patches, c(180, 57.6),
               tolerance = 1e-3)
  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mg"))

  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = NULL,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))

  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = NULL,
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))


  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = NULL,
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))

  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = NULL,
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))

  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = NULL,
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))

  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = NULL,
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))

  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = NULL,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))

  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = NULL,
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))

  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = NULL,
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))
  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = NULL,
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))


  # no brand information
  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  res <- costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                   name_med = "patch_name",
                                   brand_med = NULL,
                                   dose_med = "patch_strength",
                                   unit_med = NULL,
                                   no_taken = "patch_no_taken",
                                   freq_taken = "patch_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = NULL)

  expect_equal(res$totalmed_equiv_dose_period_patches, c(180, 57.6),
               tolerance = 1e-3)
  res <- costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                   name_med = "patch_name",
                                   brand_med = NA,
                                   dose_med = "patch_strength",
                                   unit_med = NULL,
                                   no_taken = "patch_no_taken",
                                   freq_taken = "patch_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = NULL)
  expect_equal(res$totalmed_equiv_dose_period_patches, c(180, 57.6),
               tolerance = 1e-3)

  res <- costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                   name_med = "patch_name",
                                   brand_med = NULL,
                                   dose_med = "patch_strength",
                                   unit_med = NULL,
                                   no_taken = "patch_no_taken",
                                   freq_taken = "patch_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = NA)
  expect_equal(res$totalmed_equiv_dose_period_patches, c(180, 57.6),
               tolerance = 1e-3)

  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = NULL,
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
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
                                "medicaton_costs_all_nobrandcol.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  expect_error(costing_opioid_patches_averageMED_wide(
                                         ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))

  med_costs_file <- system.file("extdata",
                                "medicaton_costs_all_nosizecol.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))



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
  res <- costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                   name_med = "patch_name",
                                   brand_med = "patch_brand",
                                   dose_med = "patch_strength",
                                   unit_med = "patch_unit_str",
                                   no_taken = "patch_no_taken",
                                   freq_taken = "patch_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names =
                                     list(c(1, 2), c("Buprenorphine", "Fentanyl")),
                                   list_of_code_freq =
                                     list(c(1, 2, 3), c("once a day", "twice a day", "once a week")),
                                   list_of_code_dose_unit =
                                     list(c(1, 2), c("mcg/hr", "mcg/day")),
                                   list_of_code_brand = list(c(1, 2, 3),
                                                             c("BuTrans", "Fencino", "Butec")),
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "mcg/hr")


  expect_equal(res$totalmed_equiv_dose_period_patches, c(180, 57.6),
               tolerance = 1e-3)
  expect_error(costing_opioid_patches_averageMED_wide(
    ind_part_data = ind_part_data,
    name_med = "patch_name",
    brand_med = "patch_brand",
    dose_med = "patch_strength",
    unit_med = NULL,
    no_taken = "patch_no_taken",
    freq_taken = "patch_frequency",
    timeperiod = "4 months",
    unit_cost_data = med_costs,
    unit_cost_column = "UnitCost",
    cost_calculated_per  = "Basis",
    strength_column = "Strength",
    list_of_code_names =
      list(c(1, 2),
           c("Buprenorphine", "Fentanyl")),
    list_of_code_freq =
      list(c(1, 2, 3),
           c("once a day", "twice a day",
             "once a week")),
    list_of_code_dose_unit =
      list(c(1, 2),
           c("mcg/hr", "mcg/day")),
    list_of_code_brand =
      list(c(1, 2, 3), c("BuTrans",
                         "Fencino", "Butec")),
    eqdose_cov_tab = table,
    basis_strength_unit = "mcg/hr"))


  med_costs_file <- system.file("extdata",
                                "medicaton_costs_all_unitdiff.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  #using the package price when the brand name of the medication is known
  costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr")
  expect_equal(res$totalmed_equiv_dose_period_patches, c(180, 57.6),
               tolerance = 1e-3)

  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_nopatchname.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  #using the package price when the brand name of the medication is known
  expect_error(costing_opioid_patches_averageMED_wide(
                                         ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))

  med_costs_file <- system.file("extdata",
                                "medicaton_costs_all_nounitcost.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  #using the package price when the brand name of the medication is known
  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))

  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc_unit_notright.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  #using the package price when the brand name of the medication is known
  expect_error(costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))

  conv_file <- system.file("extdata", "Med_calc_testNA.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)

  #using the package price when the brand name of the medication is known
  costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                            name_med = "patch_name",
                            brand_med = "patch_brand",
                            dose_med = "patch_strength",
                            unit_med = NULL,
                            no_taken = "patch_no_taken",
                            freq_taken = "patch_frequency",
                            timeperiod = "4 months",
                            unit_cost_data = med_costs,
                            unit_cost_column = "UnitCost",
                            cost_calculated_per  = "Basis",
                            strength_column = "Strength",
                            list_of_code_names = NULL,
                            list_of_code_freq = NULL,
                            list_of_code_dose_unit = NULL,
                            list_of_code_brand = NULL,
                            eqdose_cov_tab = table,
                            basis_strength_unit = "mcg/hr")
  expect_equal(res$totalmed_equiv_dose_period_patches, c(180, 57.6),
                                                      tolerance = 1e-1)


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
  res <- costing_opioid_patches_averageMED_wide(ind_part_data = ind_part_data,
                                   name_med = "patch_name",
                                   brand_med = "patch_brand",
                                   dose_med = "patch_strength",
                                   unit_med = NULL,
                                   no_taken = "patch_no_taken",
                                   freq_taken = "patch_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   list_of_code_brand = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "mcg/hr")
  expect_equal(res$totalmed_equiv_dose_period_patches, c(180, NA),
               tolerance = 1e-3)
})
###############################################################################
context("testing costing patches ")
test_that("testing costing patches", {
  med_costs_file <- system.file("extdata", "Medications_unit_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "med_patches_timepoints.csv",
                           package = "packDAMipd")
  med_patches_timepoints <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  opioid_patches_timepoints <-
    med_patches_timepoints[med_patches_timepoints$patchdrugname != 97, ]
  list_code_names <- list(seq(1:2), c("Buprenorphine", "Fentanyl"))
  list_code_freq <- list(seq(1:10), c("Once a day", "Twice a day",
                                      "Three times a day",
                                      "Once every 2 days", "Once every 3 days",
                                      "Once every 4 days", "Once every 5 days",
                                      "Once every 6 days", "Once a week",
                                      "Not stated"))

  list_code_dose_unit <- list(1, "mcg/hr")
  index <- which(colnames(opioid_patches_timepoints) == "patchdrugnameother")
  colnames(opioid_patches_timepoints)[index] <- "patchdrugother"

  opioid_patches_timepoints[["patch_actual"]] <- ""
  for (i in 1:nrow(opioid_patches_timepoints)) {
    m <- opioid_patches_timepoints$patchdrugname[i]
    if (m == 97) {
      opioid_patches_timepoints$patch_actual[i] <- opioid_patches_timepoints$patchdrugother[i]
    } else {
      opioid_patches_timepoints$patch_actual[i] <- list_code_names[[2]][m]
    }
  }
  tmp <- opioid_patches_timepoints[1:10, ]
  tmp <- costing_opioid_patches_averageMED_wide(
    tmp, "patch_actual", "patchbrandname", "patchmedicationstrength",
    "patchmedicationunit", "patchescount", "patchesfrequency", "1 day",
    med_costs, "PackageRate", "Basis", "Strength",
    list_of_code_names = NULL,
    list_of_code_freq = list_code_freq,
    list_of_code_dose_unit = list_code_dose_unit,
    list_of_code_brand = NULL, eqdose_cov_tab = table)

  expect_equal(tmp$totalmed_equiv_dose_period_patches,
               c(126.0, 126.0, 126.0, 126.0, 72.0, 12.0,
                 24.0, 120.0, 28.8,  60.0))

  expect_error(costing_opioid_patches_averageMED_wide(
    tmp, "patch_actual", "patchbrandname", "patchmedicationstrength",
    "patchmedicationunit", "patchescount", "patchesfrequency", "1 day",
    med_costs, "PackageRate", "Basis", "Strength",
    list_of_code_names = NULL,
    list_of_code_freq = list_code_freq,
    list_of_code_dose_unit = NULL,
    list_of_code_brand = NULL, eqdose_cov_tab = table))
})



###############################################################################
context("testing costing patches when data being long format")
test_that("testing costing patches when data being long format", {
  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication.xlsx",
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

  #using the package price when the brand name of the medication is known
  res <- costing_opioid_patches_averageMED_long(the_columns,
                                   ind_part_data_long = ind_part_data_long,
                                   name_med = "patch_name",
                                   brand_med = "patch_brand",
                                   dose_med = "patch_strength",
                                   unit_med = NULL,
                                   no_taken = "patch_no_taken",
                                   freq_taken = "patch_frequency",
                                   timeperiod = "4 months",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per  = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_freq = NULL,
                                   list_of_code_dose_unit = NULL,
                                   list_of_code_brand = NULL,
                                   eqdose_cov_tab = table,
                                   basis_strength_unit = "mcg/hr")

  index <-  which(res$measurement == "totalmed_equiv_dose_period_patches")[1]

  expect_equal(as.numeric(res$value[index]), 180,
               tolerance = 1e-3)
  expect_error(costing_opioid_patches_averageMED_long(the_columns,
                                         ind_part_data_long = NULL,
                                         name_med = "patch_name",
                                         brand_med = "patch_brand",
                                         dose_med = "patch_strength",
                                         unit_med = NULL,
                                         no_taken = "patch_no_taken",
                                         freq_taken = "patch_frequency",
                                         timeperiod = "4 months",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per  = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_freq = NULL,
                                         list_of_code_dose_unit = NULL,
                                         list_of_code_brand = NULL,
                                         eqdose_cov_tab = table,
                                         basis_strength_unit = "mcg/hr"))
})
###############################################################################
