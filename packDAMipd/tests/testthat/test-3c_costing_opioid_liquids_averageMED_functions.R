#####################################################################
context("testing costing liquids")
test_that("testing costing liquids", {
  med_costs_file <- system.file("extdata", "medicaton_costs_all_brandcolname_error.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_liq.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  expect_error(costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                name_med = "liq_name",
                                                brand_med =  "liq_brand",
                                                bottle_size = "liq_bottle_size",
                                                bottle_size_unit = NULL,
                                                bottle_lasts = "liq_lasts",
                                                bottle_lasts_unit = NULL,
                                                preparation_dose = "liq_strength",
                                                preparation_unit = NULL,
                                                timeperiod = "1 day",
                                                unit_cost_data = med_costs,
                                                unit_cost_column = "UnitCost",
                                                cost_calculated_per = "Basis",
                                                strength_column = "Strength",
                                                list_of_code_names = NULL,
                                                list_of_code_brand = NULL,
                                                list_of_code_bottle_size_unit = NULL,
                                                list_of_code_bottle_lasts_unit = NULL,
                                                list_preparation_dose_unit = NULL,
                                                eqdose_covtab = table,
                                                basis_strength_unit = NULL))

  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_liq_brand_empty.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  res <- costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                name_med = "liq_name",
                                                brand_med =  "liq_brand",
                                                bottle_size = "liq_bottle_size",
                                                bottle_size_unit = NULL,
                                                bottle_lasts = "liq_lasts",
                                                bottle_lasts_unit = NULL,
                                                preparation_dose = "liq_strength",
                                                preparation_unit = NULL,
                                                timeperiod = "1 day",
                                                unit_cost_data = med_costs,
                                                unit_cost_column = "UnitCost",
                                                cost_calculated_per = "Basis",
                                                strength_column = "Strength",
                                                list_of_code_names = NULL,
                                                list_of_code_brand = NULL,
                                                list_of_code_bottle_size_unit = NULL,
                                                list_of_code_bottle_lasts_unit = NULL,
                                                list_preparation_dose_unit = NULL,
                                                eqdose_covtab = NULL,
                                                basis_strength_unit = NULL)

  expect_equal(res$totmed_per_equiv_period_liquid, c(54.1, 17.9), tolerance = 1e-3)

  res <- costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                         name_med = "liq_name",
                                         brand_med =  "liq_brand",
                                         bottle_size = "liq_bottle_size",
                                         bottle_size_unit = NULL,
                                         bottle_lasts = "liq_lasts",
                                         bottle_lasts_unit = NULL,
                                         preparation_dose = "liq_strength",
                                         preparation_unit = NULL,
                                         timeperiod = "1 day",
                                         unit_cost_data = med_costs,
                                         unit_cost_column = "UnitCost",
                                         cost_calculated_per = "Basis",
                                         strength_column = "Strength",
                                         list_of_code_names = NULL,
                                         list_of_code_brand = NULL,
                                         list_of_code_bottle_size_unit = NULL,
                                         list_of_code_bottle_lasts_unit = NULL,
                                         list_preparation_dose_unit = NULL,
                                         eqdose_covtab = NA,
                                         basis_strength_unit = NULL)

  expect_equal(res$totmed_per_equiv_period_liquid, c(54.1, 17.9), tolerance = 1e-3)

  med_costs_file <- system.file("extdata", "medicaton_costs_all_nobrandcol.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_liq.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  res <- costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                name_med = "liq_name",
                                                brand_med =  NULL,
                                                bottle_size = "liq_bottle_size",
                                                bottle_size_unit = NULL,
                                                bottle_lasts = "liq_lasts",
                                                bottle_lasts_unit = NULL,
                                                preparation_dose = "liq_strength",
                                                preparation_unit = NULL,
                                                timeperiod = "1 day",
                                                unit_cost_data = med_costs,
                                                unit_cost_column = "UnitCost",
                                                cost_calculated_per = "Basis",
                                                strength_column = "Strength",
                                                list_of_code_names = NULL,
                                                list_of_code_brand = NULL,
                                                list_of_code_bottle_size_unit = NULL,
                                                list_of_code_bottle_lasts_unit = NULL,
                                                list_preparation_dose_unit = NULL,
                                                eqdose_covtab = NULL,
                                                basis_strength_unit = NULL)

  expect_equal(res$totmed_per_equiv_period_liquid, c(54.1, 17.9), tolerance = 1e-3)


  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_liq_emptyrow.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  #using the package price when the brand name is given

  res <- costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                name_med = "liq_name",
                                                brand_med =  "liq_brand",
                                                bottle_size = "liq_bottle_size",
                                                bottle_size_unit = NULL,
                                                bottle_lasts = "liq_lasts",
                                                bottle_lasts_unit = NULL,
                                                preparation_dose = "liq_strength",
                                                preparation_unit = NULL,
                                                timeperiod = "1 day",
                                                unit_cost_data = med_costs,
                                                unit_cost_column = "UnitCost",
                                                cost_calculated_per = "Basis",
                                                strength_column = "Strength",
                                                list_of_code_names = NULL,
                                                list_of_code_brand = NULL,
                                                list_of_code_bottle_size_unit = NULL,
                                                list_of_code_bottle_lasts_unit = NULL,
                                                list_preparation_dose_unit = NULL,
                                                eqdose_covtab = table,
                                                basis_strength_unit = NULL)


  expect_equal(res$totmed_per_equiv_period_liquid, c(56.2, NA), tolerance = 1e-2)




  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_liq.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  #using the package price when the brand name is given

  res <- costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                name_med = "liq_name",
                                                brand_med =  "liq_brand",
                                                bottle_size = "liq_bottle_size",
                                                bottle_size_unit = NULL,
                                                bottle_lasts = "liq_lasts",
                                                bottle_lasts_unit = NULL,
                                                preparation_dose = "liq_strength",
                                                preparation_unit = NULL,
                                                timeperiod = "1 day",
                                                unit_cost_data = med_costs,
                                                unit_cost_column = "UnitCost",
                                                cost_calculated_per = "Basis",
                                                strength_column = "Strength",
                                                list_of_code_names = NULL,
                                                list_of_code_brand = NULL,
                                                list_of_code_bottle_size_unit = NULL,
                                                list_of_code_bottle_lasts_unit = NULL,
                                                list_preparation_dose_unit = NULL,
                                                eqdose_covtab = table,
                                                basis_strength_unit = NULL)


  expect_equal(res$totmed_per_equiv_period_liquid, c(56.2, 26.8), tolerance = 1e-3)







  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_liq_brand_empty.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  res <- costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                   name_med = "liq_name",
                                   brand_med =  "liq_brand",
                                   bottle_size = "liq_bottle_size",
                                   bottle_size_unit = NULL,
                                   bottle_lasts = "liq_lasts",
                                   bottle_lasts_unit = NULL,
                                   preparation_dose = "liq_strength",
                                   preparation_unit = NULL,
                                   timeperiod = "1 day",
                                   unit_cost_data = med_costs,
                                   unit_cost_column = "UnitCost",
                                   cost_calculated_per = "Basis",
                                   strength_column = "Strength",
                                   list_of_code_names = NULL,
                                   list_of_code_brand = NULL,
                                   list_of_code_bottle_size_unit = NULL,
                                   list_of_code_bottle_lasts_unit = NULL,
                                   list_preparation_dose_unit = NULL,
                                   eqdose_covtab = table,
                                   basis_strength_unit = NULL)

expect_equal(res$totmed_per_equiv_period_liquid, c(56.16, 26.79), tolerance = 1e-3)


expect_error(costing_opioid_liquids_averageMED_wide(ind_part_data = NULL,
                                                    name_med = "liq_name",
                                                    brand_med =  "liq_brand",
                                                    bottle_size = "liq_bottle_size",
                                                    bottle_size_unit = NULL,
                                                    bottle_lasts = "liq_lasts",
                                                    bottle_lasts_unit = NULL,
                                                    preparation_dose = "liq_strength",
                                                    preparation_unit = NULL,
                                                    timeperiod = "1 day",
                                                    unit_cost_data = med_costs,
                                                    unit_cost_column = "UnitCost",
                                                    cost_calculated_per = "Basis",
                                                    strength_column = "Strength",
                                                    list_of_code_names = NULL,
                                                    list_of_code_brand = NULL,
                                                    list_of_code_bottle_size_unit = NULL,
                                                    list_of_code_bottle_lasts_unit = NULL,
                                                    list_preparation_dose_unit = NULL,
                                                    eqdose_covtab = table,
                                                    basis_strength_unit = NULL))

expect_error(costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                    name_med = NULL,
                                                    brand_med =  "liq_brand",
                                                    bottle_size = "liq_bottle_size",
                                                    bottle_size_unit = NULL,
                                                    bottle_lasts = "liq_lasts",
                                                    bottle_lasts_unit = NULL,
                                                    preparation_dose = "liq_strength",
                                                    preparation_unit = NULL,
                                                    timeperiod = "1 day",
                                                    unit_cost_data = med_costs,
                                                    unit_cost_column = "UnitCost",
                                                    cost_calculated_per = "Basis",
                                                    strength_column = "Strength",
                                                    list_of_code_names = NULL,
                                                    list_of_code_brand = NULL,
                                                    list_of_code_bottle_size_unit = NULL,
                                                    list_of_code_bottle_lasts_unit = NULL,
                                                    list_preparation_dose_unit = NULL,
                                                    eqdose_covtab = table,
                                                    basis_strength_unit = NULL))


expect_error(costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                    name_med = "liq_name",
                                                    brand_med =  "liq_brand",
                                                    bottle_size = NULL,
                                                    bottle_size_unit = NULL,
                                                    bottle_lasts = "liq_lasts",
                                                    bottle_lasts_unit = NULL,
                                                    preparation_dose = "liq_strength",
                                                    preparation_unit = NULL,
                                                    timeperiod = "1 day",
                                                    unit_cost_data = med_costs,
                                                    unit_cost_column = "UnitCost",
                                                    cost_calculated_per = "Basis",
                                                    strength_column = "Strength",
                                                    list_of_code_names = NULL,
                                                    list_of_code_brand = NULL,
                                                    list_of_code_bottle_size_unit = NULL,
                                                    list_of_code_bottle_lasts_unit = NULL,
                                                    list_preparation_dose_unit = NULL,
                                                    eqdose_covtab = table,
                                                    basis_strength_unit = NULL))


expect_error(costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                    name_med = "liq_name",
                                                    brand_med =  "liq_brand",
                                                    bottle_size = "liq_bottle_size",
                                                    bottle_size_unit = NULL,
                                                    bottle_lasts = NULL,
                                                    bottle_lasts_unit = NULL,
                                                    preparation_dose = "liq_strength",
                                                    preparation_unit = NULL,
                                                    timeperiod = "1 day",
                                                    unit_cost_data = med_costs,
                                                    unit_cost_column = "UnitCost",
                                                    cost_calculated_per = "Basis",
                                                    strength_column = "Strength",
                                                    list_of_code_names = NULL,
                                                    list_of_code_brand = NULL,
                                                    list_of_code_bottle_size_unit = NULL,
                                                    list_of_code_bottle_lasts_unit = NULL,
                                                    list_preparation_dose_unit = NULL,
                                                    eqdose_covtab = table,
                                                    basis_strength_unit = NULL))

expect_error(costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                    name_med = "liq_name",
                                                    brand_med =  "liq_brand",
                                                    bottle_size = "liq_bottle_size",
                                                    bottle_size_unit = NULL,
                                                    bottle_lasts = "liq_lasts",
                                                    bottle_lasts_unit = NULL,
                                                    preparation_dose = NULL,
                                                    preparation_unit = NULL,
                                                    timeperiod = "1 day",
                                                    unit_cost_data = med_costs,
                                                    unit_cost_column = "UnitCost",
                                                    cost_calculated_per = "Basis",
                                                    strength_column = "Strength",
                                                    list_of_code_names = NULL,
                                                    list_of_code_brand = NULL,
                                                    list_of_code_bottle_size_unit = NULL,
                                                    list_of_code_bottle_lasts_unit = NULL,
                                                    list_preparation_dose_unit = NULL,
                                                    eqdose_covtab = table,
                                                    basis_strength_unit = NULL))

expect_error(costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                    name_med = "liq_name",
                                                    brand_med =  "liq_brand",
                                                    bottle_size = "liq_bottle_size",
                                                    bottle_size_unit = NULL,
                                                    bottle_lasts = "liq_lasts",
                                                    bottle_lasts_unit = NULL,
                                                    preparation_dose = "liq_strength",
                                                    preparation_unit = NULL,
                                                    timeperiod = "1 day",
                                                    unit_cost_data = NULL,
                                                    unit_cost_column = "UnitCost",
                                                    cost_calculated_per = "Basis",
                                                    strength_column = "Strength",
                                                    list_of_code_names = NULL,
                                                    list_of_code_brand = NULL,
                                                    list_of_code_bottle_size_unit = NULL,
                                                    list_of_code_bottle_lasts_unit = NULL,
                                                    list_preparation_dose_unit = NULL,
                                                    eqdose_covtab = table,
                                                    basis_strength_unit = NULL))

expect_error(costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                    name_med = "liq_name",
                                                    brand_med =  "liq_brand",
                                                    bottle_size = "liq_bottle_size",
                                                    bottle_size_unit = NULL,
                                                    bottle_lasts = "liq_lasts",
                                                    bottle_lasts_unit = NULL,
                                                    preparation_dose = "liq_strength",
                                                    preparation_unit = NULL,
                                                    timeperiod = "1 day",
                                                    unit_cost_data = med_costs,
                                                    unit_cost_column = NULL,
                                                    cost_calculated_per = "Basis",
                                                    strength_column = "Strength",
                                                    list_of_code_names = NULL,
                                                    list_of_code_brand = NULL,
                                                    list_of_code_bottle_size_unit = NULL,
                                                    list_of_code_bottle_lasts_unit = NULL,
                                                    list_preparation_dose_unit = NULL,
                                                    eqdose_covtab = table,
                                                    basis_strength_unit = NULL))

expect_error(costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                    name_med = "liq_name",
                                                    brand_med =  "liq_brand",
                                                    bottle_size = "liq_bottle_size",
                                                    bottle_size_unit = NULL,
                                                    bottle_lasts = "liq_lasts",
                                                    bottle_lasts_unit = NULL,
                                                    preparation_dose = "liq_strength",
                                                    preparation_unit = NULL,
                                                    timeperiod = "1 day",
                                                    unit_cost_data = med_costs,
                                                    unit_cost_column = "UnitCost",
                                                    cost_calculated_per = NULL,
                                                    strength_column = "Strength",
                                                    list_of_code_names = NULL,
                                                    list_of_code_brand = NULL,
                                                    list_of_code_bottle_size_unit = NULL,
                                                    list_of_code_bottle_lasts_unit = NULL,
                                                    list_preparation_dose_unit = NULL,
                                                    eqdose_covtab = table,
                                                    basis_strength_unit = NULL))

expect_error(costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                    name_med = "liq_name",
                                                    brand_med =  "liq_brand",
                                                    bottle_size = "liq_bottle_size",
                                                    bottle_size_unit = NULL,
                                                    bottle_lasts = "liq_lasts",
                                                    bottle_lasts_unit = NULL,
                                                    preparation_dose = "liq_strength",
                                                    preparation_unit = NULL,
                                                    timeperiod = "1 day",
                                                    unit_cost_data = med_costs,
                                                    unit_cost_column = "UnitCost",
                                                    cost_calculated_per = "Basis",
                                                    strength_column = NULL,
                                                    list_of_code_names = NULL,
                                                    list_of_code_brand = NULL,
                                                    list_of_code_bottle_size_unit = NULL,
                                                    list_of_code_bottle_lasts_unit = NULL,
                                                    list_preparation_dose_unit = NULL,
                                                    eqdose_covtab = table,
                                                    basis_strength_unit = NULL))

res <- costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                              name_med = "liq_name",
                                              brand_med =  "liq_brand",
                                              bottle_size = "liq_bottle_size",
                                              bottle_size_unit = NULL,
                                              bottle_lasts = "liq_lasts",
                                              bottle_lasts_unit = NULL,
                                              preparation_dose = "liq_strength",
                                              preparation_unit = NULL,
                                              timeperiod = "1 day",
                                              unit_cost_data = med_costs,
                                              unit_cost_column = "UnitCost",
                                              cost_calculated_per = "Basis",
                                              strength_column = "Strength",
                                              list_of_code_names = NULL,
                                              list_of_code_brand = NULL,
                                              list_of_code_bottle_size_unit = NULL,
                                              list_of_code_bottle_lasts_unit = NULL,
                                              list_preparation_dose_unit = NULL,
                                              eqdose_covtab = table,
                                              basis_strength_unit = "mg/ml")
expect_equal(res$totmed_per_equiv_period_liquid, c(56.16, 26.79), tolerance = 1e-3)



res <- costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                              name_med = "liq_name",
                                              brand_med =  "liq_brand",
                                              bottle_size = "liq_bottle_size",
                                              bottle_size_unit = NULL,
                                              bottle_lasts = "liq_lasts",
                                              bottle_lasts_unit = NULL,
                                              preparation_dose = "liq_strength",
                                              preparation_unit = NULL,
                                              timeperiod = "1 day",
                                              unit_cost_data = med_costs,
                                              unit_cost_column = "UnitCost",
                                              cost_calculated_per = "Basis",
                                              strength_column = "Strength",
                                              list_of_code_names = NULL,
                                              list_of_code_brand = NULL,
                                              list_of_code_bottle_size_unit = NULL,
                                              list_of_code_bottle_lasts_unit = NULL,
                                              list_preparation_dose_unit = NULL,
                                              eqdose_covtab = table,
                                              basis_strength_unit = NA)
expect_equal(res$totmed_per_equiv_period_liquid, c(56.2, 26.79), tolerance = 1e-3)
expect_error(costing_opioid_liquids_averageMED_wide(ind_part_data = ind_part_data,
                                                    name_med = "liq_name",
                                                    brand_med =  "liq_brand",
                                                    bottle_size = "liq_bottle_size",
                                                    bottle_size_unit = NULL,
                                                    bottle_lasts = "liq_lasts",
                                                    bottle_lasts_unit = NULL,
                                                    preparation_dose = "liq_strength",
                                                    preparation_unit = NULL,
                                                    timeperiod = "1 day",
                                                    unit_cost_data = med_costs,
                                                    unit_cost_column = "UnitCost",
                                                    cost_calculated_per = "Basis",
                                                    strength_column = "Strength",
                                                    list_of_code_names = NULL,
                                                    list_of_code_brand = NULL,
                                                    list_of_code_bottle_size_unit = NULL,
                                                    list_of_code_bottle_lasts_unit = NULL,
                                                    list_preparation_dose_unit = NULL,
                                                    eqdose_covtab = table,
                                                    basis_strength_unit = "mg/hr"))

med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_codes.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
res <- costing_opioid_liquids_averageMED_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  "liq_brand",
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = "bottle_unit",
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = "lasts_unit",
                          preparation_dose = "liq_strength",
                          preparation_unit = "str_unit",
                          timeperiod = "1 day",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names =
                            list(c(1, 2), c("Morphine", "Oxycodone")),
                          list_of_code_brand =
                            list(c(1, 2), c("Oramorph", "Oxycodone")),
                          list_of_code_bottle_size_unit =
                            list(c(1, 2), c("ml", "l")),
                          list_of_code_bottle_lasts_unit =
                            list(c(1, 2, 3), c("days", "weeks", "months")),
                          list_preparation_dose_unit =
                            list(c(1, 2), c("mg/ml", "g/ml")),
                          eqdose_covtab = NULL,
                          basis_strength_unit = "mg/ml")

expect_equal(res$totmed_per_equiv_period_liquid, c(54.106, 17.857), tolerance = 1e-3)




med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_codes.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
res <- costing_opioid_liquids_averageMED_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  bottle_size = "liq_bottle_size",
  bottle_size_unit = "bottle_unit",
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = "lasts_unit",
  preparation_dose = "liq_strength",
  preparation_unit = "str_unit",
  timeperiod = "1 day",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names =
    list(c(1, 2), c("Morphine", "Oxycodone")),
  list_of_code_brand =
    list(c(1, 2), c("Oramorph", "Oxycodone")),
  list_of_code_bottle_size_unit =
    list(c(1, 2), c("ml", "l")),
  list_of_code_bottle_lasts_unit =
    list(c(1, 2, 3), c("days", "weeks", "months")),
  list_preparation_dose_unit =
    list(c(1, 2), c("mg/ml", "g/ml")),
  eqdose_covtab = NULL,
  basis_strength_unit = NULL)

expect_equal(res$totmed_per_equiv_period_liquid, c(54.106, 17.857), tolerance = 1e-3)


data_file <- system.file("extdata",
                         "medication_liq_codes_namenotsameasdose.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
expect_error(costing_opioid_liquids_averageMED_wide(
                  ind_part_data = ind_part_data,
                  name_med = "liq_name",
                  brand_med =  "liq_brand",
                  bottle_size = "liq_bottle_size",
                  bottle_size_unit = "bottle_unit",
                  bottle_lasts = "liq_lasts",
                  bottle_lasts_unit = "lasts_unit",
                  preparation_dose = "liq_strength",
                  preparation_unit = "str_unit",
                  timeperiod = "1 day",
                  unit_cost_data = med_costs,
                  unit_cost_column = "UnitCost",
                  cost_calculated_per = "Basis",
                  strength_column = "Strength",
                  list_of_code_names =
                    list(c(1, 2), c("Morphine", "Oxycodone")),
                  list_of_code_brand =
                    list(c(1, 2), c("Oramorph", "Oxycodone")),
                  list_of_code_bottle_size_unit =
                    list(c(1), c("ml")),
                  list_of_code_bottle_lasts_unit =
                    list(c(1, 2, 3), c("days", "weeks", "months")),
                  list_preparation_dose_unit =
                    list(c(1), c("mg/ml")),
                  eqdose_covtab = NULL,
                  basis_strength_unit = "mg/ml"))

med_costs_file <-
  system.file("extdata", "medicaton_costs_all_errorbrand.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)

expect_error(costing_opioid_liquids_averageMED_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  "liq_brand",
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = "liq_pre",
                          preparation_unit = NULL,
                          timeperiod = "1 day",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_bottle_size_unit =
                            NULL,
                          list_of_code_bottle_lasts_unit =
                            NULL,
                          list_preparation_dose_unit =
                            NULL,
                          eqdose_covtab = NULL,
                          basis_strength_unit = "mg/ml"))

med_costs_file <-
  system.file("extdata", "medicaton_costs_all.xlsx",
              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_liqlastsbeforeday.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
res <- costing_opioid_liquids_averageMED_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = "liq_pre",
  preparation_unit = NULL,
  timeperiod = "1 day",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_bottle_size_unit =
    NULL,
  list_of_code_bottle_lasts_unit =
    NULL,
  list_preparation_dose_unit =
    NULL,
  eqdose_covtab = NULL,
  basis_strength_unit = "mg/ml")

expect_equal(res$totmed_per_equiv_period_liquid, c(1204.106,  17.857), tolerance = 1e-3)


med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_nonamecol.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the med

expect_error(costing_opioid_liquids_averageMED_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = "liq_strength",
  preparation_unit = NULL,
  timeperiod = "1 day",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_bottle_size_unit =
    NULL,
  list_of_code_bottle_lasts_unit =
    NULL,
  list_preparation_dose_unit =
    NULL,
  eqdose_covtab = NULL,
  basis_strength_unit = "mg/ml"))


med_costs_file <- system.file("extdata",
                "medication_costs_all_brand_nounitcostcol.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(costing_opioid_liquids_averageMED_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = "liq_strength",
  preparation_unit = NULL,
  timeperiod = "1 day",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_bottle_size_unit =
    NULL,
  list_of_code_bottle_lasts_unit =
    NULL,
  list_preparation_dose_unit =
    NULL,
  eqdose_covtab = NULL,
  basis_strength_unit = "mg/ml"))


med_costs_file <- system.file("extdata",
                "medication_costs_all_bottlevol_notmatching.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(costing_opioid_liquids_averageMED_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = "liq_strength",
  preparation_unit = NULL,
  timeperiod = "1 day",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_bottle_size_unit =
    NULL,
  list_of_code_bottle_lasts_unit =
    NULL,
  list_preparation_dose_unit =
    NULL,
  eqdose_covtab = NULL,
  basis_strength_unit = "mg/ml"))

med_costs_file <- system.file("extdata",
                  "medication_costs_all_notcostedperbottle.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
res <- costing_opioid_liquids_averageMED_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = "liq_strength",
  preparation_unit = NULL,
  timeperiod = "1 day",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_bottle_size_unit =
    NULL,
  list_of_code_bottle_lasts_unit =
    NULL,
  list_preparation_dose_unit =
    NULL,
  eqdose_covtab = NULL,
  basis_strength_unit = "mg/ml")
expect_equal(res$totmed_per_equiv_period_liquid, c(54.106, 17.857), tolerance = 1e-3)




med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_strength_unitdiff.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(costing_opioid_liquids_averageMED_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = "liq_pre",
  preparation_unit = NULL,
  timeperiod = "1 day",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_bottle_size_unit =
    NULL,
  list_of_code_bottle_lasts_unit =
    NULL,
  list_preparation_dose_unit =
    NULL,
  eqdose_covtab = NULL,
  basis_strength_unit = "mg/ml"))




med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_bottlevol_unitdiff.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
res <- costing_opioid_liquids_averageMED_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = "liq_strength",
  preparation_unit = NULL,
  timeperiod = "1 day",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_bottle_size_unit =
    NULL,
  list_of_code_bottle_lasts_unit =
    NULL,
  list_preparation_dose_unit =
    NULL,
  eqdose_covtab = NULL,
  basis_strength_unit = "mg/ml")
expect_equal(res$totmed_per_equiv_period_liquid, c(50004, 17.857), tolerance = 1e-3)


med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc_unit_notright.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(costing_opioid_liquids_averageMED_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = "liq_strength",
  preparation_unit = NULL,
  timeperiod = "1 day",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_bottle_size_unit =
    NULL,
  list_of_code_bottle_lasts_unit =
    NULL,
  list_preparation_dose_unit =
    NULL,
  eqdose_covtab = table,
  basis_strength_unit = "mg/ml"))


med_costs_file <- system.file("extdata", "medicaton_costs_all_preparation_unitwrong.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_all.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(costing_opioid_liquids_averageMED_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  bottle_size = "liq_bottle_size",
  bottle_size_unit = NULL,
  bottle_lasts = "liq_lasts",
  bottle_lasts_unit = NULL,
  preparation_dose = "liq_strength",
  preparation_unit = NULL,
  timeperiod = "1 day",
  unit_cost_data = med_costs,
  unit_cost_column = "UnitCost",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_bottle_size_unit =
    NULL,
  list_of_code_bottle_lasts_unit =
    NULL,
  list_preparation_dose_unit =
    NULL,
  eqdose_covtab = table,
  basis_strength_unit = "mg/ml"))
})
###############################################################################
data_file <- system.file("extdata", "med_liquids_timepoints.csv",
                         package = "packDAMipd")
med_liquids_timepoints <- load_trial_data(data_file)
opioids_liquids_timepoints <- med_liquids_timepoints[med_liquids_timepoints$liquiddrugname != 97, ]
list_code_names <- list(seq(1:3), c("Morphine", "Oxycodone", "Methadone"))

list_code_dose_unit <- list(seq(1:1), "mg/ml")

opioids_liquids_timepoints[["liquidfrequency_actual"]] <-
  paste( opioids_liquids_timepoints$liquidfrequency, "day", sep = "")

opioids_liquids_timepoints[["liquidpreparation"]] <-
  paste( opioids_liquids_timepoints$liquidmedicationstrengthmass, "mg", "/", opioids_liquids_timepoints$liquidmedicationstrengthvol, "ml", sep = "")

opioids_liquids_timepoints[["liquidbottlesize_actual"]] <- ""
opioids_liquids_timepoints[opioids_liquids_timepoints$liquidbottlesize == 1 & !is.na(opioids_liquids_timepoints$liquidbottlesize),]$liquidbottlesize_actual <- "100 ml"

opioids_liquids_timepoints[opioids_liquids_timepoints$liquidbottlesize == 2 & !is.na(opioids_liquids_timepoints$liquidbottlesize),]$liquidbottlesize_actual <- "200 ml"

opioids_liquids_timepoints[opioids_liquids_timepoints$liquidbottlesize == 3 & !is.na(opioids_liquids_timepoints$liquidbottlesize),]$liquidbottlesize_actual <- "300 ml"

opioids_liquids_timepoints[opioids_liquids_timepoints$liquidbottlesize == 4 & !is.na(opioids_liquids_timepoints$liquidbottlesize),]$liquidbottlesize_actual <- "400 ml"

opioids_liquids_timepoints[opioids_liquids_timepoints$liquidbottlesize == 5 & !is.na(opioids_liquids_timepoints$liquidbottlesize),]$liquidbottlesize_actual <- "500 ml"

# opioids_liquids_timepoints[opioids_liquids_timepoints$liquidbottlesize == 6 & !is.na(opioids_liquids_timepoints$liquidbottlesize),]$liquidbottlesize_actual <- "30 ml"

opioids_liquids_timepoints[opioids_liquids_timepoints$liquidbottlesize == 7 & !is.na(opioids_liquids_timepoints$liquidbottlesize),]$liquidbottlesize_actual <- "120 ml"

opioids_liquids_timepoints[opioids_liquids_timepoints$liquidbottlesize == 8 & !is.na(opioids_liquids_timepoints$liquidbottlesize),]$liquidbottlesize_actual <- "150 ml"

opioids_liquids_timepoints[opioids_liquids_timepoints$liquidbottlesize == 9 & !is.na(opioids_liquids_timepoints$liquidbottlesize),]$liquidbottlesize_actual <- "250 ml"

med_unit_costs <- load_trial_data(system.file("extdata", "Medications_unit_costs_all.xlsx", package = "packDAMipd"))
equiv_dose_calc <- load_trial_data(system.file("extdata", "Med_calc.xlsx", package = "packDAMipd"))

colindex <- which(colnames(opioids_liquids_timepoints) == "liquiddrugnameother")
colnames(opioids_liquids_timepoints)[colindex] <- "liquiddrugother"

opioids_liquids_timepoints[["liquid_actual"]] <- ""
for (i in 1:nrow(opioids_liquids_timepoints)) {
  m = opioids_liquids_timepoints$liquiddrugname[i]
  if (m == 97) {
    opioids_liquids_timepoints$liquid_actual[i] = opioids_liquids_timepoints$liquiddrugother[i]
  } else {
    opioids_liquids_timepoints$liquid_actual[i] = list_code_names[[2]][m]
  }
}
tmp <- opioids_liquids_timepoints[1,]
tmp <- packDAMipd::costing_opioid_liquids_averageMED_wide(
  ind_part_data = tmp,
  name_med = "liquid_actual",
  brand_med = "liquidbrandname",
  bottle_size = "liquidbottlesize_actual",
  bottle_size_unit = NULL,
  bottle_lasts = "liquidfrequency_actual",
  bottle_lasts_unit = NULL,
  preparation_dose = "liquidpreparation",
  timeperiod = "1 day",
  unit_cost_data = med_unit_costs,
  unit_cost_column = "PackageRate",
  cost_calculated_per = "Basis",
  strength_column = "Strength",
  list_of_code_names = NULL,
  list_of_code_brand = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = equiv_dose_calc,
  basis_strength_unit = NULL)
expect_equal(tmp$totmed_per_equiv_period_liquid, 25, tolerance = 1e-2)

###############################################################################

context("testing costing liquids when data being long format")
test_that("testing costing liquids when data being long format", {
  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_liq.xlsx",
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
  res <- costing_opioid_liquids_averageMED_long(the_columns,
    ind_part_data_long = ind_part_data_long,
    name_med = "liq_name",
    brand_med =  NULL,
    bottle_size = "liq_bottle_size",
    bottle_size_unit = NULL,
    bottle_lasts = "liq_lasts",
    bottle_lasts_unit = NULL,
    preparation_dose = "liq_pre",
    preparation_unit = NULL,
    timeperiod = "1 day",
    unit_cost_data = med_costs,
    unit_cost_column = "UnitCost",
    cost_calculated_per = "Basis",
    strength_column = "Strength",
    list_of_code_names = NULL,
    list_of_code_brand = NULL,
    list_of_code_bottle_size_unit = NULL,
    list_of_code_bottle_lasts_unit = NULL,
    list_preparation_dose_unit = NULL,
    eqdose_covtab = table,
    basis_strength_unit = NULL)

  index <- which(res$measurement == "totmed_per_equiv_period_liquid")[1]

  expect_equal(as.numeric(res$value[index]), 56.2, tolerance = 1e-3)


  expect_error(costing_opioid_liquids_averageMED_long(the_columns,
                            ind_part_data_long = NULL,
                            name_med = "liq_name",
                            brand_med =  NULL,
                            bottle_size = "liq_bottle_size",
                            bottle_size_unit = NULL,
                            bottle_lasts = "liq_lasts",
                            bottle_lasts_unit = NULL,
                            preparation_dose = "liq_pre",
                            preparation_unit = NULL,
                            timeperiod = "1 day",
                            unit_cost_data = med_costs,
                            unit_cost_column = "UnitCost",
                            cost_calculated_per = "Basis",
                            strength_column = "Strength",
                            list_of_code_names = NULL,
                            list_of_code_brand = NULL,
                            list_of_code_bottle_size_unit = NULL,
                            list_of_code_bottle_lasts_unit = NULL,
                            list_preparation_dose_unit = NULL,
                            eqdose_covtab = table,
                            basis_strength_unit = NULL))

})
