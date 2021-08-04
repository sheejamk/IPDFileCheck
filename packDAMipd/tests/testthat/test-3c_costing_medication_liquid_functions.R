#####################################################################
context("testing microcosting liquids")
test_that("testing microcosting liquids", {
  med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                                package = "packDAMipd")
  data_file <- system.file("extdata", "medication_liq_brand_empty.xlsx",
                           package = "packDAMipd")
  ind_part_data <- load_trial_data(data_file)
  med_costs <- load_trial_data(med_costs_file)
  conv_file <- system.file("extdata", "Med_calc.xlsx",
                           package = "packDAMipd")
  table <- load_trial_data(conv_file)
  res <- microcosting_liquids_wide(ind_part_data = ind_part_data,
                                   name_med = "liq_name",
                                   brand_med =  "liq_brand",
                                   dose_med = "liq_strength",
                                   unit_med = NULL,
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
                                   list_of_code_dose_unit = NULL,
                                   list_of_code_bottle_size_unit = NULL,
                                   list_of_code_bottle_lasts_unit = NULL,
                                   list_preparation_dose_unit = NULL,
                                   eqdose_covtab = table,
                                   basis_strength_unit = NULL)

expect_equal(res$totmed_wt_period_liquid_mg, c(54.106, 17.857), tolerance = 1e-3)

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

res <- microcosting_liquids_wide(ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  NULL,
                                 dose_med = "liq_strength",
                                 unit_med = NULL,
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
                                 list_of_code_dose_unit = NULL,
                                 list_of_code_bottle_size_unit = NULL,
                                 list_of_code_bottle_lasts_unit = NULL,
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = table,
                                 basis_strength_unit = NULL)


expect_equal(res$totmed_wt_period_liquid_mg, c(54.106, 17.857), tolerance = 1e-3)


expect_error(microcosting_liquids_wide(
                          ind_part_data = NULL,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
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
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = NULL,
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
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
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = NULL,
                          unit_med = NULL,
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
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = NULL,
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
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = NULL,
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
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = NULL,
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
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = NULL,
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "1 day",
                          unit_cost_data = NULL,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "1 day",
                          unit_cost_data = med_costs,
                          unit_cost_column = NULL,
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = NULL,
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = NULL,
                          preparation_dose = NULL,
                          preparation_unit = NULL,
                          timeperiod = "1 day",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = NULL,
                          strength_column = "Strength",
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

expect_error(microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
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
                          strength_column = NULL,
                          list_of_code_names = NULL,
                          list_of_code_brand = NULL,
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = table,
                          basis_strength_unit = NULL
                          ))

res <- microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  NULL,
                          dose_med = "liq_strength",
                          unit_med = NULL,
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
                          list_of_code_dose_unit = NULL,
                          list_of_code_bottle_size_unit = NULL,
                          list_of_code_bottle_lasts_unit = NULL,
                          list_preparation_dose_unit = NULL,
                          eqdose_covtab = NULL,
                          basis_strength_unit = NULL
                          )
expect_equal(res$totmed_wt_period_liquid_mg, c(54.106, 17.857), tolerance = 1e-3)



res <- microcosting_liquids_wide(ind_part_data = ind_part_data,
                                       name_med = "liq_name",
                                       brand_med =  NULL,
                                       dose_med = "liq_strength",
                                       unit_med = NULL,
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
                                       list_of_code_dose_unit = NULL,
                                       list_of_code_bottle_size_unit = NULL,
                                       list_of_code_bottle_lasts_unit = NULL,
                                       list_preparation_dose_unit = NULL,
                                       eqdose_covtab = NULL,
                                       basis_strength_unit = NA)
expect_equal(res$totmed_wt_period_liquid_mg, c(54.106, 17.857), tolerance = 1e-3)
res <- microcosting_liquids_wide(
                                 ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  NULL,
                                 dose_med = "liq_strength",
                                 unit_med = NULL,
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
                                 list_of_code_dose_unit = NULL,
                                 list_of_code_bottle_size_unit = NULL,
                                 list_of_code_bottle_lasts_unit = NULL,
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = NULL,
                                 basis_strength_unit = "mg/ml")
expect_equal(res$totmed_wt_period_liquid_mg, c(54.106, 17.857), tolerance = 1e-3)
expect_error(microcosting_liquids_wide(
                                 ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  NULL,
                                 dose_med = "liq_strength",
                                 unit_med = NULL,
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
                                 list_of_code_dose_unit = NULL,
                                 list_of_code_bottle_size_unit = NULL,
                                 list_of_code_bottle_lasts_unit = NULL,
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = NULL,
                                 basis_strength_unit = "mg/hh"))

med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_codes.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
res <- microcosting_liquids_wide(
                          ind_part_data = ind_part_data,
                          name_med = "liq_name",
                          brand_med =  "liq_brand",
                          dose_med = "liq_strength",
                          unit_med = "str_unit",
                          bottle_size = "liq_bottle_size",
                          bottle_size_unit = "bottle_unit",
                          bottle_lasts = "liq_lasts",
                          bottle_lasts_unit = "lasts_unit",
                          preparation_dose = "liq_pre",
                          preparation_unit = "pre_unit",
                          timeperiod = "1 day",
                          unit_cost_data = med_costs,
                          unit_cost_column = "UnitCost",
                          cost_calculated_per = "Basis",
                          strength_column = "Strength",
                          list_of_code_names =
                            list(c(1, 2), c("Morphine", "Oxycodone")),
                          list_of_code_brand =
                            list(c(1, 2), c("Oramorph", "Oxycodone")),
                          list_of_code_dose_unit =
                            list(c(1, 2), c("mg/ml", "g/ml")),
                          list_of_code_bottle_size_unit =
                            list(c(1, 2), c("ml", "l")),
                          list_of_code_bottle_lasts_unit =
                            list(c(1, 2, 3), c("days", "weeks", "months")),
                          list_preparation_dose_unit =
                            list(c(1, 2), c("mg/ml", "g/ml")),
                          eqdose_covtab = NULL,
                          basis_strength_unit = "mg/ml")

expect_equal(res$totmed_wt_period_liquid_mg, c(54.106, 17.857), tolerance = 1e-3)


med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_codes_notprep.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
res <- microcosting_liquids_wide(
                                 ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  "liq_brand",
                                 dose_med = "liq_strength",
                                 unit_med = "str_unit",
                                 bottle_size = "liq_bottle_size",
                                 bottle_size_unit = "bottle_unit",
                                 bottle_lasts = "liq_lasts",
                                 bottle_lasts_unit = "lasts_unit",
                                 preparation_dose = "liq_pre",
                                 preparation_unit = NULL,
                                 timeperiod = "1 day",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names =
                                   list(c(1, 2), c("Morphine", "Oxycodone")),
                                 list_of_code_brand =
                                   list(c(1, 2), c("Oramorph", "Oxycodone")),
                                 list_of_code_dose_unit =
                                   list(c(1, 2), c("mg/ml", "g/ml")),
                                 list_of_code_bottle_size_unit =
                                   list(c(1, 2), c("ml", "l")),
                                 list_of_code_bottle_lasts_unit =
                                   list(c(1, 2, 3), c("days", "weeks", "months")),
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = NULL,
                                 basis_strength_unit = "mg/ml")

expect_equal(res$totmed_wt_period_liquid_mg, c(54.106, 17.857), tolerance = 1e-3)

res <- microcosting_liquids_wide(
                                 ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  "liq_brand",
                                 dose_med = "liq_strength",
                                 unit_med = "str_unit",
                                 bottle_size = "liq_bottle_size",
                                 bottle_size_unit = "bottle_unit",
                                 bottle_lasts = "liq_lasts",
                                 bottle_lasts_unit = "lasts_unit",
                                 preparation_dose = "liq_pre",
                                 preparation_unit = NULL,
                                 timeperiod = "1 day",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names =
                                   list(c(1, 2), c("Morphine", "Oxycodone")),
                                 list_of_code_brand =
                                   list(c(1, 2), c("Oramorph", "Oxycodone")),
                                 list_of_code_dose_unit =
                                   list(c(1, 2), c("mg/ml", "g/ml")),
                                 list_of_code_bottle_size_unit =
                                   list(c(1, 2), c("ml", "l")),
                                 list_of_code_bottle_lasts_unit =
                                   list(c(1, 2, 3), c("days", "weeks", "months")),
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = NA,
                                 basis_strength_unit = "mg/ml")

expect_equal(res$totmed_wt_period_liquid_mg, c(54.106, 17.857), tolerance = 1e-3)


data_file <- system.file("extdata",
                         "medication_liq_codes_namenotsameasdose.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)

expect_error(microcosting_liquids_wide(
                                 ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  "liq_brand",
                                 dose_med = "liq_strength",
                                 unit_med = "str_unit",
                                 bottle_size = "liq_bottle_size",
                                 bottle_size_unit = "bottle_unit",
                                 bottle_lasts = "liq_lasts",
                                 bottle_lasts_unit = "lasts_unit",
                                 preparation_dose = "liq_pre",
                                 preparation_unit = "pre_unit",
                                 timeperiod = "1 day",
                                 unit_cost_data = med_costs,
                                 unit_cost_column = "UnitCost",
                                 cost_calculated_per = "Basis",
                                 strength_column = "Strength",
                                 list_of_code_names =
                                   list(c(1, 2), c("Morphine", "Oxycodone")),
                                 list_of_code_brand =
                                   list(c(1, 2), c("Oramorph", "Oxycodone")),
                                 list_of_code_dose_unit =
                                   list(c(1, 2), c("mg/ml", "g/ml")),
                                 list_of_code_bottle_size_unit =
                                   list(c(1, 2), c("ml", "l")),
                                 list_of_code_bottle_lasts_unit =
                                   list(c(1, 2, 3), c("days", "weeks", "months")),
                                 list_preparation_dose_unit =
                                   list(c(1, 2), c("mg/ml", "g/ml")),
                                 eqdose_covtab = NA,
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
#using the package price when the brand name of the me
expect_error(microcosting_liquids_wide(
                                 ind_part_data = ind_part_data,
                                 name_med = "liq_name",
                                 brand_med =  "liq_brand",
                                 dose_med = "liq_strength",
                                 unit_med = NULL,
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
                                 list_of_code_dose_unit = NULL,
                                 list_of_code_bottle_size_unit = NULL,
                                 list_of_code_bottle_lasts_unit = NULL,
                                 list_preparation_dose_unit = NULL,
                                 eqdose_covtab = table,
                                 basis_strength_unit = NULL
                                 ))

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
res <- microcosting_liquids_wide(
     ind_part_data = ind_part_data,
                                       name_med = "liq_name",
                                       brand_med =  "liq_brand",
                                       dose_med = "liq_strength",
                                       unit_med = NULL,
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
                                       list_of_code_dose_unit = NULL,
                                       list_of_code_bottle_size_unit = NULL,
                                       list_of_code_bottle_lasts_unit = NULL,
                                       list_preparation_dose_unit = NULL,
                                       eqdose_covtab = table,
                                       basis_strength_unit = NULL)

expect_equal(res$totmed_wt_period_liquid_mg, c(1204.106,  17.857), tolerance = 1e-3)


med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq_nonamecol.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  NULL,
  dose_med = "liq_strength",
  unit_med = NULL,
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
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))

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
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  NULL,
  dose_med = "liq_strength",
  unit_med = NULL,
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
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))

med_costs_file <- system.file("extdata",
                  "medication_costs_all_nomatchingdose.xlsx",
                  package = "packDAMipd")
data_file <- system.file("extdata", "medication_liq.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  dose_med = "liq_strength",
  unit_med = NULL,
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
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))



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
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  dose_med = "liq_strength",
  unit_med = NULL,
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
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))

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
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  dose_med = "liq_strength",
  unit_med = NULL,
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
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))

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
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  dose_med = "liq_strength",
  unit_med = NULL,
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
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))
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
expect_error(microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  dose_med = "liq_strength",
  unit_med = NULL,
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
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL))


med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
                              package = "packDAMipd")
data_file <- system.file("extdata", "medication_all.xlsx",
                         package = "packDAMipd")
ind_part_data <- load_trial_data(data_file)
med_costs <- load_trial_data(med_costs_file)
conv_file <- system.file("extdata", "Med_calc.xlsx",
                         package = "packDAMipd")
table <- load_trial_data(conv_file)
#using the package price when the brand name of the me
res <- microcosting_liquids_wide(
  ind_part_data = ind_part_data,
  name_med = "liq_name",
  brand_med =  "liq_brand",
  dose_med = "liq_strength",
  unit_med = NULL,
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
  list_of_code_dose_unit = NULL,
  list_of_code_bottle_size_unit = NULL,
  list_of_code_bottle_lasts_unit = NULL,
  list_preparation_dose_unit = NULL,
  eqdose_covtab = table,
  basis_strength_unit = NULL)
expect_equal(res$totmed_wt_period_liquid_mg, c(54.106, NA), tolerance = 1e-3)


})
###############################################################################

context("testing microcosting liquids when data being long format")
test_that("testing microcosting liquids when data being long format", {
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
  res <- microcosting_liquids_long(the_columns,
    ind_part_data_long = ind_part_data_long,
    name_med = "liq_name",
    brand_med =  NULL,
    dose_med = "liq_strength",
    unit_med = NULL,
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
    list_of_code_dose_unit = NULL,
    list_of_code_bottle_size_unit = NULL,
    list_of_code_bottle_lasts_unit = NULL,
    list_preparation_dose_unit = NULL,
    eqdose_covtab = table,
    basis_strength_unit = NULL)

  index <- which(res$measurement == "totmed_wt_period_liquid_mg")[1]

  expect_equal(as.numeric(res$value[index]), 54.106, tolerance = 1e-3)


  expect_error(microcosting_liquids_long(the_columns,
                            ind_part_data_long = NULL,
                            name_med = "liq_name",
                            brand_med =  NULL,
                            dose_med = "liq_strength",
                            unit_med = NULL,
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
                            list_of_code_dose_unit = NULL,
                            list_of_code_bottle_size_unit = NULL,
                            list_of_code_bottle_lasts_unit = NULL,
                            list_preparation_dose_unit = NULL,
                            eqdose_covtab = table,
                            basis_strength_unit = NULL))

})
