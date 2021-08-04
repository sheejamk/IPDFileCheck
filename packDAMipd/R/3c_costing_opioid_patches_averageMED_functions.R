
###############################################################################
#' Function to estimate the cost of patches taken (from IPD)
#' @param ind_part_data IPD
#' @param name_med name of medication
#' @param brand_med brand name of medication if revealed
#' @param dose_med dose of medication used
#' @param unit_med unit of medication ; use null if its along with the dose
#' @param no_taken how many taken
#' @param freq_taken frequency of medication
#' @param timeperiod time period for cost calculation
#' @param unit_cost_data  unit costs data
#' @param unit_cost_column column name of unit cost in unit_cost_data
#' @param cost_calculated_per column name of unit where the cost is calculated
#' @param strength_column column column name that contain strength of
#' medication
#' @param list_of_code_names if names is coded, give the code:name pairs,
#' optional
#' @param list_of_code_freq if frequency is coded, give the
#' code:frequency pairs, optional
#' @param list_of_code_dose_unit if unit is coded, give the code:unit pairs,
#' optional
#' @param list_of_code_brand if brand names  are coded, give the code:brand
#' pairs, optional
#' @param eqdose_cov_tab table to get the conversion factor for equivalent
#' doses, optional, but the column names have to unique
#' Similar to c("Drug",	"form", "unit",	"factor") or
#' c("Drug",	"form", "unit",	"conversion")

#' @param basis_strength_unit strength unit to be taken as basis
#' required for total medication calculations
#' @return the calculated cost of tablets along with original data
#' @examples
#' med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
#' package = "packDAMipd")
#' data_file <- system.file("extdata", "medication.xlsx",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(data_file)
#' med_costs <- load_trial_data(med_costs_file)
#' conv_file <- system.file("extdata", "Med_calc.xlsx", package = "packDAMipd")
#' table <- load_trial_data(conv_file)
#' res <- costing_opioid_patches_averageMED_wide(
#' ind_part_data = ind_part_data, name_med = "patch_name",
#' brand_med = "patch_brand", dose_med = "patch_strength", unit_med = NULL,
#' no_taken = "patch_no_taken", freq_taken = "patch_frequency",
#' timeperiod = "4 months", unit_cost_data = med_costs,
#' unit_cost_column = "UnitCost", cost_calculated_per  = "Basis",
#' strength_column = "Strength", list_of_code_names = NULL,
#' list_of_code_freq = NULL, list_of_code_dose_unit = NULL,
#' list_of_code_brand = NULL, eqdose_cov_tab = table,
#' basis_strength_unit = "mcg/hr")
#' @export
#' @details
#' Assumes individual level data has name of medication, dose, dose unit,
#' number taken, frequency taken, and basis time
#' Assumes unit cost data contains the name of medication, form/type,
#' strength, unit of strength (or the unit in which the cost calculated),
#' preparation, unit cost, size and size unit
#' (in which name, forms, size, size unit, and preparation  are not passed on)
#'  @importFrom dplyr %>%

costing_opioid_patches_averageMED_wide <- function(ind_part_data,
                                      name_med,
                                      brand_med = NULL,
                                      dose_med,
                                      unit_med = NULL,
                                      no_taken, freq_taken,
                                      timeperiod,
                                      unit_cost_data,
                                      unit_cost_column,
                                      cost_calculated_per,
                                      strength_column,
                                      list_of_code_names = NULL,
                                      list_of_code_freq = NULL,
                                      list_of_code_dose_unit = NULL,
                                      list_of_code_brand = NULL,
                                      eqdose_cov_tab = NULL,
                                      basis_strength_unit = NULL) {
  internal_basis_time <- "day"
  generated_list <- generate_wt_time_units()
  wt_per_time_units <- generated_list$weight_per_times
  wt_units <- generated_list$weight_units
  time_units <-  generated_list$time_units
  #Error - data should not be NULL
  if (is.null(ind_part_data) | is.null(unit_cost_data))
    stop("data should not be NULL")

  #Checking if the required parameters are NULL or NA
  variables_check <- list(name_med, dose_med,
                          no_taken, freq_taken,
                          timeperiod, unit_cost_column,
                          cost_calculated_per, strength_column)
  results <- sapply(variables_check, check_null_na)
  names_check <- c("name_med", "dose_med",
                   "no_taken", "freq_taken",
                   "timeperiod", "unit_cost_column",
                   "cost_calculated_per", "strength_column")
  if (any(results != 0)) {
    indices <- which(results < 0)
    stop(paste("Error - the variables can not be NULL or NA,
               check the variable(s)", names_check[indices]))
  }
  if (!is.null(basis_strength_unit)) {
    if (is.na(basis_strength_unit))
      basis_strength_unit <- "mcg/hr"
  } else {
    basis_strength_unit <- "mcg/hr"
  }
  if (!(basis_strength_unit %in% wt_per_time_units))
    stop("the basis strength unit is not valid")

  index <- stringr::str_locate(basis_strength_unit, "/")
  basis_wt_unit <- stringr::str_sub(basis_strength_unit, 1, index[1] - 1)
  basis_time_unit <- stringr::str_sub(basis_strength_unit, index[2] + 1,
                                      nchar(basis_strength_unit))

  brand_check <- return0_if_not_null_na(brand_med)
  unit_med_check <-  return0_if_not_null_na(unit_med)

  check_list <- c(unit_med_check, brand_check)
  partial_list <- c(name_med, dose_med, no_taken, freq_taken)
  another_list <- list(unit_med, brand_med)
  another_list[check_list == -1] <- -1
  info_list <- unlist(append(partial_list, another_list))

  ipd_cols_exists <- list()
  for (i in seq_len(length(info_list))) {
    if (info_list[i] != -1) {
      check <- IPDFileCheck::check_column_exists(info_list[i], ind_part_data)
      res <- grep(info_list[i], colnames(ind_part_data))
      if (sum(check) != 0) {
        if (length(res) == 0)
          stop("Atleast one of the required columns not found")
        ipd_cols_exists[length(ipd_cols_exists) + 1] <- list(res)
      } else {
        ipd_cols_exists[length(ipd_cols_exists) + 1] <- list(res)
      }
    } else {
      ipd_cols_exists[length(ipd_cols_exists) + 1] <- -1
    }
  }
  names_med_cols <- unlist(ipd_cols_exists[1])
  doses_med_cols <- unlist(ipd_cols_exists[2])
  numbers_taken_cols <- unlist(ipd_cols_exists[3])
  freq_taken_cols <- unlist(ipd_cols_exists[4])
  if (unit_med_check != -1)
    unit_med_cols  <- unlist(ipd_cols_exists[5])
  if (brand_check != -1)
    brand_med_cols  <- unlist(ipd_cols_exists[6])

  # if the codes are being used for name, dosage, frequency and time period
  # list_of_code_names is a list of (list of codes and list of names)
  # if they are valid, assign the names to codes, read the code from data
  # and read the corresponding names using the earlier assignment
  names_from_code <- encode_codes_data(list_of_code_names, names_med_cols,
                                       ind_part_data)
  if (is.null(unlist(names_from_code)) | sum(is.na(unlist(names_from_code))) ==
      length(unlist(names_from_code))) {
    stop("Error - name_from_code can not be null - check the input for
         list of names and codes")
  }
  freq_desc_from_code <- encode_codes_data(list_of_code_freq, freq_taken_cols,
                                           ind_part_data)
  freq_given_basis_list <- as.list(as.data.frame(t(freq_desc_from_code)))
  freq_given_basis <- list()
  for (i in seq_len(length(freq_given_basis_list))) {
    this_list <- unlist(freq_given_basis_list[i])
    this_row <- lapply(this_list, convert_freq_diff_basis, internal_basis_time)
    freq_given_basis <- append(freq_given_basis, this_row)
  }
  freq_given_basis <- unlist(freq_given_basis)
  freq_given_basis <- matrix(freq_given_basis,
                             nrow = dim(freq_desc_from_code)[1], byrow = TRUE)
  colnames(freq_given_basis) <- colnames(freq_desc_from_code)
  freq_given_basis <- as.data.frame(freq_given_basis)

  if (brand_check != -1) {
    brand_and_code <- encode_codes_data(list_of_code_brand, brand_med_cols,
                                        ind_part_data)
    if (is.null(unlist(brand_and_code)) | sum(is.na(unlist(brand_and_code))) ==
        length(unlist(brand_and_code))) {
      stop("Error - size_unit_from_code can not be null - check the input for
         brand  code")
    }
  }
  if (unit_med_check == -1) {
    med_dose <- ind_part_data %>% dplyr::select(dplyr::all_of(doses_med_cols))
    med_dose_unlist <- unlist(med_dose)
    unit_from_code <- gsub("[0-9\\.]", "", med_dose_unlist)
    unit_from_code <- matrix(unit_from_code,
                             nrow = dim(med_dose)[1])
    colnames(unit_from_code) <- colnames(med_dose)
    unit_from_code <- as.data.frame(unit_from_code)
  } else {
    med_dose <- ind_part_data %>% dplyr::select(dplyr::all_of(doses_med_cols))
    med_dose <- as.data.frame(med_dose)
    unit_from_code <- encode_codes_data(list_of_code_dose_unit, unit_med_cols,
                                        ind_part_data)
  }
  if (is.null(unlist(unit_from_code)) | sum(is.na(unlist(unit_from_code))) ==
      length(unlist(unit_from_code))) {
    stop("Error - unit_from_code can not be null - check the input for
         list of units")
  }

  # check columns exist in unit cost  data
  info_list <- c(unit_cost_column, cost_calculated_per, strength_column)
  checks <- sapply(info_list, IPDFileCheck::check_column_exists, unit_cost_data)
  if (sum(checks) != 0) {
    stop("Atleast one of the required columns in unit cost data not found")
  }
  # get column names for name, form, dosage and unit
  name_pattern <- c("name", "drug", "medication", "patch")
  name_col_no <- get_single_col_multiple_pattern(name_pattern, unit_cost_data)

  form_pattern <- c("form")
  form_col_no <- get_single_col_multiple_pattern(form_pattern, unit_cost_data)

  unit_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                     cost_calculated_per)
  dosage_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                       strength_column)
  brand_pattern <- c("brand", "trade")
  res <- unlist(lapply(brand_pattern,
                       IPDFileCheck::get_colno_pattern_colname, colnames(unit_cost_data)))
  if (length(res[which(res != -1)]) < 1) {
    if (brand_check != -1) {
      stop("Error - No brand column in the unit cost data")
    } else{
      brand_col_no <- -1
    }
  } else {
    brand_col_no <-
      get_single_col_multiple_pattern(brand_pattern, unit_cost_data)
  }
  size_pattern <- c("size")
  res <- unlist(lapply(size_pattern,
                       IPDFileCheck::get_colno_pattern_colname, colnames(unit_cost_data)))
  if (length(res[which(res != -1)]) < 1) {
    if (brand_check != -1) {
      stop("Error - No size column in the unit cost data")
    } else{
      size_pack_col_no <- -1
    }
  } else {
    size_pack_col_no <-
      get_single_col_multiple_pattern(size_pattern, unit_cost_data)
  }
  if (is.null(eqdose_cov_tab)) {
    conversion_factor <- 1
    eqdose_check <- -1
  } else {
    if (typeof(eqdose_cov_tab) != "closure" &
        typeof(eqdose_cov_tab) != "list") {
      if (is.na(eqdose_cov_tab)) {
        eqdose_check <- -1
        conversion_factor <- 1
      }
    } else {
      eqdose_check <- 0
      name_pattern <- c("name", "drug", "medication")
      drug_col_conv_table <- get_single_col_multiple_pattern(name_pattern,
                                                             eqdose_cov_tab)

      form_pattern <- c("form", "type")
      form_col_conv_table <- get_single_col_multiple_pattern(form_pattern,
                                                             eqdose_cov_tab)

      dose_unit_pattern <- c("unit")
      dose_unit_col_conv_table <- get_single_col_multiple_pattern(dose_unit_pattern,
                                                                  eqdose_cov_tab)

      conv_factor_pattern <- c("conversion", "factor")
      conv_factor_col <- get_single_col_multiple_pattern(conv_factor_pattern,
                                                         eqdose_cov_tab)
    }
  }
  list_total_med_equiv_dose_period <- list()
  list_total_cost_per_equiv_period <- list()
  for (i in 1:nrow(ind_part_data)) {
    name_medication <- names_from_code[i, ]
    if (brand_check != -1)
      brand_medication <- brand_and_code[i, ]
    dose_medication <- med_dose[i, ]
    if (length(dose_medication) != length(name_medication))
      stop("number of doses and number of medications should be equal")
    if (is.null(name_medication)) {
      medication_valid_check <- -1
    } else {
      if (sum(is.na(unname(name_medication))) >= length(name_medication))
        medication_valid_check <- -1
      else
        medication_valid_check <- 0
    }
    if (medication_valid_check != -1) {
      how_many_taken <- as.numeric(ind_part_data[i, numbers_taken_cols])
      freq_multiplier_basis <- freq_given_basis[i, ]
      freq_multiplier_basis <-
        freq_multiplier_basis[!is.na(freq_multiplier_basis)]
      this_unit <- unit_from_code[i, ]
      this_unit <- this_unit[!is.na(this_unit)]
      total_med_equiv_dose_period <-  0
      total_cost_per_equiv_period <-  0
      for (j in seq_len(length(name_medication))) {
        if (!is.null(name_medication[j]) & !is.na(name_medication[j])) {
          subset1 <- return_equal_str_col(name_col_no,
                                          unit_cost_data, name_medication[j])
          indices_form <- which(stringr::str_detect(toupper(
                                      subset1[[form_col_no]]), "PATCH"))
          subset2 <- subset1[indices_form, ]
          if (brand_check != -1) {
            if (is.null(brand_medication[j])) {
              subset2 <- subset2
            } else {
              if (is.na(brand_medication[j])) {
                subset2 <- subset2
              } else {
                if (brand_medication[j] == "" | brand_medication[j]  == " ") {
                  subset2 <- subset2
                } else {
                  subset2 <- return_equal_str_col(brand_col_no,
                                                  subset2, brand_medication[j])
                  if (nrow(subset2) < 1)
                    stop("Did not find matching brand name of medication")
                }
              }
            }
          }
          if (unit_med_check == -1)
            dose_num_val <-
            as.numeric(stringr::str_extract(dose_medication[j],
                                            "\\d+\\.*\\d*"))
          else
            dose_num_val <- as.numeric(dose_medication[j])
          if (eqdose_check != -1) {
            temp <- return_equal_str_col(drug_col_conv_table, eqdose_cov_tab,
                                         name_medication[j])
            words <- c("patch", "patches")
            if (nrow(temp) != 0) {
                tempa <- return_equal_liststring_listcol(form_col_conv_table, temp,
                                                         words)

                unit_conv_table <- tempa[[dose_unit_col_conv_table]]
                unit_converts <-
                  unlist(lapply(unit_conv_table, convert_wtpertimediff_basis,
                                this_unit[j]))

                unit_same <- which(unit_converts == 1)
                temp2 <- tempa[unit_same, ]
                if (nrow(temp2) < 1)
                  stop("The unit in the conversion table is not correct or
                       can not be checked")
                conver_factor <- temp2[[conv_factor_col]]
                if (!is.numeric(conver_factor)) {
                  if (conver_factor == "N/A" | is.na(conver_factor)) {
                    conversion_factor <- 1
                  } else {
                    check_num <- suppressWarnings(as.numeric(conver_factor))
                    if (is.na(check_num)) {
                      conversion_factor <-
                        as.numeric(stringr::str_extract(conver_factor,
                                                        "\\d+\\.*\\d*"))
                     unit_in_conversion_factor <-
                                              gsub("[0-9].", "", conver_factor)
                     if (unit_in_conversion_factor == "mg/day" |
                        unit_in_conversion_factor == "mg/d" |
                        unit_in_conversion_factor == "milligrams/day" |
                        unit_in_conversion_factor == "milligrams/d" |
                        unit_in_conversion_factor == "milli grams/day" |
                          unit_in_conversion_factor == "milli grams/d") {

                     } else {
                        stop("expecting the conversion factor in the units of
                             mg/day")
                     }
                    }else{
                      conversion_factor <- as.numeric(conver_factor)
                    }
                  }
                } else {
                  conversion_factor <- as.numeric(conver_factor)
                }
            } else {
              conversion_factor <- 0
            }
          }
          strength_unit_cost <- trimws(gsub("[0-9\\.]", "",
                                            subset2[[dosage_col_no]]))
          strength_val_cost <-
            as.numeric(stringr::str_extract(subset2[[dosage_col_no]],
                                            "\\d+\\.*\\d*"))

          strength_unit_multiplier <- c()
          basis_str_unit_multiply <- convert_wtpertimediff_basis(this_unit[j],
                                                basis_strength_unit)

          strength_unit_cost[which(strength_unit_multiplier == 1)] <-
            this_unit[j]
          subset3 <- subset2
          unit_costs <- subset3[[unit_cost_column]]
          unit_used_costing <- tolower(unique(subset3[[unit_col_no]]))
          costing_package <- c("per pack", "per package", "pack", "package")
          if (sum(unit_used_costing %in% costing_package) >= 1) {
              pack_size <- as.numeric(unlist(subset3[size_pack_col_no]))
          } else {
              pack_size <- 1
          }
          equivalent_dose <- conversion_factor * strength_val_cost
          MED_eachpack <- pack_size * equivalent_dose
          # average cost per pack per med
          cost_permed <- unit_costs / MED_eachpack
          average_cost_permed <- sum(cost_permed) / length(cost_permed)

          # number of patches used for a day (as day is internal basis)
          # 2 patches taken once a week = 2/7 patches a day
          no_taken_basis <- how_many_taken[j] * freq_multiplier_basis[j]

          # convert internal basis time to period, i.e. a day to given time
          # period, if its 4 weeks, the time_multiplier = 28
          time_multiplier <- convert_to_given_timeperiod(timeperiod,
                                                         internal_basis_time)
          # no taken basis multiplied by the time multiplier will give the
          # number taken during time period = ie 2/7 patches a day* 28 day
          # 56/7 patches in 28 days
          number_taken_period <- no_taken_basis * time_multiplier

          # medication in strength unit after converting to basis strength unit
          # given if not the default mcg/hr
          med_str_period <-  dose_num_val * basis_str_unit_multiply *
                                  number_taken_period

          #convert basis time to the time period i.e in this case hr is the
          #basis time unit, so hr converted to 2 weeks. 28*24 hr/ 28 d
          time_multi <- convert_to_given_timeperiod(timeperiod, basis_time_unit)

          index <- stringr::str_locate(this_unit[j], "/")
          this_wt_unit <- stringr::str_sub(this_unit[j], 1, index[1] - 1)

          # convert the wt to the basis weight unit
          wt_unit_multiplier <- convert_weight_diff_basis(this_wt_unit,
                                                          basis_wt_unit)

          # medication in wt units 2 mcg/hr * 56/7 patches * (1/1000) mg/mcg
          #               * 28*24 hr
          med_wt_period <- dose_num_val * number_taken_period *
                                      wt_unit_multiplier * time_multi
          med_str_equiv_period <- dose_num_val * basis_str_unit_multiply *
            conversion_factor * how_many_taken[j]

          cost_per_equiv_period  <- average_cost_permed * med_str_equiv_period

        } else {
          med_str_equiv_period <- 0
          cost_per_equiv_period <- 0
        }
        total_med_equiv_dose_period <- total_med_equiv_dose_period +
          med_str_equiv_period
        total_cost_per_equiv_period <- total_cost_per_equiv_period +
          cost_per_equiv_period
      }
    } else {
       total_med_equiv_dose_period <- NA
       total_cost_per_equiv_period <- NA
    }
    keywd <- "patches"
    list_total_med_equiv_dose_period <- append(list_total_med_equiv_dose_period,
                                               total_med_equiv_dose_period)
    list_total_cost_per_equiv_period <- append(list_total_cost_per_equiv_period,
                                               total_cost_per_equiv_period)
  }
  this_name <- paste("totalmed_equiv_dose_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_equiv_dose_period)
  this_name <- paste("totcost_per_equiv_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_per_equiv_period)
  return(ind_part_data)
}
#' #'###########################################################################
#' Function to estimate the cost of patches when IPD is in long format
#' using a IPD data of long format
#' @param the_columns columns that are to be used to convert the data
#' from long to wide
#' @param ind_part_data_long IPD
#' @param name_med name of medication
#' @param brand_med brand name of medication if revealed
#' @param dose_med dose of medication used
#' @param unit_med unit of medication ; use null if its along with the dose
#' @param no_taken how many taken
#' @param freq_taken frequency of medication
#' @param timeperiod time period for cost calculation
#' @param unit_cost_data  unit costs data
#' @param unit_cost_column column name of unit cost in unit_cost_data
#' @param cost_calculated_per column name of unit in the cost is calculated
#' @param strength_column column column name that contain strength of
#' medication
#' @param list_of_code_names if names is coded, give the code:name pairs,
#' optional
#' @param list_of_code_freq if frequency is coded, give the
#' code:frequency pairs, optional
#' @param list_of_code_dose_unit if unit is coded, give the code:unit pairs,
#' optional
#' @param list_of_code_brand if brand names  are coded, give the code:brand
#' pairs, optional
#' @param eqdose_cov_tab table to get the conversion factor for equivalent
#' doses, optional
#' @param basis_strength_unit strength unit to be taken as basis
#' required for total medication calculations
#' @return the calculated cost of tablets along with original data
#' @examples
#' med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
#' package = "packDAMipd")
#' data_file <- system.file("extdata", "medication.xlsx",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(data_file)
#' med_costs <- load_trial_data(med_costs_file)
#' conv_file <- system.file("extdata", "Med_calc.xlsx",package = "packDAMipd")
#' table <- load_trial_data(conv_file)
#' names <- colnames(ind_part_data)
#' ending <- length(names)
#' ind_part_data_long <- tidyr::gather(ind_part_data, measurement, value,
#' names[2]:names[ending], factor_key = TRUE)
#' the_columns <- c("measurement", "value")
#' res <- costing_opioid_patches_averageMED_long(the_columns,
#' ind_part_data_long = ind_part_data_long, name_med = "patch_name",
#' brand_med = "patch_brand", dose_med = "patch_strength",unit_med = NULL,
#' no_taken = "patch_no_taken", freq_taken = "patch_frequency",
#' timeperiod = "4 months", unit_cost_data = med_costs,
#' unit_cost_column = "UnitCost", cost_calculated_per  = "Basis",
#' strength_column = "Strength", list_of_code_names = NULL,
#' list_of_code_freq = NULL, list_of_code_dose_unit = NULL,
#' list_of_code_brand = NULL, eqdose_cov_tab = table,
#' basis_strength_unit = "mcg/hr")
#' @export
#' @importFrom tidyr gather
#' @importFrom tidyr spread_
costing_opioid_patches_averageMED_long <- function(the_columns,
                                      ind_part_data_long,
                                      name_med,
                                      brand_med = NULL,
                                      dose_med,
                                      unit_med = NULL,
                                      no_taken, freq_taken,
                                      timeperiod,
                                      unit_cost_data,
                                      unit_cost_column,
                                      cost_calculated_per,
                                      strength_column,
                                      list_of_code_names = NULL,
                                      list_of_code_freq = NULL,
                                      list_of_code_dose_unit = NULL,
                                      list_of_code_brand = NULL,
                                      eqdose_cov_tab = NULL,
                                      basis_strength_unit = NULL) {
  #Error - data should not be NULL
  if (is.null(ind_part_data_long) | is.null(unit_cost_data))
    stop("data should not be NULL")

  ind_part_data_wide <- tidyr::spread_(ind_part_data_long, the_columns[1],
                                       the_columns[2])

  result_wide <- costing_opioid_patches_averageMED_wide(ind_part_data_wide,
                                           name_med,
                                           brand_med,
                                           dose_med,
                                           unit_med,
                                           no_taken, freq_taken,
                                           timeperiod,
                                           unit_cost_data,
                                           unit_cost_column,
                                           cost_calculated_per,
                                           strength_column,
                                           list_of_code_names,
                                           list_of_code_freq,
                                           list_of_code_dose_unit,
                                           list_of_code_brand,
                                           eqdose_cov_tab,
                                           basis_strength_unit)
  result_wide <- as.data.frame(result_wide)
  columns <- colnames(result_wide)
  num <- length(columns)
  result_long <- tidyr::gather(result_wide, key = "measurement", value = "value",
                               columns[2]:columns[num], factor_key = TRUE)
  return(result_long)

}
