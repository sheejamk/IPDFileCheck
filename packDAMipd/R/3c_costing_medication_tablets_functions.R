
##############################################################################
#' Function to estimate the cost of tablets taken (from IPD)
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
#' data_file <- system.file("extdata", "medication_all.xlsx",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(data_file)
#' med_costs <- load_trial_data(med_costs_file)
#' conv_file <- system.file("extdata", "Med_calc.xlsx",package = "packDAMipd")
#' table <- load_trial_data(conv_file)
#' res <- microcosting_tablets_wide(ind_part_data = ind_part_data,
#' name_med = "tab_name", brand_med = "tab_brand", dose_med = "tab_strength",
#' unit_med = "tab_str_unit", no_taken = "tab_no_taken",
#' freq_taken = "tab_frequency",timeperiod = "2 months",
#' unit_cost_data = med_costs,unit_cost_column = "UnitCost",
#' cost_calculated_per  = "Basis", strength_column = "Strength",
#' list_of_code_names = NULL, list_of_code_freq = NULL,
#' list_of_code_dose_unit = NULL, eqdose_cov_tab = table,
#' basis_strength_unit = "mg")
#' @export
#' @details
#' Assumes individual level data has name of medication, dose, dose unit,
#' number taken, frequency taken, and basis time
#' Assumes unit cost data contains the name of medication, form/type,
#' strength, unit of strength (or the unit in which the cost calculated),
#' preparation, unit cost, size and size unit
#' (in which name, forms, size, size unit, and preparation  are not passed on)
#'  @importFrom dplyr %>%
microcosting_tablets_wide <- function(ind_part_data,
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
  wt_units <- generated_list$weight_units
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
      basis_strength_unit <- "mg"
  } else {
    basis_strength_unit <- "mg"
  }
  if (!(basis_strength_unit %in% wt_units))
    stop("the basis strength unit is not valid")

  basis_wt_unit <- basis_strength_unit

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
  name_pattern <- c("name", "drug", "medication", "med", "patch")
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
  list_total_med_str_period <- list()
  list_total_med_equiv_dose_period <- list()
  list_total_cost_period <- list()
  list_total_cost_per_equiv_period <- list()
  for (i in 1:nrow(ind_part_data)) {
    name_medication <- names_from_code[i, ]
    if (brand_check != -1)
      brand_medication <- brand_and_code[i, ]
    dose_medication <- med_dose[i, ]
    if (is.null(name_medication)) {
      medication_valid_check <- -1
    } else {
      if (sum(is.na(unname(name_medication))) == length(name_medication))
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
      total_med_str_period <- 0
      total_med_equiv_dose_period <-  0
      total_cost_period <-  0
      total_cost_per_equiv_period <-  0
      for (j in seq_len(length(name_medication))) {
        if (!is.null(name_medication[j]) & !is.na(name_medication[j])) {
          index <- gregexpr(pattern = "/", name_medication[j])[[1]][1]
          index_bracket <- gregexpr(pattern = "\\(", name_medication[j])[[1]][1]
          strength_term <- NULL
          this_med_name <- name_medication[j]
          if (index != -1) {
              index_lastspace <- gregexpr(pattern = " ", name_medication[j])[[1]][1]
              if (index_lastspace != -1) {
                   strength_term <- substr(name_medication[j], index_lastspace + 1,
                                    nchar(name_medication[j]))
                   this_med_name <- trimws(substr(name_medication[j], 1, index_lastspace))

              }
          } else {
            index_bracket <- gregexpr(pattern = "\\(", name_medication[j])[[1]][1]
            if (index_bracket != -1) {
              index_lastbracket <- gregexpr(pattern = "\\)", name_medication[j])[[1]][1]
              strength_term <- substr(name_medication[j], index_bracket + 1,
                                      index_lastbracket - 1)
              this_med_name <- trimws(substr(name_medication[j], 1, index_bracket - 1))
            }
          }
          subset1 <- return_equal_str_col(name_col_no,
                                          unit_cost_data, this_med_name)
          indices_form1 <- which(stringr::str_detect(toupper(subset1[[form_col_no]]), "TABLET"))
          indices_form2 <- which(stringr::str_detect(toupper(subset1[[form_col_no]]), "CAPSULE"))
          indices_form3 <- which(stringr::str_detect(toupper(subset1[[form_col_no]]), "SACHET"))

          indices_form <- unique(c(indices_form1, indices_form2, indices_form3))
          subset2 <- subset1[indices_form, ]
          if (brand_check != -1) {
              if (is.null(brand_medication[j])) {
                subset2 <- subset2
              } else {
                if (is.na(brand_medication[j])) {
                  subset2 <- subset2
                } else {
                  if (brand_medication[j] == "" | brand_medication[j]  == " "
                      | brand_medication[j]  == "NA") {
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
          actual_dose_from_name <- NULL
          # name of the medication has 8mg/500mg information
          if (!is.null(strength_term)) {
            if (index != -1) {
              indices1 <- unlist(stringr::str_locate_all(strength_term, "/"))
              first_dose <- substr(strength_term, 1, indices1[1] - 1)
              sec_dose <- substr(strength_term, indices1[1] + 1, nchar(strength_term))
              first_dose_srength  <- gsub("[[:digit:]]+", "", first_dose)
              sec_dose_srength  <- gsub("[[:digit:]]+", "", sec_dose)
              first_dose_num <- gsub("[^0-9.-]", "", first_dose)
              sec_dose_num <- gsub("[^0-9.-]", "", sec_dose)
              dose_num_val_name <- paste(first_dose_num, "/",
                                         sec_dose_num, sep = "")
              strength_val_name <- paste(first_dose_srength, "/",
                                         sec_dose_srength, sep = "")

              actual_dose_from_name <- paste(first_dose_num, first_dose_srength, "/",
                                             sec_dose_num, sec_dose_srength, sep = "")
            } else {
              if (index_bracket != -1) {
                dose_num_val_name <- NA
                strength_val_name <- NA
              }
            }

          }
          #if the dosage is given with two weights units
          # if dose value is given as 8/500 or 8mg/500mg
          index_slash <- gregexpr(pattern = "/", dose_medication[j])[[1]][1]
          index_slash_unit <- gregexpr(pattern = "/",  this_unit[j])[[1]][1]
          #now will check if the units are given separate or along with dosage

          # unit is not given separate, but dose with no "/" eg. 2mg
          # so need to extract the numerical value
          if (unit_med_check == -1 & index_slash == -1) {
            if (!is.null(actual_dose_from_name)) {
              dose_num_val <- dose_num_val_name
              if (index_slash_unit == -1) {
                strength_val <- paste(this_unit[j], "/", this_unit[j], sep = "")
              } else {
                strength_val <- strength_val_name
              }
            } else {
              dose_num_val <-
                as.numeric(stringr::str_extract(dose_medication[j],
                                                "\\d+\\.*\\d*"))
              strength_val <- this_unit[j]
            }
          }

          # unit is not given separate, but dose with "/" e.g 2mg/500mg
          if (unit_med_check == -1 & index_slash != -1) {
             indices1 <- unlist(stringr::str_locate_all(dose_medication[j], "/"))
             first_dose <- substr(dose_medication[j], 1, indices1[1] - 1)
             sec_dose <- substr(dose_medication[j], indices1[1] + 1,
                                nchar(dose_medication[j]))
             first_dose_srength  <- gsub("[[:digit:]]+", "", first_dose)
             sec_dose_srength  <- gsub("[[:digit:]]+", "", sec_dose)
             first_dose_num <- gsub("[^0-9.-]", "", first_dose)
             sec_dose_num <- gsub("[^0-9.-]", "", sec_dose)
             dose_num_val <- paste(first_dose_num, "/",
                                   sec_dose_num, sep = "")
             strength_val <- paste(first_dose_srength, "/",
                                  sec_dose_srength, sep = "")

          }
          # unit is given separate, but dose with no "/" e.g 2
          if (unit_med_check != -1 & index_slash == -1) {
            if (is.null(actual_dose_from_name)) {
              upper_comb_meds <- c("CO-CODAMOL", "CO-DYDRAMOL")
              if (toupper(this_med_name) %in% upper_comb_meds)
                stop("Error - dose should be revealed from name or dose")
              dose_num_val <- as.numeric(dose_medication[j])
              strength_val <- this_unit[j]
            } else {
              dose_num_val <- dose_num_val_name
              if (index_slash_unit == -1) {
                strength_val <- paste(this_unit[j], "/", this_unit[j], sep = "")
              }
            }
          }
          # unit is given separate, but dose with "/" e.g 2/50
          if (unit_med_check != -1 & index_slash != -1) {
            dose_num_val <- (dose_medication[j])
            if (index_slash_unit != -1) {
              strength_val <- this_unit[j]
            } else {
              if (!is.null(actual_dose_from_name)) {
                strength_val <- strength_val_name
              } else {
                strength_val <- NULL
                stop("unit of medication had to be wt/wt")
              }
            }
          }
          if (index_slash != -1) {
            find1 <- unlist(stringr::str_locate_all(dose_num_val, "/"))
            first_dos <- substr(dose_num_val, 1, find1[1] - 1)
            sec_dos <- substr(dose_num_val, find1[1] + 1,
                            nchar(dose_num_val))
            find2 <- unlist(stringr::str_locate_all(strength_val, "/"))
            first_stre <- substr(strength_val, 1, find2[1] - 1)
            sec_stre <- substr(strength_val, find2[1] + 1,
                             nchar(strength_val))
            actual_dose_ipd <- paste(first_dos, first_stre, "/", sec_dos,
                               sec_stre, sep = "")
          } else {
            if (index_bracket != -1)
              actual_dose_ipd <- NA
            else
              actual_dose_ipd <- paste(dose_num_val, strength_val, sep = "")
          }
          if (index_slash == -1 &
              !is.null(actual_dose_from_name) & index_slash_unit == -1) {
            find1 <- unlist(stringr::str_locate_all(dose_num_val, "/"))
            first_dos <- substr(dose_num_val, 1, find1[1] - 1)
            sec_dos <- substr(dose_num_val, find1[1] + 1,
                              nchar(dose_num_val))
            find2 <- unlist(stringr::str_locate_all(strength_val, "/"))
            first_stre <- substr(strength_val, 1, find2[1] - 1)
            sec_stre <- substr(strength_val, find2[1] + 1,
                               nchar(strength_val))
            actual_dose_ipd <- paste(first_dos, first_stre, "/", sec_dos,
                                     sec_stre, sep = "")
          }

          if (eqdose_check != -1) {
            temp <- return_equal_str_col(drug_col_conv_table, eqdose_cov_tab,
                                         this_med_name)
            if (nrow(temp) != 0) {
              words <- c("tablet", "tablets")
              tempa <- return_equal_liststring_listcol(form_col_conv_table, temp,
                                                       words)
              unit_conv_table <- tempa[[dose_unit_col_conv_table]]
              unit_conv_table <- stringr::str_replace_all(unit_conv_table,
                                                  stringr::fixed(" "), "")

              if (index_slash == -1 & !is.na(actual_dose_ipd)) {
                if (is.null(actual_dose_from_name) & index_slash_unit == -1) {
                  unit_converts <-
                    unlist(lapply(unit_conv_table, convert_weight_diff_basis,
                                  basis_strength_unit))

                } else {
                  unit_converts <- (unit_conv_table == actual_dose_ipd)
                  unit_converts[(unit_converts)] <- 1
                }
              } else {
                if (is.na(actual_dose_ipd)) {
                  unit_converts <- unit_conv_table == "N/A"
                  unit_converts[(unit_converts)] <- 1
                } else {
                  unit_converts <- (unit_conv_table == actual_dose_ipd)
                  unit_converts[(unit_converts)] <- 1
                }
              }
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
                  if (is.na(check_num))
                    conversion_factor <-
                      as.numeric(stringr::str_extract(conver_factor,
                                                      "\\d+\\.*\\d*"))
                  else
                    conversion_factor <- as.numeric(conver_factor)
                }
              } else {
                conversion_factor <- as.numeric(conver_factor)
              }
            } else {
              conversion_factor <- 0
            }
          }
          if (index_slash == -1 & !is.na(actual_dose_ipd)) {
            if (is.null(actual_dose_from_name) & index_slash_unit == -1) {
                strength_unit_cost <- trimws(gsub("[0-9\\.]", "",
                                                  subset2[[dosage_col_no]]))
                strength_val_cost <-
                  as.numeric(stringr::str_extract(subset2[[dosage_col_no]],
                                                  "\\d+\\.*\\d*"))

                strength_unit_multiplier <- c()
                basis_str_unit_multiply <- convert_weight_diff_basis(this_unit[j],
                                                      basis_strength_unit)
                for (ii in seq_len(length(strength_unit_cost))) {
                  unit_multiply <- convert_weight_diff_basis(this_unit[j],
                                                             strength_unit_cost[ii])
                  strength_unit_multiplier <- append(strength_unit_multiplier,
                                                     unit_multiply)
                }
                if (sum(is.na(strength_unit_multiplier)) != 0)
                  stop("The unit is not identifiable to convert for costing")
                strength_unit_cost[which(strength_unit_multiplier == 1)] <-
                  this_unit[j]
                dose_in_cost_data <- paste(strength_val_cost,
                                           strength_unit_cost, sep = "")
            } else {
              basis_str_unit_multiply <- 1
              dose_in_cost_data <- subset2[[dosage_col_no]]
              strength_unit_multiplier <- rep(1, length(subset2[[dosage_col_no]]))
            }
          } else {
            basis_str_unit_multiply <- 1
            dose_in_cost_data <- subset2[[dosage_col_no]]
            strength_unit_multiplier <- rep(1, length(subset2[[dosage_col_no]]))
          }
          if (is.na(actual_dose_ipd)) {
            subset3 <- subset2
            unit_cost_med_prep <- sum(subset3[[unit_cost_column]] /
                                        nrow(subset3))
            dosage_unit_cost <- subset3[[dosage_col_no]]
            num_valu_dose <- c()
            for (m in 1:length(dosage_unit_cost)) {
              inds1 <- unlist(stringr::str_locate_all(dosage_unit_cost[m], "/"))
              first_dose <- substr(dosage_unit_cost[m], 1, inds1[1] - 1)
              first_dose_num <- gsub("[^0-9.-]", "", first_dose)
              num_valu_dose <- append(num_valu_dose, first_dose_num)
            }
            strength_unit_multip <- 1
            dose_num_val <- sum(as.numeric(num_valu_dose)) / length(dosage_unit_cost)
          } else {
             if (any(dose_in_cost_data == actual_dose_ipd)) {
                subset3 <- subset2[dose_in_cost_data ==  actual_dose_ipd, ]
                unit_cost_med_prep <- sum(subset3[[unit_cost_column]] /
                                        nrow(subset3))
                strength_unit_multip <- strength_unit_multiplier[dose_in_cost_data ==
                                                                   actual_dose_ipd]

            } else {
               stop("The used dosage is not in costing table")
            }
          }
          unit_used_costing <- tolower(unique(subset3[[unit_col_no]]))
          costing_package <- c("per pack", "per package", "pack", "package")
          if (sum(unit_used_costing %in% costing_package) >= 1) {
              pack_size <- sum(as.numeric(subset3[size_pack_col_no])) /
                                          nrow(subset3)
          } else {
              pack_size <- 1
          }
          # number of tablets taken for the base unit of time internally
          #it is a day
          no_taken_basis <- how_many_taken[j] * freq_multiplier_basis[j]

          time_multiplier <- convert_to_given_timeperiod(timeperiod,
                                                         internal_basis_time)
          number_taken_period <- no_taken_basis * time_multiplier
          if (sum(unit_used_costing %in% costing_package) >= 1) {
            pack_size <- as.numeric(subset3[size_pack_col_no])
            packs_taken_period <- ceiling(number_taken_period / pack_size)
          } else {
            pack_size <- 1
            packs_taken_period <- number_taken_period
          }
          if (index_slash == -1) {
            if (is.null(actual_dose_from_name) & index_slash_unit == -1)
                med_str_period <-  dose_num_val * number_taken_period *
                      basis_str_unit_multiply
            else
              med_str_period <-  as.numeric(first_dos) * number_taken_period *
                basis_str_unit_multiply
          } else {
            med_str_period <-  as.numeric(first_dos) * number_taken_period *
              basis_str_unit_multiply
          }
          cost_period <- packs_taken_period * unit_cost_med_prep
          med_str_equiv_period <- med_str_period * conversion_factor
          cost_per_equiv_period  <- cost_period / med_str_equiv_period

        } else {
          med_str_period <- 0
          cost_period <- 0
          med_str_equiv_period <- 0
          cost_per_equiv_period <- 0

        }
        total_med_str_period <- total_med_str_period + med_str_period
        total_med_equiv_dose_period <- total_med_equiv_dose_period +
          med_str_equiv_period
        total_cost_period <- total_cost_period + cost_period
        total_cost_per_equiv_period <- total_cost_per_equiv_period +
          cost_per_equiv_period
      }
    } else {
      total_med_str_period <- 0
      total_med_equiv_dose_period <- NA
      total_cost_period <- NA
      total_cost_per_equiv_period <- NA
    }
    keywd <- "tablets"
    list_total_med_str_period <- append(list_total_med_str_period,
                                        total_med_str_period)
    list_total_med_equiv_dose_period <- append(list_total_med_equiv_dose_period,
                                               total_med_equiv_dose_period)
    list_total_cost_period <- append(list_total_cost_period,
                                     total_cost_period)
    list_total_cost_per_equiv_period <- append(list_total_cost_per_equiv_period,
                                               total_cost_per_equiv_period)
  }

  this_name <- paste("totmed_period_", keywd, "_", basis_strength_unit, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_str_period)

  this_name <- paste("totmed_equiv_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_equiv_dose_period)

  this_name <- paste("totcost_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_period)

  this_name <- paste("totcost_per_equiv_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_per_equiv_period)
  return(ind_part_data)
}

##############################################################################
#' Function to estimate the cost of tablets when IPD is in long format
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
#' doses, optional
#' @param basis_strength_unit strength unit to be taken as basis
#' required for total medication calculations
#' @return the calculated cost of tablets along with original data
#' @examples
#' med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
#' package = "packDAMipd")
#' data_file <- system.file("extdata", "medication_all.xlsx",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(data_file)
#' med_costs <- load_trial_data(med_costs_file)
#' conv_file <- system.file("extdata", "Med_calc.xlsx", package = "packDAMipd")
#' table <- load_trial_data(conv_file)
#' names <- colnames(ind_part_data)
#' ending <- length(names)
#' ind_part_data_long <- tidyr::gather(ind_part_data, measurement, value,
#' names[2]:names[ending], factor_key = TRUE)
#' the_columns <- c("measurement", "value")
#' res <- microcosting_tablets_long(the_columns,
#' ind_part_data_long = ind_part_data_long, name_med = "tab_name",
#' brand_med = "tab_brand", dose_med = "tab_strength",
#' unit_med = "tab_str_unit",
#' no_taken = "tab_no_taken", freq_taken = "tab_frequency",
#' timeperiod = "2 months",unit_cost_data = med_costs,
#' unit_cost_column = "UnitCost", cost_calculated_per  = "Basis",
#' strength_column = "Strength", list_of_code_names = NULL,
#' list_of_code_freq = NULL,list_of_code_dose_unit = NULL,
#' eqdose_cov_tab = table, basis_strength_unit = "mg")
#' @export
#' @importFrom tidyr gather
#' @importFrom tidyr spread_
microcosting_tablets_long <- function(the_columns,
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

  results_wide <- microcosting_tablets_wide(ind_part_data_wide,
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
  results_wide <- as.data.frame(results_wide)
  columns <- colnames(results_wide)
  num <- length(columns)
  result_long <- tidyr::gather(results_wide, key = "measurement", value =  "value",
                               columns[2]:columns[num], factor_key = TRUE)
  return(result_long)
}
