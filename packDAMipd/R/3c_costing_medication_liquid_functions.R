
##############################################################################
#' Function to estimate the cost of liquids taken (from IPD)
#' @param ind_part_data IPD
#' @param name_med name of medication
#' @param brand_med brand name of medication if revealed
#' @param dose_med dose of medication used
#' @param unit_med unit of medication ; use null if its along with the dose
#' @param bottle_size size of the bottle used
#' @param bottle_size_unit unit of bottle volume
#' @param bottle_lasts how long the bottle lasted
#' @param bottle_lasts_unit time unit of how long the bottle lasted
#' @param preparation_dose dose if preparation is given
#' @param preparation_unit unit of preparatio dose
#' @param timeperiod time period for cost calculation
#' @param unit_cost_data  unit costs data
#' @param unit_cost_column column name of unit cost in unit_cost_data
#' @param cost_calculated_per column name of unit where the cost is calculated
#' @param strength_column column column name that has strength of medication
#' @param list_of_code_names if names is coded, give the code:name pairs,
#' optional
#' @param list_of_code_brand if brand names  are coded, give the
#' code:brand pairs, optional
#' @param list_of_code_dose_unit if unit is coded, give the code:unit pairs,
#' optional
#' @param list_of_code_bottle_size_unit  list of bottle size units and codes
#' @param list_of_code_bottle_lasts_unit list of time of bottle lasts and codes
#' @param list_preparation_dose_unit list of preparation dose units and codes
#' @param eqdose_covtab table to get the conversion factor for equivalent
#' doses, optional, but the column names have to unique
#' Similar to c("Drug",	"form", "unit",	"factor") or
#' c("Drug",	"form", "unit",	"conversion")
#' @param basis_strength_unit strength unit to be taken as basis
#' required for total medication calculations
#' @return the calculated cost of tablets along with original data
#' @examples
#' med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
#' package = "packDAMipd")
#' data_file <- system.file("extdata", "medication_liq.xlsx",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(data_file)
#' med_costs <- load_trial_data(med_costs_file)
#' conv_file <- system.file("extdata", "Med_calc.xlsx",package = "packDAMipd")
#' table <- load_trial_data(conv_file)
#' res <- microcosting_liquids_wide(
#' ind_part_data = ind_part_data, name_med = "liq_name", brand_med =  NULL,
#' dose_med = "liq_strength", unit_med = NULL, bottle_size = "liq_bottle_size",
#' bottle_size_unit = NULL, bottle_lasts = "liq_lasts",
#' bottle_lasts_unit = NULL, preparation_dose = NULL, preparation_unit = NULL,
#' timeperiod = "4 months", unit_cost_data = med_costs,
#' unit_cost_column = "UnitCost", cost_calculated_per = "Basis",
#' strength_column = "Strength", list_of_code_names = NULL,
#' list_of_code_brand = NULL, list_of_code_dose_unit = NULL,
#' list_of_code_bottle_size_unit = NULL, list_of_code_bottle_lasts_unit = NULL,
#' list_preparation_dose_unit = NULL, eqdose_covtab = table,
#' basis_strength_unit = NULL)
#' @export
#' @importFrom dplyr %>%
microcosting_liquids_wide <- function(ind_part_data,
                                      name_med,
                                      brand_med = NULL,
                                      dose_med,
                                      unit_med = NULL,
                                      bottle_size,
                                      bottle_size_unit = NULL,
                                      bottle_lasts,
                                      bottle_lasts_unit = NULL,
                                      preparation_dose = NULL,
                                      preparation_unit = NULL,
                                      timeperiod,
                                      unit_cost_data,
                                      unit_cost_column,
                                      cost_calculated_per,
                                      strength_column,
                                      list_of_code_names = NULL,
                                      list_of_code_brand = NULL,
                                      list_of_code_dose_unit = NULL,
                                      list_of_code_bottle_size_unit = NULL,
                                      list_of_code_bottle_lasts_unit = NULL,
                                      list_preparation_dose_unit = NULL,
                                      eqdose_covtab = NULL,
                                      basis_strength_unit = NULL) {
  internal_basis_time <- "day"
  # check the form as liquids
  words <- c("liquid", "liq", "solution", "liquids", "solutions")
  generated_list <- generate_wt_vol_units()
  wt_per_vol_units <- generated_list$weight_per_vol
  #Error - data should not be NULL
  if (is.null(ind_part_data) | is.null(unit_cost_data))
    stop("data should not be NULL")

  #Checking if the required parameters are NULL or NA
  variables_check <- list(name_med, dose_med,
                          bottle_size, bottle_lasts,
                          timeperiod, unit_cost_column,
                          cost_calculated_per, strength_column)
  results <- sapply(variables_check, check_null_na)
  names_check <- c("name_med", "dose_med", "bottle_size", "bottle_lasts",
                   "timeperiod", "unit_cost_column",
                   "cost_calculated_per", "strength_column")
  if (any(results != 0)) {
    indices <- which(results < 0)
    stop(paste("Error - the variables can not be NULL or NA,
               check the variable(s)", names_check[indices]))
  }
  # if null,keep proper strength unit and time unit
  if (!is.null(basis_strength_unit)) {
    if (is.na(basis_strength_unit)) {
      basis_strength_unit <- "mg/ml"
      basis_wt_unit <- "mg"
      basis_vol_unit <- "ml"
    } else {
      if (!(basis_strength_unit %in% wt_per_vol_units))
        stop("Basis strength unit is not valid")
      index <- stringr::str_locate(basis_strength_unit, "/")
      basis_wt_unit <- stringr::str_sub(basis_strength_unit, 1, index[1] - 1)
      basis_vol_unit <- stringr::str_sub(basis_strength_unit, index[2] + 1,
                                         nchar(basis_strength_unit))
    }
  } else {
    basis_strength_unit <- "mg/ml"
    basis_wt_unit <- "mg"
    basis_vol_unit <- "ml"
  }

  ## Check the columns in IPD and get the columns
  brand_check <- return0_if_not_null_na(brand_med)
  unit_med_check <-  return0_if_not_null_na(unit_med)
  bottle_size_unit_check <- return0_if_not_null_na(bottle_size_unit)
  bottle_lasts_unit_check <- return0_if_not_null_na(bottle_lasts_unit)
  preparation_dose_check <- return0_if_not_null_na(preparation_dose)
  preparation_unit_check <- return0_if_not_null_na(preparation_unit)

  check_list <- c(unit_med_check, brand_check, bottle_size_unit_check,
                  bottle_lasts_unit_check, preparation_dose_check,
                  preparation_unit_check)
  partial_list <- c(name_med, dose_med, bottle_size, bottle_lasts)
  another_list <- list(unit_med, brand_med, bottle_size_unit, bottle_lasts_unit,
                       preparation_dose, preparation_unit)
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
  names_med_ipd_cols <- unlist(ipd_cols_exists[1])
  doses_med_ipd_cols <- unlist(ipd_cols_exists[2])
  bottle_size_ipd_cols <- unlist(ipd_cols_exists[3])
  bottle_lasts_ipd_cols <- unlist(ipd_cols_exists[4])
  if (unit_med_check != -1)
    unit_med_ipd_cols  <- unlist(ipd_cols_exists[5])
  if (brand_check != -1)
    brand_med_ipd_cols  <- unlist(ipd_cols_exists[6])
  if (bottle_size_unit_check != -1)
    bottle_size_unit_ipd_cols  <- unlist(ipd_cols_exists[7])
  if (bottle_lasts_unit_check != -1)
    bottle_lasts_unit_ipd_cols  <- unlist(ipd_cols_exists[8])
  if (preparation_dose_check != -1)
    preparation_dose_ipd_cols  <- unlist(ipd_cols_exists[9])
  if (preparation_unit_check != -1)
    preparation_unit_ipd_cols  <- unlist(ipd_cols_exists[10])

  # check columns exist in unit cost  data
  info_list <- c(unit_cost_column, cost_calculated_per, strength_column)
  checks <- sapply(info_list, IPDFileCheck::check_column_exists, unit_cost_data)
  if (sum(checks) != 0) {
    stop("Atleast one of the required columns in unit cost data not found")
  }

  ## if the information is coded
  names_from_ipd_code <- encode_codes_data(list_of_code_names,
                                           names_med_ipd_cols, ind_part_data)
  if (is.null(unlist(names_from_ipd_code)) |
      sum(is.na(unlist(names_from_ipd_code))) ==
      length(unlist(names_from_ipd_code))) {
    stop("Error - name_from_code can not be null - check the input for
         list of names and codes")
  }

  if (unit_med_check == -1) {
    med_ipd_dose <- ind_part_data %>%
      dplyr::select(dplyr::all_of(doses_med_ipd_cols))
    med_dose_unlist <- unlist(med_ipd_dose)
    unit_from_ipd_code <- gsub("[0-9\\.]", "", med_dose_unlist)
    unit_from_ipd_code <- matrix(unit_from_ipd_code,
                                 nrow = dim(med_ipd_dose)[1])
    colnames(unit_from_ipd_code) <- colnames(med_ipd_dose)
    unit_from_ipd_code <- as.data.frame(unit_from_ipd_code)
  } else {
    med_ipd_dose <- ind_part_data %>%
      dplyr::select(dplyr::all_of(doses_med_ipd_cols))
    med_ipd_dose <- as.data.frame(med_ipd_dose)
    unit_from_ipd_code <- encode_codes_data(list_of_code_dose_unit,
                                            unit_med_ipd_cols, ind_part_data)
    if (is.null(unlist(unit_from_ipd_code)) |
        sum(is.na(unlist(unit_from_ipd_code))) ==
        length(unlist(unit_from_ipd_code))) {
      stop("Error - unit_from_code can not be null - check the input for
         list of units")
    }
  }
  if (bottle_lasts_unit_check == -1) {
    bottle_lasts_ipd <- ind_part_data %>%
      dplyr::select(dplyr::all_of(bottle_lasts_ipd_cols))
    bottle_lasts_unit_unlist <- unlist(bottle_lasts_ipd)
    unit_from_lasts <- gsub("[0-9\\.]", "", bottle_lasts_unit_unlist)
    unit_from_lasts <- matrix(unit_from_lasts,
                              nrow = dim(bottle_lasts_ipd)[1])
    colnames(unit_from_lasts) <- colnames(bottle_lasts_ipd)
    bottle_lasts_unit_from_ipd_code <- as.data.frame(unit_from_lasts)
  } else {
    bottle_lasts_ipd <- ind_part_data %>%
      dplyr::select(dplyr::all_of(bottle_lasts_ipd_cols))
    bottle_lasts_unit_from_ipd_code <-
      encode_codes_data(list_of_code_bottle_lasts_unit,
                        bottle_lasts_unit_ipd_cols, ind_part_data)
    if (is.null(unlist(bottle_lasts_unit_from_ipd_code)) |
        sum(is.na(unlist(bottle_lasts_unit_from_ipd_code))) ==
        length(unlist(bottle_lasts_unit_from_ipd_code))) {
      stop("Error - bottle_lasts_unit_from_ipd_code can not be null -
      check the input for bottle lasts unit code")
    }
  }
  if (bottle_size_unit_check == -1) {
    bottle_size_ipd <- ind_part_data %>%
      dplyr::select(dplyr::all_of(bottle_size_ipd_cols))
    bottle_size_unlist <- unlist(bottle_size_ipd)
    unit_from_size <- gsub("[0-9]+", "", bottle_size_unlist)
    unit_from_size <- matrix(unit_from_size,
                             nrow = dim(bottle_size_ipd)[1])
    colnames(unit_from_size) <- colnames(bottle_size_ipd)
    bottle_size_unit_from_ipd_code <- as.data.frame(unit_from_size)
  } else {
    bottle_size_ipd <- ind_part_data %>%
      dplyr::select(dplyr::all_of(bottle_size_ipd_cols))
    bottle_size_unit_from_ipd_code <-
      encode_codes_data(list_of_code_bottle_size_unit,
                        bottle_size_unit_ipd_cols,
                        ind_part_data)
    if (is.null(unlist(bottle_size_unit_from_ipd_code)) |
        sum(is.na(unlist(bottle_size_unit_from_ipd_code))) ==
        length(unlist(bottle_size_unit_from_ipd_code))) {
      stop("Error - bottle_size_unit_from_ipd_code can not be null - check the input for
         bottel size unit code")
    }
  }
  if (brand_check != -1) {
    brand_from_ipd_code <- encode_codes_data(list_of_code_brand,
                                             brand_med_ipd_cols, ind_part_data)
    if (is.null(unlist(brand_from_ipd_code)) |
        sum(is.na(unlist(brand_from_ipd_code))) ==
        length(unlist(brand_from_ipd_code))) {
      stop("Error - brand_from_ipd_code can not be null - check the input for
         brand  code")
    }
  }

  # dose is specified as 2mg/ml unit is not separate, no coding required
  if (preparation_dose_check != -1 & preparation_unit_check == -1) {
    preparation_ipd_dose <- ind_part_data %>%
      dplyr::select(dplyr::all_of(preparation_dose_ipd_cols))
    preparation_dose_unlist <- unlist(preparation_ipd_dose)
    prepare_unit_from_ipd_code <- gsub("[0-9\\.]", "", preparation_dose_unlist)
    prepare_unit_from_ipd_code <- matrix(prepare_unit_from_ipd_code,
                                         nrow = dim(preparation_ipd_dose)[1])
    colnames(prepare_unit_from_ipd_code) <- colnames(preparation_ipd_dose)
    prepare_unit_from_ipd_code <- as.data.frame(prepare_unit_from_ipd_code)
  }
  if (preparation_dose_check != -1 & preparation_unit_check != -1) {
    preparation_ipd_dose <- ind_part_data %>%
      dplyr::select(dplyr::all_of(preparation_dose_ipd_cols))
    preparation_ipd_dose <- as.data.frame(preparation_ipd_dose)
    prepare_unit_from_ipd_code <- encode_codes_data(list_preparation_dose_unit,
                                                    preparation_unit_ipd_cols, ind_part_data)
  }

  # get column names for name, form, dosage and unit from unit cost data
  name_pattern <- c("name", "drug", "med", "patch", "tablet", "liquid", "injection")
  form_pattern <- c("form", "patch/tablet")
  size_pattern <- c("size")
  size_unit_pattern <- c("type")
  vol_pattern <- c("volume")
  vol_unit_pattern <- c("measured")
  brand_pattern <- c("brand", "trade")
  preparation_pattern <- c("preparation", "prepare", "make")

  name_cost_col_no <- get_single_col_multiple_pattern(name_pattern, unit_cost_data)
  form_cost_col_no <- get_single_col_multiple_pattern(form_pattern, unit_cost_data)
  size_pack_cost_col_no <-
    get_single_col_multiple_pattern(size_pattern, unit_cost_data)
  size_unit_cost_col_no <-
    get_single_col_multiple_pattern(size_unit_pattern, unit_cost_data)
  vol_cost_col_no <-
    get_single_col_multiple_pattern(vol_pattern, unit_cost_data)
  vol_unit_cost_col_no <-
    get_single_col_multiple_pattern(vol_unit_pattern, unit_cost_data)

  unit_cost_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                          cost_calculated_per)
  dosage_cost_col_no <- IPDFileCheck::get_columnno_fornames(unit_cost_data,
                                                            strength_column)

  brand_pattern <- c("brand", "trade")
  res <- unlist(lapply(brand_pattern,
                       IPDFileCheck::get_colno_pattern_colname, colnames(unit_cost_data)))
  if (length(res[which(res != -1)]) < 1) {
    if (brand_check != -1) {
      stop("Error - No brand column in the unit cost data")
    } else{
      brand_cost_col_no <- -1
    }
  } else {
    brand_cost_col_no <-
      get_single_col_multiple_pattern(brand_pattern, unit_cost_data)
  }
  if (preparation_dose_check != -1) {
    preparation_cost_col_no <-  get_single_col_multiple_pattern(preparation_pattern,
                                                                unit_cost_data)
  }

  ## information from equivalent dose tables
  if (is.null(eqdose_covtab)) {
    conversion_factor <- 1
    eqdose_check <- -1
  } else {
    if (typeof(eqdose_covtab) != "closure" & typeof(eqdose_covtab) != "list") {
      if (is.na(eqdose_covtab)) {
        eqdose_check <- -1
        conversion_factor <- 1
      }
    } else {
      eqdose_check <- 0
      name_pattern <- c("name", "drug", "medication")
      form_pattern <- c("form")
      dose_unit_pattern <- c("unit")
      conv_factor_pattern <- c("conversion", "factor")

      drug_col_conv_table <- get_single_col_multiple_pattern(name_pattern,
                                                             eqdose_covtab)

      form_col_conv_table <- get_single_col_multiple_pattern(form_pattern,
                                                             eqdose_covtab)
      dose_unit_col_conv_table <- get_single_col_multiple_pattern(dose_unit_pattern,
                                                                  eqdose_covtab)
      conv_factor_col_conv_table <-
        get_single_col_multiple_pattern(conv_factor_pattern, eqdose_covtab)

    }
  }

  list_total_med_wt_period <- list()
  list_total_cost_period <- list()
  list_total_med_equiv_dose_period <- list()
  list_total_cost_per_equiv_period <- list()
  for (i in 1:nrow(ind_part_data)) {
    name_ipd <- names_from_ipd_code[i, ]
    dose_ipd <- med_ipd_dose[i, ]
    unit_dose_ipd <- unit_from_ipd_code[i, ]

    bot_lasts_ipd <- bottle_lasts_ipd[i, ]
    bot_lasts_unit_ipd <- bottle_lasts_unit_from_ipd_code[i, ]
    bot_size_ipd <- bottle_size_ipd[i, ]
    bot_size_unit_ipd <- trimws(bottle_size_unit_from_ipd_code[i, ])

    if (brand_check != -1)
      brand_ipd <- brand_from_ipd_code[i, ]
    if (preparation_dose_check != -1)
      prepare_dose_ipd <- preparation_ipd_dose[i, ]
    if (preparation_unit_check != -1)
      prepare_unit_ipd <- prepare_unit_from_ipd_code[i, ]
    no_na_dose_ipd <- dose_ipd[!is.na(dose_ipd)]
    no_na_name_ipd <- name_ipd[!is.na(name_ipd)]
    if (length(no_na_name_ipd) != length(no_na_dose_ipd))
      stop("number of doses and number of medications should be equal")
    if (is.null(name_ipd)) {
      med_valid_check <- -1
    } else {
      if (sum(is.na(unname(name_ipd))) >= length(name_ipd))
        med_valid_check <- -1
      else
        med_valid_check <- 0
    }
    if (med_valid_check != -1) {
      total_med_wt_period <- 0
      total_cost_period <- 0
      total_med_equiv_dose_period <- 0
      total_cost_per_equiv_period <- 0
      for (j in seq_len(length(name_ipd))) {
        if (!is.null(name_ipd[j]) & !is.na(name_ipd[j])) {
          #(name, form, brand, dose, preparation,and volume of bottle)
          match_name <- return_equal_str_col(name_cost_col_no, unit_cost_data,
                                             name_ipd[j])

          indices_form1 <- which(stringr::str_detect(toupper(match_name[[form_cost_col_no]]), "LIQUID"))
          indices_form2 <- which(stringr::str_detect(toupper(match_name[[form_cost_col_no]]), "SOLUTION"))
          indices_form3 <- which(stringr::str_detect(toupper(match_name[[form_cost_col_no]]), "DROP"))
          indices_form4 <- which(stringr::str_detect(toupper(match_name[[form_cost_col_no]]), "AMPOULE"))
          indices_form5 <- which(stringr::str_detect(toupper(match_name[[form_cost_col_no]]), "SUSPENSION"))
          indices_form6 <- which(stringr::str_detect(toupper(match_name[[form_cost_col_no]]), "VIAL"))
          indices_form <- unique(c(indices_form1, indices_form2, indices_form3, indices_form4,
                                   indices_form5, indices_form6))
          match_form <- match_name[indices_form, ]

          if (brand_check != -1) {
            if (is.null(brand_ipd[j])) {
              match_form_brand <- match_form
            } else {
              if (is.na(brand_ipd[j])) {
                match_form_brand <- match_form
              } else {
                if (brand_ipd[j] == "" | brand_ipd[j] == " ") {
                  match_form_brand <- match_form
                } else {
                  match_form_brand <- return_equal_str_col(brand_cost_col_no,
                                                           match_form, brand_ipd[j])
                  if (nrow(match_form_brand) < 1)
                    stop("Did not find matching brand name of medication")
                }
              }
            }
          } else {
            match_form_brand <- match_form
          }
          # get the unit of doses from the ipd
          if (unit_med_check == -1)
            dose_num_val_ipd <-
            as.numeric(stringr::str_extract(dose_ipd[j], "\\d+\\.*\\d*"))
          else
            dose_num_val_ipd <- as.numeric(dose_ipd[j])
          dose_in_ipd <- paste(dose_num_val_ipd, unit_dose_ipd[j], sep = "")


          strength_unit_cost <- trimws(gsub("[0-9\\.]", "",
                                            match_form_brand[[dosage_cost_col_no]]))
          strength_val_cost <-
            as.numeric(stringr::str_extract(match_form_brand[[dosage_cost_col_no]],
                                            "\\d+\\.*\\d*"))

          # if the unit cost are listed for different unit of doses say
          # mg/ml or g/ml
          # choose the right one after finding the multiplier which is 1.

          dose_in_cost_data <- paste(strength_val_cost, strength_unit_cost,
                                     sep = "")

          if (any(dose_in_cost_data == dose_in_ipd)) {
            match_form_brand_unit <-
              match_form_brand[dose_in_cost_data ==  dose_in_ipd, ]

          } else {
            stop("The used dosage is not in costing table")
          }

          basis_str_unit_multiply <-
            convert_wtpervoldiff_basis(unit_dose_ipd[j],
                                       basis_strength_unit)
          # get the unit of preparation from the ipd
          if (preparation_dose_check != -1) {
            if (preparation_unit_check == -1) {
              ipd_preparation_dose <- prepare_dose_ipd[j]
              index_slash <- stringr::str_locate(ipd_preparation_dose, "/")
              first_dose_withstr <- stringr::str_sub(ipd_preparation_dose, 1,
                                             index_slash[1] - 1)
              second_dose_withstr <- stringr::str_sub(ipd_preparation_dose,
                          index_slash[2] + 1, nchar(ipd_preparation_dose))
              strength_val_seconddose <-
                as.numeric(stringr::str_extract(second_dose_withstr,
                                                "\\d+\\.*\\d*"))
              vol_unit <- trimws(gsub("[0-9\\.]", "", second_dose_withstr))
              if (strength_val_seconddose == 1)
                ipd_preparation_dose <- paste(first_dose_withstr, "/",
                                             vol_unit, sep = "")

            } else {
              prepare_dose_val <- (prepare_dose_ipd[j])
              prepare_dose_unit_val <- prepare_unit_ipd[j]

              index_slash <- stringr::str_locate(prepare_dose_val, "/")
              first_dose <- stringr::str_sub(prepare_dose_val, 1,
                                             index_slash[1] - 1)

              second_dose <- stringr::str_sub(prepare_dose_val,
                                              index_slash[2] + 1, nchar(prepare_dose_val))

              index <- stringr::str_locate(prepare_dose_unit_val, "/")
              wt_unit <- stringr::str_sub(prepare_dose_unit_val, 1,
                                          index[1] - 1)

              vol_unit <- stringr::str_sub(prepare_dose_unit_val,
                                           index[2] + 1, nchar(prepare_dose_unit_val))
              if (as.numeric(second_dose) == 1) {
                ipd_preparation_dose <- paste(first_dose, wt_unit, "/",
                                              vol_unit, sep = "")
              } else {
                ipd_preparation_dose <- paste(first_dose, wt_unit, "/",
                                              second_dose, vol_unit, sep = "")
              }

            }
            match_form_brand_unit_prepare <-
              match_form_brand_unit[match_form_brand_unit[[preparation_cost_col_no]] ==
                                      ipd_preparation_dose, ]
            if (nrow(match_form_brand_unit_prepare) == 0) {
              match_form_brand_unit_prepare <- match_form_brand_unit
            }
          } else {
            match_form_brand_unit_prepare <- match_form_brand_unit
          }
          unit_used_costing <-
            tolower(unique(match_form_brand_unit_prepare[[unit_cost_col_no]]))
          if (sum(unit_used_costing %in% "per bottle") >= 1) {
            bottle_vol_cost <-
              as.numeric(unlist(match_form_brand_unit_prepare[vol_cost_col_no]))
            bottle_vol_unit_cost <-
              unlist(match_form_brand_unit_prepare[vol_unit_cost_col_no])

            bottle_volandunit_cost <- paste(bottle_vol_cost,
                                            bottle_vol_unit_cost, sep = "")

            if (bottle_size_unit_check == -1)
              bottle_size_num_val_ipd <-
              as.numeric(stringr::str_extract(bot_size_ipd[j], "\\d+\\.*\\d*"))
            else
              bottle_size_num_val_ipd <- as.numeric(bot_size_ipd[j])
            bottle_size_in_ipd <- paste(bottle_size_num_val_ipd,
                                        bot_size_unit_ipd[j], sep = "")

            if (any(bottle_volandunit_cost == bottle_size_in_ipd)) {
              match_form_brand_unit_prepare_size <-
                match_form_brand_unit_prepare[bottle_volandunit_cost ==
                                                bottle_size_in_ipd, ]
              uni_cost_per_bottle <-
                sum(match_form_brand_unit_prepare_size[[unit_cost_column]]) /
                nrow(match_form_brand_unit_prepare_size)
            } else {
              stop("The used vol and unit of bottle is not in costing table")
            }

          } else {
            stop("Error- liquids needs to be costed per bottle")
          }

          if (eqdose_check != -1) {
            temp <- return_equal_str_col(drug_col_conv_table, eqdose_covtab,
                                         name_ipd[j])
            if (nrow(temp) != 0) {
                tempa <- return_equal_liststring_listcol(form_col_conv_table, temp,
                                                         words)
                unit_conv_table <- tempa[[dose_unit_col_conv_table]]
                unit_converts <-
                  unlist(lapply(unit_conv_table, convert_wtpervoldiff_basis,
                                unit_dose_ipd[j]))
                temp2 <- tempa[which(unit_converts == 1), ]
                if (nrow(temp2) < 1)
                  stop("The unit in the conversion table is not correct or
                       can not be checked")
                conver_factor <- temp2[[conv_factor_col_conv_table]]
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
                }
            } else {
              conversion_factor <- 0
            }
          }
          if (bottle_lasts_unit_check == -1)
            bottle_lasts_num_val_ipd <-
            as.numeric(stringr::str_extract(bot_lasts_ipd[j], "\\d+\\.*\\d*"))
          else
            bottle_lasts_num_val_ipd <- as.numeric(bot_lasts_ipd[j])
          ipd_bottle_lasts <- paste(bottle_lasts_num_val_ipd,
                                    bot_lasts_unit_ipd[j], sep = " ")
          basis_time_multiply <-
            convert_to_given_timeperiod(ipd_bottle_lasts, internal_basis_time)
          no_bottles_used_basis <- (1 / basis_time_multiply)

          vol_unit_multiplier <- convert_volume_basis(bot_size_unit_ipd[j],
                                                      basis_vol_unit)
          index <- stringr::str_locate(unit_dose_ipd[j], "/")
          this_wt_unit <- stringr::str_sub(unit_dose_ipd[j], 1, index[1] - 1)

          wt_unit_multiplier <- convert_weight_diff_basis(this_wt_unit,
                                                          basis_wt_unit)

          # no of bottle times the unit cost
          cost_basis <- ceiling(no_bottles_used_basis) * uni_cost_per_bottle

          time_multiplier <- convert_to_given_timeperiod(timeperiod,
                                                         internal_basis_time)

          no_bottles_period <- no_bottles_used_basis * time_multiplier
          bottles_taken_period_costing <- ceiling(no_bottles_period)

          #converting to correct strength unit
          med_str_correct_unit <-  dose_num_val_ipd  * basis_str_unit_multiply

          bottle_size_correct_unit <- bottle_size_num_val_ipd *
            vol_unit_multiplier

          # 2mg/ml * 500ml in 40 days - calculating for a day
          # 2 mg/ml * (500ml/40 days) = 25 mg per day
          med_wt_period <- med_str_correct_unit * wt_unit_multiplier *
            bottle_size_correct_unit * no_bottles_period * time_multiplier

          cost_period <- bottles_taken_period_costing * uni_cost_per_bottle
          med_wt_equiv_period <- med_wt_period * conversion_factor
          cost_per_equiv_period  <- cost_period / med_wt_equiv_period

        } else {
          med_wt_period <- 0
          cost_period <- 0
          med_wt_equiv_period <- 0
          cost_per_equiv_period <- 0

        }
        total_med_wt_period <- total_med_wt_period + med_wt_period
        total_cost_period <- total_cost_period + cost_period
        total_med_equiv_dose_period <- total_med_equiv_dose_period +
          med_wt_equiv_period
        total_cost_per_equiv_period <- total_cost_per_equiv_period +
          cost_per_equiv_period
      }
    } else {
      total_med_wt_period <- NA
      total_cost_period <- NA
      total_med_equiv_dose_period <- NA
      total_cost_per_equiv_period <- NA
    }
    keywd <- "liquid"
    list_total_med_wt_period <- append(list_total_med_wt_period,
                                       total_med_wt_period)
    list_total_cost_period <- append(list_total_cost_period, total_cost_period)
    list_total_med_equiv_dose_period <- append(list_total_med_equiv_dose_period,
                                               total_med_equiv_dose_period)
    list_total_cost_per_equiv_period <- append(list_total_cost_per_equiv_period,
                                               total_cost_per_equiv_period)
  }
  this_name <- paste("totmed_wt_period_", keywd, "_", basis_wt_unit, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_wt_period)
  this_name <- paste("totcost_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_period)
  this_name <- paste("totmed_per_equiv_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_med_equiv_dose_period)
  this_name <- paste("totcost_per_equiv_period_", keywd, sep = "")
  ind_part_data[[this_name]] <- unlist(list_total_cost_per_equiv_period)
  return(ind_part_data)
}
##############################################################################
#' Function to estimate the cost of liquids when IPD is in long format
#' @param the_columns columns that are to be used to convert the data
#' from long to wide
#' @param ind_part_data_long IPD
#' @param name_med name of medication
#' @param brand_med brand name of medication if revealed
#' @param dose_med dose of medication used
#' @param unit_med unit of medication ; use null if its along with the dose
#' @param bottle_size size of the bottle used
#' @param bottle_size_unit unit of bottle volume
#' @param bottle_lasts how long the bottle lasted
#' @param bottle_lasts_unit time unit of how long the bottle lasted
#' @param preparation_dose dose if preparation is given
#' @param preparation_unit unit of preparatio dose
#' @param timeperiod time period for cost calculation
#' @param unit_cost_data  unit costs data
#' @param unit_cost_column column name of unit cost in unit_cost_data
#' @param cost_calculated_per column name of unit where the cost is calculated
#' @param strength_column column column name that has strength of medication
#' @param list_of_code_names if names is coded, give the code:name pairs,
#' optional
#' @param list_of_code_brand if brand names  are coded, give the
#' code:brand pairs, optional
#' @param list_of_code_dose_unit if unit is coded, give the code:unit pairs,
#' optional
#' @param list_of_code_bottle_size_unit  list of bottle size units and codes
#' @param list_of_code_bottle_lasts_unit list of time of bottle lasts and codes
#' @param list_preparation_dose_unit list of preparation dose units and codes
#' @param eqdose_covtab table to get the conversion factor for equivalent
#' doses, optional
#' @param basis_strength_unit strength unit to be taken as basis
#' required for total medication calculations
#' @return the calculated cost of tablets along with original data
#' @examples
#'med_costs_file <- system.file("extdata", "medicaton_costs_all.xlsx",
#'package = "packDAMipd")
#'data_file <- system.file("extdata", "medication_liq.xlsx",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(data_file)
#' med_costs <- load_trial_data(med_costs_file)
#' conv_file <- system.file("extdata", "Med_calc.xlsx",
#' package = "packDAMipd")
#' table <- load_trial_data(conv_file)
#' names <- colnames(ind_part_data)
#' ending <- length(names)
#' ind_part_data_long <- tidyr::gather(ind_part_data, measurement, value,
#' names[2]:names[ending], factor_key = TRUE)
#' the_columns <- c("measurement", "value")
#' res <- microcosting_liquids_long(the_columns,
#' ind_part_data_long = ind_part_data_long,
#' name_med = "liq_name", brand_med =  NULL, dose_med = "liq_strength",
#' unit_med = NULL, bottle_size = "liq_bottle_size",bottle_size_unit = NULL,
#' bottle_lasts = "liq_lasts",bottle_lasts_unit = NULL,preparation_dose = NULL,
#' preparation_unit = NULL,timeperiod = "4 months",unit_cost_data = med_costs,
#' unit_cost_column = "UnitCost",cost_calculated_per = "Basis",
#' strength_column = "Strength",list_of_code_names = NULL,
#' list_of_code_brand = NULL,list_of_code_dose_unit = NULL,
#' list_of_code_bottle_size_unit = NULL,list_of_code_bottle_lasts_unit = NULL,
#' list_preparation_dose_unit = NULL,eqdose_covtab = table,
#' basis_strength_unit = NULL)
#' @export
#' @importFrom tidyr gather
#' @importFrom tidyr spread_
microcosting_liquids_long <- function(the_columns,
                                      ind_part_data_long,
                                      name_med,
                                      brand_med = NULL,
                                      dose_med,
                                      unit_med = NULL,
                                      bottle_size,
                                      bottle_size_unit = NULL,
                                      bottle_lasts,
                                      bottle_lasts_unit = NULL,
                                      preparation_dose = NULL,
                                      preparation_unit = NULL,
                                      timeperiod,
                                      unit_cost_data,
                                      unit_cost_column,
                                      cost_calculated_per,
                                      strength_column,
                                      list_of_code_names = NULL,
                                      list_of_code_brand = NULL,
                                      list_of_code_dose_unit = NULL,
                                      list_of_code_bottle_size_unit = NULL,
                                      list_of_code_bottle_lasts_unit = NULL,
                                      list_preparation_dose_unit = NULL,
                                      eqdose_covtab = NULL,
                                      basis_strength_unit = NULL) {

  #Error - data should not be NULL
  if (is.null(ind_part_data_long) | is.null(unit_cost_data))
    stop("data should not be NULL")

  ind_part_data_wide <- tidyr::spread_(ind_part_data_long, the_columns[1],
                                       the_columns[2])
  results_wide <- microcosting_liquids_wide(ind_part_data_wide,
                                            name_med,
                                            brand_med,
                                            dose_med,
                                            unit_med,
                                            bottle_size,
                                            bottle_size_unit,
                                            bottle_lasts,
                                            bottle_lasts_unit,
                                            preparation_dose,
                                            preparation_unit,
                                            timeperiod,
                                            unit_cost_data,
                                            unit_cost_column,
                                            cost_calculated_per,
                                            strength_column,
                                            list_of_code_names,
                                            list_of_code_brand,
                                            list_of_code_dose_unit,
                                            list_of_code_bottle_size_unit,
                                            list_of_code_bottle_lasts_unit,
                                            list_preparation_dose_unit,
                                            eqdose_covtab,
                                            basis_strength_unit)
  results_wide <- as.data.frame(results_wide)
  columns <- colnames(results_wide)
  num <- length(columns)
  result_long <- tidyr::gather(results_wide, key = "measurement", value = "value",
                               columns[2]:columns[num], factor_key = TRUE)
  return(result_long)
}
