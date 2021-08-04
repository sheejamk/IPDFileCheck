##############################################################################
#' Function to estimate the cost of resource use taken (from IPD)
#' @param ind_part_data IPD
#' @param name_use_col name of the column containing resource use
#' @param each_length_num_use list of column names that shows
#' length/number of  repeated use eg. hospital admission
#' @param each_use_provider_indicator list of column names that shows the
#' bool indicators for the use of resource if this
#' is to be included for the particular provider, say an nhs hospital use
#' @param unit_length_use the column name that contains how many or
#' how long used
#' @param unit_cost_data  unit costs data where the assumption is that
#' the unit cost for resources
#' such as hospital use, gp visit are listed in column resource/resource
#' use with unit costs in another
#' column and the units calculated as in another column
#' @param name_use_unit_cost name of resource use (the column name in
#' the unit cost
#' data is assumed to be name/resource/type etc) in unit cost data
#' @param unit_cost_column column name of unit cost in unit_cost_data
#' @param cost_calculated_in column name of unit where the cost is calculated
#' @param list_code_use_indicator if the column name_use_col  shows codes
#' to indicate the resource use provide the list of codes and resource use
#' for eg., list(c("yes", "no", c(1,2)))
#' @param list_code_provider_indicator  column each_use_provider_indicator
#' shows codes to indicate the resource use
#' provide the list of codes and resource use for eg.,
#' list(c("yes", "no", c(1,2)))
#' @return the calculated cost of resource uses along with original data
#' @examples
#' costs_file <- system.file("extdata", "costs_resource_use.csv",
#' package = "packDAMipd")
#' datafile <- system.file("extdata", "resource_use_hc_2.csv",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(datafile)
#' unit_cost_data <- load_trial_data(costs_file)
#' res <- costing_resource_use(
#'   ind_part_data[1, ],
#'   "hospital_admission_1",
#'   list("length_1", "length_2"),
#'   list("nhs_1", "nhs_2"),
#'   "day",
#'   unit_cost_data, "Inpatient hospital admissions", "UnitCost",
#'   "UnitUsed",
#'   NULL, NULL
#' )
#' @export
costing_resource_use <- function(ind_part_data,
                                 name_use_col,
                                 each_length_num_use = NULL,
                                 each_use_provider_indicator = NULL,
                                 unit_length_use = "day",
                                 unit_cost_data,
                                 name_use_unit_cost,
                                 unit_cost_column,
                                 cost_calculated_in,
                                 list_code_use_indicator = NULL,
                                 list_code_provider_indicator = NULL) {
  # Error - data should not be NULL
  if (is.null(ind_part_data) | is.null(unit_cost_data))
    stop("data should not be NULL")

  # check columns exist in individual data
  info_list <- c(name_use_col)
  checks <- sapply(info_list, IPDFileCheck::check_column_exists, ind_part_data)
  if (sum(checks) != 0) {
    stop("Any one of the required columns can not be found in IPD")
  }
  # if the use is indicated by a code in the data
  if (is.null(list_code_use_indicator)) {
    use_ind_from_code <- toupper(ind_part_data[[name_use_col]])
  } else {
    use_code <- stats::setNames(as.list(list_code_use_indicator[[1]]),
                                list_code_use_indicator[[2]])
    use_from_data <- ind_part_data[[name_use_col]]
    use_ind_from_code <- toupper(use_code[use_from_data])

    index <- which(is.na(use_from_data))
    if (length(index) > 0)
      use_ind_from_code[index] <- NA
  }

  # check length of each_length_use, each_number_use,
  # each_use_provider_indicator to be same
  if (!is.null(each_length_num_use) & !is.null(each_use_provider_indicator)) {
    if (length(each_length_num_use) != length(each_use_provider_indicator)) {
      stop("length of each event of resource use should be equal to
           length of use from provider indicator")
    }
  }

  # check columns exist in unit cost data
  info_list <- c(unit_cost_column, cost_calculated_in)
  checks <- sapply(info_list, IPDFileCheck::check_column_exists,
                   unit_cost_data)
  if (sum(checks) != 0) {
    stop("Any one of the required columns can not be found in unit cost data")
  }
  # find the column name under which each resource use is listed
  name_pattern <- c("name", "resource", "resource use", "type of resource")
  bool_res <- unlist(lapply(name_pattern,
                            IPDFileCheck::check_col_pattern_colname,
                            colnames(unit_cost_data)))
  if (any(bool_res)) {
    name_ind <- which(bool_res == TRUE)
  } else {
    stop("Error- Name of the resource use is not found. Please use name,
    resource, resource use,
         or type of resource to indicate the medication")
  }
  name_col_no <-
    IPDFileCheck::get_colno_pattern_colname(name_pattern[name_ind],
                                            colnames(unit_cost_data))

  # initializing the result columns
  column_names <- colnames(ind_part_data)
  new_col <- paste("totcost_", name_use_col, sep = "")
  if (is.na(match(new_col, column_names))) {
    ind_part_data[[new_col]] <- NA
  }
  use_desc_from_code <- c()
  if (!is.null(each_use_provider_indicator)) {
    for (kk in 1:length(each_use_provider_indicator)) {
      if (IPDFileCheck::check_column_exists(each_use_provider_indicator[kk],
                                            ind_part_data) == 0) {
        # if the use corresponding to the provider is coded,
        # then use the code
        this_column_name <- unlist(each_use_provider_indicator[kk])
        if (is.null(list_code_provider_indicator[kk])) {
          this_use_desc_from_code <- toupper(ind_part_data[[this_column_name]])
        } else {
          # else read it from ind data
          use_code <- stats::setNames(as.list(list_code_provider_indicator[[1]]),
                                      list_code_provider_indicator[[2]])
          use_from_data <- ind_part_data[[this_column_name]]
          this_use_desc_from_code <- toupper(use_code[use_from_data])
          index <- which(is.na(use_from_data))
          if (length(index) > 0)
            this_use_desc_from_code[index] <- NA
        }
      } else {
        if (!is.null(list_code_provider_indicator))
          stop("Error - column do not exist and given codes for provider
                   indicator")
        else
          this_use_desc_from_code <- rep("YES",nrow(ind_part_data))
      }
      use_desc_from_code <- cbind(use_desc_from_code, this_use_desc_from_code)
    }
    use_desc_from_code <- as.data.frame(use_desc_from_code)
  } else {
    this_use_desc_from_code <- rep("YES",nrow(ind_part_data))
    use_desc_from_code <- cbind(use_desc_from_code, this_use_desc_from_code)
    use_desc_from_code <- as.data.frame(use_desc_from_code)
  }
  # if the name of the resource use in unit cost is not null, find the subset of
  # data with that resource use
  # or get the column name with the resource use given at the ith row of the ind
  # data (these might be
  # the resource use where it is not needed to specify the use by a yes/no and
  # rather by number of contacts)
  for (i in 1:nrow(ind_part_data)) {
    if (!is.null(name_use_unit_cost)) {
      subset1 <- unit_cost_data[toupper(unit_cost_data[[name_col_no]]) ==
                                  toupper(name_use_unit_cost), ]
    } else {
      service_cost_data <- trimws(toupper(unit_cost_data[[name_col_no]]))
      service_from_ipd <- trimws(toupper(use_ind_from_code[i]))
      subset1 <- unit_cost_data[ service_cost_data == service_from_ipd, ]
    }
    # if no matching rows found, throw error
    if (nrow(subset1) < 1) {
      stop("Error- Name of resource use in unit cost data not found")
    }
    # get the unit cost of that resource use
    unit_cost <- subset1[[unit_cost_column]]
    cost_calculatedin <- subset1[[cost_calculated_in]]
    if (cost_calculatedin == "per admission" |
        cost_calculatedin == "admission")
      peradmission_cost = TRUE
    else peradmission_cost = FALSE

    # if they need to indicate that use is to be accounted admitted or not
    if (!is.null(name_use_unit_cost)) {
      # identify, yes or true as indicator for use
      ind_yes <- match(toupper(use_ind_from_code[i]), c("YES", "TRUE"))
      ind_no <- match(toupper(use_ind_from_code[i]), c("NO", "FALSE"))
    } else {
      # else assume all are to be included
      ind_yes <- 1
      ind_no <- 0
    }
    if (!is.na(ind_no) & is.na(ind_yes)) {
      ind_part_data[[new_col]][i] <- 0
    } else {
      if (!is.na(ind_yes) & ind_yes > 0) {
        if (IPDFileCheck::check_column_exists(unit_length_use,
                                              ind_part_data) != 0) {
          uni_expr <- unit_length_use
        } else {
          uni_expr <- ind_part_data[[unit_length_use]][i]
        }
        total_length_num <- 0
        # each_length_num_use is the column names where it is given the
        # length of use
        if (is.null(each_length_num_use) | peradmission_cost) {
          if (is.na(use_desc_from_code[i, ])) {
            total_length_num <- 0
          } else{
            if (use_desc_from_code[i,] == "YES" | use_desc_from_code[i, ] == "TRUE") {
              total_length_num <- 1
            } else {
              total_length_num <- 0
            }
          }
        } else {
          for (m in seq_len(length(each_length_num_use))) {
            # check each column exists
            if (IPDFileCheck::check_column_exists(each_length_num_use[m],
                                                  ind_part_data) == 0) {
              if (!is.na(use_desc_from_code[i, m])) {
                # for multiple uses of resource, add the numbers together
                if (use_desc_from_code[i,m] == "YES" |
                    use_desc_from_code[i,m] == "TRUE") {
                  this_column_name <- unlist(each_length_num_use[m])
                  total_length_num <-
                    total_length_num +
                        as.numeric(ind_part_data[[this_column_name]][i])
                } else {
                  total_length_num = total_length_num + 0
                }
              } else {
                if (total_length_num == 0) {
                  total_length_num <-  total_length_num + NA
                } else {
                  total_length_num <-  total_length_num + 0
                }
              }
            } else {
              stop("Error - length or number of use havent indicated")
            }
          }
        }
        # find the cost calculated subset

        if (cost_calculatedin == uni_expr |
            cost_calculatedin == paste("per", uni_expr)) {
          if (cost_calculatedin == "per admission" |
              cost_calculatedin == "admission")
            total_cost <- total_length_num * unit_cost
          else
            total_cost <- total_length_num * unit_cost
        } else {
          stop("units of resource use expressed and calculated are different")
        }
        ind_part_data[[new_col]][i] <- total_cost
      } else {
        ind_part_data[[new_col]][i] <- NA
      }
    }
    # get the unit expressed

  }
  return(ind_part_data)
}
##############################################################################
#' Function to extract the unit hospital inpatient admission by
#' matching HRG code
#' @param hrg hrg code corresponding to the inpatient admission
#' @param ref_cost_data_file file that has unit cost
#' @param col_name_hrg_code name of the column that has the hrg code
#' @param unit_cost_col name of the colum with the unit cost
#' @param sheet sheet if excel file is given
#' @return unit cost the unit cost matching the hrg code
#' @examples
#' ref_cost_data_file <- system.file("extdata",
#' "National_schedule_of_NHS_costs_2019.csv", package = "packDAMipd")
#' get_cost_ip_dc_hrg("AA22C", ref_cost_data_file, "Currency_Code",
#' "National_Average_Unit_Cost")
#' @export
get_cost_ip_dc_hrg <- function(hrg, ref_cost_data_file, col_name_hrg_code,
                                   unit_cost_col, sheet = NULL) {
  check_list <- list(hrg, col_name_hrg_code, unit_cost_col)
  checks <- sapply(check_list, check_null_na)
  if (any(checks != 0)) {
    stop("Error - the variables can not be NULL or NA")
  }
  if (is.null(ref_cost_data_file))
    stop("Error - dataset can not be NULL")
  if (is.character(ref_cost_data_file)) {
    if (is.null(sheet))
      ref_cost_data <- load_trial_data(ref_cost_data_file)
    else
      ref_cost_data <- load_trial_data(ref_cost_data_file, sheet)
  } else {
    ref_cost_data <- ref_cost_data_file
  }
  col_checks <- c(col_name_hrg_code, unit_cost_col)
  results <- sapply(col_checks, IPDFileCheck::check_column_exists,
                    ref_cost_data)
  if (sum(results) == 0) {
    colno <- IPDFileCheck::get_columnno_fornames(ref_cost_data,
                                                 col_name_hrg_code)
    cor_row <- ref_cost_data[ref_cost_data[[colno]] == hrg, ]
    if (nrow(cor_row) < 1) {
      stop("Error HRG code should match atleast to 1")
    } else {
      unit_cost_hrg <- sum(cor_row[[unit_cost_col]]) / nrow(cor_row)
    }
  }
  return(unit_cost_hrg)

}
##############################################################################
#' Function to extract the unit hospital inpatient admission by
#' matching description
#' @param description description corresponding to the inpatient admission
#' @param ref_cost_data_file file that has unit cost
#' @param col_name_description name of the column that has the description
#' @param unit_cost_col name of the column with the unit cost
#' @param sheet sheet if excel file is given
#' @return unit cost the unit cost matching the hrg code
#' @export
#' @examples
#' ref_cost_data_file <- system.file("extdata",
#' "National_schedule_of_NHS_costs_2019.csv", package = "packDAMipd")
#' result <- get_cost_ip_dc_description("Cerebrovascular Accident",
#' ref_cost_data_file, "Currency_Description",
#' "National_Average_Unit_Cost")
get_cost_ip_dc_description <- function(description, ref_cost_data_file,
                                           col_name_description,
                                           unit_cost_col, sheet = NULL) {

  check_list <- list(description, col_name_description, unit_cost_col)
  checks <- sapply(check_list, check_null_na)
  if (any(checks != 0)) {
    stop("Error - description or column name not valid")
  }
  if (is.null(ref_cost_data_file))
    stop("Error - dataset can not be NULL")
  if (is.character(ref_cost_data_file)) {
    if (is.null(sheet))
      ref_cost_data <- load_trial_data(ref_cost_data_file)
    else
      ref_cost_data <- load_trial_data(ref_cost_data_file, sheet)
  } else {
    ref_cost_data <- ref_cost_data_file
  }
  col_checks <- c(col_name_description, unit_cost_col)
  results <- sapply(col_checks, IPDFileCheck::check_column_exists,
                    ref_cost_data)
  if (sum(results) == 0) {
    colno <- IPDFileCheck::get_columnno_fornames(ref_cost_data,
                                                 col_name_description)
    cor_row <- grep(description, ref_cost_data[[colno]])
    the_rows <- ref_cost_data[cor_row, ]
    if (nrow(the_rows) < 1) {
      stop("Error description should match atleast to 1")
    } else {
      unit_cost <- sum(the_rows[[unit_cost_col]]) / nrow(the_rows)
    }
  }
  return(unit_cost)
}
##############################################################################
#' Function to estimate the cost of inpatient admission but taken from
#' GP records where HRG code or description known
#' @param ind_part_data IPD
#' @param hrg_code_ip_admi column name of hrg code (for inpatient admission)
#' @param descrip_ip_admi column name of description for inpatient admission
#' @param number_use_ip_admi the number of days spent in each admission if that
#' is a criteria to be included. Otherwise each admission will be costed
#' @param elective_col colname to say whether it is an elective admission or
#' non elective admission
#' @param unit_cost_data unit cost data file with hrg code/descriptions
#' and unit costs are listed for inpatient admission
#' @param hrg_code_col  hrg code column name in unit cost data
#' @param description_col column name of description of inpatient admission in
#' the unit cost data
#' @param unit_cost_col column name of unit cost in unit_cost_data
#' @param cost_calculated_in  name of unit where the cost is calculated
#' assumed to be per admission
#' @return the calculated cost of inpatient admission long with original data
#' @examples
#' costs_file <- system.file("extdata",
#'  "National_schedule_of_NHS_costs_2019.csv",
#'  package = "packDAMipd")
#'  datafile <- system.file("extdata", "resource_use_hc_ip.csv",
#'  package = "packDAMipd")
#'  ind_part_data <- packDAMipd::load_trial_data(datafile)
#'  unit_cost_data <- packDAMipd::load_trial_data(costs_file)
#'  result <- costing_inpatient_daycase_admission(ind_part_data,
#'  hrg_code_ip_admi = "HRGcode", descrip_ip_admi = NULL,
#' number_use_ip_admi = "number_use", elective_col = "EL",
#' unit_cost_data, hrg_code_col = "Currency_Code", description_col = NULL,
#'  unit_cost_col ="National_Average_Unit_Cost",
#'  cost_calculated_in = "admission")
#' @export
costing_inpatient_daycase_admission <- function(ind_part_data,
                                        hrg_code_ip_admi,
                                        descrip_ip_admi,
                                        number_use_ip_admi,
                                        elective_col,
                                        unit_cost_data,
                                        hrg_code_col,
                                        description_col,
                                        unit_cost_col,
                                        cost_calculated_in = "admission") {
  check_list <- list(unit_cost_col, cost_calculated_in, elective_col)
  res <- sapply(check_list, check_null_na)
  if (sum(res) != 0)
    stop("Error - unit cost column, unit for calculation or column that
         indicates elective admission can not be null")

  # Error - data should not be NULL
  if (is.null(ind_part_data) | is.null(unit_cost_data))
    stop("data should not be NULL")
  check1 <- sum(is.null(hrg_code_ip_admi))
  check2 <- sum(is.null(descrip_ip_admi))
  if (check1 != 0 & check2 != 0)
    stop("both hrg code and description can not be NULL")
  if (check1 != 0) {
    info_list <- c(descrip_ip_admi)
  } else {
    info_list <- c(hrg_code_ip_admi)
  }
  # check columns exist in individual data
  checks <- sum(sapply(info_list, IPDFileCheck::check_column_exists,
                       ind_part_data))
  if (checks != 0)
    cols_ip_data <- grep(info_list, colnames(ind_part_data))
  if (sum(cols_ip_data) == 0) {
    stop("Any one of the required columns can not be found in IPD")
  }
  if (is.null(hrg_code_col) & is.null(description_col))
    stop("both hrg code and description can not be NULL")

  if (check1 != 0) {
    info <- c(description_col)
  } else {
    info <- c(hrg_code_col)
  }
  check_el <- IPDFileCheck::check_column_exists(elective_col, ind_part_data)
  if (check_el != 0) {
    cols_elec_ip_data <- grep(elective_col, colnames(ind_part_data))
    if (sum(cols_elec_ip_data) == 0)
      stop("Column desribing elective or non elective admission missing")
  }
  use_col <- sum(is.null(number_use_ip_admi))
  if (sum(use_col) == 0) {
    checks <- sapply(number_use_ip_admi, IPDFileCheck::check_column_exists,
                     ind_part_data)
    if (sum(checks) != 0) {
      cols_use_ip_data <- grep(number_use_ip_admi, colnames(ind_part_data))
      if (sum(cols_use_ip_data) == 0)
        stop("Columns on number of admissions can not be found in ip data")
    }
  }
  # check columns exist in unit cost data
  info_list <- c(info, unit_cost_col)
  checks <- sapply(info_list, IPDFileCheck::check_column_exists,
                   unit_cost_data)
  if (sum(checks) != 0) {
    stop("Any one of the required columns can not be found in unit cost data")
  }
  if (cost_calculated_in != "admission")
    stop("Error- cost should be calculated per admission")
  # initialising the result columns
  column_names <- colnames(ind_part_data)
  new_col <- paste("totcost_ip_admission", sep = "")
  if (is.na(match(new_col, column_names))) {
    ind_part_data[[new_col]] <- NA
  }
  for (i in 1:nrow(ind_part_data)) {
    if (check1 == 0) {
      if (is.null(hrg_code_col))
        stop("Error - hrg code shouldnt be null in cost data")
      hrg_codes <- unlist(unname(ind_part_data[i, cols_ip_data]))
      hrg_codes <- hrg_codes[hrg_codes != "" & !is.na(hrg_codes)]
      content <- unlist(unname(toupper(ind_part_data[i, cols_elec_ip_data])))
      content <- content[content != "" & !is.na(content)]

      unit_costs_hrg <- c()
      for (j in seq_len(length(hrg_codes))) {
        if (length(content) != length(hrg_codes))
          stop("the number of elective/non elective should be equal
          to the number of hrg codes")
        if (content[j] == "EL" | content[j]  == "ELECTIVE" |
            content[j]  == "ELEC")
          unit_costs <- get_cost_ip_dc_hrg(hrg_codes[j],
                                   unit_cost_data, hrg_code_col,
                                   unit_cost_col, sheet = "EL")
        else
          unit_costs <- get_cost_ip_dc_hrg(hrg_codes[j],
                                               unit_cost_data, hrg_code_col,
                                               unit_cost_col, sheet = "NEL")
        unit_costs_hrg <- append(unit_costs_hrg, unit_costs)
      }
      if (sum(use_col) == 0) {
        numbers_use <- unlist(unname(ind_part_data[i, cols_use_ip_data]))
        numbers_use <- numbers_use[numbers_use != "" & !is.na(numbers_use)]
        len_num_use <- length(numbers_use)
        len_cost_des <- length(unit_costs_hrg)
        if (len_num_use > len_cost_des) {
          stop("Number of admissions and unit costs extracted are different")

        } else {
          if (len_num_use < len_cost_des) {
            needed <- len_cost_des - len_num_use
            start <- len_num_use + 1
            end <- len_num_use + needed
            numbers_use[start:end] <- 1
          }
        }
        unit_cost_ip_adm <- sum(numbers_use * unit_costs_hrg)
      }else {
        unit_cost_ip_adm <- sum(unit_costs_hrg)

      }
    } else {
      if (is.null(description_col))
        stop("Error - description col shouldnt be null in cost data")
      descriptions <- unlist(unname(ind_part_data[i, cols_ip_data]))
      descriptions <- descriptions[descriptions != ""]
      content <- unlist(unname(toupper(ind_part_data[i, cols_elec_ip_data])))
      content <- content[content != "" & !is.na(content)]

      unit_costs_descriptions <- c()
      for (j in seq_len(length(descriptions))) {
        if (length(content) != length(descriptions))
          stop("the number of elective/non elective should be equal
          to the number of descriptions")
        if (content[j] == "EL" | content[j]  == "ELECTIVE" |
            content[j]  == "ELEC")
          unit_costs <- get_cost_ip_dc_description(descriptions[j],
                                               unit_cost_data, description_col,
                                               unit_cost_col, sheet = "EL")
        else
          unit_costs <- get_cost_ip_dc_description(descriptions[j],
                                          unit_cost_data, description_col,
                                          unit_cost_col, sheet = "NEL")
        unit_costs_descriptions <- append(unit_costs_descriptions, unit_costs)
      }
      if (sum(use_col) == 0) {
        numbers_use <- unlist(unname(ind_part_data[i, cols_use_ip_data]))
        numbers_use <- numbers_use[numbers_use != "" & !is.na(numbers_use)]
        len_num_use <- length(numbers_use)
        len_cost_des <- length(unit_costs_descriptions)
        if (len_num_use > len_cost_des) {
          stop("Number of admissions and unit costs extracted are different")

        } else {
          if (len_num_use < len_cost_des) {
            needed <- len_cost_des - len_num_use
            start <- len_num_use + 1
            end <- len_num_use + needed
            numbers_use[start:end] <- 1
          }
        }
        unit_cost_ip_adm <- sum(numbers_use * unit_costs_descriptions)
      } else{
        unit_cost_ip_adm <- sum(unit_costs_descriptions)
      }
    }
    ind_part_data[[new_col]][i] <- unit_cost_ip_adm
  }
  return(ind_part_data)
}

##############################################################################
#' Function to extract the unit hospital inpatient admission by
#' matching code
#' @param code code for AE attendance
#' @param type_admit term indicating admission and type of attendance
#' @param ref_cost_data_file file that has unit cost
#' @param col_name_code name of the column that has the code
#' @param unit_cost_col name of the column with the unit cost
#' @param type_admit_col colname that describes type of the attendance and
#' that indicates admitted or not
#' @param sheet sheet if excel file is given
#' @return unit cost the unit cost matching the code
#' @examples
#' ref_cost_data_file <- system.file("extdata",
#' "National_schedule_of_NHS_costs_2019_AandE.csv", package = "packDAMipd")
#' re = get_cost_AandE_code("VB02Z", "T01A", ref_cost_data_file,
#' "Currency_Code","National_Average_Unit_Cost", "Service_Code")
#' @export
get_cost_AandE_code <- function(code, type_admit, ref_cost_data_file,
                                col_name_code, unit_cost_col, type_admit_col,
                                sheet = NULL) {
  check_list <- list(code, col_name_code, unit_cost_col, type_admit,
                     type_admit_col)
  checks <- sapply(check_list, check_null_na)
  if (any(checks != 0)) {
    stop("Error - the variables can not be NULL or NA")
  }

  if (is.null(ref_cost_data_file))
    stop("Error - dataset can not be NULL")
  if (is.character(ref_cost_data_file)) {
    if (is.null(sheet))
      ref_cost_data <- load_trial_data(ref_cost_data_file)
    else
      ref_cost_data <- load_trial_data(ref_cost_data_file, sheet)
  } else {
    ref_cost_data <- ref_cost_data_file
  }
  col_checks <- c(col_name_code, unit_cost_col, type_admit_col)
  results <- sapply(col_checks, IPDFileCheck::check_column_exists,
                    ref_cost_data)
  if (sum(results) == 0) {
    colno <- IPDFileCheck::get_columnno_fornames(ref_cost_data,
                                                 col_name_code)
    type_admit_colno <- IPDFileCheck::get_columnno_fornames(ref_cost_data,
                                                            type_admit_col)
    cor_row <- ref_cost_data[ref_cost_data[[colno]] == code &
                               ref_cost_data[[type_admit_colno]]
                             == type_admit, ]
    if (nrow(cor_row) < 1) {
      stop("Error code should match atleast to 1")
    } else {
      unit_cost_code <- sum(cor_row[[unit_cost_col]]) / nrow(cor_row)
    }
  }
  return(unit_cost_code)
}
##############################################################################
#' Function to extract the unit cost by descirption of AandE att
#' matching description
#' @param description description of the AE attendance
#' @param type_admit term indicating admission and type of attendance
#' @param ref_cost_data_file file that has unit cost
#' @param col_name_description name of the column that has the description
#' @param unit_cost_col name of the column with the unit cost
#' @param type_admit_col colname that descirbes type of the attendance and
#' @param sheet sheet if excel file is given
#' @return unit cost the unit cost matching the hrg code
#' @export
#' @examples
#' ref_cost_data_file <- system.file("extdata",
#' "National_schedule_of_NHS_costs_2019_AandE.csv", package = "packDAMipd")
#' re = get_cost_AandE_description("Emergency Medicine", "T01A",
#' ref_cost_data_file, "Currency_Description", "National_Average_Unit_Cost",
#' "Service_Code")
get_cost_AandE_description <- function(description, type_admit,
                                       ref_cost_data_file,
                                       col_name_description,
                                       unit_cost_col, type_admit_col,
                                       sheet = NULL) {

  check_list <- list(description, col_name_description, unit_cost_col,
                     type_admit, type_admit_col)
  checks <- sapply(check_list, check_null_na)
  if (any(checks != 0)) {
    stop("Error - description or column name not valid")
  }
  if (is.null(ref_cost_data_file))
    stop("Error - dataset can not be NULL")
  if (is.character(ref_cost_data_file)) {
    if (is.null(sheet))
      ref_cost_data <- load_trial_data(ref_cost_data_file)
    else
      ref_cost_data <- load_trial_data(ref_cost_data_file, sheet)
  } else {
    ref_cost_data <- ref_cost_data_file
  }
  col_checks <- c(col_name_description, unit_cost_col, type_admit_col)
  results <- sapply(col_checks, IPDFileCheck::check_column_exists,
                    ref_cost_data)
  if (sum(results) == 0) {
    colno <- IPDFileCheck::get_columnno_fornames(ref_cost_data,
                                                 col_name_description)
    type_admit_colno <- IPDFileCheck::get_columnno_fornames(ref_cost_data,
                                                            type_admit_col)
    typeadmit <-
      ref_cost_data[ref_cost_data[[type_admit_colno]] == type_admit, ]
    cor_row <- grep(description, typeadmit[[colno]])
    the_rows <- ref_cost_data[cor_row, ]
    if (nrow(the_rows) < 1) {
      stop("Error- description should match atleast to 1")
    } else {
      unit_cost <- sum(the_rows[[unit_cost_col]]) / nrow(the_rows)
    }
  }
  return(unit_cost)
}
##############################################################################
#' Function to estimate the cost of inpatient admission but taken from
#' GP records where  code or description known
#' @param ind_part_data IPD
#' @param code_ae column name of  code (for inpatient admission)
#' @param descrip_ae column name of description for inpatient admission
#' @param number_use_ae the number of days spent in each admission if that
#' is a criteria to be included. Otherwise each admission will be costed
#' @param type_admit_ae term indicating admission and type of attendance
#' @param unit_cost_data unit cost data file with  code/descriptions
#' and unit costs are listed for inpatient admission
#' @param code_col   code column name in unit cost data
#' @param type_admit_col colname that describes type of the attendance and
#' @param description_col column name of description of inpatient admission in
#' the unit cost data
#' @param unit_cost_col column name of unit cost in unit_cost_data
#' @param cost_calculated_in  name of unit where the cost is calculated
#' assumed to be per admission
#' @param sheet sheet where the unit costs are listed in the unit costs data
#' file
#' @return the calculated cost of inpatient admission long with original data
#' @examples
#' costs_file <- system.file("extdata",
#' "National_schedule_of_NHS_costs_2019_AandE.csv", package = "packDAMipd")
#' datafile <- system.file("extdata", "resource_use_ae_ip.csv",
#' package = "packDAMipd")
#' ind_part_data <- packDAMipd::load_trial_data(datafile)
#' unit_cost_data <- packDAMipd::load_trial_data(costs_file)
#' result <- costing_AandE_admission(ind_part_data = ind_part_data,
#' code_ae = "code", descrip_ae = NULL, number_use_ae = "number_use",
#' type_admit_ae = "type_admit", unit_cost_data = unit_cost_data,
#' code_col = "Currency_Code", type_admit_col = "Service_Code",
#' description_col = NULL, unit_cost_col ="National_Average_Unit_Cost",
#' cost_calculated_in = "attendance")
#' @export
costing_AandE_admission <- function(ind_part_data,
                                    code_ae,
                                    descrip_ae,
                                    number_use_ae,
                                    type_admit_ae,
                                    unit_cost_data,
                                    code_col,
                                    type_admit_col,
                                    description_col,
                                    unit_cost_col,
                                    cost_calculated_in = "attendance",
                                    sheet = NULL) {
  check_list <- list(unit_cost_col, cost_calculated_in, type_admit_ae,
                     type_admit_col)
  res <- sapply(check_list, check_null_na)
  if (sum(res) != 0)
    stop("Error - should have valid entries for unit cost and cost unit")
  # Error - data should not be NULL
  if (is.null(ind_part_data) | is.null(unit_cost_data))
    stop("data should not be NULL")
  check1 <- sum(is.null(code_ae))
  check2 <- sum(is.null(descrip_ae))
  if (check1 != 0 & check2 != 0)
    stop("both  code and description can not be NULL")
  if (check1 != 0) {
    info_list <- c(descrip_ae)
  } else {
    info_list <- c(code_ae)
  }
  # check columns exist in individual data
  checks <- sum(sapply(info_list, IPDFileCheck::check_column_exists,
                       ind_part_data))
  if (checks != 0)
    cols_data <- grep(info_list, colnames(ind_part_data))
  if (sum(cols_data) == 0) {
    stop("Any one of the required columns can not be found in IPD")
  }
  # check columns exist in individual data
  checks <- sum(sapply(type_admit_ae, IPDFileCheck::check_column_exists,
                       ind_part_data))
  if (checks != 0)
    cols_type_admit <- grep(type_admit_ae, colnames(ind_part_data))
  if (sum(cols_type_admit) == 0) {
    stop("Columns for type and admit can not be found in IPD")
  }
  if (is.null(code_col) & is.null(description_col))
    stop("both  code and description can not be NULL")

  use_col <- sum(is.null(number_use_ae))
  if (sum(use_col) == 0) {
    checks <- sapply(number_use_ae, IPDFileCheck::check_column_exists,
                     ind_part_data)
    if (sum(checks) != 0) {
      cols_use_data <- grep(number_use_ae, colnames(ind_part_data))
      if (sum(cols_use_data) == 0)
        stop("Columns on number of admissions can not be found in ip data")
    }
  }
  if (check1 != 0) {
    info <- c(description_col)
  } else {
    info <- c(code_col)
  }
  # check columns exist in unit cost data
  info_list <- c(info, unit_cost_col, type_admit_col)
  checks <- sapply(info_list, IPDFileCheck::check_column_exists,
                   unit_cost_data)
  if (sum(checks) != 0) {
    stop("Any one of the required columns can not be found in unit cost data")
  }
  if (cost_calculated_in != "attendance")
    stop("Error- cost should be calculated per attendance")
  # initialising the result columns
  column_names <- colnames(ind_part_data)
  new_col <- paste("totcost_AE_Admission", sep = "")
  if (is.na(match(new_col, column_names))) {
    ind_part_data[[new_col]] <- NA
  }
  for (i in 1:nrow(ind_part_data)) {
    if (check1 == 0) {
      if (is.null(code_col))
        stop("Error - code shouldnt be null in cost data")
      codes <- unlist(unname(ind_part_data[i, cols_data]))
      type_admit_codes <- unlist(unname(ind_part_data[i, cols_type_admit]))
      codes <- codes[codes != "" & !is.na(codes)]
      type_admit_codes <- type_admit_codes[type_admit_codes != "" &
                                             !is.na(type_admit_codes)]
      if (length(codes) != length(type_admit_codes))
        stop("length of codes and types should be equal")
      unit_costs_code <- c()
      for (j in  seq_len(length(codes))) {
        unit_costs <-  get_cost_AandE_code(codes[j],
                                           type_admit_codes[j],
                                           unit_cost_data, code_col,
                                           unit_cost_col,
                                           type_admit_col, sheet)
        unit_costs_code <- append(unit_costs_code, unit_costs)
      }
      if (sum(use_col) == 0) {
        numbers_use <- unlist(unname(ind_part_data[i, cols_use_data]))
        numbers_use <- numbers_use[numbers_use != "" & !is.na(numbers_use)]
        len_num_use <- length(numbers_use)
        len_cost_des <- length(unit_costs_code)
        if (len_num_use > len_cost_des) {
          stop("Number of admissions and unit costs extracted are different")

        } else {
          if (len_num_use < len_cost_des) {
            needed <- len_cost_des - len_num_use
            start <- len_num_use + 1
            end <- len_num_use + needed
            numbers_use[start:end] <- 1
          }
        }
        unit_cost_adm <- sum(numbers_use * unit_costs_code)
      } else {
        unit_cost_adm <- sum(unit_costs_code)
      }
    } else {
      if (is.null(description_col))
        stop("Error - description col should not be null in cost data")
      descriptions <- unlist(unname(ind_part_data[i, cols_data]))
      descriptions <- descriptions[descriptions != ""]
      type_admit_codes <- unlist(unname(ind_part_data[i, cols_type_admit]))
      type_admit_codes <- type_admit_codes[type_admit_codes != "" &
                                             !is.na(type_admit_codes)]
      if (length(descriptions) != length(type_admit_codes))
        stop("length of descriptions and types should be equal")
      unit_costs_descriptions <- c()
      for (j in  seq_len(length(descriptions))) {
        unit_costs <-  get_cost_AandE_description(descriptions[j],
                                              type_admit_codes[j],
                                              unit_cost_data, description_col,
                                              unit_cost_col,
                                              type_admit_col, sheet)
        unit_costs_descriptions <- append(unit_costs_descriptions, unit_costs)
      }
      if (sum(use_col) == 0) {
        numbers_use <- unlist(unname(ind_part_data[i, cols_use_data]))
        numbers_use <- numbers_use[numbers_use != "" & !is.na(numbers_use)]
        len_num_use <- length(numbers_use)
        len_cost_des <- length(unit_costs_descriptions)
        if (len_num_use > len_cost_des) {
          stop("Number of admissions and unit costs extracted are different")

        } else {
          if (len_num_use < len_cost_des) {
            needed <- len_cost_des - len_num_use
            start <- len_num_use + 1
            end <- len_num_use + needed
            numbers_use[start:end] <- 1
          }
        }
        unit_cost_adm <- sum(numbers_use * unit_costs_descriptions)
      } else{
        unit_cost_adm <- sum(unit_costs_descriptions)
      }
    }
    ind_part_data[[new_col]][i] <- unit_cost_adm
  }
  return(ind_part_data)
}
