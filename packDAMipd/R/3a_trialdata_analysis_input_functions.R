#######################################################################
# 1. Get the packages required
#######################################################################
# 2 Get the trial data ready
# 2.1 Load the data
#######################################################################
# 2.2 Get the required fields and codes for time points, demography
# - age and gender, qol measure -EQ5D and any other analysis requires
#######################################################################
# 2.2.1 Get the details of the trial arm
#' Function to get the details of the trial arm
#' @param trialdata, data containing individual level trial data
#' @return the name of the variable related to trial arm and the unique contents
#' if success, else error
#' @examples
#' get_trial_arm_details(data.frame(
#'   "Age" = c(21, 15),
#'   "arm" = c("control", "intervention")
#' ))
#' @export
#' @details
#'  expecting the data contains the information on trial arm
#' preferably  column names "arm", "trial" or "trial arm". If
#' multiple column names
#' match these, then first match will be chosen.
get_trial_arm_details <- function(trialdata) {
  trialdata <- data.table::data.table(trialdata, stringsAsFactors = FALSE)
  names <- colnames(trialdata)
  related_words <- c("arm", "trial", "trialarm")
  exists <- unlist(lapply(
    related_words,
    IPDFileCheck::check_col_pattern_colname, names
  ))
  ind <- which(exists == TRUE)
  # find colnumbers that contains the related words
  colnumbers <- unlist(lapply(
    related_words[ind],
    IPDFileCheck::get_colno_pattern_colname, names
  ))
  # if there are multiple columns select the first one
  if (sum(colnumbers > 0) == 1) {
    index <- which(colnumbers > 0)
    this_name <- names[colnumbers[index]]
  }
  if (sum(colnumbers > 0) > 1) {
    index <- which(colnumbers > 0)[1]
    this_name <- names[colnumbers[index]]
  }
  if (sum(colnumbers > 0) < 1) {
    stop("no matching columns found")
  }
  # get the codes - the entries

  codes <- unique(trialdata[[this_name]])
  result <- list(name = this_name, codes = codes)
  return(result)
}
#######################################################################
#' Function to get the details of the gender column
#' @param trialdata, data containing individual level trial data
#' @return the name of the variable related to gender and the unique contents
#' if success, else error
#' @examples
#' get_gender_details(data.frame("Age" = c(21, 15), "sex" = c("m", "f")))
#' @importFrom IPDFileCheck  check_col_pattern_colname
#' @importFrom IPDFileCheck get_colno_pattern_colname
#' @export
#' @details
#' expecting the data contains the information on gender
#' preferably  column names "gender", "sex" or "male" or "female".
#' If multiple column names match these, then first match will be chosen.
get_gender_details <- function(trialdata) {
  #Error - no null trial data
  if (is.null(trialdata)) {
    stop("Error - trial data should not be NULL")
  }
  names <- colnames(trialdata)
  related_words <- c("sex", "gender", "female", "male", "females", "males")
  exists <- unlist(lapply(related_words,
    IPDFileCheck::check_col_pattern_colname, names
  ))
  ind <- which(exists == TRUE)
  colnumbers <- unlist(lapply(
    related_words[ind],
    IPDFileCheck::get_colno_pattern_colname, names
  ))
  if (sum(colnumbers > 0) == 1) {
    index <- which(colnumbers > 0)
    this_name <- names[colnumbers[index]]
  }
  if (sum(colnumbers > 0) > 1) {
    index <- which(colnumbers > 0)[1]
    this_name <- names[colnumbers[index]]
  }
  if (sum(colnumbers > 0) < 1) {
    stop("No matching columns found")
  }
  # get the codes - the entries

  codes <- unique(trialdata[[this_name]])
  result <- list(name = this_name, codes = codes)
  return(result)
}
#######################################################################
# 2.2.3 Get the column names and codes used for age column
#' Function to get the details of the age column
#' @param trialdata, data containing individual level trial data
#' @return the name of the variable related to age and the unique contents
#' if success, else error
#' @examples
#' get_age_details(data.frame("Age" = c(21, 15),
#' "arm" = c("control", "intervention")))
#' @importFrom IPDFileCheck  check_col_pattern_colname
#' @importFrom IPDFileCheck get_colno_pattern_colname
#' @export
#' @details
#' expecting the data contains the information on age
#' preferably  column names "age", "dob" or "yob" or "date of birth".
#' "year of birth", "birth year"
#' If multiple column names match these, then first match will be chosen.
get_age_details <- function(trialdata) {
  #Error - no null trial data
  if (is.null(trialdata)) {
    stop("Error - trial data should not be NULL")
  }
  names <- colnames(trialdata)
  related_words <- c("age", "dob", "yob", "date of birth",
                     "year of birth", "birth year")
  exists <- unlist(lapply(
    related_words, IPDFileCheck::check_col_pattern_colname,
    names
  ))
  ind <- which(exists == TRUE)
  colnumbers <- unlist(lapply(
    related_words[ind], IPDFileCheck::get_colno_pattern_colname,
    names
  ))
  if (sum(colnumbers > 0) == 1) {
    index <- which(colnumbers > 0)
    this_name <- names[colnumbers[index]]
  }
  if (sum(colnumbers > 0) > 1) {
    index <- which(colnumbers > 0)[1]
    this_name <- names[colnumbers[index]]
  }
  if (sum(colnumbers > 0) < 1) {
    stop("No matching columns found")
  }
  # get the codes - the entries

  codes <- unique(trialdata[[this_name]])
  result <- list(name = this_name, codes = codes)
  return(result)
}
#######################################################################
# 2.2.4 Get the colnames of "time point" column
#' Function to get the details of the time point column
#' @param trialdata, data containing individual level trial data
#' @return the name of the variable related to time point and
#' the unique contents if success, else error
#' @examples
#' get_timepoint_details(data.frame("time" = c(21, 15),
#' "arm" = c("control", "intervention")))
#' @importFrom IPDFileCheck  check_col_pattern_colname
#' @importFrom IPDFileCheck get_colno_pattern_colname
#' @export
#' @details
#' expecting the data contains the information on timepoints
#' preferably  column names "time point", "times" or "time" or "timepoint".
#' If multiple column names match these, then first match will be chosen.

get_timepoint_details <- function(trialdata) {
  #Error - no null trial data
  if (is.null(trialdata)) {
    stop("Error - trial data should not be NULL")
  }
  names <- colnames(trialdata)
  related_words <- c("time point", "times", "time", "timepoint")
  exists <- unlist(lapply(
    related_words, IPDFileCheck::check_col_pattern_colname,
    names
  ))
  ind <- which(exists == TRUE)
  colnumbers <- unlist(lapply(
    related_words[ind], IPDFileCheck::get_colno_pattern_colname,
    names
  ))
  if (sum(colnumbers > 0) == 1) {
    index <- which(colnumbers > 0)
    this_name <- names[colnumbers[index]]
  }
  if (sum(colnumbers > 0) > 1) {
    index <- which(colnumbers > 0)[1]
    this_name <- names[colnumbers[index]]
  }
  if (sum(colnumbers > 0) < 1) {
    return(NA)
  }
  # get the codes - the entries

  codes <- unique(trialdata[[this_name]])
  result <- list(name = this_name, codes = codes)
  return(result)
}
#######################################################################
# 2.3 Get the required fields and codes for qol measure - EQ5D and
# any other the analysis requires
#######################################################################
# 2.3, 1 Get the column names of outcome column
#' Function to get the details of the outcome column
#' @param trialdata, data containing individual level trial data
#' @param name, name of the variable
#' @param related_words, probable column names
#' @param multiple, indicates true if there are multiple columns
#' @return the name of the variable related to health outcome (any) and
#' the unique contents if success, else error
#' @examples
#' get_outcome_details(
#'   data.frame("qol.MO" = c(1, 2), "qol.PD" = c(1, 2), "qol.AD" = c(1, 2)),
#'   "eq5d", "qol", TRUE
#' )
#' @export
#' @details
#' if the words related to outcome is given, the function will get the
#' columns and the codes used for the outcome, the difference here is that
#' certain outcomes can  be distributed in multiple columns
get_outcome_details <- function(trialdata, name, related_words,
                                multiple = FALSE) {
  #Error - no null trial data
  if (is.null(trialdata))
    stop("Error - trial data should not be NULL")
  if (is.null(name))
    stop("Error - name should not be NULL")

  names <- colnames(trialdata)
  exists <- unlist(lapply(related_words,
                          IPDFileCheck::check_col_pattern_colname, names))
  ind <- which(exists == TRUE)
  colnumbers <- unlist(lapply(
    related_words[ind], IPDFileCheck::get_colno_pattern_colname,
    names
  ))
  # just one column match
  if (sum(colnumbers > 0) == 1) {
    index <- which(colnumbers > 0)
    this_name <- names[colnumbers[index]]
    codes <- sort(unique(trialdata[[this_name]]))
  }
  # multiple columns matched
  # get the codes - the entries
  if (sum(colnumbers > 0) > 1) {
    if (multiple == TRUE) {
      index <- which(colnumbers > 0)
      this_name <- names[colnumbers[index]]
      all.codes <- c(0)
      for (i in seq_len(length(this_name))) {
        this_ind <- this_name[i]
        codes <- sort(unique(trialdata[[this_ind]]))
        all.codes <- list(all.codes, codes)
        if (i == 1) {
          all.codes <- all.codes[-1]
        }
      }
      unlist_all <- unlist(all.codes)
      codes <- sort(unique(unlist_all))
    } else {
      index <- which(colnumbers > 0)[1]
      this_name <- names[colnumbers[index]]
      codes <- sort(unique(trialdata[[this_name]]))
    }
  }
  if (sum(colnumbers > 0) < 1) {
    stop("no matching columns found")
  }
  result <- list(name = this_name, codes = codes)
  return(result)
}
#######################################################################
# 2.3.2 Get the column names of eq5d column
#' Function to get the details of the EQ5D column
#' @param trialdata, data containing individual level trial data
#' @return the name of the variable related to EQ5D and the unique contents
#' if success, else error
#' @examples
#' get_eq5d_details(data.frame(
#'   "MO" = c(1, 2), "SC" = c(1, 2), "UA" = c(1, 2),
#'   "PD" = c(1, 2), "AD" = c(1, 2)
#' ))
#' @importFrom IPDFileCheck  check_col_pattern_colname
#' @importFrom IPDFileCheck get_colno_pattern_colname
#' @export
#' @details
#' Specific to the EQ5D data - the column names are given as certain sets,
#' Tried to give 15 sets as the column names
get_eq5d_details <- function(trialdata) {
  #Error - no null trial data
  if (is.null(trialdata))
    stop("Error - trial data should not be NULL")
  names <- colnames(trialdata)
  words_set <- structure(list(
    set1 = c(
      "mobility", "self care", "usual activity",
      "pain discomfort", "anxiety"
    ),
    set2 = c(
      "mobility", "self_care", "usual_activity",
      "pain_discomfort", "anxiety"
    ),
    set3 = c(
      "mobility", "selfcare", "usualactivity",
      "paindiscomfort", "anxiety"
    ),
    set4 = c("qol"), set5 = c("eq5d"),
    set6 = c("MO", "SC", "UA", "PD", "AD"),
    set7 = c("mobility", "selfcare", "usualact", "pain", "anxiety"),
    set8 = c("mobility", "selfcare", "usualact", "paindis", "anxiety"),
    set9 = c("mob", "selfcare", "usualact", "paindis", "anxiety"),
    set10 = c("mob", "selfcare", "usualact", "pain", "anxiety"),
    set11 = c("mob", "selfcare", "usualact", "paindis", "anx"),
    set12 = c("mob", "selfcare", "usualact", "pain", "anx"),
    set13 = c(
      "mobility", "self care", "usual activity",
      "pain discomfort", "anxiety depression"
    ),
    set14 = c(
      "mobility", "self_care", "usual_activity",
      "pain_discomfort", "anxiety_depression"
    ),
    set15 = c(
      "mobility", "selfcare", "usualactivity",
      "paindiscomfort", "anxietydepression"
    )
  ))
  i <- 1
  # try if the column names match with any of the set given above
  colnumbers <- 0
  while (i <= length(words_set)) {
    this <- unlist(words_set[i])
    result <- unlist(lapply(this,
                            IPDFileCheck::check_col_pattern_colname, names))
    if (any(result == FALSE)) {
      i <- i + 1
    } else {
      colnumbers <- unlist(lapply(this,
                          IPDFileCheck::get_colno_pattern_colname, names))
      i <- length(words_set) + 1
    }
  }
  # no matching columns
  if (sum(colnumbers == 0) == 1)
    stop("No matching columns")
  # should match all 5 columns for EQ5D
  if (sum(colnumbers > 0) != 5) {
    stop("Need to match 5 columns")
  }
  # get the codes - the entries
  index <- which(colnumbers > 0)
  this_name <- names[colnumbers[index]]
  codes <- 0
  for (j in seq_len(length(this_name))) {
    this_ind <- this_name[j]
    this_codes <- (unique(trialdata[[this_ind]]))
    codes <- append(codes, this_codes)
  }
  codes <- (unique(codes[-1]))
  result <- list(name = this_name, codes = codes)
  return(result)
}
#######################################################################
# 3 Miscellaneous
#######################################################################
# 3.1 Keep the column name, coded values and non response code into a dataframe
#' Function to keep the column name, coded values and non response code
#' into a dataframe
#' @param variable, name of the variable in the column
#' @param name, column name
#' @param code, coded values
#' @param nrcode, code for non response
#' @return data frame with all the above information
#' @examples
#' get_colnames_codedvalues("arm", "pat_trial_arm", c("Y", "N"))
#' @export
get_colnames_codedvalues <- function(variable, name, code, nrcode = NA) {
  if (is.null(name)   | is.null(variable)) {
    stop("column name or coded values can not be NULL")
  }else{
      if (is.na(name) | sum(is.na(code) != 0) | is.na(variable)) {
        stop("column name or coded values can not be NA")
      }else{
            colname <- name
            nrcode <- nrcode
            if (is.null(code)) {
              df <- data.frame(c(variable, colname, nrcode),
                               stringsAsFactors = FALSE)
              the_names <- (c("variable", "colname", "nonrescode"))
            } else {
              coded_values <- code
              lizt <- seq(1, length(coded_values))
              coded_value_names <- sapply(lizt, paste0, "_coded_value")
              df <- data.frame(c(variable, colname, coded_values, nrcode),
                               stringsAsFactors = FALSE)
              the_names <- (c("variable", "colname", unlist(coded_value_names),
                              "nonrescode"))
            }
            df  <- as.data.frame(t(df))
            colnames(df) <- the_names
            rownames(df) <- NULL
            return((df))
      }
  }
}
#######################################################################
#' Function to return treatment arm
#' @param arm the arm of the trial
#' @return 0, if success -1, if failure
#' @examples
#' check_treatment_arm("control")
#' @export
check_treatment_arm <- function(arm) {
  allcaps <- toupper(arm)
  if (allcaps == "CONTROL" || allcaps == "INTERVENTION") {
    return(0)
  } else {
    stop("Error specifying the treatment arm")
  }
}
#######################################################################
