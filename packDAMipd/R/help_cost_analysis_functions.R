##############################################################################
#' Function to get the subset of data compared to a string after
#' trimming the white spaces
#' @param col the form of medication either tablet or patch
#' @param the_data the data to be get the subset from
#' @param the_str the string to be compared
#' @return the subset data
#' @examples
#' the_data <- as.data.frame(cbind(c("one", "two"), c("a", "b")))
#' colnames(the_data) <- c("name", "brand")
#' ans <- return_equal_str_col(2, the_data, "a")
#' @export
return_equal_str_col <- function(col, the_data, the_str) {
  the_col <- trimws(toupper(the_data[[col]]))
  compare <- trimws(toupper(the_str))
  temp <- the_data[the_col == compare & !is.na(the_col), ]
  return(temp)
}
##############################################################################
#' Function to get the subset of data compared to a string after
#' trimming the white spaces
#' @param col the form of medication either tablet or patch
#' @param the_data the data to be get the subset from
#' @param list_str list of strings to be compared
#' @return the subset data
#' @examples
#' the_data <- as.data.frame(cbind(c("one", "two"), c("a", "b")))
#' colnames(the_data) <- c("name", "brand")
#' ans <- return_equal_liststring_col(2, the_data, c("a", "cc"))
#' @export
return_equal_liststring_col <- function(col, the_data, list_str) {
  the_col <- unique(trimws(toupper(the_data[[col]])))
  return_data <- c()
  for (i in seq_len(length(the_col))) {
     index <- which(the_col[i] %in%  trimws(toupper(list_str)))
     if (length(index) >= 1) {
        temp <- the_data[trimws(toupper(the_data[[col]])) == the_col[i], ]
        return_data <- rbind(return_data, temp)
     }
  }
  return(return_data)
}
##############################################################################
#' Function to get the subset of data compared to a string after
#' trimming the white spaces
#' @param col the form of medication either tablet or patch
#' @param the_data the data to be get the subset from
#' @param list_str list of strings to be compared
#' @return the subset data
#' @examples
#' the_data <- as.data.frame(cbind(c("one", "two"), c("tablet", "tablets"),
#' c("aa", "bb")))
#' colnames(the_data) <- c("name", "brand_a", "xx")
#' ans <- return_equal_liststring_listcol(2, the_data, c("tablet", "tablets"))
#' @export
return_equal_liststring_listcol <- function(col, the_data, list_str) {
  col_temp <- unique(trimws(toupper(the_data[[col]])))
  strings <- trimws(toupper(list_str))
  i <- 1
  indices <- c()
  temp_a <- data.frame()
  while (i <= length(col_temp)) {
    index <- which(col_temp[i] == strings)
    if (length(index) != 0) {
      row <- the_data[which(trimws(toupper(the_data[[col]])) == strings[index]), ]
      temp_a <- rbind(row, temp_a)
      indices <- append(indices, index)
    }
    i <- i + 1
  }
  if (length(indices) == 0) {
    stop("Matching columns cant be found")
  }
  return(temp_a)
}

##############################################################################
#' Function to return 0 if the param is not null or NA
#' trimming the white spaces
#' @param param the form of medication either tablet or patch
#' @return zero or -1
#' @examples
#' parame = NULL
#' ans <- return0_if_not_null_na(parame)
#' parame = 1
#' ans <- return0_if_not_null_na(parame)
#' @export
return0_if_not_null_na <- function(param) {
  if (!is.null(param)) {
    if (is.na(param)) val <- -1
    else val <- 0
  } else {
    val <- -1
  }
  return(val)
}
##############################################################################
#' Function to get cols for the pattern given
#' @param pattern the pattern to look for
#' @param the_data data where to look at
#' @return zero or -1
#' @examples
#' the_data <- as.data.frame(cbind(c("one", "two"), c("a", "b"), c("aa", "bb")))
#' colnames(the_data) <- c("name", "brand_one", "two")
#' get_single_col_multiple_pattern(c("brand", "trade"), the_data)
#' @export
get_single_col_multiple_pattern <- function(pattern, the_data) {
  res <- unlist(lapply(pattern,
              IPDFileCheck::get_colno_pattern_colname, colnames(the_data)))
  if (length(res[which(res != -1)]) == 1) {
    col_no <- res[which(res != -1)]
  } else {
    stop("Error- cols with pattern not found")
  }
  return(col_no)
}
#############################################################################
#' Function to get the weight and time units
#' @return weight and time units
#' @examples
#' ans <- generate_wt_time_units()
#' @export
generate_wt_time_units <- function() {
  weight_units <- c("mg", "milligram", "milli gram",
                 "gm", "g", "gram", "microgm",
                 "mcg", "micro gram", "microgram", "micro gm")
  time_units <- c("sec", "s", "second", "seconds",
               "minute", "min", "m", "minutes",
               "hour", "hr", "h", "hours",
               "d", "day", "days")
  weight_per_times <- list()

  for (i in seq_len(length(weight_units))) {
    for (j in seq_len(length(time_units))) {
      this_one <- paste(weight_units[i], "/", time_units[j], sep = "")
      weight_per_times <- append(weight_per_times, this_one)
    }
  }
  the_list <- structure(list
    (weight_units = weight_units,
      time_units = time_units,
      weight_per_times = weight_per_times))
  return(the_list)
}
#############################################################################
#' Function to get the weight and volume units
#' @return weight and vol units
#' @examples
#' ans <- generate_wt_vol_units()
#' @export
generate_wt_vol_units <- function() {
  weight_units <- c("mg", "milligram", "milli gram",
                   "gm", "g", "gram",
                   "mcg", "micro gram", "microgram")
  vol_units <- c("ml", "milii liter", "l", "liter",
                 "milliliter", "litre", "milii litre", "millilitre")
  weight_per_vol <- list()

  for (i in seq_len(length(weight_units))) {
    for (j in seq_len(length(vol_units))) {
      this_one <- paste(weight_units[i], "/", vol_units[j], sep = "")
      weight_per_vol <- append(weight_per_vol, this_one)
    }
  }
  the_list <- structure(list
                        (weight_units = weight_units,
                          vol_units = vol_units,
                          weight_per_vol = weight_per_vol))
  return(the_list)
}

#############################################################################
#' Function to get the codes and the corresponding entries
#' @param list_code_values list of codes and values, given as list of lists
#' @param data_column_nos the column numbers of data to look for the entries
#' @param the_data the data where to look for
#' @return weight and vol units
#' @examples
#' data_file <- system.file("extdata", "medication_liq_codes.xlsx",
#' package = "packDAMipd")
#' ind_part_data <- load_trial_data(data_file)
#' data_column_nos = c(2,12)
#' list_of_code_names = list(c(1, 2),c("Morphine", "Oxycodone"))
#' encode_codes_data(list_of_code_names, data_column_nos, ind_part_data)
#' @export
encode_codes_data <- function(list_code_values, data_column_nos, the_data) {
  if (!is.null(list_code_values) & sum(is.na(list_code_values)) == 0) {

    h <- hash::hash(key = unlist(list_code_values[1]),
                    values = unlist(list_code_values[2]))
    leys <- h$key
    vals <- h$values
    ipd_codes <- as.data.frame(the_data %>%
                              dplyr::select(dplyr::all_of(data_column_nos)))
    this_dim <- dim(ipd_codes)
    values_from_code <- c()

    for (i in 1:this_dim[1]) {
      for (j in 1:this_dim[2]) {
          if (!is.na(ipd_codes[i, j])) {
            this_val <- vals[leys == ipd_codes[i, j]]
          } else {
            this_val <- NA
          }
          values_from_code <- append(values_from_code, this_val)
      }
    }
    values_from_code <- matrix(values_from_code, nrow = this_dim[1],
                               byrow = TRUE)
    colnames(values_from_code) <- colnames(ipd_codes)
    index <- which(is.na(ipd_codes))
    if (length(index) > 0)
      values_from_code[index] <- NA
  } else {
    values_from_code <- the_data %>%
      dplyr::select(dplyr::all_of(data_column_nos))
    values_from_code <- as.data.frame(values_from_code)
  }
  return(values_from_code)
}
##############################################################################
#' Function to check the variable null or NA
#' @param word word for the number
#' @return return the number
#' @details
#' https://stackoverflow.com/questions/18332463/convert-written-number-to-number-in-r
#' @examples
#' answer <- word2num("one forty one")
#' answer <- word2num("forty one and five hundred")
#' answer <- word2num("five thousand two hundred and eight")
#' @export
word2num <- function(word) {
  wsplit <- strsplit(tolower(word), " ")[[1]]
  one_digits <- list(zero = 0, one = 1, two = 2, three = 3, four = 4, five = 5,
                     six = 6, seven = 7, eight = 8, nine = 9)
  teens <- list(eleven = 11, twelve = 12, thirteen = 13, fourteen = 14,
                fifteen = 15,
                sixteen = 16, seventeen = 17, eighteen = 18, nineteen = 19)
  ten_digits <- list(ten = 10, twenty = 20, thirty = 30, forty = 40,
                     fifty = 50,
                     sixty = 60, seventy = 70, eighty = 80, ninety = 90)
  doubles <- c(teens, ten_digits)
  out <- 0
  i <- 1
  while (i <= length(wsplit)) {
    if (i != 1 && wsplit[i] == "and")
      i <- i + 1
    j <- 1
    if (i == 1 && wsplit[i] == "hundred")
      temp <- 100
    else if (i == 1 && wsplit[i] == "thousand")
      temp <- 1000
    else if (wsplit[i] %in% names(one_digits))
      temp <- as.numeric(one_digits[wsplit[i]])
    else if (wsplit[i] %in% names(teens))
      temp <- as.numeric(teens[wsplit[i]])
    else if (wsplit[i] %in% names(ten_digits))
      temp <- (as.numeric(ten_digits[wsplit[i]]))
    if (i < length(wsplit) && wsplit[i + 1] == "hundred") {
      if (i > 1 && wsplit[i - 1] %in% c("hundred", "thousand"))
        out <- out + 100 * temp
      else
        out <- 100 * (out + temp)
      j <- 2
    }
    else if (i < length(wsplit) && wsplit[i + 1] == "thousand") {
      if (i > 1 && wsplit[i - 1] %in% c("hundred", "thousand"))
        out <- out + 1000 * temp
      else
        out <- 1000 * (out + temp)
      j <- 2
    }
    else if (i < length(wsplit) && wsplit[i + 1] %in% names(doubles)) {
      temp <- temp * 100
      out <- out + temp
    }
    else{
      out <- out + temp
    }
    i <- i + j
  }
  return(list(word, out))
}
#######################################################################
#' Convert frequency medication to given basis
#' @param freq_given given frequency
#' @param basis given basis, default is daily
#' @return converted frequency
#' @examples
#' convert_freq_diff_basis("once daily")
#' convert_freq_diff_basis("bd", "week")
#' convert_freq_diff_basis("Every 4 days", "day")
#' @export
#' @importFrom rlang is_empty
convert_freq_diff_basis <- function(freq_given, basis = "day") {
  freq_req_basis <- NA
  if (is.null(freq_given) | is.null(basis))
    stop("Error given unit/ basis is null")
  freq_given <- trimws(tolower(freq_given))
  if (basis != "day" & basis != "hour" & basis != "week" &
      basis != "month" & basis != "year" & basis != "hr" &
      basis != "days" & basis != "months" & basis != "weeks" &
      basis != "years")
    stop("Error - basis unit of time has to be hour, day, week,
         month or year")
  if (rlang::is_empty(freq_given) |
      any(is.na(freq_given)) |
      is.null(freq_given) |
      any(freq_given == "null") |
      any(freq_given == "Null")) {
    freq_req_basis <- NA
  } else {
    if (freq_given == "once in a day" |
        freq_given == "once a day" |
        freq_given == "daily" |
        freq_given == "once daily" |
        freq_given == "one in a day" |
        freq_given == "one a day" |
        freq_given == "one daily") {
      freq_req_basis <- 1
    } else {
      if (freq_given == "twice in a day" |
          freq_given == "twice a day" |
          freq_given == "twice daily" |
          freq_given == "bd" |
          freq_given == "b.d" |
          freq_given == "two in a day" |
          freq_given == "two a day" |
          freq_given == "two daily" |
          freq_given == "b.i.d" |
          freq_given == "bid") {
        freq_req_basis <- 2
      } else {
        if (freq_given == "thrice in a day" |
            freq_given == "thrice a day" |
            freq_given == "thrice daily" |
            freq_given == "tds" |
            freq_given == "t.d.s" |
            freq_given == "three in a day" |
            freq_given == "three a day" |
            freq_given == "three daily" |
            freq_given == "tid" |
            freq_given == "t.i.d" |
            freq_given == "three times a day" |
            freq_given == "three times in a day" |
            freq_given == "three times daily") {
          freq_req_basis <- 3
        } else {
          if (freq_given == "four in a day" |
              freq_given == "four a day" |
              freq_given == "four daily"
              | freq_given == "four times a day" |
              freq_given == "four times in a day" |
              freq_given == "four times daily") {
            freq_req_basis <- 4
          } else {
            if (freq_given == "five in a day" |
                freq_given == "five a day" |
                freq_given == "five daily"
                | freq_given == "five times a day" |
                freq_given == "five times in a day" |
                freq_given == "five times daily") {
              freq_req_basis <- 5
            } else {
              if (freq_given == "six in a day" |
                  freq_given == "six a day" |
                  freq_given == "six daily"
                  | freq_given == "six times a day" |
                  freq_given == "six times in a day" |
                  freq_given == "six times daily") {
                freq_req_basis <- 6
              } else {
                if (freq_given == "seven in a day" |
                    freq_given == "seven a day" |
                    freq_given == "seven daily"
                    | freq_given == "seven times a day" |
                    freq_given == "seven times in a day" |
                    freq_given == "seven times daily") {
                  freq_req_basis <- 7
                } else {
                  if (freq_given == "eight in a day" |
                      freq_given == "eight a day" |
                      freq_given == "eight daily"
                      | freq_given == "eight times a day" |
                      freq_given == "eight times in a day" |
                      freq_given == "eight times daily") {
                    freq_req_basis <- 8
                  } else {
                    if (freq_given == "nine in a day" |
                        freq_given == "nine a day" |
                        freq_given == "nine daily"
                        | freq_given == "nine times a day" |
                        freq_given == "nine times in a day" |
                        freq_given == "nine times daily") {
                      freq_req_basis <- 9
                    } else {
                      if (freq_given == "ten in a day" |
                          freq_given == "ten a day" |
                          freq_given == "ten daily"
                          | freq_given == "ten times a day" |
                          freq_given == "ten times in a day" |
                          freq_given == "ten times daily") {
                        freq_req_basis <- 10
                      } else {
                        if (freq_given == "eleven in a day" |
                            freq_given == "eleven a day" |
                            freq_given == "eleven daily"
                            | freq_given == "eleven times a day" |
                            freq_given == "eleven times in a day" |
                            freq_given == "eleven times daily") {
                          freq_req_basis <- 11
                        } else {
                          if (freq_given == "twelve in a day" |
                              freq_given == "twelve a day" |
                              freq_given == "twelve daily"
                              | freq_given == "twelve times a day" |
                              freq_given == "twelve times in a day" |
                              freq_given == "twelve times daily") {
                            freq_req_basis <- 12
                          } else {
                            if (freq_given == "once in every 2 days" |
                                freq_given == "once every 2 days" |
                                freq_given == "every 2 days" |
                                freq_given == "every 2 days") {
                              freq_req_basis <- 0.5
                            } else {
                              if (freq_given == "once in every 3 days" |
                                  freq_given == "once every 3 days" |
                                  freq_given == "every 3 days" |
                                  freq_given == "every 3 days") {
                                freq_req_basis <- 1 / 3
                              } else {
                                if (freq_given == "once in every 4 days" |
                                    freq_given == "once every 4 days" |
                                    freq_given == "every 4 days" |
                                    freq_given == "every 4 days") {
                                  freq_req_basis <- 1 / 4
                                } else {
                                  if (freq_given == "once in every 5 days" |
                                      freq_given == "once every 5 days" |
                                      freq_given == "every 5 days" |
                                      freq_given == "every 5 days") {
                                    freq_req_basis <- 1 / 5
                                  } else {
                                    if (freq_given == "once in every 6 days" |
                                        freq_given == "once every 6 days" |
                                        freq_given == "every 6 days" |
                                        freq_given == "every 6 days") {
                                      freq_req_basis <- 1 / 6
                                    } else {
                                      if (freq_given == "once in every 7 days" |
                                          freq_given == "once every 7 days" |
                                          freq_given == "every 7 days" |
                                          freq_given == "every 7 days") {
                                        freq_req_basis <- 1 / 7
                                      } else {
                                        if (freq_given == "once a week" |
                                            freq_given == "once weekly" |
                                            freq_given == "once in a week" |
                                            freq_given == "once in week" |
                                            freq_given == "weekly once" |
                                            freq_given == "every week") {
                                          freq_req_basis <- 1 / 7
                                        } else {
                                          if (freq_given == "twice a week" |
                                              freq_given == "twice in a week" |
                                              freq_given == "two times in a week" |
                                              freq_given == "weekly twice" |
                                              freq_given == "weekly two times" |
                                              freq_given == "twice weekly" |
                                              freq_given == "two times weekly") {
                                            freq_req_basis <- 2 / 7
                                          } else {
                                            if (freq_given == "thrice in a week" |
                                                freq_given == "three times in a week" |
                                                freq_given == "thrice a week" |
                                                freq_given == "three times in week" |
                                                freq_given == "weekly thrice" |
                                                freq_given == "weekly three times" |
                                                freq_given == "thrice weekly" |
                                                freq_given == "three times weekly") {
                                              freq_req_basis <- 3 / 7
                                            } else {
                                              if (freq_given == "four times in a week" |
                                                  freq_given == "four times in week" |
                                                  freq_given == "four times a week" |
                                                  freq_given == "weekly four times" |
                                                  freq_given == "four times a week" |
                                                  freq_given == " four times weekly") {
                                                freq_req_basis <- 4 / 7
                                              } else {
                                                if (freq_given == "five times in a week" |
                                                    freq_given == "five times in week" |
                                                    freq_given == "five times a week" |
                                                    freq_given == "weekly five times" |
                                                    freq_given == "five times weekly ") {
                                                  freq_req_basis <- 5 / 7
                                                } else {
                                                  if (freq_given == "six times in a week" |
                                                      freq_given == "six times in week" |
                                                      freq_given == "six times a week" |
                                                      freq_given == "weekly six times" |
                                                      freq_given == "six times weekly") {
                                                    freq_req_basis <- 6 / 7
                                                  } else {
                                                    if (freq_given == "seven times in a week" |
                                                        freq_given == "seven times in week" |
                                                        freq_given == "seven times a week" |
                                                        freq_given == "weekly seven times" |
                                                        freq_given == "seven times weekly ") {
                                                      freq_req_basis <- 1
                                                    } else {
                                                      if (freq_given == "every 1 hour " |
                                                          freq_given == "every one hour"
                                                          | freq_given == "every hour" |
                                                          freq_given == "once hourly"
                                                          | freq_given == "hourly once" |
                                                          freq_given == "hourly one time") {
                                                        freq_req_basis <- 24
                                                      } else {
                                                        if (freq_given == "every 2 hour" |
                                                            freq_given == "every two hour" |
                                                            freq_given == "every 2 hours" |
                                                            freq_given == "every two hours") {
                                                          freq_req_basis <- 12
                                                        } else {
                                                          if (freq_given == "every 3 hour" |
                                                              freq_given == "every three hour" |
                                                              freq_given == "every 3 hours" |
                                                              freq_given == "every three hours") {
                                                            freq_req_basis <- 8
                                                          } else {
                                                            if (freq_given == "every 4 hour" |
                                                                freq_given == "every four hour" |
                                                                freq_given == "every 4 hours" |
                                                                freq_given == "every four hours") {
                                                              freq_req_basis <- 6
                                                            } else {
                                                              if (freq_given == "every 6 hour" | freq_given == "every six hour" |
                                                                  freq_given == "every 6 hours" | freq_given == "every six hours") {
                                                                freq_req_basis <- 4
                                                              } else {
                                                                if (freq_given == "every 12 hour" |
                                                                    freq_given == "every twelve hour" |
                                                                    freq_given == "every 12 hours" |
                                                                    freq_given == "every twelve hours") {
                                                                  freq_req_basis <- 2
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  basis <- tolower(basis)
  if (is.null(freq_req_basis))
    stop("Given frequency is not identifiable ")
  if ((basis == "hour" | basis == "hr" | basis == "hours"  | basis == "hrs") &
      !is.na(freq_req_basis)) {
    freq_req_basis <- freq_req_basis / 24
  }
  if ((basis == "week" | basis == "weeks") & !is.na(freq_req_basis)) {
    freq_req_basis <- freq_req_basis * 7
  }
  if ((basis == "month" | basis == "months") & !is.na(freq_req_basis)) {
    freq_req_basis <- freq_req_basis * 30.44
  }
  if ((basis == "year" | basis == "year") & !is.na(freq_req_basis)) {
    freq_req_basis <- freq_req_basis * 365.25
  }
  return(freq_req_basis)
}
##################################################
#' Convert unit strength to given basis
#' @param given_unit given unit
#' @param basis given basis, default is "mg"
#' @return converted unit
#' @examples
#' convert_weight_diff_basis("mg")
#' convert_weight_diff_basis("kilogram", "micro gram")
#' @export
convert_weight_diff_basis <- function(given_unit, basis = "mg") {
  unit_req_basis <- NA
  if (is.null(given_unit) | is.null(basis))
    stop("Error given unit/ basis is null")
  given_unit <- trimws(tolower(given_unit))

  nospace_unit <- tolower(gsub("[[:space:]]", "", given_unit))
  basis <- tolower(gsub("[[:space:]]", "", basis))
  if (nospace_unit != "g" & nospace_unit != "gram"  &
      nospace_unit != "milligram" &
      nospace_unit != "gm"  & nospace_unit != "mg" &
      nospace_unit != "microgram"  &
      nospace_unit != "microgm" &
      nospace_unit != "mcg" &
      nospace_unit != "kilogram"  & nospace_unit != "kilo"  &
      nospace_unit != "kg") {
    stop("given unit is not of weight")
  }

  if (basis == "mcg" | basis == "microgram"
        | basis == "microgm") {
      if (nospace_unit == "mg" | nospace_unit == "milligram") {
        unit_req_basis <- 1e3
      }
      if (nospace_unit == "g" | nospace_unit == "gm"
          | nospace_unit == "gram") {
        unit_req_basis <- 1e6
      }
      if (nospace_unit == "kg" | nospace_unit == "kilo"
          | nospace_unit == "kilogram") {
        unit_req_basis <- 1e9
      }
      if (nospace_unit == "mcg" | nospace_unit == "microgram"
          | nospace_unit == "microgm") {
        unit_req_basis <- 1
      }
    }
  if (basis == "mg" | basis == "milligram") {
      if (nospace_unit == "mg" | nospace_unit == "milligram") {
        unit_req_basis <- 1
      }
      if (nospace_unit == "g" | nospace_unit == "gm"
          | nospace_unit == "gram") {
        unit_req_basis <- 1000
      }
      if (nospace_unit == "kg" | nospace_unit == "kilo"
          | nospace_unit == "kilogram") {
        unit_req_basis <- 1e6
      }
      if (nospace_unit == "mcg" | nospace_unit == "microgram"
          | nospace_unit == "microgm") {
        unit_req_basis <- 0.001
      }
    }
  if (basis == "gram" | basis == "gm" | basis == "g") {
      if (nospace_unit == "mg" | nospace_unit == "milligram") {
        unit_req_basis <- 1e-3
      }
      if (nospace_unit == "g" | nospace_unit == "gm"
          | nospace_unit == "gram") {
        unit_req_basis <- 1
      }
      if (nospace_unit == "kg" | nospace_unit == "kilo"
          | nospace_unit == "kilogram") {
        unit_req_basis <- 1e3
      }
      if (nospace_unit == "mcg" | nospace_unit == "microgram"
          | nospace_unit == "microgm") {
        unit_req_basis <- 1e-6
      }
    }
  if (basis == "kg" | basis == "kilo"
        | basis == "kilogram") {
      if (nospace_unit == "mg" | nospace_unit == "milligram") {
        unit_req_basis <- 1e-6
      }
      if (nospace_unit == "g" | nospace_unit == "gm"
          | nospace_unit == "gram") {
        unit_req_basis <- 1e-3
      }
      if (nospace_unit == "kg" | nospace_unit == "kilo"
          | nospace_unit == "kilogram") {
        unit_req_basis <- 1
      }
      if (nospace_unit == "mcg" | nospace_unit == "microgram"
          | nospace_unit == "microgm") {
        unit_req_basis <- 1e-9
      }
    }
  return(unit_req_basis)
}

#######################################################################
#' Convert volume  to given basis
#' @param given_unit given unit
#' @param basis given basis, default is "ml"
#' @return converted unit
#' @examples
#' convert_volume_basis("ml", "liter")
#' @export
convert_volume_basis <- function(given_unit, basis = "ml") {
  if (is.null(given_unit) | is.null(basis))
    stop("Error given unit/ basis is null")
  given_unit <- trimws(tolower(given_unit))
  unit_req_vol <- NA
  given_unit <- tolower(gsub("[[:space:]]", "", given_unit))
  basis <- tolower(gsub("[[:space:]]", "", basis))
  if (given_unit != "liter"  & given_unit != "litre"  & given_unit != "l" &
      given_unit != "milliliter" & given_unit != "milli liter" &
      given_unit != "millilitre" & given_unit != "milli litre" &
      given_unit != "ml" &
      given_unit != "microliter" & given_unit != "micro liter" &
      given_unit != "microlitre" & given_unit != "micro litre" &
      given_unit != "mcl") {
    stop("given unit is not of volume")
  }
    unit_req_vol <- 1
    if (basis == "ml" | basis == "milliliter" |
        basis == "milli liter" | basis == "milli litre" |
        basis == "millilitre") {
      if (given_unit == "microliter" | given_unit == "micro liter" |
          given_unit == "micro litre" | given_unit == "microlitre" |
          given_unit == "mcl") {
        unit_req_vol <- 1 / 1000
      }
      if (given_unit == "l" | given_unit == "liter" | given_unit == "litre") {
        unit_req_vol <- 1000
      }
    }
    if (basis == "microliter" | basis == "micro liter" |
        basis == "micro litre" | basis == "microlitre" |
        basis == "mcl") {
      if (given_unit == "l" | given_unit == "liter" | given_unit == "litre") {
        unit_req_vol <- 1e6
      }
      if (given_unit == "ml" | given_unit == "milliliter" |
          given_unit == "milli liter" | given_unit == "milli litre" |
          given_unit == "millilitre") {
        unit_req_vol <- 1000
      }
    }
    if (basis == "l" | basis == "liter" | given_unit == "litre") {
      if (given_unit == "ml" |
          given_unit == "milliliter" | given_unit == "milli liter" |
          given_unit == "milli litre" |
          given_unit == "millilitre") {
        unit_req_vol <- 1 / 1000
      }

      if (given_unit == "microliter" | given_unit == "micro liter" |
          given_unit == "micro litre" | given_unit == "microlitre" |
          given_unit == "mcl") {
        unit_req_vol <- 1 / 1e6
      }
    }
    return(unit_req_vol)
}
#######################################################################
#' Convert weight per time  to given basis
#' @param given_unit given unit
#' @param basis given basis, default is "mg"
#' @return converted unit
#' @examples
#' convert_wtpertimediff_basis("mg/day")
#' convert_wtpertimediff_basis("mcg/day")
#' convert_wtpertimediff_basis("mg/hour")
#' @export
convert_wtpertimediff_basis <- function(given_unit, basis = "mcg/hour") {
  unit_req_basis <- NA
  if (is.null(given_unit) | is.null(basis))
    stop("Error given unit/ basis is null")
  given_unit <- trimws(tolower(given_unit))
  if (rlang::is_empty(given_unit) | any(is.na(given_unit)) |
      length(given_unit) == 0 |
      identical(given_unit, "")
      | any(given_unit == "null") | any(given_unit == "Null")) {
    unit_req_basis <- NA
  } else {
        unit_req_basis <- NULL
        index <- stringr::str_locate(given_unit, "/")
        if (sum(is.na(index)) != 0) {
          stop("given unit should be in the form wt/time")
        }
        given_wt <- stringr::str_sub(given_unit, 1, index[1] - 1)
        given_time <- stringr::str_sub(given_unit, index[2] + 1,
                                         nchar(given_unit))
        basis_index <- stringr::str_locate(basis, "/")
        if (sum(is.na(basis_index)) != 0) {
          stop("basis unit should be in the form wt/time")
        }
        basis_wt <- stringr::str_sub(basis, 1, basis_index[1] - 1)
        basis_time <- stringr::str_sub(basis, basis_index[2] + 1, nchar(basis))
        basis_wt <- tolower(gsub("[[:space:]]", "", basis_wt))
        basis_time <- tolower(gsub("[[:space:]]", "", basis_time))
        unit_req_wt <- NULL
        unit_req_time <- NULL
        if (basis_wt == "mg" | basis_wt == "milligram" |
              basis_wt == "milli gram") {
            if (given_wt == "mcg" | given_wt == "microgram"  |
                given_wt == "micro gram") {
              unit_req_wt <- 1 / 1000
            }
            if (given_wt == "gram" | given_wt == "gm") {
              unit_req_wt <- 1000
            }
            if (given_wt == "mg" | given_wt == "milligram" |
                given_wt == "milli gram") {
              unit_req_wt <- 1
            }
        }
        if (basis_wt == "mcg" | basis_wt == "microgram" |
              basis_wt == "micro gram") {
            if (given_wt == "mg" | given_wt == "milligram" |
                given_wt == "milli gram") {
              unit_req_wt <- 1000
            }
            if (given_wt == "gram" | given_wt == "gm") {
              unit_req_wt <- 1e6
            }
            if (given_wt == "mcg" | given_wt == "microgram"  |
                given_wt == "micro gram") {
              unit_req_wt <- 1
            }
          }
        if (basis_wt == "gram" | basis_wt == "gm") {
            if (given_wt == "mg" |  given_wt == "milligram" |
                given_wt == "milli gram") {
              unit_req_wt <- 1 / 1000
            }
            if (given_wt == "mcg" |  given_wt == "microgram" |
                given_wt == "micro gram") {
              unit_req_wt <- 1 / 1e6
            }
            if (given_wt == "gram" |  given_wt == "gm") {
              unit_req_wt <- 1
            }
         }
        if (basis_time == "day" | basis_time == "d") {
            if (given_time == "hour" | given_time == "hr") {
              unit_req_time <- 1 / 24
            }
            if (given_time == "sec" | given_time == "second") {
              unit_req_time <- 1 / (24 * 3600)
            }
            if (given_time == "minute" | given_time == "min") {
              unit_req_time <- 1 / (24 * 60)
            }
            if (given_time == "day" | given_time == "d") {
              unit_req_time <- 1
            }
        }
        if (basis_time == "hour" | basis_time == "hr") {
            if (given_time == "day" | given_time == "d") {
              unit_req_time <- 24
            }
            if (given_time == "sec" | given_time == "second") {
              unit_req_time <- 1 / 3600
            }
            if (given_time == "minute" | given_time == "min") {
              unit_req_time <- 1 / 60
            }
            if (given_time == "hour" | given_time == "hr") {
              unit_req_time <- 1
            }
        }
        if (basis_time == "minute" | basis_time == "min") {
            if (given_time == "day" | given_time == "d") {
              unit_req_time <- 24 * 60
            }
            if (given_time == "sec" | given_time == "second") {
              unit_req_time <- 1 / 60
            }
            if (given_time == "hour" | given_time == "hr") {
              unit_req_time <- 60
            }
            if (given_time == "minute" | given_time == "min") {
              unit_req_time <- 1
            }
        }
        if (basis_time == "sec" | basis_time == "second") {
            if (given_time == "day" | given_time == "d") {
              unit_req_time <- 24 * 3600
            }
            if (given_time == "minute" | given_time == "min") {
              unit_req_time <- 60
            }
            if (given_time == "hour" | given_time == "hr") {
              unit_req_time <- 3600
            }
            if (given_time == "sec" | given_time == "second") {
              unit_req_time <- 1
            }
        }
        if (!is.null(unit_req_wt) & !is.null(unit_req_time)) {
            unit_req_basis <- unit_req_wt / unit_req_time
        }
  }
  return(unit_req_basis)
}
#######################################################################
#' Convert period to given basis
#' @param given_time given time
#' @param basis_time given basis, default is "day"
#' @return converted unit
#' @examples
#' convert_to_given_timeperiod("4 weeks")
#' convert_to_given_timeperiod("a month")
#' convert_to_given_timeperiod("1 week")
#' @export
convert_to_given_timeperiod <- function(given_time, basis_time = "day") {
  if (is.null(given_time) | is.null(basis_time))
    stop("Error given unit is null")
  given_time <- trimws(tolower(given_time))
  unit_req_basis <- NA
  if (rlang::is_empty(given_time) | any(is.na(given_time)) |
      length(given_time) == 0 |
      identical(given_time, "")
      | any(given_time == "null") | any(given_time == "Null")) {
    unit_req_basis <- NA
  } else {
    index <- stringr::str_locate(given_time, " ")
    if (sum(is.na(index)) > 0)
      stop("Error - the time unit should have a space
           for eg. a week or 2 months")
    first_part <- stringr::str_sub(given_time, 1, index[1] - 1)
    result <- sum(is.na(suppressWarnings(as.numeric(first_part))))
    if (result > 0) {
      out <- tryCatch({
          as.numeric(unlist(word2num(first_part))[2])
        },
        error = function(cond) {
          if (first_part == "a" | first_part == "an" | first_part == "one") {
            out <- 1
          }
        }
      )
    } else {
      out <- as.numeric(first_part)
    }
    sec_part <- trimws(stringr::str_sub(given_time, index[2] + 1,
                                        nchar(given_time)))
    unit_req_time <- NULL
    if (basis_time == "day" | basis_time == "days") {
      if (sec_part == "day" | sec_part == "days") {
        unit_req_time <- 1
      }
      if (sec_part == "week" | sec_part == "weeks") {
        unit_req_time <- 7
      }
      if (sec_part == "month" | sec_part == "months") {
        unit_req_time <- 30.44
      }
      if (sec_part == "year" | sec_part == "years") {
        unit_req_time <- 365.25
      }
      if (sec_part == "hour" | sec_part == "hr" | sec_part == "hours") {
        unit_req_time <- 1 / 24
      }
      if (sec_part == "minute" | sec_part == "min" | sec_part == "minutes") {
        unit_req_time <- 1 / (24 * 60)
      }
      if (sec_part == "sec" | sec_part == "second" | sec_part == "seconds") {
        unit_req_time <- 1 / (24 * 60 * 60)
      }
    }
    if (basis_time == "month" | basis_time == "months") {
      if (sec_part == "month" | sec_part == "months") {
        unit_req_time <- 1
      }
      if (sec_part == "week" | sec_part == "weeks") {
        unit_req_time <- 1 / 4
      }
      if (sec_part == "day" | sec_part == "days") {
        unit_req_time <- 1 / 30.44
      }
      if (sec_part == "year" | sec_part == "years") {
        unit_req_time <- 12
      }
      if (sec_part == "hour" | sec_part == "hr" | sec_part == "hours") {
        unit_req_time <- 1 / (30.44 * 24)
      }
      if (sec_part == "minute" | sec_part == "min" | sec_part == "minutes") {
        unit_req_time <- 1 / (30.44 * 24 * 60)
      }
      if (sec_part == "sec" | sec_part == "second" | sec_part == "seconds") {
        unit_req_time <- 1 / (30.44 * 24 * 60 * 60)
      }
    }
    if (basis_time == "week" | basis_time == "weeks") {
      if (sec_part == "week" | sec_part == "weeks") {
        unit_req_time <- 1
      }
      if (sec_part == "month" | sec_part == "months") {
        unit_req_time <- 4
      }
      if (sec_part == "day" | sec_part == "days") {
        unit_req_time <- 1 / 7
      }
      if (sec_part == "year" | sec_part == "years") {
        unit_req_time <- 52.1429
      }
      if (sec_part == "hour" | sec_part == "hr" | sec_part == "hours") {
        unit_req_time <- 1 / (7 * 24)
      }
      if (sec_part == "minute" | sec_part == "min" | sec_part == "minutes") {
        unit_req_time <- 1 / (7 * 24 * 60)
      }
      if (sec_part == "sec" | sec_part == "second" | sec_part == "seconds") {
        unit_req_time <- 1 / (7 * 24 * 60 * 60)
      }
    }
    if (basis_time == "hour" | basis_time == "hr" | basis_time == "hours") {
      if (sec_part == "hour" | sec_part == "hr" | sec_part == "hours") {
        unit_req_time <- 1
      }
      if (sec_part == "month" | sec_part == "months") {
        unit_req_time <- 24 * 30.44
      }
      if (sec_part == "day" | sec_part == "days") {
        unit_req_time <- 24
      }
      if (sec_part == "year" | sec_part == "years") {
        unit_req_time <- 24 * 365.25
      }
      if (sec_part == "week" | sec_part == "weeks") {
        unit_req_time <- 7 * 24
      }
      if (sec_part == "minute" | sec_part == "min" | sec_part == "minutes") {
        unit_req_time <- 1 / (60)
      }
      if (sec_part == "sec" | sec_part == "second" | sec_part == "seconds") {
        unit_req_time <- 1 / (60 * 60)
      }
    }
    if (is.null(unit_req_time)) {
      stop("Error in converting the unit")
    }
    unit_req_basis <- out * unit_req_time
  }
  return(unit_req_basis)
}
########################################################################
#' Convert wt per unit volume  to given basis
#' @param given_unit given unit
#' @param basis given basis, default is "mg/ml"
#' @return converted unit
#' @examples
#' convert_wtpervoldiff_basis("g/ml")
#' @export
convert_wtpervoldiff_basis <- function(given_unit, basis = "mg/ml") {
  unit_req_basis <- NA
  if (is.null(given_unit) | is.null(basis))
    stop("Error given unit/ basis is null")
  given_unit <- trimws(tolower(given_unit))
  if (rlang::is_empty(given_unit) | any(is.na(given_unit)) |
      length(given_unit) == 0 |
      identical(given_unit, "")
      | any(given_unit == "null") | any(given_unit == "Null")) {
    unit_req_basis <- NA
  } else {
    unit_req_basis <- NULL
    index <- stringr::str_locate(given_unit, "/")
    given_wt <- stringr::str_sub(given_unit, 1, index[1] - 1)
    given_vol <- stringr::str_sub(given_unit, index[2] + 1,
                                   nchar(given_unit))
    basis_index <- stringr::str_locate(basis, "/")
    basis_wt <- stringr::str_sub(basis, 1, basis_index[1] - 1)
    basis_vol <- stringr::str_sub(basis, basis_index[2] + 1, nchar(basis))
    basis_wt <- tolower(gsub("[[:space:]]", "", basis_wt))
    basis_vol <- tolower(gsub("[[:space:]]", "", basis_vol))
  }
  multiply_wt <- convert_weight_diff_basis(given_wt, basis_wt)
  multiply_vol <- convert_volume_basis(given_vol, basis_vol)

  if (is.numeric(multiply_wt) & is.numeric(multiply_vol))
    unit_req_basis <- multiply_wt / multiply_vol
  else
    stop("Error converting wt per vol unit")
  return(unit_req_basis)
}
########################################################################
#' Convert the combined dose to its individual component numerical value
#' and units
#' @param the_string given combined unit
#' @param separator given character for separation , default is "/"
#' @return separated numerical value and its units
#' @examples
#' get_doses_combination_units("10g/2ml")
#' @export
get_doses_combination_units <- function(the_string, separator = "/"){
  whole_res <- c()
  for (i in 1:length(the_string)) {
    mix1 <- unlist(stringr::str_locate_all(the_string[i], separator))
    if (length(mix1) != 0) {
      firstdos <- substr(the_string[i], 1, mix1[1] - 1)
      secdos <- substr(the_string[i], mix1[1] + 1,
                       nchar(the_string[i]))
      firstdos_num <- gsub("[^0-9.-]", "", firstdos)
      seconddos_num <- gsub("[^0-9.-]", "", secdos)
      unit1 <- trimws(gsub("[0-9\\.]", "", firstdos))
      unit2 <- trimws(gsub("[0-9\\.]", "", secdos))
    }
    this_doses <- c(firstdos_num, unit1, seconddos_num, unit2)
    whole_res <- append(whole_res, this_doses)
  }
  whole_res <- as.data.frame(matrix(whole_res, ncol = 4, byrow = T))
  colnames(whole_res) <- c("num1", "unit1", "num2", "unit2")
  return(whole_res)
}
########################################################################
#' Convert the combined dose to its individual component numerical value
#' or can be unit/unit
#' @param the_string given combined unit
#' @param separator given character for separation , default is "/"
#' @return separated texts
#' @examples
#' get_doses_combination("g/ml")
#' @export
get_doses_combination <- function(the_string, separator = "/"){
  whole_res <- c()
  for (i in 1:length(the_string)) {
    mix1 <- unlist(stringr::str_locate_all(the_string[i], separator))
    if (length(mix1) != 0) {
      firstdos <- substr(the_string[i], 1, mix1[1] - 1)
      secdos <- substr(the_string[i], mix1[1] + 1,
                       nchar(the_string[i]))
    }
    this_doses <- c(firstdos, secdos)
    whole_res <- append(whole_res, this_doses)
  }
  whole_res <- as.data.frame(matrix(whole_res, ncol = 2, byrow = T))
  colnames(whole_res) <- c("first", "second")
  return(whole_res)
}
