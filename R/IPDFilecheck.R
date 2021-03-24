###############################################################################
#' Function to check the package is installed, if not install
#' @param pkg name of package(s)
#' @return 0, if packages cant be installed and loaded, else error
#' @examples
#' check_load_packages("dplyr")
#' @export
#' @importFrom methods is
check_load_packages <- function(pkg) {
  for (i in seq_len(length(pkg))) {
    x <- pkg[i]
    if (!suppressWarnings(require(x, character.only = TRUE))) {
      trythis <- tryCatch(install.packages(x, dependencies = TRUE,
                              repos = "http://cran.us.r-project.org"),
        error = function(e) {
          cat("Error ")
        },
        warning = function(e) {
          cat("Warning ")
        }
      )
      if (is(trythis, "warning")) {
        warning("Warning installing")
      }
      if (is(trythis, "error")) {
        warning("Error installing")
      }
      trythis <- tryCatch(require(x, character.only = TRUE),
        error = function(e) {
          warning("Error in adding to library ")
        },
        warning = function(e) {
          warning("Invalid package ")
        }
      )
    }
  }
  return(0)
}
###############################################################################
#' Function to test column names of a data being different from what specified
#' @param column_names column names of the data frame
#' @param data a data frame
#' @return 0, if success error, if failure
#' @examples
#' test_columnnames(c("name", "age"), data.frame(
#'   "Age" =  c(21, 15),
#'   "Name" =  c("John", "Dora")
#' ))
#' @export
test_columnnames <- function(column_names, data) {
  upper_given_colnames <- sort(toupper(column_names))
  upper_data_colnames <- sort(toupper(colnames(data)))
  if (sum(upper_given_colnames == upper_data_colnames) ==
      length(column_names)) {
    return(0)
  } else {
    return(-1)
  }
}
###############################################################################
#' Function to throw error on invalid directory or file and if not readable
#' @param filename name of a file or dir
#' @return 0, if success error, if failure
#' @examples
#' test_file_exist_read(system.file("extdata", "blank.txt",
#'   package = "IPDFileCheck"
#' ))
#' @export
test_file_exist_read <- function(filename) {
  if (file.exists(filename)) {
    if (file.access(filename, 0) != 0) {
      return(-1)
    }
    return(0)
  } else {
    return(-2)
  }
}
###############################################################################
#' Function to return the column number for column name
#' @param data a data frame
#' @param column_name column names of the data frame
#' @return column number, if success error, if failure
#' @examples
#' get_columnno_fornames(data.frame("Age" = c(21, 15),
#' "Name" = c("John", "Dora")), "Name")
#' @export
get_columnno_fornames <- function(data, column_name) {
  data_column_names <- toupper(colnames(data))
  if (any(data_column_names == toupper(column_name))) {
    column_no <- which(data_column_names == toupper(column_name))
    return(column_no)
  } else {
    stop("Column name does not exist")
  }
}
###############################################################################
#' Function to check the format of 'age' in data
#' @param data a data frame
#' @param agecolumn column name that corresponds to age or date pf birth
#' @param nrcode non response code corresponding to age column
#' @return 0, if success error if failure
#' @examples
#' df <- data.frame("Age" = c(21, 15), "Name" = c("John", "Dora"))
#' test_age(df, "age", 999)
#' @export
test_age <- function(data, agecolumn = "age", nrcode = NA) {
  column_no <- get_columnno_fornames(data, agecolumn)
    entry <- data[[column_no]]
    blanks <- c(which(entry == ""), which(is.na(entry)))
    if (length(blanks) != 0) {
      entry[blanks] <- nrcode
    }
    if (is.na(nrcode)) {
      this_entry <- entry[!is.na(entry)]
      this_entry_num <- suppressWarnings(as.numeric(this_entry))
      if (sum(is.na(this_entry_num)) == 0) {
        newentry <- as.numeric(this_entry)
        if (any(newentry > 150) || any(newentry < 0)) {
          return(-1)
        } else {
          return(0)
        }
      } else {
        return(-2)
      }
    } else {
      this_entry <- entry[entry != nrcode]
      this_entry_num <- suppressWarnings(as.numeric(this_entry))
      if (sum(is.na(this_entry_num)) == 0) {
        newentry <- as.numeric(this_entry)
        if (any(newentry > 150) || any(newentry < 0)) {
          return(-3)
        } else {
          return(0)
        }
      } else {
        return(-4)
      }
    }
}
###############################################################################
#' Function to check the format of 'gender' column in data
#' @param data a data frame
#' @param gendercolumn column name for gender
#' @param gendercode how gender is coded
#' @param nrcode non response code corresponding to gender column
#' @return 0, if success error if failure
#' @examples
#' test_gender(data.frame("sex" = c("m", "f"), "Name" = c("John", "Dora")),
#' c("f", "m"), "sex", 999)
#' @export
test_gender <- function(data, gendercode, gendercolumn = "gender",
                        nrcode = NA) {
  gendercode <- toupper(gendercode)
  if (get_columnno_fornames(data, gendercolumn) > 0) {
    column_no <- get_columnno_fornames(data, gendercolumn)
    entry <- data[column_no]
    if (is.na(nrcode)) {
      newentry <- entry[!is.na(entry)]
      gendercode <- gendercode[!is.na(gendercode)]
    } else {
      newentry <- entry[entry != nrcode, ]
      gendercode <- gendercode[which(gendercode != nrcode)]
    }
    facs <- levels(factor(toupper(newentry)))
    if (all(facs %in% gendercode)) {
      return(0)
    } else {
      return(-1)
    }
  }
}
###############################################################################
#' Function to get the actual value of column content if its coded
#' @param data a data frame
#' @param column column name for value
#' @param list_codes_values list of codes to understand the codes and value
#' @param nrcode non response code corresponding to gender column
#' @return 0, if success error if failure
#' @examples
#' data = data.frame("sex" = c(1, 2, 2, 1, 1),
#'  "Name" = c("John", "Dora","Dora", "John","John"))
#' list_codes_values = list(c("F", "M"),c(1,2))
#' ans <- get_value_from_codes(data, column = "sex", nrcode = NA,
#' list_codes_values)
#' @export
#' @importFrom dplyr %>%
get_value_from_codes <- function(data, column,
                              nrcode = NA, list_codes_values) {
     if (is.null(column)) {
       stop("Column name cant be null")
     } else {
       if (is.na(column))
         stop("Column name cant be NA")
     }
     if (is.null(data))
       stop("data cant be null")
     if (is.null(list_codes_values))
       stop("list_codes_values cant be null")
     if (get_columnno_fornames(data, column) > 0) {
        h <- hash::hash(key = unlist(list_codes_values[1]),
                 values = unlist(list_codes_values[2]))
        leys <- h$key
        vals <- h$values
        ipd_codes <- unlist(data %>% dplyr::select(dplyr::all_of(column)))
        this_values <- c()
        for (i in seq_len(length(ipd_codes))) {
          if (is.na(ipd_codes[i])) {
           this_val <- NA
          } else {
            this_val <- vals[leys == ipd_codes[i]]
          }
          this_values <- append(this_values, this_val)
        }
        return((this_values))
     }
}
###############################################################################
#' Function to check the format of column contents
#' @param data a data frame
#' @param column column name for gender
#' @param code how column values are  coded
#' @param nrcode non response code corresponding to gender column
#' @return 0, if success error if failure
#' @examples
#' test_column_contents(data.frame(
#'   "sex" = c("m", "f"),
#'   "Name" = c("John", "Dora")
#' ), "sex", c("m", "f"), 999)
#' @export
test_column_contents <- function(data, column, code, nrcode = NA) {
  column_no <- get_columnno_fornames(data, column)
    entry <- data[column_no]
    if (is.na(nrcode)) {
      newentry <- entry[!is.na(entry)]
      code <- code[!is.na(code)]
    } else {
      newentry <- entry[entry != nrcode, ]
      code <- code[which(code != nrcode)]
    }
    facs <- levels(factor(newentry))
    if (all(facs %in% code)) {
      return(0)
    } else {
      return(-2)
    }
}
###############################################################################
#' Function to check the format of a numeric column
#' @param column_name the column name
#' @param data data frame
#' @param nrcode non response code corresponding to the column
#' @param minval minimum value allowed
#' @param maxval maximum value allowed
#' @return 0, if success error, if failure
#' @examples
#' test_data_numeric("age", data.frame(
#'   "Age" =  c(21, 15),
#'   "Name"  = c("John", "Dora")
#' ), -99, 0, 100)
#' @export
test_data_numeric <- function(column_name, data, nrcode = NA, minval, maxval) {
  column_no <- get_columnno_fornames(data, column_name)
    entry <- (data[[column_no]])
    if (is.na(nrcode)) {
      new_entry <- (entry[which(!is.na(entry))])
    } else {
      new_entry <- (entry[which(entry != nrcode)])
    }
    if (is.numeric(new_entry)) {
      if (any(new_entry < minval) || any(new_entry > maxval)) {
        return(-2)
      } else {
        return(0)
      }
    } else {
      return(-3)
    }
}
###############################################################################
#' Function to check the format of a numeric column when the values
#' are not bounded
#' @param column_name the column name
#' @param data data frame
#' @param nrcode non response code corresponding to the column
#' @return 0, if success error, if failure
#' @examples
#' test_data_numeric_norange("marks", data.frame(
#'   "marks"  =  c(210, 99),
#'   "Name" =  c("John", "Dora")
#' ), -99)
#' @export
test_data_numeric_norange <- function(column_name, data, nrcode = NA) {
  column_no <- get_columnno_fornames(data, column_name)
    entry <- unlist(data.frame(data[[column_no]], stringsAsFactors = FALSE))
    if (is.na(nrcode)) {
      no_nrcode_entries <- entry[which(!is.na(entry))]
    }
    else {
      no_nrcode_entries <- entry[which(entry != nrcode)]
    }
    if (is.numeric(no_nrcode_entries)) {
      return(0)
    } else {
      return(-2)
    }
}
###############################################################################
#' Function to check the format of a string column
#' @param data data frame
#' @param column_name the column name
#' @param nrcode non response code corresponding to the column
#' @return 0, if success error, if failure
#' @examples
#' test_data_string(data.frame("Age" = c(21, 15), "Name" = c("John", "Dora")),
#' "name", -999)
#' @export
test_data_string <- function(data, column_name, nrcode = NA) {
  column_no <- get_columnno_fornames(data, column_name)
    temp <- data[column_no]
    temp <- unlist(temp[!is.na(temp)])
    if (!is.na(nrcode)) {
      new_entry <- temp[temp != nrcode]
    } else {
      new_entry <- temp[!is.na(temp)]
    }
    new_entry <- suppressWarnings(as.numeric(as.character(new_entry)))
    if (any(!is.na(new_entry))) {
      return(-2)
    } else {
      return(0)
    }
}
###############################################################################
#' Function to check the format of a string column when the string values
#' are given
#' @param data data frame
#' @param column_name the column name
#' @param nrcode non response code corresponding to the column
#' @param allowed_strings allowed strings or characters to represent
#' meaningful entry
#' @return 0, if success error, if failure
#' @examples
#' test_data_string_restriction(
#'   data.frame("Age" = c(21, 15), "sex" = c("m", "f")),
#'   "sex", -999, c("f", "m")
#' )
#' @export
test_data_string_restriction <- function(data, column_name, nrcode = NA,
                                         allowed_strings) {
  res <- test_data_string(data, column_name, nrcode)
  if (res == 0) {
    column_no <- get_columnno_fornames(data, column_name)
      if (length(allowed_strings) >= 1) {
        entry <- toupper(data[[column_no]])
        if (!is.na(nrcode)) {
          new_entry <- entry[entry != nrcode]
        } else {
          new_entry <- entry[!is.na(entry)]
        }
        if (any(is.na(new_entry) == TRUE) ||
          sum(toupper(allowed_strings) %in% unique(new_entry))
          < length(unique(new_entry))) {
          return(-2)
        } else {
          return(0)
        }
      } else {
        return(-3)
      }
  } else {
    if (res == -2) {
      return(-5)
    }
  }
}
#' ############################################################################
#' Function to check if a given pattern is contained in
#' the column names of a data
#' @param pattern a string that needs to be checked
#' @param column_names column names actually have
#' @return TRUE, if success FALSE, if failure
#' @examples
#' check_col_pattern_colname("age", "female_age")
#' @export
check_col_pattern_colname <- function(pattern, column_names) {
  if (is.na(pattern) || pattern == "") {
    return(-1)
  } else {
    if (is.numeric(pattern)) {
      test <- grep(toString(pattern), toupper(column_names))
    } else {
      test <- grep(toupper(pattern), toupper(column_names))
    }
    if (length(test) == 0) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}
###############################################################################
#' Function to return the column number if a given pattern is contained
#' in the column names of a data
#' @param pattern a string that needs to be checked
#' @param column_names column names actually have
#' @return column number, if success error, if failure
#' @examples
#' get_colno_pattern_colname("age", "female_age")
#' @export
get_colno_pattern_colname <- function(pattern, column_names) {
  if (check_col_pattern_colname(pattern, column_names) == TRUE) {
    test <- grep(toupper(pattern), toupper(column_names))
    return(test)
  } else {
    return(-1)
  }
}
#' ############################################################################
#'  Function to keep only certain variables
#' @param variables list of variables
#' @param the_data data to be sub setting
#' @return subset
#' @examples
#' the_data <- data.frame("Age" = c(21, 15), "sex" = c("m", "f"))
#' variable <- "Age"
#' keep_required_columns(variable, the_data)
#' @export
keep_required_columns <- function(variables, the_data) {
  for (i in seq_len(length(variables))) {
    if (!is.null(variables[i])) {
      if (is.na(variables[i])) stop("Some variables are NA")
    } else {
      stop("Some variables are NULL, please check")
    }
  }
  exists <- unlist(lapply(variables, IPDFileCheck::check_column_exists,
                         the_data))
  if (sum(exists) != 0) {
    stop("Some variables do not exists in data, please check")
  }
  subset <- the_data[(names(the_data) %in% variables)]
  if (ncol(subset) == 0 | nrow(subset) == 0)
    stop(" null value provided")
  return(subset)
}
#' ############################################################################
#' Function to return mode
#' @param v a vector
#' @return mode
#' @examples
#' get_mode_from_vector(c(1, 1, 2, 3))
#' @export
get_mode_from_vector <- function(v) {
  if (is.numeric(v)) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  } else {
    return(-1)
  }
}
###############################################################################
#' Function to return descriptive statistics, sum, no of observations, mean,
#' mode. median, range, standard deviation and standard error
#' @param data data frame
#' @param column_name the column name
#' @param nrcode non response code corresponding to the column
#' @return the descriptive statistics for success , error for failure
#' @examples
#' descriptive_stats_col_excl_nrcode(data.frame("age" = c(21, 15),
#' "Name" = c("John", "Dora")), "age", NA)
#' @import stats
#' @export
descriptive_stats_col_excl_nrcode <- function(data, column_name, nrcode = NA) {
  col.names <- colnames(data)
  if (column_name %in% col.names) {
    if (test_data_numeric_norange(column_name, data, nrcode) == 0) {
      this_column <- data[column_name]
      if (is.na(nrcode)) {
        missing_count <- sum(is.na(this_column))
        this_column <- this_column[!is.na(data[column_name])]
      } else {
        missing_count <- sum(this_column == nrcode)
        this_column <- this_column[data[column_name] != nrcode
        & !is.na(data[column_name])]
      }
      if (length(this_column) == 0) {
        return(0)
      } else {
        this_sum <- round(sum(this_column), 3)
        this_av <- round(mean(this_column), 3)
        this_med <- round(median(this_column), 3)
        this_mode <- round(get_mode_from_vector(this_column), 3)
        this_range_low <- round(min(this_column), 3)
        this_range_high <- round(max(this_column), 3)
        this_sd <- round(sd(this_column), 3)
        this_se <- round(this_sd / sqrt(length(this_column)), 3)
        this_lq <- round(quantile(this_column, c(0.25)), 3)
        this_uq <- round(quantile(this_column, c(0.75)), 3)
        this_ci_low <- round(quantile(this_column, c(0.025)), 3)
        this_ci_high <- round(quantile(this_column, c(0.975)), 3)
        this_range <- paste(this_range_low, "-", this_range_high)
        results <- matrix(c(
          this_sum, this_av, this_sd, this_med, this_mode,
          this_se, this_range_low, this_range_high, this_range,
          length(this_column), this_lq, this_uq, this_ci_low, this_ci_high,
          missing_count
        ), byrow = TRUE, nrow = 1)
        colnames(results) <- c(
          "Sum", "Mean", "SD", "Median", "Mode", "SE", "Minimum", "Maximum",
          "Range", "Count", "LQ", "UQ", "CIlow", "CIhigh", "MissingCount"
        )
        rownames(results) <- column_name
        return(results)
      }
    } else {
      stop("Error - column contents not numeric")
    }
  } else {
    stop("Error - no column or column name different")
  }
}
###############################################################################
#' Function to check the given column exists
#' @param column_name a column name
#' @param data data frame
#' @return 0 if success error if failure
#' @examples
#' check_column_exists("age", data.frame("Age" = c(21, 15),
#' "Name" = c("John", "Dora")))
#' @export
check_column_exists <- function(column_name, data) {
  if (any(toupper(colnames(data)) == toupper(column_name))) {
    return(0)
  } else {
    return(-1)
  }
}
#' ############################################################################
#' Function to present the mean and sd of a data set in the form Mean (SD)
#' @param data data frame
#' @param column_name the column name
#' @param nrcode non response code corresponding to the column
#' @return the mean(sd), error for failure
#' @examples
#' present_mean_sd_rmna_text(data.frame(
#'   "age" = c(21, 15),
#'   "Name" = c("John", "Dora")
#' ), "age", NA)
#' @export
present_mean_sd_rmna_text <- function(data, column_name, nrcode = NA) {
  desc <- descriptive_stats_col_excl_nrcode(data, column_name, nrcode)
    desc <- data.frame(desc)
    this_mean <- as.numeric(desc$Mean)
    this_sd <- as.numeric(desc$SD)
    ans <- paste(round(this_mean, 2), " (", round(this_sd, 2), ")", sep = "")
    return(ans)

}
###############################################################################
#' Function to return a subgroup when certain variable equals the given value
#' while omitting those with NA
#' @param data data frame
#' @param variable that corresponds to a column
#' @param value a value that can be taken by the variable
#' @examples
#' return_subgroup_omitna(data.frame(
#'   "age" = c(21, 15),
#'   "Name" = c("John", "Dora")
#' ), "age", 10)
#' @return subgroup a data frame if success error if failure
#' @export
return_subgroup_omitna <- function(data, variable, value) {
  if (check_column_exists(variable, data) == 0) {
    column_no <- get_columnno_fornames(data, variable)
    subgroup <- data[which(data[column_no] == value &
                             !is.na(data[column_no])), ]
    return(subgroup)
  } else {
    stop("Data does not contain the column with the specfied column name")
  }
}
###############################################################################
#' Function to return a subgroup when certain variable equals the given value
#' while omitting those with NA
#' @param data data frame
#' @param variable that corresponds to a column
#' @param value a value that can be taken by the variable
#' @examples
#' return_subgroup_omitna(data.frame(
#'   "age" = c(21, 15),
#'   "Name" = c("John", "Dora")
#' ), "age", 10)
#' @return subgroup a data frame if success error if failure
#' @export
return_subgroup_omitna <- function(data, variable, value) {
  if (check_column_exists(variable, data) == 0) {
    column_no <- get_columnno_fornames(data, variable)
    subgroup <- data[which(data[column_no] == value &
                             !is.na(data[column_no])), ]
    return(subgroup)
  } else {
    stop("Data does not contain the column with the specfied column name")
  }
}
###############################################################################
#' Function to return a subgroup when certain variable equals the given value
#' while omitting those with NA
#' @param data data frame
#' @param variable that corresponds to a column
#' @param value a value that can be taken by the variable
#' @examples
#' return_subgroup_withNA(data.frame(
#'   "age" = c(21, 15),
#'   "Name" = c("John", "Dora")
#' ), "age", 10)
#' @return subgroup a data frame if success error if failure
#' @export
return_subgroup_withNA <- function(data, variable, value) {
  if (check_column_exists(variable, data) == 0) {
    column_no <- get_columnno_fornames(data, variable)
    if (is.na(value)) {
      subgroup <- data[is.na(data[column_no]), ]
    } else {
      subgroup <- data[which(data[column_no] == value), ]
    }
    return(subgroup)
  } else {
    stop("Data does not contain the column with the specfied column name")
  }
}
###############################################################################
#' Function to find the effect size Cohen's d
#' @param x, a vector
#' @param y, another vector
#' @return cohens d estimated with 95% CI or error if failure
#' @examples
#' cohensd(c(1, 2, 3, 4), c(3, 4, 5, 6))
#' @export
cohensd <- function(x, y) {
  xx <- suppressWarnings(as.numeric(x))
  yy <- suppressWarnings(as.numeric(y))
  xnotna <- sum(!is.na(xx))
  ynotna <- sum(!is.na(yy))
  if (xnotna == length(x) && ynotna == length(y)) {
    lx <- length(x) - 1
    ly <- length(y) - 1
    md <- abs(mean(x) - mean(y)) ## mean difference (numerator)
    csd <- lx * var(x) + ly * var(y)
    csd <- csd / (lx + ly)
    csd <- sqrt(csd) ## common sd computation
    cd <- md / csd ## cohen's d
    var_d <- 1 / (lx + 1) + 1 / (ly + 1) + (cd^2) / (2 * (lx + ly + 2))
    ans <- c(cd, cd - 1.96 * sqrt(var_d), cd + 1.96 * sqrt(var_d))
    return(ans)
  } else {
    stop("Vector contains atleast one NA or string")
  }
}
#' ############################################################################
#' Function to estimate standard error of the mean
#' @param x, a vector
#' @return SE the standard error of the mean
#' @examples
#' get_sem(c(1, 2, 3, 4))
#' @export
get_sem <- function(x) {
  xx <- suppressWarnings(as.numeric(x))
  if (sum(is.na(xx)) > 0) {
    stop("Vector contains non numeric data")
  } else {
    ans <- sd(x) / sqrt(length(x))
    return(ans)
  }
}
#############################################################################
#' Function to find the number and percentages of categories
#' @param data, a data frame
#' @param variable the column name
#' @param nrcode non response code
#' @return number and percentages or error if failure
#' @examples
#' this.df <- data.frame(c(11, 78), c("m", "f"), stringsAsFactors = FALSE)
#' colnames(this.df) <- c("mark", "gender")
#' represent_categorical_data_exclude_missing(this.df, "gender", NA)
#' @export
represent_categorical_data_exclude_missing <- function(data, variable,
                                                       nrcode = NA) {
  coding <- unique(toupper(factor(data[[variable]])))
  if (is.na(nrcode)) {
    coding <- coding[!is.na(coding)]
  } else {
    coding <- coding[coding != nrcode]
  }
  coding <- sort(coding)
  num_categories <- length(coding)
  if (check_column_exists(variable, data) == 0) {
    ans <- rep(0, 2 * num_categories)
    all_names <- list()
    for (i in 1:num_categories) {
      if (coding[i] == "NA") {
        uppervals <- toupper(factor(data[[variable]]))
        num <- nrow(data[which(uppervals == coding[i]), ])
      } else {
        uppervals <- toupper(factor(data[[variable]]))
        if (is.na(nrcode)) {
          num <- nrow(data[which(uppervals == coding[i] &
                                   !is.na(uppervals)), ])
        } else {
          num <- nrow(data[which(uppervals == coding[i] &
                                   uppervals != nrcode), ])
        }
      }
      perc <- 100 * num / nrow(data)
      ans[2 * i] <- round(perc, 2)
      ans[2 * i - 1] <- round(num, 2)
      names_here <- c(paste(coding[i]))
      all_names <- c(all_names, names_here)
    }
    mat_ans <- matrix(ans, ncol = length(coding))
    colnames(mat_ans) <- all_names
    rownames(mat_ans) <- c("Number", "Percentage")
    return(mat_ans)
  } else {
    stop("Data does not contain the column with the specfied column name")
  }
}
#############################################################################
#' Function to find the number and percentages of categories
#' @param data, a data frame
#' @param variable the column name
#' @param nrcode non response code
#' @return number and percentages or error if failure
#' @examples
#' this.df <- data.frame(c(11, 78), c("m", "f"), stringsAsFactors = FALSE)
#' colnames(this.df) <- c("mark", "gender")
#' represent_categorical_data_include_missing(this.df, "gender", NA)
#' @export
represent_categorical_data_include_missing <- function(data, variable,
                                                       nrcode = NA) {
  coding <- unique(toupper(factor(data[[variable]])))
  num_categories <- length(coding)
  if (check_column_exists(variable, data) == 0) {
    ans <- rep(0, 2 * num_categories)
    all_names <- list()
    uppervals <- toupper(factor(data[[variable]]))
    for (i in 1:num_categories) {
      if (is.na(coding[i])) {
        num <- nrow(data[which(is.na(uppervals)), ])
      } else {
        num <- nrow(data[which(uppervals == coding[i]), ])
      }
      perc <- 100 * num / nrow(data)
      ans[2 * i] <- round(perc, 2)
      ans[2 * i - 1] <- round(num, 2)
      names_here <- c(paste(coding[i]))
      all_names <- c(all_names, names_here)
    }
    mat_ans <- matrix(ans, ncol = length(coding))
    colnames(mat_ans) <- all_names
    rownames(mat_ans) <- c("Number", "Percentage")
    return(mat_ans)
  } else {
    stop("Data does not contain the column with the specfied column name")
  }
}
#############################################################################
#' Function to represent categorical data in the form - numbers (percentage)
#' @param data data frame
#' @param variable column name
#' @param nrcode non response code
#' @return the numbers (percentage) , error for failure
#' @examples
#' df <- data.frame(c(11, 78), c("m", "f"), stringsAsFactors = FALSE)
#' colnames(df) <- c("mark", "gender")
#' represent_categorical_textdata(df, "gender", NA)
#' @export
represent_categorical_textdata <- function(data, variable, nrcode) {
  intresult <- represent_categorical_data_include_missing(data, variable,
                                                          nrcode)
  ans <- rep(0, ncol(intresult))
  i <- 1
  while (i <= ncol(intresult)) {
    num <- intresult[1, i]
    perc <- intresult[2, i]
    temp <- c(paste(round(num, 2), " (", round(perc, 2), ")", sep = ""))
    ans[i] <- temp
    i <- i + 1
  }
  names(ans) <- colnames(intresult)
  return(ans)
}

#############################################################################
#' Function to find the number and percentages of categories
#' @param data, a data frame
#' @param variable1 the column name of the variable to be grouped based on
#' @param variable2 the column name of the variable to represented
#' @param nrcode non response code for the variable2
#' @return the subgroup
#' @examples
#' this.df <- data.frame(c(11, 78,22), c("m", "f", "f"), c(1,2,2),
#' stringsAsFactors = FALSE)
#' colnames(this.df) <- c("mark", "gender", "group")
#' represent_categorical_data_forsubgroups(this.df, "group", "gender", NA)
#' @export
represent_categorical_data_forsubgroups <- function(data, variable1, variable2,
                                                    nrcode = NA) {
  if (is.null(variable1) | is.null(variable2) | is.null(data)) {
    stop("Some of the arguments are NULL")
  }
  cols_shouldhave <- c(variable1, variable2)
  resuts <- sum(unlist(lapply(cols_shouldhave, check_column_exists, data)))
  if (resuts != 0) {
    stop("Some variables are not in the data")
  } else {
    coding <- unique(toupper(factor(data[[variable1]])))
    variables <- unique(toupper(factor(data[[variable2]])))
    coding_len <- length(coding)
    var_len <- length(variables)
    all_list <-  c()
    for (i in seq_len(length(coding))) {
      this_subgroup1 <- return_subgroup_withNA(data, variable1, coding[i])
      this_rep <- (represent_categorical_data_include_missing(
        this_subgroup1, variable2, nrcode))
      if (ncol(this_rep) < var_len) {
        not_repr <- c()
        for (j in seq_len(var_len)) {
          if (is.na(variables[j])) {
            check <- sum("NA" %in% colnames(this_rep))
          } else {
            check <- variables[j] %in% colnames(this_rep)
          }
          if (!check)
            not_repr <- append(not_repr, variables[j])
        }
        num_not_repr <- length(not_repr)
        new_col <- rep(0, nrow(this_rep))
        new_colnames <- append(colnames(this_rep), not_repr)
        for (i in seq_len(num_not_repr)) {
          this_rep <- cbind(this_rep, new_col)
        }
        colnames(this_rep) <- new_colnames
        this_rep <- this_rep[, order(colnames(this_rep))]
      }
      this_rep <- this_rep[, order(colnames(this_rep))]
      names_list <- (colnames(this_rep))
      all_list <- cbind(all_list, this_rep)
    }
    all_list <- data.frame(all_list)
    row.names(all_list) <- row.names(this_rep)
    out <- kableExtra::kbl(all_list, "html", booktabs = T, align = c("r"),
               col.names = rep(names_list, coding_len))
    out2 <- kableExtra::kable_styling(out, "striped", full_width = F,
                                      position = "left", font_size = 12)
    header <- rep(var_len, coding_len)
    names(header) <- coding
    header <- c("", header)
    out3 <- kableExtra::add_header_above(out2, header = header)
    return(out3)
  }
}
#############################################################################
#' Function to find the number and percentages of categories
#' @param data, a data frame
#' @param variable1 the column name of the variable to be grouped based on
#' (categorical column)
#' @param variable2 the column name of the variable to represented
#' (numerical data)
#' @param nrcode non response code for the variable2
#' @return the subgroup
#' @examples
#' this.df <- data.frame(c(11, 78,22), c("m", "f", "f"), c(1,2,2),
#' stringsAsFactors = FALSE)
#' colnames(this.df) <- c("mark", "gender", "group")
#' represent_numerical_data_forsubgroups(this.df, "group", "mark", NA)
#' @export
represent_numerical_data_forsubgroups <- function(data, variable1, variable2,
                                                    nrcode = NA) {
  if (is.null(variable1) | is.null(variable2) | is.null(data)) {
    stop("Some of the arguments are NULL")
  }
  cols_shouldhave <- c(variable1, variable2)
  resuts <- sum(unlist(lapply(cols_shouldhave, check_column_exists, data)))
  if (resuts != 0) {
    stop("Some variables are not in the data")
  } else {
    coding <- unique(toupper(factor(data[[variable1]])))
    all_list <- new_list <- c()
    for (i in seq_len(length(coding))) {
      this_subgroup1 <- return_subgroup_withNA(data, variable1, coding[i])
      this_rep <- data.frame(descriptive_stats_col_excl_nrcode(this_subgroup1,
                                                    variable2, nrcode))
      if (nrow(this_rep) < 1) {
        this_rep <- rep(0, 15)
      }
      all_list <- rbind(all_list, this_rep)
    }
    all_list <- data.frame(all_list)
    new_list <- as.data.frame(append(new_list, coding))
    colnames(new_list) <- "Group"
    newlist <- data.frame(append(new_list, all_list))
    out <- kableExtra::kbl(newlist, "html", booktabs = T, align = c("l"))
    out2 <- kableExtra::kable_styling(out, "striped", full_width = F,
                        position = "left", font_size = 12)
    return(out2)
  }
}


################################################################################
#' Function that convert a number represented as character array
#' @param character_array a character array of numbers
#' @return converted_number in numeric form
#' @examples
#' convert_to_number(c("1", "9", "8"))
#' @export
convert_to_number <- function(character_array) {
  converted <- 0
  ending <- length(character_array)
  for (i in 1:ending) {
    converted <- converted +
      suppressWarnings(as.numeric(character_array[i]) * 10 ^ (ending - i))
  }
  if (!is.na(converted)) {
    return(converted)
  } else {
    return(-1)
  }
}
###############################################################################
#' Helper function to keep date formats in year-month-date
#' @param column a data frame or a  vector
#' @param index those correspond to valid date in numeic form (omitting non
#' response code or no entry)
#' @param orderby give the order such as mdy, dmy etc where d refers to day,
#' m to month and y to year
#' @return entry corrected entries as in standard date format
#' @examples
#' convert_date_numeric_stdform(c("01/01/2000", "02/02/2002"), c(1, 2), "dmy")
#' @export
convert_date_numeric_stdform <- function(column, index, orderby = "dmy") {
  required <- column[index]
  leng <- length(required)
  converted <- list()
  for (i in 1:leng) {
    converted_date <- convert_date_string_stdform(required[i], orderby)
    converted <- append(converted, converted_date)
  }
  converted <- unlist(converted)
  column[index] <- converted
  return(column)
}
###############################################################################
#' Helper function to keep date formats in year-month-date
#' @param entry a date e.g 1 Jan 2020 with no commas
#' @param orderby give the order such as mdy, dmy etc where d refers to day,
#' m to month and y to year
#' @return entry corrected entries as in standard date format
#' @examples
#' convert_date_string_stdform("Jan-1-2020", "mdy")
#' @export
convert_date_string_stdform <- function(entry, orderby) {
  contents <- unlist(strsplit(entry, ""))
  months <- toupper(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                      "Sep", "Oct", "Nov", "Dec"))
  months_full <- toupper(c(
    "January", "February", "March", "April", "May", "June", "July", "August",
    "September", "October", "November", "December"
  ))

  spec_chars <- c(" ", "/", "-")
  spec_char_exist <- NULL
  stop <- length(spec_chars)
  i <- 1
  while (i <= stop) {
    spec_char_exist <- grep(spec_chars[i], contents)
    if (length(spec_char_exist) != 0) {
      i <- length(spec_chars) + 1
    } else {
      i <- i + 1
    }
  }
  len_spchar <- length(spec_char_exist)
  if (len_spchar != 2) {
    stop("Many special characters, check your date format -
         only mid separators needed including the white space")
  }

  first <- list()
  for (i in 1:spec_char_exist[1] - 1) {
    this <- (contents[i])
    first <- append(first, this)
  }
  first <- unlist(first)
  if ("," %in% first) {
    comma_ind <- grep(",", first)
    first <- first[-comma_ind]
  }
  last <- list()
  start <- spec_char_exist[2] + 1
  ending <- length(contents)
  for (i in start:ending) {
    this <- (contents[i])
    last <- append(last, this)
  }
  last <- unlist(last)
  if ("," %in% last) {
    comma_ind <- grep(",", last)
    last <- last[-comma_ind]
  }

  mid <- list()
  start <- spec_char_exist[1] + 1
  ending <- spec_char_exist[2] - 1
  for (i in start:ending) {
    this <- (contents[i])
    mid <- append(mid, this)
  }
  mid <- unlist(mid)
  if ("," %in% mid) {
    comma_ind <- grep(",", mid)
    mid <- mid[-comma_ind]
  }

  if (orderby == "dmy") {
    if (convert_to_number(first) == -1) {
      stop("The format or the date given wrong -error in converting to number")
    } else {
      day <- convert_to_number(first)
    }
    month <- toupper(paste(mid, collapse = ""))
    if (convert_to_number(last) == -1) {
      stop("The format or the date given wrong -error in converting to number")
    } else {
      year <- convert_to_number(last)
    }
  }
  if (orderby == "dym") {
    if (convert_to_number(first) == -1) {
      stop("The format or the date given wrong -error in converting to number")
    } else {
      day <- convert_to_number(first)
    }
    month <- toupper(paste(last, collapse = ""))
    if (convert_to_number(mid) == -1) {
      stop("The format or the date given wrong -error in converting to number")
    } else {
      year <- convert_to_number(mid)
    }
  }
  if (orderby == "mdy") {
    if (convert_to_number(mid) == -1) {
      stop("The format or the date given wrong -error in converting to number")
    } else {
      day <- convert_to_number(mid)
    }
    month <- toupper(paste(first, collapse = ""))
    if (convert_to_number(last) == -1) {
      stop("The format or the date given wrong -error in converting to number")
    } else {
      year <- convert_to_number(last)
    }
  }
  if (orderby == "myd") {
    if (convert_to_number(last) == -1) {
      stop("The format or the date given wrong -error in converting to number")
    } else {
      day <- convert_to_number(last)
    }
    month <- toupper(paste(first, collapse = ""))
    if (convert_to_number(mid) == -1) {
      stop("The format or the year given wrong -error in converting to number")
    } else {
      year <- convert_to_number(mid)
    }
  }
  if (orderby == "ydm") {
    if (convert_to_number(mid) == -1) {
      stop("The format or the year given wrong -error in converting to number")
    } else {
      day <- convert_to_number(mid)
    }
    month <- toupper(paste(last, collapse = ""))
    if (convert_to_number(first) == -1) {
      stop("The format or the date given wrong -error in converting to number")
    } else {
      year <- convert_to_number(first)
    }
  }
  if (orderby == "ymd") {
    if (convert_to_number(last) == -1) {
      stop("The format or the date given wrong -error in converting to number")
    } else {
      day <- convert_to_number(last)
    }
    month <- toupper(paste(mid, collapse = ""))

    if (convert_to_number(first) == -1) {
      stop("The format or the year given wrong -error in converting to number")
    } else {
      year <- convert_to_number(first)
    }
  }
  if (suppressWarnings(is.na(as.numeric(month)))) {
    if (month %in% months) {
      month_index <- which(month == months)
    } else {
      if (month %in% months_full) {
        month_index <- which(month == months_full)
      } else {
        stop("Check the spelling for the month")
      }
    }
  } else {
    month <- as.numeric(month)
    month_index <- month
  }
  if (month_index == 2) {
    if (year %% 4 != 0) {
      if (day > 28) {
        stop("Date is not valid")
      }
    } else {
      if (day > 29) {
        stop("Date is not valid")
      }
    }
  }
  if (day > 30 & (month_index == 4 | month_index == 6 |
                  month_index == 9 | month_index == 11)) {
    stop("Date can not larger than 31")
  }
  if (day > 31 | day < 1 | month_index > 12 | month_index < 1) {
    stop("Month or date not valid")
  }
  date_return <- paste(year, "-", month_index, "-", day, sep = "")
  return(date_return)
}

###############################################################################
#' Function to calculate age from date of birth
#' @param data a data frame
#' @param columnname name of column corresponding to date of birth
#' @param enddatecol column contaiining when to calculate the age to,
#' default value is null, this means the age is calculated to the current date
#' @param dateformat format of date e.g. dmy default is dmy
#' @param nrcode non response code corresponding to date of birth
#' @return data if success error if failure
#' @examples
#' library(IPDFileCheck)
#' this.df <- data.frame(c("1987-05-28", "1987-06-18"), c(1, 2),
#' stringsAsFactors = FALSE)
#' colnames(this.df) <- c("dob", "num")
#' calculate_age_from_dob(this.df, "dob", NULL, "ymd")
#' @importFrom eeptools age_calc
#' @export
calculate_age_from_dob <- function(data, columnname, enddatecol = NULL,
                                   dateformat = "dmy",
                                   nrcode = NA) {
    column_no <- get_columnno_fornames(data, columnname)
    data <- as.data.frame(data, string.as.factors = FALSE)
    entry <- data[[column_no]]
    entry <- as.character(entry)
    blanks <- c(which(entry == ""), which(is.na(entry)))
    if (length(blanks) != 0) {
      entry[blanks] <- nrcode
    }
    calculated_ages <- rep(0, length(entry))
    if (is.na(nrcode)) {
      index <- which(!is.na(entry))
    } else {
      index <- which(entry != nrcode)
    }
    mod_entry <- convert_date_numeric_stdform(entry, index,
                                              orderby = dateformat)
    if (is.null(enddatecol)) {
      enddate <- Sys.Date()
    } else {
      if (is.na(enddatecol)) {
        enddate <- Sys.Date()
      } else {
        enddate <- as.character(data[[enddatecol]])
        mod_end_entry <- convert_date_numeric_stdform(enddate, index,
                                                      orderby = dateformat)
        enddate <- as.Date(mod_end_entry[index])
      }
    }
    result <- eeptools::age_calc(as.Date(mod_entry[index]), enddate, units = "years")
    calculated_ages[index] <- result
    calculated_ages[blanks] <- NA
    non_na_ages <- calculated_ages[!is.na(calculated_ages)]
    if (any(non_na_ages > 150) || any(non_na_ages < 0)) {
      stop("Age can not be negative OR greater than 150")
    } else {
      data["age"] <- calculated_ages
      return(data)
    }
}

###############################################################################
#' Function to calculate age from year of birth
#' @param data a data frame
#' @param columnname name of column corresponding to year of birth
#' @param endyearcol name of column where the year is entered to calculate
#' the age upto, by default its the current year
#' @param nrcode non response code corresponding to date of birth
#' @return data, if success error if failure
#' @examples
#' this.data.frame <- data.frame(c(1951, 1980), c("John", "Dora"))
#' colnames(this.data.frame) <- c("yob", "name")
#' calculate_age_from_year(this.data.frame, "yob", NULL, NA)
#' @export
calculate_age_from_year <- function(data, columnname, endyearcol = NULL, nrcode = NA) {
  column_no <- get_columnno_fornames(data, columnname)
    entry <- data[[column_no]]
    blanks <- c(which(entry == ""), which(is.na(entry)))
    if (length(blanks) != 0) {
      entry[blanks] <- nrcode
    }
    calculated_ages <- rep(0, length(entry))
    if (is.null(endyearcol)) {
      this_year <- lubridate::year(as.Date(Sys.Date(), format = "%d/%m/%y"))
    } else {
      if (is.na(endyearcol)) {
        this_year <- lubridate::year(as.Date(Sys.Date(), format = "%d/%m/%y"))
      } else {
        res <- sum(is.na(suppressWarnings(as.numeric(data[[endyearcol]]))))
        if (res == 0) {
          this_year <- as.numeric(data[[endyearcol]])
        } else {
          stop("The year is not numeric")
        }
      }
    }
    if (is.na(nrcode)) {
      index <- which(!is.na(entry))
      if (length(this_year) == 1) the_year <- this_year
      else the_year <- this_year[index]
      calculated_ages[index] <- the_year -
        as.numeric(as.character(entry[index]))
      calculated_ages[blanks] <- NA
    } else {
      index <- which(entry != nrcode)
      if (length(this_year) == 1) the_year <- this_year
      else the_year <- this_year[index]
      calculated_ages[index] <- the_year -
        as.numeric(as.character(entry[index]))
      calculated_ages[blanks] <- NA
    }
    non_na_ages <- calculated_ages[!is.na(calculated_ages)]
    if (any(non_na_ages > 150) || any(non_na_ages < 0)) {
      stop("Age can not be negative OR greater than 150")
    } else {
      data["calc.age.yob"] <- calculated_ages
      return(data)
    }
}
#############################################################################
#' Function to return the unique contents of the column given the column name
#' @param data a data frame
#' @param colname name of column corresponding to year of birth
#' @return the contents of the column, if success error if failure
#' @examples
#' get_contents_cols(data.frame(
#'   "yob" =  c(1951, 1980),
#'   "Name" =  c("John", "Dora")
#' ), "yob")
#' @export
get_contents_cols <- function(data, colname) {
  # check to see if the columnname exists
  if (check_column_exists(colname, data) == 0) {
    data <- as.data.frame(data, stringAsFactors = FALSE)
    codes <- unique(data[[colname]])
    if (sum(is.na(suppressWarnings(as.numeric(codes)))) < length(codes)) {
      return(as.numeric(codes))
    } else {
      return(codes)
    }
  } else {
    stop("Data does not contain the column with the specfied column name")
  }
}
