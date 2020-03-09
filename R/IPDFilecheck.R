###############################################################################
#' Function to check the package is installed, if not install
#' @param pkg name of package(s)
#' @return 0, if packages cant be installed and loaded, else error
#' @examples check_load_packages("dplyr")
#' @export
#' @importFrom methods is
check_load_packages <- function(pkg) {
  for (i in seq_len(length(pkg))) {
    x <- pkg[i]
    if (!suppressWarnings(require(x,character.only = TRUE)))
    {
      trythis <- tryCatch(install.packages(x,dependencies = TRUE,repos = "http://cran.us.r-project.org"),
               error = function(e)
                 cat("Error "),
               warning = function(e)
                 cat("Warning "))
      if (is(trythis,"warning")) {
        warning("Warning installing")
      }
      if (is(trythis,"error")) {
        warning("Error installing")
      }
      trythis <- tryCatch(require(x,character.only = TRUE),
                          error = function(e)
                            warning("Error in adding to library "),
                          warning = function(e)
                            warning("Invalid package ")
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
#' test_columnnames(c("name","age"), data.frame("Age" =  c(21,15),
#' "Name" =  c("John","Dora")))
#' @export
test_columnnames <- function(column_names, data) {
  upper_given_colnames <- sort(toupper(column_names))
  upper_data_colnames <- sort(toupper(colnames(data)))
  if (sum(upper_given_colnames == upper_data_colnames) == length(column_names)) {
    return(0)
  }else{
    warning("One or more columns may have different names")
  }
}
###############################################################################
#' Function to throw error on invalid directory or file and if not readable
#' @param filename name of a file or dir
#' @return 0, if success error, if failure
#' @examples
#' test_file_exist_read(system.file("extdata", "blank.txt",
#' package = "IPDFileCheck"))
#' @export
test_file_exist_read <- function(filename) {
  if (file.exists(filename)) {
    if (file.access(filename, 0)  !=  0) {
      warning("Error reading file")
    }
    return(0)
  }else{
    warning("Invalid directory or file")
  }
}
###############################################################################
#' Function to return the column number for column name
#' @param data a data frame
#' @param column_name column names of the data frame
#' @return column number, if success error, if failure
#' @examples
#' get_columnno_fornames(data.frame("Age" =  c(21,15), "Name" =  c("John","Dora")),"Name")
#' @export
get_columnno_fornames <- function(data, column_name) {
  data_column_names  <-  toupper(colnames(data))
  if (any(data_column_names == toupper(column_name))) {
    column_no <- which(data_column_names == toupper(column_name))
    return(column_no)
  }else{
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
#' df<-data.frame("Age" =  c(21,15), "Name" =  c("John", "Dora"))
#' test_age(df,"age", 999)
#' @export
test_age <- function(data, agecolumn = "age", nrcode = NA) {
  column_no <- get_columnno_fornames(data, agecolumn)
   if (column_no < 0) {
     warning("Column name age does not exist")
  }else{
    entry  <- data[[column_no]]
    blanks <- c(which(entry == ""), which(is.na(entry)))
    if (length(blanks) !=  0) {
      entry[blanks] <- nrcode
    }
   if (is.na(nrcode)) {
      this_entry <- entry[!is.na(entry)]
      this_entry_num <- suppressWarnings(as.numeric(this_entry))
      if (sum(is.na(this_entry_num)) == 0) {
       newentry <- as.numeric(this_entry)
       if (any(newentry > 150) || any(newentry < 0)) {
         warning("Invalid entry in age column")
       }else{
         return(0)
       }
      }else{
       warning("Error - some entries other then nrcode is not numeric")
      }
    }else{
      this_entry <- entry[entry !=  nrcode]
      this_entry_num <- suppressWarnings(as.numeric(this_entry))
      if (sum(is.na(this_entry_num)) == 0) {
        newentry <- as.numeric(this_entry)
        if (any(newentry > 150) || any(newentry < 0)) {
          warning("Invalid entry in age column")
        }else{
          return(0)
        }
      }else{
        stop("Error - some entries other then nrcode is not numeric")
      }
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
#' test_gender(data.frame("sex" =  c("m","f"), "Name" =  c("John","Dora")),c("f", "m"),"sex",999)
#' @export
test_gender <- function(data, gendercode, gendercolumn = "gender", nrcode = NA) {
  gendercode <- toupper(gendercode)
  if (get_columnno_fornames(data, gendercolumn) > 0) {
    column_no = get_columnno_fornames(data, gendercolumn)
    entry  <-  data[column_no]
    if (is.na(nrcode)) {
      newentry <- entry[!is.na(entry)]
      gendercode <- gendercode[!is.na(gendercode)]
    }else{
      newentry <- entry[entry != nrcode, ]
      gendercode <- gendercode[which(gendercode !=  nrcode)]
    }
    facs <- levels(factor(toupper(newentry)))
    if (all(facs %in% gendercode)) {
        return(0)
    }else{
      warning("Invalid entry in gender column")
    }
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
#' test_column_contents(data.frame("sex" =  c("m","f"),
#' "Name"=  c("John","Dora")),"sex",c("m","f"),999)
#' @export
test_column_contents <- function(data, column, code, nrcode = NA) {
  column_no <- get_columnno_fornames(data, column)
  if (column_no < 0) {
    warning("Column name does not exist")
  }else{
    entry  <-  data[column_no]
    if (is.na(nrcode)) {
      newentry <- entry[!is.na(entry)]
      code <- code[!is.na(code)]
    }else{
      newentry <- entry[entry !=  nrcode, ]
      code <- code[which(code !=  nrcode)]
    }
    facs <- levels(factor(newentry))
    if (all(facs %in% code)) {
      return(0)
    }else{
      warning("Invalid entry in column")

    }
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
#' test_data_numeric("age", data.frame("Age" =  c(21,15),
#' "Name"  = c("John","Dora")),-99,0,100)
#' @export
test_data_numeric <- function(column_name, data, nrcode = NA, minval, maxval) {
  column_no <- get_columnno_fornames(data, column_name)
  if (column_no < 0) {
    warning("Column name does not exist")
  }else{
    entry  <- (data[[column_no]])
    if (is.na(nrcode)) {
      new_entry  <-  (entry[which(!is.na(entry))])
    }else{
      new_entry  <-  (entry[which(entry != nrcode)])
    }
    if (is.numeric(new_entry)) {
      if (any(new_entry < minval) || any(new_entry > maxval)) {
        warning("Invalid ranges in column")
      }else{
        return(0)
      }
    }else{
      warning("Non numeric values in column")
    }
  }
}
###############################################################################
#' Function to check the format of a numeric column when the values are not bounded
#' @param column_name the column name
#' @param data data frame
#' @param nrcode non response code corresponding to the column
#' @return 0, if success error, if failure
#' @examples
#' test_data_numeric_norange("marks", data.frame("marks"  =  c(210,99),
#' "Name" =  c("John","Dora")),-99)
#' @export
test_data_numeric_norange <- function(column_name, data, nrcode = NA) {
  column_no <- get_columnno_fornames(data, column_name)
  if (column_no < 0) {
    warning("Column name does not exist")

  }else{
    entry  <- unlist(data.frame(data[[column_no]], stringsAsFactors  =  FALSE))
    if (is.na(nrcode)) {
      no_nrcode_entries <- entry[which(!is.na(entry))]
    }
    else{
      no_nrcode_entries <- entry[which(entry !=  nrcode)]
    }
    if (is.numeric(no_nrcode_entries)) {
      return(0)
    }else{
      warning("Some values - other than NR code is not numeric")

    }
  }
}
###############################################################################
#' Function to check the format of a string column
#' @param data data frame
#' @param column_name the column name
#' @param nrcode non response code corresponding to the column
#' @return 0, if success error, if failure
#' @examples
#' test_data_string(data.frame("Age" = c(21,15), "Name" = c("John","Dora")),"name",-999)
#' @export
test_data_string <- function(data, column_name, nrcode = NA) {
  column_no <- get_columnno_fornames(data, column_name)
  if (column_no < 0) {
    warning("Column name does not exist")
  }else{
    temp <- data[column_no]
    temp <- unlist(temp[!is.na(temp)])
    if (!is.na(nrcode)) {
      new_entry <- temp[temp != nrcode]
    }else{
      new_entry <- temp[!is.na(temp)]
    }
    new_entry <- suppressWarnings(as.numeric(as.character(new_entry)))
    if (any(!is.na(new_entry))) {
      warning("Numeric entry in column")

    }else{
      return(0)
    }
  }
}
###############################################################################
#' Function to check the format of a string column when the string values are given
#' @param data data frame
#' @param column_name the column name
#' @param nrcode non response code corresponding to the column
#' @param allowed_strings allowed strings or characters to represent meaningful entry
#' @return 0, if success error, if failure
#' @examples
#' test_data_string_restriction(data.frame("Age" = c(21,15), "sex" =  c("m","f")),
#' "sex",-999,c("f","m"))
#' @export
test_data_string_restriction <- function(data, column_name, nrcode = NA, allowed_strings) {
  res <- test_data_string(data, column_name, nrcode)
  if (res == 0) {
    column_no <- get_columnno_fornames(data, column_name)
    if (column_no < 0) {
      warning("column name does not exist")
    }else{
      if (length(allowed_strings) >= 1) {
        entry  <- toupper(data[[column_no]])
        if (!is.na(nrcode)) {
          new_entry <- entry[entry !=  nrcode]
        }else{
          new_entry <- entry[!is.na(entry)]
        }
        if (any(is.na(new_entry) == TRUE) ||
            sum(toupper(allowed_strings) %in% unique(new_entry))
            < length(unique(new_entry))) {
          warning("Invalid entry in column")

        }else{
          return(0)
        }
      }else{
        warning("Please provide the restriction on allowed strings, else use test_data_string(..)")
      }
    }
  }else{
    if (res == -1) {
      warning("Column name does not exist")
   }
    if (res == -2) {
      warning("atleast one non string entry in column")
    }

  }
}
#' ###############################################################################
#' Function to return the column number if a given pattern is contained in
#' the column names of a data
#' @param pattern a string that needs to be checked
#' @param column_names column names actually have
#' @return TRUE , if success FALSE, if failure
#' @examples check_colno_pattern_colname("age","female_age")
#' @export
check_colno_pattern_colname <- function(pattern, column_names) {
  if (is.na(pattern) || pattern == "") {
    warning("Error pattern NA or empty")
  }else{
    if (is.numeric(pattern)) {
      test <- grep(toString(pattern), toupper(column_names))
    }else{
      test <- grep(toupper(pattern), toupper(column_names))
    }
    if (length(test) == 0) {
      return(FALSE)
    }else{
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
#' @examples get_colno_pattern_colname("age","female_age")
#' @export
get_colno_pattern_colname <- function(pattern, column_names) {
  if (check_colno_pattern_colname(pattern, column_names) == TRUE) {
    test <- grep(toupper(pattern), toupper(column_names))
    return(test)
  }else{
    warning("The pattern does not form any part of columnnames")
  }
}
#' ###############################################################################
#' Function to return mode
#' @param v a vector
#' @return mode
#' @examples get_mode_from_vector(c(1,1,2,3))
#' @export
get_mode_from_vector  <-  function(v) {
  if (is.numeric(v)) {
    uniqv  <-  unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }else{
    warning("Non numeric data")

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
#' descriptive_stats_col(data.frame("age" = c(21,15), "Name"  = c("John","Dora")), "age", NA)
#' @import stats
#' @export
descriptive_stats_col <- function(data, column_name, nrcode = NA) {
  col.names <- colnames(data)
  if (column_name %in% col.names) {
    if (test_data_numeric_norange(column_name, data, nrcode) ==  0) {
      this_column <- data[column_name]
      if (is.na(nrcode)) {
        this_column <- this_column[!is.na(data[column_name])]
      }else{
        this_column <- this_column[data[column_name] !=  nrcode
                                   & !is.na(data[column_name])]
      }
      this_sum <- sum(this_column)
      this_av <- mean(this_column)
      this_med <- median(this_column)
      this_mode <- get_mode_from_vector(this_column)
      this_range_low <- min(this_column)
      this_range_high <- max(this_column)
      this_sd <- sd(this_column)
      this_se <-  this_sd / sqrt(length(this_column))
      this_lq <-  quantile(this_column, c(0.25))
      this_uq <-  quantile(this_column, c(0.75))
      this_ci_low <-  quantile(this_column, c(0.025))
      this_ci_high <-  quantile(this_column, c(0.975))
      results <- matrix(c(this_sum, this_av, this_sd, this_med, this_mode,
                          this_se, this_range_low, this_range_high,
                          length(this_column), this_lq, this_uq, this_ci_low, this_ci_high),
                          byrow = TRUE, nrow = 1)
      colnames(results) <- c("Sum", "Mean", "SD", "Median", "Mode", "SE", "Minimum", "Maximum",
                             "Count", "LQ", "UQ", "95%CI.low", "95%CI.high")
      rownames(results) <- column_name
      return(results)
    }
  }else{
    stop("Error - no column or column name different")

  }
}
###############################################################################
#' Function to check the given column exists
#' @param column_name a column name
#' @param data data frame
#' @return 0 if success error if failure
#' @examples
#' check_column_exists("age", data.frame("Age" = c(21,15), "Name" = c("John","Dora")))
#' @export
check_column_exists <- function(column_name, data) {
  if (any(toupper(colnames(data)) == toupper(column_name))) {
    return(0)
  }else{
    return(-1)
  }
}
#' ###############################################################################
#' Function to present the mean and sd of a data set in the form Mean (SD)
#' @param data data frame
#' @param column_name the column name
#' @param nrcode non response code corresponding to the column
#' @return the mean(sd), error for failure
#' @examples present_mean_sd_rmna_text(data.frame("age" = c(21,15),
#' "Name" = c("John","Dora")),"age",NA)
#' @export
present_mean_sd_rmna_text <- function(data, column_name, nrcode = NA) {
  desc <- descriptive_stats_col(data, column_name, nrcode)
  if (length(desc) == 1 & desc[1] == -1) {
    stop("Error or no data to analyse ")
  }else{
    desc <- data.frame(desc)
    this_mean <- as.numeric(desc$Mean)
    this_sd <- as.numeric(desc$SD)
    ans <- paste(round(this_mean, 2), " (", round(this_sd, 2), ")", sep = "")
    return(ans)
  }
}
###############################################################################
#' Function to return a subgroup when certain variable equals the given value
#' while omitting those with NA
#' @param data data frame
#' @param variable that corresponds to a column
#' @param value a value that can be taken by the variable
#' @examples return_subgroup_omitna(data.frame("age" = c(21,15),
#' "Name" = c("John","Dora")),"age",10)
#' @return subgroup a data frame if success error if failure
#' @export
return_subgroup_omitna <- function(data, variable, value) {
  if (check_column_exists(variable, data) == 0) {
      column_no <- get_columnno_fornames(data, variable)
      subgroup  <-  data[which(data[column_no] == value & !is.na(data[column_no])), ]
      return(subgroup)
  }else{
    stop("Data does not contain the column with the specfied column name")
  }
}
###############################################################################
#' Function to find the effect size Cohen's d
#' @param x, a vector
#' @param y, another vector
#' @return cohens d estimated with 95% CI or error if failure
#' @examples cohensd(c(1,2,3,4),c(3,4,5,6))
#' @export
cohensd <-  function(x, y) {
  xx <- suppressWarnings(as.numeric(x))
  yy <- suppressWarnings(as.numeric(y))
  xnotna <- sum(!is.na(xx))
  ynotna <- sum(!is.na(yy))
  if (xnotna == length(x) && ynotna == length(y)) {
    lx  <-  length(x) - 1
    ly  <-  length(y) - 1
    md   <-  abs(mean(x) - mean(y))        ## mean difference (numerator)
    csd  <-  lx * var(x) + ly * var(y)
    csd  <-  csd / (lx + ly)
    csd  <-  sqrt(csd)                     ## common sd computation
    cd   <-  md / csd                        ## cohen's d
    var_d  <-  1 / (lx + 1) + 1 / (ly + 1) + (cd ^ 2) / (2 * (lx + ly + 2))
    ans  <-  c(cd, cd - 1.96 * sqrt(var_d), cd + 1.96 * sqrt(var_d))
    return(ans)
  }else{
    stop("Vector contains atleast one NA or string")
  }
}
#' ###############################################################################
#' Function to estimate standard error of the mean
#' @param x, a vector
#' @return SE the standard error of the mean
#' @examples get_sem(c(1,2,3,4))
#' @export
get_sem  <-  function(x) {
  xx <- suppressWarnings(as.numeric(x))
  if (sum(is.na(xx)) > 0) {
    stop("Vector contains non numeric data")

  }else{
    ans <- sd(x) / sqrt(length(x))
    return(ans)
  }
}
###############################################################################
#' Function to find the number and percentages of categories
#' @param data, a data frame
#' @param variable the column name
#' @param nrcode non response code
#' @return number and percentages or error if failure
#' @examples
#' this.df <- data.frame(c(11,78),c("m","f"),stringsAsFactors = FALSE)
#' colnames(this.df) <- c("mark","gender")
#' represent_categorical_data(this.df,"gender",NA)
#' @export
represent_categorical_data <- function(data, variable, nrcode = NA) {
  coding <- unique(toupper(factor(data[[variable]])))
  if (is.na(nrcode)) {
    coding <- coding[!is.na(coding)]
  }else{
    coding <- coding[coding !=  nrcode]
  }
  coding <- sort(coding)
  num_categories <- length(coding)
  if (check_column_exists(variable, data) == 0) {
    ans  <-  rep(0, 2 * num_categories)
    all_names <- list()
    for (i in 1:num_categories) {
      if (coding[i] == "NA") {
        num <- nrow(data[which(is.na(data[variable])), ])
      }else{
        uppervals <- toupper(factor(data[[variable]]))
        if (is.na(nrcode)) {
          num <- nrow(data[which(uppervals == coding[i] & !is.na(uppervals)), ])
        }else{
          num <- nrow(data[which(uppervals == coding[i] & uppervals !=  nrcode), ])
        }
      }
      perc <- 100 * num / nrow(data)
      ans[2 * i] <- round(perc, 2)
      ans[2 * i - 1] <- round(num, 2)
      names_here <- c(paste(coding[i]))
      all_names <- c(all_names,names_here)
    }
    mat_ans <- matrix(ans, ncol = length(coding))
    colnames(mat_ans)  <- all_names
    rownames(mat_ans) <- c("Number", "Percentage")
    return(mat_ans)
  }else{
    stop("Data does not contain the column with the specfied column name")
  }
}
###############################################################################
#' Function to represent categorical data in the form - numbers (percentage)
#' @param data data frame
#' @param variable column name
#' @param nrcode non response code
#' @return the numbers (percentage) , error for failure
#' @examples
#' df <- data.frame(c(11,78),c("m","f"),stringsAsFactors = FALSE)
#' colnames(df) <- c("mark","gender")
#' represent_categorical_textdata(df,"gender",NA)
#' @export
represent_categorical_textdata <- function(data, variable, nrcode) {
    intresult <- represent_categorical_data(data, variable, nrcode)
    ans  <-  rep(0, ncol(intresult))
    i <- 1
    while (i <= ncol(intresult)) {
      print(i)
      num <- intresult[1,i]
      perc <- intresult[2,i]
      temp <- c(paste(round(num, 2), " (", round(perc, 2), ")", sep = ""))
      ans[i] <- temp
      i <- i + 1
    }
    names(ans) <- colnames(intresult)
    return(ans)
}
###############################################################################
#' Helper function to keep date formats in year/month/date
#' @param entry a data frame or a  vector
#' @param index those correspond to valid date (omitting non response code or no entry)
#' @param monthfirst if month is given before date, NULL by default
#' @return entry corrected entries
#' @examples convert_stddate_format(c("01/01/2000","02/02/2002"),c(1,2),NULL)
#' @export
convert_stddate_format <- function(entry, index, monthfirst = NULL) {
  contents <- unlist(strsplit(entry[1], ""))
  first <- which(!grepl("^[0-9]", contents))[1]
  ch <- contents[first]
  one <- two <- three <- c(0)
  for (i in seq_len(length(index))) {
    one <- c(one, unlist(strsplit(entry[index[i]], ch))[1])
    two <- c(two, unlist(strsplit(entry[index[i]], ch))[2])
    three <- c(three, unlist(strsplit(entry[index[i]], ch))[3])
  }
  one <- (one[-1])
  two <- (two[-1])
  three <- (three[-1])
  test1 <- sum(is.na(suppressWarnings(as.numeric(one)))) == length(one)
  test2 <- sum(is.na(suppressWarnings(as.numeric(two)))) == length(two)
  test3 <- sum(is.na(suppressWarnings(as.numeric(three)))) == length(three)
  check <- test1 || test2 || test3
  if (check == FALSE) {
    one <- as.numeric(one)
    two <- as.numeric(two)
    three <- as.numeric(three)
    if (min(one) >= 1000) {
      if (is.null(monthfirst)) {
        if (max(two) <= 12 || max(three) > 12) {
          for (i in seq_len(length(index)))
            entry[index[i]] <- paste(one[i], "/", two[i], "/", three[i], sep = "")
        }else{
          for (i in seq_len(length(index)))
            entry[index[i]] <- paste(one[i], "/", three[i], "/", two[i], sep = "")
        }
      }else{
        if (monthfirst == FALSE) {
          for (i in seq_len(length(index)))
            entry[index[i]] <- paste(one[i], "/", three[i], "/", two[i], sep = "")
        }else{
          for (i in seq_len(length(index)))
            entry[index[i]] <- paste(one[i], "/", two[i], "/", three[i], sep = "")
        }
      }
    }else{
      if (min(three) >= 1000) {
        if (is.null(monthfirst)) {
          if (max(two) <= 12 || max(one) > 12) {
            for (i in seq_len(length(index))) {
              entry[index[i]] <- paste(three[i], "/", two[i], "/", one[i], sep = "")
            }
          }else{
            for (i in seq_len(length(index))) {
              entry[index[i]] <- paste(three[i], "/", one[i], "/", two[i], sep = "")
            }
          }
        }else{
          if (monthfirst == FALSE) {
            for (i in seq_len(length(index))) {
              entry[index[i]] <- paste(three[i], "/", two[i], "/", one[i], sep = "")
            }
          }else{
            for (i in seq_len(length(index))) {
              entry[index[i]] <- paste(three[i], "/", one[i], "/", two[i], sep = "")
            }
          }
        }
      }else{
        stop("Error-no year shown in date")
      }
    }
  }else{
    stop("Date not in numeric formats")
  }
  return(entry)
}
###############################################################################
#' Function to calculate age from date of birth
#' @param data a data frame
#' @param columnname name of column corresponding to date of birth
#' @param dateformat format of date e.g. dmy default is FALSE
#' @param nrcode non response code corresponding to date of birth
#' @return data if success error if failure
#' @examples
#' library(IPDFileCheck)
#' this.df <- data.frame(c("1987-05-28", "1987-06-18"),c(1,2), stringsAsFactors = FALSE)
#' colnames(this.df) <- c("dob","num")
#' calculate_age_from_dob(this.df,"dob")
#' @importFrom eeptools age_calc
#' @export
calculate_age_from_dob <- function(data, columnname, dateformat = FALSE, nrcode = NA) {
  column_no <- get_columnno_fornames(data, columnname)
  if (column_no < 0) {
    stop("Column name for date of birth does not exist")
  }else{
    data  <- as.data.frame(data, string.as.factors = FALSE)
    entry <- data[[column_no]]
    blanks <- c(which(entry == ""), which(is.na(entry)))
    if (length(blanks) !=  0) {
      entry[blanks] <- nrcode
    }
    calculated_ages <- rep(0, length(entry))
    if (is.na(nrcode)) {
      index <- which(!is.na(entry))
    }else{
      index <- which(entry !=  nrcode)
    }
    if (dateformat == FALSE) {
      mod_entry <- convert_stddate_format(entry, index, monthfirst <- NULL)
    }else{
      if (dateformat == "%y/%m/%d" || dateformat == "%y-%m-%d") {
        mod_entry <- entry
      }
      if (dateformat == "%y/%d/%m" || dateformat == "%y-%d-%m") {
        monthfirst <- FALSE
        mod_entry <- convert_stddate_format(entry, index, monthfirst)
      }
      if (dateformat == "%m/%d/%y" || dateformat == "%m-%d-%y") {
        monthfirst <- TRUE
        mod_entry <- convert_stddate_format(entry, index, monthfirst)
      }
      if (dateformat == "%d/%m/%y" || dateformat == "%d-%m-%y") {
        monthfirst <- FALSE
        mod_entry <- convert_stddate_format(entry, index, monthfirst)
      }
    }
    result  <-  eeptools::age_calc(as.Date(mod_entry[index]), units = "years")
    calculated_ages[index] <- result
    calculated_ages[blanks] <- NA
    non_na_ages <- calculated_ages[!is.na(calculated_ages)]
    if (any(non_na_ages > 150) || any(non_na_ages < 0)) {
      stop("Age can not be negative OR greater than 150")
    }else{
      data["calc_age_dob"] <- calculated_ages
      return(data)
    }
  }
}

###############################################################################
#' Function to calculate age from year of birth
#' @param data a data frame
#' @param columnname name of column corresponding to year of birth
#' @param nrcode non response code corresponding to date of birth
#' @return data, if success error if failure
#' @examples
#' this.data.frame <-data.frame(c(1951,1980),c("John","Dora"))
#' colnames(this.data.frame) <- c("yob","name")
#' calculate_age_from_year(this.data.frame,"yob",NA)
#' @export
calculate_age_from_year <- function(data, columnname, nrcode = NA) {
  column_no <- get_columnno_fornames(data, columnname)
  if (column_no < 0) {
    stop("Column name for year of birth does not exist")
  }else{
    entry <- data[[column_no]]
    blanks <- c(which(entry == ""), which(is.na(entry)))
    if (length(blanks) !=  0) {
      entry[blanks] <- nrcode
    }
    calculated_ages <- rep(0, length(entry))
    this_year  <-  lubridate::year(as.Date(Sys.Date(), format = "%d/%m/%y"))
    if (is.na(nrcode)) {
      index <- which(!is.na(entry))
      calculated_ages[index] <- this_year - as.numeric(as.character(entry[index]))
      calculated_ages[blanks] <- NA
    }else{
      index <- which(entry !=  nrcode)
      calculated_ages[index] <- this_year - as.numeric(as.character(entry[index]))
      calculated_ages[blanks] <- NA
    }
    non_na_ages <- calculated_ages[!is.na(calculated_ages)]
    if (any(non_na_ages > 150) || any(non_na_ages < 0)) {
      stop("Age can not be negative OR greater than 150")

    }else{
      data["calc.age.yob"] <- calculated_ages
      return(data)
    }
  }
}
###############################################################################
#' Function to return the unique contents of the column given the column name
#' @param data a data frame
#' @param colname name of column corresponding to year of birth
#' @return the contents of the column, if success error if failure
#' @examples
#' get_contents_cols(data.frame("yob" =  c(1951,1980),
#' "Name" =  c("John","Dora")),"yob")
#' @export
get_contents_cols <- function(data, colname) {
  #check to see if the columnname exists
  if (check_column_exists(colname, data) == 0) {
    data <- as.data.frame(data, stringAsFactors =  FALSE)
    codes <- unique(data[[colname]])
    if (sum(is.na(suppressWarnings(as.numeric(codes)))) < length(codes)) {
      return(as.numeric(codes))
    }else{
      return(codes)
    }
  }else{
    stop("Data does not contain the column with the specfied column name")
  }
}
