
###############################################################################
context("testing package installation")
test_that("testing package installation", {
  expect_identical(check_load_packages("config"), 0)
  expect_identical(check_load_packages("valueEQ5D"), 0)
  expect_warning(check_load_packages("Sheeja"), "Invalid package",
                 fixed = TRUE)
  reqd_packages <- c("gmodels", "lmtest", "survival", "eha", "nlme",
                     "coda", "lattice", "R2WinBUGS", "MASS", "foreign", "plyr")
  expect_identical(check_load_packages(reqd_packages), 0)
})
##############################################################################
context("testing file existence")
test_that("test for file existence and access", {
  thisfile <- system.file("extdata", "blank.txt",
    package = "IPDFileCheck"
  )
  expect_identical(test_file_exist_read(thisfile), 0)
  #nofile <- system.file("extdata", "read.txt", package = "IPDFileCheck")
  #expect_identical(test_file_exist_read(nofile), -2)
})
# #############################################################################
context("testing column names of a data")
test_that("testing column names of a data", {
  x <- c("F", "M", "cvb", "sheeja")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("num", "name")
  expect_identical(test_columnnames(c("num", "name"), tempdata), 0)
  colnames(tempdata) <- c("num", "sex")
  expect_identical(test_columnnames(c("num", "name"), tempdata), -1)
})
# #########################################################################
context("testing age calculated from year of birth")
test_that("testing age calculated from year of birth", {
  x <- c("1957", "1987", 0, "1989")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "dob")
  ag1 <- as.numeric(format(Sys.Date(), "%Y")) - 1957
  ag2 <- as.numeric(format(Sys.Date(), "%Y")) - 1987
  ag3 <- as.numeric(format(Sys.Date(), "%Y")) - 1989
  ages <- c(ag1, ag2, 0, ag3)
  mod_data <- calculate_age_from_year(tempdata, "dob", NULL, 0)$calc.age.yob
  expect_equivalent(ages, mod_data, tolerance = 0.001)


  x <- c("1957", "1987", 0, "1989")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "dob")
  ag1 <- as.numeric(format(Sys.Date(), "%Y")) - 1957
  ag2 <- as.numeric(format(Sys.Date(), "%Y")) - 1987
  ag3 <- as.numeric(format(Sys.Date(), "%Y")) - 1989
  ages <- c(ag1, ag2, 0, ag3)
  mod_data <- calculate_age_from_year(tempdata, "dob", NA, 0)$calc.age.yob
  expect_equivalent(ages, mod_data, tolerance = 0.001)

  x <- c("1957", "1987", 0, "1989")
  y <- c(1, 2, 3, 4)
  y2 <- c("1987", "1997", 0, "2009")
  tempdata <- as.data.frame(cbind(y, x, y2))
  colnames(tempdata) <- c("name", "dob", "enddate")
  ages <- c(30, 10, 0, 20)
  mod_data <- calculate_age_from_year(tempdata, "dob",
                                      "enddate", 0)$calc.age.yob
  expect_equivalent(ages, mod_data, tolerance = 0.001)

  y2 <- c("sh", "sh", 0, "sh")
  tempdata <- as.data.frame(cbind(y, x, y2))
  colnames(tempdata) <- c("name", "dob", "enddate")
  expect_error(calculate_age_from_year(tempdata, "dob",
                                       "enddate", 0)$calc.age.yob)


  x <- c("1957", "1987", NA, "1989")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "dob")
  ages <- c(ag1, ag2, NA, ag3)
  mod_data <- calculate_age_from_year(tempdata, "dob", NULL, NA)$calc.age.yob
  expect_equivalent(ages, mod_data, tolerance = 0.001)
  x <- c(1957, 1987, NA, 1989)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "dob")
  expect_equivalent(ages, mod_data, tolerance = 0.001)
  colnames(tempdata) <- c("name", "date")
  expect_error(calculate_age_from_year(tempdata, "dob", NULL, NA),
    "Column name does not exist",
    fixed = TRUE
  )
  x <- c(1957, 1987, 1800, 1989)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "dob")
  expect_error(calculate_age_from_year(tempdata, "dob", NULL, NA),
    "Age can not be negative OR greater than 150",
    fixed = TRUE
  )
})
# ############################################################################
context("testing age")
test_that("test for age checks for valid age", {
  x <- c(0, 11, NA, 120)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = F)
  colnames(tempdata) <- c("name", "age")
  expect_identical(test_age(tempdata), 0)

  x <- c(0, 11, 78, 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = F)
  colnames(tempdata) <- c("name", "age")
  expect_identical(test_age(tempdata, "age", ""), -3)

  x <- c(0, 11, NA, "age")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = F)
  colnames(tempdata) <- c("name", "age")
  expect_identical(test_age(tempdata), -2)

  x <- c(0, 11, 78, 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = F)
  colnames(tempdata) <- c("name", "AGE")
  expect_identical(test_age(tempdata, "AGE"), -1)

  x <- c(-8, 99, 2, 5, -99)
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = F)
  colnames(tempdata) <- c("name", "age")
  expect_identical(test_age(tempdata, "age", -99), -3)

  colnames(tempdata) <- c("name", "sex")
  expect_error((test_age(tempdata, "age", -99)),
    "Column name does not exist",
    fixed = TRUE
  )

  x <- c(0, 11.5, "", 120, "noresponse")
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = F)
  colnames(tempdata) <- c("name", "AGE")
  expect_identical(test_age(tempdata, "AGE", "noresponse"), 0)

  x <- c(0, 11.5, "", 120, "noresponse")
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = F)
  colnames(tempdata) <- c("name", "age")
  expect_error((test_age(tempdata, "years", "noresponse")),
    "Column name does not exist",
    fixed = TRUE
  )

  x <- c(0, 11.5, "sh", 120, "noresponse")
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = F)
  colnames(tempdata) <- c("name", "age")
  expect_identical(test_age(tempdata, "age", "noresponse"), -4)
})
###############################################################################
context("testing gender")
test_that("test for gender checks for correct gender", {
  x <- c("F", "M", "m", "M")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "sex")
  # warning binrcpp version appeared and suppressing it
  expect_identical((test_gender(tempdata, c("f", "m"), "sex")), 0)
  x <- c("f", "f", "f", "f", 99)
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "gender")
  expect_identical((test_gender(tempdata, c("f", "m", 99), "gender", 99)), 0)
  x <- c("f", "f", "m", "m", 99)
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "gender")
  expect_identical(test_gender(tempdata, c("f", "m", 99), "gender", 99), 0)
  colnames(tempdata) <- c("name", "age")
  expect_error(test_gender(tempdata, c("f", "m", 99), "sex", 99),
    "Column name does not exist",
    fixed = TRUE)
  colnames(tempdata) <- c("name", "sex")
  expect_identical(test_gender(tempdata, c(1, 2), "sex", 99), -1)

  x <- c("f", "f", "male", "m", 99)
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "gender")
  expect_identical(test_gender(tempdata, c("f", "m", 99), "gender", 99), -1)

  x <- c("female", "female", "male", "male", 99)
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "gender")
  expect_identical(test_gender(
    tempdata, c("female", "male", 99),
    "gender", 99), 0)
})
# ############################################################################
context("testing get value from codes")
test_that("test  get value from codes", {
  data <- data.frame("sex" = c(1, 2, 2, 1, 1),
   "Name" = c("John", "Dora", "Dora", "John", "John"))
  list_codes_values <- list(c(1, 2), c("F", "M"))
  ans <- get_value_from_codes(data, column = "sex", nrcode = NA,
  list_codes_values)
  expect_equal(ans, c("F", "M", "M", "F", "F"))
  expect_error(get_value_from_codes(data, column = NULL, nrcode = NA,
                       list_codes_values))
  expect_error(get_value_from_codes(data, column = NA, nrcode = NA,
                                    list_codes_values))
  expect_error(get_value_from_codes(data, column = "sex", nrcode = NA,
                                    list_codes_values = NULL))
  expect_error(get_value_from_codes(NULL, column = "sex", nrcode = NA,
                                    list_codes_values))

  data <- data.frame("sex" = c(1, 2, 3, 1, 2),
                    "Name" = c("John", "Dora", "Dora", "John", "John"))
  list_codes_values <- list(c(1, 2, 3), c("F", "M", "Other"))
  ans <- get_value_from_codes(data, column = "sex", nrcode = NA,
                                     list_codes_values)
  expect_equal(ans, c("F", "M", "Other", "F", "M"))

  data <- data.frame("sex" = c(1, 2, 3, 1, NA),
                    "Name" = c("John", "Dora", "Dora", "John", "John"))
  list_codes_values <- list(c(1, 2, 3), c("F", "M", "Other"))
  ans <- get_value_from_codes(data, column = "sex", nrcode = NA,
                              list_codes_values)
  expect_equal(ans, c("F", "M", "Other", "F", NA))
})
# ############################################################################
context("testing column contents")
test_that("test column contents", {
  x <- c("f", "m", "m", "m")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "sex")
  expect_identical(test_column_contents(tempdata, "sex", c("f", "m")), 0)
  x <- c("m", "u", "m", "u", 99)
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "status")
  expect_identical(test_column_contents(tempdata, "status", c("m", "u", 99),
                                        99), 0)
  colnames(tempdata) <- c("name", "age")
  expect_error(test_column_contents(tempdata, "sex", c("f", "M"), 99),
    "Column name does not exist",
    fixed = TRUE)
  x <- c(1, 2, 3, 4, 1, 3, 99)
  y <- c(1, 2, 3, 4, 5, 6, 7)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "level")
  expect_identical(test_column_contents(
    tempdata, "level", c(1, 2, 3, 4, 5), 99), 0)

  expect_identical(test_column_contents(
    tempdata, "level", c(1, 2, 3), 99), -2)
})
# # ############################################################################
context("testing the column number for column name")
test_that("test for sex checks for correct gender", {
  x <- c("f", "female", "m", "male")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "sex")
  expect_equal(get_columnno_fornames(tempdata, "sex"), 2)
  x <- c("f", "female", "m", "male", 99)
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "gender")
  expect_error(get_columnno_fornames(tempdata, "sex"),
    "Column name does not exist",
    fixed = TRUE
  )
  colnames(tempdata) <- c("name", "age")
  expect_equal(get_columnno_fornames(tempdata, "age"), 2)
})
# ############################################################################
context("testing numeric column")
test_that("test for numeric values in a specific column", {
  x <- c(0, 11, 78, NA)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "dose")
  expect_identical(test_data_numeric("dose", tempdata, NA, 0, 200), 0)
  x <- c(0, 11, 78, 120)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "dose")
  expect_identical(test_data_numeric("dose", tempdata, 0, 0, 200), 0)
  x <- c(-8, 99, 2, 5, -99)
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "dose")
  expect_identical(test_data_numeric("dose", tempdata, -99, 0, 200), -2)

  x <- c("sheeja", 99, 2, 5, -99)
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "dose")
  expect_identical(test_data_numeric("dose", tempdata, -99, 0, 200), -3)

  colnames(tempdata) <- c("name", "sex")
  expect_error(test_data_numeric("dose", tempdata, -99, 0, 200),
    "Column name does not exist",
    fixed = TRUE
  )
})

# # # ########################################################################
context("testing numeric column")
test_that("test for numeric values in a column but with no range given", {
  x <- c(0, 11, 78, 120)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "dose")
  expect_identical(test_data_numeric_norange("dose", tempdata, 0), 0)
  x <- c(-8, 99, 2, 5, -99)
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "dose")
  expect_identical(test_data_numeric_norange("dose", tempdata, -99), 0)
  x <- c("sheeja", 99, 2, 5, -99)
  y <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "dose")
  expect_identical(test_data_numeric_norange("dose", tempdata, -99), -2)
  colnames(tempdata) <- c("name", "sex")
  expect_error(test_data_numeric_norange("dose", tempdata, -99),
    "Column name does not exist",
    fixed = TRUE
  )
})
# ############################################################################
context("testing string column with restriction on allowed entries")
test_that("test for string values in a specific column", {
  x <- c("F", "M", "cvb", "sheeja")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("num", "name")
  expect_identical(test_data_string_restriction(
    tempdata,
    "name", 0, c("F", "M")
  ), -2)
  x <- c("F", "M", "m", "m")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("num", "name")
  expect_identical(test_data_string_restriction(tempdata, "name", 0,
                                                c("F", "M")), 0)
  expect_identical(test_data_string_restriction(tempdata, "name", NA,
                                                c("F", "M")), 0)
  expect_identical(test_data_string_restriction(tempdata, "name", NA,
                                                c()), -3)

  colnames(tempdata) <- c("num", "sex")
  expect_error(test_data_string_restriction(
    tempdata, "name", -99,
    c()
  ), "Column name does not exist", fixed = TRUE)
  x <- c("F", "M", 100, "m")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("num", "name")
  expect_identical(test_data_string_restriction(
    tempdata, "name", 0,
    c("F", "M")
  ), -5)
})
# # ###########################################################################
context("testing string column")
test_that("test for string values in a specific column", {
  x <- c("F", "M", "cvb", "sheeja")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("num", "name")
  expect_identical(test_data_string(tempdata, "name", 0), 0)
  expect_identical(test_data_string(tempdata, "name", "NA"), 0)
  x <- c("F", "M", "cvb", -99)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("num", "name")
  expect_identical(test_data_string(tempdata, "name", -99), 0)
  x <- c("F", "M", "cvb", NA)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("num", "name")
  expect_identical(test_data_string(tempdata, "name", -99), 0)
  x <- c("F", "M", 100, NA)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("num", "name")
  expect_identical(test_data_string(tempdata, "name", -99), -2)
  colnames(tempdata) <- c("num", "sex")
  expect_error(test_data_string(tempdata, "name", -99),
    "Column name does not exist",
    fixed = TRUE
  )
})
# ############################################################################
context("testing part of columnmames")
test_that("testing part of columnmames", {
  x <- c("F", "M", "cvb", "sheeja")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("num.x", "name_x")
  expect_equal(get_colno_pattern_colname("name", colnames(tempdata)), 2)
  colnames(tempdata) <- c("num.x", "name_x")
  expect_identical(get_colno_pattern_colname("sex", colnames(tempdata)), -1)
  expect_identical(get_colno_pattern_colname("", colnames(tempdata)), -1)
  colnames(tempdata) <- c("num.x", "num_x")
  expect_equal(get_colno_pattern_colname("num", colnames(tempdata)), c(1, 2))
})
# # ##########################################################################
context("testing descriptive statistics")
test_that("testing descriptive statistics", {
  x <- c(0, 11, 78, 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "age")
  results <- matrix(c(
    "249", "62.25", "73.722", "44.5", "0", "36.861", "0", "160", "0 - 160", "4",
    "8.25", "98.5", "0.825", "153.85", "0"), nrow = 1, byrow = TRUE)
  colnames(results) <- c(
    "Sum", "Mean", "SD", "Median", "Mode", "SE",
    "Minimum", "Maximum", "Range", "Count", "LQ", "UQ", "CIlow",
    "CIhigh", "MissingCount"
  )
  rownames(results) <- "age"
  expect_equal(descriptive_stats_col_excl_nrcode(tempdata, "age", NA), results)
 })

context("testing descriptive statistics")
test_that("testing descriptive statistics", {
  x <- c(0, 11, 78, 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "age")
  results <- matrix(c(
    "249", "83", "74.626", "78", "11", "43.085",
    "11", "160", "11 - 160", "3", "44.5", "119", "14.35", "155.9", "1"
  ), nrow = 1, byrow = TRUE)
  colnames(results) <- c(
    "Sum", "Mean", "SD", "Median", "Mode", "SE",
    "Minimum", "Maximum", "Range", "Count", "LQ", "UQ", "CIlow",
    "CIhigh", "MissingCount"
  )
  rownames(results) <- "age"
  expect_equal(descriptive_stats_col_excl_nrcode(tempdata, "age", 0), results,
               tolerance = 0.001)
})
context("testing descriptive statistics")
test_that("testing descriptive statistics", {
  x <- c(0, NA, 78, 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "age")
  results <- matrix(c(
    "238", "79.333", "80.008", "78", "0", "46.193", "0", "160", "0 - 160",
    "3", "39", "119", "3.9", "155.9", "1"), nrow = 1, byrow = TRUE)
  colnames(results) <- c(
    "Sum", "Mean", "SD", "Median",
    "Mode", "SE", "Minimum", "Maximum", "Range", "Count", "LQ", "UQ",
    "CIlow", "CIhigh", "MissingCount"
  )
  rownames(results) <- "age"
  expect_equal(descriptive_stats_col_excl_nrcode(tempdata, "age", NA),
    results)
})

context("testing descriptive statistics")
test_that("testing descriptive statistics", {
  x <- c(0, NA, "dd", 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "age")
  expect_error(descriptive_stats_col_excl_nrcode(tempdata, "age", NA),
    "Error - column contents not numeric",
    fixed = TRUE
  )
})
context("testing descriptive statistics")
test_that("testing descriptive statistics", {
  x <- c(0, 11, 78, 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "dose")
  expect_error(descriptive_stats_col_excl_nrcode(tempdata, "age", NA),
    "Error - no column or column name different",
    fixed = TRUE
  )
})
# ############################################################################
context("testing keep required variables")
test_that("testing keep required variables", {
  the_data <- data.frame("Age" = c(21, 15), "sex" = c("m", "f"))
  ans <- keep_required_columns("Age", the_data)
  expect_equal(ans$Age, c(21, 15))
  expect_error(keep_required_columns("num", the_data))
  ans <- keep_required_columns(c("Age", NULL), the_data)
  expect_equal(ans$Age, c(21, 15))
  expect_error(keep_required_columns(NA, the_data))
  expect_error(keep_required_columns(c(NULL), the_data))
  expect_error(keep_required_columns(NULL, the_data))
})

# ############################################################################
context("testing mode function")
test_that("testing mode function", {
  x <- c(0, 11, 78, 160)
  expect_equal(get_mode_from_vector(x), 0)
})
context("testing mode function")
test_that("testing mode function", {
  x <- c(0, "f", 78, 160)
  expect_identical(get_mode_from_vector(x), -1)
})
context("testing mode function")
test_that("testing mode function", {
  x <- c(78, NA, 78, 78)
  expect_equal(get_mode_from_vector(x), 78)
})
context("testing mode function")
test_that("testing mode function", {
  x <- c(78, "NA", 78, 78)
  expect_identical(get_mode_from_vector(x), -1)
})
# ############################################################################
context("testing column number from pattern")
test_that("testing number from pattern", {
  expect_equal(check_col_pattern_colname("dd", "female_age"), FALSE)
  expect_equal(check_col_pattern_colname("age", "female_age"), TRUE)
  expect_equal(check_col_pattern_colname(12, "12age"), TRUE)
})
# ############################################################################
context("testing column existence")
test_that("testing column existence", {
  x <- c(0, NA, "dd", 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "age")
  expect_equal(check_column_exists("age", tempdata), 0)
})
context("testing column existence")
test_that("testing column existence", {
  x <- c(0, NA, "dd", 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "num")
  expect_equal(check_column_exists("age", tempdata), -1)
})
# ############################################################################
context("testing column existence")
test_that("testing column existence", {
  x <- c(0, NA, "dd", 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "age")
  expect_equal(check_column_exists("age", tempdata), 0)
})
context("testing column existence")
test_that("testing column existence", {
  x <- c(0, NA, "dd", 160)
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x))
  colnames(tempdata) <- c("name", "num")
  expect_equal(check_column_exists("age", tempdata), -1)
})
# ############################################################################
context("testing returning a subgroup with NA")
test_that("testing returning a subgroup with NA", {
  x <- c(0, 11, 78, 160)
  y1 <- c("f", "m", "f", NA)
  y2 <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y2, y1, x))
  colnames(tempdata) <- c("num", "gender", "mark")
  subgp <- tempdata[is.na(tempdata$gender), ]
  expect_equal(return_subgroup_withNA(tempdata, "gender", NA), subgp)

  x <- c(0, 11, 78, 160)
  y1 <- c("f", "m", "f", NA)
  y2 <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y2, y1, x))
  colnames(tempdata) <- c("num", "vv", "mark")
  expect_error(return_subgroup_withNA(tempdata, "gender", "f"))
})
# ############################################################################
context("testing returning a subgroup omitting NA")
test_that("testing returning a subgroup omitting NA", {
  x <- c(0, 11, 78, 160)
  y1 <- c("f", "m", "f", "m")
  y2 <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y2, y1, x))
  colnames(tempdata) <- c("num", "gender", "mark")
  subgp <- tempdata[tempdata$gender == "f" & !is.na(tempdata$gender), ]
  expect_equal(return_subgroup_omitna(tempdata, "gender", "f"), subgp)
})
context("testing returning a subgroup omitting NA")
test_that("testing returning a subgroup omitting NA", {
  x <- c(0, 11, 78, 160)
  y1 <- c("f", NA, "f", "m")
  y2 <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y2, y1, x))
  colnames(tempdata) <- c("num", "gender", "mark")
  subgp <- tempdata[tempdata$gender == "f" & !is.na(tempdata$gender), ]
  expect_equal(return_subgroup_omitna(tempdata, "gender", "f"), subgp)
})
context("testing returning a subgroup omitting NA")
test_that("testing returning a subgroup omitting NA", {
  x <- c(0, 11, 78, 160)
  y1 <- c("f", NA, "f", "m")
  y2 <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y2, y1, x))
  colnames(tempdata) <- c("num", "desc", "mark")
  expect_error(return_subgroup_omitna(tempdata, "gender", "f"),
    "Data does not contain the column with the specfied column name",
    fixed = TRUE
  )
})
# ############################################################################
context("testing representing categorical data while excluding missing data")
test_that("testing represnting categorical data  excluding missing data", {
  x <- c(0, 11, 78, 160)
  y1 <- c("f", "m", "f", "m")
  y2 <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y2, y1, x))
  colnames(tempdata) <- c("num", "gender", "mark")
  ans <- matrix(c(2, 50, 2, 50), ncol = 2)
  colnames(ans) <- c("F", "M")
  rownames(ans) <- c("Number", "Percentage")
  expect_equal(represent_categorical_data_exclude_missing(tempdata, "gender",
                                                          NA), ans)
  colnames(tempdata) <- c("num", "a", "mark")
  expect_error(represent_categorical_data_exclude_missing(tempdata, "gender",
                                                          NA),
    "Data does not contain the column with the specfied column name",
    fixed = TRUE
  )

  x <- c(0, 11, 78, 10)
  y1 <- c("f", "m", NA, "m")
  y2 <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y2, y1, x))
  colnames(tempdata) <- c("num", "gender", "age")
  ans <- matrix(c(1, 25, 2, 50), ncol = 2)
  colnames(ans) <- c("F", "M")
  rownames(ans) <- c("Number", "Percentage")
  expect_equal(represent_categorical_data_exclude_missing(tempdata, "gender",
                                                          -99), ans)

  x <- c(0, 11, 3, 99)
  y1 <- c("f", "m", "NA", "m")
  y2 <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y2, y1, x))
  colnames(tempdata) <- c("num", "gender", "age")
  ans <- matrix(c(1, 25, 2, 50, 1, 25), ncol = 3)
  colnames(ans) <- c("F", "M", "NA")
  rownames(ans) <- c("Number", "Percentage")
  expect_equal(represent_categorical_data_exclude_missing(tempdata, "gender",
                                                          NA), ans)
})

# ############################################################################
context("testing representing categorical data while including missing data")
test_that("testing representing categorical data including missing data", {
  x <- c(0, 11, 78, 160, NA)
  y1 <- c("f", "m", "f", "m", NA)
  y2 <- c(1, 2, 3, 4, 5)
  tempdata <- as.data.frame(cbind(y2, y1, x))
  colnames(tempdata) <- c("num", "gender", "mark")
  ans <- matrix(c(2, 40, 2, 40, 1, 20), ncol = 3)
  colnames(ans) <- c("F", "M", "NA")
  rownames(ans) <- c("Number", "Percentage")
  expect_equal(represent_categorical_data_include_missing(tempdata, "gender",

                                                          NA), ans)
  colnames(tempdata) <- c("num", "a", "mark")
  expect_error(represent_categorical_data_include_missing(tempdata, "gender",
                                                          NA),
               "Data does not contain the column with the specfied column name",
               fixed = TRUE)
 })
# ############################################################################
context("testing representing categorical data from subgroup")
test_that("testing representing categorical data from subgroup", {
  this.df <- data.frame(c(11, 78, 22), c("m", "f", "f"), c(1, 2, 2),
                        stringsAsFactors = FALSE)
  colnames(this.df) <- c("mark", "gender", "group")
  ans <- represent_numerical_data_forsubgroups(this.df, "group",
                                                 "mark", NA)
  expect_error(represent_numerical_data_forsubgroups(this.df, NULL,
                                        "mark", NA))
  expect_error(represent_numerical_data_forsubgroups(this.df, "group",
                                                     "xx", NA))
})
# ############################################################################
context("testing representing categorical data from subgroup")
test_that("testing representing categorical data from subgroup", {
  this.df <- data.frame(c(11, 78, 22), c("m", "f", "f"), c(1, 2, 2),
                        stringsAsFactors = FALSE)
  colnames(this.df) <- c("mark", "gender", "group")
  ans <- represent_categorical_data_forsubgroups(this.df, "group",
                                                 "gender", NA)
  expect_error(represent_categorical_data_forsubgroups(this.df, NULL,
                                                       "gender", NA))

  this.df <- data.frame(c(11, 78, 22), c("m", "f", "f"), c(1, 2, 2),
                        stringsAsFactors = FALSE)
  colnames(this.df) <- c("mark", "vv", "group")
  expect_error(represent_categorical_data_forsubgroups(this.df, "group",
                                                       "gender", NA))


  this.df <- data.frame(c(11, 78, 22, 22, 33), c("m", "f", "f", "m", NA),
                        c(1, 1, 2, 2, 2), stringsAsFactors = FALSE)
  colnames(this.df) <- c("mark", "gender", "group")
  ans <- represent_categorical_data_forsubgroups(this.df, "group",
                                                       "gender", NA)

  this.df <- data.frame(c(11, 78, 22, 22, 33), c("m", "f", "f", "f", NA),
                        c(1, 1, 2, 2, 2), stringsAsFactors = FALSE)
  colnames(this.df) <- c("mark", "gender", "group")
  ans <- represent_categorical_data_forsubgroups(this.df, "group",
                                                 "gender", NA)
})
# ############################################################################
context("testing cohens d")
test_that("testing cohens d", {
  x <- c(0, 11, 78, 160)
  y <- c(1, 2, 3, 4)
  g1mean <- mean(x)
  g2mean <- mean(y)
  sdpooled <- sqrt((sd(x)^2 + sd(y)^2) / 2)
  ans <- abs(g2mean - g1mean) / sdpooled
  res <- cohensd(x, y)[1]
  expect_equal(res, ans)
  x <- c(0, 11, 78, NA)
  expect_error(cohensd(x, y), "Vector contains atleast one NA or string",
    fixed = TRUE
  )
  x <- c(0, 11, 78, "sh")
  expect_error(cohensd(x, y), "Vector contains atleast one NA or string",
    fixed = TRUE
  )
})
# #############################################################################
context("testing standard error of mean function")
test_that("testing standard error of mean function", {
  x <- c(0, 11, 78, 160)
  semhere <- sd(x) / sqrt(length(x))
  res <- get_sem(x)
  expect_equal(res, semhere)
  x <- c(0, 11, 78, "sh")
  expect_error(get_sem(x), "Vector contains non numeric data", fixed = TRUE)
  x <- c(0, 11, 78, "160")
  expect_equal(res, semhere)
})
###############################################################################
context("testing age calculated from date of birth")
test_that("testing age calculated from date of birth", {
  x <- c("1987-05-28", "1987-06-18", "0", "1987-07-09")
  y <- c(1, 2, 3, 4)
  tempdata <- data.frame(cbind(y, x), stringsAsFactors = FALSE)
  colnames(tempdata) <- c("name", "dob")
  ag1 <- eeptools::age_calc(as.Date("1987-05-28"), units = "years")
  ag2 <- eeptools::age_calc(as.Date("1987-06-18"), units = "years")
  ag3 <- eeptools::age_calc(as.Date("1987-07-09"), units = "years")
  ages <- c(ag1, ag2, 0, ag3)
  mod_data <- calculate_age_from_dob(tempdata, "dob", NULL, "ymd", 0)$age
  expect_equivalent(ages, mod_data, tolerance = 0.001)
  mod_data <- calculate_age_from_dob(tempdata, "dob", NA, "ymd", 0)$age
  expect_equivalent(ages, mod_data, tolerance = 0.001)

  x <- c("1770-05-28", "1987-06-18", "0", "1987-07-09")
  y <- c(1, 2, 3, 4)
  tempdata <- data.frame(cbind(y, x), stringsAsFactors = FALSE)
  colnames(tempdata) <- c("name", "dob")
  expect_error(calculate_age_from_dob(tempdata, "dob",
                                                  NULL, "ymd", 0))

  x <- c("1947-05-28", "1957-06-18", "0", "1967-07-09")
  y <- c(1, 2, 3, 4)
  y2 <- c("1990-05-28", "1990-06-18", "0", "1990-07-09")
  tempdata <- data.frame(cbind(y, x, y2), stringsAsFactors = FALSE)
  colnames(tempdata) <- c("name", "dob", "start")
  ages <- c(43, 33, 0, 23)
  mod_data <- calculate_age_from_dob(tempdata, "dob", "start", "ymd", 0)$age
  expect_equivalent(ages, mod_data, tolerance = 0.001)

  x <- c("1987-05-28", "1987-06-18", "0", "1987-07-09")
  y <- c(1, 2, 3, 4)
  tempdata <- data.frame(cbind(y, x), stringsAsFactors = FALSE)
  colnames(tempdata) <- c("name", "dob")
  ag1 <- eeptools::age_calc(as.Date("1987-05-28"), units = "years")
  ag2 <- eeptools::age_calc(as.Date("1987-06-18"), units = "years")
  ag3 <- eeptools::age_calc(as.Date("1987-07-09"), units = "years")
  ages <- c(ag1, ag2, 0, ag3)
  mod_data <- calculate_age_from_dob(tempdata, "dob", NULL, "ymd", 0)$age
  expect_equivalent(ages, mod_data, tolerance = 0.001)


  x <- c("1287-05-28", "1987-06-18", NA, "1987-07-09")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = FALSE)
  colnames(tempdata) <- c("name", "dob")
  expect_error(calculate_age_from_dob(tempdata, "dob", "ymd", NA))

  x <- c("1987-05-28", "1987-06-18", NA, "1987-07-09")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = FALSE)
  colnames(tempdata) <- c("name", "dob")
  mod_data <- calculate_age_from_dob(
    tempdata, "dob", NULL, "ymd", NA)$age
  ages <- c(ag1, ag2, NA, ag3)
  expect_equivalent(ages, mod_data, tolerance = 0.001)

  x <- c("1987-05-28", "1987-06-18", NA, "1987-07-09")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = FALSE)
  colnames(tempdata) <- c("name", "dob")
  mod_data <- calculate_age_from_dob(tempdata, "dob", NULL, "ymd", NA
  )$age
  ages <- c(ag1, ag2, NA, ag3)
  expect_equivalent(ages, mod_data, tolerance = 0.001)

  x <- c("28/05/1987", "18/06/1987", NA, "9/7/1987")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = FALSE)
  colnames(tempdata) <- c("name", "dob")
  mod_data <- calculate_age_from_dob(
    tempdata, "dob", NULL,
    "dmy", NA
  )$age
  ages <- c(ag1, ag2, NA, ag3)
  expect_equivalent(ages, mod_data, tolerance = 0.001)

  x <- c("05/28/1987", "06/18/1987", NA, "7/9/1987")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = FALSE)
  colnames(tempdata) <- c("name", "dob")
  mod_data <- calculate_age_from_dob(
    tempdata, "dob", NULL,
    "mdy", NA
  )$age
  ages <- c(ag1, ag2, NA, ag3)
  expect_equivalent(ages, mod_data, tolerance = 0.001)


  x <- c("1997-05-28", "1987-06-18", NA, "2015-07-09")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = FALSE)
  colnames(tempdata) <- c("name", "dob")
  ag1 <- eeptools::age_calc(as.Date("1997-05-28"), units = "years")
  ag2 <- eeptools::age_calc(as.Date("1987-06-18"), units = "years")
  ag3 <- eeptools::age_calc(as.Date("2015-07-09"), units = "years")
  ages <- c(ag1, ag2, NA, ag3)
  mod_data <- calculate_age_from_dob(tempdata, "dob", NULL, "ymd", NA)$age
  expect_equivalent(ages, mod_data, tolerance = 0.001)

  x <- c("1997 May 28", "1987-June-18", NA, "2015/July/09")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = FALSE)
  colnames(tempdata) <- c("name", "dob")
  mod_data <- calculate_age_from_dob(tempdata, "dob", NULL, "ymd", NA)$age
  expect_equivalent(ages, mod_data, tolerance = 0.001)

  colnames(tempdata) <- c("name", "date")
  expect_error(calculate_age_from_dob(tempdata, "dob", NULL, "ymd", NA),
    "Column name does not exist",
    fixed = TRUE
  )
})
# ############################################################################
context("testing the unique contents of a column")
test_that("testing the unique contents of a column", {
  x <- c("f", "m", "m", "m")
  y <- c(1, 2, 3, 4)
  tempdata <- as.data.frame(cbind(y, x), stringsAsFactors = FALSE)
  colnames(tempdata) <- c("number", "sex")
  expect_identical(get_contents_cols(tempdata, "sex"), c("f", "m"))
  expect_identical(get_contents_cols(tempdata, "number"), c(1, 2, 3, 4))
  expect_error(get_contents_cols(tempdata, "gender"),
            "Data does not contain the column with the specfied column name",
            fixed = TRUE)
})
# ############################################################################
context("testing presenting mean and ad after removing NA")
test_that("testing presenting mean and ad after removing NA", {
  this_data <- data.frame(
    "age" = c(21, 15),
    "Name" = c("John", "Dora")
  )
  this_res <-  "18 (4.24)"
  expect_equal(present_mean_sd_rmna_text(this_data, "age", NA), this_res)
 })
# ############################################################################
context("testing representing categorical data")
test_that("testing representing categorical data", {
  this.df <- data.frame(c(11, 78), c("m", "f"), stringsAsFactors = FALSE)
  colnames(this.df) <- c("mark", "gender")
  result <- represent_categorical_data_exclude_missing(this.df, "gender", NA)
  expect_equal(result[1], 1)
})
# ############################################################################
context("testing representing categorical text data")
test_that("testing representing categorical text data", {

  df <- data.frame(c(11, 78), c("m", "f"), stringsAsFactors = FALSE)
  colnames(df) <- c("mark", "gender")
  result <- represent_categorical_textdata(df, "gender", NA)
  expect_equal(unname(result[1]), "1 (50)")
})
# ############################################################################
context("testing convert date numeric form to std form")
test_that("testingconvert date numeric form to std form", {
  result <- convert_date_numeric_stdform(c("01/01/2000", "02/02/2002"),
                                         c(1, 2), "dmy")
  expect_equal(unname(result[1]), "2000-1-1")
})

# ############################################################################
context("testing convert date string form to std form")
test_that("testingconvert date sting form to std form", {
  result <- convert_date_string_stdform("Feb-1-2020", "mdy")
  expect_equal(unname(result[1]), "2020-2-1")
  expect_error(convert_date_string_stdform("Feb-30-2020", "mdy"))
  expect_error(convert_date_string_stdform("April-31-2020", "mdy"))
  expect_error(convert_date_string_stdform("Jan-34-2020", "mdy"))
  expect_error(convert_date_string_stdform("Feb-29-2017", "mdy"))
  expect_error(convert_date_string_stdform("dcc-29-2017", "mdy"))

  result <- convert_date_string_stdform("2020-1-Feb", "ydm")
  expect_equal(unname(result[1]), "2020-2-1")
  result <- convert_date_string_stdform("2020-Feb-1", "ymd")
  expect_equal(unname(result[1]), "2020-2-1")
  expect_error(convert_date_string_stdform("sh-Feb-1", "ymd"))
  expect_error(convert_date_string_stdform("2020-Feb-sh", "ymd"))
  expect_error(convert_date_string_stdform("2020-sh-Feb", "ydm"))
  expect_error(convert_date_string_stdform("ss-Feb-1", "ydm"))

  expect_error(convert_date_string_stdform("Feb-2020-sh", "myd"))
  expect_error(convert_date_string_stdform("Feb-ss-1", "myd"))
  result <- convert_date_string_stdform("Feb 8, 2020", "mdy")
  expect_equal(unname(result[1]), "2020-2-8")
  result <- convert_date_string_stdform("2020, Feb 8", "ymd")
  expect_equal(unname(result[1]), "2020-2-8")
  result <- convert_date_string_stdform("2020 Feb, 8", "ymd")
  expect_equal(unname(result[1]), "2020-2-8")
  result <- convert_date_string_stdform("2020 Feb 8,", "ymd")
  expect_equal(unname(result[1]), "2020-2-8")
  expect_error(convert_date_string_stdform("Feb-2020-12-1", "myd"))

  expect_error(convert_date_string_stdform("s2-Feb-2020", "dmy"))
  expect_error(convert_date_string_stdform("12-Feb-sh", "dmy"))
  result <- convert_date_string_stdform("8 Feb 2020", "dmy")
  expect_equal(unname(result[1]), "2020-2-8")

  expect_error(convert_date_string_stdform("12-2020-sh", "dym"))
  expect_error(convert_date_string_stdform("sh-2020-Feb", "dym"))
  expect_error(convert_date_string_stdform("12-sh-Feb", "dym"))
  result <- convert_date_string_stdform("Feb 2020 8", "myd")
  expect_equal(unname(result[1]), "2020-2-8")

  expect_error(convert_date_string_stdform("ss-12-Feb", "ydm"))
  expect_error(convert_date_string_stdform("Feb-ss-2020", "mdy"))
  expect_error(convert_date_string_stdform("Feb-12-ss", "mdy"))
})
# ############################################################################
context("testing getting summary from gtsummary")
test_that("testing getting summary from gtsummary", {
  y <- c(10, 20.4, 32, 43)
  x <- c("f", "m", "m", "m")
  tempdata <- data.frame(y, x, stringsAsFactors = FALSE)

  colnames(tempdata) <- c("mark", "sex")
  summary_tempdata <- get_summary_gtsummary(tempdata, c("mark", "sex"),
                                            byvar = NULL)
  expect_equal(summary_tempdata$N, 4)
  my_data <- gtsummary::trial

  this <- get_summary_gtsummary(my_data, selectvar = c("trt", "age", "marker", "response"),
          byvar = "trt")
  expect_equal(this$N, 200)


  expect_error(get_summary_gtsummary(my_data, selectvar = c("age", "grade"),
                        byvar = "trt", label = grade ~ "Tumor Grade"))

  expect_error(get_summary_gtsummary(NULL, selectvar = c("trt", "age", "grade"),
                      byvar = "trt", label = grade ~ "Tumor Grade"))
  expect_error(get_summary_gtsummary(my_data, selectvar = NULL,
                        byvar = "trt", label = grade ~ "Tumor Grade"))
  expect_error(get_summary_gtsummary(my_data, selectvar = NA,
                                     byvar = "trt", label = grade ~ "Tumor Grade"))
})
# ############################################################################
context("testing for returning the longitudinal summary")
test_that("testing for returning the longitudinal summary", {
  test_data <- as.data.frame(cbind(c(1,2,3,4,5), c(20,40,60,80,100),
                                   c("F", "F", "M", "M", "F")))
  colnames(test_data) <- c("no", "marks", "gender")
  test_data$marks <- as.numeric(test_data$marks)
  results <- return_longitudinal_summary(test_data, "marks", NA)
  expect_equal(results$means, 60)
  expect_error( return_longitudinal_summary(test_data, "gender", NA))
  expect_error( return_longitudinal_summary(test_data, "gen", NA))
})

