## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(IPDFileCheck)

## ------------------------------------------------------------------------
 set.seed(17)
 rctdata <- data.frame(age=abs(rnorm(10, 60, 20)),
                           sex=c("M", "F","M", "F","M", "F","M", "F","F","F"),
                           yob=sample(seq(1930,2000), 10, replace=T),
                           dob=c("07/12/1969","16/02/1962","03/09/1978","17/02/1969",                                      "25/11/1960","17/04/1970","18/03/1997","30/01/1988",
                                               "03/02/1990","25/09/1978"),
                           arm=c("Control", "Intervention","Control", "Intervention","Control", "Intervention","Control", "Intervention","Control", "Intervention"),stringsAsFactors = FALSE)
 
rctdata_error <- data.frame(age=runif(10, -60, 120),
                           sex=c("M", "F","M", "F","M", "F","M", "F","F","F"),
                           yob=sample(seq(1930,2000), 10, replace=T),
                           dob=c("1997 May 28","1987-June-18",NA,"2015/July/09","1997 May 28","1987-June-18",NA,"2015/July/09","1997 May 28","1987-June-18"),
                           arm=c("Control", "Intervention","Control", "Intervention","Control", "Intervention","Control", "Intervention","Control", "Intervention"),stringsAsFactors = FALSE)

## ------------------------------------------------------------------------
  thisfile = system.file("extdata", "blank.txt", package = "IPDFileCheck")
  test_file_exist_read(thisfile)

## ------------------------------------------------------------------------
check_column_exists("sex",rctdata)

## ------------------------------------------------------------------------
get_columnno_fornames(rctdata,"sex")

## ------------------------------------------------------------------------
test_column_contents(rctdata,"sex",c("M","F"),NA)
test_column_contents(rctdata,"sex",c("M","F"))

## ------------------------------------------------------------------------
test_columnnames(c("age","sex","dob","yob","arm"),rctdata)

## ------------------------------------------------------------------------
test_age(rctdata,"age",NA)

## ------------------------------------------------------------------------
test_gender(rctdata,c("M","F"),"sex",NA)

## ------------------------------------------------------------------------
test_data_numeric("age",rctdata,NA,0,100)

## ------------------------------------------------------------------------
test_data_numeric_norange("age",rctdata,NA)
test_data_numeric_norange("yob",rctdata,NA)

## ------------------------------------------------------------------------
test_data_string(rctdata,"arm",NA)

## ------------------------------------------------------------------------
test_data_string_restriction(rctdata,"arm",NA,c("Intervention","Control"))
test_data_string_restriction(rctdata,"sex",NA,c("M","F"))


## ------------------------------------------------------------------------
get_colno_pattern_colname("ob",colnames(rctdata))


## ------------------------------------------------------------------------
descriptive_stats_col(rctdata, "age")

## ------------------------------------------------------------------------
present_mean_sd_rmna_text(rctdata, "age")

## ------------------------------------------------------------------------
return_subgroup_omitna(rctdata, "sex","F")
return_subgroup_omitna(rctdata, "arm","control")

## ------------------------------------------------------------------------
represent_categorical_textdata(rctdata, "sex",NA)
represent_categorical_textdata(rctdata, "arm",NA)

## ------------------------------------------------------------------------
calculate_age_from_dob(rctdata,"dob","%d/%m/%y",NA)

## ------------------------------------------------------------------------
calculate_age_from_year(rctdata,"yob",NA)

