## code to prepare `promis 3a scoring table` dataset goes here
promis3a_scoring <- "data-raw/promis3a_scoring.csv"

promis3a_scoring <- read.csv(file = promis3a_scoring, stringsAsFactors = F)

# Create .rda object for trial_data in data folder
usethis::use_data(promis3a_scoring, overwrite = T)

