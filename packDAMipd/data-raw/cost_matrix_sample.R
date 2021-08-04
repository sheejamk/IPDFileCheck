## cost data for testing
file <- "data-raw/cost_matrix_sample.csv"

cost_data <- read.csv(file, stringsAsFactors = F)

# Create .rda object for trial_data in data folder
usethis::use_data(cost_data, overwrite = TRUE)
