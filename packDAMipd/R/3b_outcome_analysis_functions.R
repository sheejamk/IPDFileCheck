##############################################################################
#' Function to return mean age from a data frame
#' @param this_data the data containing column with age
#' @param age_nrcode non response code
#' @return mean and sd, if success -1, if failure
#' @examples
#' this_data <- as.data.frame(cbind(num = c(1, 2, 3, 4),
#' age = c(14, 25, 26, 30)))
#' get_mean_sd_age(this_data, NA)
#' @export
#' @details
#' Age data is complete with the nr code given and get the mean and sd
get_mean_sd_age <- function(this_data, age_nrcode) {
  # Assumption is that age data is complete or incomplete data
  # is denoted by empty entry or a valid non response code.
  # if age format is not right throw error
  #Error - data should not be NULL
  if (is.null(this_data))
    stop("data should not be NULL")
  age_details <- get_age_details(this_data)
  if (IPDFileCheck::test_age(this_data, age_details$name, age_nrcode) != 0) {
    stop("Error - age data format")
  } else {
    # else read the age column
    age_data <- this_data[[age_details$name]]
    # if any missing values, ignore them
    if (!is.na(age_nrcode)) {
      age_data <- age_data[age_data != age_nrcode]
    }
    age_data <- age_data[age_data != " "]
    meanage <- mean(as.numeric(age_data[!is.na(age_data)]))
    sdage <- stats::sd(as.numeric(age_data[!is.na(age_data)]))
    results <- list(mean = meanage, sd = sdage)
    return(results)
  }
}
##############################################################################
#' Function to add EQ5D3L scores to IPD data
#' @param ind_part_data a dataframe
#' @param eq5d_nrcode non response code for EQ5D3L, default is NA
#' @return qaly included modified data, if success -1, if failure
#' @examples
#' datafile <- system.file("extdata", "trial_data.csv", package = "packDAMipd")
#' trial_data <- load_trial_data(datafile)
#' value_eq5d5L_IPD(trial_data, NA)
#' @export
#' @source
#' http://eprints.whiterose.ac.uk/121473/1/Devlin_et_al-2017-Health_Economics.pdf
value_eq5d3L_IPD <- function(ind_part_data, eq5d_nrcode) {
  #Error - data should not be NULL
  if (is.null(ind_part_data))
    stop("data should not be NULL")
  ind_part_data <- data.frame(ind_part_data)
  # get the eq5d details and time point
  eq5d_details <- get_eq5d_details(ind_part_data)
  eq5d_columnnames <- eq5d_details$name
  timepoint_details <- get_timepoint_details(ind_part_data)
  # get the number of time points
  if (sum(is.na(timepoint_details)) == 0) {
    timepointscol <- timepoint_details$name
    timepoints <- unique(ind_part_data[[timepointscol]])
    nooftimepoints <- length(timepoints)
  } else {
    timepointscol <- NA
    timepoints <- NA
    nooftimepoints <- 1
  }
  # get the rows for the time points identified
  for (j in 1:nooftimepoints) {
    if (is.na(timepointscol) || timepointscol == "NA") {
      rows_needed <- seq(1:nrow(ind_part_data))
    } else {
      rows_needed <- which(ind_part_data[[timepointscol]] == timepoints[j])
    }
    # pick the responses assumes the order
    eq5d_responses <- ind_part_data[rows_needed, eq5d_columnnames]
    # Check if the responses are numeric with range 1 to 3
    results <- sapply(eq5d_columnnames, IPDFileCheck::test_data_numeric,
                      eq5d_responses, eq5d_nrcode, 1, 3)
    if (any(results < 0)) {
      stop("eq5d responses do not seem right")
    } else {
      if (is.na(eq5d_nrcode)) {
        original_ind <- c()
        ind <- c()
        for (i in 1:ncol(eq5d_responses)) {
          which_one <-  which(is.na(eq5d_responses[[i]]))
          original_ind <- append(original_ind, rows_needed[which_one])
          ind <- append(ind, which_one)
        }
      } else {
        ind <- c()
        original_ind <- c()
        for (i in 1:ncol(eq5d_responses)) {
          which_one <- which(eq5d_responses[[i]] == eq5d_nrcode)
          original_ind <- append(original_ind, rows_needed[which_one])
          ind <- append(ind, which_one)
        }
      }
      ind <- sort(unique(ind))
      original_ind <-  sort(unique(original_ind))
      if (length(ind > 0))
        eq5d_responses <- eq5d_responses[-ind, ]
      index3L <- rep(0, nrow(eq5d_responses))
      for (i in seq(nrow(eq5d_responses))) {
        index3L[i] <- valueEQ5D::value_3L_Ind(
          "UK", "TTO", eq5d_responses[i, 1],
          eq5d_responses[i, 2], eq5d_responses[i, 3],
          eq5d_responses[i, 4], eq5d_responses[i, 5]
        )
      }
      new_colname <- paste("EQ5D3LIndex")
      if (length(ind > 0))
        rows_needed <- rows_needed[-ind]
      ind_part_data[rows_needed, new_colname] <- index3L
      if (length(original_ind > 0))
        ind_part_data[original_ind, new_colname] <- eq5d_nrcode
    }
  }
  return(ind_part_data)
}
##############################################################################
#' Function to add EQ5D5L scores to IPD data
#' @param ind_part_data a dataframe
#' @param eq5d_nrcode non response code for EQ5D5L, default is NA
#' @return qaly included modified data, if success -1, if failure
#' @examples
#' datafile <- system.file("extdata", "trial_data.csv", package = "packDAMipd")
#' trial_data <- load_trial_data(datafile)
#' value_eq5d5L_IPD(trial_data, NA)
#' @export
#' @source
#' http://eprints.whiterose.ac.uk/121473/1/Devlin_et_al-2017-Health_Economics.pdf
value_eq5d5L_IPD <- function(ind_part_data, eq5d_nrcode) {
  #Error - data should not be NULL
  if (is.null(ind_part_data))
    stop("data should not be NULL")
  ind_part_data <- data.frame(ind_part_data)
  # get the eq5d details and time point
  eq5d_details <- get_eq5d_details(ind_part_data)
  eq5d_columnnames <- eq5d_details$name
  timepoint_details <- get_timepoint_details(ind_part_data)
  # get the number of time points
  if (sum(is.na(timepoint_details)) == 0) {
    timepointscol <- timepoint_details$name
    timepoints <- unique(ind_part_data[[timepointscol]])
    nooftimepoints <- length(timepoints)
  } else {
    timepointscol <- NA
    timepoints <- NA
    nooftimepoints <- 1
  }
  # get the rows for the time points identified
  for (j in 1:nooftimepoints) {
    if (is.na(timepointscol) || timepointscol == "NA") {
      rows_needed <- seq(1:nrow(ind_part_data))
    } else {
      rows_needed <- which(ind_part_data[[timepointscol]] == timepoints[j])
    }
    # pick the responses assumes the order
    eq5d_responses <- ind_part_data[rows_needed, eq5d_columnnames]
    # Check if the responses are numeric with range 1 to 5
    results <- sapply(eq5d_columnnames, IPDFileCheck::test_data_numeric,
                      eq5d_responses, eq5d_nrcode, 1, 5)
    if (any(results < 0)) {
      stop("eq5d responses do not seem right")
    } else {
      if (is.na(eq5d_nrcode)) {
        original_ind <- c()
        ind <- c()
        for (i in 1:ncol(eq5d_responses)) {
          which_one <-  which(is.na(eq5d_responses[[i]]))
          original_ind <- append(original_ind, rows_needed[which_one])
          ind <- append(ind, which_one)
        }
      } else {
        ind <- c()
        original_ind <- c()
        for (i in 1:ncol(eq5d_responses)) {
          which_one <- which(eq5d_responses[[i]] == eq5d_nrcode)
          original_ind <- append(original_ind, rows_needed[which_one])
          ind <- append(ind, which_one)
        }
      }
      ind <- sort(unique(ind))
      original_ind <-  sort(unique(original_ind))
      if (length(ind > 0))
          eq5d_responses <- eq5d_responses[-ind, ]
      index5L <- rep(0, nrow(eq5d_responses))
      for (i in seq(nrow(eq5d_responses))) {
        index5L[i] <- valueEQ5D::value_5L_Ind(
          "England", eq5d_responses[i, 1],
          eq5d_responses[i, 2], eq5d_responses[i, 3],
          eq5d_responses[i, 4], eq5d_responses[i, 5]
        )
      }
      new_colname <- paste("EQ5D5LIndex")
      if (length(ind > 0))
          rows_needed <- rows_needed[-ind]
      ind_part_data[rows_needed, new_colname] <- index5L
      if (length(original_ind > 0))
        ind_part_data[original_ind, new_colname] <- eq5d_nrcode
    }
  }
  return(ind_part_data)
}
##############################################################################
#' Function to map EQ5D5L scores to EQ5D3L scores and then add to IPD data
#' @param ind_part_data a data frame
#' @param eq5d_nrcode non response code for EQ5D5L, default is NA
#' @return qaly included modified data, if success -1, if failure
#' @examples
#' \donttest{
#' library(valueEQ5D)
#' datafile <- system.file("extdata", "trial_data.csv",
#' package = "packDAMipd")
#' trial_data <- load_trial_data(datafile)
#' map_eq5d5Lto3L_VanHout(trial_data, NA)
#' }
#' @source
#' http://eprints.whiterose.ac.uk/121473/1/Devlin_et_al-2017-Health_Economics.pdf
#' @export
map_eq5d5Lto3L_VanHout <- function(ind_part_data, eq5d_nrcode) {
  #Error - data should not be NULL
  if (is.null(ind_part_data))
    stop("data should not be NULL")
  eq5d_details <- get_eq5d_details(ind_part_data)
  eq5d_columnnames <- eq5d_details$name
  ind_part_data <- data.frame(ind_part_data)
  # get the time point details
  timepoint_details <- get_timepoint_details(ind_part_data)
  if (sum(is.na(timepoint_details)) == 0) {
    timepointscol <- timepoint_details$name
    timepoints <- unique(ind_part_data[[timepointscol]])
    nooftimepoints <- length(timepoints)
  } else {
    timepointscol <- NA
    timepoints <- NA
    nooftimepoints <- 1
  }
  for (j in 1:nooftimepoints) {
    if (is.na(timepointscol) || timepointscol == "NA") {
      rows_needed <- seq(1:nrow(ind_part_data))
    } else {
      rows_needed <- which(ind_part_data[[timepointscol]] == timepoints[j])
    }
    # pick the responses assumes the order
    eq5d_responses <- ind_part_data[rows_needed, eq5d_columnnames]
    # Check if the responses are numeric with range 1 to 5
    results <- sapply(eq5d_columnnames, IPDFileCheck::test_data_numeric,
                      eq5d_responses, eq5d_nrcode, 1, 5)
    if (any(results != 0)) {
      stop("eq5d responses do not seem right")
    } else {
      # remove those with non response codes, if missing data has been removed
      # this will do no harm
      if (is.na(eq5d_nrcode)) {
        original_ind <- c()
        ind <- c()
        for (i in 1:ncol(eq5d_responses)) {
          which_one <-  which(is.na(eq5d_responses[[i]]))
          original_ind <- append(original_ind, rows_needed[which_one])
          ind <- append(ind, which_one)
        }
      } else {
        ind <- c()
        original_ind <- c()
        for (i in 1:ncol(eq5d_responses)) {
          which_one <- which(eq5d_responses[[i]] == eq5d_nrcode)
          original_ind <- append(original_ind, rows_needed[which_one])
          ind <- append(ind, which_one)
        }
      }
      ind <- sort(unique(ind))
      original_ind <-  sort(unique(original_ind))
      if (length(ind > 0))
          eq5d_responses <- eq5d_responses[-ind, ]
      index5L <- rep(0, nrow(eq5d_responses))
      for (i in seq(nrow(eq5d_responses))) {
        score_5L <- as.numeric(paste(eq5d_responses[i, 1],
          eq5d_responses[i, 2], eq5d_responses[i, 3],
          eq5d_responses[i, 4], eq5d_responses[i, 5],
          sep = ""
        ))
        index5L[i] <- valueEQ5D::map_5Lto3L_Ind("UK", "CW", score_5L)
      }
      new_colname <- paste("EQ5D3L_from5L")
      if (length(ind > 0))
          rows_needed <- rows_needed[-ind]
      ind_part_data[rows_needed, new_colname] <- index5L
      if (length(original_ind > 0))
          ind_part_data[original_ind, new_colname] <- eq5d_nrcode
    }
  }
  return(ind_part_data)
}
##############################################################################
#' Function to convert ADL scores to a T score
#' @param ind_part_data a data frame containing IPD data
#' @param adl_related_words related words to find out which columns
#' contain adl data
#' @param adl_nrcode non response code for ADL
#' @param adl_scoring_table ADL scoring table, if given as NULL use
#' the default one
#' @return ADL scores converted to T score included modified data, if
#' success -1, if failure
#' @examples
#' datafile <- system.file("extdata", "trial_data.csv", package = "packDAMipd")
#' trial_data <- load_trial_data(datafile)
#' value_ADL_scores_IPD(trial_data,c("tpi"), NA, adl_scoring_table = NULL)
#' @export
value_ADL_scores_IPD <- function(ind_part_data, adl_related_words,
                                 adl_nrcode, adl_scoring_table = NULL) {
  #Error - data should not be NULL
  if (is.null(ind_part_data))
    stop("data should not be NULL")
  #Error - data should not be NULL
  if (!is.null(adl_scoring_table))
    adl_scores <- adl_scoring_table
  else
    adl_scores <- packDAMipd::adl_scoring

  adl_scoring_data_columns <- colnames(adl_scores)
  adl_details <- get_outcome_details(ind_part_data, "adl",
                                     adl_related_words, multiple = TRUE)
  adl_columnnames <- adl_details$name
  ind_part_data <- data.frame(ind_part_data)
  timepoint_details <- get_timepoint_details(ind_part_data)
  if (sum(is.na(timepoint_details)) == 0) {
    timepointscol <- timepoint_details$name
    timepoints <- unique(ind_part_data[[timepointscol]])
    nooftimepoints <- length(timepoints)
  } else {
    timepointscol <- NA
    timepoints <- NA
    nooftimepoints <- 1
  }
  for (j in 1:nooftimepoints) {
    if (is.na(timepointscol) | timepointscol == "NA") {
      rows_needed <- seq(1:nrow(ind_part_data))
    } else {
      rows_needed <- which(ind_part_data[[timepointscol]] == timepoints[j])
    }
    # get ADL responses
    adl_responses <- ind_part_data[rows_needed, adl_columnnames]
    # remove those with non response codes, if missing data has been removed
    # this will do no harm
    if (is.na(adl_nrcode)) {
      original_ind <- c()
      ind <- c()
      for (i in 1:ncol(adl_responses)) {
        which_one <-  which(is.na(adl_responses[[i]]))
        original_ind <- append(original_ind, rows_needed[which_one])
        ind <- append(ind, which_one)
      }
    } else {
      ind <- c()
      original_ind <- c()
      for (i in 1:ncol(adl_responses)) {
        which_one <- which(adl_responses[[i]] == adl_nrcode)
        original_ind <- append(original_ind, rows_needed[which_one])
        ind <- append(ind, which_one)
      }
    }
    ind <- sort(unique(ind))
    original_ind <-  sort(unique(original_ind))
    if (length(ind > 0))
      adl_responses <- adl_responses[-ind, ]
    # Check if the responses are 8 for an individual
    if (length(adl_columnnames) != 8) {
      stop("error- ADL should have 8 columns")
    } else {
      # Check if the responses are numeric with range 1 to 5
      results <- sapply(adl_columnnames, IPDFileCheck::test_data_numeric,
                        adl_responses, adl_nrcode, 1, 5)
    }
    if (any(results < 0)) {
      stop("ADL responses do not seem right")
    } else {

      # Check if ADL scoring table has columns defined in the config file
      if (IPDFileCheck::test_columnnames(adl_scoring_data_columns,
                                         adl_scores) == 0) {
        # Replace NA with 0
        adl_scores[is.na(adl_scores)] <- 0
        # Find the sum of scores
        sumADL <- rowSums(adl_responses)
        TscoreADL <- rep(0, length(sumADL))
        for (i in seq_len(length(sumADL))) {
          ithrow <- which(adl_scores$Raw.score == sumADL[i])
          # Get the T score corresponding to raw sum
          TscoreADL[i] <- adl_scores$T.Score[ithrow]
        }
        # Add the T score to data , save and return
        new_colname <- paste("ADLTscore")
        if (length(ind > 0))
          rows_needed <- rows_needed[-ind]
        ind_part_data[rows_needed, new_colname] <- TscoreADL
        if (length(original_ind > 0))
          ind_part_data[original_ind, new_colname] <- adl_nrcode

        # Add the total raw score to data , save and return
        new_colname <- paste("ADLtotalrawscore")
        ind_part_data[rows_needed, new_colname] <- sumADL
        if (length(original_ind > 0))
          ind_part_data[original_ind, new_colname] <- adl_nrcode
      } else {
        stop("Error ADL scoring column names are not equal to what specified
             in configuration file")
      }
    }
  }
  return(ind_part_data)
}
##############################################################################
#' Function to estimate the cost of tablets taken (from IPD)
#' @param ind_part_data a data frame containing IPD
#' @param shows_related_words a data frame containing IPD
#' @param shows_nrcode non response code for ADL, default is NA
#' @return sum of scores, if success -1, if failure
#' @examples
#' datafile <- system.file("extdata", "trial_data.csv", package = "packDAMipd")
#' trial_data <- load_trial_data(datafile)
#' value_Shows_IPD(trial_data, "qsy", NA)
#' @export
value_Shows_IPD <- function(ind_part_data, shows_related_words, shows_nrcode) {
  #Error - data should not be NULL
  if (is.null(ind_part_data))
    stop("data should not be NULL")

  shows_details <- get_outcome_details(ind_part_data, "shows",
                                       shows_related_words, multiple = TRUE)
  shows_columnnames <- shows_details$name
  ind_part_data <- data.frame(ind_part_data)
  timepoint_details <- get_timepoint_details(ind_part_data)
  if (sum(is.na(timepoint_details)) == 0) {
    timepointscol <- timepoint_details$name
    timepoints <- unique(ind_part_data[[timepointscol]])
    nooftimepoints <- length(timepoints)
  } else {
    timepointscol <- NA
    timepoints <- NA
    nooftimepoints <- 1
  }
  for (j in 1:nooftimepoints) {
    if (is.na(timepointscol) || timepointscol == "NA") {
      rows_needed <- seq(1:nrow(ind_part_data))
    } else {
      rows_needed <- which(ind_part_data[[timepointscol]] == timepoints[j])
    }
    # get shows responses
    shows_responses <- ind_part_data[rows_needed, shows_columnnames]
    # Check if the responses are 8 for anindividual
    if (length(shows_columnnames) != 10) {
      stop("Error- ShOWS should have 10 columns")
    } else {
      # Check if the responses are numeric with range 0 to 3 qctually
      ## --in the data it is coded from 1to 4.
      results <- sapply(shows_columnnames, IPDFileCheck::test_data_numeric,
                        shows_responses, shows_nrcode, 1, 4)
    }
    if (any(results < 0)) {
      stop("ShOWS responses do not seem right")
    } else {
      # remove those with non response codes, if missing data has been removed
      # this will do no harm
      if (is.na(shows_nrcode)) {
        original_ind <- c()
        ind <- c()
        for (i in 1:ncol(shows_responses)) {
          which_one <-  which(is.na(shows_responses[[i]]))
          original_ind <- append(original_ind, rows_needed[which_one])
          ind <- append(ind, which_one)
        }
      } else {
        ind <- c()
        original_ind <- c()
        for (i in 1:ncol(shows_responses)) {
          which_one <- which(shows_responses[[i]] == shows_nrcode)
          original_ind <- append(original_ind, rows_needed[which_one])
          ind <- append(ind, which_one)
        }
      }
      ind <- sort(unique(ind))
      original_ind <-  sort(unique(original_ind))
      if (length(ind > 0))
          shows_responses <- shows_responses[-ind, ]
      # Check if shows scoring table has columns defined in the config file
      sumShows <- rowSums(shows_responses) - 10
      # Add the score to data , save and return
      new_colname <- paste("ShOWSscore")
      if (length(ind > 0))
        rows_needed <- rows_needed[-ind]
      ind_part_data[rows_needed, new_colname] <- sumShows
      if (length(original_ind > 0))
          ind_part_data[original_ind, new_colname] <- shows_nrcode
    }
  }
  return(ind_part_data)
}

##############################################################################
#' Function to convert promis3a scores to a T score
#' @param ind_part_data a data frame containing IPD data
#' @param promis3a_related_words related words to find out which columns
#' contain promis3a data
#' @param promis3a_nrcode non response code for promis3a
#' @param promis3a_scoring_table promis3a scoring table, if given as NULL use
#' the default one
#' @return promis3a scores converted to T score included modified data, if
#' success -1, if failure
#' @examples
#' trial_data <- data.frame("tpi.q1" = c(1, 2),
#' "tpi.q2" = c(1, 2), "tpi.q3" = c(1, 2))
#' results <- value_promis3a_scores_IPD(trial_data, c("tpi"), NA, NULL)
#' @export
value_promis3a_scores_IPD <- function(ind_part_data, promis3a_related_words,
                                 promis3a_nrcode, promis3a_scoring_table = NULL) {
  #Error - data should not be NULL
  if (is.null(ind_part_data))
    stop("data should not be NULL")
  #Error - data should not be NULL
  if (!is.null(promis3a_scoring_table))
    promis3a_scores <- promis3a_scoring_table
  else
    promis3a_scores <- packDAMipd::promis3a_scoring

  promis3a_scoring_data_columns <- colnames(promis3a_scores)
  promis3a_details <- get_outcome_details(ind_part_data, "promis3a",
                                     promis3a_related_words, multiple = TRUE)
  promis3a_columnnames <- promis3a_details$name
  ind_part_data <- data.frame(ind_part_data)
  timepoint_details <- get_timepoint_details(ind_part_data)
  if (sum(is.na(timepoint_details)) == 0) {
    timepointscol <- timepoint_details$name
    timepoints <- unique(ind_part_data[[timepointscol]])
    nooftimepoints <- length(timepoints)
  } else {
    timepointscol <- NA
    timepoints <- NA
    nooftimepoints <- 1
  }
  for (j in 1:nooftimepoints) {
    if (is.na(timepointscol) | timepointscol == "NA") {
      rows_needed <- seq(1:nrow(ind_part_data))
    } else {
      rows_needed <- which(ind_part_data[[timepointscol]] == timepoints[j])
    }
    # get promis3a responses
    promis3a_responses <- ind_part_data[rows_needed, promis3a_columnnames]
    # remove those with non response codes, if missing data has been removed
    # this will do no harm
    if (is.na(promis3a_nrcode)) {
      original_ind <- c()
      ind <- c()
      for (i in 1:ncol(promis3a_responses)) {
        which_one <-  which(is.na(promis3a_responses[[i]]))
        original_ind <- append(original_ind, rows_needed[which_one])
        ind <- append(ind, which_one)
      }
    } else {
      ind <- c()
      original_ind <- c()
      for (i in 1:ncol(promis3a_responses)) {
        which_one <- which(promis3a_responses[[i]] == promis3a_nrcode)
        original_ind <- append(original_ind, rows_needed[which_one])
        ind <- append(ind, which_one)
      }
    }
    ind <- sort(unique(ind))
    original_ind <-  sort(unique(original_ind))
    if (length(ind > 0))
      promis3a_responses <- promis3a_responses[-ind, ]
    # Check if the responses are 8 for an individual
    if (length(promis3a_columnnames) != 3) {
      stop("error- promis3a should have 3 columns")
    } else {
      # Check if the responses are numeric with range 1 to 5
      results <- sapply(promis3a_columnnames, IPDFileCheck::test_data_numeric,
                        promis3a_responses, promis3a_nrcode, 1, 5)
    }
    if (any(results < 0)) {
      stop("promis3a responses do not seem right")
    } else {

      # Check if promis3a scoring table has columns defined in the config file
      if (IPDFileCheck::test_columnnames(promis3a_scoring_data_columns,
                                         promis3a_scores) == 0) {
        # Replace NA with 0
        promis3a_scores[is.na(promis3a_scores)] <- 0
        # Find the sum of scores
        sumpromis3a <- rowSums(promis3a_responses)
        Tscorepromis3a <- rep(0, length(sumpromis3a))
        for (i in seq_len(length(sumpromis3a))) {
          ithrow <- which(promis3a_scores$Raw.score == sumpromis3a[i])
          # Get the T score corresponding to raw sum
          Tscorepromis3a[i] <- promis3a_scores$T.Score[ithrow]
        }
        # Add the T score to data , save and return
        new_colname <- paste("promis3aTscore")
        if (length(ind > 0))
          rows_needed <- rows_needed[-ind]
        ind_part_data[rows_needed, new_colname] <- Tscorepromis3a
        if (length(original_ind > 0))
          ind_part_data[original_ind, new_colname] <- promis3a_nrcode

        # Add the total raw score to data , save and return
        new_colname <- paste("promis3a_rawscore")
        ind_part_data[rows_needed, new_colname] <- sumpromis3a
        if (length(original_ind > 0))
          ind_part_data[original_ind, new_colname] <- promis3a_nrcode
      } else {
        stop("Error promis3a scoring column names are not equal to what specified
             in configuration file")
      }
    }
  }
  return(ind_part_data)
}
