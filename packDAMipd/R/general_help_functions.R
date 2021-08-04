#' Create a table to compare the descriptive analysis from gtsummary of
#' two groups
#' @param variables  variables that interested
#' @param gtsummary a gtsummary object that contains summary parameters
#' @param name_use  name of the variable or category
#' @return the table
#' @examples
#' eg_data <- as.data.frame(list(no = c(1,2,3,4), mark = c(12,34,23,45),
#' gender = c("M", "F", "M", "F")))
#' outcome_summary <- IPDFileCheck::get_summary_gtsummary(eg_data,
#' c("gender", "mark"), byvar = "gender")
#' variables <- "Mark"
#' create_table_from_gtsummary_compare_twogroups(variables,
#' outcome_summary, "Category")
#' @export
create_table_from_gtsummary_compare_twogroups <- function(variables,
                                                          gtsummary, name_use) {
  desc_summmary <- gtsummary$table_body
  total_variables <- unique(desc_summmary$variable)
  no_variables <- length(total_variables)
  each_variable_has <- nrow(desc_summmary) / no_variables
  all_results <- c()
  for (i in 1:no_variables) {
    startrow <- (i - 1) * each_variable_has + 1
    endrow <- i * each_variable_has
    this_summary <- desc_summmary[startrow:endrow, ]
    mean_sd <- this_summary[this_summary$label == "Mean (SD)", ]
    mean_sd_all <- as.data.frame(cbind(variables[i], mean_sd$stat_1,
                                       mean_sd$stat_2))
    Difference  <- this_summary$estimate[1]
    ci <- this_summary$ci[1]
    pvalue <- this_summary$p.value[1]
    all <- as.data.frame(cbind(mean_sd_all, Difference, ci, pvalue))
    all_results <- rbind(all_results, all)
  }
  all_results <- as.data.frame(all_results)
  ft <- flextable::flextable(as.data.frame(all_results))
  ft <- flextable::delete_part(ft, part = "header")
  ft <- flextable::add_header(ft, V1 = name_use, V2 = "Group 0", V3 = "Group 1",
                   Difference = "Difference in means", ci = "95% CI",
                   pvalue = "p-value", top = TRUE)
  ft
  return(ft)
}
########################################################################
#' Create a table to compare the descriptive analysis (short) from gtsummary of
#' two groups, but at different timepoints
#' @param variables  variables that interested
#' @param gtsummary a gtsummary object that contains summary parameters
#' @param name_use  name of the variable or category
#' @param timepoints the timepoints at which the descriptive analysis is done
#' @return the table
#' @examples
#' eg_data <- as.data.frame(list(no = c(1, 2, 3, 4),
#' mark_at_1 = c(12, 34, 23, 45), gender = c("M", "F", "M", "F"),
#' mark_at_2 = c(12, 34, 23, 45)))
#' outcome_summary <- IPDFileCheck::get_summary_gtsummary(eg_data,
#' c("gender", "mark_at_1", "mark_at_2"), byvar = "gender")
#' variables <- "Mark"
#' k <- create_shorttable_from_gtsummary_compare_twogroups_timpoints(variables,
#' outcome_summary, "Category", c(1, 2))
#' @export
create_shorttable_from_gtsummary_compare_twogroups_timpoints <- function(
  variables, gtsummary, name_use, timepoints) {
  desc_summmary <- gtsummary$table_body
  total_variables <- unique(desc_summmary$variable)
  no_variables <- length(total_variables) / length(timepoints)
  each_variable_has <- nrow(desc_summmary) / no_variables
  all_results <- c()
  for (i in 1:no_variables) {
    startrow <- (i - 1) * each_variable_has + 1
    endrow <- i * each_variable_has
    this_summary <- desc_summmary[startrow:endrow, ]
    mean_sd <- this_summary[this_summary$label == "Mean (SD)", ]
    mean_sd_all <- as.data.frame(c(paste(variables[i], "Mean (SD)"),
                                   mean_sd$stat_1, mean_sd$stat_2))
    ns <-  this_summary[this_summary$label == "N", ]
    ns_all <- as.data.frame(c(paste(variables[i], "(N)"), ns$stat_1, ns$stat_2))
    mean_sd_ns_all <- t(cbind(mean_sd_all, ns_all))
    all_results <- rbind(all_results, mean_sd_ns_all)
  }
  ft <- flextable::flextable(as.data.frame(all_results))
  ft <- flextable::delete_part(ft, part = "header")
  ft <- flextable::add_header(ft, V1 = name_use, V2 = timepoints[1],
                   V3 = timepoints[2],
                   V4 = timepoints[3],
                   V5 = timepoints[4], V6 = timepoints[1], V7 = timepoints[2],
                   V8 = timepoints[3], V9 = timepoints[4], top = FALSE)
  ft <- flextable::add_header(ft, V1 = "",
                   V2 = "Group 0", V3 = "Group 0",
                   V4 = "Group 0", V5 = "Group 0",
                   V6 = "Group 1", V7 = "Group 1",
                   V8 = "Group 1", V9 = "Group 1", top = TRUE)
  ft <- flextable::merge_h(ft, part = "header")
  ft
  return(ft)
}
########################################################################
#' Function to plot mean and SE for longitudinal observations for twogroups compared
#' @param thedata  the data where the observations are held
#' @param columnnames columnnames in the data where the intersted observations
#' @param timepoints the timepoints at which the descriptive analysis is done
#' @param observation  name of the observations
#' @return the plot that shows mean and SE
#' @examples
#' eg_data <- as.data.frame(list(no = c(1, 2, 3, 4),
#' mark_at_1 = c(12, 7, 23, 45), gender = c("M", "F", "M", "F"),
#' mark_at_2 = c(12, 34, 89, 45), trialarm = c("1","1","2","2")))
#' plot_meanSE_longitudinal_twogroups(eg_data,  c("mark_at_1", "mark_at_2"),
#' c("1","2"), "mark")
#' @export
plot_meanSE_longitudinal_twogroups <- function(thedata, columnnames, timepoints,
                                     observation) {
  trial_arm_details <- packDAMipd::get_trial_arm_details(thedata)
  trial_arm_name <- trial_arm_details$name
  trial_arm_codes <- labelled::remove_labels(trial_arm_details$codes)
  group1 <- IPDFileCheck::return_subgroup_withNA(thedata, trial_arm_name,
                                                 trial_arm_codes[1])
  group2 <- IPDFileCheck::return_subgroup_withNA(thedata, trial_arm_name,
                                                 trial_arm_codes[2])
  group1_res <- as.data.frame(IPDFileCheck::return_longitudinal_summary(
    group1, columnnames, NA))
  group1_res[["group"]] <- trial_arm_codes[1]
  group2_res <- as.data.frame(IPDFileCheck::return_longitudinal_summary(
    group2, columnnames, NA))
  group2_res[["group"]] <- trial_arm_codes[2]
  timepoints_all <- rep(timepoints, 2)
  results <- rbind(group1_res, group2_res)

  mytheme <- ggplot2::theme_classic()
  ggplot2::theme(axis.title.x = ggplot2::element_text(face = "bold", size = 16),
    axis.title.y = ggplot2::element_text(face = "bold", angle = 90, size = 16),
    axis.text.x = ggplot2::element_text(face = "bold", size = 14),
    axis.text.y = ggplot2::element_text(face = "bold", size = 14))
  p <- ggplot2::ggplot(data = results, ggplot2::aes(x = timepoints_all,
                                                y = means,
                                                group = group)) +
    ggplot2::geom_line(ggplot2::aes(color = group), size = 2) +
    ggplot2::geom_point(show.legend = FALSE) + mytheme +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = means - se,
                                        ymax = means + se,
                                        color = group, width = 1),
    width = .2, show.legend = FALSE, position = ggplot2::position_dodge(0.0)) +
    ggplot2::labs(y = observation, x = "Timepoints")
  p
}


########################################################################
#' Function to plot mean and SE for longitudinal observations for twogroups compared
#' @param thedata  the data where the observations are held
#' @param colname columnname in the data where the intersted observations
#' @param timepointstring the text that correspond to timepoints
#' at which the descriptive analysis is done
#' @param xstring  xlable text
#' @param ylimits on hsitogram plot
#' @param nbins nbins the number of bins to plot histogram
#' @return the historgram plot at a timepoint for two groups
#' @examples
#' eg_data <- as.data.frame(list(no = c(1, 2, 3, 4),
#' mark_at_1 = c(12, 7, 23, 45), gender = c("M", "F", "M", "F"),
#' mark_at_2 = c(12, 34, 89, 45), trialarm = c("1","1","2","2")))
#' plot_histogram_onetimepoint_twogroups(eg_data,  c("mark_at_2"), c("1","2"),
#'  "mark")
#' @export
plot_histogram_onetimepoint_twogroups <- function(thedata, colname, timepointstring,
                                       xstring, ylimits = NULL, nbins = NULL) {
  if (is.null(nbins)) {
    nbins <- 30
  }
  trial_arm_details <- packDAMipd::get_trial_arm_details(thedata)
  trial_arm_name <- trial_arm_details$name
  trial_arm_codes <- (trial_arm_details$codes)
  group1 <- IPDFileCheck::return_subgroup_withNA(thedata, trial_arm_name,
                                                 trial_arm_codes[1])
  group2 <- IPDFileCheck::return_subgroup_withNA(thedata, trial_arm_name,
                                                 trial_arm_codes[2])
  mean1 <- mean(group1[[colname]], na.rm = TRUE)
  mean2 <- mean(group2[[colname]], na.rm = TRUE)

  xmin <- min(group1[[colname]],  group2[[colname]], na.rm = TRUE)
  xmax <-  max(group1[[colname]], group2[[colname]], na.rm = TRUE)

  hist1 <- graphics::hist(group1[[colname]], plot = FALSE)
  hist2 <- graphics::hist(group2[[colname]], plot = FALSE)

  ymin <- min(hist1$counts, hist2$counts, na.rm = TRUE)
  ymax <-  max(hist1$counts, hist2$counts, na.rm = TRUE)

  if (is.null(ylimits)) {
    ylimits <- c(ymin - ymin * 0.1, ymax + ymax * 0.1)
  }
  xlimits <- c(xmin - xmin * 0.1, xmax + xmax * 0.1)

  one <-  ggplot2::ggplot(group1, ggplot2::aes_string(x = colname)) +
    ggplot2::geom_histogram(bins = nbins, color = "darkblue", fill = "lightblue") +
    ggplot2::scale_y_continuous(limits = ylimits) +
    ggplot2::scale_x_continuous(limits = xlimits) +
    ggplot2::geom_vline(xintercept = mean1, color = "darkblue",
                        linetype = "dashed") +
    ggplot2::labs(title = "", x = xstring, y = "Frequency") +
    ggplot2::geom_text(ggplot2::aes(label =
    paste("mean = ", round(mean1, 2), sep = ""), y = 0, x = mean1), hjust = -1,
    vjust = -10, col = "black", size = 3)


  two <-  ggplot2::ggplot(group2, ggplot2::aes_string(x = colname)) +
    ggplot2::geom_histogram(bins = nbins, color = "darkgreen",
                            fill = "lightgreen") +
    ggplot2::scale_y_continuous(limits = ylimits) +
    ggplot2::scale_x_continuous(limits = xlimits) +
    ggplot2::geom_vline(xintercept = mean2, color = "darkgreen",
                        linetype = "dashed") +
    ggplot2::labs(title = "", x = xstring, y = "Frequency") +
    ggplot2::geom_text(ggplot2::aes(label = paste("mean = ",
                              round(mean2, 2), sep = ""), y = 0,
          x = mean2), hjust = -1, vjust = -10, col = "black", size = 3)

  figure <- ggpubr::ggarrange(one, two, labels = c(paste("Group",
            trial_arm_codes[1], sep = " "),
            paste("Group", trial_arm_codes[2], sep = " ")))
  figure
}

########################################################################
#' Function to get sum of  entries  of resource per individual at diff timepoints
#' if same cateogry has listed multiple time for same id of pariticpant , this
#' method comes in handy to get the sum
#' @param use_data  the data where the observations are held
#' @param timepointcol columnname in the data where the timepoints are noted
#' @param timepointval which time point is considered now
#' at which the descriptive analysis is done
#' @param idcolumn id for each participant
#' @param result_col name of the column where the sum of entries to be saved
#' @return the data with added sum of resource use
#' @examples
#' eg_data <- as.data.frame(list(no = c(1, 2, 3, 4),
#' mark_at_1 = c(12, 7, 23, 45), gender = c("M", "F", "M", "F"),
#' mark_at_2 = c(12, 34, 89, 45), trialarm = c("1","1","2","2"),
#' time = c(1,1,2,2), id = c(1, 1, 1, 2)))
#' add_entries_sameuse_timepoint(eg_data,  "time", 1, "id",
#' ("mark_at_2"))
#' @export
add_entries_sameuse_timepoint <- function(use_data, timepointcol, timepointval,
                                         idcolumn, result_col) {
  if (!is.null(timepointcol)) {
    check <- IPDFileCheck::check_column_exists(timepointcol, use_data)
    if (check != 0)
      stop("Error - No column representing time points")
  } else {
    stop("Error - timepoint column cant be NULL")
  }
  if (!is.null(idcolumn)) {
    check <- IPDFileCheck::check_column_exists(idcolumn, use_data)
    if (check != 0)
      stop("Error - No column representing id ")
  }else {
    stop("Error - id column cant be NULL")
  }
  if (!is.null(result_col)) {
    check <- IPDFileCheck::check_column_exists(result_col, use_data)
    if (check != 0)
      stop("Error - No column representing results")
  }else {
    stop("Error - results column cant be NULL")
  }
  use_data_timepoint <- use_data[use_data[[timepointcol]] == timepointval, ]
  unique_tno <- unique(use_data_timepoint[[idcolumn]])
  result <- rep(0, length(unique_tno))
  for (i in 1:length(unique_tno)) {
    this_id <- unique_tno[i]
    indices <- which(use_data_timepoint[[idcolumn]] == this_id)
    result_val <- use_data_timepoint[[result_col]]
    result[i] <- sum(as.numeric(result_val[indices]), na.rm = TRUE)
  }
  tno_and_result <- as.data.frame(cbind(unique_tno, result))
  colnames(tno_and_result) <- c(idcolumn, result_col)
  tno_and_result[[result_col]] <- as.numeric(tno_and_result[[result_col]])
  return(tno_and_result)
}

########################################################################
#' Function to get sum of  entries  of resource per individual at diff timepoints
#' if same cateogry has listed multiple time for same id of pariticpant , this
#' method comes in handy to get the sum
#' @param use_data  the data where the observations are held
#' @param timepointcol columnname in the data where the timepoints are noted
#' @param timepointval which time point is considered now
#' at which the descriptive analysis is done
#' @param idcolumn id for each participant
#' @param result_col name of the column where the sum of entries to be saved
#' @return the data with added sum of resource use
#' @examples
#' eg_data <- as.data.frame(list(no = c(1, 2, 3, 4),
#' mark_at_1 = c(12, 7, 23, 45), gender = c("M", "F", "M", "F"),
#' mark_at_2 = c(12, 34, 89, 45), trialarm = c("1","1","2","2"),
#' time = c(1,1,2,2), id = c(1, 1, 1, 2)))
#' add_entries_sameuse_onetno_onetimepoint(eg_data,  "time", 1, "id",
#' ("mark_at_2"))
#' @export
add_entries_sameuse_onetno_onetimepoint <- function(use_data, timepointcol,
                                        timepointval, idcolumn, result_col) {
  if (!is.null(timepointcol)) {
    check <- IPDFileCheck::check_column_exists(timepointcol, use_data)
    if (check != 0)
      stop("Error - No column representing time points")
  } else {
    stop("Error - timepoint column cant be NULL")
  }
  if (!is.null(idcolumn)) {
    check <- IPDFileCheck::check_column_exists(idcolumn, use_data)
    if (check != 0)
      stop("Error - No column representing id ")
  } else {
    stop("Error - id column cant be NULL")
  }
  if (!is.null(result_col)) {
    check <- IPDFileCheck::check_column_exists(result_col, use_data)
    if (check != 0)
      stop("Error - No column representing results")
  }else {
    stop("Error - results column cant be NULL")
  }
  use_data_timepoint <- use_data[use_data[[timepointcol]] == timepointval, ]
  unique_tno <- unique(use_data_timepoint[[idcolumn]])
  result <- rep(0, length(unique_tno))
  for (i in 1:length(unique_tno)) {
    this_id <- unique_tno[i]
    indices <- which(use_data_timepoint[[idcolumn]] == this_id)
    result_val <- use_data_timepoint[[result_col]]
    result[i] <- sum(as.numeric(result_val[indices]))
  }
  tno_and_result <- as.data.frame(cbind(unique_tno, result))
  colnames(tno_and_result) <- c(idcolumn, result_col)
  tno_and_result[[result_col]] <- as.numeric(tno_and_result[[result_col]])
  return(tno_and_result)
}
########################################################################
#' Function to check the equality of column contents between two data sets
#' with omitting NA
#' @param data1  first data set
#' @param data2  second data set
#' @param samecol  a unique col in both datasets, like tno or id
#' @param column_data1 column name in data 1 to be compared
#' @param column_data2 column name in data 2 to be compared
#' @return 0 if they are equal else error message
#' @examples
#' eg_data <- as.data.frame(list(no = c(1, 2, 3, 4),
#' mark_at_1 = c(12, 7, 23, 45), gender = c("M", "F", "M", "F"),
#' mark_at_2 = c(12, 34, 89, 45), trialarm = c("1","1","2","2"),
#' time = c(1,1,2,2), id = c(1, 1, 1, 2)))
#' eg_data2 <- as.data.frame(list(no = c(1, 2, 3, 4),
#' mark_at_1 = c(12, 27, 23, 45), gender = c("M", "F", "M", "F"),
#' mark_at_2 = c(12, 34, 89, 45), trialarm = c("1","1","2","2"),
#' time = c(1,1,2,2), id = c(1, 1, 1, 2)))
#' check_equal_columncontents_NAomitted(eg_data, eg_data2, "no",
#' "mark_at_1","mark_at_2")
#' @export
check_equal_columncontents_NAomitted <- function(data1, data2, samecol,
                                                 column_data1, column_data2) {
  if (IPDFileCheck::check_column_exists(samecol, data1) != 0)
    stop("Column doesnt exists in 1st dataset")
  if (IPDFileCheck::check_column_exists(column_data1, data1) != 0)
    stop("Column doesnt exists in 1st dataset")
  if (IPDFileCheck::check_column_exists(samecol, data2) != 0)
    stop("Column doesnt exists in 2nddataset")
  if (IPDFileCheck::check_column_exists(column_data2, data2) != 0)
    stop("Column doesnt exists in 2nd dataset")
  ordered1 <- data1[order(data1[[samecol]]), ]
  ordered2 <- data2[order(data2[[samecol]]), ]
  ordered1_needed <- as.data.frame(cbind(ordered1[samecol],
                                         ordered1[column_data1]))
  ordered2_needed <- as.data.frame(cbind(ordered2[samecol],
                                         ordered2[column_data2]))
  check <- nrow(ordered1_needed) < nrow(ordered2_needed)
  if (check) {
    temp <- dplyr::left_join(ordered1_needed, ordered2_needed, by = samecol)
  } else {
    temp <- dplyr::left_join(ordered2_needed, ordered1_needed, by = samecol)
  }
  index_NA_1 <- which(is.na(temp[[column_data1]]))
  index_NA_2 <- which(is.na(temp[[column_data2]]))
  if (length(index_NA_1) == length(index_NA_2)) {
    if (sum(index_NA_1 != index_NA_2) > 0) {
      warning("NA indices do not match")
      return(c(ordered1[index_NA_1][[samecol]], ordered2[index_NA_2][[samecol]]))
    }
  } else {
    stop("NA entries do not match")
  }
  temp[["difference"]] <- NA
  temp[-index_NA_1, ]$difference <-
    as.numeric(temp[-index_NA_1, ][[column_data1]]) -
    as.numeric(temp[-index_NA_2, ][[column_data2]])
  checking <- temp[!is.na(temp$difference), ]$difference
  if (sum(checking > 1e-2) != 0) {
    notequalindex <- which(temp[!is.na(temp$difference), ]$difference > 1e-2)
    return(temp[!is.na(temp$difference), ][notequalindex, samecol])
  } else {
    return(0)
  }
}
########################################################################
#' Function to check the sum of column contents between two data sets
#' with omitting NA
#' @param data1  first data set
#' @param data2  second data set
#' @param samecol  a unique col in both datasets, like tno or id
#' @param column_data1 column name in data 1 to be compared
#' @param column_data2 column name in data 2 to be compared
#' @return 0 if they are equal else error message
#' @examples
#' eg_data <- as.data.frame(list(no = c(1, 2, 3, 4),
#' mark_at_1 = c(12, 7, 23, 45), gender = c("M", "F", "M", "F"),
#' mark_at_2 = c(12, 34, 89, 45), trialarm = c("1","1","2","2"),
#' time = c(1,1,2,2), id = c(1, 1, 1, 2)))
#' eg_data2 <- as.data.frame(list(no = c(1, 2, 3, 4),
#' mark_at_1 = c(12, 27, 23, 45), gender = c("M", "F", "M", "F"),
#' mark_at_2 = c(12, 34, 89, 45), trialarm = c("1","1","2","2"),
#' time = c(1,1,2,2), id = c(1, 1, 1, 2)))
#' check_equal_sumcolumncontents_NAomitted(eg_data, eg_data2, "no",
#' "mark_at_1","mark_at_2")
#' @export
check_equal_sumcolumncontents_NAomitted <- function(data1, data2, samecol,
                                                column_data1, column_data2) {
  if (IPDFileCheck::check_column_exists(samecol, data1) != 0)
    stop("Column doesnt exists in 1st dataset")
  if (IPDFileCheck::check_column_exists(column_data1, data1) != 0)
    stop("Column doesnt exists in 1st dataset")
  if (IPDFileCheck::check_column_exists(samecol, data2) != 0)
    stop("Column doesnt exists in 2nddataset")
  res <- sapply(column_data2, IPDFileCheck::check_column_exists, data2)
  if (sum(res) != 0)
    stop("Column doesnt exists in 2nd dataset")
  ordered1 <- data1[order(data1[[samecol]]), ]
  ordered2 <- data2[order(data2[[samecol]]), ]
  ordered1_needed <- as.data.frame(cbind(ordered1[samecol],
                                         ordered1[column_data1]))
  the_sum <- rep(NA, nrow(ordered2))
  for (i in 1:nrow(ordered2)) {
    if (sum(is.na(ordered2[i, column_data2])) == length(column_data2)) {
      the_sum[i] <- NA
    } else {
      the_sum[i] <- sum(ordered2[i, column_data2], na.rm = TRUE)
    }
  }
  ordered2_needed <- as.data.frame(cbind(ordered2[samecol], the_sum))
  check <- nrow(ordered1_needed) < nrow(ordered2_needed)
  if (check) {
    temp <- dplyr::left_join(ordered1_needed, ordered2_needed, by = samecol)
  } else {
    temp <- dplyr::left_join(ordered2_needed, ordered1_needed, by = samecol)
  }

  index_NA_1 <- which(is.na(temp[[column_data1]]))
  index_NA_2 <- which(is.na(temp$the_sum))

  if (sum(index_NA_1 != index_NA_2) > 0) {
    warning("NA indices do not match")
    return(c(ordered1[index_NA_1][[samecol]], ordered2[index_NA_2][[samecol]]))
  }
  temp[["difference"]] <- NA
  temp[-index_NA_1, ]$difference <-
    as.numeric(temp[-index_NA_1, ][[column_data1]]) -
    as.numeric(temp[-index_NA_2, ]$the_sum)

  checking <- temp[!is.na(temp$difference), ]$difference
  if (sum(checking > 1e-2) != 0) {
    notequalindex <- which(temp[!is.na(temp$difference), ]$difference > 1e-2)
    return(temp[!is.na(temp$difference), ][notequalindex, samecol])
  } else {
    return(0)
  }
}
########################################################################
#' Function to get sum of  multiple columns of observations
#' @param the_data  the data where the observations are held
#' @param colnames columnname in the data whose sum to be obtianed
#' @param sumcolname name of the new column where sum to be saved
#' @return the data with added sum
#' @examples
#' eg_data <- as.data.frame(list(no = c(1, 2, 3, 4),
#' mark_at_1 = c(12, 7, 23, 45), gender = c("M", "F", "M", "F"),
#' mark_at_2 = c(12, 34, 89, 45), trialarm = c("1","1","2","2"),
#' time = c(1,1,2,2), id = c(1, 1, 1, 2)))
#' find_rowwise_sum_multiplecol(eg_data, c("mark_at_1","mark_at_2"),
#' "totalmarks")
#' @export
find_rowwise_sum_multiplecol <- function(the_data, colnames, sumcolname) {
  results <- unlist(lapply(colnames, IPDFileCheck::check_column_exists,
                           the_data))
  if (sum(results) != 0) {
    stop("Error - some of the columns do not exists in the data")
  }
  for (i in 1:nrow(the_data)) {
    indices <- c()
    for (j in 1:length(colnames)) {
      index <- which(colnames(the_data) == colnames[j])
      indices <- append(indices, index)
    }
    required <- the_data[i, indices]
    if (sum(is.na(required)) == length(required)) {
      the_data[[sumcolname]][i] <- NA
    }else{
      the_data[[sumcolname]][i]  <- sum(required, na.rm = TRUE)
    }
  }
  return(the_data)
}
########################################################################
#' Function to to read the text form of resource use and replace it with
#' standard texts of resoure use ie. some one can describe GP visit as GP surgery
#' visit, surgery visit or general practioners visit etc. Here all these texts
#' should be given in a excel or csv file and then corresponidng standard form
#' will be read from the file and will be replaced.
#' @param the_data  the data where the observations are held
#' @param service_actual columna name of the actual service use
#' @param new_column the name of the column where the mapped resource use to be
#' @param mapped_data data where the service name and mapped service name
#' has been stored
#' @param mapped_use columan name of mapped resource use in mapped_data
#' @param analysis base case or secondary
#' @param replace_only if we want to replace only certain resource use
#' @param relevant_column the name of the column where the mapped resource use
#' is indicated as relevant or not
#' @param check_value_relevant how is the mapped resource
#' is indicated as relevant by a value
#' @param nhs_use_column the name of the column where the mapped resource use
#' comes under NHS or not
#' @param check_value_nhs_use value that is used to indicated the nhs use
#' @return the data with added sum
#' @export
map_resource_use_categories <- function(the_data, service_actual, new_column,
                                        mapped_data,
                                        mapped_use, analysis, replace_only,
                                        relevant_column = NULL,
                                        check_value_relevant = NULL,
                                        nhs_use_column = NULL,
                                        check_value_nhs_use = NULL) {
  if (IPDFileCheck::check_column_exists(service_actual, the_data) != 0) {
    stop("Error- the service actual column is not found")
  }
  if (IPDFileCheck::check_column_exists(service_actual, mapped_data) != 0) {
    stop("Error- the service actual column in mapped data is not found")
  }
  if (IPDFileCheck::check_column_exists(mapped_use, mapped_data) != 0) {
    stop("Error- the mapped column in  mapped data is not found")
  }
  if (IPDFileCheck::check_column_exists(mapped_use, mapped_data) != 0) {
    stop("Error- the mapped column in  mapped data is not found")
  }
  if (!is.null(relevant_column)) {
    if (IPDFileCheck::check_column_exists(relevant_column, mapped_data) != 0) {
      stop("Error-the column to check relevancy in  mapped data is not found")
    }
  }
  if (!is.null(nhs_use_column)) {
    if (IPDFileCheck::check_column_exists(nhs_use_column, mapped_data) != 0) {
      stop("Error-column to check the nhs use in  mapped data is not found")
    }
  }
  if (is.null(replace_only))
    stop("replace only cant be null")

  the_data[[new_column]] <- the_data[[service_actual]]
  actual_contents <- the_data[[service_actual]]
  mapped_contents <- mapped_data[[service_actual]]
  baseline_4mnth_8mnth <- trimws((mapped_contents))

  for (i in 1:nrow(the_data)) {
    baseline_4mnth <- trimws((actual_contents[i]))
    indices <- which(baseline_4mnth == baseline_4mnth_8mnth)
    if (length(indices) > 0) {
      index <- indices[1]
      tobe_included_sec <- mapped_data[[relevant_column]][index] ==
        check_value_relevant &
        mapped_data[[nhs_use_column]][index] == check_value_nhs_use
      tobe_included_base <- tobe_included_sec &
        mapped_data[[mapped_use]][index] == replace_only
      if (analysis == "base_case_analysis" & tobe_included_base) {
        the_data[[new_column]][i] <- mapped_data[[mapped_use]][index]
      } else {
        if (analysis == "secondary_analysis" & tobe_included_sec)
          the_data[[new_column]][i] <- mapped_data[[mapped_use]][index]
        else
          the_data[[new_column]][i] <- ""
      }
    }
  }
  return(the_data)
}
########################################################################
