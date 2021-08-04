# 5. Estimate ICER and NMB

#' Estimation of ICER and NMB
#' @param list_markov  list of Markov model objects with their Markov trace,
#' cost matrix and utility matrix
#' @param threshold threshold value of WTP
#' @param comparator the strategy to be compared with
#' @return ICER and NMB for all the strategies compared to comparator
#' @examples
#' \donttest{
#' well <- health_state("well", cost = 0, utility = 1)
#' disabled <- health_state("disabled", cost = 100, utility = 1)
#' dead <- health_state("dead", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
#' colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
#' tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1),
#' colnames(tmat))
#' health_states <- combine_state(well, disabled, dead)
#' this.strategy <- strategy(tm, health_states, "control")
#' this_markov <- markov_model(this.strategy, 24, c(1000, 0, 0), c(0,0))
#' well <- health_state("well", cost = 0, utility = 1)
#' disabled <- health_state("disabled", cost = 10, utility = 0.5)
#' dead <- health_state("dead", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
#' colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
#' tm <- populate_transition_matrix(3, tmat, c(0.4, 0.4, 0.2, 0.6, 0.4, 1),
#' colnames(tmat))
#' health_states <- combine_state(well, disabled, dead)
#' this.strategy <- strategy(tm, health_states, "intervention")
#' sec_markov <- markov_model(this.strategy, 24, c(1000, 0, 0), c(0,0))
#' list_markov <- combine_markov(this_markov, sec_markov)
#' calculate_icer_nmb(list_markov, 20000, comparator = "control")
#' }
#' @export
calculate_icer_nmb <- function(list_markov, threshold, comparator = NULL) {
  checks_var <- list(list_markov, threshold)
  checks <- sapply(checks_var, is.null)
  if (sum(checks) != 0)
    stop("One or more parameter passed is NULL")

  # extract the names of strategies from the markov models
  no_comparison <- nrow(list_markov)
  list_names_strategy <- list()
  for (i in seq_len(no_comparison)) {
    list_names_strategy <- append(list_names_strategy,
                                  list_markov[, "strategy"][[i]]$name_strategy)
  }
  # if comparator is not in any of the strategies, error
  if (!is.null(comparator)) {
    if (!any(list_names_strategy == comparator)) {
           stop("Comparator has to be any of the strategies
                provided - please check")
    } else{
       comparator_index <- which(list_names_strategy == comparator)
    }
  }else{
    comparator_index <- 0
  }
  # check the expected columns in list of markov
  expected_colnames <- c(
    "strategy", "method", "half_cycle_correction", "transition_matrix",
    "param_matrix", "list_param_values", "health_states", "cycles",
    "initial_state", "initial_cost", "discount",
    "trace_matrix", "cost_matrix", "utility_matrix"
  )
  if (sum(colnames(list_markov) %in% expected_colnames) <
      ncol(list_markov) - 2) {
    stop("column names of list of markov_model objects look different")
  }
  # checking all the markov model objects
  if (check_list_markov_models(list_markov) == 0) {
    cost_matrix_all <- list_markov[, "cost_matrix"]
    utility_matrix_all <- list_markov[, "utility_matrix"]
    cycles_all <- list_markov[, "cycles"]
    cycles <- unlist(cycles_all[1])
    av_cum_cost_pp <- list()
    av_cum_utility_pp <- list()
    # get the average cumulative cost and qaly
    for (i in seq_len(no_comparison)) {
      no_states <- list_markov[, "strategy"][[i]]$transition_matrix$no_states
      start_cohort <- sum(list_markov[, "trace_matrix"][[i]][1, ])
      this_cost_matrix <- matrix(unlist(cost_matrix_all[i]),
                                 ncol = no_states + 2, byrow = FALSE)
      av_cum_cost <- this_cost_matrix[cycles + 1, no_states + 2]
      av_cum_cost_pp <- append(av_cum_cost_pp, av_cum_cost / start_cohort)
      this_utility_matrix <- matrix(unlist(utility_matrix_all[i]),
                                    ncol = no_states + 2, byrow = FALSE)
      av_cum_utility <- this_utility_matrix[cycles + 1, no_states + 2]
      av_cum_utility_pp <- append(av_cum_utility_pp, av_cum_utility /
                                    start_cohort)
    }
    list_icer <- list()
    list_nmb <- list()
    list_del_E <- list()
    list_del_C <- list()
    # find icer and NMB
    for (i in seq_len(no_comparison)) {
      if (comparator_index != 0) {
        if (i != comparator_index) {
            del_E <- (av_cum_utility_pp[[i]] -
                        av_cum_utility_pp[[comparator_index]])
            del_c <- (av_cum_cost_pp[[i]] -
                        av_cum_cost_pp[[comparator_index]])
            icer <- del_c / del_E
        } else {
            del_E <- NA
            del_c <- NA
            icer <- NA
        }
      }else{
        del_E <- NA
        del_c <- NA
        icer <- NA
      }
      if (threshold != 0 & !is.na(threshold) & is.numeric(threshold))
          nmb <- av_cum_utility_pp[[i]] * threshold - av_cum_cost_pp[[i]]
      else
          stop("Threshold value is not valid")
      list_icer <- append(list_icer, icer)
      list_nmb <- append(list_nmb, nmb)
      list_del_E <- append(list_del_E, del_E)
      list_del_C <- append(list_del_C, del_c)
    }
    # results and plot
    results_cea <- matrix(c(
      unlist(list_names_strategy), unlist(av_cum_cost_pp),
      unlist(av_cum_utility_pp),
      unlist(list_nmb), unlist(list_del_C), unlist(list_del_E),
      unlist(list_icer)

    ), ncol = 7, byrow = FALSE)
    colnames(results_cea) <- c("Strategy", "Cost", "Effect", "NMB",
                               "Inc_Cost", "Inc_Effect", "ICER")
    rownames(results_cea) <- NULL
    results_cea <- data.table::data.table(results_cea)
    return(results_cea)
  }
}
#######################################################################

#' check the list of Markov models
#' @param list_markov  list of Markov model objects with their Markov
#' trace, cost matrix and utility matrix
#' @return 0 if success else error
#' @examples
#' \donttest{
#' well <- health_state("well", cost = 0, utility = 1)
#' disabled <- health_state("disabled", cost = 100, utility = 1)
#' dead <- health_state("dead", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
#' colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
#' tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1),
#' colnames(tmat))
#' health_states <- combine_state(well, disabled, dead)
#' this.strategy <- strategy(tm, health_states, "example")
#' this_markov <- markov_model(this.strategy, 24, c(1000, 0, 0), c(0,0))
#' well <- health_state("well", cost = 0, utility = 1)
#' disabled <- health_state("disabled", cost = 10, utility = 0.5)
#' dead <- health_state("dead", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
#' colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
#' tm <- populate_transition_matrix(3, tmat, c(0.4, 0.4, 0.2, 0.6, 0.4, 1),
#' colnames(tmat))
#' health_states <- combine_state(well, disabled, dead)
#' this.strategy <- strategy(tm, health_states, "example_two")
#' sec_markov <- markov_model(this.strategy, 24, c(1000, 0, 0), c(0, 0))
#' list_markov <- combine_markov(this_markov, sec_markov)
#' check_list_markov_models(list_markov)
#' }
#' @export
check_list_markov_models <- function(list_markov) {
  if (is.null(list_markov)) stop("Error - list markov can not be null")
  no_comparison <- nrow(list_markov)
  no_states_all <- list()
  start_cohort_all <- list()
  names_strategies <- list()
  cycles_all <- list_markov[, "cycles"]
  for (i in 1:no_comparison) {
    names_strategies <- append(names_strategies,
                               list_markov[, "strategy"][[i]]$name_strategy)
  }
  if (length(unique(unlist(names_strategies))) < no_comparison) {
    stop("All strategies should have unique names")
  }
  if (length(unique(cycles_all)) != 1) {
    stop("All strategies should have same number of cycles")
  }
  for (i in seq_len(no_comparison)) {
    no_states_all <- append(no_states_all,
                  list_markov[, "strategy"][[i]]$transition_matrix$no_states)
    start_cohort_all <- append(start_cohort_all,
                               sum(list_markov[, "trace_matrix"][[i]][1, ]))
  }
  if (length(unique(no_states_all)) != 1) {
    stop("All strategies should have same number of states")
  }
  if (length(unique(start_cohort_all)) != 1) {
    stop("All strategies should have same number of population at start")
  }
  return(0)
}
#######################################################################

#' Plot cost effectiveness acceptability curve
#' @param list_markov  markov_model objects
#' @param threshold_values  list of threshold values
#' @param comparator  the comparator
#' @param currency currency
#' @return plots
#' @examples
#' \donttest{
#' well <- health_state("well", cost = 0, utility = 1)
#' disabled <- health_state("disabled", cost = 100, utility = 1)
#' dead <- health_state("dead", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
#' colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
#' tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1),
#' colnames(tmat))
#' health_states <- combine_state(well, disabled, dead)
#' this.strategy <- strategy(tm, health_states, "control")
#' this_markov <- markov_model(this.strategy, 24, c(1000, 0, 0),c(0, 0))
#' well <- health_state("well", cost = 0, utility = 1)
#' disabled <- health_state("disabled", cost = 10, utility = 0.5)
#' dead <- health_state("dead", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
#' colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
#' tm <- populate_transition_matrix(3, tmat, c(0.4, 0.4, 0.2, 0.6, 0.4, 1),
#' colnames(tmat))
#' health_states <- combine_state(well, disabled, dead)
#' this.strategy <- strategy(tm, health_states, "intervention")
#' sec_markov <- markov_model(this.strategy, 24, c(1000, 0, 0), c(0, 0))
#' list_markov <- combine_markov(this_markov, sec_markov)
#' plot_ceac(list_markov, c(1000, 2000, 3000), comparator = "control")
#' }
#' @export
plot_ceac <- function(list_markov, threshold_values, comparator,
                      currency = "GBP") {
  if (is.null(threshold_values))
    stop("Error - Threshold values can not be null")
  if (check_list_markov_models(list_markov) == 0) {
    nmb_all <- matrix(0, ncol = 2)
    colnames(nmb_all) <- c("NMB", "Threshold")
    if (length(threshold_values) <= 1)
      stop("Plotting with one or less values is not allowed")
    for (i in seq_len(length(threshold_values))) {
      result <- calculate_icer_nmb(list_markov, threshold_values[i],
                                   comparator)
      result_nmb <- c(as.numeric(result[2, "NMB"]), threshold_values[i])
      nmb_all <- rbind(nmb_all, result_nmb)
    }
    nmb_all <- (nmb_all[-1, ])
    rownames(nmb_all) <- NULL
    nmb_all <- as.data.frame(nmb_all)
    xvalues <- nmb_all[, 2]
    yvalues <- nmb_all[, 1]
    name_file_plot <- paste0("Cost-effectiveness acceptability curve.pdf",
                             sep = "")
    grDevices::pdf(name_file_plot)
    p <- ggplot2::ggplot(data = nmb_all, ggplot2::aes(x = xvalues,
                                                      y = yvalues, group = 1)) +
      ggplot2::geom_line(color = "red") +
      ggplot2::geom_point() +
      ggplot2::labs(
        title = "Cost-effectiveness acceptability curve",
        x = paste("Threshold values (", currency, ")", sep = " "), y = "NMB"
      )
    graphics::plot(p) # plot result
    grDevices::dev.off()
    return(p)
  }

}
#######################################################################
#' Plot efficiency frontier
#' @param results_calculate_icer_nmb  results from cea
#' (from calculate_icer_nmb method)
#' @param threshold  threshold value
#' @return plot
#' well <- health_state("well", cost = 0, utility = 1)
#' disabled <- health_state("disabled", cost = 100, utility = 1)
#' dead <- health_state("dead", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
#' colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
#' tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1),
#' colnames(tmat))
#' health_states <- combine_state(well, disabled, dead)
#' this.strategy <- strategy(tm, health_states, "control")
#' this_markov <- markov_model(this.strategy, 24, c(1000, 0, 0), c(0,0))
#' well <- health_state("well", cost = 0, utility = 1)
#' disabled <- health_state("disabled", cost = 10, utility = 0.5)
#' dead <- health_state("dead", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
#' colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
#' tm <- populate_transition_matrix(3, tmat, c(0.4, 0.4, 0.2, 0.6, 0.4, 1),
#' colnames(tmat))
#' health_states <- combine_state(well, disabled, dead)
#' this.strategy <- strategy(tm, health_states, "intervention")
#' sec_markov <- markov_model(this.strategy, 24, c(1000, 0, 0), c(0,0))
#' list_markov <- combine_markov(this_markov, sec_markov)
#' results_cea <- calculate_icer_nmb(list_markov, 20000, comparator = "control")
#' plot_efficiency_frontier(results_cea, c(1000, 2000))
#' @export
plot_efficiency_frontier <- function(results_calculate_icer_nmb, threshold) {
  results_calculate_icer_nmb$Cost <- as.numeric(results_calculate_icer_nmb$Cost)
  results_calculate_icer_nmb$Effect <- as.numeric(results_calculate_icer_nmb$Effect)
  results_calculate_icer_nmb$Inc_Cost <-
    as.numeric(results_calculate_icer_nmb$Inc_Cost)
  results_calculate_icer_nmb$Inc_Effect <-
    as.numeric(results_calculate_icer_nmb$Inc_Effect)
  results_calculate_icer_nmb$NMB <-
    as.numeric(results_calculate_icer_nmb$NMB)
  results_calculate_icer_nmb$ICER <-
    as.numeric(results_calculate_icer_nmb$ICER)

  if (is.null(results_calculate_icer_nmb) | is.null(threshold))
    stop("Error- reultss or threshold can not be null")
  needed_colnames <- c("Cost", "Effect")
  if (sum(colnames(results_calculate_icer_nmb) %in% needed_colnames) !=
      length(needed_colnames))
    stop("Error - columns do not contain the cost and effect ")
  if (nrow(results_calculate_icer_nmb) > 1) {
    name_file_plot <- paste0("Efficiency frontier", threshold,
                             ".pdf", sep = "")
    grDevices::pdf(name_file_plot)
    p <- ggplot2::ggplot(data = results_calculate_icer_nmb, ggplot2::aes(
      x = results_calculate_icer_nmb$Effect,
      y = results_calculate_icer_nmb$Cost)) +
      ggplot2::geom_point(color = "red", size = 3) +
      ggplot2::geom_text(ggplot2::aes(label = results_calculate_icer_nmb$Strategy),
                          hjust = -0.1, vjust = -0.5) +
      ggplot2::labs(title = "Efficiency frontier", x = "Effect (QALYs)",
                    y = "Cost")
    print(p)
    grDevices::dev.off()

    icer_result <- results_calculate_icer_nmb[!is.na(results_calculate_icer_nmb$ICER), ]
    name_file_plot <- paste0("Cost effectiveness plane",
                             ".pdf", sep = "")
    grDevices::pdf(name_file_plot)
    p <- ggplot2::ggplot(icer_result, ggplot2::aes(
      x = icer_result$Inc_Effect,
      y = icer_result$Inc_Cost)) +
      ggplot2::geom_abline(intercept = 0, slope = threshold, color = "red",
                           size = 2) +
      ggplot2::geom_point(color = "red", size = 3) +
      ggplot2::geom_text(ggplot2::aes(label = icer_result$Strategy),
                         hjust = -0.1, vjust = -0.5) +

      ggplot2::labs(title = "Cost effectiveness plane", x = "Delta Effect (QALYs)",
                    y = "Delta Cost")
    print(p)
    grDevices::dev.off()
  }
}
