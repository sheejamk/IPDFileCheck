#' Define parameter lists for deterministic sensitivity analysis
#' @param base_param_list  list of parameters that used to define Markov model
#' @param sample_list list of parameter values with their sampling distributions
#' @return table for probability sensitivity analysis
#' @examples
#' param_list <- define_parameters(
#'   cost_zido = 2278, cost_direct_med_A = 1701,
#'   cost_comm_care_A = 1055, cost_direct_med_B = 1774,
#'   cost_comm_care_B = 1278,
#'   cost_direct_med_C = 6948, cost_comm_care_C = 2059,
#'   tpAtoA = 1251 / (1251 + 483),
#'   tpAtoB = 350 / (350 + 1384), tpAtoC = 116 / (116 + 1618),
#'   tpAtoD = 17 / (17 + 1717),
#'   tpBtoB = 731 / (731 + 527), tpBtoC = 512 / (512 + 746),
#'   tpBtoD = 15 / (15 + 1243),
#'   tpCtoC = 1312 / (1312 + 437), tpCtoD = 437 / (437 + 1312), tpDtoD = 1,
#'   cost_health_A = "cost_direct_med_A+ cost_comm_care_A",
#'   cost_health_B = "cost_direct_med_B+ cost_comm_care_B",
#'   cost_health_C = "cost_direct_med_C+ cost_comm_care_C",
#'   cost_drug = "cost_zido"
#' )
#' sample_list <- define_parameters(cost_zido = "gamma(mean = 2756,
#' sd = sqrt(2756))")
#' param_table <- define_parameters_psa(param_list, sample_list)
#' @export
define_parameters_psa <- function(base_param_list, sample_list) {
  if (is.null(base_param_list) | is.null(sample_list)) {
    stop("Error - one or more of the parameters are null")
  }
  if (typeof(base_param_list) != "list" || typeof(sample_list) != "list") {
    stop("Parameter list should be of type list - use define_parameters")
  }
  sample_list_all <- base_param_list
  short1 <- names(sample_list)
  long <- names(base_param_list)
  index1 <- match(short1, long)
  if (sum(is.na(index1)) == length(short1)) {
    stop("Parameters in sample list should be there in base parameter list")
  }
  len <- length(index1)
  for (i in 1:len) {
    if (!is.na(index1[i])) {
      this_name <- names(sample_list_all[index1[i]])
      sample_list_all[index1[i]] <- sample_list[this_name]
    }
  }
  psa_table <- structure(list(
    base_param_list = base_param_list,
    sample_list = sample_list_all
  ))
  return(psa_table)
}
#######################################################################
#' Function to do probabilistic sensitivity analysis
#' @param this_markov  Markov model object
#' @param psa_table table object from define_parameters_psa
#' @param num_rep number of repetitions
#' @return result after sensitivity analysis
#' @examples
#' \donttest{
#' param_list <- define_parameters(
#'   cost_zido = 2278, cost_direct_med_A = 1701,
#'   cost_comm_care_A = 1055, cost_direct_med_B = 1774,
#'   cost_comm_care_B = 1278,
#'   cost_direct_med_C = 6948, cost_comm_care_C = 2059,
#'   tpAtoA = 1251 / (1251 + 483),
#'   tpAtoB = 350 / (350 + 1384), tpAtoC = 116 / (116 + 1618),
#'   tpAtoD = 17 / (17 + 1717),
#'   tpBtoB = 731 / (731 + 527), tpBtoC = 512 / (512 + 746),
#'   tpBtoD = 15 / (15 + 1243),
#'   tpCtoC = 1312 / (1312 + 437), tpCtoD = 437 / (437 + 1312), tpDtoD = 1,
#'   cost_health_A = "cost_direct_med_A+ cost_comm_care_A",
#'   cost_health_B = "cost_direct_med_B+ cost_comm_care_B",
#'   cost_health_C = "cost_direct_med_C+ cost_comm_care_C",
#'   cost_drug = "cost_zido"
#' )
#' A <- health_state("A", cost = "cost_health_A+ cost_drug ", utility = 1)
#' B <- health_state("B", cost = "cost_health_B + cost_drug", utility = 1)
#' C <- health_state("C", cost = "cost_health_C + cost_drug", utility = 1)
#' D <- health_state("D", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3, 4), c(NA, 5, 6, 7), c(NA, NA, 8, 9),
#' c(NA, NA, NA, 10))
#' colnames(tmat) <- rownames(tmat) <- c("A", "B", "C", "D")
#' tm <- populate_transition_matrix(4, tmat, c(
#'   "tpAtoA", "tpAtoB", "tpAtoC", "tpAtoD",
#'   "tpBtoB", "tpBtoC", "tpBtoD", "tpCtoC", "tpCtoD", "tpDtoD"
#' ), colnames(tmat))
#' health_states <- combine_state(A, B, C, D)
#' mono_strategy <- strategy(tm, health_states, "mono")
#' mono_markov <- markov_model(mono_strategy, 20, discount = c(0.06, 0),
#' initial_state =c(1,0,0,0),param_list)
#' sample_list <- define_parameters(cost_zido = "gamma(mean = 2756,
#' sd = sqrt(2756))")
#' param_table <- define_parameters_psa(param_list, sample_list)
#' result <- do_psa(mono_markov, param_table, 10)
#' }
#' @export
do_psa <- function(this_markov, psa_table, num_rep) {
  if (class(this_markov) != "markov_model") {
    stop("Error - the model should be of class markov_model")
  }
  check <- sum(names(psa_table) == c("base_param_list", "sample_list"))
  if (check != 2) {
    stop("parameter table should have  2 entries  - default parameter values,
         and parameters with sampling distributions")
  }
  no_entries <- length(psa_table$base_param_list)
  result_all_params <- list()
  names_rep <- list()
  this_markov_rep_all <- list()
  this_param_list <- NULL
  if (num_rep <= 0) {
    warning("Returning the base case scenario as number of repetitions <= 0")
  }
  for (j in 1:num_rep) {
    for (i in 1:no_entries) {
      this_name <- names(psa_table$base_param_list[i])
      if (psa_table$base_param_list[i][[this_name]] !=
          psa_table$sample_list[i][[this_name]]) {
        this_param_list <- psa_table$base_param_list
        the_expr <- psa_table$sample_list[i][[this_name]]
        the_real_expr <- check_estimate_substitute_proper_params(the_expr)
        this_param_list[i][[this_name]] <- eval(parse(text = the_real_expr))
      }
    }
    if (is.null(this_param_list)) {
       this_param_list <- psa_table$base_param_list
    }
    this_markov_rep <- markov_model(
      this_markov$strategy, this_markov$cycles, this_markov$initial_state,
      this_markov$discount, this_param_list, this_markov$half_cycle_correction,
      this_markov$state_cost_only_prevalent,
      this_markov$state_util_only_prevalent,
      this_markov$method,
      this_markov$startup_cost, this_markov$startup_util
    )
    this_markov_rep_all <- append(this_markov_rep_all, list(this_markov_rep))
    names_rep <- append(names_rep, paste("rep_", j, sep = ""))
  }
  names(this_markov_rep_all) <- unlist(names_rep)
  if (j == num_rep) {
    names_all <- c(names(this_markov_rep_all), "base_result")
    result_all_params <- append(this_markov_rep_all, list(this_markov))
    names(result_all_params) <- names_all
  }
  return(result_all_params)
}
#######################################################################
#' Function to list probabilistic sensitivity analysis results parameterwise
#' @param result_psa_params_control  result from probabilistic
#' sensitivity analysis for first or control model
#' @param result_psa_params_treat result from probabilistic sensitivity
#'  analysis for the comparative Markov model
#' @param threshold threshold value of WTP
#' @param comparator the strategy to be compared with
#' @return plot of  sensitivity analysis
#' @examples
#' \donttest{
#' param_list <- define_parameters(
#'   cost_zido = 2278, cost_direct_med_A = 1701,
#'   cost_comm_care_A = 1055, cost_direct_med_B = 1774,
#'   cost_comm_care_B = 1278,
#'   cost_direct_med_C = 6948, cost_comm_care_C = 2059,
#'   tpAtoA = 1251 / (1251 + 483),
#'   tpAtoB = 350 / (350 + 1384), tpAtoC = 116 / (116 + 1618),
#'   tpAtoD = 17 / (17 + 1717),
#'   tpBtoB = 731 / (731 + 527), tpBtoC = 512 / (512 + 746),
#'   tpBtoD = 15 / (15 + 1243),
#'   tpCtoC = 1312 / (1312 + 437), tpCtoD = 437 / (437 + 1312), tpDtoD = 1,
#'   cost_health_A = "cost_direct_med_A + cost_comm_care_A",
#'   cost_health_B = "cost_direct_med_B + cost_comm_care_B",
#'   cost_health_C = "cost_direct_med_C + cost_comm_care_C",
#'   cost_drug = "cost_zido"
#' )
#' A <- health_state("A", cost = "cost_health_A + cost_drug ", utility = 1)
#' B <- health_state("B", cost = "cost_health_B + cost_drug", utility = 1)
#' C <- health_state("C", cost = "cost_health_C + cost_drug", utility = 1)
#' D <- health_state("D", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3, 4), c(NA, 5, 6, 7), c(NA, NA, 8, 9),
#' c(NA, NA, NA, 10))
#' colnames(tmat) <- rownames(tmat) <- c("A", "B", "C", "D")
#' tm <- populate_transition_matrix(4, tmat, c(
#'   "tpAtoA", "tpAtoB", "tpAtoC", "tpAtoD",
#'   "tpBtoB", "tpBtoC", "tpBtoD", "tpCtoC", "tpCtoD", "tpDtoD"
#' ), colnames(tmat))
#' health_states <- combine_state(A, B, C, D)
#' mono_strategy <- strategy(tm, health_states, "mono")
#' mono_markov <- markov_model(mono_strategy, 20, initial_state = c(1,0,0,0),
#' discount = c(0.06, 0),param_list)
#' sample_list <- define_parameters(cost_zido = "gamma(mean = 2756,
#' sd = sqrt(2756))")
#' param_table <- define_parameters_psa(param_list, sample_list)
#' result <- do_psa(mono_markov, param_table, 10)
#' list_paramwise_psa_result(result, NULL, NULL, NULL)
#' }
#' @export
list_paramwise_psa_result <- function(result_psa_params_control,
                                      result_psa_params_treat,
                                      threshold, comparator) {
  if (is.null(result_psa_params_control))
    stop("Error - result for psa for control strategy should not be null")
  if (!is.null(result_psa_params_treat)) {
    if (is.null(threshold) | is.null(comparator) | !is.numeric(threshold) |
        threshold <= 0)
      stop("Error - threshold values/comparator is not valid")
  }
  len <- length(result_psa_params_control) - 1
  results_all_param_mat <- data.frame()
  results_cost_util <- data.frame()
  results_icer_nmb <- data.frame()
  first_name <- names(result_psa_params_control)[1]
  no_params_parammatrix <-
    ncol(result_psa_params_control[[first_name]]$param_matrix)
  for (i in 1:len) {
    this_var_result_name <- names(result_psa_params_control)[i]
    if (!is.null(result_psa_params_treat)) {
      var_name_treat <- names(result_psa_params_treat)[i]
      control_params <-
      colnames(result_psa_params_control[[this_var_result_name]]$param_matrix)
      treat_params  <-
        colnames(result_psa_params_treat[[var_name_treat]]$param_matrix)

      if (length(control_params) <=  length(treat_params)) {
          mem_sum <- sum(control_params  %in% treat_params)
        if (mem_sum != length(control_params))
          stop("Error - Parameters seems to be different")
      } else {
         mem_sum <- sum(treat_params %in% control_params)
         if (mem_sum != length(treat_params))
           stop("Error - Parameters seems to be different")
      }
      if (this_var_result_name != var_name_treat) {
        stop("Repetition names are different")
      }
      treat_len <- length(result_psa_params_treat) - 1
      if (treat_len != len) {
        stop("No of repetitions are different")
      }
      list_markov <- combine_markov(
        result_psa_params_control[[this_var_result_name]],
        result_psa_params_treat[[this_var_result_name]]
      )

      res_icer_nmb <- calculate_icer_nmb(list_markov, threshold, comparator)
      res_icer_nmb_df <- data.frame(res_icer_nmb)
      comparator_row <- which(res_icer_nmb_df$Strategy == comparator)
      results_icer_nmb <-
        rbind(results_icer_nmb, res_icer_nmb_df[-comparator_row, ])
      colnames(results_icer_nmb) <- colnames(res_icer_nmb_df)
    } else {
      this_var_result <- result_psa_params_control[[this_var_result_name]]
      cost_matr <- this_var_result[["cost_matrix"]]
      util_matr <- this_var_result[["utility_matrix"]]
      this_cost <- cost_matr[nrow(cost_matr), ncol(cost_matr)]
      this_util <- util_matr[nrow(util_matr), ncol(util_matr)]
      results_cost_util <- rbind(results_cost_util, c(this_cost, this_util))
      results_this_rep <- list()
      for (j in 2:no_params_parammatrix) {
        param_matr <- this_var_result[["param_matrix"]]
        this_value <- param_matr[nrow(param_matr), j]
        results_this_rep <- append(results_this_rep, this_value)
      }
      results_all_param_mat <- rbind(results_all_param_mat, results_this_rep)
    }
  }
  if (!is.null(result_psa_params_treat)) {
    names(results_icer_nmb) <- colnames(res_icer_nmb)
    results_all <- as.data.frame(list(results_icer_nmb))
    results_all[["rep"]] <- seq(1:len)
  }
  else {
    names(results_cost_util) <- c("cost", "utility")
    results_all <- as.data.frame(list(results_cost_util, results_all_param_mat))
    results_all[["rep"]] <- seq(1:len)
  }
  return(results_all)
}
#######################################################################
#' Function to summarise and plot probabilistic sensitivity analysis
#' @param result_psa_params_control  result from probabilistic
#'  sensitivity analysis for first or control model
#' @param result_psa_params_treat result from probabilistic sensitivity
#' analysis for the comparative Markov model
#' @param threshold threshold value of WTP
#' @param comparator the strategy to be compared with
#' @return plot of  sensitivity analysis
#' @examples
#' \donttest{
#' param_list <- define_parameters(
#' cost_direct_med_A = 1701,
#' cost_direct_med_B = 1774, tpAtoA = 0.2,
#'  tpAtoB = 0.5, tpAtoC = 0.3,
#'  tpBtoB = 0.3, tpBtoC = 0.7,
#'  tpCtoC = 1,cost_health_A = "cost_direct_med_A",
#'  cost_health_B = "cost_direct_med_B")
#'  sample_list <- define_parameters(cost_direct_med_A = "gamma(mean = 1701,
#'  sd = sqrt(1701))")
#'  A <- health_state("A", cost = "cost_health_A ", utility = 1)
#'  B <- health_state("B", cost = "cost_health_B", utility = 1)
#'  C <- health_state("C", cost = 0, utility = 0, absorb = "TRUE")
#'  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
#'  colnames(tmat) <- rownames(tmat) <- c("A", "B", "C")
#'  tm <- populate_transition_matrix(3, tmat, c(
#'  "tpAtoA", "tpAtoB", "tpAtoC",  "tpBtoB", "tpBtoC", "tpCtoC"),
#'  colnames(tmat))
#'  health_states <- combine_state(A, B, C)
#'  mono_strategy <- strategy(tm, health_states, "mono")
#'  mono_markov <- markov_model(mono_strategy, 20, initial_state =c(1,0,0),
#'  discount = c(0.06, 0),param_list)
#'  param_table <- define_parameters_psa(param_list, sample_list)
#'  result <- do_psa(mono_markov, param_table, 3)
#'  result_plot <- summary_plot_psa(result, NULL, NULL, NULL)
#'  param_list_comb <- define_parameters(
#'  cost_direct_med_A = 1800, cost_direct_med_B = 1774, tpAtoA = 0.6,
#'  tpAtoB = 0.1, tpAtoC = 0.3,tpBtoB = 0.3, tpBtoC = 0.7,tpCtoC = 1,
#'  cost_health_A = "cost_direct_med_A",cost_health_B = "cost_direct_med_B")
#'  comb_strategy <- strategy(tm, health_states, "comb")
#'  comb_markov <- markov_model(comb_strategy, 20, c(1, 0, 0),
#'  discount = c(0.06, 0), param_list)
#'  param_table_comb <- define_parameters_psa(param_list_comb, sample_list)
#'  result_comb <- do_psa(comb_markov, param_table_comb, 3)
#'  summary_plot_psa(result, result_comb, 2000, "mono")
#'  }
#' @export
summary_plot_psa <- function(result_psa_params_control,
                             result_psa_params_treat = NULL, threshold = NULL,
                             comparator = NULL) {
  if (is.null(result_psa_params_control))
    stop("Error - result for psa for control strategy should not be null")
  if (!is.null(result_psa_params_treat)) {
    if (is.null(threshold) | is.null(comparator) | !is.numeric(threshold) |
        threshold <= 0)
      stop("Error - threshold values/comparator is not valid")
  }
  if (!is.null(result_psa_params_treat)) {
    list_all <- list_paramwise_psa_result(
      result_psa_params_control, result_psa_params_treat,
      threshold, comparator
    )

    list_all_df <- data.frame(list_all, stringsAsFactors = FALSE)
    this_icer <- as.numeric(as.character(list_all_df$ICER))
    this_nmb <- as.numeric(as.character(list_all_df$NMB))
    mean_icer <- mean(this_icer)
    mean_nmb <- mean(this_nmb)
    sd_icer <- stats::sd(this_icer)
    sd_nmb <- stats::sd(this_nmb)
    name_file_plot <- paste0("Probabilistic sensitivity analysis_ICER_NMB.pdf",
                             sep = "")
    grDevices::pdf(name_file_plot)
    plot1 <- ggplot2::ggplot(data = list_all, ggplot2::aes(
      x = this_icer,
      y = this_nmb
    )) +
      ggplot2::geom_point(color = "blue", size = 3) +
      ggplot2::labs(x = "ICER") +
      ggplot2::labs(y = "NMB") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    graphics::plot(plot1) # plot result
    grDevices::dev.off()

    name_file_plot <- paste0("Histogram of ICER.pdf", sep = "")
    grDevices::pdf(name_file_plot)
    plot2 <- ggplot2::ggplot(data = list_all, ggplot2::aes(x = this_icer)) +
      ggplot2::geom_histogram(bins = 20, color = "black", fill = "white") +
      ggplot2::labs(title = "Histogram of ICER", x = "ICER", y = "Count") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(this_icer)),
        color = "blue", linetype = "dashed", size = 1
      ) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    graphics::plot(plot2) # plot result
    grDevices::dev.off()

    name_file_plot <- paste0("Histogram of NMB.pdf", sep = "")
    grDevices::pdf(name_file_plot)
    plot3 <- ggplot2::ggplot(data = list_all, ggplot2::aes(x = this_nmb)) +
      ggplot2::geom_histogram(bins = 20, color = "black", fill = "white") +
      ggplot2::labs(title = "Histogram of NMB", x = "NMB", y = "Count") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(this_nmb)),
        color = "red", linetype = "dashed", size = 1
      ) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    graphics::plot(plot3) # plot result
    grDevices::dev.off()

    result <- structure(list(
      mean_icer = mean_icer,
      mean_nmb = mean_nmb,
      sd_icer = sd_icer,
      sd_nmb = sd_nmb,
      icer_nmb_plot = plot1,
      hist_icer = plot2,
      hist_nmb = plot3
    ))
  } else {
    list_all <- list_paramwise_psa_result(result_psa_params_control,
      result_psa_params_treat = NULL, threshold = NULL, comparator = NULL
    )
    no_entries <- ncol(list_all) - 1
    mean_all <- data.frame()
    sd_all <- data.frame()
    for (i in 1:no_entries) {
      mean_this_entry <- mean(list_all[, i])
      sd_this_entry <- stats::sd(list_all[, i])
      mean_all <- append(mean_all, mean_this_entry)
      sd_all <- append(sd_all, sd_this_entry)
    }
    names(mean_all) <- colnames(list_all[1:no_entries])
    names(sd_all) <- colnames(list_all[1:no_entries])

    name_file_plot <- paste0("Utility or Effect.pdf", sep = "")
    grDevices::pdf(name_file_plot)
    plot1 <- ggplot2::ggplot(data = list_all, ggplot2::aes_(
      x = ~utility,
      y = ~cost
    )) +
      ggplot2::geom_point(color = "blue", size = 3) +
      ggplot2::labs(x = "Utility / Effect") +
      ggplot2::labs(y = "Cost") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    graphics::plot(plot1) # plot result
    grDevices::dev.off()

    name_file_plot <- paste0("Histogram of Cost.pdf", sep = "")
    grDevices::pdf(name_file_plot)
    plot2 <- ggplot2::ggplot(data = list_all, ggplot2::aes_(x = ~cost)) +
      ggplot2::geom_histogram(bins = 20, color = "black", fill = "white") +
      ggplot2::labs(title = "Histogram of cost", x = "Cost", y = "Count") +
      ggplot2::geom_vline(ggplot2::aes_(xintercept = mean(list_all$cost)),
        color = "blue", linetype = "dashed", size = 1
      ) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    graphics::plot(plot2) # plot result
    grDevices::dev.off()

    name_file_plot <- paste0("Histogram of Utility.pdf", sep = "")
    grDevices::pdf(name_file_plot)
    plot3 <- ggplot2::ggplot(data = list_all, ggplot2::aes_(x = ~utility)) +
      ggplot2::geom_histogram(bins = 20, color = "black", fill = "white") +
      ggplot2::labs(title = "Histogram of utility values", x = "Utility",
                    y = "Count") +
      ggplot2::geom_vline(ggplot2::aes_(xintercept = mean(list_all$utility)),
        color = "red", linetype = "dashed", size = 1
      ) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    graphics::plot(plot3) # plot result
    grDevices::dev.off()

    result <- structure(list(
      mean_cost = mean_all$cost,
      mean_utility = mean_all$utility,
      sd_cost = sd_all$cost,
      sd_utility = sd_all$utility,
      cost_utility_plot = plot1,
      hist_cost = plot2,
      hist_utility = plot3
    ))
  }
  return(result)
}
