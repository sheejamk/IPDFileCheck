#######################################################################
#' Define parameter lists for deterministic sensitivity analysis
#' @param param_list  list of parameters that used to define Markov model
#' @param low_values list of lower values of those parameters for whom the
#' sensitivity is to be estimated
#' @param upp_values list of upper values of those parameters for whom the
#' sensitivity is to be estimated
#' @return table for sensitivity analysis
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
#'   cost_health_A = "cost_direct_med_A +  cost_comm_care_A",
#'   cost_health_B = "cost_direct_med_B +  cost_comm_care_B",
#'   cost_health_C = "cost_direct_med_C +  cost_comm_care_C",
#'   cost_drug = "cost_zido"
#' )
#' low_values <- define_parameters(cost_direct_med_B = 177.4,
#' cost_comm_care_C = 205.9)
#' upp_values <- define_parameters(cost_direct_med_B = 17740,
#' cost_comm_care_C = 20590)
#' param_table <- define_parameters_sens_anal(param_list, low_values,
#' upp_values)
#' @export
#' @details
#' Get the parameter list, min and maximum values of the parameters.
#' The min and max values should have same entries, but they should be
#' contained in param_list too. Copy the exact values of parameters that
#' are in param list but not in min and max values
define_parameters_sens_anal <- function(param_list, low_values, upp_values) {
  if (is.null(param_list) | is.null(low_values) | is.null(upp_values)) {
    stop("Error - one or more of the parameters are null")
  }
  if (typeof(param_list) != "list" || typeof(low_values) != "list" ||
      typeof(upp_values) != "list") {
    stop("Error - Parameter list should be of type list")
  }
  low_values_all <- param_list
  upp_values_all <- param_list
  short1 <- names(low_values)
  short2 <- names(upp_values)
  long <- names(param_list)
  index1 <- match(short1, long)
  index2 <- match(short2, long)
  len1 <- length(index1)
  len2 <- length(index2)
  if (len1 != len2) {
    stop("Error -list of min and max values should have same entries")
  }
  for (i in 1:len1) {
    if (!is.na(index1[i])) {
      # if the name matches copy the parameter values to min and max list
      this_name <- names(low_values_all[index1[i]])
      upp_values_all[index2[i]] <- upp_values[this_name]
      low_values_all[index1[i]] <- low_values[this_name]
    }
  }
  dsa_table <- structure(list(param_list = param_list,
                              low_values = low_values_all,
                              upp_values = upp_values_all))
  return(dsa_table)
}
#######################################################################
#' Function to do deterministic sensitivity analysis
#' @param this_markov  Markov model object
#' @param param_table table object from define_parameters_sens_anal()
#' with parameters (base case value, lower and upper)
#' @return result after sensitivity analysis
#' @examples
#' \donttest{
#' param_list <- define_parameters(
#' cost_zido = 2278, cost_direct_med_A = 1701,
#' cost_comm_care_A = 1055, cost_direct_med_B = 1774,
#' cost_comm_care_B = 1278,
#' cost_direct_med_C = 6948, cost_comm_care_C = 2059,
#' tpAtoA = 1251 / (1251 + 483),
#' tpAtoB = 350 / (350 + 1384), tpAtoC = 116 / (116 + 1618),
#' tpAtoD = 17 / (17 + 1717),
#' tpBtoB = 731 / (731 + 527), tpBtoC = 512 / (512 + 746),
#' tpBtoD = 15 / (15 + 1243),
#' tpCtoC = 1312 / (1312 + 437), tpCtoD = 437 / (437 + 1312),
#' tpDtoD = 1,
#' cost_health_A = "cost_direct_med_A +  cost_comm_care_A",
#' cost_health_B = "cost_direct_med_B +  cost_comm_care_B",
#' cost_health_C = "cost_direct_med_C +  cost_comm_care_C",
#' cost_drug = "cost_zido")
#' low_values <- define_parameters(cost_direct_med_B = 177.4,
#' cost_comm_care_C = 205.9)
#' upp_values <- define_parameters(cost_direct_med_B = 17740,
#' cost_comm_care_C = 20590)
#' A <- health_state("A", cost = "cost_health_A +  cost_drug ", utility = 1)
#' B <- health_state("B", cost = "cost_health_B + cost_drug", utility = 1)
#' C <- health_state("C", cost = "cost_health_C + cost_drug", utility = 1)
#' D <- health_state("D", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3, 4), c(NA, 5, 6, 7), c(NA, NA, 8, 9),
#' c(NA, NA, NA, 10))
#' colnames(tmat) <- rownames(tmat) <- c("A", "B", "C", "D")
#' tm <- populate_transition_matrix(4, tmat, c("tpAtoA", "tpAtoB", "tpAtoC",
#' "tpAtoD","tpBtoB", "tpBtoC", "tpBtoD", "tpCtoC", "tpCtoD", "tpDtoD"),
#' colnames(tmat))
#' health_states <- combine_state(A, B, C, D)
#' mono_strategy <- strategy(tm, health_states, "mono")
#' mono_markov <- markov_model(mono_strategy, 20, c(1, 0, 0, 0),
#' discount = c(0.06, 0), param_list)
#' param_table <- define_parameters_sens_anal(param_list, low_values,
#' upp_values)
#' result <- do_sensitivity_analysis(mono_markov, param_table)
#' }
#' @export
do_sensitivity_analysis <- function(this_markov, param_table) {
  #checking for null error
  if (is.null(this_markov) | is.null(param_table)) {
    stop("Error - markov model or parameter table can not be null")
  }
  if (class(this_markov) != "markov_model") {
    stop("Error - the model should be of class markov_model")
  }
  check <- sum(names(param_table) %in%
                 c("param_list", "low_values", "upp_values"))
  if (check != 3) {
    stop("parameter table should have 3 entries  -
         default parameter value, low and upp values")
  }
  this_markov_param <- this_markov
  no_entries <- length(param_table$param_list)
  all_param <- list()
  # run the model for all the list of parameters in param table,
  # mean , min and max values
  for (i in 1:no_entries) {
    this_name <- names(param_table$param_list[i])
    if (param_table$param_list[i][[this_name]] !=
        param_table$low_values[i][[this_name]]) {
      this_param_list <- param_table$param_list
      this_param_list[i][[this_name]] <- param_table$low_values[i][[this_name]]
      this_markov_low <- markov_model(
        this_markov$strategy, this_markov$cycles, this_markov$initial_state,
        this_markov$discount, this_param_list,
        this_markov$half_cycle_correction,
        this_markov$state_cost_only_prevalent,
        this_markov$state_util_only_prevalent,
        this_markov$method,
        this_markov$startup_cost, this_markov$startup_util
      )
      this_param_list[i][[this_name]] <- param_table$upp_values[i][[this_name]]
      this_markov_upp <- markov_model(
        this_markov$strategy, this_markov$cycles, this_markov$initial_state,
        this_markov$discount,
        this_param_list,
        this_markov$half_cycle_correction,
        this_markov$state_cost_only_prevalent,
        this_markov$state_util_only_prevalent,
        this_markov$method,
        this_markov$startup_cost, this_markov$startup_util
      )
      name_var2 <- paste("low_result", this_name, sep = "_")
      name_var3 <- paste("upp_result", this_name, sep = "_")
      name_var1 <- paste("result", this_name, sep = "_")
      all_this_param <- (list(this_markov_param, this_markov_low,
                              this_markov_upp))
      names(all_this_param) <- c(name_var1, name_var2, name_var3)
      all_param <- append(all_param, all_this_param)
    }
  }
  result_dsa <- structure(all_param)
  return(result_dsa)
}
#######################################################################

#' Function to report deterministic sensitivity analysis
#' @param result_dsa_control  result from deterministic sensitivity analysis
#' for first or control model
#' @param result_dsa_treat result from deterministic sensitivity analysis
#' for the comparative Markov model
#' @param threshold threshold value of WTP
#' @param comparator the strategy to be compared with
#' @return report in the form of a table
#' @examples
#' \donttest{
#' param_list <- define_parameters(
#' cost_zido = 2278, cost_direct_med_A = 1701,
#' cost_comm_care_A = 1055, cost_direct_med_B = 1774,
#' cost_comm_care_B = 1278,
#' cost_direct_med_C = 6948, cost_comm_care_C = 2059,
#' tpAtoA = 1251 / (1251 + 483),
#' tpAtoB = 350 / (350 + 1384), tpAtoC = 116 / (116 + 1618),
#' tpAtoD = 17 / (17 + 1717),
#' tpBtoB = 731 / (731 + 527), tpBtoC = 512 / (512 + 746),
#' tpBtoD = 15 / (15 + 1243),
#' tpCtoC = 1312 / (1312 + 437), tpCtoD = 437 / (437 + 1312),
#' tpDtoD = 1,
#' cost_health_A = "cost_direct_med_A +  cost_comm_care_A",
#' cost_health_B = "cost_direct_med_B +  cost_comm_care_B",
#' cost_health_C = "cost_direct_med_C +  cost_comm_care_C",
#' cost_drug = "cost_zido")
#' low_values <- define_parameters(cost_direct_med_B = 177.4,
#' cost_comm_care_C = 205.9)
#' upp_values <- define_parameters(cost_direct_med_B = 17740,
#' cost_comm_care_C = 20590)
#' A <- health_state("A", cost = "cost_health_A +  cost_drug ", utility = 1)
#' B <- health_state("B", cost = "cost_health_B + cost_drug", utility = 1)
#' C <- health_state("C", cost = "cost_health_C + cost_drug", utility = 1)
#' D <- health_state("D", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3, 4), c(NA, 5, 6, 7), c(NA, NA, 8, 9),
#' c(NA, NA, NA, 10))
#' colnames(tmat) <- rownames(tmat) <- c("A", "B", "C", "D")
#' tm <- populate_transition_matrix(4, tmat, c("tpAtoA", "tpAtoB", "tpAtoC",
#' "tpAtoD","tpBtoB", "tpBtoC", "tpBtoD", "tpCtoC", "tpCtoD", "tpDtoD"),
#' colnames(tmat))
#' health_states <- combine_state(A, B, C, D)
#' mono_strategy <- strategy(tm, health_states, "mono")
#' mono_markov <- markov_model(mono_strategy, 20, c(1, 0, 0, 0),
#' discount = c(0.06, 0), param_list)
#' param_table <- define_parameters_sens_anal(param_list, low_values,
#' upp_values)
#' result <- do_sensitivity_analysis(mono_markov, param_table)
#'  reporting <- report_sensitivity_analysis(result)
#'  }
#' @export
report_sensitivity_analysis <- function(result_dsa_control,
                                        result_dsa_treat = NULL,
                                        threshold = NULL,
                                        comparator = NULL) {
  if (is.null(result_dsa_control) | typeof(result_dsa_control) != "list")
    stop("Error - parameter null or not of type list")
  len <- length(result_dsa_control)
  results_all_param <- data.frame()
  results_cost_util <- data.frame()
  results_icer_nmb <- data.frame()
  names <- list()
  for (i in seq(1, len, 3)) {
    results_this_param <- data.frame()
    this_var_result_name <- names(result_dsa_control)[i]
    low_var_result_name <- names(result_dsa_control)[i + 1]
    upp_var_result_name <- names(result_dsa_control)[i + 2]
    index <- stringr::str_locate(this_var_result_name, "result_")
    this_var_name <- substr(this_var_result_name, index[2] + 1,
                            nchar(this_var_result_name))
    if (!is.null(result_dsa_treat)) {
      list_markov <- combine_markov(result_dsa_control[[this_var_result_name]],
                                    result_dsa_treat[[this_var_result_name]])
      results_icer_nmb_base <- as.data.frame(calculate_icer_nmb(list_markov,
                                                  threshold, comparator))
      results_icer_nmb_base[["parameter"]] <- c(this_var_name, this_var_name)
      results_icer_nmb_base[["value_limit"]] <- c("base", "base")
      results_icer_nmb <- rbind(results_icer_nmb, results_icer_nmb_base)
      list_markov <- combine_markov(result_dsa_control[[low_var_result_name]],
                                  result_dsa_treat[[low_var_result_name]])
      results_icer_nmb_low <- as.data.frame(calculate_icer_nmb(list_markov,
                                                    threshold, comparator))
      results_icer_nmb_low[["parameter"]] <- c(this_var_name, this_var_name)
      results_icer_nmb_low[["value_limit"]] <- c("lower", "lower")

      results_icer_nmb <- rbind(results_icer_nmb, results_icer_nmb_low)
      list_markov <- combine_markov(result_dsa_control[[upp_var_result_name]],
                                    result_dsa_treat[[upp_var_result_name]])
      results_icer_nmb_upp <- as.data.frame(calculate_icer_nmb(list_markov,
                                                  threshold, comparator))
      results_icer_nmb_upp[["parameter"]] <- c(this_var_name, this_var_name)
      results_icer_nmb_upp[["value_limit"]] <- c("upper", "upper")

      results_icer_nmb <- rbind(results_icer_nmb, results_icer_nmb_upp)
      names <- append(names, this_var_name)
    } else {
      # markov model results corresponding the default , min and max values
      this_var_result <- result_dsa_control[[this_var_result_name]]
      cost_matr <- this_var_result[["cost_matrix"]]
      util_matr <- this_var_result[["utility_matrix"]]
      mean_cost <- cost_matr[nrow(cost_matr), ncol(cost_matr)]
      mean_util <- util_matr[nrow(util_matr), ncol(util_matr)]

      low_var_result <- result_dsa_control[[low_var_result_name]]
      cost_matr_low <- low_var_result[["cost_matrix"]]
      util_matr_low <- low_var_result[["utility_matrix"]]
      low_cost <- cost_matr_low[nrow(cost_matr_low), ncol(cost_matr_low)]
      low_util <- util_matr_low[nrow(util_matr_low), ncol(util_matr_low)]

      upp_var_result <- result_dsa_control[[upp_var_result_name]]
      cost_matr_upp <- upp_var_result[["cost_matrix"]]
      util_matr_upp <- upp_var_result[["utility_matrix"]]
      upp_cost <- cost_matr_upp[nrow(cost_matr_upp), ncol(cost_matr_upp)]
      upp_util <- util_matr_upp[nrow(util_matr_upp), ncol(util_matr_upp)]
      results_cost_util <- rbind(results_cost_util, c(mean_cost, mean_util,
                                  low_cost, low_util, upp_cost, upp_util))
      ending <- ncol(this_var_result$param_matrix)
      for (j in 2:ending) {
        param_matr <- this_var_result[["param_matrix"]]
        this_mean <- param_matr[nrow(param_matr), j]
        param_matr_low <- low_var_result[["param_matrix"]]
        this_low <- param_matr_low[nrow(param_matr_low), j]
        param_matr_upp <- upp_var_result[["param_matrix"]]
        this_upp <- param_matr_upp[nrow(param_matr_upp), j]
        results_this_param <- rbind(results_this_param,
                                    c(this_mean, this_low, this_upp))
      }
      names(results_this_param) <- c("mean", "low", "upper")
      results_all_param <- rbind(results_all_param, results_this_param)
      names <- append(names, this_var_name)
    }
  }
  if (!is.null(result_dsa_treat)) {
    results_icer_nmb_all <- data.frame(results_icer_nmb)
    return(results_icer_nmb_all)
  } else {
    results_all_param_all <- data.frame(results_all_param)
    num_par_varied <- length(names)
    all_param_col <- list()
    for (kk in 1:num_par_varied) {
      this_list <- rep(unlist(names)[kk], ending - 1)
      all_param_col <- append(all_param_col, this_list)
    }
    results_all_param_all[["parameter varied"]] <- all_param_col
    cols <- colnames(result_dsa_control$result_cost_direct_med_B$param_matrix)
    results_all_param_all[["estimated param"]] <- rep(cols[2:ending], len / 3)
    colnames(results_all_param_all) <- c("base", "lower", "upper ",
                                         "parameter varied", "estimated param")
    results_cost_util_all <- data.frame(results_cost_util)
    results_cost_util_all[["parameter"]] <- unlist(names)
    colnames(results_cost_util_all) <- c("base cost", "base util",
                            "lower cost", "lower util", "upper cost",
                            "upper util", "parameter")
    return(structure(list(results_all_param_all, results_cost_util_all)))
  }
}
#######################################################################
#' Function to plot results of sensitivity analysis do_sensitivity_analysis()
#' @param result_dsa_control  result from deterministic sensitivity analysis
#' for first or control model
#' @param plotfor the variable to plotfor e.g. cost, utility NMB etc
#' @param type type of analysis, range or difference
#' @param result_dsa_treat result from deterministic sensitivity analysis
#' for the comparative Markov model
#' @param threshold threshold value of WTP
#' @param comparator the strategy to be compared with
#' @return plot of  sensitivity analysis
#' @examples
#' \donttest{
#' param_list <- define_parameters(
#' cost_zido = 2278, cost_direct_med_A = 1701,
#' cost_comm_care_A = 1055, cost_direct_med_B = 1774,
#' cost_comm_care_B = 1278,
#' cost_direct_med_C = 6948, cost_comm_care_C = 2059,
#' tpAtoA = 1251 / (1251 + 483),
#' tpAtoB = 350 / (350 + 1384), tpAtoC = 116 / (116 + 1618),
#' tpAtoD = 17 / (17 + 1717),
#' tpBtoB = 731 / (731 + 527), tpBtoC = 512 / (512 + 746),
#' tpBtoD = 15 / (15 + 1243),
#' tpCtoC = 1312 / (1312 + 437), tpCtoD = 437 / (437 + 1312),
#' tpDtoD = 1,
#' cost_health_A = "cost_direct_med_A +  cost_comm_care_A",
#' cost_health_B = "cost_direct_med_B +  cost_comm_care_B",
#' cost_health_C = "cost_direct_med_C +  cost_comm_care_C",
#' cost_drug = "cost_zido")
#' low_values <- define_parameters(cost_direct_med_B = 177.4,
#' cost_comm_care_C = 205.9)
#' upp_values <- define_parameters(cost_direct_med_B = 17740,
#' cost_comm_care_C = 20590)
#' A <- health_state("A", cost = "cost_health_A +  cost_drug ",
#' utility = 1)
#' B <- health_state("B", cost = "cost_health_B + cost_drug",
#' utility = 1)
#' C <- health_state("C", cost = "cost_health_C + cost_drug",
#' utility = 1)
#' D <- health_state("D", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3, 4), c(NA, 5, 6, 7), c(NA, NA, 8, 9),
#' c(NA, NA, NA, 10))
#' colnames(tmat) <- rownames(tmat) <- c("A", "B", "C", "D")
#' tm <- populate_transition_matrix(4, tmat, c("tpAtoA", "tpAtoB", "tpAtoC",
#' "tpAtoD","tpBtoB", "tpBtoC", "tpBtoD", "tpCtoC", "tpCtoD", "tpDtoD"),
#' colnames(tmat))
#' health_states <- combine_state(A, B, C, D)
#' mono_strategy <- strategy(tm, health_states, "mono")
#' mono_markov <- markov_model(mono_strategy, 20, c(1, 0, 0, 0),
#' discount = c(0.06, 0), param_list)
#' param_table <- define_parameters_sens_anal(param_list, low_values,
#' upp_values)
#' result <- do_sensitivity_analysis(mono_markov, param_table)
#' param_list_treat <- define_parameters(
#' cost_zido = 3000, cost_direct_med_A = 890,
#' cost_comm_care_A = 8976, cost_direct_med_B = 2345,
#' cost_comm_care_B = 1278,
#' cost_direct_med_C = 6948, cost_comm_care_C = 2059,
#' tpAtoA = 1251 / (1251 + 483),
#' tpAtoB = 350 / (350 + 1384), tpAtoC = 116 / (116 + 1618),
#' tpAtoD = 17 / (17 + 1717),
#' tpBtoB = 731 / (731 + 527), tpBtoC = 512 / (512 + 746),
#' tpBtoD = 15 / (15 + 1243),
#' tpCtoC = 1312 / (1312 + 437), tpCtoD = 437 / (437 + 1312),
#' tpDtoD = 1,
#' cost_health_A = "cost_direct_med_A +  cost_comm_care_A",
#' cost_health_B = "cost_direct_med_B +  cost_comm_care_B",
#' cost_health_C = "cost_direct_med_C +  cost_comm_care_C",
#' cost_drug = "cost_zido")
#' treat_strategy <- strategy(tm, health_states, "treat")
#' treat_markov <- markov_model(treat_strategy, 20, c(1, 0, 0, 0),
#' discount = c(0.06, 0), param_list_treat)
#' treat_low_values <- define_parameters(cost_direct_med_B = 234.5,
#' cost_comm_care_C = 694.8)
#' treat_upp_values <- define_parameters(cost_direct_med_B = 23450,
#'  cost_comm_care_C = 69480)
#' param_table_treat <- define_parameters_sens_anal(param_list_treat,
#' treat_low_values,treat_upp_values)
#' result_treat <- do_sensitivity_analysis(treat_markov, param_table)
#' plot_dsa(result,"NMB","range",result_treat, 20000, "treat")
#' }
#' @export
plot_dsa <- function(result_dsa_control, plotfor, type = "range",
                     result_dsa_treat = NULL,
                     threshold = NULL, comparator = NULL) {
  info <- checks_plot_dsa(result_dsa_control, plotfor, type, result_dsa_treat,
                  threshold, comparator)
  plot_variable <- info$plot_variable
  plot_var <- info$plot_var
  ob_results <- keep_results_plot_dsa(result_dsa_control, plotfor,
                                      result_dsa_treat,
                          plot_variable, threshold, comparator)
  if (type == "range") {
    if (plotfor == "ICER") {
        p <- plot_dsa_icer_range(ob_results, plot_var)
    }else{
      if (plotfor == "NMB") {
           p <- plot_dsa_nmb_range(ob_results, plot_var)
      }
      else{
         p <- plot_dsa_others_range(ob_results, plot_var)
      }
    }
  }else{
    if (type == "difference") {
          p <- plot_dsa_difference(ob_results, plotfor, plot_var)
    }
  }
  print(p)
}
#######################################################################
#' Function to do some checks before plotting sensitivity analysis results
#' @param result_dsa_control  result from deterministic sensitivity analysis
#' for first or control model
#' @param plotfor the variable to plotfor e.g. cost, utility NMB etc
#' @param type type of analysis, range or difference
#' @param result_dsa_treat result from deterministic sensitivity analysis
#' for the comparative Markov model
#' @param threshold threshold value of WTP
#' @param comparator the strategy to be compared with
#' @return the plot variable
checks_plot_dsa <- function(result_dsa_control, plotfor, type, result_dsa_treat,
                            threshold, comparator) {
  check_list <- list(type, result_dsa_control, plotfor)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Error - some parameters are NULL or NA")

  if (type != "range" && type != "difference") {
    stop("Error -type should be either range or difference")
  }
  if (type == "difference" && is.null(result_dsa_treat)) {
    stop("Error - if type is difference, result_dsa_treat should not be NULL")
  }
  if (plotfor == "ICER" && is.null(result_dsa_treat)) {
    stop("Error - if plotting for ICER, result_dsa_treat should not be NULL")
  }
  if (plotfor == "ICER" && is.null(threshold)) {
    stop("Error - if plotting for ICER, threshold should not be NULL")
  }
  if (plotfor == "ICER" && is.null(comparator)) {
    stop("Error - if plotting for ICER, comparator should not be NULL")
  }
  if (plotfor == "NMB" && is.null(threshold)) {
    stop("Error - if plotting for NMB, threshold should not be NULL")
  }
  if (plotfor == "ICER" && type == "difference") {
    stop("Error - if plotting for ICER, type should be range")
  }
  if (toupper(plotfor) == "COST") {
    plot_variable <- "cost_matrix"
    plot_var <- "Cost"
  } else {
    if (toupper(plotfor) == "UTILITY") {
      plot_var <- "Utility"
      plot_variable <- "utility_matrix"
    } else {
      if (toupper(plotfor) == "NMB") {
        plot_var <- "NMB"
        plot_variable <- "NMB"
      } else {
        if (toupper(plotfor) == "ICER") {
          plot_var <- "ICER"
          plot_variable <- "ICER"
        } else {
          plot_variable <- "param_matrix"
          plot_var <- plotfor
        }
      }
    }
  }
  plot_info <- list(plot_variable = plot_variable, plot_var = plot_var)
  return(plot_info)
}
#######################################################################
#' Function to do some checks before plotting sensitivity analysis results
#' @param result_dsa_control  result from deterministic sensitivity analysis
#' for first or control model
#' @param plotfor the variable to plotfor e.g. cost, utility NMB etc
#' @param result_dsa_treat result from deterministic sensitivity analysis
#' for the comparative Markov model
#' @param  plot_variable variable for plotting
#' @param threshold threshold value of WTP
#' @param comparator the strategy to be compared with
#' @return results to plot dsa
#' @export
keep_results_plot_dsa <- function(result_dsa_control, plotfor,
                                  result_dsa_treat,
                                  plot_variable,
                                  threshold, comparator) {
  check_list <- list(result_dsa_control, plotfor, plot_variable)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Error - some parameters are NULL or NA")

  results <- data.frame()
  results_treat <- data.frame()
  res_icer_nmb <- data.frame()
  names <- list()
  len <- length(result_dsa_control)
  for (i in seq(1, len, 3)) {
    this_var_result_name <- names(result_dsa_control)[i]
    low_var_result_name <- names(result_dsa_control)[i + 1]
    upp_var_result_name <- names(result_dsa_control)[i + 2]

    index <- stringr::str_locate(this_var_result_name, "result_")
    this_var_name <- substr(this_var_result_name, index[2] + 1,
                            nchar(this_var_result_name))
    if (plotfor == "ICER" | plotfor == "NMB") {
      if (!is.null(result_dsa_treat)) {
        list_markov <-
          combine_markov(result_dsa_control[[this_var_result_name]],
                                  result_dsa_treat[[this_var_result_name]])
        list_markov_low <-
          combine_markov(result_dsa_control[[low_var_result_name]],
                                  result_dsa_treat[[low_var_result_name]])
        list_markov_upp <-
          combine_markov(result_dsa_control[[upp_var_result_name]],
                                  result_dsa_treat[[upp_var_result_name]])

      }else{
        list_markov <-
          combine_markov(result_dsa_control[[this_var_result_name]])
        list_markov_low <-
          combine_markov(result_dsa_control[[low_var_result_name]])
        list_markov_upp <-
          combine_markov(result_dsa_control[[upp_var_result_name]])
      }
      results_icer_nmb_base <- as.data.frame(calculate_icer_nmb(list_markov,
                                                threshold, comparator))
      results_icer_nmb_base[["parameter"]] <- rep(this_var_name,
                                                  nrow(results_icer_nmb_base))
      results_icer_nmb_base[["value_limit"]] <-
        rep("base", nrow(results_icer_nmb_base))
      res_icer_nmb <-
        rbind(res_icer_nmb, results_icer_nmb_base)
      results_icer_nmb_low <-
        as.data.frame(calculate_icer_nmb(list_markov_low, threshold,
                                         comparator))
      results_icer_nmb_low[["parameter"]] <- rep(this_var_name,
                                                nrow(results_icer_nmb_low))
      results_icer_nmb_low[["value_limit"]] <- rep("lower",
                                                   nrow(results_icer_nmb_low))
      res_icer_nmb <- rbind(res_icer_nmb, results_icer_nmb_low)
      results_icer_nmb_upp <-
        as.data.frame(calculate_icer_nmb(list_markov_upp, threshold,
                                         comparator))
      results_icer_nmb_upp[["parameter"]] <- rep(this_var_name,
                                                 nrow(results_icer_nmb_upp))
      results_icer_nmb_upp[["value_limit"]] <- rep("upper",
                                                   nrow(results_icer_nmb_upp))
      res_icer_nmb <- rbind(res_icer_nmb, results_icer_nmb_upp)
      names <- append(names, this_var_name)

    } else {
      this_var_result <- result_dsa_control[[this_var_result_name]]
      matr <- this_var_result[[plot_variable]]
      if (plot_variable == "param_matrix") {
        ind <- match(plotfor, colnames(matr))
        if (is.na(ind)) {
          stop("error - Parameter doesnt exist")
        } else {
          this_col <- ind
        }
      } else {
        this_col <- ncol(matr)
      }
      mean_var <- matr[nrow(matr), this_col]
      low_var_result <- result_dsa_control[[low_var_result_name]]
      matr_low <- low_var_result[[plot_variable]]
      low_var <- matr_low[nrow(matr_low), this_col]
      upp_var_result <- result_dsa_control[[upp_var_result_name]]
      matr_upp <- upp_var_result[[plot_variable]]
      upp_var <- matr_upp[nrow(matr_upp), this_col]
      results <- rbind(results, c(mean_var, low_var, upp_var))
      names <- append(names, this_var_name)
      if (!is.null(result_dsa_treat)) {
        this_var_result <- result_dsa_treat[[this_var_result_name]]
        matr <- this_var_result[[plot_variable]]
        if (plot_variable == "param_matrix") {
          ind <- match(plotfor, colnames(matr))
          if (is.na(ind)) {
            stop("error - Parameter doesnt exist")
          } else {
            this_col <- ind
          }
        } else {
          this_col <- ncol(matr)
        }
        mean_var <- matr[nrow(matr), this_col]
        low_var_result_name <- names(result_dsa_treat)[i + 1]
        low_var_result <- result_dsa_treat[[low_var_result_name]]
        matr_low <- low_var_result[[plot_variable]]
        low_var <- matr_low[nrow(matr_low), this_col]
        upp_var_result_name <- names(result_dsa_treat)[i + 2]
        upp_var_result <- result_dsa_treat[[upp_var_result_name]]
        matr_upp <- upp_var_result[[plot_variable]]
        upp_var <- matr_upp[nrow(matr_upp), this_col]
        results_treat <- rbind(results_treat, c(mean_var, low_var, upp_var))
      }
    }

  }

  if (plotfor == "ICER" | plotfor == "NMB") {
    results <-  res_icer_nmb
    return(list(results_icer_nmb = res_icer_nmb))
  }else{
    results[["parameter"]] <- unlist(names)
    colnames(results) <- c("base", "lower", "upper", "parameter")
    if (!is.null(result_dsa_treat)) {
      results_treat[["parameter"]] <- unlist(names)
      colnames(results_treat) <- c("base", "lower", "upper", "parameter")
      return(list(results = results, results_treat = results_treat))
    } else{
      return(list(results = results))
    }
  }
}
#######################################################################
#' Function to do some checks before plotting sensitivity analysis results
#' @param ob_results  results from deterministic sensitivity analysis
#' @param plot_var the variable
#' @return plot
#' @export
plot_dsa_icer_range <- function(ob_results, plot_var) {
  check_list <- list(ob_results, plot_var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Error - some parameters are NULL or NA")

  results_icer_nmb <- ob_results$results_icer_nmb
  res_icer_nmb_all <- data.frame(results_icer_nmb)
  low <-
    as.numeric(as.character(res_icer_nmb_all[res_icer_nmb_all$value_limit
                                                      == "lower", ]$ICER))
  base <-
    as.numeric(as.character(res_icer_nmb_all[res_icer_nmb_all$value_limit
                                                       == "base", ]$ICER))
  upp <-
    as.numeric(as.character(res_icer_nmb_all[res_icer_nmb_all$value_limit
                                                      == "upper", ]$ICER))
  low <- low[which(!is.na(low))]
  low <- low[which(!is.na(low))]
  base <- base[which(!is.na(base))]
  upp <- upp[which(!is.na(upp))]
  name_file_plot <- paste0("Deterministic sensitivity analysis ICER.pdf",
                           sep = "")
  grDevices::pdf(name_file_plot)

  p <- ggplot2::ggplot(res_icer_nmb_all[res_icer_nmb_all$value_limit ==
                                              "lower"
                                 & !is.na(res_icer_nmb_all$ICER), ]) +
    ggplot2::geom_segment(ggplot2::aes(
      x = low, xend = upp,
      y = unique(res_icer_nmb_all$parameter),
      yend = unique(res_icer_nmb_all$parameter)
    ),
    size = 3, color = "orange"
    ) +
    ggplot2::geom_point(ggplot2::aes(
      x = base,
      y = unique(res_icer_nmb_all$parameter),
      color = "base value"
    ), size = 4) +
    ggplot2::geom_point(ggplot2::aes(
      x = low,
      y = unique(res_icer_nmb_all$parameter),
      color = "lower"
    ), size = 4, shape = 15) +
    ggplot2::geom_point(ggplot2::aes(
      x = upp,
      y = unique(res_icer_nmb_all$parameter),
      color = "upper"
    ), size = 4, shape = 15) +
    ggplot2::labs(colour = "", y = "Parameters") +
    ggplot2::labs(x = paste("Range in ", plot_var, sep = "")) +
    ggplot2::theme(legend.position = "bottom")

  graphics::plot(p) # plot result
  grDevices::dev.off()
  return(p)
}
#######################################################################
#' Function to do some checks before plotting sensitivity analysis results
#' @param ob_results  results from deterministic sensitivity analysis
#' @param plot_var the variable
#' @return plot
#' @export
plot_dsa_nmb_range <- function(ob_results, plot_var) {
  check_list <- list(ob_results, plot_var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Error - some parameters are NULL or NA")

  results_icer_nmb <- ob_results$results
  strategy_names <- unique(results_icer_nmb$Strategy)
  parameters <- unique(results_icer_nmb$parameter)
  results <- results_icer_nmb[results_icer_nmb$Strategy
                              == strategy_names[1], ]
  results_parameters <- as.data.frame(parameters)
  results_parameters[["lower"]] <- as.numeric(results[results$value_limit
                                                      == "lower", ]$NMB)
  results_parameters[["base"]] <- as.numeric(results[results$value_limit
                                                     == "base", ]$NMB)
  results_parameters[["upper"]] <- as.numeric(results[results$value_limit
                                                      == "upper", ]$NMB)

  if (length(unique(results_icer_nmb$Strategy)) == 2) {
    results_treat <- results_icer_nmb[results_icer_nmb$Strategy
                                   == strategy_names[2], ]

    results_parameters_treat <- as.data.frame(parameters)
    results_parameters_treat[["lower"]] <-
      as.numeric(results_treat[results_treat$value_limit == "lower", ]$NMB)
    results_parameters_treat[["base"]] <-
      as.numeric(results_treat[results_treat$value_limit == "base", ]$NMB)
    results_parameters_treat[["upper"]] <-
      as.numeric(results_treat[results_treat$value_limit == "upper", ]$NMB)
  }
  name_file_plot <- paste0("Deterministic sensitivity analysis NMB.pdf",
                           sep = "")
  grDevices::pdf(name_file_plot)
  p <- ggplot2::ggplot(results_parameters) +
    ggplot2::geom_segment(ggplot2::aes_(
      x = ~lower,
      xend = ~upper,
      y = ~parameters,
      yend = ~parameters
    ),
    size = 3, color = "orange"
    ) +
    ggplot2::geom_point(ggplot2::aes_(
      x = ~base,
      y = ~parameters, color = "base value"
    ),
    size = 4
    ) +
    ggplot2::geom_point(ggplot2::aes_(
      x = ~lower,
      y = ~parameters,
      color = "lower"
    ), size = 4, shape = 15) +
    ggplot2::geom_point(ggplot2::aes_(
      x = ~upper,
      y = ~parameters,
      color = "upper"
    ), size = 4, shape = 15) +
    ggplot2::labs(colour = "", y = "Parameters") +
    ggplot2::labs(x = paste("Range in ", plot_var, sep = "")) +
    ggplot2::theme(legend.position = "bottom")
  graphics::plot(p) # plot result
  grDevices::dev.off()

  return(p)
}
#######################################################################
#' Function to do some checks before plotting sensitivity analysis results
#' @param ob_results  results from deterministic sensitivity analysis
#' @param plot_var the variable
#' @return plot
#' @export
plot_dsa_others_range <- function(ob_results, plot_var) {
  check_list <- list(ob_results, plot_var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Error - some parameters are NULL or NA")

  results_parameters <- ob_results$results
  name_file_plot <- paste0("Deterministic sensitivity analysis.pdf", sep = "")
  grDevices::pdf(name_file_plot)

  p <- ggplot2::ggplot(results_parameters) +
    ggplot2::geom_segment(ggplot2::aes_(
      x = ~lower,
      xend = ~upper,
      y = ~parameter,
      yend = ~parameter
    ),
    size = 3, color = "orange"
    ) +
    ggplot2::geom_point(ggplot2::aes_(
      x = ~base,
      y = ~parameter, color = "base value"
    ),
    size = 4
    ) +
    ggplot2::geom_point(ggplot2::aes_(
      x = ~lower,
      y = ~parameter,
      color = "lower"
    ), size = 4, shape = 15) +
    ggplot2::geom_point(ggplot2::aes_(
      x = ~upper,
      y = ~parameter,
      color = "upper"
    ), size = 4, shape = 15) +
    ggplot2::labs(colour = "", y = "Parameters") +
    ggplot2::labs(x = paste("Range in ", plot_var, sep = "")) +
    ggplot2::theme(legend.position = "bottom")
  graphics::plot(p) # plot result
  grDevices::dev.off()
  return(p)
}
#######################################################################
#' Function to do some checks before plotting sensitivity analysis results
#' @param ob_results  results from deterministic sensitivity analysis
#' @param plot_var the variable
#' @param plotfor the quantity plotting
#' @return plot
#' @export
plot_dsa_difference <- function(ob_results, plotfor, plot_var) {
  check_list <- list(ob_results, plotfor, plot_var)
  checks <- sapply(check_list, check_null_na)
  if (sum(checks) != 0) stop("Error - some parameters are NULL or NA")
  if (plotfor == "NMB") {
    results_icer_nmb <- ob_results$results_icer_nmb
    strategy_names <- unique(results_icer_nmb$Strategy)
    parameter <- unique(results_icer_nmb$parameter)
    results <- results_icer_nmb[results_icer_nmb$Strategy
                                == strategy_names[1], ]
    results_parameters <- as.data.frame(parameter)
    results_parameters[["lower"]] <- as.numeric(results[results$value_limit
                                                        == "lower", ]$NMB)
    results_parameters[["base"]] <- as.numeric(results[results$value_limit
                                                       == "base", ]$NMB)
    results_parameters[["upper"]] <- as.numeric(results[results$value_limit
                                                        == "upper", ]$NMB)

      results_treat <- results_icer_nmb[results_icer_nmb$Strategy
                                        == strategy_names[2], ]

      results_parameters_treat <- as.data.frame(parameter)
      results_parameters_treat[["lower"]] <-
        as.numeric(results_treat[results_treat$value_limit == "lower", ]$NMB)
      results_parameters_treat[["base"]] <-
        as.numeric(results_treat[results_treat$value_limit == "base", ]$NMB)
      results_parameters_treat[["upper"]] <-
        as.numeric(results_treat[results_treat$value_limit == "upper", ]$NMB)

  } else {
    results <- ob_results$results
    results_treat <- ob_results$results_treat
    results_parameters <- as.data.frame(results)
    results_parameters_treat <- as.data.frame(results_treat)
  }
  low_diff <- results_parameters_treat$lower - results_parameters$lower
  base_diff <- results_parameters_treat$base - results_parameters$base
  upp_diff <- results_parameters_treat$upper - results_parameters$upper
  name_file_plot <- paste0("Deterministic sensitivity analysis (diff).pdf",
                           sep = "")
  grDevices::pdf(name_file_plot)

  p <- ggplot2::ggplot(results_parameters_treat) +
    ggplot2::geom_segment(ggplot2::aes_(
      x = low_diff, xend = upp_diff,
      y = ~parameter,
      yend = ~parameter
    ),
    size = 3, color = "orange"
    ) +
    ggplot2::geom_point(ggplot2::aes_(
      x = base_diff,
      y = ~parameter,
      color = "base value"
    ), size = 4) +
    ggplot2::geom_point(ggplot2::aes_(
      x = low_diff,
      y = ~parameter,
      color = "lower"
    ), size = 4, shape = 15) +
    ggplot2::geom_point(ggplot2::aes_(
      x = upp_diff,
      y = ~parameter,
      color = "upper"
    ), size = 4, shape = 15) +
    ggplot2::labs(colour = "", y = "Parameters") +
    ggplot2::labs(x = paste("Difference in ", plot_var, sep = "")) +
    ggplot2::theme(legend.position = "bottom")
  graphics::plot(p) # plot result
  grDevices::dev.off()
  return(p)
}
