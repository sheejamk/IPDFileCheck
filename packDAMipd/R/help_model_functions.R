#######################################################################
#' Function to return a list of parameters given
#' @param ... any parameters set of name value pairs expected
#' @return a list of parameters
#' @examples
#' define_parameters(rr = 1)
#' @export
#' @details
#' To return a list of parameters
#' For using with assign_parameters() just list or enumerate the
#' parameters, do not use c() or list() to create a data type list
define_parameters <- function(...) {
  param_list <- list(...)
  return(param_list)
}

#######################################################################
#' Function to assign the values of nested parameters from the parameter list
#' @param param_list list of parameters, can be nested,
#' or can be used the list returned from define_parameters()
#' @return list of assigned parameters
#' @examples
#' param_list <- define_parameters(
#'   cost_direct_med_A = 1701, cost_comm_care_A = 1055,
#'   cost_direct_med_B = 1774, cost_comm_care_B = 1278,
#'   cost_direct_med_C = 6948,
#'   cost_comm_care_C = 2059, cost_zido = 2456, cost_health_A =
#'   "cost_direct_med_A + cost_comm_care_A",
#'   cost_health_B = "cost_direct_med_B + cost_comm_care_B",
#'   cost_health_C = "cost_direct_med_C + cost_comm_care_C",
#'   cost_drug = "cost_zido"
#' )
#' assign_parameters(param_list)
#' @export
#' @details
#' The parameter list should be a list of parameters in the form
#' name value pairs. If the name value pairs is given as a string it throws
#' error as in assign_parameters(c("cost_A = 100", "a = 10")) even if you
#' use assign_parameters(define_parameters(c("cost_A = 100","a = 10")))
#' but this will be ok if you use the below forms
#' assign_list2 <- c(a = 10, cost_A = "a + 100", cost_B = 10)
#' assign_parameters(assign_list2) OR
#' param_list <- define_parameters(a = 10, cost_A = "a + 100", cost_B = 10)
#' assign_list <- assign_parameters(param_list)
#' Also for nested parameters, remember to give the parameters in order so
#' that at run time, the parameters can be evaluated
#' for example, assign_list = define_parameters(cost_A="a+100", a=10)
#' assign_parameters(assign_list) will throw an error, while
#' assign_list = define_parameters( a = 10, cost_A = "a + 100")
#' assign_parameters(assign_list) will successfully assign parameters
#' as the parameters 'a' is visible before the calculation of 'cost_A'
#' Another thing to note is that while using define_parameters, just enumerate
#' them, no need to create as a list by using c() or list function
assign_parameters <- function(param_list) {
  if (is.null(param_list)) {
    stop("Error - list can not be NULL")
  }else{
    if (sum(is.na(param_list)) != 0) stop("Error - list can not be NA")
  }
  list_len <- length(param_list)
  assigned_list <- list()
  names_assigned_list <- list()
  j <- 1
  while (j <= list_len) {
    this_name <- names(param_list[j])
    this_value <- param_list[[this_name]]
    # if the value of the ith parameter is numeric, assign
    if (is.numeric(this_value)) {
      assign(names(param_list[j]), this_value)
      assigned_list <- append(assigned_list, this_value)
      names_assigned_list <- append(names_assigned_list, names(param_list[j]))
    } else {
      # if the value of the jth parameter is not numeric,
      # convert to string and then evaluate
      if (is.character(this_value)) {
        string_this_value <- toString(this_value)
        assign(names(param_list[j]), eval(parse(text = string_this_value)))
        assigned_list <- append(assigned_list,
                                eval(parse(text = string_this_value)))
        names_assigned_list <- append(names_assigned_list,
                                      names(param_list[j]))
      }
    }
    j <- j + 1
  }
  names(assigned_list) <- names_assigned_list
  return(assigned_list)
}
#######################################################################
#' Function to return parameters with in a expression containing operators
#' @param expr an expression
#' @return parameters in the expression expr
#' @examples
#' find_parameters_btn_operators("a+b")
#' @export
#' @details
#' This function returns the parameters between the operators
#' if the state value or probabilities are defined as expressions, we need
#' to extract the parameters and then assign
#' First the position of all operators are found and then return the parameters
#' separated by those operators
#' This happens only for one level
#' find_parameters_btn_operators("a+b") provides a and b
#' but  for find_parameters_btn_operators("mean(a,b)+b")
#' provides mean(a,b) and b
find_parameters_btn_operators <- function(expr) {
  if (is.null(expr)) {
    stop("Error - expression can not be NULL")
  }else{
    if (is.na(expr)) stop("Error - expression can not be NA")
  }
  trimed_expr <- sub("[[:space:]]", "", expr)
  parsed <- toString(parse(text = trimed_expr))
  characters <- c("\\+", "-", "\\*", "/", "%%", "%/%", "\\^", "<", ">", "<=",
                  ">=", "==", "!=", "&", "\\|", "!")
  # Find all indices of characters in the expression
  posi <- sort(unlist(stringr::str_locate_all(parsed, characters)))
  if (length(posi) == 0) {
    params_noempty_string <- expr
  }else{
    params <- list()
    final <- length(posi) + 1
    i <- 1
    # Find start and end of substring between each operator
    while (i <= final) {
      if (i == 1) {
        start <- 1
      } else {
        start <- posi[i - 1] + 1
      }
      if (i == final) {
        end <- nchar(parsed)
      } else {
        if (i == 1) {
          end <- posi[i + 1] - 1
        } else {
          end <- posi[i] - 1
        }
      }
      params <- append(params, trimws(stringr::str_sub(parsed, start = start,
                                                       end = end)))
      i <- i + 2
    }
    params <- unlist(params)
    params_noempty_string <- params[!params == ""]
  }
  return(params_noempty_string)
}
#######################################################################
#' Checks the input to run the Markov cycles and picks correct method
#' @param current_strategy  strategy object
#' @param initial_state value of states initially
#' @param discount rate of discount for costs and qalys
#' @param method what type of half cycle correction needed
#' @param half_cycle_correction boolean to indicate half cycle correction
#' @param startup_cost cost of states initially
#' @param startup_util utility of states initially if any
#' @param state_cost_only_prevalent boolean parameter to indicate
#' if the costs for state occupancy is only for those in the state excluding
#' those that transitioned new. This is relevant when the transition cost
#' is provided for eg. in a state with dialysis the cost of previous dialysis
#' is different from the newly dialysis cases.Then the
#' state_cost_only_prevalent should be TRUE
#' @param state_util_only_prevalent boolean parameter to indicate if the
#' utilities for state occupancy is only for those in the state excluding
#' those that transitioned new.
#' @return changed method name
#' @examples
#' \donttest{
#' tmat <- rbind(c(1, 2), c(3, 4))
#' colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
#' tm <- populate_transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
#' a <- health_state("Healthy", 1, 1, 0, FALSE)
#' b <- health_state("Dead", 1, 0, 0, TRUE)
#' health_states <- combine_state(a, b)
#' this.strategy <- strategy(tm, health_states, "intervention")
#' checks_markov_pick_method(this.strategy, c(1, 0), c(0, 0),
#' "half cycle correction", TRUE,NULL,NULL)
#' }
#' @export
checks_markov_pick_method <- function(current_strategy, initial_state, discount,
                        method, half_cycle_correction, startup_cost,
                        startup_util,
                        state_cost_only_prevalent, state_util_only_prevalent) {


  health_states <- current_strategy$states
  no_states <- length(health_states)
  changed_method <- toupper(gsub("[^A-Za-z]", "", method))
  if (changed_method == "HCCORRECTION" | changed_method == "HALFCYCLECORRECTION"
      | changed_method == "HALFCYCLECORR" | changed_method == "HALFCYCLE" |
      changed_method == "HALF" | changed_method == "CORRECTION" |
      changed_method == "CYCLECORR" | changed_method == "CYCLECORRECTION") {
    changed_method <- "hc_correction"
  } else {
    if (changed_method == "LIFETABLE") {
      changed_method <- "life_table"
    } else {
      stop("Method should be half cycle correction or life table")
    }
  }
  # check the number of values in discount -should be 2
  if (length(discount) != 2) {
    stop("Please provide the discount rates for both qalys and costs")
  }
  # check the class of current strategy
  if (class(current_strategy) != "strategy") {
    stop("Class is not a strategy")
  }
  # check the length of start up cost should be equal to number of states
  if (!is.null(startup_cost)) {
    if (length(startup_cost) != no_states) {
      stop("Length of startup cost should be equal to that of health states")
    }
  }
  # check the length of start up utility should be equal to number of states
  if (!is.null(startup_util)) {
    if (length(startup_util) != no_states) {
      stop("Length of startup utility should be equal to that of health states")
    }
  }
  # check the length of initial state should be equal to number of states
  if (length(initial_state) != no_states) {
    stop("number of intital values should be equal to number of health states")
  }
  if (changed_method != "hc_correction" & half_cycle_correction == TRUE) {
    stop("Only say yes to half cycle correction if you want to use half
            cycle correction method other than life-table method")
  }
  if (!is.null(current_strategy$transition_cost)) {
    if (state_cost_only_prevalent != FALSE & state_cost_only_prevalent != TRUE)
      stop("The parameter 'state_cost_only_prevalent' should be boolean")
  }
  if (!is.null(current_strategy$transition_util)) {
    if (state_util_only_prevalent != FALSE & state_util_only_prevalent != TRUE)
      stop("The parameter 'state_util_only_prevalent' should be boolean")
  }
  return(changed_method)
}

##############################################################################
#' Function to check the variable null or NA
#' @param variable name of variable or list of variable to check
#' @return -1 or -2 as error, else return 0 as success
#' @examples
#' var = c("a")
#' check_null_na(var)
#' @export
check_null_na <- function(variable) {
  #Error - variable can not be NULL or NA
  if (is.null(variable)) {
    return(-1)
  }else{
    if (sum(is.na(variable)) != 0) return(-2)
    else return(0)
  }
}
