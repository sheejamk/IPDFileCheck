#######################################################################
# 1. Markov model definition

# Markov model consists of health states
# Each health state will have its cost and utility associated with it.
# It will have a name and may be an absorbing state

# A. Definition of health state

#' Definition of health state class or health state constructor
#' @param name  name of the health state
#' @param cost value or expression that represents cost of the health state
#' @param utility value or expression that represents utility
#' of the health state
#' @param state_time time denoting how long in the state
#' @param absorb boolean indicating health state absorbing or not
#' @return value of the state
#' @examples
#' st <- health_state("IT", 100, 0.4, 0, FALSE)
#' st <- health_state("IT", "cost_A", 0.4, 0, FALSE)
#' @export
#' @details
#' Initialising the name, cost, utility and time spent for the health state
#' name is the name of the health state
#' cost/utility can be defined as characters e.g. "cost_A"
#' if they are characters, the value is assigned after parsing the text.
#' state_time is integer and absorb is boolean
health_state <- function(name, cost, utility, state_time = 0, absorb = FALSE) {
  if (is.character(cost))
    cost <- parse(text = cost)
  if (is.numeric(cost))
    cost <- cost
  if (is.character(utility))
    utility <- parse(text = utility)
  if (is.numeric(utility))
    utility <- utility
  if (!is.numeric(state_time))
    stop("state_time has to be numeric")
  dt <- structure(list(
    name = name,
    cost = cost,
    utility = utility,
    state_time = state_time,
    absorb = absorb
  ),
  class = "health_state"
  )
  dt
}
#######################################################################
# A.1 Methods of health state
#' Get the attribute for the health state
#' @param state  object of class health state
#' @param var attribute of the health state
#' @return modified health state
#' @examples
#' get_var_state(health_state("IT", 100, 0.4, 0, FALSE), "cost")
#' @export
#' @details
#' After checking the given state is a health state and given variable
#' is defined in the health state, the value of the variable is returned
get_var_state <- function(state, var) {
  if (class(state) == "health_state") {
    if (sum(attributes(state)$names == var) < 1) {
      stop("Given variable is not a class attribute")
    }
    state[[var]]
  } else {
    stop("class is not a health state")
  }
}
#######################################################################
#' Set the attribute for the health state
#' @param state  object of class health state
#' @param var attribute of the health state
#' @param new_value new value to be assigned
#' @return modified health state
#' @examples
#' set_var_state(health_state("IT", 100, 0.4, 0, FALSE), "cost", 1)
#' @export
#' @details
#' After checking the given state is a health state
#' the value of the variable is set
#' if the value is not numeric, it is being parsed to form an expression
set_var_state <- function(state, var, new_value) {
  if (class(state) == "health_state") {
    if (var %in% names(state)) {
      if (is.numeric(new_value)) {
        state[[var]] <- new_value
      } else {
        expr <- parse(text = new_value)
        state[[var]] <- expr
      }
      state
    }else{
      stop("Given variable is not associated with a name in health
           state object")
    }
  } else {
    stop("class is not a health state")
  }
}
#######################################################################
# A2. Join health states
#' Join health states
#' @param ...  any additional objects
#' @return joined health states
#' @examples
#' a <- health_state("IT", 100, 0.4, 0, FALSE)
#' b <- health_state("PT", 100, 0.4, 0, FALSE)
#' combine_state(a, b)
#' @export
#' @details
#' checking each state is a health state and join them
combine_state <- function(...) {
  .dots <- list(...)
  for (i in seq_len(length(.dots))) {
    if (class(.dots[[i]]) != "health_state") {
      if (typeof(.dots[[i]]) == "list") {
        this <- .dots[[i]]
        for (m in seq_len(length(this))) {
          if (class(this[[m]]) != "health_state")
            stop("Each object should be of class health_state")
        }
      combined <- unlist(.dots, recursive = FALSE)
      }else{
        stop("Each object should be of class health_state or a list")
      }
    }else{
      combined <- .dots
    }
  }
  return(combined)
}
#######################################################################
# A3. Check if values are assigned to attributes of health state
#' Check if the values of health states are provided
#' @param health_states  list of health_state objects
#' @return true or false
#' @examples
#' \donttest{
#' well <- health_state("well", cost = 0, utility = 1)
#' disabled <- health_state("disabled", cost = 100, utility = 1)
#' dead <- health_state("dead", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
#' colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
#' tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1))
#' health_states <- combine_state(well, disabled, dead)
#' check_values_states(health_states)
#' }
#' @export
#' @details
#' This is to check if the values are numeric during  the run time,
#' else to throw an error
check_values_states <- function(health_states) {
  no_states <- length(health_states)
  for (j in 1:no_states) {
    this_state <- health_states[[j]]
    if (class(this_state) != "health_state") {
      stop("Class of object should be a health_state")
    }
    names_length <- length(names(this_state))
    for (i in seq_len(names_length)) {
        this_name <- names(this_state)[i]
        if (this_name %in% c("cost", "utility", "state_time")) {
          if (is.language(this_state[this_name]) ||
            is.na(as.numeric(this_state[this_name]))) {
            return(FALSE)
          }
        }
    }
  }
  return(TRUE)
}
#######################################################################
# A4.  Evaluate and assign values to attributes of health state
#' Attribute values in health states
#' @param health_states  list of health_state objects
#' @param assigned_param name value pairs of parameter values in the
#' probability matrix
#' expected created using function assign_parameters()
#' @return health states with assigned values
#' @examples
#' \donttest{
#' well <- health_state("well", cost = "cost_A + cost_B", utility = 1)
#' disabled <- health_state("disabled", cost = 100, utility = 1)
#' dead <- health_state("dead", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
#' colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
#' tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1))
#' health_states <- combine_state(well, disabled, dead)
#' eval_assign_values_states(health_states, c(cost_A = 100, cost_B = 11))
#' }
#' @export
#' @details
#' Assigning the param is done for the cost and utility
#' if the param is not numeric, check if it can be evaluated at the run time
#' if yes, assign the evaluated numeric value
#' if not get the parameters between operators, and assign the values to
#' each individual parameters and then evaluate. only works for two levels.
#' For the example shown the cost is sum of cost_A and cost_B which will
#' only get added in the call eval_assign_values_states
#' While initialising the state "well" it will be only saved as
#' expression(cost_A + cost_B)
#' assigned_param (a list) can be expected to be created using
#' assign_parameters() the exception is if parameter is directly assigned
#' with no nested calculation and no missing parameters.
#' For example assigned_param = c(cost_a = 10, cost_b=10) will be ok
#' but not assigned_param = c(a=10, cost_A = "a+100", cost_B =10) as it requires
#' a nested calculation then use define_parameters() with assign_parameters()
#' as in param_list <- define_parameters(a = 10, cost_A = "a + 100",
#' cost_B = 10)
#' assign_list <- assign_parameters(param_list)
eval_assign_values_states <- function(health_states, assigned_param) {
  no_states <- length(health_states)
  states <- list()
  for (j in 1:no_states) {
     this_state <- health_states[[j]]
     names_length <- length(names(this_state))
     for (i in seq_len(names_length)) {
       this_name <- names(this_state)[i]
       # Only evaluating and assigning the values for cost and utility
       if (this_name %in% c("cost", "utility")) {
         entry <- this_state[[this_name]]
         # Need to process the entry if it is not numeric
         if (!is.numeric(entry)) {
           string_entry_evalu <- eval(substitute(toString(entry)))
           # Need to process the converted string expression further
           # if it is not numeric
           if (is.na(suppressWarnings(as.numeric(string_entry_evalu)))) {

             # Identify the operators and get the parameters between them
             # in string expression
             all_params_expr <-
               find_parameters_btn_operators(string_entry_evalu)

             for (m in seq_len(length(all_params_expr))) {

               this_par_name <- all_params_expr[[m]]
               if (is.na(suppressWarnings(as.numeric(this_par_name)))) {
                 this_ind <- match(this_par_name, names(assigned_param))
                 if (is.na(this_ind)) {
                   stop("Name of the parameter do not contain in the assigned
                        param list")
                 }
               }
               assign(this_par_name, assigned_param[[this_ind]])
             }
             # Once assigned we can evaluate the original entry
             if (!is.numeric(eval(parse(text = entry)))) {
               stop("The evaluation didnt turn into a numeric value for the
                    health state no: ", j)
             }
             this_state <- set_var_state(this_state, this_name,
                                         eval(parse(text = entry)))
           } else {
             this_state <- set_var_state(this_state, this_name,
                                         as.numeric(string_entry_evalu))
           }
         } else {
           this_state <- set_var_state(this_state, this_name, entry)
         }
       }
     }
     states[[j]] <- this_state
  }
  return(states)
}
#######################################################################
# B. Definition of transition matrix
# B1.  Define the table for transition
#' Define the table for transition
#' @param tmat transition matrix in the format as in package 'mstate'
#' @return the transition table with the probabilities
#' @examples
#' tmat <- rbind(c(1, 2), c(3, 4))
#' colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
#' define_transition_table(tmat)
#' @export
#' @details
#' Generating a table for transition matrix for efficient understanding and
#' checking
#' The transition matrix in the format as per 'mstate' package is
#' transformed to a table. if tmat is not a square matrix, it gives error
#' else it spells out the transition number, probability name and from state
#'  to state
define_transition_table <- function(tmat) {
  rows <- nrow(tmat)
  cols <- ncol(tmat)
  names <- rownames(tmat)
  trans_table <- data.table::data.table(NULL)
  if (rows == cols) {
    for (i in 1:rows) {
      row_contents <- tmat[i, ]
      trans_no <- row_contents[which(!is.na(row_contents))]
      from <- rep(i, length(trans_no))
      to <- which(!is.na(row_contents))
      probability_names <- paste("prob_", names[from], "_to_",
                                 names[to], sep = "")
      this_table <- data.table::data.table(trans_no, probability_names,
                                           from, names[from], to,
                                           names[which(!is.na(row_contents))])
      names(this_table) <- c("transition number", "probability name", "from",
                             "from state", "to",
                             "to state")
      l <- list(trans_table, this_table)
      trans_table <- data.table::rbindlist(l)
    }
    return(trans_table)
  } else {
    stop("Given matrix is not a square matrix")
  }
}
#######################################################################
# B2.  Finally populate transition matrix
#' Populate transition matrix
#' @param no_states  number of the health states
#' @param list_prob list of probabilities as in the order of
#' transitions (row wise)
#' @param tmat A transition matrix in the format from the package 'mstate'
#' @param name_states names of the health states
#' @return value of the transition matrix
#' @examples
#' tmat <- rbind(c(1, 2), c(3, 4))
#' colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
#' populate_transition_matrix(2, tmat, list_prob = c(0.2, 0.5, 0, 0.3))
#' @export
#' @details
#' If the state names are null, they are replaced with numbers starting from 1
#' First find those missing probabilities, and fill a list from the given
#' list of probabilities and fill those are not NA in the matrix
#' Note that the probabilities need not be numeric here and no checks are
#'  needed for sum
populate_transition_matrix <- function(no_states, tmat, list_prob,
                                       name_states = NULL) {
  if (!is.numeric(no_states))
    stop("no of states should be numeric")
  if (is.null(name_states)) {
    names <- seq(1:no_states)
  } else {
    names <- name_states
  }
  nonmissing <- no_states * no_states - length(which(is.na(tmat)))
  tmat_list <- as.vector(tmat)
  # only given the non NA probabilities
  if (length(list_prob) == nonmissing) {
    for (i in seq_len(length(list_prob))) {
      index <- which(tmat_list == i)
      tmat_list[index] <- list_prob[i]
    }
  } else {
    stop("Length of probabilities not same as the number of required
         probabilities")
  }
  print("The transition matrix as explained")
  print(define_transition_table(tmat))
  index_na <- which(is.na(tmat_list))
  tmat_list[index_na] <- 0
  mat <- matrix(tmat_list, ncol = no_states)
  colnames(mat) <- names
  rownames(mat) <- names
  value <- list(trans_matrix = mat, name_states = names, no_states = no_states)
  attr(value, "class") <- "transition_matrix"
  value
}
#######################################################################
# B3.  Attribute parameters to probabilities of transition matrix
#' Attribute parameters to probabilities of transition matrix
#' @param tm A transition matrix in the format from the package 'mstate'
#' @param parameter_values name value pairs of parameter values in the
#' probability matrix
#' @return the transition table with the probabilities
#' @examples
#' tmat <- rbind(c(1, 2), c(3, 4))
#' colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
#' tmat <- populate_transition_matrix(2, tmat,
#' list_prob = c("p1", "p2", "p3", "p4"))
#' tmat_assigned <- eval_assign_trans_prob(tmat,
#' c(p1 = 0.2, p2 = 0.3, p3 = 0.4, p4 = 0.5))
#' @export
#' @details
#' Once the transition matrix is populated, the probabilities in
#' transition matrix gets evaluated and assigned in this function call
#' If the entry in transition matrix is NA, replaces it with zero
#' similarly to evaluate and assign health states, the parameter values
#' is excepted to be a list from assign_parameter() and define_parameter().
#' The exception is that if the parameters are defined directly and no nested
#' calculation is required. For eg.
#' assign_list = c(p1 = 0.2, p2 = 0.3, p3 = 0.4, p4 = 0.5)
#' prob <- eval_assign_trans_prob(tmat, assign_list) will work
#' For those with nested calculations, this has to be defined as below
#' assign_list<-assign_parameters(define_parameters(p1 = 0.2, p2 = 0.3,
#' p3 = 0.4, p4 = 0.5))
#' prob <- eval_assign_trans_prob(tmat, assign_list)
#' The below will give error
#' assign_list <- c(p1=0.1, p2 = "p1 + 0.2", p3=0, p4=0.3)
#' prob <- eval_assign_trans_prob(tmat, assign_list)
eval_assign_trans_prob <- function(tm, parameter_values) {
  if (!("transition_matrix" %in% class(tm)) &
      !("transition_cost_util" %in% class(tm))) {
    stop("class of matrix should be transition_matrix - use
         populate_transition_matrix")
  }
  # if transition is not allowed replace with zero
  tm$trans_matrix[which(is.na(tm$trans_matrix))] <- 0
  dimen <- dim(tm$trans_matrix)
  for (i in seq_len(dimen[1])) {
    for (j in seq_len(dimen[2])) {
      entry <- tm$trans_matrix[i, j]
      convert <- suppressWarnings(as.numeric(entry))
      if (is.na(convert)) {
          # if entry is character process them else numeric value
          # entry should be there in parameter_values passed else error
          index <- match(entry, names(parameter_values))
          if (is.null(index) || is.na(index)) {
             stop("Can't determine the value of probability - please check
                   the parameter values")
          } else {
             tm$trans_matrix[i, j] <- parameter_values[[index]]
            if (is.na(tm$trans_matrix[i, j])) {
                stop("NA occured, check the parameter values")
            }
          }
      } else {
          tm$trans_matrix[i, j] <- as.numeric(entry)
      }
    }
  }
  tm$trans_matrix <- apply(tm$trans_matrix, 2, as.numeric)
  return(tm)
}
#######################################################################
# B4. Check the transition matrix
#' Check the transition probabilities for numeric values and unity  row sum
#' @param trans_mat  transition matrix
#' @return 0 if they add to 1 else error
#' @examples
#' tmat <- rbind(c(1, 2), c(3, 4))
#' colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
#' tm <- populate_transition_matrix(2, tmat, list_prob = c(0.5, 0.5, 0, 1))
#' check_trans_prob(tm)
#' @export
#' @details
#' checking for rowsum - checks for the class of transition matrix,
#' value of rowsum (to be 1) and numeric values
check_trans_prob <- function(trans_mat) {
  if (!("transition_matrix" %in% class(trans_mat))) {
    stop("class is not a transition matrix")
  }
  trans_mat <- trans_mat$trans_matrix
  if (is.numeric(trans_mat) == TRUE) {
    if (any(rowSums(trans_mat) != 1)) {
      sums <- rowSums(trans_mat)
      diff <- abs(sums - 1)
      if (any(diff < 1e-8 & diff > 0)) {
        index <- which(diff < 1e-8 & diff > 0)
        for (ind in seq_len(length(index))) {
          trans_mat[ind, 1] <- 1 - sum(trans_mat[ind, 2:ncol(trans_mat)])
        }
      } else {
        stop("Transition matrix not valid - row sum not  equal to 1")
      }
    }
    return(0)
  } else {
    stop("Probailities must be numeric")
  }
}
#######################################################################
# B5.  Create the values of cost and utility while transition
#' Create the the values of cost and utility while transition
#' @param no_states  number of the health states
#' @param tmat_cost_util A transition matrix for the cost/utility
#' values in the format from the package 'mstate' use NA to indicate
#' if the value is zero
#' @param list_values list of probabilities as in the order of transitions
#' (row wise)
#' @param name_states names of the health states
#' @return value of the transition matrix
#' @examples
#' tmat_cost <- rbind(c(NA, 1), c(NA, NA))
#' colnames(tmat_cost) <- rownames(tmat_cost) <- c("Healthy", "Dead")
#' transition_cost_util(2, tmat_cost, list_values = c(500))
#' @export
#' @details
#' Similar to transition matrix but for denoting one time change
#' during transitions
transition_cost_util <- function(no_states, tmat_cost_util, list_values,
                                 name_states = NULL) {
  if (!is.numeric(no_states))
    stop("no of states should be numeric")
  if (is.null(name_states)) {
    names <- seq(1:no_states)
  } else {
    names <- name_states
  }
  nonmissing <- no_states * no_states - length(which(is.na(tmat_cost_util)))
  tmat_list <- as.vector(tmat_cost_util)
  # only given the non NA values
  if (length(list_values) == nonmissing) {
    for (i in seq_len(length(tmat_cost_util))) {
      index <- which(tmat_list == i)
      tmat_list[index] <- list_values[i]
    }
  } else {
    stop("Length of values not same as the number of required")
  }
  index_na <- which(is.na(tmat_list))
  tmat_list[index_na] <- 0
  mat <- matrix(tmat_list, ncol = no_states)
  colnames(mat) <- names
  rownames(mat) <- names
  value <- list(trans_matrix = mat, name_states = names, no_states = no_states)
  attr(value, "class") <- "transition_cost_util"
  value
}
#######################################################################
# C. Define the treatment strategy
#' Definition of strategy - or arm
#' @param trans_mat  transition matrix
#' @param states health states
#' @param name name of the strategy
#' @param trans_cost values of costs if these are attached to transitions
#' @param trans_util values of utility if these are attached to transitions
#' @return object strategy
#' @examples
#' tmat <- rbind(c(1, 2), c(3, 4))
#' colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
#' tm <- populate_transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
#' a <- health_state("Healthy", 1, 1, 0, FALSE)
#' b <- health_state("Dead", 1, 0.5, 0, FALSE)
#' states <- combine_state(a, b)
#' strategy(tm, states, "intervention")
#' @export
#' @details
#' Defining strategy keeping all transition matrix, states and names
#' together to use in defining Markov model
strategy <- function(trans_mat, states, name, trans_cost = NULL,
                     trans_util = NULL) {
  if (!("transition_matrix" %in% class(trans_mat))) {
    stop("class is not a transition matrix")
  }
  for (i in length(states)) {
    if (class(states[[i]]) != "health_state") {
      stop("state objects should of class health_state")
    }
  }
  if (!is.null(trans_cost) & class(trans_cost) != "transition_cost_util") {
    stop("Error - transition cost should be of type transition_cost_util with
         same dimentions are transition matrix")
  }
  if (!is.null(trans_util) & class(trans_util) != "transition_cost_util") {
    stop("Error - transition utility should be of type transition_cost_util
         with same dimentions are transition matrix")
  }
  name_strategy <- name
  transition_cost <- trans_cost
  transition_utility <- trans_util
  value <- list(name_strategy = name_strategy, transition_matrix = trans_mat,
                states = states, transition_cost = transition_cost,
                transition_utility = transition_utility)
  attr(value, "class") <- "strategy"
  value
}
#######################################################################
# D. Definition of Markov model

# D1. All zero trace matrix
#' Define an all zero trace matrix
#' @param health_states health states
#' @param cycles no of cycles
#' @return trace matrix -all zero
#' @examples
#' a <- health_state("Healthy", 1, 1, 0, FALSE)
#' b <- health_state("Dead", 1, 0.5, 0, FALSE)
#' health_states <- combine_state(a, b)
#' init_trace(health_states, 10)
#' @export
#' @details
#' Initialise the trace matrix with all zeros
#' trace matrix will be with no_cycles+1 by no_states matrix
init_trace <- function(health_states, cycles) {
  no_states <- length(health_states)
  zeros <- rep(0, (cycles + 1) * no_states)
  trace_matrix <- matrix(zeros, nrow = cycles + 1, ncol = no_states)
  col_names <- list()
  for (i in seq_len(no_states)) {
    col_names <- append(col_names, health_states[[i]]$name)
  }
  colnames(trace_matrix) <- col_names
  rownames(trace_matrix) <- 0:cycles
  return(trace_matrix)
}
#######################################################################
# D2.Markov model and trace
#' Definition of Markov model and trace
#' @param current_strategy  strategy object
#' @param cycles no of cycles
#' @param initial_state value of states initially
#' @param discount rate of discount for costs and qalys
#' @param half_cycle_correction boolean to indicate half cycle correction
#' @param parameter_values parameters for assigning health states and
#' probabilities
#' @param state_cost_only_prevalent boolean parameter to indicate if the costs
#' for state occupancy is only for those in the state excluding those that
#' transitioned new. This is relevant when the transition cost is provided for
#' eg. in a state with dialysis the cost of previous dialysis is different from
#' the newly dialysis cases. Then the state_cost_only_prevalent should be TRUE
#' @param state_util_only_prevalent boolean parameter to indicate if the
#' utilities for state occupancy is only for those in the state excluding
#' those that transitioned new.
#' @param method what type of half cycle correction needed
#' @param startup_cost cost of states initially
#' @param startup_util utility of states initially if any
#' @return Markov trace
#' @examples
#' tmat <- rbind(c(1, 2), c(3, 4))
#' colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
#' tm <- populate_transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
#' a <- health_state("Healthy", 1, 1, 0, FALSE)
#' b <- health_state("Dead", 1, 0, 0, TRUE)
#' health_states <- combine_state(a, b)
#' this.strategy <- strategy(tm, health_states, "intervention")
#' markov_model(this.strategy, 10, c(1, 0))
#' @export
#' @details
#' Use the strategy, cycles, initial state values creating the markov model
#' and trace. As many probabilities /cost/utility value depend on age/time the
#' evaluation and assignment happens during each cycle. At the heart it does
#' a matrix multiplication using the previous row of the trace matrix and the
#' columns of the transition matrix. Also checks for population loss, calculates
#' cumulative costs and qalys (accounts for discounting
#'  and half cycle correction)
markov_model <- function(current_strategy, cycles, initial_state,
                         discount = c(0, 0),
                         parameter_values = NULL, half_cycle_correction = TRUE,
                         state_cost_only_prevalent = FALSE,
                         state_util_only_prevalent = FALSE,
                         method = "half cycle correction",
                         startup_cost = NULL, startup_util = NULL) {

  # Do all checks and picks the method
  changed_method <- checks_markov_pick_method(current_strategy, initial_state,
                                              discount, method,
                                              half_cycle_correction,
                                              startup_cost, startup_util,
                                              state_cost_only_prevalent,
                                              state_util_only_prevalent)
  # initialising matrices
  health_states <- current_strategy$states
  no_states <- length(health_states)
  trans_mat <- current_strategy$transition_matrix
  trace_matrix <- init_trace(health_states, cycles)
  utility_matrix <- init_trace(health_states, cycles)
  cost_matrix <- init_trace(health_states, cycles)
  param_matrix <- matrix(0, nrow = cycles + 1, ncol =
                           length(parameter_values) + 1)
  ending <- cycles + 1
  trace_matrix[1, ] <- initial_state
  # Run for each cycle
  for (i in 1:ending) {
    markov_cycle <- i - 1
    # Get parameter values
    extended_parameter_values <- c(markov_cycle = markov_cycle,
                                   parameter_values)
    assigned_param <- assign_parameters(extended_parameter_values)

    # Get all the value of health states assigned
    health_states_assigned <- eval_assign_values_states(health_states,
                                                        assigned_param)
    param_matrix[i, ] <- unlist(assigned_param)

    # Get all the values of probability assigned
    trans_mat <- eval_assign_trans_prob(current_strategy$transition_matrix,
                                        assigned_param)

    if (check_trans_prob(trans_mat) == 0) {
      for (j in 1:no_states) {
      # at start of of cycle no start up cost (utility), transition
      # cost (utility) and half cycle correction is not to be accounted
      if (i > 1) {
        # total transitions made to jth state including those
        # from the same state
        transitions_made_state <-
          trace_matrix[i - 1, ] * trans_mat$trans_matrix[, j]
        # total transitions made to jth state excluding those
        # from the same state
        new_transitions_to_state <- transitions_made_state
        new_transitions_to_state[j] <- 0

        # tracematrix ith row and jth cycle
        trace_matrix[i, j] <- trace_matrix[i - 1, ] %*%
                                                  trans_mat$trans_matrix[, j]

        #cost calculation if the transition cost is different that state costs
        if (is.null(current_strategy$transition_cost)) {
          cost_occured_due_transitions <- 0
        } else {
          trans_cost <- eval_assign_trans_prob(current_strategy$transition_cost,
                                               assigned_param)
          cost_occured_due_transitions <- transitions_made_state %*%
                                          trans_cost$trans_matrix[, j]
        }

        #utility calculation if the transition utility is different
        #that state utilities
        if (is.null(current_strategy$transition_utility)) {
          utility_due_transitions <- 0
        } else {
          trans_util <-
            eval_assign_trans_prob(current_strategy$transition_utility,
                                               assigned_param)
          utility_due_transitions <- transitions_made_state %*%
                                            trans_util$trans_matrix[, j]
        }
        if (i == 2) {
          if (changed_method != "hc_correction" &
              changed_method == "life_table")
            trace_matrix[i, j] <- (trace_matrix[i, j] +
                                     trace_matrix[i - 1, j]) / 2
        }

        # cost entries in cost matrix
         cost_matrix[i, j] <- trace_matrix[i, j] *
            (as.numeric(unlist(health_states_assigned[[j]]$cost))) +
            cost_occured_due_transitions

        # if the transition cost are only for those newly transitioned,
        # then we need to split it up. Flagged by the presence of
        # transition_cost and state_cost_only_prevalent values
        if (!is.null(current_strategy$transition_cost)) {
          trans_cost_which_state <- which(trans_cost$trans_matrix[, j] != 0)
          if (length(trans_cost_which_state) != 0) {
            if (state_cost_only_prevalent) {
              already_in_state <- trace_matrix[i, j] -
                sum(new_transitions_to_state)
              cost_matrix[i, j]  <-  already_in_state *
                (as.numeric(unlist(health_states_assigned[[j]]$cost))) +
                new_transitions_to_state %*% trans_cost$trans_matrix[, j]
            }
          }
        }
        # cost entries in cost matrix
        utility_matrix[i, j] <- trace_matrix[i, j] *
                    (as.numeric(unlist(health_states_assigned[[j]]$utility))) +
                      utility_due_transitions
        # if the transition cost are only for those newly transitioned,
        # then we need to split it up. Flagged by the presence of
        # transition_cost and state_cost_only_prevalent values
        if (!is.null(current_strategy$transition_util)) {
          trans_cost_which_state <- which(trans_util$trans_matrix[, j] != 0)
          if (length(trans_cost_which_state) != 0) {
            if (state_util_only_prevalent) {
              already_in_state <- trace_matrix[i, j] -
                sum(new_transitions_to_state)
              cost_matrix[i, j]  <-  already_in_state *
                (as.numeric(unlist(health_states_assigned[[j]]$utility))) +
                new_transitions_to_state %*% trans_util$trans_matrix[, j]
            }
          }
        }
      } else {
        if (!is.null(startup_cost)) {
          cost_matrix[1, j] <- trace_matrix[1, j] * (startup_cost[j] +
                    (as.numeric(unlist(health_states_assigned[[j]]$cost))))

        } else {
            cost_matrix[1, j] <- trace_matrix[1, j] *
                    (as.numeric(unlist(health_states_assigned[[j]]$cost)))
        }
        if (!is.null(startup_util)) {
            utility_matrix[1, j] <- trace_matrix[1, j] * (startup_util[j] +
                  as.numeric(unlist(health_states_assigned[[j]]$utility)))
        } else {
            utility_matrix[1, j] <- trace_matrix[1, j] *
                (as.numeric(unlist(health_states_assigned[[j]]$utility)))
         }
      }
    }
    }
    if (sum(trace_matrix[i, ]) - sum(initial_state) > 1e-4)
      stop(paste("Population loss - check at cycle", i, sep = ""))
  }
  nozeros <- rep(0, cycles + 1)
  cost_matrix <- cbind(cost_matrix, nozeros, nozeros)
  utility_matrix <- cbind(utility_matrix, nozeros, nozeros)
  trace_matrix <- cbind(trace_matrix, nozeros)
  # Taking care of discounting
  for (i in 1:ending) {
    cost_matrix[i, no_states + 1] <-
      sum(cost_matrix[i, ]) / ((1 + discount[1]) ^ (i - 1))
    utility_matrix[i, no_states + 1] <-
      sum(utility_matrix[i, ]) / ((1 + discount[2]) ^ (i - 1))
  }
  if (changed_method == "hc_correction" & half_cycle_correction) {
    cost_matrix[1, no_states + 1] <- cost_matrix[1, no_states + 1] * 0.5
    utility_matrix[1, no_states + 1] <- utility_matrix[1, no_states + 1] * 0.5
    cost_matrix[ending, no_states + 1] <-
      cost_matrix[ending, no_states + 1] * 0.5
    utility_matrix[ending, no_states + 1] <-
    utility_matrix[ending, no_states + 1] * 0.5
  }
  # Find the cumulative sums and total values of cost and utility
  cost_matrix[, no_states + 2] <- cumsum(cost_matrix[, no_states + 1])
  utility_matrix[, no_states + 2] <- cumsum(utility_matrix[, no_states + 1])
  names_trace_matrix <- colnames(trace_matrix)
  names_cost_matrix <- colnames(cost_matrix)
  names_utility_matrix <- colnames(utility_matrix)
  names_cost_matrix[no_states + 1] <- "Total discounted cost"
  names_cost_matrix[no_states + 2] <- "Cumulative cost"
  names_utility_matrix[no_states + 1] <- "Total discounted utility"
  names_utility_matrix[no_states + 2] <- "Cumulative utility"
  trace_matrix[, no_states + 1] <- c(0, seq_len(cycles))
  names_trace_matrix[no_states + 1] <- "Cycles"
  colnames(trace_matrix) <- names_trace_matrix
  colnames(cost_matrix) <- names_cost_matrix
  colnames(utility_matrix) <- names_utility_matrix
  colnames(param_matrix) <- c("cycle", names(parameter_values))
  value <- list(
        strategy = current_strategy,
        method = method,
        half_cycle_correction = half_cycle_correction,
        transition_matrix = trans_mat,
        param_matrix = param_matrix,
        list_param_values = parameter_values,
        health_states = health_states,
        cycles = cycles,
        initial_state = initial_state,
        discount = discount,
        trace_matrix = trace_matrix,
        cost_matrix = cost_matrix,
        utility_matrix = utility_matrix,
        startup_cost = startup_cost,
        startup_util = startup_util)
  attr(value, "class") <- "markov_model"
  value
}
#######################################################################
# D3. Join Markov model objects
#' Join Markov model objects
#' @param markov1  object 1 of class markov_model
#' @param ...  any additional objects
#' @return joined objects of type markov_model
#' @examples
#' well <- health_state("well", cost = 0, utility = 1)
#' disabled <- health_state("disabled", cost = 100, utility = 1)
#' dead <- health_state("dead", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
#' colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
#' tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1))
#' health_states <- combine_state(well, disabled, dead)
#' this.strategy <- strategy(tm, health_states, "example")
#' this_markov <- markov_model(this.strategy, 24, c(1000, 0, 0))
#' well <- health_state("well", cost = 0, utility = 1)
#' disabled <- health_state("disabled", cost = 10, utility = 0.5)
#' dead <- health_state("dead", cost = 0, utility = 0)
#' tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
#' colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
#' tm <- populate_transition_matrix(3, tmat, c(0.4, 0.4, 0.2, 0.6, 0.4, 1))
#' health_states <- combine_state(well, disabled, dead)
#' this.strategy <- strategy(tm, health_states, "example")
#' sec_markov <- markov_model(this.strategy, 24, c(1000, 0, 0))
#' list_markov <- combine_markov(this_markov, sec_markov)
#' @export
#' @importFrom methods cbind2
#' @details
#' Combining Markov models for easiness of comparison
combine_markov <- function(markov1, ...) {
  if (class(markov1) == "markov_model") {
    all_markovs <- markov1
  } else {
    if (typeof(markov1) == "list") {
      all_markovs <- c()
      for (i in seq_len(length(markov1))) {
        if (class(markov1[[i]]) != "markov_model")
            stop("class is not a Markov model")
        all_markovs <- methods::cbind2(all_markovs, markov1[[i]])
      }
    }else{
      stop("argument should be a list of markov models or a markov model")
    }
  }
  .dots <- list(...)
  for (i in seq_len(length(.dots))) {
    all_markovs <- methods::cbind2(all_markovs, .dots[[i]])
  }
  combined <- (t(all_markovs))
  return(combined)
}
#######################################################################
#' E1. Plot a Markov model
#' @param markov  markov_model object
#' @return plots
#' @examples
#' \donttest{
#' tmat <- rbind(c(1, 2), c(3, 4))
#' colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
#' tm <- populate_transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
#' a <- health_state("Healthy", 1, 1, 0, FALSE)
#' b <- health_state("Dead", 1, 0, 0, TRUE)
#' health_states <- combine_state(a, b)
#' this.strategy <- strategy(tm, health_states, "intervention")
#' this_markov <- markov_model(this.strategy, 10, c(1, 0), c(0, 0))
#' p <- plot_model(this_markov)
#' }
#' @export
#' @importFrom reshape2 melt
plot_model <- function(markov) {
  if (class(markov) != "markov_model") {
    stop("The object has to be of class markov_model")
  }
  this_trace <- data.frame(markov$trace_matrix)
  this_trace_melted <- reshape2::melt(this_trace, id.var = "Cycles")

  name_file_plot <- paste0("Model_states.pdf", sep = "")
  grDevices::pdf(name_file_plot)
  p <- ggplot2::ggplot(this_trace_melted, ggplot2::aes_(x = ~Cycles,
                  y = ~value, col = ~variable)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Cycles") +
    ggplot2::labs(y = "States") +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  graphics::plot(p)
  grDevices::dev.off()
  return(p)
}
