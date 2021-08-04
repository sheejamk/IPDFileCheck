###############################################################################
context("testing defining a health state")
test_that("testing defining a health state", {
  st <- health_state("IT", 100, 0.4, 0, FALSE)
  expect_identical(class(st), "health_state")
  # checking the attributes of the health state with name "IT"
  expect_identical(st$name, "IT")
  expect_identical(st$cost, 100)
  expect_identical(st$utility, 0.4)
  expect_identical(st$absorb, FALSE)
  # This causes an error as the time is not numeric, but given as "time"
  expect_error(health_state("IT", 100, 0.4, "time", FALSE))
  # This causes an error as the time is not numeric, but given as FALSE,
  # though it might be intention of the user that the state_time will be 0
  # default, but if the variable "absorb" is provided explicitly, the state_time
  # should also be provided
  expect_error(health_state("IT", 100, 0.4, TRUE))
  st <- health_state("IT", "cost_IT", "ut_IT", 0, FALSE)
  expect_true(is.expression(st$cost))
  expect_true(is.expression(st$utility))

})

test_that("testing defining a health state", {
  st <- health_state("IT", "cost_IT", "a+b", 1, FALSE)
  # works fine even if we give cost and utility as expressions
  expect_identical(class(st), "health_state")
  expect_identical(st$name, "IT")
  expect_identical(toString(parse(text = st$cost)), "cost_IT")
  expect_identical(toString(parse(text = st$utility)), "a + b")
  expect_identical(st$absorb, FALSE)
})
##############################################################################
context("testing to get the attribute for the health state")
test_that("testing to get the attribute for the health state", {
  a <- 10
  this_cost <- get_var_state(health_state("IT", 100, 0.4, 0, FALSE), "cost")
  this_util <- get_var_state(health_state("IT", 100, 0.4, 0, FALSE), "utility")
  # get the variable cost as 100 and utility as 0.4
  expect_identical(this_cost, 100)
  expect_identical(this_util, 0.4)
  st <- health_state("IT", 100, 0.4, 0, FALSE)
  # there is no variable called "time"
  expect_error(get_var_state(st, "time"))
  # "a" is not a health state
  expect_error(get_var_state(a, "time"))
  # if you assign NA, then you can retrieve it
  expect_equal(get_var_state(health_state("IT", NA, 0.4, 0, FALSE), "cost"), NA)

})
##############################################################################
context("testing to set the attribute for the health state")
test_that("testing to set the attribute for the health state", {
  a <- 10
  st <- health_state("IT", "cost_IT", "util_IT", 0, FALSE)
  st <- set_var_state(st, "cost", 200)
  # set the cost to be 200 and check
  expect_identical(st$cost, 200)
  # set the cost to be 300 but this time as additive operation and check
  st <- set_var_state(st, "cost", 200 + 100)
  expect_identical(st$cost, 300)
  # "a" is not a health state
  expect_error(set_var_state(a, "cost", 200))
  # "b is not a name in the health state class
  expect_error(set_var_state(st, "b", 200))
  st <- set_var_state(st, "cost", "cost_a")
  expect_true(is.expression(st$cost))
})
##############################################################################
context("testing combining health states")
test_that("testing combining health states", {
  a <- health_state("IT", 100, 0.4, 0, FALSE)
  b <- health_state("PT", 100, 0.4, 0, FALSE)
  all_states <- combine_state(a, b)
  expect_identical(c(all_states[1][[1]]$name, all_states[2][[1]]$name),
                   c("IT", "PT"))
  # c is not a health state
  expect_error(combine_state(c, b))
  all_states <- combine_state(list(a, b))
  expect_identical(c(all_states[1][[1]]$name, all_states[2][[1]]$name),
                   c("IT", "PT"))
  a <- 10
  b <- 10
  expect_error(combine_state(list(a, b)))

})
##############################################################################
context("testing values of states")
test_that("testing values of the variables in states", {
  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 100, utility = 1)
  dead <- health_state("dead", cost = 0, utility = 0)
  health_states <- combine_state(well, disabled, dead)
  # values are numeric
  expect_equal(check_values_states(health_states), TRUE)
  well <- health_state("well", cost = "cost_A", utility = 1)
  health_states <- combine_state(well, disabled, dead)
  # the values are not numeric, it returns false
  expect_equal(check_values_states(health_states), FALSE)
  # the values are numeric, but not health state
  expect_error(check_values_states(1))
})
##############################################################################

context("testing evaluating and assigning values to health states")
test_that("testing evaluating and assigning values to health states", {
  well <- health_state("well", cost = "cost_A", utility = "p2 + 0.1")
  disabled <- health_state("disabled", cost = "100", utility = 1)
  dead <- health_state("dead", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  health_states <- combine_state(well, disabled, dead)
  param_list <- define_parameters(cost_A = 200, p2 = 0.2)
  assign_list <- assign_parameters(param_list)
  states_assigned <- eval_assign_values_states(health_states, assign_list)
  # the values of health states are assigned
  expect_equal(states_assigned[2][[1]]$cost, 100)
  expect_equal(states_assigned[1][[1]]$utility, 0.3)
  param_list <- define_parameters(cost_A = 200)
  assign_list <- assign_parameters(param_list)
  #  parameter missing
  expect_error(eval_assign_values_states(health_states, assign_list))

})
context("testing evaluating and assigning values to health states")
test_that("testing evaluating and assigning values to health states", {
  well <- health_state("well", cost = "cost_A", utility = 1)
  disabled <- health_state("disabled", "cost_B", utility = 1)
  dead <- health_state("dead", cost = "0", utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
  tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1))
  health_states <- combine_state(well, disabled, dead)

  # example1 - direct assignment with no nested calculation
  assign_list1 <- c(cost_A = 100, cost_B = 10)
  states_assigned <- eval_assign_values_states(health_states, assign_list1)
  expect_equal(states_assigned[1][[1]]$cost, 100)

  # example2 - direct assignment with  nested calculation - cant do
  assign_list2 <- c(a = 10, cost_A = "a + 100", cost_B = 10)
  expect_error(eval_assign_values_states(health_states, assign_list2))

  # example3 - use assign parameters and define_parameters with
  # nested calculation
  param_list <- define_parameters(a = 10, cost_A = "a+100", cost_B = 10)
  assign_list <- assign_parameters(param_list)
  states_assigned <- eval_assign_values_states(health_states, assign_list)
  expect_equal(states_assigned[1][[1]]$cost, 110)

  # example4 - direct assignment with  nested calculation but missing
  # a parameter- error
  assign_list2 <- c("a=10", "cost_A= a+100")
  expect_error(eval_assign_values_states(health_states, assign_list2))

  # example5 - direct assignment with  no nested calculation but missing
  # a parameter- error
  assign_list3 <- c("cost_A=100", "a=10")
  expect_error(eval_assign_values_states(health_states, assign_list3))

  # example6 - direct assignment with  no nested calculation but missing
  # a parameter- error
  assign_list4 <- "cost_A=100"
  expect_error(eval_assign_values_states(health_states, assign_list4))

  # example7 - use assign_parameters and define_parameters with  no nested
  # calculation but missing  a parameter- error
  assign_list6 <- assign_parameters(define_parameters(cost_A = 100))
  expect_error(eval_assign_values_states(health_states, assign_list6))
})

# ##############################################################################
context("testing defining a transition table")
test_that("testing defining a transition table", {
  tmat <- rbind(c(1, 2), c(3, 4))
  colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
  tt <- define_transition_table(tmat)
  expect_equal(tt$from, c(1, 1, 2, 2))
  expect_identical(tt$`from state`, c("Healthy", "Healthy", "Dead", "Dead"))
  expect_equal(tt$to, c(1, 2, 1, 2))
  expect_identical(tt$`to state`, c("Healthy", "Dead", "Healthy", "Dead"))
  tmat <- rbind(c(1, 2), c(3, 4), c(4, 5))
  # tmat not a square matrix -error
  expect_error(define_transition_table(tmat))
})
# ##############################################################################
context("testing populating transition matrix")
test_that("testing populating transition matrix", {
  tmat <- rbind(c(1, 2), c(3, 4))
  colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
  # transition matrix with numeric probabilities
  transmat <- populate_transition_matrix(2, tmat,
                                         list_prob = c(0.2, 0.5, 0, 0.3))
  m <- matrix(c(0.2, 0.5, 0, 0.3), nrow = 2, byrow = T)

  expect_equal(transmat$trans_matrix, m, check.attributes = FALSE)
  expect_equal(transmat$name_states, c(1, 2))
  expect_equal(transmat$no_states, 2)

  # transition matrix with non numeric probabilities
  transmat <- populate_transition_matrix(2, tmat,
                                  list_prob = c("p1", "1-p1", "p2", "1-p2"))
  expect_equal(transmat$name_states, c(1, 2))
  expect_equal(transmat$no_states, 2)

  transmat <- populate_transition_matrix(2, tmat,
                                         list_prob = c(0.2, 0.5, 0, 0.3),
                                         name_states = rownames(tmat))
  expect_equal(transmat$name_states, c("Healthy", "Dead"))
  tmat <- rbind(c(1, 2), c(NA, 3))
  colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")

  # length of probabilities required does not match with the number of
  # values provided
  expect_error(populate_transition_matrix(2, tmat,
                                          list_prob = c(0.2, 0.5, 0.5, 0.3)))
  mat <- populate_transition_matrix(2, tmat, list_prob = c(0.2, 0.5, 0.3))
  expect_equal(mat$trans_matrix, m, check.attributes = FALSE)
  #Error - no of states is not numeric
  expect_error(populate_transition_matrix("two", tmat,
                                          list_prob = c(0.2, 0.5, 0.5, 0.3)))
})
# ##############################################################################
context("testing evaluating and assigning values to transition matrix")
test_that("testing evaluating and assigning values to transition matrix", {
  tm <- rbind(c(1, 2), c(3, 4))
  colnames(tm) <- rownames(tm) <- c("Healthy", "Dead")
  tmat <- populate_transition_matrix(2, tm,
                                     list_prob = c("p1", "p2", "p3", "p4"))
  param_list <- define_parameters(p1 = 0.2, p2 = 0.8, p3 = 0.5, p4 = 0.5)
  assigned_list <- assign_parameters(param_list)
  tmat_assigned <- eval_assign_trans_prob(tmat, assigned_list)
  m <- matrix(c(0.2, 0.8, 0.5, 0.5), nrow = 2, byrow = T)

  # Error - tm is not a class of transition_matrix
  expect_error(eval_assign_trans_prob(tm, assigned_list))

  expect_equal(tmat_assigned$trans_matrix, m, check.attributes = FALSE)
  expect_equal(tmat_assigned$name_states, c(1, 2))
  expect_equal(tmat_assigned$no_states, 2)
  tmat <- rbind(c(1, 2), c(3, 4))
  colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
  tmat <- populate_transition_matrix(2, tmat,
                                     list_prob = c("p1", "p2", "p3", "p4"))
  param_list <- define_parameters(p1 = 0.4, a = 0.2, p2 = "a+p1",
                                  p3 = 0, p4 = 1)
  assigned_list <- assign_parameters(param_list)
  tmat_assigned <- eval_assign_trans_prob(tmat, assigned_list)
  m <- matrix(c(0.4, 0.6, 0, 1), nrow = 2, byrow = T)
  expect_equal(tmat_assigned$trans_matrix, m, check.attributes = FALSE)

  tmat <- populate_transition_matrix(2, tm,
                                     list_prob = c("p1", "p2", "p3", "p4"))
  param_list <- define_parameters(p1 = 0.2, p2 = 0.8, p3 = 0.5, p5 = 0.5)
  assigned_list <- assign_parameters(param_list)
  expect_error(eval_assign_trans_prob(tmat, assigned_list))

  tm <- rbind(c(1, 2), c(3, 4))
  colnames(tm) <- rownames(tm) <- c("Healthy", "Dead")
  tmat <- populate_transition_matrix(2, tm,
                                     list_prob = c("p1", "p2", "p3", "p4"))
  param_list <- define_parameters(p1 = 0.2, p2 = 0.8, p3 = 0.5, p4 = NA)
  expect_error(eval_assign_trans_prob(tmat, param_list))

})
# ##############################################################################
context("testing sum of transition probabilites")
test_that("testing sum of transition probabilites", {
  tmat <- rbind(c(1, 2), c(3, 4))
  colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
  tmat <- populate_transition_matrix(2, tmat,
                                     list_prob = c("p1", "p2", "p3", "p4"))
  param_list <- define_parameters(p1 = 0.2, p2 = 0.79999999,
                                  p3 = 0.5, p4 = 0.5)
  assigned_list <- assign_parameters(param_list)
  tmat_assigned <- eval_assign_trans_prob(tmat, assigned_list)
  expect_equal(check_trans_prob(tmat_assigned), 0)

  param_list <- define_parameters(p1 = 0.4, a = 0.2, p2 = "a+p1",
                                  p3 = 0, p4 = 1)
  assigned_list <- assign_parameters(param_list)
  tmat_assigned <- eval_assign_trans_prob(tmat, assigned_list)
  expect_equal(check_trans_prob(tmat_assigned), 0)

  param_list <- define_parameters(p1 = 0.4, a = 0.02, p2 = "a+p1", p3 = 0,
                                  p4 = 1)
  assigned_list <- assign_parameters(param_list)
  tmat_assigned <- eval_assign_trans_prob(tmat, assigned_list)
  # sum is not 1
  expect_error(check_trans_prob(tmat_assigned))
  # probabilities are not numeric
  expect_error(check_trans_prob(tmat))

  # tmat is not of class transition_matrix
  tmat <- rbind(c(1, 2), c(3, 4))
  expect_error(check_trans_prob(tmat))
})
# ##############################################################################
context("testing creating the values of cost and utility during transition")
test_that("testing creating the cost and utility during transition", {
  tmat_cost <- rbind(c(NA, 1), c(NA, NA))
  colnames(tmat_cost) <- rownames(tmat_cost) <- c("Healthy", "Dead")
  trans_cost <- transition_cost_util(2, tmat_cost,
                                     list_values = c(500), c("A", "B"))
  m <- matrix(c(0, 500, 0, 0), ncol = 2, byrow = T)
  expect_equal(trans_cost$trans_matrix, m, check.attributes = FALSE)
  expect_equal(class(trans_cost), "transition_cost_util")
  expect_error(transition_cost_util(2, tmat_cost, list_values = c(500, 200)))
  tmat_cost <- rbind(c(NA, 1), c(2, NA))
  #Error - there should be two values
  expect_error(transition_cost_util(2, tmat_cost, list_values = c(200)))
  # Error -  no of states is not numeric
  expect_error(transition_cost_util("two", tmat_cost,
                                    list_values = c(500, 200)))
})

# ##############################################################################
context("testing creating strategy")
test_that("testing creating strategyl", {
  tmat <- rbind(c(1, 2), c(3, 4))
  colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
  tm <- populate_transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
  a <- health_state("Healthy", 1, 1, 0, FALSE)
  b <- health_state("Dead", 1, 0, 0, TRUE)
  health_states <- combine_state(a, b)
  this_strategy <- strategy(tm, health_states, "intervention")
  expect_equal(class(this_strategy), "strategy")
  mm <- matrix(c(0.5, 0.5, 0, 1), nrow = 2, byrow = T)
  expect_equal(this_strategy$transition_matrix$trans_matrix, mm,
               check.attributes = FALSE)
  expect_equal(this_strategy$transition_matrix$name_states, c(1, 2))
  expect_equal(this_strategy$transition_matrix$no_states, 2)
  expect_equal(this_strategy$name_strategy, "intervention")
  # Error - tmat is not a class of transition_matrix
  expect_error(strategy(tmat, health_states, "intervention"))

  # Error - "hs" is not a health state or a list of health states
  expect_error(strategy(tm, "hs", "intervention"))

  tmat <- rbind(c(1, 2), c(3, 4))
  colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
  tm <- populate_transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))

  tmat_cost <- rbind(c(NA, 1), c(NA, NA))
  colnames(tmat_cost) <- rownames(tmat_cost) <- c("Healthy", "Dead")
  tm_cost <- transition_cost_util(2, tmat_cost, c(10))
  tmat_util <- rbind(c(NA, 1), c(NA, NA))
  colnames(tmat_util) <- rownames(tmat_cost) <- c("Healthy", "Dead")
  tm_util <- transition_cost_util(2, tmat_cost, c(0.8))
  this_strategy <- strategy(tm, health_states, "intervention",
                            tm_cost, tm_util)
  expect_equal(this_strategy$name_strategy, "intervention")
  m <- matrix(c(0, 10, 0, 0), nrow = 2, byrow = T)
  expect_equal(this_strategy$transition_cost$trans_matrix, m,
               check.attributes = FALSE)
  expect_error(strategy(tm, health_states, "intervention", tm, tm_util))
  expect_error(strategy(tm, health_states, "intervention", tm_cost, tm))
})
# ##############################################################################
context("testing creating markov model")
test_that("testing creating markov model", {
  tmat <- rbind(c(1, 2), c(3, 4))
  colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
  tm <- populate_transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
  a <- health_state("Healthy", 1, 1, 0, FALSE)
  b <- health_state("Dead", 1, 0, 0, TRUE)
  health_states <- combine_state(a, b)
  this_strategy <- strategy(tm, health_states, "intervention")
  mm <- markov_model(this_strategy, 10, c(1, 0), c(0, 0))
  # class of mm is markov_model
  expect_equal(class(mm), "markov_model")
  trace_matrix_1 <- mm$trace_matrix
  df <- matrix(unlist(trace_data), nrow = 11)
  expect_equal(trace_matrix_1, df, check.attributes = FALSE, tolerance = 1e-4)


  tmat <- rbind(c(1, 2), c(3, 4))
  colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
  tm <- populate_transition_matrix(2, tmat, c(0.4, 0.5, 0, 1))
  a <- health_state("Healthy", 1, 1, 0, FALSE)
  b <- health_state("Dead", 1, 0, 0, TRUE)
  health_states <- combine_state(a, b)
  this_strategy <- strategy(tm, health_states, "intervention")
  expect_error(markov_model(this_strategy, 10, c(1, 0), c(0, 0)))

  tmat <- rbind(c(1, 2), c(3, 4))
  colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
  tm <- populate_transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
  a <- health_state("Healthy", 1, 1, 0, FALSE)
  b <- health_state("Dead", 1, 0, 0, TRUE)
  health_states <- combine_state(a, b)
  tmat_cost <- rbind(c(NA, 1), c(NA, NA))
  colnames(tmat_cost) <- rownames(tmat_cost) <- c("Healthy", "Dead")
  tm_cost <- transition_cost_util(2, tmat_cost, c(10))
  tmat_util <- rbind(c(NA, 1), c(NA, NA))
  colnames(tmat_util) <- rownames(tmat_cost) <- c("Healthy", "Dead")
  tm_util <- transition_cost_util(2, tmat_cost, c(0.8))
  this_strategy <- strategy(tm, health_states, "intervention",
                            tm_cost, tm_util)
  mm <- markov_model(this_strategy, 10, c(1, 0), c(0, 0))
  trace_matrix_1 <- mm$trace_matrix
  df <- matrix(unlist(trace_data), nrow = 11)
  expect_equal(trace_matrix_1, df, check.attributes = FALSE, tolerance = 1e-4)

  mm <- markov_model(this_strategy, 5, c(1, 0), c(0, 0), NULL,
                     FALSE, FALSE, FALSE, "life_table")
  trace_matrix_1 <- mm$trace_matrix
  healthy_trace <- c(1.000000, 0.750000, 0.375000, 0.187500, 0.093750,
                     0.046875)
  expect_equal(trace_matrix_1[, 1], healthy_trace, check.attributes = FALSE,
               tolerance = 1e-4)

  mm <- markov_model(this_strategy, 10, c(1, 0), c(0, 0), NULL, TRUE, TRUE,
                     TRUE)
  trace_matrix_1 <- mm$trace_matrix
  expect_equal(trace_matrix_1, df, check.attributes = FALSE, tolerance = 1e-4)

  mm <- markov_model(this_strategy, 2, c(1, 0), c(0, 0), NULL, TRUE, TRUE,
                     TRUE, "half_cycle", c(10, 0), c(0, 0))
  cost_matrix_1 <- mm$cost_matrix
  cost_df <- c(5.5, 0.9, 0.225)
  expect_equal(cost_matrix_1[, 3], cost_df, check.attributes = FALSE,
               tolerance = 1e-4)


})
##############################################################################
context("testing combining markov states")
test_that("testing combining markov states", {
  tmat <- rbind(c(1, 2), c(3, 4))
  colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
  tm <- populate_transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
  a <- health_state("Healthy", 2, 1, 0, FALSE)
  b <- health_state("Dead", 1, 0, 0, TRUE)
  health_states <- combine_state(a, b)
  this_strategy <- strategy(tm, health_states, "intervention")
  mm1 <- markov_model(this_strategy, 10, c(1, 0), c(0, 0))
  a <- health_state("Healthy", 1, 0.8, 0, FALSE)
  b <- health_state("Dead", 1, 0, 0, TRUE)
  this_strategy <- strategy(tm, health_states, "control")
  mm2 <- markov_model(this_strategy, 10, c(1, 0), c(0, 0))
  list1 <- combine_markov(mm1, mm2)
  list2 <- combine_markov(list(mm1, mm2))
  expect_equal(list1[1, ]$method, list2[1, ]$method)
  expect_error(combine_markov(list(a, mm2)))
  expect_error(combine_markov(a, mm2))
})
##############################################################################
context("testing plotting markov states")
test_that("testing plotting markov states", {
  tmat <- rbind(c(1, 2), c(3, 4))
  colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
  tm <- populate_transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
  a <- health_state("Healthy", 2, 1, 0, FALSE)
  b <- health_state("Dead", 1, 0, 0, TRUE)
  health_states <- combine_state(a, b)
  this_strategy <- strategy(tm, health_states, "intervention")
  mm1 <- markov_model(this_strategy, 10, c(1, 0), c(0, 0))
  expect_error(plot_model(a))
  plot_model(mm1)
})
