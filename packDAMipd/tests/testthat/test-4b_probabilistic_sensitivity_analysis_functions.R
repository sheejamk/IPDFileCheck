###############################################################################
context("testing defining parameter for probabilistic sensitivity analysis")
test_that("testing defining parameter for probabilistic sensitivity analysis", {
  param_list <- define_parameters(
    cost_direct_med_B = 1774,
    cost_comm_care_C = 2059
  )
  sample_list <- define_parameters(cost_direct_med_B =
                                     "gamma(mean = 1774, sd = sqrt(1774))")
  param_table <- define_parameters_psa(param_list, sample_list)
  expect_equal(param_table$base_param_list$cost_direct_med_B, 1774)
  expect_equal(param_table$base_param_list$cost_comm_care_C, 2059)
  expect_equal(param_table$sample_list$cost_direct_med_B,
               "gamma(mean = 1774, sd = sqrt(1774))")
  expect_equal(param_table$sample_list$cost_comm_care_C, 2059)
  sample_list <- define_parameters(cost_zido =
                                     "gamma(mean = 1774, sd = sqrt(1774))")
  # Error - parameter given in sample list not in baselist
  expect_error(define_parameters_psa(param_list, sample_list))
  # Error - Parameter list should be of list
  sample_list <- c(cost_direct_med_B = "gamma(mean = 1774, sd = sqrt(1774))")
  expect_error(define_parameters_psa(param_list, sample_list))
  # Error -  the parameters can not be NULL
  expect_error(define_parameters_psa(param_list, NULL))

})
###############################################################################
context("testing doing probabilistic sensitivity analysis")
test_that("testing doing probabilistic sensitivity analysis", {
  param_list <- define_parameters(
    cost_direct_med_A = 1701,
    cost_direct_med_B = 1774, tpAtoA = 0.2,
    tpAtoB = 0.5, tpAtoC = 0.3,
    tpBtoB = 0.3, tpBtoC = 0.7,
    tpCtoC = 1,
    cost_health_A = "cost_direct_med_A",
    cost_health_B = "cost_direct_med_B"
  )
  sample_list <- define_parameters(cost_direct_med_A =
                                     "gamma(mean = 1701, sd = sqrt(1701))")
  A <- health_state("A", cost = "cost_health_A", utility = 1)
  B <- health_state("B", cost = "cost_health_B", utility = 1)
  C <- health_state("C", cost = 0, utility = 0, absorb = "TRUE")
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("A", "B", "C")
  tm <- populate_transition_matrix(3, tmat, c(
    "tpAtoA", "tpAtoB", "tpAtoC",
    "tpBtoB", "tpBtoC", "tpCtoC"
  ), colnames(tmat))
  health_states <- combine_state(A, B, C)
  mono_strategy <- strategy(tm, health_states, "mono")
  mono_markov <- markov_model(mono_strategy, 20, discount = c(0.06, 0),
                              initial_state = c(1, 0, 0), param_list)
  param_table <- define_parameters_psa(param_list, sample_list)
  result <- do_psa(mono_markov, param_table, 3)
  expect_equal(result$base_result$strategy$name_strategy, "mono")
  expect_equal(result$rep_1$strategy$name_strategy, "mono")
  expect_equal(result$rep_2$strategy$name_strategy, "mono")
  expect_equal(result$rep_3$strategy$name_strategy, "mono")

  do_psa(mono_markov, define_parameters_psa(param_list, sample_list), 3)

  #Error - markov model worng type
  expect_error(do_psa(NULL, param_table, 2))
  #Error - Parameter list should be of type list
  expect_error(do_psa(mono_markov, define_parameters_psa(c(1, 2), c(1, 2)), 2))
  # Error - parameter table should have  2 entries  - default parameter values,
  # and parameters with sampling distributions
  expect_error(do_psa(mono_markov, c(1, 2), 2))
  # error- number of replications less than or equal to 0
  expect_warning(do_psa(mono_markov, define_parameters_psa(param_list,
                                                           sample_list), 0))

  sample_list <- define_parameters(cost_direct_med_B = 1774)
  param_table <- define_parameters_psa(param_list, sample_list)
  do_psa(mono_markov, param_table, 3)

})
###############################################################################
context("testing listing probabilistic sensitivity analysis results")
test_that("testing listing probabilistic sensitivity analysis results", {
  param_list <- define_parameters(
    cost_direct_med_A = 1701,
    cost_direct_med_B = 1774, tpAtoA = 0.2,
    tpAtoB = 0.5, tpAtoC = 0.3,
    tpBtoB = 0.3, tpBtoC = 0.7,
    tpCtoC = 1,
    cost_health_A = "cost_direct_med_A",
    cost_health_B = "cost_direct_med_B"
  )
  sample_list <- define_parameters(cost_direct_med_A = "gamma(mean = 1701,
                                   sd = sqrt(1701))")
  A <- health_state("A", cost = "cost_health_A ", utility = 1)
  B <- health_state("B", cost = "cost_health_B", utility = 1)
  C <- health_state("C", cost = 0, utility = 0, absorb = "TRUE")
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("A", "B", "C")
  tm <- populate_transition_matrix(3, tmat, c(
    "tpAtoA", "tpAtoB", "tpAtoC",
    "tpBtoB", "tpBtoC", "tpCtoC"
  ), colnames(tmat))
  health_states <- combine_state(A, B, C)
  mono_strategy <- strategy(tm, health_states, "mono")
  mono_markov <- markov_model(mono_strategy, 20, initial_state = c(1, 0, 0),
                              discount = c(0.06, 0), param_list)
  param_table <- define_parameters_psa(param_list, sample_list)
  result <- do_psa(mono_markov, param_table, 3)

  result_plot <- summary_plot_psa(result, NULL, NULL, NULL)

  #Error   null control result psa
  expect_error(summary_plot_psa(NULL))

  param_list_comb <- define_parameters(
    cost_direct_med_A = 1800,
    cost_direct_med_B = 1774, tpAtoA = 0.6,
    tpAtoB = 0.1, tpAtoC = 0.3,
    tpBtoB = 0.3, tpBtoC = 0.7,
    tpCtoC = 1,
    cost_health_A = "cost_direct_med_A",
    cost_health_B = "cost_direct_med_B"
  )
  comb_strategy <- strategy(tm, health_states, "comb")
  comb_markov <- markov_model(comb_strategy, 20, c(1, 0, 0),
                              discount = c(0.06, 0), param_list)

  param_table_comb <- define_parameters_psa(param_list_comb, sample_list)

  # error - no of repetitions differnt
  result_comb <- do_psa(comb_markov, param_table_comb, 5)
  expect_error(summary_plot_psa(result, result_comb, 2000, "mono"))

  # error - threshold not valid
  result_comb <- do_psa(comb_markov, param_table_comb, 3)
  expect_error(summary_plot_psa(result, result_comb, -1, "mono"))


  # Error - strategies should have unique names
  comb_strategy <- strategy(tm, health_states, "mono")
  comb_markov <- markov_model(comb_strategy, 20, c(1, 0, 0),
                              discount = c(0.06, 0), param_list
  )
  result_comb <- do_psa(comb_markov, param_table_comb, 3)
  expect_error(summary_plot_psa(result, result_comb, 2000, "mono"))

  comb_strategy <- strategy(tm, health_states, "comb")
  comb_markov <- markov_model(comb_strategy, 20, c(1, 0, 0),
    discount = c(0.06, 0), param_list
  )
  result_comb <- do_psa(comb_markov, param_table_comb, 3)
  results_list_parm <- summary_plot_psa(result, result_comb, 2000, "mono")
})
###############################################################################
context("testing summary plots psa")
test_that("testing summary plots psa", {
  param_list <- define_parameters(
    cost_direct_med_A = 1701,
    cost_direct_med_B = 1774, tpAtoA = 0.2,
    tpAtoB = 0.5, tpAtoC = 0.3,
    tpBtoB = 0.3, tpBtoC = 0.7,
    tpCtoC = 1,
    cost_health_A = "cost_direct_med_A",
    cost_health_B = "cost_direct_med_B"
  )
  sample_list <- define_parameters(cost_direct_med_A = "gamma(mean = 1701,
                                   sd = sqrt(1701))")
  A <- health_state("A", cost = "cost_health_A ", utility = 1)
  B <- health_state("B", cost = "cost_health_B", utility = 1)
  C <- health_state("C", cost = 0, utility = 0, absorb = "TRUE")
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("A", "B", "C")
  tm <- populate_transition_matrix(3, tmat, c(
    "tpAtoA", "tpAtoB", "tpAtoC",
    "tpBtoB", "tpBtoC", "tpCtoC"
  ), colnames(tmat))
  health_states <- combine_state(A, B, C)
  mono_strategy <- strategy(tm, health_states, "mono")
  mono_markov <- markov_model(mono_strategy, 20, initial_state = c(1, 0, 0),
                              discount = c(0.06, 0), param_list)
  param_table <- define_parameters_psa(param_list, sample_list)
  result <- do_psa(mono_markov, param_table, 3)
  result_paramwise <- list_paramwise_psa_result(result, NULL, NULL, NULL)

  expect_equal(result_paramwise$utility, c(1.642857, 1.642857, 1.642857),
               tolerance = 1e-4)
  expect_equal(result_paramwise$cost_health_B, c(1774, 1774, 1774),
               tolerance = 1e-4)
  #Error   null control result psa
  expect_error(list_paramwise_psa_result(NULL, NULL, NULL, NULL))

  param_list_comb <- define_parameters(
    cost_direct_med_A = 1800,
    cost_direct_med_B = 1774, tpAtoA = 0.6,
    tpAtoB = 0.1, tpAtoC = 0.3,
    tpBtoB = 0.3, tpBtoC = 0.7,
    tpCtoC = 1,
    cost_health_A = "cost_direct_med_A",
    cost_health_B = "cost_direct_med_B"
  )
  comb_strategy <- strategy(tm, health_states, "comb")
  comb_markov <- markov_model(comb_strategy, 20, c(1, 0, 0),
                              discount = c(0.06, 0), param_list)

  param_table_comb <- define_parameters_psa(param_list_comb, sample_list)

  # error - no of repetitions differnt
  result_comb <- do_psa(comb_markov, param_table_comb, 5)
  expect_error(list_paramwise_psa_result(result, result_comb, 2000, "mono"))

  # error - threshold not valid
  result_comb <- do_psa(comb_markov, param_table_comb, 3)
  expect_error(list_paramwise_psa_result(result, result_comb, -1, "mono"))


  # Error - stratgies should have unique names
  comb_strategy <- strategy(tm, health_states, "mono")
  comb_markov <- markov_model(comb_strategy, 20, c(1, 0, 0),
                              discount = c(0.06, 0), param_list
  )
  result_comb <- do_psa(comb_markov, param_table_comb, 3)
  expect_error(list_paramwise_psa_result(result, result_comb, 2000, "mono"))

  comb_strategy <- strategy(tm, health_states, "comb")
  comb_markov <- markov_model(comb_strategy, 20, c(1, 0, 0),
                              discount = c(0.06, 0), param_list
  )
  result_comb <- do_psa(comb_markov, param_table_comb, 3)
  results_list_parm <- list_paramwise_psa_result(result,
                                                 result_comb, 2000, "mono")

  A <- health_state("A", cost = "costA", utility = 1)
  B <- health_state("B", cost = 10, utility = 1)
  C <- health_state("C", cost = 0, utility = 0, absorb = "TRUE")
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("A", "B", "C")
  tm <- populate_transition_matrix(3, tmat, c(
    "tpAtoA", "tpAtoB", "tpAtoC",
    "tpBtoB", "tpBtoC", "tpCtoC"
  ), colnames(tmat))
  health_states <- combine_state(A, B, C)
  comb_strategy <- strategy(tm, health_states, "comb")
  param_list_comb <- define_parameters(
    costA = 1800, tpAtoA = 0.6,
    tpAtoB = 0.1, tpAtoC = 0.3,
    tpBtoB = 0.3, tpBtoC = 0.7,
    tpCtoC = 1
  )
  comb_markov <- markov_model(comb_strategy, 20, c(1, 0, 0),
                              discount = c(0.06, 0), param_list_comb
  )
  sample_list <- define_parameters(costA = "gamma(mean = 1701,
                                   sd = sqrt(1701))")
  param_table_comb <- define_parameters_psa(param_list_comb, sample_list)
  result_comb <- do_psa(comb_markov, param_table_comb, 3)
  #variable names are different, please check the markov model lists
  expect_error(list_paramwise_psa_result(result, result_comb, 2000, "mono"))

})
