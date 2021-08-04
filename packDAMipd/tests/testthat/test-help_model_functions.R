# ##############################################################################
context("testing assigning parameters")
test_that("testing assigning parameters", {
  param_list <- define_parameters(
    cost_direct_med_A = 1701, cost_comm_care_A = 1055,
    cost_health_A = "cost_direct_med_A + cost_comm_care_A"
  )
  assigned_list <- assign_parameters(param_list)
  # the values are correctly added to get cost_health_A
  expect_equal(assigned_list$cost_health_A, 2756)
  expect_equal(assigned_list$cost_direct_med_A, 1701)

  param_list <- define_parameters(cycle = 1, age = 50,
                                  cost = "1-exp(-(age - cycle))")
  assigned_list <- assign_parameters(param_list)
  # the cost is calculated correctly
  expect_equal(assigned_list$cost, 1)

  param_list <- define_parameters(dd = 1, ee = 2, ff = "dd + ee",
                                  cost = "dd +ff")
  assigned_list <- assign_parameters(param_list)
  expect_equal(assigned_list$ff, 3)
  # can give multiple expressions provided the unknown can be estimated
  # sequentially
  expect_equal(assigned_list$cost, 4)

  assign_list <- c(cost_A = 100, cost_B = 10)
  assigned_list <- assign_parameters(assign_list)
  expect_equal(assigned_list$cost_A, 100)

  assign_list <- c(a = 10, cost_A = "a+100", cost_B = 10)
  assigned_list <- assign_parameters(assign_list)
  # direct assignment is possible
  expect_equal(assigned_list$cost_A, 110)

  #use define parameters and then use assign_parameters
  param_list <- define_parameters(a = 10, cost_A = "a+100", cost_B = 10)
  assigned_list <- assign_parameters(assign_list)
  expect_equal(assigned_list$cost_A, 110)

  # while using direct assignment, it has to be name value pairs
  assign_list <- c("a=10", "cost_A= a+100")
  expect_error(assign_parameters(assign_list))

  assign_list <- c("cost_A=100", "a=10")
  expect_error(assign_parameters(assign_list))

  # When using define_parameters also use name value pair
  assign_list <- define_parameters(c("cost_A=100", "a=10"))
  expect_error(assign_parameters(assign_list))

  # When using define_parameters also use name value pair, this is fine
  assign_list <- define_parameters(a = 10, cost_A = "a+100")
  assigned_list <- assign_parameters(assign_list)
  expect_equal(assigned_list$cost_A, 110)
  # When using define_parameters also use name value pair, but here
  #  error occurs as the unknown a comes before its assignment
  assign_list <- define_parameters(c(cost_A = "a+100", a = 10))
  expect_error(assign_parameters(assign_list))
  # Error - list can not be NA or null
  expect_error(assign_parameters(NA))
  expect_error(assign_parameters(NULL))
  assign_list <- define_parameters(a = "100", cost = "a+10")
  assign_parameters(assign_list)
})
###############################################################################
context("testing getting parameters between operators")
test_that("testing getting parameters between operators", {
  # parameters between arithmetic operations
  expect_equal(find_parameters_btn_operators("a+b"), c("a", "b"))
  # parameters even if they are function
  expect_equal(find_parameters_btn_operators("mean(a,b)+c"),
               c("mean(a, b)", "c"))

  expect_equal(find_parameters_btn_operators("cost_a + cost_b * rr - cost_c"),
               c("cost_a", "cost_b", "rr", "cost_c"))
  expect_equal(find_parameters_btn_operators("cost_a + cost_b |  rr - cost_c"),
               c("cost_a", "cost_b", "rr", "cost_c"))
  expect_equal(find_parameters_btn_operators("cost_a + cost_b ||  rr - cost_c"),
               c("cost_a", "cost_b", "rr", "cost_c"))
  # if no operators, return the string
  expect_equal(find_parameters_btn_operators("it works"), "it works")
  # Error - expression can not be NA or null
  expect_error(find_parameters_btn_operators(NA))
  expect_error(find_parameters_btn_operators(NULL))

})
###############################################################################
context("testing check the inputs for markov model and get the correct method")
test_that("testing check the inputs for model and get the correct method", {
  tmat <- rbind(c(1, 2), c(3, 4))
  colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
  tm <- populate_transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
  a <- health_state("Healthy", 1, 1, 0, FALSE)
  b <- health_state("Dead", 1, 0, 0, TRUE)
  health_states <- combine_state(a, b)
  this_strategy <- strategy(tm, health_states, "intervention")
  startup_cost <- c(100, 100)
  startup_util <- c(0.5, 0.5)
  # half cycle correction is a correct method
  changedmethod <- checks_markov_pick_method(this_strategy, c(1, 0), c(0, 0),
                                  "half cycle correction", TRUE, startup_cost,
                                  startup_util, FALSE, FALSE)
  expect_equal(changedmethod, "hc_correction")
  ## can give half_cycle too
  changedmethod <- checks_markov_pick_method(this_strategy, c(1, 0), c(0, 0),
                                  "half_cycle", TRUE, NULL, NULL, FALSE, FALSE)
  expect_equal(changedmethod, "hc_correction")
  ## method can belife_table too
  changedmethod <- checks_markov_pick_method(this_strategy, c(1, 0), c(0, 0),
                                            "life table", FALSE, NULL, NULL
                                            , FALSE, FALSE)
  expect_equal(changedmethod, "life_table")
  ## cycle is not a valid method
  expect_error(checks_markov_pick_method(this_strategy, c(1, 0), c(0, 0),
                                         "cycle", TRUE, NULL, NULL,
                                         FALSE, FALSE))
  ## size of discount vector should be 2
  expect_error(checks_markov_pick_method(this_strategy, c(1, 0), c(0, 0, 0),
                                         "life table", FALSE, NULL, NULL,
                                         FALSE, FALSE))
  ##method life table should be followed by false for the parameter
  ## "half_cycle_correction"
  expect_error(checks_markov_pick_method(this_strategy, c(1, 0), c(0, 0),
                                         "life table", TRUE, NULL, NULL,
                                         FALSE, FALSE))
  ## length of initial state vector should be equal to the number of states
  expect_error(checks_markov_pick_method(this_strategy, c(1), c(0, 0),
                                         "life table", FALSE, NULL, NULL,
                                         FALSE, FALSE))
  ## tm is not a stratgey, expected an object of type strategy
  expect_error(checks_markov_pick_method(tm, c(1), c(0, 0),
                                         "life table", FALSE, NULL, NULL,
                                         FALSE, FALSE))

  startup_cost <- c(100, 100, 0)
  startup_util <- c(0.5, 0.5)
  expect_error(checks_markov_pick_method(this_strategy, c(1, 0), c(0, 0),
                                         "life table", FALSE, startup_cost,
                                         startup_util,
                                         FALSE, FALSE))
  startup_cost <- c(100, 100)
  startup_util <- c(0.5)
  expect_error(checks_markov_pick_method(this_strategy, c(1, 0), c(0, 0),
                                         "life table", FALSE, startup_cost,
                                         startup_util,
                                         FALSE, FALSE))
  # transiiton costs present -but state prevalency calculation not boolean
  tmat_cost <- rbind(c(NA, 1), c(NA, NA))
  colnames(tmat_cost) <- rownames(tmat) <- c("Healthy", "Dead")
  tm_cost <- transition_cost_util(2, tmat_cost, c(10))
  tmat_util <- rbind(c(NA, 1), c(NA, NA))
  colnames(tmat_util) <- rownames(tmat) <- c("Healthy", "Dead")
  tm_util <- transition_cost_util(2, tmat_cost, c(0.2))
  this_strategy <- strategy(tm, health_states, "intervention", tm_cost, tm_util)
  expect_error(checks_markov_pick_method(this_strategy, c(1, 0), c(0, 0),
                            "life table", FALSE, NULL, NULL,
                            "No", FALSE))
  expect_error(checks_markov_pick_method(this_strategy, c(1, 0), c(0, 0),
                                         "life table", FALSE, NULL, NULL,
                                         TRUE, "No"))
})
# ##############################################################################
context("testing checking null or na")
test_that("testing checking null or na", {
  expect_equal(check_null_na(12), 0)
  expect_equal(check_null_na(list(12, "she")), 0)
  b <- NULL
  expect_equal(check_null_na(b), -1)
  b <- c(NA, NA)
  expect_equal(check_null_na(b), -2)
})
