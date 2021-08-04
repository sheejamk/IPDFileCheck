###############################################################################
context("testing calculating icer nmb")
test_that("testing calculating icer nmb", {
  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 100, utility = 1)
  dead <- health_state("dead", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
  tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1),
                                   colnames(tmat))
  health_states <- combine_state(well, disabled, dead)
  this_strategy <- strategy(tm, health_states, "control")
  this_markov <- markov_model(this_strategy, 24, c(1000, 0, 0),
                              discount = c(0, 0))
  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 10, utility = 0.5)
  dead <- health_state("dead", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
  tm <- populate_transition_matrix(3, tmat, c(0.4, 0.4, 0.2, 0.6, 0.4, 1),
                                   colnames(tmat))
  health_states <- combine_state(well, disabled, dead)
  this_strategy <- strategy(tm, health_states, "intervention")
  sec_markov <- markov_model(this_strategy, 24, c(1000, 0, 0),
                             discount = c(0, 0))
  list_markov <- combine_markov(this_markov, sec_markov)
  icer_nmb <- calculate_icer_nmb(list_markov, 20000, comparator = "control")
  expect_equal(as.numeric(icer_nmb$ICER[2]), 86.67, tol = 1e-3)

  expect_equal(as.numeric(icer_nmb$ICER[2]), 86.67, tol = 1e-3)
  # Error comparator con is not valid
  expect_error(calculate_icer_nmb(list_markov, 20000, comparator = "con"))
  # Error threshold value  0 is not valid
  expect_error(calculate_icer_nmb(list_markov, 0, comparator = "control"))

  # Error list markov can not be null
  expect_error(calculate_icer_nmb(NULL, 200, comparator = "control"))

  icer_nmb <- calculate_icer_nmb(list_markov, 200, comparator = NULL)
  expect_equal(as.numeric(icer_nmb$NMB[1]), 524.9899, tol = 1e-3)

  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 100, utility = 1)
  dead <- health_state("dead", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
  tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1),
                                   colnames(tmat))
  health_states <- combine_state(well, disabled, dead)
  this_strategy <- strategy(tm, health_states, "control")
  this_markov <- markov_model(this_strategy, 24, c(1000, 0, 0),
                              discount = c(0, 0))
  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 10, utility = 0.5)
  dead <- health_state("dead", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
  tm <- populate_transition_matrix(3, tmat, c(0.4, 0.4, 0.2, 0.6, 0.4, 1),
                                   colnames(tmat))
  health_states <- combine_state(well, disabled, dead)
  this_strategy <- strategy(tm, health_states, "intervention")

  # diffenet number of cycles
  sec_markov <- markov_model(this_strategy, 12, c(1000, 0, 0),
                             discount = c(0, 0))
  list_markov <- combine_markov(this_markov, sec_markov)
  expect_error(calculate_icer_nmb(list_markov, 20000, comparator = "control"))

  # column names different
  sec_markov <- markov_model(this_strategy, 24, c(1000, 0, 0),
                             discount = c(0, 0))
  list_markov <- combine_markov(this_markov, sec_markov)
  newcolnames <- colnames(list_markov)
  newcolnames[2] <- "changedcost"
  colnames(list_markov) <- newcolnames
  expect_error(calculate_icer_nmb(list_markov, 20000, comparator = "control"))

})
###############################################################################
context("testing checking list of markov models")
test_that("testing checking list of markov models", {
  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 100, utility = 1)
  dead <- health_state("dead", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
  tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1),
                                   colnames(tmat))
  health_states <- combine_state(well, disabled, dead)
  this_strategy <- strategy(tm, health_states, "control")
  this_markov <- markov_model(this_strategy, 24, c(1000, 0, 0),
                              discount = c(0, 0))
  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 10, utility = 0.5)
  dead <- health_state("dead", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
  tm <- populate_transition_matrix(3, tmat, c(0.4, 0.4, 0.2, 0.6, 0.4, 1),
                                   colnames(tmat))
  health_states <- combine_state(well, disabled, dead)
  this_strategy <- strategy(tm, health_states, "intervention")
  sec_markov <- markov_model(this_strategy, 24, c(1000, 0, 0),
                             discount = c(0, 0))
  list_markov <- combine_markov(this_markov, sec_markov)
  expect_equal(check_list_markov_models(list_markov), 0)

  this_strategy <- strategy(tm, health_states, "control")
  sec_markov <- markov_model(this_strategy, 24, c(1000, 0, 0),
                             discount = c(0, 0))
  list_markov <- combine_markov(this_markov, sec_markov)
  expect_error(check_list_markov_models(list_markov))

  this_strategy <- strategy(tm, health_states, "intervention")
  sec_markov <- markov_model(this_strategy, 12, c(1000, 0, 0),
                             discount = c(0, 0))
  list_markov <- combine_markov(this_markov, sec_markov)
  expect_error(check_list_markov_models(list_markov))

  this_strategy <- strategy(tm, health_states, "intervention")
  sec_markov <- markov_model(this_strategy, 24, c(500, 0, 0),
                             discount = c(0, 0))
  list_markov <- combine_markov(this_markov, sec_markov)
  expect_error(check_list_markov_models(list_markov))

  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 10, utility = 0.5)
  dead <- health_state("dead", cost = 0, utility = 0)
  dead2 <- health_state("dead2", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3, 4), c(NA, 5, 6, 7), c(NA, NA, 8, 9),
                c(NA, NA, NA, 10))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead", "dead2")
  tm <- populate_transition_matrix(4, tmat, c(0.4, 0.3, 0.2, 0.1, 0.5, 0.4,
                                          0.1, 0.5, 0.5, 1), colnames(tmat))
  health_states <- combine_state(well, disabled, dead, dead2)
  this_strategy <- strategy(tm, health_states, "intervention")
  sec_markov <- markov_model(this_strategy, 24, c(1000, 0, 0, 0),
                             discount = c(0, 0))
  list_markov <- combine_markov(this_markov, sec_markov)
  expect_error(check_list_markov_models(list_markov))

})
###############################################################################
context("testing plotting ceac")
test_that("testing plotting ceac", {
  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 100, utility = 1)
  dead <- health_state("dead", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
  tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1),
                                   colnames(tmat))
  health_states <- combine_state(well, disabled, dead)
  this.strategy <- strategy(tm, health_states, "control")
  this_markov <- markov_model(this.strategy, 24, c(1000, 0, 0), c(0, 0))
  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 10, utility = 0.5)
  dead <- health_state("dead", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
  tm <- populate_transition_matrix(3, tmat, c(0.4, 0.4, 0.2, 0.6, 0.4, 1),
                                   colnames(tmat))
  health_states <- combine_state(well, disabled, dead)
  this.strategy <- strategy(tm, health_states, "intervention")
  sec_markov <- markov_model(this.strategy, 24, c(1000, 0, 0), c(0, 0))
  list_markov <- combine_markov(this_markov, sec_markov)
  plot_ceac(list_markov, c(1000, 2000, 3000), comparator = "control")

  expect_error(plot_ceac(NULL, c(1000, 2000, 3000), comparator = "control"))
  expect_error(plot_ceac(list_markov, NULL, comparator = "control"))
  expect_error(plot_ceac(list_markov, c(1000), comparator = "control"))
})
###############################################################################
context("testing plotting efficieny frontier")
test_that("testing plotting efficieny frontier", {
  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 100, utility = 1)
  dead <- health_state("dead", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
  tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1),
                                   colnames(tmat))
  health_states <- combine_state(well, disabled, dead)
  this.strategy <- strategy(tm, health_states, "control")
  this_markov <- markov_model(this.strategy, 24, c(1000, 0, 0), c(0, 0))
  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 10, utility = 0.5)
  dead <- health_state("dead", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
  tm <- populate_transition_matrix(3, tmat, c(0.4, 0.4, 0.2, 0.6, 0.4, 1),
                                   colnames(tmat))
  health_states <- combine_state(well, disabled, dead)
  this.strategy <- strategy(tm, health_states, "intervention")
  sec_markov <- markov_model(this.strategy, 24, c(1000, 0, 0), c(0, 0))
  list_markov <- combine_markov(this_markov, sec_markov)
  results_cea <- calculate_icer_nmb(list_markov, 20000, comparator = "control")

  plot_efficiency_frontier(results_cea, c(1000, 2000))
  expect_error(plot_efficiency_frontier(NULL, c(1000, 2000)))
  expect_error(plot_efficiency_frontier(results_cea, NULL))
  newcolnames <- colnames(results_cea)
  newcolnames[2] <- "changedcost"
  colnames(results_cea) <- newcolnames
  expect_error(plot_efficiency_frontier(results_cea, c(1000, 2000)))
})
