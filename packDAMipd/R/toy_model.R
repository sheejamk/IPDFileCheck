# For strategy A
# matrix of allowed transition
tmat <- rbind(c(1, 2), c(3, 4))
#define colnames
colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
# populate the matrix - fixed values here, can use variables to estimate
# during run time, then they need to use define_parameters() and should be
# passed on when using markov_model
tm <- populate_transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
# state a named healthy with cost 1 and utility 1, not an absorbing state
a <- health_state("Healthy", 1, 0.5, 0, FALSE)
# state a named dead with cost 1 and utility 0,  an absorbing state
b <- health_state("Dead", 1, 0, 0, TRUE)
# combine the health states
health_states <- combine_state(a, b)
# strategy named A
A_strategy <- strategy(tm, health_states, "A")
# run markov model using A_strategy, for 10  cyles and initial state (1,0)
# no discounting, with half cycle correction, no startup costs etc
A_model <- markov_model(A_strategy, 10, c(1, 0))
# Can check the model's trace, cost and utility form A_model
A_model$trace_matrix
A_model$cost_matrix
A_model$utility_matrix
# plot the markov states - no commented please uncomment to
# run plot_model(A_model)

# Now follow the same for the strategy B
#if the transitions are same we can use the same tmat, and tm -
# no need to redefine health state costs and utilities are different,
# so redefining them state a named healthy with cost 10 and utility 1,
# not an absoring state
a <- health_state("Healthy", 10, 1, 0, FALSE)
# state a named dead with cost 1 and utility 0,  an absoring state
b <- health_state("Dead", 1, 0, 0, TRUE)
# combine the health states
health_states <- combine_state(a, b)
# strategy named B
B_strategy <- strategy(tm, health_states, "B")
# run markov model using A_strategy, for 10  cycles and initial state (1,0)
# no discounting, with half cycle correction, no start up costs etc
B_model <- markov_model(B_strategy, 10, c(1, 0))
# Can check the model's trace, cost and utility form A_model
B_model$trace_matrix
B_model$cost_matrix
B_model$utility_matrix
# plot the markov states - now commented please uncomment to
# run plot_model(B_model)

#Here the trace is same only cost and utility different
# now can calculate ICER nad NMB if threshold given use 20000
#combine the markov models A and B

list_markov <- combine_markov(A_model, B_model)
calculate_icer_nmb(list_markov, 20000, "A")
