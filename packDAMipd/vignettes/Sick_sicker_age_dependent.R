## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ">",
 fig.width = 6, 
 fig.height = 4
)

## ----setup--------------------------------------------------------------------
library(packDAMipd)

## -----------------------------------------------------------------------------
H <- health_state("H", cost = "c_H ", utility = "u_H")
S1 <- health_state("S1", cost = "c_S1",utility = "u_S1")
S2 <- health_state("S2", cost = "c_S2",utility = "u_S2")
D <- health_state("D", cost = "c_D ",utility = "u_D")

## -----------------------------------------------------------------------------
tmat <- rbind(c(1, 2, NA, 3), c(4, 5, 6, 7),c(NA, NA, 8, 9), c(NA, NA, NA, 10))
colnames(tmat) <- rownames(tmat) <- c("H","S1" ,"S2","D")

## -----------------------------------------------------------------------------
tmat_cost <- rbind(c(NA, 1, NA, 2), c(NA, NA, NA, 3),c(NA, NA, NA, 4), 
                   c(NA, NA, NA, NA))
colnames(tmat_cost) <- rownames(tmat_cost) <- c("H", "S1", "S2", "D")
tmat_util <- rbind(c(NA, 1, NA, NA), c(NA, NA, NA, NA),c(NA, NA, NA, NA), 
                   c(NA, NA, NA, NA))
colnames(tmat_util) <- rownames(tmat_util) <- c("H", "S1", "S2", "D")


## -----------------------------------------------------------------------------
tm <- populate_transition_matrix(4, tmat, c("p_HH","p_HS1","p_HDage","p_S1H",
                                   "p_S1S1", "p_S1S2", "p_S1Dage",
                                   "p_S2S2","p_S2Dage","p_DD" ), 
                                 colnames(tmat))
tm_cost <- transition_cost_util(4, tmat_cost, c("ic_HS1","ic_D","ic_D","ic_D"), colnames(tmat_cost))
tm_util <- transition_cost_util(4, tmat_util, c("du_HS1" ), colnames(tmat_util))



## -----------------------------------------------------------------------------
mortality_file = system.file("extdata", "LifeTable_USA_Mx_2015.csv",  package = "packDAMipd")
param_list <- define_parameters(age_init = 25,
                      mortality_age = "get_mortality_from_file(mortality_file, age_init - 1 + markov_cycle, \"total\")",
                      p_HDage = "1-exp(-mortality_age)", p_HS1  = 0.15, 
                      p_S1H = 0.5,
                      p_S1S2  = 0.105, hr_S1   = 3,hr_S2   = 10,
                      p_S1Dage  =  "1-exp(-mortality_age *hr_S1)",
                      p_S2Dage  = "1-exp(-mortality_age *hr_S2)",
                      p_HH = "1 - (p_HS1 + p_HDage)",
                      p_S1S1 = "1 - (p_S1H + p_S1S2+ p_S1Dage)",
                      p_S2S2 = "1 - ( p_S2Dage)",
                      p_DD = 1,
                      c_H   = 2000,c_S1  = 4000,c_S2  = 15000,
                      c_D   = 0, c_Trt = 12000,u_H   = 1,
                      u_S1  = 0.75, u_S2  = 0.5,u_D   = 0,
                      u_Trt = 0.95, du_HS1 = -0.01, ic_HS1 = 1000, ic_D = 2000)

## -----------------------------------------------------------------------------
health_states <- combine_state(H,S1,S2,D)
uc_strategy <- strategy(tm, health_states, "Usual care",tm_cost,tm_util)
uc_markov <- markov_model(current_strategy = uc_strategy, cycles = 85, 
                          initial_state = c(1, 0,0,0), discount = c(0.03,0.03), parameter_values = param_list, TRUE, 
                          method = "half cycle correction")
plot_model(uc_markov)


## -----------------------------------------------------------------------------
H <- health_state("H", cost = "c_H ", utility = "u_H")
S1 <- health_state("S1", cost = "c_S1 + c_Trt ",utility = "u_Trt")
S2 <- health_state("S2", cost = "c_S2 + c_Trt",utility = "u_S2")
D <- health_state("D", cost = "c_D ",utility = "u_D")
health_states <- combine_state(H,S1,S2,D)
trt_strategy <- strategy(tm, health_states, "New treatment",tm_cost,tm_util)
trt_markov <- markov_model(current_strategy = trt_strategy, cycles = 85, 
                           initial_state = c(1, 0,0,0), discount = c(0.03,0.03),parameter_values = param_list,TRUE, 
                           method = "half cycle correction")


## -----------------------------------------------------------------------------
list_markov <- combine_markov(uc_markov, trt_markov)
cal <- calculate_icer_nmb(list_markov, threshold = 20000, 
                          comparator = "Usual care" )
cal

## -----------------------------------------------------------------------------
plot_ceac(list_markov,threshold_values = c(1000, 2000, 5000, 7000, 10000, 
                                           150000, 20000), 
          comparator = "Usual care" , currency = "USD")

