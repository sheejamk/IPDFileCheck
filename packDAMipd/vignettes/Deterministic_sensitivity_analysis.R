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
A <- health_state("A", cost = "cost_health_A+ cost_drug ",utility = 1)
B <- health_state("B", cost = "cost_health_B + cost_drug",utility = 1)
C <- health_state("C", cost = "cost_health_C + cost_drug",utility = 1)
D <- health_state("D", cost = 0,utility = 0)

## -----------------------------------------------------------------------------
tmat <- rbind(c(1, 2,3,4), c(NA, 5,6,7),c(NA, NA, 8,9), c(NA,NA,NA,10))
colnames(tmat) <- rownames(tmat) <- c("A","B" ,"C","D")

## -----------------------------------------------------------------------------
tm <- populate_transition_matrix(4, tmat, c("tpAtoA","tpAtoB","tpAtoC",
                                            "tpAtoD",
                                   "tpBtoB", "tpBtoC", "tpBtoD",
                                   "tpCtoC","tpCtoD","tpDtoD" ), 
                                 colnames(tmat))

## -----------------------------------------------------------------------------
health_states <- combine_state(A,B,C,D)
mono_strategy <- strategy(tm, health_states, "mono")


## -----------------------------------------------------------------------------
mono_params = define_parameters(cost_zido = 2278,
cost_direct_med_A = 1701,
cost_comm_care_A = 1055,
cost_direct_med_B = 1774,
cost_comm_care_B = 1278,
cost_direct_med_C = 6948,
cost_comm_care_C = 2059,
tpAtoA = 1251/(1251 + 483),
tpAtoB = 350/(350 + 1384),
tpAtoC = 116/(116 + 1618),
tpAtoD = 17/(17 + 1717),
tpBtoB = 731/(731 + 527),
tpBtoC = 512/(512 + 746),
tpBtoD = 15/(15 + 1243),
tpCtoC = 1312/(1312 + 437),
tpCtoD = 437/(437 + 1312),
tpDtoD = 1,
cost_health_A = "cost_direct_med_A+ cost_comm_care_A",
cost_health_B = "cost_direct_med_B+ cost_comm_care_B",
cost_health_C = "cost_direct_med_C+ cost_comm_care_C",
cost_drug = "cost_zido")

## -----------------------------------------------------------------------------

mono_markov <- markov_model(mono_strategy, 20, c(1,0,0,0), discount = c(0.06,0),mono_params)

## -----------------------------------------------------------------------------
 #Define function to set the cost to be different for first two cycles
define_comb_cost = function(cycle,cost_lami) {
  if (cycle == 2 || cycle == 3)
    return(cost_lami)
  else
    return(0)
}
 #Define function to set the risk ratio to be different for first two cycles

define_rr = function(cycle,rr) {
  if (cycle == 2 || cycle == 3)
    return(rr)
  else
    return(1)
}

## -----------------------------------------------------------------------------
A <-  health_state("A", cost = "cost_health_A + cost_drug",utility = 1)
B <- health_state("B", cost = "cost_health_B + cost_drug",utility = 1)
C <- health_state("C", cost = "cost_health_C + cost_drug",utility = 1)
D <- health_state("D", cost = 0,utility = 0)


## -----------------------------------------------------------------------------
tmat <- rbind(c(1, 2,3,4), c(NA, 5,6,7),c(NA, NA, 8,9), c(NA,NA,NA,10))
colnames(tmat) <- rownames(tmat) <- c("A","B" ,"C","D")


## -----------------------------------------------------------------------------
tm <- populate_transition_matrix(4, tmat, c("tpAtoA_rr","tpAtoB_rr","tpAtoC_rr","tpAtoD_rr",
                                   "tpBtoB_rr", "tpBtoC_rr", "tpBtoD_rr",
                                   "tpCtoC_rr","tpCtoD_rr","tpDtoD_rr" ), colnames(tmat) )


## -----------------------------------------------------------------------------
comb_params <- define_parameters(cost_zido = 2278,
                     cost_direct_med_A = 1701,
                     cost_comm_care_A = 1055,
                     cost_direct_med_B = 1774,
                     cost_comm_care_B = 1278,
                     cost_direct_med_C = 6948,
                     cost_comm_care_C = 2059,
                     tpAtoA = 1251/(1251 + 483),
                     tpAtoB = 350/(350 + 1384),
                     tpAtoC = 116/(116 + 1618),
                     tpAtoD = 17/(17 + 1717),
                     tpBtoB = 731/(731 + 527),
                     tpBtoC = 512/(512 + 746),
                     tpBtoD = 15/(15 + 1243),
                     tpCtoC = 1312/(1312 + 437),
                     tpCtoD = 437/(437 + 1312),
                     tpDtoD = 1,
                     rr = 0.509,
                     cost_lami = 2086.50,
                     rr_cycle = "define_rr(markov_cycle,rr)",
                     tpAtoA_rr = "1-tpAtoB*rr_cycle-tpAtoC*rr_cycle-tpAtoD*rr_cycle",
                     tpAtoB_rr = "tpAtoB*rr_cycle",
                     tpAtoC_rr = "tpAtoC*rr_cycle",
                     tpAtoD_rr = "tpAtoD*rr_cycle",
                     tpBtoB_rr = "1-tpBtoC*rr_cycle-tpBtoD*rr_cycle",
                     tpBtoC_rr = "tpBtoC*rr_cycle",
                     tpBtoD_rr = "tpBtoD*rr_cycle",
                     tpCtoC_rr = "1-tpCtoD*rr_cycle",
                     tpCtoD_rr = "tpCtoD*rr_cycle",
                     tpDtoD_rr = 1,
                     cost_health_A = "cost_direct_med_A + cost_comm_care_A",
                     cost_health_B = "cost_direct_med_B + cost_comm_care_B",
                     cost_health_C = "cost_direct_med_C + cost_comm_care_C",
                     cost_lami_cycle = "define_comb_cost(markov_cycle,cost_lami)",
                     cost_drug = "cost_zido + cost_lami_cycle")

## -----------------------------------------------------------------------------
# Combine the health states
health_states <- combine_state(A,B,C,D)

#The current strategy ie. control or intervention - here it is combination therapy
comb_strategy <- strategy(tm, health_states, "comb")


## -----------------------------------------------------------------------------
comb_markov <- markov_model(comb_strategy, 20, c(1, 0,0,0),discount = c(0.06,0.0),comb_params)

## -----------------------------------------------------------------------------
min_values <- define_parameters(cost_direct_med_B = 177.4, 
                                cost_comm_care_C = 205.9)
max_values <- define_parameters(cost_direct_med_B = 17740, 
                                cost_comm_care_C = 20590)
param_table <- define_parameters_sens_anal(mono_params, min_values, 
                                           max_values)

result_dsa_control <- do_sensitivity_analysis(mono_markov,param_table)
report_dsa_control <- report_sensitivity_analysis(result_dsa_control)
plot_dsa(result_dsa_control,"cost")
plot_dsa(result_dsa_control,"cost_direct_med_B",type = "range")
plot_dsa(result_dsa_control,"utility",type = "range")


## -----------------------------------------------------------------------------
min_values <- define_parameters(cost_direct_med_B = 177.4, 
                                cost_comm_care_C = 205.9)
max_values <- define_parameters(cost_direct_med_B = 17740, 
                                cost_comm_care_C = 20590)
param_table <- define_parameters_sens_anal(comb_params, min_values, max_values)
result_dsa_treat <- do_sensitivity_analysis(comb_markov, param_table)
table_sa <- report_sensitivity_analysis(result_dsa_control, 
                                        result_dsa_treat, 1000, "mono")

## -----------------------------------------------------------------------------
plot_dsa(result_dsa_control,"cost", type = "range",result_dsa_treat, 
         threshold = 1000, comparator = "mono")

## -----------------------------------------------------------------------------
plot_dsa(result_dsa_control,"ICER", type = "range",result_dsa_treat, 
         threshold = 1000, comparator = "mono")

## -----------------------------------------------------------------------------
plot_dsa(result_dsa_control,"cost", type = "difference", result_dsa_treat, 
         threshold = 1000, comparator = "mono")

