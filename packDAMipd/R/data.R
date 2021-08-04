#' cost matrix
#' @format A 11 by 2 dataframe
#' @source  created on Nov 26, 2019 from
#' tmat <- rbind(c(1, 2), c(3, 4))
#' colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
#' tm <- transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
#' a <- health_state("Healthy", 1, 1, FALSE)
#' b <- health_state("Dead", 1, 0, TRUE)
#' health_states <- combine_state(a, b)
#' this.strategy <- strategy(tm, health_states, "intervention")
"cost_data"

#' utility matrix
#' @format A 11 by 2 dataframe
#' @source  created on Nov 26, 2019 from
#' tmat <- rbind(c(1, 2), c(3, 4))
#' colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
#' tm <- transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
#' a <- health_state("Healthy", 1, 1, FALSE)
#' b <- health_state("Dead", 1, 0, TRUE)
#' health_states <- combine_state(a, b)
#' this.strategy <- strategy(tm, health_states, "intervention")
"utility_data"

#' Trace matrix
#' @format A 11 by 2 dataframe
#' @source  created on Nov 26, 2019 from
#' tmat <- rbind(c(1, 2), c(3, 4))
#' colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
#' tm <- transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
#' a <- health_state("Healthy", 1, 1, FALSE)
#' b <- health_state("Dead", 1, 0, TRUE)
#' health_states <- combine_state(a, b)
#' this.strategy <- strategy(tm, health_states, "intervention")
"trace_data"

#' Parameter table created
#' @format A 11 by 2 dataframe
#' @source  created on Jan 15, 2020
"table_param"

#' Example trial data
#' @format A 31 by 33 dataframe
#' @source  created on Jan 15, 2020
"trial_data"

#' adl_scoring table
#' @format A 41 by 3 dataframe
#' @source  created on Jan 15, 2020
"adl_scoring"

#' promis 3a scoring table
#' @format A 14 by 3 dataframe
#' @source  created on April 08, 2021
"promis3a_scoring"

#' Parameter table created
#' @format A 2 column 1 observation
#' @source  created on September 5, 2020
"blank"
