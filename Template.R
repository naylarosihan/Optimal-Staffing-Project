library(MASS)
library(tidyverse)
library(DescTools) #for function `GeomSn` to calculate a summation of geometric series
set.seed(1)

#Set the simulation time
simulation_time <- 12 * 12

#Set given values
analyst_demand <-  c(95, 75, 70, 70, 110, 105, 90, 65, 80, 90, 120, 105)
month <- c("MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "JAN", "FEB", "MAR", "APR")
analyst_data <- data.frame(cbind(month, analyst_demand))

#Initialise the variables
P <- c(63, rep(0, simulation_time))
D <- c(0, rep(0, simulation_time))
month <- c(4, numeric(simulation_time))
Q_list <- c(0, rep(0, simulation_time))
j = 1

#recruiting once a year in July
##period of recruitment = annually
t = 12
##when to recruit
recruit_month <- c(7)

##run simulation
for (i in 1:simulation_time){
  #index for the list
  index <- (i - 1) %% 12 + 1
  #month number (i.e., 1 = JAN, 2 = FEB, ..., 12 = DEC)
  month_number <- (i + 3) %% 12 + 1 -> month[i + 1]
  prev_month_number <- (i + 2) %% 12 + 1
  #random variables
  X <- rnorm(1, 0, 0.05)
  Yi <- rnorm(1, 0, 0.1)
  if (prev_month_number %in% c(9, 1)){
    R <- runif(1, 0.8, 1)
  } else if (prev_month_number %in% c(5, 6, 7, 8)){
    R <- runif(1, 0.95, 1)
  } else {
    R <- runif(1, 0.9, 1)
  } 
  #get and calculate values
  Hi <- analyst_data[index, 2]
  Di <- as.numeric(Hi) * (1 + X) * (1 + Yi)
  D[i + 1] <- Di
  P_prev <- P[i]
  if (month_number %in% recruit_month){
    #define Q
    Q <- ifelse(j == 1, 98, ((mean(D[(i-t+1):i]) / (GeomSn(1, 0.95, t-1)/t)) - (P[(i-4)] * (0.95**(recruit_month-4)))) / 0.70) -> Q_list[i+1]
    Ai <- rbinom(1, ceiling(Q), 0.70) #to get an integer for Q
    Pi <- P_prev * R + Ai
    j = j + 1
  } else {
    Pi <- P_prev * R
    #--Q <- 0
  }  
  P[i + 1] <- Pi
  #--cat("i:",i, "Q:", Q, "Ai:", Ai, "P_prev:", P_prev, "Pi:", Pi, "Di:", Di, "Hi:", Hi, "\n")
}
##get the results
scenario_1 <- data.frame(cbind(month, P, D, Q_list)) |>
  mutate(PminusD = P - D) |>
  mutate(Ei = ifelse(PminusD <= 0, 400 * D + 3600 * P, 10000 * D - 6000 * P))
sum_E_scenario_1 <- numeric(simulation_time + 1)
for (i in 1:((simulation_time + 1) / t)){
  j = 1 + (i - 1) * t
  sum_E_scenario_1[j+t] <- sum(E <- scenario_1$Ei[(j+1):(j+t)])
}
scenario_1 <- scenario_1 |>
  mutate(annual_E = sum_E_scenario_1)