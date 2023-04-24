suppressPackageStartupMessages({
  require(tidyverse)
  require(SimInf)
})

# Inputs: 
# S: vector of starting population sizes (-1 in infection location)
# E: vector of zeros (length of destinations)
# I: vector of infected individuals (defined introduction location)
# R: vector of zeros (length of destinations)
# tspan: vector of times for running simulation
# events: data.frame of "external transfer" events defining travel from origins to destinations (as an average proportion of the population)
# beta: transmission rate from susceptible to infected
# epsilon: incubation rate from exposed to infected
# gamma: recovery rate from infected to recovered

calculate_beta_from_R_given_gamma <- function(R0){
  gamma <- 1/12.39
  return (R0 * gamma)
}

run_seir_model <- function(infected_location, 
                           population,
                           events,
                           R0){
  
  S <- population
  E <- rep(0, length(population))
  
  I <- rep(0, length(population))
  
  # Assign n individuals to one introduction location
  I[infected_location] <- 100
  
  R <- rep(0, length(population))
  
  events <- events
  
  events_test <- events %>% 
    mutate(select = 2)
  
  model <- SEIR(u0 = data.frame(S = S,
                                E = E,
                                I = I,
                                R = R),
                tspan = sort(unique(events$time)),
                events=events_test,
                beta = calculate_beta_from_R_given_gamma(R0), # 0.147,
                epsilon = 1/5.2,
                gamma = 1/12.39)
  
  return(model)
  
}
