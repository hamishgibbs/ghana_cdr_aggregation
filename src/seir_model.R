require(tidyverse)
require(SimInf)
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

calculate_beta_from_R_given_gamma(3)

run_seir_model <- function(mobility_type, 
                           infected_location, 
                           population,
                           events,
                           R0){
  
  S <- population
  E <- rep(0, length(population))
  
  I <- rep(0, length(population))
  # Assign n individuals to one introduction location
  I[infected_location] <- 1000
  
  R <- rep(0, length(population))
  
  events <- events
  
  events_test <- events %>% 
    mutate(select = 2)
  
  model <- SEIR(u0 = data.frame(S = S,
                                E = E,
                                I = I,
                                R = R),
                tspan = 1:max(events$time),
                events=events_test,
                beta = calculate_beta_from_R_given_gamma(R0), # 0.147,
                epsilon = 1/5.2,
                gamma = 1/12.39)
  
  return(model)
  
}


mobility_type <- "sequential" # sequential all_pairs

population <- read_rds("data/modelling/population.rds")
pcods <- read_rds("data/modelling/recoded_pcod.rds")
events <- read_rds(paste0("data/modelling/", mobility_type, "_events.rds"))

introduction_pcods <- c("fid029", "fid240", "fid207")

introduction_pcods <- pcods[which(names(pcods) %in% introduction_pcods)]

write_rds(introduction_pcods, "output/modelling/introduction_pcods.rds")

for (R0 in c(1.1, 1.5, 3.0)){
  for(mobility_type in c("all_pairs", "sequential")){
    for (i in introduction_pcods){
      print(paste0("Introduction: ", i, " Mobility: ", mobility_type, " R0: ", R0))
      model <- run_seir_model(mobility_type=mobility_type,
                               infected_location=i,
                               population=population,
                               events=events,
                               R0=R0)
      result <- run(model)
      write_rds(trajectory(result), paste0("output/modelling/preliminary/", 
                                           mobility_type, "/infected_", i,
                                           "_R0_", R0, "_trajectory.rds"))
      
    } 
  }
}

