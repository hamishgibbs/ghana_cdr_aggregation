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

run_seir_model <- function(mobility_type, 
                           infected_location, 
                           population,
                           events){
  
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
                beta = 0.147,
                epsilon = 1/5.2,
                gamma = 1/12.39)
  
  return(run(model))
  
}


mobility_type <- "sequential" # sequential all_pairs

population <- read_rds("data/modelling/population.rds")
pcods <- read_rds("data/modelling/recoded_pcod.rds")
events <- read_rds(paste0("data/modelling/", mobility_type, "_events.rds"))

# Reduce the number of introduction locations (every 6th)
pop_ranked_pcod <- tibble(pcod=names(pcods), pcod_i=pcods, population) %>% 
  arrange(-population) %>% pull(pcod_i)
pcod_i <- pop_ranked_pcod[seq(1, length(pop_ranked_pcod), 6)]

for(mobility_type in c("all_pairs", "sequential")){
  for (i in pcod_i){
    print(which(pcod_i==i))
    result <- run_seir_model(mobility_type=mobility_type,
                             infected_location=i,
                             population=population,
                             events=events)
    
    write_rds(trajectory(result), paste0("output/modelling/preliminary/", 
                                         mobility_type, "/infected_", i, 
                                         "_trajectory.rds"))
    
  } 
}


