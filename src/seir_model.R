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

n_patches <- read_rds("data/modelling/n_patches.rds")
n_dates <- read_rds("data/networks/n_intersecting_dates.rds")
n_dates <- 500

S <- read_rds("data/modelling/population.rds")
E <- rep(0, n_patches)
# For now - introduce one infected individual into each place
I <- rep(1000, n_patches)
R <- rep(0, n_patches)

events <- read_rds("data/modelling/all_pairs_events.rds")

# DEV: Should move comparison of methodologies to another file 
# this file should be for SEIR functions
# DEV: parameters should be modelled as distributions 

model <- SEIR(u0 = data.frame(S = S,
                              E = E,
                              I = I,
                              R = R),
             tspan = 1:n_dates,
             events=events,
             beta = 0.147,
             epsilon = 1/5.2,
             gamma = 1/12.39)

result <- run(model)

plot(result)

write_rds(trajectory(result), "output/modelling/preliminary/all_pairs_trajectory.rds")

