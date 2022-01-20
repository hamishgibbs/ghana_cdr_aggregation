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

model <- SEIR(u0 = data.frame(S = rep(99, 10),
                              E = rep(0, 10),
                              I = rep(1, 10),
                              R = rep(0, 10)),
             tspan = 1:200,
             beta = 0.16,
             epsilon = 0.25,
             gamma = 0.077)

result <- run(model)

plot(result)

u0 <- data.frame(S = c(100, 0), I = c(100, 0), R = c(100, 0))

events <- data.frame(event = rep("extTrans", 300),
                     time = 1:300,
                     node = 1,
                     dest = 2,
                     n = 0,
                     proportion = 0.01,
                     select = 4,
                     shift = 0)

model <- SIR(u0=u0,
             tspan = 1:300,
             events = events,
             beta = 0,
             gamma = 0)

plot(run(model), index = 1:2, range = FALSE)
model@events@E[2, 4] <- 2
plot(run(model), index = 1:2, range = FALSE)
model@events@E[2, 4] <- 10
