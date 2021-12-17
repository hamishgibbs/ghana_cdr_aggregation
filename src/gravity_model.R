require(mobility)

population <- read_csv("data/population/population_admin2.csv")
distance_matrix <- read_rds("data/distance/distance_matrix_admin2.rds")

all_pairs <- read_csv("data/networks/all_pairs_admin2.csv")
sequential <- read_csv("data/networks/sequential_admin2.csv")

model_inputs <- list()

model_inputs$N <- population$population
names(model_inputs$N) <- population$pcod2

model_inputs$D <- distance_matrix

model_inputs$M <- reshape2::acast(all_pairs, pcod_from~pcod_to, value.var="value_mean")

model_input_data <- mobility::mobility_matrices

model_input_data$M
model_input_data$D
model_input_data$N

mod <- mobility(data=model_inputs,
                model='gravity',
                type='basic',
                n_chain=4,
                n_burn=2500,
                n_samp=10000,
                n_thin=2,
                DIC=TRUE)

summary(mod, probs=c(0.025, 0.975), ac_lags=10)
check(mod)

M <- predict(mod)

M
