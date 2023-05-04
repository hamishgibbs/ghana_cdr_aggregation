suppressPackageStartupMessages({
    require(readr)
    require(data.table)
})

if (interactive()){
  .args <- c(
    "data/epi_modelling/results/gravity_exp/all_pairs/R0_3.0/infected_fid029_trajectory_0.rds",
    "data/epi_modelling/results_slim/gravity_exp.all_pairs.R0_3.0.infected_fid029.trajectory_0.rds"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

model_trajectory <- data.table(read_rds(.args[1]))

sim_cols <- c("mobility_model_type", "mobility_network_type", "introduction_location", "R0", "sample")

# Introduction of first case
disease_introduction_1 <- subset(model_trajectory, I >= 1)
disease_introduction_1 <- disease_introduction_1[order(time)]
disease_introduction_1 <- disease_introduction_1[, head(.SD, 1), by=c("node", sim_cols)]

# Introduction of first 5 cases
disease_introduction_5 <- subset(model_trajectory, I >= 5)
disease_introduction_5 <- disease_introduction_5[order(time)]
disease_introduction_5 <- disease_introduction_5[, head(.SD, 1), by=c("node", sim_cols)]

intro_cols <- c("node", "time", "I", sim_cols)
disease_introduction_1 <- disease_introduction_1[, ..intro_cols]
disease_introduction_5 <- disease_introduction_5[, ..intro_cols]

# National epi
national_epi <- model_trajectory[, .(S = sum(S), I = sum(I), E = sum(E), R = sum(R)), by = c("time", sim_cols)]

res <- list(
  intro_1 = disease_introduction_1,
  intro_5 = disease_introduction_5,
  national_epi = national_epi
)

write_rds(res, tail(.args, 1))
