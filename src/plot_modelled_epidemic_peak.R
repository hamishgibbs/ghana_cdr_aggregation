suppressPackageStartupMessages({
  require(tidyverse)
})

source("src/utils/mobility_type_scales.R")

if (interactive()){
  .args <- c(
    "data/geo/pcods_admin2.csv",
    "data/population/population_admin2.csv",
    "data/epi_modelling/results/gravity_exp/focus_locs_results_national_peaks.csv",
    "output/figures/gravity_exp/peak_infected_proportion_boxplot.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

pcods_a2 <- read.csv(.args[1], header = F, col.names = c("pcod", "name2"))

population <- read_csv(.args[2], col_types=cols())

national_peaks <- read_csv(.args[3], col_types=cols())

national_peaks_named <- national_peaks %>% 
  left_join(pcods_a2, by=c("introduction_location" = "pcod")) %>% 
  mutate(introduction_location = name2) %>% 
  select(-name2) %>% 
  mutate(introduction_location = factor(introduction_location, 
                                        levels = c("AMA-OKAIKOI SOUTH", 
                                                   "KMA-MANHYIA SOUTH", 
                                                   "TM-TAMALE SOUTH",
                                                   "LAWRA",
                                                   "NKWANTA SOUTH")),
         R0 = factor(R0, levels=c("3", "1.5", "1.25"), labels=c("R=3", "R=1.5", "R=1.25")))

national_peaks_named %>% 
  ggplot() + 
  geom_violin(aes(x = time, y = mobility_network_type)) + 
  facet_grid(R0 ~ introduction_location, scales="free_x") 


ggsave(tail(.args, 1),
       p_trajectory_curve,
       width=10, height=5, units = "in")

# also do a comparison of the infection tree (is this possible?)
