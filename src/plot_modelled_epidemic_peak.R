suppressPackageStartupMessages({
  require(tidyverse)
})

source("src/utils/mobility_type_scales.R")

if (interactive()){
  .args <- c(
    "data/modelling/recoded_pcod.rds",
    "data/geo/pcods_admin2.csv",
    "data/population/population_admin2.csv",
    list.files("output/modelling/preliminary/all_pairs", pattern="_sample_1_", full.names = T),
    list.files("output/modelling/preliminary/sequential", pattern="_sample_1_", full.names = T),
    "output/figures/peak_infected_proportion_boxplot.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

pcods <- read_rds(.args[1])
pcods_a2 <- read.csv(.args[2], header = F, col.names = c("pcod", "name2"))

population <- read_csv(.args[3])

pcod_lookup <- data.frame(pcod=names(pcods), pcod_numeric=pcods)

trajectories <- list()

trajectory_indices <- c(4:(length(.args) -1))

for (i in 1:length(trajectory_indices)){
  trajectories[[i]] <- read_rds(.args[trajectory_indices[i]]) %>% 
    select(node, time, I, mobility_type, R0, introduction_location)
}

trajectories <- do.call(rbind, trajectories)

trajectories_named <- trajectories %>% 
  left_join(pcods_a2, by=c("introduction_location" = "pcod")) %>% 
  mutate(introduction_location = name2) %>% select(-name2) %>% 
  mutate(introduction_location = factor(introduction_location, 
                                        levels = c("AMA-OKAIKOI SOUTH", 
                                          "KMA-MANHYIA SOUTH", 
                                          "TM-TAMALE SOUTH",
                                          "LAWRA",
                                          "NKWANTA SOUTH")),
         R0 = factor(R0, levels=c("3", "1.5", "1.25"), labels=c("R=3", "R=1.5", "R=1.25")))

trajectories_i_norm <- trajectories_named %>% 
  left_join(pcod_lookup, by = c("node" = "pcod_numeric")) %>% 
  left_join(population, by = c("pcod" = "pcod2")) %>% 
  mutate(value = I / population)

peak_i_norm <- trajectories_i_norm %>% 
  group_by(node, mobility_type, R0) %>% 
  top_n(1, wt=value)

plot_boxplot <- function(data, variable, ylab){
  return (data %>% 
    ggplot() + 
    geom_boxplot(aes(x = mobility_type, y = !!sym(variable), color=mobility_type), 
                 outlier.size=0.2, size=0.4) + 
    facet_grid(R0 ~ introduction_location, scales="free_y") + 
    theme_classic() + 
    theme(legend.position = "none",
          strip.background = element_blank()) + 
    mobility_type_color_scale + 
    labs(y=ylab, x=NULL)
    )
}

p_infected_proportion <- plot_boxplot(peak_i_norm, "value", ylab="Peak proportion infected") +
  scale_y_continuous(labels = scales::percent)
p_peak_timing <- plot_boxplot(peak_i_norm, "time", ylab="Timing of Peak infection")

ggsave(tail(.args, 1),
       p_infected_proportion,
       width=10, height=5.5, units="in")  

ggsave(gsub("infected_proportion", "timing", tail(.args, 1)),
       p_peak_timing,
       width=10, height=5.5, units="in")  
