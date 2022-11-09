suppressPackageStartupMessages({
  require(tidyverse)
  require(sf)
  require(cowplot)
})

if(interactive()){
  .args <-  c("data/mobility_modelling/mobility_model_predictions.csv",
              "output/figures/movement_kernel_comparison.png")
} else {
  .args <- commandArgs(trailingOnly = T)
}

kernels <- read_csv(.args[1], col_types = cols())

kernels$model <- factor(kernels$model, levels = c("empirical", "gravity_exp", "gravity_power", "radiation_basic"),
                        labels = c("Empirical", "Gravity (Exponential)", "Gravity (Power)", "Radiation"))
kernels$network_type <- factor(kernels$network_type, levels = c("all_pairs", "sequential"),
                        labels = c("All Pairs", "Sequential"))

p <- kernels %>% 
  ggplot() + 
  geom_jitter(aes(x = distance, y = value, color=network_type), size = 0.01) +
  geom_ribbon(aes(x = distance, xmin = distance, xmax = distance, y = value, ymax = value, ymin = value, 
                  fill=network_type)) +
  scale_y_continuous(trans="log10", labels = scales::comma) +
  scale_x_continuous(trans="log10") +
  scale_color_manual(values = c("blue", "red")) + 
  scale_fill_manual(values = c("blue", "red")) + 
  facet_wrap(~model) + 
  theme_classic() + 
  labs(x="Distance (km)", y = "Travellers", color=NULL, fill=NULL)

ggsave(tail(.args, 1),
       p,
       width=10, height=6, units="in")
