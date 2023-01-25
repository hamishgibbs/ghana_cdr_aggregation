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

p <- list()

for (i in 1:length(unique(kernels$model))){
  title <- letters[i]
  focus_model <- levels(kernels$model)[i]
  p[[i]] <- subset(kernels, model == focus_model) %>% 
    ggplot() + 
    geom_jitter(aes(x = distance, y = value, color=network_type), size = 0.01) +
    geom_ribbon(aes(x = distance, xmin = distance, xmax = distance, y = value, ymax = value, ymin = value, 
                    fill=network_type)) +
    scale_y_continuous(trans="log10", labels = scales::comma) +
    scale_x_continuous(trans="log10", limits = c(1, max(kernels$distance))) +
    scale_color_manual(values = c("blue", "red")) + 
    scale_fill_manual(values = c("blue", "red")) + 
    theme_classic() + 
    theme(strip.background = element_blank(),
          strip.text = element_text(size=13)) + 
    labs(title = title, subtitle = focus_model, x="Distance (km)", y = "Travellers", color=NULL, fill=NULL)
}

p <- cowplot::plot_grid(plotlist = p)

ggsave(tail(.args, 1),
       p,
       width=12, height=7, units="in")


