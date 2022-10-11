suppressPackageStartupMessages({
  require(tidyverse)
})

if(interactive()){
  .args <-  c("data/mobility_modelling/gravity_basic/all_pairs_model.rds",
              "data/mobility_modelling/gravity_exp/all_pairs_model.rds",
              "data/mobility_modelling/gravity_power/all_pairs_model.rds",
              "data/mobility_modelling/radiation_basic/all_pairs_model.rds",
              "data/mobility_modelling/gravity_basic/sequential_model.rds",
              "data/mobility_modelling/gravity_exp/sequential_model.rds",
              "data/mobility_modelling/gravity_power/sequential_model.rds",
              "data/mobility_modelling/radiation_basic/sequential_model.rds",
              "")
} else {
  .args <- commandArgs(trailingOnly = T)
}

all_pairs_models <- lapply(.args[1:4], read_rds)
sequential_models <- lapply(.args[5:8], read_rds)

format_comparison_table <- function(models, movement_type){
  model_comparison <- mobility::compare(models)
  model_comparison$movement_type <- rep(movement_type, 4)
  model_comparison$model <- stringr::str_to_title(model_comparison$model)
  model_comparison$type <- stringr::str_to_title(model_comparison$type)
  model_comparison$DIC <- scales::comma(model_comparison$DIC, accuracy=0.01)
  model_comparison$RMSE <- scales::comma(model_comparison$RMSE, accuracy=0.01)
  model_comparison$MAPE <- scales::percent(model_comparison$MAPE/100, accuracy=0.01)
  model_comparison$R2 <- round(model_comparison$R2, 2)
  return(model_comparison)
}

# display formatted table with model comparisons
model_comparison <- rbind(format_comparison_table(all_pairs_models, "All Pairs"),
      format_comparison_table(sequential_models, "Sequential"))

write_csv(model_comparison, tail(.args, 1))


# plots of observed / predicted for each model
plot_mobilty_model_obs_pred <- function(model){
  obs <- reshape2::melt(model$data$M)
  pred <- reshape2::melt(mobility::predict(model))
  
  model_title <- stringr::str_to_title(paste0(model$model, " (", model$type, ")"))
  
  p <- pred %>% 
    left_join(obs, by = c("Var1", "Var2")) %>% 
    ggplot() + 
    geom_point(aes(x = value.y, y = value.x), size=0.2) +
    geom_abline(size=0.2, linetype="dashed", color="red") + 
    scale_x_continuous(trans="log10", labels = scales::comma) + 
    scale_y_continuous(trans="log10", labels = scales::comma, limits = c(1, 10000000)) + 
    theme_classic() + 
    labs(x = bquote(Observed-Log[10]), y = bquote(Predicted-Log[10]), title=model_title)
  return(p)
}

p_all_pairs <- lapply(all_pairs_models, plot_mobilty_model_obs_pred)
p_sequential <- lapply(sequential_models, plot_mobilty_model_obs_pred)

p_all_pairs <- cowplot::plot_grid(plotlist = p_all_pairs, ncol=2, nrow=2)
p_sequential <- cowplot::plot_grid(plotlist = p_sequential, ncol=2, nrow=2)

all_pairs_title <- cowplot::ggdraw() + 
 cowplot::draw_label("a (All Pairs)", x = 0, hjust = 0
  ) + theme(plot.margin = margin(0, 0, 0, 7))
sequential_title <- cowplot::ggdraw() + 
  cowplot::draw_label("b (Sequential)", x = 0, hjust = 0
  ) + theme(plot.margin = margin(0, 0, 0, 7))

p_all_pairs <- cowplot::plot_grid(all_pairs_title, p_all_pairs, rel_heights = c(0.05, 0.95),
                   nrow=2)
p_sequential <- cowplot::plot_grid(sequential_title, p_sequential, rel_heights = c(0.05, 0.95),
                   nrow=2)

p <- cowplot::plot_grid(p_all_pairs, p_sequential, nrow=1)

ggsave("output/figures/mobility_model_comparison.png",
       p, width=10, height=5, units="in")

