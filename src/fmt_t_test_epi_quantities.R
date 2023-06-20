suppressPackageStartupMessages({
  require(data.table)
})

if (interactive()){
  .args <- c(
    "data/epi_modelling/results/t_test_epi_quantities.csv",
    "output/fmt_t_test_epi_quantities.csv"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}


t_test_res <- fread(.args[1])

t_test_res <- t_test_res[order(variable)]

t_test_res[, R0 := factor(R0, levels = rev(c("3", "1.5", "1.25")))]
t_test_res[, mobility_model_type := factor(mobility_model_type, 
                                     levels = c("gravity_exp", 
                                                "gravity_power", 
                                                "radiation_basic"),
                                     labels = c("Gravity (Exponential)",
                                                "Gravity (Power)",
                                                "Radiation (Basic)"))]
t_test_res[, annotation := ifelse(p_value <= 0.0001, "*", "○")]
x_nudge <- 0.2
t_test_res[t_test_res[, .(annotation_x = mean(ci_upper) * x_nudge), by=c("variable", "mobility_model_type")], on = c("variable", "mobility_model_type"), annotation_x := annotation_x]

plot_epi_quantity <- function(df, x_lab, title){
  ggplot(data=df) + 
    geom_segment(aes(y = R0, 
                     yend = R0, 
                     x = ci_lower, 
                     xend = ci_upper,
                     color = R0),
                 size=5, 
                 alpha=0.85) + 
    #geom_text(aes(y = R0, x = ci_upper + annotation_x, label=annotation), 
    #          position = position_nudge(y = 0.1)) +
    facet_wrap(~mobility_model_type, scales="free_x") + 
    scale_color_manual(values = c("#33a02c", "#1f78b4", "#e31a1c")) + 
    theme_classic() + 
    labs(
      x = x_lab,
      y = expression(R[0]),
      title = title
    ) + 
    theme(legend.position = "none")
}

p <- list()
p[[1]] <- plot_epi_quantity(subset(t_test_res, variable == 'peak_time'),
                  x_lab = "Days",
                  title="Epidemic peak timing")
p[[2]] <- plot_epi_quantity(subset(t_test_res, variable == 'peak_size'),
                            x_lab = "Infections",
                            title="Epidemic peak size")
p[[3]] <- plot_epi_quantity(subset(t_test_res, variable == 'national_epi_length'),
                            x_lab = "Days",
                            title="Epidemic length")
p[[4]] <- plot_epi_quantity(subset(t_test_res, variable == 'avg_arrival_1'),
                            x_lab = "Days",
                            title=expression("Average time of 1"^st~"infection"))
p[[5]] <- plot_epi_quantity(subset(t_test_res, variable == 'avg_arrival_5'),
                            x_lab = "Days",
                            title=expression("Average time of 5"^th~"infection"))

p <- cowplot::plot_grid(plotlist = p, 
                   nrow=5)

ggsave("output/figures/epi_quantities_comparison.png",
       p,
       width=10, 
       height=15, 
       units="in")
  
  
ci_precision <- 2
p_value_precision <- 4

t_test_res[, value := paste0(
  round(ci_lower, ci_precision), " to ", 
  round(ci_upper, ci_precision), 
  " (", round(p_value, p_value_precision), ")")]

t_test_res$value[grepl("NA", t_test_res$value)] <- NA
t_test_res$value <- gsub("[(]0[)]", "(<0.0001)", t_test_res$value)

fmt <- list()

for (variable_val in unique(t_test_res$variable)){
  df <- subset(t_test_res, variable == variable_val)
  df_fmt <- dcast(df, R0 ~ mobility_model_type, value.var = "value")
  df_fmt$variable <- variable_val
  fmt[[variable_val]] <- df_fmt
}

fwrite(do.call(rbind, fmt), tail(.args, 1))
