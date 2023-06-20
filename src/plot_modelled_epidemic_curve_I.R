suppressPackageStartupMessages({
  library(ggplot2)
  library(data.table)
})

if (interactive()){
  .args <- c(
    "data/geo/pcods_admin2.csv",
    list.files("data/epi_modelling/results/I_quantiles", 
               pattern = ".csv",
               full.names = T),
    "output/figures/cumulative_I_all_intros_p1.png",
    "output/figures/cumulative_I_all_intros_p2.png",
    "output/figures/cumulative_I_subset_intros.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

.outputs <- tail(.args, 3)

pcods_a2 <- read.csv(.args[1], header = F, col.names = c("pcod", "name2"))

traj <- do.call(rbind, lapply(.args[2:(length(.args)-3)], fread))

traj[pcods_a2, on=c("introduction_location" = "pcod"), name2 := name2]
traj$name2 <- gsub("AMA-", "", traj$name2)
traj$name2 <- gsub("KMA-", "", traj$name2)
traj$name2 <- gsub("TM-", "", traj$name2)
traj$name2 <- stringr::str_to_title(traj$name2) 
traj[, R0 := factor(R0, levels = c("3", "1.5", "1.25"))]
traj[, mobility_model_type := factor(mobility_model_type, 
                                    levels = c("gravity_exp", 
                                               "gravity_power", 
                                               "radiation_basic"),
                                    labels = c("Gravity (Exponential)",
                                               "Gravity (Power)",
                                               "Radiation (Basic)"))]
plot_seir_I <- function(df){
  ggplot(data = df) + 
    geom_ribbon(aes(x = time, 
                    ymin = X5., ymax = X95.,
                    fill = as.character(R0),
                    group = paste0(R0, mobility_network_type)),
                alpha=0.2) + 
    geom_ribbon(aes(x = time, 
                    ymin = X20., ymax = X80.,
                    fill = as.character(R0),
                    group = paste0(R0, mobility_network_type)),
                alpha=0.2) + 
    geom_ribbon(aes(x = time, 
                    ymin = X40., ymax = X60.,
                    fill = as.character(R0),
                    group = paste0(R0, mobility_network_type)),
                alpha=0.2) + 
    geom_path(aes(x = time, y = X50.,
                  linetype = mobility_network_type,
                  group = paste0(R0, mobility_network_type)),
              size=0.3) + 
    theme_classic() + 
    facet_grid(name2~mobility_model_type) + 
    scale_y_continuous(labels = scales::comma) +
    labs(
      fill = "R0 Value",
      linetype = "Aggregation Type",
      y = "Number of infections",
      x = "Time") + 
    guides(color="none") + 
    scale_fill_manual(values = c("#33a02c", "#1f78b4", "#e31a1c")) + 
    theme(
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(size = 8)
    )
}

traj <- traj[order(name2)]

# Paginate this
p_all_intro <- plot_seir_I(df = subset(traj, name2 %in% unique(traj$name2)[1:10]))

ggsave(.outputs[1],
       p_all_intro,
       width=10, height=10, units="in")  

p_all_intro <- plot_seir_I(df = subset(traj, name2 %in% unique(traj$name2)[11:20]))

ggsave(.outputs[2],
       p_all_intro,
       width=10, height=10, units="in")  

focus_names <- c(
  "Lawra", "Tamale South", "Nkwanta South", "Manhyia South", "Okaikoi South"
)

traj_subset <- subset(traj, name2 %in% focus_names)

traj_subset[, name2 :=  factor(name2, levels = focus_names)]

p_subset_intros <- plot_seir_I(df = traj_subset) + 
  theme(strip.text = element_text(size=5),
        legend.position = "bottom")

ggsave(.outputs[3],
       p_subset_intros,
       width=11, height=6, units="in")  