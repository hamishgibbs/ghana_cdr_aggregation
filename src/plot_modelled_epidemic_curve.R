suppressPackageStartupMessages({
  require(data.table)
  require(ggplot2)
  require(sf)
})

if (interactive()){
  .args <- c(
    "data/geo/pcods_admin2.csv",
    "data/geo/admin2_simplified.geojson",
    list.files("data/epi_modelling/results/R_cumulative_quantiles", 
               pattern = ".csv",
               full.names = T),
    "output/figures/cumulative_R_all_intros_p1.png",
    "output/figures/cumulative_R_all_intros_p2.png",
    "output/figures/cumulative_R_subset_intros.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

.outputs <- tail(.args, 3)

pcods_a2 <- read.csv(.args[1], header = F, col.names = c("pcod", "name2"))

a2 <- st_read(.args[2])

r_q <- do.call(rbind, lapply(.args[3:(length(.args)-3)], fread))

r_q[pcods_a2, on = c("introduction_location" = "pcod"), name2 := name2]

r_q[, R0 := factor(R0, levels = c("3", "1.5", "1.25"))]
r_q[, mobility_model_type := factor(mobility_model_type, 
                                    levels = c("gravity_exp", 
                                               "gravity_power", 
                                               "radiation_basic"),
                                    labels = c("Gravity (Exponential)",
                                               "Gravity (Power)",
                                               "Radiation (Basic)"))]

plot_seir_cumulative_R <- function(df){
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
      y = "Cumulative number recovered",
      x = "Time") + 
    guides(color="none") + 
    scale_fill_manual(values = c("#33a02c", "#1f78b4", "#e31a1c")) + 
    theme(
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(size = 8)
    )
  
}

r_q$name2 <- gsub("AMA-", "", r_q$name2)
r_q$name2 <- gsub("KMA-", "", r_q$name2)
r_q$name2 <- gsub("TM-", "", r_q$name2)
r_q$name2 <- stringr::str_to_title(r_q$name2) 

r_q <- r_q[order(name2)]

# Paginate this
p_all_intro <- plot_seir_cumulative_R(df = subset(r_q, name2 %in% unique(r_q$name2)[1:10]))

ggsave(.outputs[1],
       p_all_intro,
       width=10, height=10, units="in")  

p_all_intro <- plot_seir_cumulative_R(df = subset(r_q, name2 %in% unique(r_q$name2)[11:20]))

ggsave(.outputs[2],
       p_all_intro,
       width=10, height=10, units="in")  

focus_names <- c(
  "Lawra", "Tamale South", "Nkwanta South", "Manhyia South", "Okaikoi South"
)

r_q_subset <- subset(r_q, name2 %in% focus_names)

r_q_subset[, name2 :=  factor(name2, levels = focus_names)]

p_subset_intros <- plot_seir_cumulative_R(df = r_q_subset) + 
  theme(strip.text = element_text(size=5),
        legend.position = "bottom")

a2$intro <- a2$pcod %in% unique(r_q_subset$introduction_location)

a2_sub <- subset(a2, intro == T)
a2_sub <- st_centroid(a2_sub)
a2_sub$x <- st_coordinates(a2_sub)[,1]
a2_sub$y <- st_coordinates(a2_sub)[,2]
a2_sub <- as.data.table(st_drop_geometry(a2_sub))
a2_sub[r_q_subset[, .(n=.N), by=c("introduction_location", "name2")], on = c("pcod"="introduction_location"), name2 := name2]
a2_sub$name2 <- stringr::str_to_title(a2_sub$name2) 

p_map <- ggplot(data = a2) + 
  geom_sf(aes(fill = intro), size=0.2, color='black') + 
  ggrepel::geom_label_repel(data=a2_sub, aes(x = x, y = y, label = name2),
                            color="black",
                            nudge_y = 0.5) + 
  theme_void() + 
  scale_fill_manual(values = c("#EEEEEE", "red")) + 
  theme(legend.position = "none")

p <- cowplot::plot_grid(p_map,
                   p_subset_intros,
                   rel_widths = c(0.35, 0.65))

ggsave(.outputs[3],
       p,
       width=11, height=6, units="in")  

