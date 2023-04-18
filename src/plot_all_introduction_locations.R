suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
})

if (interactive()){
  .args <- c(
    "data/geo/admin2.geojson",
    "data/mobility_modelling/peak_time_differences.csv",
    ""
  )
  MOBILITY_MODEL_TITLE <- "Gravity Model (Exponential)"
  TIME_DIFFERENCE_COLOR_BREAKS <- as.numeric(stringr::str_split("-Inf,-365,-90,-30,-14,-11,-7,0,7,14,30,90,Inf", pattern=",")[[1]])
} else {
  .args <- commandArgs(trailingOnly = T)
  MOBILITY_MODEL_TITLE <- Sys.getenv("MOBILITY_MODEL_TITLE")
  TIME_DIFFERENCE_COLOR_BREAKS <- as.numeric(stringr::str_split(Sys.getenv("TIME_DIFFERENCE_COLOR_BREAKS"), pattern=",")[[1]])
}

a2 <- st_read(.args[1], quiet=T) %>% 
  mutate(geometry = st_make_valid(geometry)) %>% 
  st_simplify(dTolerance = 100, preserveTopology = T)
peaks_time_difference <- read_csv(.args[2], col_types = cols())

p_breaks <- peaks_time_difference %>% 
  ggplot() + 
  geom_density(aes(x = time_difference, color=model)) +
  geom_vline(aes(xintercept=0), color="black", linetype="dashed",size=0.2) + 
  theme_classic() + 
  labs(y="Density", x="Time Difference", color="Model") + 
  theme(legend.position = c(0.2, 0.7))

p_breaks_annotated <- p_breaks +
  geom_vline(data = data.frame(TIME_DIFFERENCE_COLOR_BREAKS), 
             aes(xintercept=TIME_DIFFERENCE_COLOR_BREAKS),
             color="red")

ggsave(gsub("figures/", "figures/validation/", tail(.args, 1)),
       p_breaks,
       width=10, height=5.5, units="in")  
ggsave(gsub("figures/", "figures/validation/annotated_", tail(.args, 1)),
       p_breaks_annotated,
       width=10, height=5.5, units="in")  

peaks_time_difference <- peaks_time_difference %>% 
  mutate(value_reclass = NA,
         value_reclass = ifelse(time_difference == 0, "0 Days", value_reclass),
         value_reclass = ifelse(time_difference > 0, "1 to 7 Days", value_reclass),
         value_reclass = ifelse(time_difference < 0, "-1 to -7 Days", value_reclass),
         value_reclass = ifelse(time_difference > 7, "8 to 30 Days", value_reclass),
         value_reclass = ifelse(time_difference < 7, "-8 to -30 Days", value_reclass),
         value_reclass = ifelse(time_difference > 30, "> 30 Days", value_reclass),
         value_reclass = ifelse(time_difference < -30, "< -30 Days", value_reclass))

peaks_time_difference$value_reclass <- factor(peaks_time_difference$value_reclass,
                                              levels = c(
                                                "< -30 Days", 
                                                "-8 to -30 Days", 
                                                "-1 to -7 Days", 
                                                "0 Days",
                                                "1 to 7 Days",
                                                "8 to 30 Days",
                                                "> 30 Days"))

peaks_time_difference$model_display <- factor(peaks_time_difference$model, 
                                      levels = c("gravity_exp", "gravity_power", "radiation_basic"),
                                      labels = c("Gravity (Exponential)", "Gravity (Power)", "Radiation"))

pal <- rev(c('#b2182b','#ef8a62','#fddbc7','#f7f7f7','#d1e5f0','#67a9cf','#2166ac'))
names(pal) <- levels(peaks_time_difference$value_reclass)

model_peaks_time_differences <- peaks_time_difference %>% 
  group_by(model) %>% 
  group_split()

plot_peak_time_differences_by_r0 <- function(data, facet_var, title){
  data <- data %>% 
    select(-all_pairs, -sequential) %>% 
    left_join(a2 %>% select(-centroid), by = c("introduction_location" = "pcod")) %>% 
    mutate(R0 = factor(R0, levels=c("3", "1.5", "1.25"), labels=c("R=3", "R=1.5", "R=1.25"))) %>% 
    st_as_sf() 
  
  data %>% 
    ggplot() + 
    geom_sf(aes(fill = value_reclass), size=0.1, color="black") + 
    scale_fill_manual(values = pal) + 
    facet_wrap(data[[facet_var]]) +
    theme_void() + 
    labs(fill="Epidemic\nPeak Delay\n(Days)", title=title)
}


for (model_data in model_peaks_time_differences){
  p <- plot_peak_time_differences_by_r0(data=model_data, facet_var="R0", title=unique(model_data$model_display))

  out_fn <- paste0("output/figures/", unique(model_data$model), "_time_delay_all_locs.png")
  
  ggsave(out_fn,
         p,
         width=10, height=5, units="in")
  
}

all_model_peak_data <- peaks_time_difference %>% 
  filter(R0 == 1.5)

p <- plot_peak_time_differences_by_r0(data=all_model_peak_data, facet_var="model_display", title="")

ggsave("output/figures/R0_1.5_time_delay_all_locs.png",
       p,
       width=10, height=5, units="in")

