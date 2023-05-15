suppressPackageStartupMessages({
  require(sf)
  require(tidyverse)
  require(data.table)
})

if (interactive()){
  .args <- c(
    "data/geo/admin2.geojson",
    list.files("data/epi_modelling/results/peak_difference", 
               pattern = ".csv",
               full.names = T),
    "output/figures/R0_1.5_time_delay_all_locs.png"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

a2 <- st_read(.args[1], quiet=T) %>% 
  mutate(geometry = st_make_valid(geometry)) %>% 
  st_simplify(dTolerance = 100, preserveTopology = T)

peaks_time_difference <- do.call(rbind, lapply(.args[2:(length(.args)-1)], fread))

peaks_time_difference$mobility_model_type <- factor(peaks_time_difference$mobility_model_type, 
                                      levels = c("gravity_exp", "gravity_power", "radiation_basic"),
                                      labels = c("Gravity (Exponential)", "Gravity (Power)", "Radiation"))

model_peaks_time_differences <- peaks_time_difference %>% 
  group_by(R0) %>% 
  group_split()

plot_peak_time_differences_by_r0 <- function(data, facet_var, title, fill_breaks=100){
  data <- data %>% 
    left_join(a2 %>% select(-centroid), by = c("introduction_location" = "pcod")) %>% 
    mutate(R0 = factor(R0, levels=c("3", "1.5", "1.25"), labels=c("R=3", "R=1.5", "R=1.25"))) %>% 
    st_as_sf() 
  
  data %>% 
    ggplot() + 
    geom_sf(aes(fill = peak_difference), size=0, color="black") + 
    facet_wrap(data[[facet_var]]) +
    colorspace::scale_fill_continuous_diverging("Purple-Green",
                                                breaks=seq(floor(min(data$peak_difference)/fill_breaks)*fill_breaks, ceiling(max(data$peak_difference)/fill_breaks)*fill_breaks, by = fill_breaks)) + 
    theme_void() + 
    labs(fill="Epidemic\nPeak Delay\n(Days)", title=title)
}

fill_breaks <- list(
  `3` = 200,
  `1.5` = 50,  
  `1.25` = 50
)

for (model_data in model_peaks_time_differences){
  p <- plot_peak_time_differences_by_r0(data=model_data, facet_var="mobility_model_type", 
                                        title=NULL, 
                                        fill_breaks = as.numeric(fill_breaks[unique(model_data$R0)]))
  
  out_fn <- paste0("output/figures/", unique(model_data$R0), "_time_delay_all_locs.png")
  
  ggsave(out_fn,
         p,
         width=10, height=5, units="in")
  
}

all_model_peak_data <- peaks_time_difference %>% 
  filter(R0 == 1.5)

p <- plot_peak_time_differences_by_r0(data=all_model_peak_data, facet_var="mobility_model_type", title="")

ggsave(tail(.args, 1),
       p,
       width=10, height=5, units="in")

