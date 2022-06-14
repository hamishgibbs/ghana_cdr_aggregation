require(tidyverse)
require(ggridges)

if (interactive()){
  .args <- c(
    "data/modelling/recoded_pcod.rds",
    "data/geo/pcods_admin2.csv",
    "output/modelling/preliminary/all_pairs/infected_30_R0_1.5_sample_1_trajectory.rds",
    "output/modelling/preliminary/sequential/infected_30_R0_1.5_sample_1_trajectory.rds",
    "output/modelling/preliminary/all_pairs/infected_30_R0_3_sample_1_trajectory.rds",
    "output/modelling/preliminary/sequential/infected_30_R0_3_sample_1_trajectory.rds"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

pcods <- read_rds(.args[1])
pcods_a2 <- read.csv(.args[2], header = F, col.names = c("pcod", "name2"))

all_pairs_trajectory_1.5 <- read_rds(.args[3]) 
sequential_trajectory_1.5 <- read_rds(.args[4]) 

all_pairs_trajectory_3 <- read_rds(.args[5]) 
sequential_trajectory_3 <- read_rds(.args[6]) 

# order by time of arrival in sequential network 
arrival_order <- all_pairs_trajectory_3 %>% 
  group_by(node) %>% 
  arrange(time) %>% 
  mutate(cases_increase = c(NA, diff(I))) %>% 
  filter(cases_increase > 1) %>% 
  summarise(introduction_time = min(time)) %>% 
  arrange(introduction_time)
  
district_subset_node <- c(30, arrival_order$node[seq(1, length(arrival_order$node), 10)])
district_subset_pcod <- names(pcods[district_subset_node])

district_subset <- data.frame(node=district_subset_node,
           pcod=district_subset_pcod) %>% 
  left_join(pcods_a2, by = "pcod")

p_curve_comparison <- rbind(
  all_pairs_trajectory_1.5,
  sequential_trajectory_1.5,
  all_pairs_trajectory_3,
  sequential_trajectory_3
) %>% 
  left_join(district_subset, by="node") %>% 
  drop_na(pcod) %>% 
  filter(I > 0) %>% 
  mutate(name2 = factor(name2, levels = district_subset$name2),
         R0 = factor(R0, levels = c("3", "1.5"), labels = c("R = 3", "R = 1.5"))) %>% 
  mutate(I = scales::rescale(I)) %>% 
  ggplot(aes(x = time, y = name2, height = I, color=mobility_type)) +
  scale_color_manual(values=c("all_pairs" = "red", "sequential" = "black"),
                    labels=c("All Pairs", "Sequential")) + 
  geom_ridgeline(alpha=0.5, size=0.7, scale = 1.3, fill="transparent") + 
  facet_wrap(~R0, scales="free_x") + 
  #scale_x_continuous(limits = c(0, 750)) + 
  labs(y = "District", x = "Time", color="Mobility\nNetwork") +
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major = element_line(color="black", size=0.05),
        strip.background = element_rect(fill="white", color="black", size=0.5))

 ggsave("output/figures/epi_curve_comparison.png",
       p_curve_comparison,
       width=10, height=10, units="in")  
  
