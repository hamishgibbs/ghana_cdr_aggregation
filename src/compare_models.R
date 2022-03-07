

total_infected_per_time <- function(result){
  return(
    result %>% 
      group_by(time) %>% 
      summarise(I = sum(I))
  )
  
}

total_infected_per_time <- function(result){
  return(
    result %>% 
      group_by(time) %>% 
      summarise(I = sum(I))
  )
}
result_data %>% 
  group_by(node) %>% 
  mutate(introduction_date = I >= 1 & !duplicated(I >= 1)) %>% 
  filter(introduction_date) %>% 
  # make sure that populations align with the order of the population vector
  arrange(node) %>% 
  ungroup() %>% 
  mutate(population = population) %>% 
  select(node, time)

#all_pairs_res_total <- total_infected_per_time(all_pairs_res) %>% mutate(type="all_pairs")
#sequential_res_total <- total_infected_per_time(sequential_res) %>% mutate(type="sequential")

#rbind(sequential_res_total, all_pairs_res_total) %>% 
#  ggplot() + 
#  geom_path(aes(x = time, y = I, color = type))


#rbind(sequential_res, all_pairs_res) %>% 
#  ggplot() + 
#  geom_path(aes(x = time, y = I, group=node, color=type))

#all_pairs_res %>% 
#  filter(I == 1000)

#all_pairs_res$I
#sequential_res$I

#which(sequential_res_total$I == max(sequential_res_total$I))
#which(all_pairs_res_total$I == max(all_pairs_res_total$I))
pcods <- read_rds("data/modelling/recoded_pcod.rds")
population <- read_rds("data/modelling/population.rds")
pcod_name_lookup <- read_csv("data/geo/pcods_admin2.csv",
                             col_names=c("pcod", "pcod_name"))

introduction_sensitivity <- list()
i <- 1

for (mobility_type in c("all_pairs", "sequential")){
  introductions_path <- paste0("output/modelling/preliminary/", mobility_type, "/")
  introductions <- list.files(introductions_path)
  for (introduction in introductions){
    introduction_location <- as.numeric(str_split(introduction, "_")[[1]][2])
    result_data <- read_rds(paste0(introductions_path, introduction))
    total_infected <- total_infected_per_time(result_data)$I
    max_infected_time <- which(total_infected == max(total_infected))
    introduction_sensitivity[[i]] <- data.frame(pcod=names(pcods)[introduction_location],
                                                total_infected=max(total_infected),
                                                max_infected_time=max_infected_time,
                                                introduction_population=population[introduction_location],
                                                mobility_type=mobility_type)
    i <- i+1
  }
}

introduction_sensitivity <- do.call(rbind, introduction_sensitivity)

peak_timing_difference <- introduction_sensitivity %>% 
  select(-total_infected, -introduction_population) %>% 
  pivot_wider(names_from = mobility_type, values_from=max_infected_time) %>% 
  mutate(timing_diff=all_pairs - sequential) %>% 
  select(-all_pairs, -sequential)

peak_cases_difference <- introduction_sensitivity %>% 
  select(-max_infected_time, -introduction_population) %>% 
  pivot_wider(names_from = mobility_type, values_from=total_infected) %>% 
  mutate(cases_diff=all_pairs - sequential) %>% 
  select(-all_pairs, -sequential)

peak_difference <- peak_timing_difference %>% 
  left_join(peak_cases_difference, by=c("pcod"))

pcod_difference_ranked <- peak_difference %>% 
  mutate(timing_diff = abs(timing_diff),
         cases_diff = abs(cases_diff)) %>% 
  arrange(-timing_diff, -cases_diff) %>% 
  pull(pcod)

biggest_difference <- pcods[which(names(pcods) == pcod_difference_ranked[1])]

smallest_difference <- pcods[which(names(pcods) == pcod_difference_ranked[length(pcod_difference_ranked)])]

plot_epidemic_comparison <- function(sequential_res_total,
                                     all_pairs_res_total, 
                                     annotation_y_offset,
                                     days_text_y_offset,
                                     days_text_x_offset,
                                     infections_text_x_offset,
                                     infections_text_y_offset,
                                     legend_x,
                                     legend_y,
                                     segment_size,
                                     title,
                                     subtitle,
                                     ymin, 
                                     ymax,
                                     bracket_offset){
  
  sequential_max_cases <- max(sequential_res_total$I)
  all_pairs_max_cases <- max(all_pairs_res_total$I)
  low_max_cases <- min(c(sequential_max_cases, all_pairs_max_cases))
  high_max_cases <- max(c(sequential_max_cases, all_pairs_max_cases))
  
  print(high_max_cases)
  
  sequential_max_cases_timing <- which(sequential_res_total$I == sequential_max_cases)
  all_pairs_max_cases_timing <- which(all_pairs_res_total$I == all_pairs_max_cases)
  low_max_cases_timing <- min(c(sequential_max_cases_timing, all_pairs_max_cases_timing))
  high_max_cases_timing <- max(c(sequential_max_cases_timing, all_pairs_max_cases_timing))
  
  p <- rbind(sequential_res_total, all_pairs_res_total) %>% 
    ggplot() + 
    geom_path(aes(x = time, y = I, color = type)) + 
    xlim(100, 600) + 
    annotate("segment", size=segment_size,
             x = sequential_max_cases_timing, 
             xend = sequential_max_cases_timing, 
             y = sequential_max_cases + bracket_offset, 
             yend = high_max_cases + annotation_y_offset) + 
    annotate("segment", size=segment_size,
             x = all_pairs_max_cases_timing, 
             xend = all_pairs_max_cases_timing, 
             y = all_pairs_max_cases + bracket_offset, 
             yend = high_max_cases + annotation_y_offset) + 
    annotate("segment", size=segment_size,
             x = all_pairs_max_cases_timing, 
             xend = sequential_max_cases_timing, 
             y = high_max_cases + annotation_y_offset, 
             yend = high_max_cases + annotation_y_offset) + 
    annotate("text", 
             x = all_pairs_max_cases_timing + days_text_x_offset, 
             y = high_max_cases + days_text_y_offset,
             label = paste0(low_max_cases_timing - high_max_cases_timing, " days"),
             hjust=0) + 
    annotate("text", 
             x = all_pairs_max_cases_timing + infections_text_x_offset, 
             y = high_max_cases + infections_text_y_offset,
             label = paste0(scales::comma(low_max_cases - high_max_cases), " infections"),
             hjust=0) + 
    theme_classic() + 
    scale_y_continuous(limits=c(ymin, ymax), labels=scales::comma) + 
    labs(y = "Infections",
         x = "Time",
         color=NULL,
         title=title,
         subtitle=subtitle) + 
    scale_color_manual(values=c("all_pairs"="red", "sequential"="black"),
                       labels=c("All Pairs Aggregation", "Sequential Aggregation")) + 
    theme(legend.position = c(legend_x, legend_y),
          legend.background = element_rect(fill="transparent"))
  
  return(p)
}

plot_epidemic_peak_difference <- function(infected_index,
                                          annotation_y_offset,
                                          days_text_y_offset,
                                          days_text_x_offset,
                                          infections_text_x_offset,
                                          infections_text_y_offset,
                                          legend_x,
                                          legend_y,
                                          segment_size,
                                          title,
                                          subtitle,
                                          ymin,
                                          ymax,
                                          bracket_offset){
  all_pairs_res <- read_rds(
    paste0("output/modelling/preliminary/all_pairs/infected_", infected_index, "_trajectory.rds")) %>% 
    mutate(type="all_pairs")
  sequential_res <- read_rds(
    paste0("output/modelling/preliminary/sequential/infected_", infected_index, "_trajectory.rds")) %>% 
    mutate(type="sequential")
  
  all_pairs_res_total <- total_infected_per_time(all_pairs_res) %>% mutate(type="all_pairs")
  sequential_res_total <- total_infected_per_time(sequential_res) %>% mutate(type="sequential")
  
  p <- plot_epidemic_comparison(
        sequential_res_total, 
        all_pairs_res_total,
        annotation_y_offset,
        days_text_y_offset,
        days_text_x_offset,
        infections_text_x_offset,
        infections_text_y_offset,
        legend_x,
        legend_y,
        segment_size,
        title,
        subtitle,
        ymin,
        ymax,
        bracket_offset)
  
  return(p)
  
}

ymin <- 0
ymax <- 2400000
legend_x <- 0.83
legend_y <- 0.5
bracket_offset <- 40000
segment_size <- 0.35

high_time <- 158
p_time_high_diff <- plot_epidemic_peak_difference(high_time,
                              annotation_y_offset=100000,
                              days_text_x_offset=-40,
                              days_text_y_offset=170000,
                              infections_text_x_offset=7,
                              infections_text_y_offset=20000,
                              legend_x=legend_x,
                              legend_y=legend_y,
                              segment_size=segment_size,
                              title="b",
                              subtitle=paste0("Infection introduced into ",
                                              str_to_title(pcod_name_lookup %>% 
                                                             filter(pcod == names(pcods[high_time])) %>% 
                                                             pull(pcod_name)),
                                              " (Population: ", scales::comma(round(population[high_time], -3)), ")"),
                              ymin=ymin,
                              ymax=ymax,
                              bracket_offset=bracket_offset)
low_cases <- 243
p_cases_low_diff <- plot_epidemic_peak_difference(low_cases,
                              annotation_y_offset=100000,
                              days_text_y_offset=180000,
                              days_text_x_offset=-20,
                              infections_text_x_offset=15,
                              infections_text_y_offset=40000,
                              legend_x=legend_x,
                              legend_y=legend_y,
                              segment_size=segment_size,
                              title="a",
                              subtitle=paste0("Infection introduced into ",
                                              str_to_title(pcod_name_lookup %>% 
                                                             filter(pcod == names(pcods[low_cases])) %>% 
                                                             pull(pcod_name) %>% 
                                                             str_replace("CCM - ", "")),
                                              " (Population: ", scales::comma(round(population[low_cases], -3)), ")"),
                              ymin=ymin,
                              ymax=ymax,
                              bracket_offset=bracket_offset)

p <- cowplot::plot_grid(p_cases_low_diff, p_time_high_diff)
ggsave("/Users/hamishgibbs/Documents/Covid-19/ghana_cdr_aggregation/output/figures/figure_2.png",
       p,
       width=11, height=5, units="in")

# then - does all pairs "tend" to produce an earlier or later epidemic?

# Peak timing (days)
# Peak infections
# Peak duration (days)

# Need to think how to do this - and ...weight it by population...? 
# i.e. most people will experience an epidemic earlier or later
# May be worth doing it for district specific trajectories with consideration for population
# Does the effect wash out on admin level 1 data?

# Timing of first arrival in each location
# Peak number of infections
# Timing of peak epidemic


introduction_sensitivity %>% 
  select(-introduction_population) %>% 
  pivot_longer(!c(pcod, mobility_type)) %>% 
  ggplot() + 
  geom_density(aes(x = value, color = mobility_type)) + 
  facet_wrap(~name, scales = 'free')

p_introduction_sensitivity <- peak_difference %>% 
  pivot_longer(!c(pcod)) %>% 
  mutate(name = factor(name, 
                       levels=c("cases_diff", "timing_diff"), 
                       labels=c("Peak Infections", "Peak Timing (Days)"))) %>% 
  ggplot() + 
  geom_boxplot(aes(x = value)) + 
  facet_wrap(~name, scales = "free_x") + 
  geom_vline(aes(xintercept=0), color="red") + 
  theme_classic() + 
  labs(subtitle = "All Pairs Aggregation minus Sequential Aggregation")

ggsave("output/figures/figure_3.png", 
       p_introduction_sensitivity,
       width=10, height=4, units="in")

peak_difference %>% 
  ggplot() + 
  geom_density(aes(x=cases_diff))

introduction_sensitivity %>% 
  ggplot() + 
  geom_point(aes(x=introduction_population, y=total_infected,
                 color=mobility_type)) + 
  scale_x_continuous(trans="log10")

# Full data on introduction time

introduction_sensitivity <- list()
i <- 1

for (mobility_type in c("all_pairs", "sequential")){
  introductions_path <- paste0("output/modelling/preliminary/", mobility_type, "/")
  introductions <- list.files(introductions_path)
  for (introduction in introductions){
    introduction_location <- as.numeric(str_split(introduction, "_")[[1]][2])
    result_data <- read_rds(paste0(introductions_path, introduction))
    result_data$mobility_type <- mobility_type
    result_data$introduction_location <- introduction_location
    introduction_sensitivity[[i]] <- result_data
    i <- i+1
  }
}

introduction_sensitivity <- do.call(rbind, introduction_sensitivity)

population_data <- data.frame(pcod=pcods, population)

# distribution of introduction times by introduction location
introduction_sensitivity %>% 
  group_by(node, introduction_location, mobility_type) %>% 
  mutate(introduction_date = I >= 1 & !duplicated(I >= 1)) %>% 
  filter(introduction_date) %>% 
  select(-c(S, E, I, R)) %>% 
  left_join(population_data, by = c("node" = "pcod")) %>% 
  ggplot() + 
  geom_boxplot(aes(x = time, y=mobility_type))
  group_by(mobility_type) %>% 
  summarise(wt_mean_introduction_time = weighted.mean(time, population),
            mean_introduction_time = mean(time))







