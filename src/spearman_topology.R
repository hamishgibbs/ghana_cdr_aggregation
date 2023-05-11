suppressPackageStartupMessages({
  require(data.table)
})

if (interactive()){
  .args <- c(
    "data/geo/pcods_admin2.csv",
    "output/fmt_spearman.csv",
    "output/fmt_spearman_focus.csv"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

get_epi_quantities <- function(fn){
  in_df <- readr::read_rds(fn)
  intro_1 <- in_df$intro_1
  intro_1$type <- "intro_1"
  intro_5 <- in_df$intro_5
  intro_5$type <- "intro_5"
  return (rbind(intro_1, intro_5))
}

spearman_results <- function(x){
  data.table(
    statistic=x$statistic,
    p_value=x$p.value,
    rho=x$estimate
  )
}

.outputs <- tail(.args, 2)

pcods_a2 <- read.csv(.args[1], header = F, col.names = c("pcod", "name2"))

in_fn <- list.files("data/epi_modelling/results_slim/focus", 
                    pattern='.rds',
                    full.names = T)

#in_fn <- c(in_fn[grepl('gravity_exp.all_pairs.R0_1.25.infected_fid136', in_fn)],
#           in_fn[grepl('gravity_exp.sequential.R0_1.25.infected_fid136', in_fn)],
#           in_fn[grepl('gravity_exp.all_pairs.R0_3.infected_fid136', in_fn)],
#           in_fn[grepl('gravity_exp.sequential.R0_3.infected_fid136', in_fn)])


epi_intro <- do.call(rbind, lapply(in_fn, get_epi_quantities))
epi_intro <- as.data.table(epi_intro)

epi_avg_intro <- epi_intro[, .(time = mean(time)), by = c("mobility_model_type", "mobility_network_type", "introduction_location", "R0", "node", "type")]

spearman_res <- list()

for (R0_value in unique(epi_avg_intro$R0)){
  for (mobility_model_type_value in unique(epi_avg_intro$mobility_model_type)){
    for (introduction_location_value in unique(epi_avg_intro$introduction_location)){
      df <- epi_avg_intro[
          epi_avg_intro$R0 == R0_value &
          epi_avg_intro$mobility_model_type == mobility_model_type_value & 
          epi_avg_intro$introduction_location == introduction_location_value
        ,]
      
      df1 <- subset(df, type == "intro_1")
      df5 <- subset(df, type == "intro_5")
      df1 <- dcast(df1, mobility_model_type + introduction_location + R0 + node ~ mobility_network_type, value.var = 'time')
      df5 <- dcast(df5, mobility_model_type + introduction_location + R0 + node ~ mobility_network_type, value.var = 'time')
      
      res1 <- spearman_results(cor.test(x=df1$all_pairs, y=df1$sequential, method = 'spearman', exact=F))
      res1$type <- "intro_1"
      res5 <- spearman_results(cor.test(x=df5$all_pairs, y=df5$sequential, method = 'spearman', exact=F))
      res5$type <- "intro_5"
      res <- rbind(res1, res5)
      res$R0 <- R0_value
      res$mobility_model_type <- mobility_model_type_value
      res$introduction_location <- introduction_location_value
      spearman_res[[paste0(R0_value, mobility_model_type_value, introduction_location_value)]] <- res
    
    }
  }  
}

spearman_res <- do.call(rbind, spearman_res)

rho_precision <- 2
p_value_precision <- 4

spearman_res[, value := paste0(
  round(rho, rho_precision), 
  " (", round(p_value, p_value_precision), ")")]

spearman_res$value <- gsub("[(]0[)]", "(<0.0001)", spearman_res$value)

spearman_res[pcods_a2, on=c("introduction_location"="pcod"), name2 := name2]

spearman_res$name2 <- gsub("AMA-", "", spearman_res$name2)
spearman_res$name2 <- gsub("KMA-", "", spearman_res$name2)
spearman_res$name2 <- gsub("TM-", "", spearman_res$name2)
spearman_res$name2 <- stringr::str_to_title(spearman_res$name2) 

spearman_res <- dcast(spearman_res, name2 + R0 ~ mobility_model_type + type, value.var = "value")

focus_names <- c(
  "Lawra", "Tamale South", "Nkwanta South", "Manhyia South", "Okaikoi South"
)

fwrite(spearman_res, .outputs[1])
fwrite(subset(spearman_res, name2 %in% focus_names), .outputs[2])
