suppressPackageStartupMessages({
  require(data.table)
})

if (interactive()){
  .args <- c(
    "data/epi_modelling/results/t_test_results.csv"
  )
} else {
  .args <- commandArgs(trailingOnly = T)
}

get_epi_quantities <- function(fn){
  epi <- readr::read_rds(fn)
  national_epi <- epi$national_epi
  return (data.frame(
    mobility_model_type=unique(national_epi$mobility_model_type),
    mobility_network_type=unique(national_epi$mobility_network_type),
    R0=unique(national_epi$R0),
    sample=unique(national_epi$sample),
    # timing of the peak
    peak_time=national_epi[which(national_epi$I == max(national_epi$I)), ][1]$time,
    # size of the peak
    peak_size=national_epi[which(national_epi$I == max(national_epi$I)), ][1]$I,
    # length of the national_epidemic
    national_epi_length=min(national_epi$time[national_epi$I == 0]),
    # average arrival time (1 recorded case)
    avg_arrival_1=mean(epi$intro_1$time),
    # average arrival time (5 recorded cases)
    avg_arrival_5=mean(epi$intro_5$time)
  ))
}

t_test_results <- function(x){
  data.frame(
    variable=gsub(" by mobility_network_type", "", x$data.name),
    statistic=x$statistic,
    p_value=x$p.value,
    ci_lower=x$conf.int[1],
    ci_upper=x$conf.int[2]
  )
}

in_fn <- list.files("data/epi_modelling/results_slim/focus", 
                    pattern='.rds',
                    full.names = T)

#in_fn <- c(in_fn[grepl('gravity_exp.all_pairs.R0_1.25.infected_fid136', in_fn)],
#           in_fn[grepl('gravity_exp.sequential.R0_1.25.infected_fid136', in_fn)],
#           in_fn[grepl('gravity_exp.all_pairs.R0_3.infected_fid136', in_fn)],
#           in_fn[grepl('gravity_exp.sequential.R0_3.infected_fid136', in_fn)])

epi_quantities <- do.call(rbind, lapply(in_fn, get_epi_quantities))
epi_quantities <- as.data.table(epi_quantities)

t_test_res <- list()

for (R0_value in unique(epi_quantities$R0)){
  for (mobility_model_type_value in unique(epi_quantities$mobility_model_type)){
        df <- epi_quantities[
          epi_quantities$R0 == R0_value &
          epi_quantities$mobility_model_type == mobility_model_type_value
        ,]
        
        df[, mobility_network_type := factor(mobility_network_type, levels=c("sequential", "all_pairs"))]
        
        t.peak_time <- t_test_results(t.test(peak_time ~ mobility_network_type, var.equal=TRUE, data = df))
        t.peak_size <- t_test_results(t.test(peak_size ~ mobility_network_type, var.equal=TRUE, data = df))
        
        # NA when an epidemic never ends
        if (T %in% c(df$national_epi_length == Inf)){
          t.national_epi_length <- data.frame(
            variable="national_epi_length",statistic=NA,p_value=NA,ci_lower=NA,ci_upper=NA
          )
        } else {
          t.national_epi_length <- t_test_results(t.test(national_epi_length ~ mobility_network_type, var.equal=TRUE, data = df))
        }
        
        t.avg_arrival_1 <- t_test_results(t.test(avg_arrival_1 ~ mobility_network_type, var.equal=TRUE, data = df))
        t.avg_arrival_5 <- t_test_results(t.test(avg_arrival_5 ~ mobility_network_type, var.equal=TRUE, data = df))
        
        res <- rbind(t.peak_time, t.peak_size, t.national_epi_length, t.avg_arrival_1, t.avg_arrival_5)
        res$R0 <- R0_value
        res$mobility_model_type <- mobility_model_type_value
        t_test_res[[paste0(R0_value, mobility_model_type_value)]] <- res
  }  
}

t_test_res <- do.call(rbind, t_test_res)

fwrite(t_test_res, tail(.args, 1))



