from glob import glob

network_types = ["all_pairs", "sequential"]
mobility_model_types = ["gravity_power", "gravity_exp", "radiation_basic"]

rule all: 
    input: 
        expand("data/mobility_modelling/{mobility_model}/{network}_model.rds", 
            mobility_model = mobility_model_types, network = network_types)

rule aggregate_networks:
    input: 
        "src/aggregate_networks.R",
        glob("../../LSHTM/Filr/My_Files/Projects/Ghana/movement/update_09_2021/home/flowkit/playground/data/sensitive/aggregates/trips_per_day_admin2*.csv"),
        "../../LSHTM/Filr/My_Files/Projects/Ghana/movement/consecutive_trips/data/consecutive_trips_od_matrix_admin.csv"
    output:
        "data/networks/n_intersecting_dates.rds",
        "data/networks/all_pairs_admin2_timeseries.csv",
        "data/networks/sequential_admin2_timeseries.csv",
        "data/networks/all_pairs_admin2.csv",
        "data/networks/sequential_admin2.csv"
    shell:
        "Rscript {input} {output}"

rule run_mobility_model:
    input:
        "src/train_mobility_models.R",
        "data/population/population_admin2.csv",
        "data/distance/distance_matrix_admin2.rds",
        "data/networks/{network}_admin2.csv"
    output:
        "data/mobility_modelling/{mobility_model}/{network}_model.rds",
        "data/mobility_modelling/{mobility_model}/{network}_model_predictions.rds",
        "data/mobility_modelling/{mobility_model}/{network}_model_check.png",
        "data/mobility_modelling/{mobility_model}/{network}_model_check.csv"
    shell:
        "Rscript {input} {output}"