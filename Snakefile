import os
import pandas as pd
from glob import glob
import json

with open("config.json") as f:
    config = json.load(f)

network_types = config["network_types"]
mobility_model_types = config["mobility_model_types"]
R0_values = config["R0_values"]

password = os.environ["SERVER_PASSWORD"]

with open("hosts.txt", "r") as f:
    lines = f.readlines()
servers = [x.strip() for x in lines]

rule all: 
    input: 
        "output/figures/movement_kernel_comparison.png",
        "output/figures/movement_raster_comparison.png",
        "output/figures/figure_1.png",
        expand("output/figures/{mobility_model}_modelled_trajectory.png", mobility_model=mobility_model_types),
        "output/figures/modelled_trajectories_comparison.png",
        "data/mobility_modelling/peak_time_differences.csv"

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

rule compare_networks:
    input: 
        "src/compare_networks.R",
        "data/networks/all_pairs_admin2_timeseries.csv",
        "data/networks/sequential_admin2_timeseries.csv",
        "data/population/population_admin2.csv",
        "data/cell_sites/cell_sites_admin2.csv",
        "data/geo/admin2.geojson",
        "data/geo/journey_lines.geojson"
    output: 
        "output/figures/figure_1.png",
        "output/figures/journey_comparison.png"
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
    
rule combine_mobility_model_predictions:
    input: 
        "src/combine_mobility_model_predictions.R",
        "data/distance/distance_matrix_admin2.rds",
        "data/networks/all_pairs_admin2.csv",
        "data/networks/sequential_admin2.csv",
        expand("data/mobility_modelling/{mobility_model}/{network}_model_predictions.rds", mobility_model = mobility_model_types, network = network_types)
    output: 
        "data/mobility_modelling/mobility_model_predictions.csv"
    shell: 
        "Rscript {input} {output}"

rule compare_mobility_model_kernel:
    input:
        "src/compare_mobility_model_kernel.R",
		"data/mobility_modelling/mobility_model_predictions.csv"
    output:
        "output/figures/movement_kernel_comparison.png"
    shell:
        "Rscript {input} {output}"

rule compare_mobility_model_raster:
    input:
        "src/compare_mobility_model_raster.R",
        "data/mobility_modelling/mobility_model_predictions.csv",
        "data/geo/admin2.geojson"
    output:
        "output/figures/movement_raster_comparison.png"
    shell:
        "Rscript {input} {output}"

rule prepare_epi_modelling_events:
    input: 
        "src/prepare_epi_modelling_data.R",
        "data/population/population_admin2.csv",
        "data/mobility_modelling/{mobility_model}/{network}_model_predictions.rds"
    output: 
        "data/epi_modelling/events/{mobility_model}/{network}_events.rds"
    shell:
        "Rscript {input} {output}"

rule sample_focus_intro_locs:
    input: 
        "src/sample_focus_intro_locs.R",
        "data/geo/pcods_admin2.csv",
        "data/geo/admin2.geojson"
    output: 
        "data/epi_modelling/intro_pcods_focus.csv",
        "data/epi_modelling/intro_pcods_all.csv",
        "output/introduction_locations.png"
    shell: 
        "Rscript {input} {output}"

def get_locs_from_fn(fn):
    return pd.read_csv(fn)["pcod"].to_list()

rule create_epi_model_jobs_focus:
    input:
        "data/epi_modelling/intro_pcods_focus.csv"
    params:
        expand(
        "data/epi_modelling/results/{mobility_model}/{network}/R0_{R0}/infected_{infected}_trajectory_{iteration}.rds", 
        mobility_model = mobility_model_types,
        network = network_types,
        R0 = R0_values,
        infected = get_locs_from_fn("data/epi_modelling/intro_pcods_focus.csv"),
        iteration = range(0, 100)
     ) 
    output:
        "data/epi_modelling/jobs/jobs_focus_locations.txt"
    run:
        with open(output[0],'w') as f:
            for i in params[0]:
                f.write(i + '\n')

rule create_epi_model_jobs_all:
    input:
        "data/epi_modelling/intro_pcods_all.csv"
    params:
        expand(
        "data/epi_modelling/results/{mobility_model}/{network}/R0_{R0}/infected_{infected}_trajectory_{iteration}.rds", 
        mobility_model = mobility_model_types,
        network = network_types,
        R0 = R0_values,
        infected = get_locs_from_fn("data/epi_modelling/intro_pcods_all.csv"),
        iteration = range(0, 10)
     ) 
    output:
        "data/epi_modelling/jobs/jobs_all_locations.txt"
    run:
        with open(output[0],'w') as f:
            for i in params[0]:
                f.write(i + '\n')

rule chunk_epi_model_jobs:
    input: 
        "src/chunk_epi_model_jobs.py",
        "data/epi_modelling/jobs/jobs_{type}_locations.txt"
    output:
        "data/epi_modelling/jobs/{type}/{server}.txt"
    shell:
        "python {input} {output}"

rule scp: # scp everything needed to run the epidemic model
    input:
        "src/epi_model/Snakefile",
        "src/epi_model/seir_model.R",
        "src/epi_model/run_seir_model.R",
        expand("data/epi_modelling/events/{mobility_model}/{network}_events.rds", 
            mobility_model=mobility_model_types, 
            network=network_types),
        "data/epi_modelling/population.rds",
        expand("data/epi_modelling/jobs/{type}/{server}.txt", type=["all", "focus"], server=servers)
    output:
        "data/epi_modelling/scp.txt"
    run: 
        for i in input:
            os.system(f"sshpass -p '{password}' scp {i} {servers[0]}:ghana_cdr_aggregation/" + i)
        os.system(f"touch {output}")

rule dispatch_epi_model_jobs:
    input: 
        "data/epi_modelling/scp.txt",
        "data/epi_modelling/jobs/{type}/{server}.txt"
    output:
        "data/epi_modelling/run/{type}/{server}.txt"
    shell:
        f"sshpass -p '{password}' ssh "
        "{wildcards.server} 'conda activate Renv && cd ghana_cdr_aggregation && "
        "{{ snakemake -s src/epi_model/Snakefile --cores 7 -k "
        "--jobname {wildcards.type}_{wildcards.server} --drop-metadata --rerun-incomplete $(< {input[1]}) ; }} > /tmp/log/run_epi_model.{wildcards.type}.{wildcards.server} 2>&1 & disown %1' & touch {output}"

rule epi_model_jobs_dispatched_focus:
    input:
        expand("data/epi_modelling/run/focus/{server}.txt", server=servers)
    output:
        "data/epi_modelling/epi_model_jobs_dispatched_focus.txt"
    shell:
        "touch {output}"

rule epi_model_jobs_dispatched_all:
    input:
        expand("data/epi_modelling/run/all/{server}.txt", server=servers)
    output:
        "data/epi_modelling/epi_model_jobs_dispatched_all.txt"
    shell:
        "touch {output}"


# rule combine_epi_modelling_focus_results:
#     input: 
#         "src/combine_epi_model_results.R",
#         expand(
#             "data/epi_modelling/results/{{mobility_model}}/{network}/R0_{R0}/infected_{infected}_trajectory_{iteration}.rds", 
#             network = network_types,
#             R0 = R0_values,
#             infected = get_focus_locs(),
#             iteration = range(0, 50)
#         )
#     output: 
#         "data/epi_modelling/results/{mobility_model}/focus_locs_results_national.csv",
#         "data/epi_modelling/results/{mobility_model}/focus_locs_results_national_peaks.csv"
#     shell: 
#         "Rscript {input} {output}"

# rule combine_epi_modelling_all_results:
#     input: 
#         "src/combine_epi_model_results.R",
#         expand(
#             "data/epi_modelling/results/{{mobility_model}}/{network}/R0_{R0}/infected_{infected}_trajectory_{iteration}.rds", 
#             network = network_types,
#             R0 = R0_values,
#             infected = get_all_locs(),
#             iteration = 0
#         )
#     output: 
#         "data/epi_modelling/results/{mobility_model}/all_locs_results_national.csv",
#         "data/epi_modelling/results/{mobility_model}/all_locs_results_national_peaks.csv"
#     shell: 
#         "Rscript {input} {output}"

# rule epi_model_trajectory: 
#     input:
#         "src/plot_modelled_epidemic_curve.R",
#         "data/geo/pcods_admin2.csv",
#         "data/epi_modelling/results/{mobility_model}/focus_locs_results_national.csv"
#     output:
#         "output/figures/{mobility_model}_modelled_trajectory.png"
#     shell:
#         "Rscript {input} {output}"

# rule epi_model_trajectory_comparison: 
#     input:
#         "src/compare_modelled_epi_curve.R",
#         "data/geo/pcods_admin2.csv",
#         expand("data/epi_modelling/results/{mobility_model}/focus_locs_results_national.csv", mobility_model=mobility_model_types)
#     output:
#         "output/figures/modelled_trajectories_comparison.png"
#     shell:
#         "Rscript {input} {output}"

# rule calculate_peak_time_diff:
#     input: 
#         "src/calculate_peak_time_difference.R",
#         expand("data/epi_modelling/results/{mobility_model}/all_locs_results_national_peaks.csv", mobility_model=mobility_model_types)
#     output:
#         "data/mobility_modelling/peak_time_differences.csv"
#     shell:
#        "Rscript {input} {output}"