import subprocess
from glob import glob
import pandas as pd

network_types = ["all_pairs", "sequential"]
mobility_model_types = ["gravity_exp", "gravity_power", "radiation_basic"]
R0_values = [3, 1.5, 1.25]

with open("hosts.txt", "r") as f:
    lines = f.readlines()
server = lines[0].split(":")[0] + "@" + lines[0].split("@")[-1].strip()
server = server.split("/")[-1]
password = lines[0].split(":")[-1].split("@")[0]

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
        "Rscript {input} {output} && " + f"sshpass -p '{password}'" + " scp {output} " + f"{server}:ghana_cdr_aggregation/" + "{output}"

def get_focus_locs():
    return pd.read_csv("data/epi_modelling/intro_locs_focus.csv")["pcod2"].to_list()

def get_all_locs():
    return pd.read_csv("data/epi_modelling/intro_locs_all.csv")["pcod2"].to_list()

rule create_epi_model_jobs:
    input:
        expand("data/epi_modelling/events/{mobility_model}/{network}_events.rds", mobility_model=mobility_model_types, network=network_types)
    output:
        "data/epi_modelling/jobs.txt"
    run:
        jobs = expand(
            "data/epi_modelling/results/{mobility_model}/{network}/R0_{R0}/infected_{infected}_trajectory_{iteration}.rds", 
            mobility_model = mobility_model_types,
            network = network_types,
            R0 = R0_values,
            infected = get_focus_locs(),
            iteration = range(0, 50)
        ) + \
        expand(
            "data/epi_modelling/results/{mobility_model}/{network}/R0_{R0}/infected_{infected}_trajectory_{iteration}.rds", 
            mobility_model = mobility_model_types,
            network = network_types,
            R0 = R0_values,
            infected = get_all_locs(),
            iteration = 0
        )
        ssh = f"sshpass -p '{password}' ssh {server} "
        cmd = "'ls ghana_cdr_aggregation/data/epi_modelling/results/**/**/**/*.rds'"
        finished_files = subprocess.Popen(ssh + cmd, 
            shell=True, 
            stdout=subprocess.PIPE, 
            stderr=subprocess.PIPE).communicate()
        finished_files = [x.replace("ghana_cdr_aggregation/", "") for x in finished_files[0].decode("utf-8").split("\n")]

        unfinished_files = list(set(jobs) - set(finished_files))

        with open(output[0], 'w') as f:
            [f.write(f"{x}\n") for x in unfinished_files]

rule scp_epi_model:
    input: 
        "src/run_seir_model.R",
        "src/run_seir_model.sh",
        "src/seir_model.R",
        "data/epi_modelling/population.rds"
    output:
        "data/epi_modelling/scp_epi_model.txt"
    shell: 
        f"sshpass -p '{password}'" + " scp {input[0]} " + f"{server}:ghana_cdr_aggregation/" + "{input[0]} && \\" +
        f"sshpass -p '{password}'" + " scp {input[1]} " + f"{server}:ghana_cdr_aggregation/" + "{input[1]} && \\" + 
        f"sshpass -p '{password}'" + " scp {input[2]} " + f"{server}:ghana_cdr_aggregation/" + "{input[2]} && \\" + 
        f"sshpass -p '{password}'" + " scp {input[3]} " + f"{server}:ghana_cdr_aggregation/" + "{input[3]} && \\" + 
        "touch {output}"

# Send epi modelling jobs to hosts with GNU Parallel
# Do not run in parallel!
rule run_epi_model:
    input: 
        "hosts.txt",
        "data/epi_modelling/jobs.txt",
        "data/epi_modelling/scp_epi_model.txt"
    output:
        "run_epi_model.txt"
    shell:
        "parallel --sshloginfile {input[0]} --jobs 100% -a {input[1]} ghana_cdr_aggregation/src/run_seir_model.sh " + "{{}} && \\" + 
        "touch {output}"

rule combine_epi_modelling_focus_results:
    input: 
        "src/combine_epi_model_results.R",
        expand(
            "data/epi_modelling/results/{{mobility_model}}/{network}/R0_{R0}/infected_{infected}_trajectory_{iteration}.rds", 
            network = network_types,
            R0 = R0_values,
            infected = get_focus_locs(),
            iteration = range(0, 50)
        )
    output: 
        "data/epi_modelling/results/{mobility_model}/focus_locs_results_national.csv",
        "data/epi_modelling/results/{mobility_model}/focus_locs_results_national_peaks.csv"
    shell: 
        "Rscript {input} {output}"

rule combine_epi_modelling_all_results:
    input: 
        "src/combine_epi_model_results.R",
        expand(
            "data/epi_modelling/results/{{mobility_model}}/{network}/R0_{R0}/infected_{infected}_trajectory_{iteration}.rds", 
            network = network_types,
            R0 = R0_values,
            infected = get_all_locs(),
            iteration = 0
        )
    output: 
        "data/epi_modelling/results/{mobility_model}/all_locs_results_national.csv",
        "data/epi_modelling/results/{mobility_model}/all_locs_results_national_peaks.csv"
    shell: 
        "Rscript {input} {output}"

rule epi_model_trajectory: 
    input:
        "src/plot_modelled_epidemic_curve.R",
        "data/geo/pcods_admin2.csv",
        "data/epi_modelling/results/{mobility_model}/focus_locs_results_national.csv"
    output:
        "output/figures/{mobility_model}_modelled_trajectory.png"
    shell:
        "Rscript {input} {output}"

rule epi_model_trajectory_comparison: 
    input:
        "src/compare_modelled_epi_curve.R",
        "data/geo/pcods_admin2.csv",
        expand("data/epi_modelling/results/{mobility_model}/focus_locs_results_national.csv", mobility_model=mobility_model_types)
    output:
        "output/figures/modelled_trajectories_comparison.png"
    shell:
        "Rscript {input} {output}"

rule calculate_peak_time_diff:
    input: 
        "src/calculate_peak_time_difference.R",
        expand("data/epi_modelling/results/{mobility_model}/all_locs_results_national_peaks.csv", mobility_model=mobility_model_types)
    output:
        "data/mobility_modelling/peak_time_differences.csv"
    shell:
        "Rscript {input} {output}"