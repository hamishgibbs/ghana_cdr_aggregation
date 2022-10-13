PYTHON_INTERPRETER = python3
R_INTERPRETER = /usr/local/bin/Rscript

.PHONY: default

default: \
	${PWD}/output/figures/movement_raster_comparison.png \
	${PWD}/output/figures/peak_infected_proportion_boxplot.png \
	${PWD}/output/figures/modelled_trajectory.png \
	${PWD}/output/figures/cell_sites_per_district.png \
	${PWD}/output/figures/figure_1.png \
	data_cleaning \
	mobility_modelling

mobility_modelling: \
	${PWD}/data/mobility_modelling/gravity_basic/all_pairs_model.rds \
	${PWD}/data/mobility_modelling/gravity_basic/sequential_model.rds \
	${PWD}/data/mobility_modelling/gravity_power/all_pairs_model.rds \
	${PWD}/data/mobility_modelling/gravity_power/sequential_model.rds \
	${PWD}/data/mobility_modelling/gravity_exp/all_pairs_model.rds \
	${PWD}/data/mobility_modelling/gravity_exp/sequential_model.rds \
	${PWD}/data/mobility_modelling/radiation_basic/all_pairs_model.rds \
	${PWD}/data/mobility_modelling/radiation_basic/sequential_model.rds \
	${PWD}/output/tables/mobility_model_comparison.csv \
	${PWD}/output/figures/movement_raster_comparison.png

epi_modelling_results: \
	${PWD}/data/epi_modelling/results/gravity_exp/focus_locs_results.csv \
	${PWD}/data/epi_modelling/results/gravity_power/focus_locs_results.csv \
	${PWD}/data/epi_modelling/results/radiation_basic/focus_locs_results.csv

epi_modelling: \
	epi_modelling_data_preparation \
	${PWD}/data/epi_modelling/results/DONE_ALL_LOCS.rds \
	${PWD}/data/epi_modelling/results/DONE_FOCUS_LOCS.rds

epi_modelling_data_preparation: \
	${PWD}/data/epi_modelling/events/gravity_exp/all_pairs_events.rds \
	${PWD}/data/epi_modelling/events/gravity_exp/sequential_events.rds \
	${PWD}/data/epi_modelling/events/gravity_power/all_pairs_events.rds \
	${PWD}/data/epi_modelling/events/gravity_power/sequential_events.rds \
	${PWD}/data/epi_modelling/events/radiation_basic/all_pairs_events.rds \
	${PWD}/data/epi_modelling/events/radiation_basic/sequential_events.rds

data_cleaning: \
	${PWD}/data/networks/all_pairs_admin2.csv

${PWD}/data/networks/all_pairs_admin2.csv: ${PWD}/src/aggregate_networks.R \
		${PWD}/../../LSHTM/Filr/My_Files/Projects/Ghana/movement/update_09_2021/home/flowkit/playground/data/sensitive/aggregates/trips_per_day_admin2.*.csv \
		${PWD}/../../LSHTM/Filr/My_Files/Projects/Ghana/movement/consecutive_trips/data/consecutive_trips_od_matrix_admin.csv
	$(R_INTERPRETER) $^ $@

########## MOBILITY MODELLING ##########

${PWD}/data/mobility_modelling/gravity_basic/all_pairs_model.rds: ${PWD}/src/train_mobility_models.R \
		${PWD}/data/population/population_admin2.csv \
    ${PWD}/data/distance/distance_matrix_admin2.rds \
    ${PWD}/data/networks/all_pairs_admin2.csv
	export MOBILITY_MODEL="gravity" && \
	export MOBILITY_MODEL_TYPE="basic" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/mobility_modelling/gravity_basic/sequential_model.rds: ${PWD}/src/train_mobility_models.R \
		${PWD}/data/population/population_admin2.csv \
    ${PWD}/data/distance/distance_matrix_admin2.rds \
    ${PWD}/data/networks/sequential_admin2.csv
	export MOBILITY_MODEL="gravity" && \
	export MOBILITY_MODEL_TYPE="basic" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/mobility_modelling/gravity_power/all_pairs_model.rds: ${PWD}/src/train_mobility_models.R \
		${PWD}/data/population/population_admin2.csv \
    ${PWD}/data/distance/distance_matrix_admin2.rds \
    ${PWD}/data/networks/all_pairs_admin2.csv
	export MOBILITY_MODEL="gravity" && \
	export MOBILITY_MODEL_TYPE="power" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/mobility_modelling/gravity_power/sequential_model.rds: ${PWD}/src/train_mobility_models.R \
		${PWD}/data/population/population_admin2.csv \
    ${PWD}/data/distance/distance_matrix_admin2.rds \
    ${PWD}/data/networks/sequential_admin2.csv
	export MOBILITY_MODEL="gravity" && \
	export MOBILITY_MODEL_TYPE="power" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/mobility_modelling/gravity_exp/all_pairs_model.rds: ${PWD}/src/train_mobility_models.R \
		${PWD}/data/population/population_admin2.csv \
    ${PWD}/data/distance/distance_matrix_admin2.rds \
    ${PWD}/data/networks/all_pairs_admin2.csv
	export MOBILITY_MODEL="gravity" && \
	export MOBILITY_MODEL_TYPE="exp" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/mobility_modelling/gravity_exp/sequential_model.rds: ${PWD}/src/train_mobility_models.R \
		${PWD}/data/population/population_admin2.csv \
    ${PWD}/data/distance/distance_matrix_admin2.rds \
    ${PWD}/data/networks/sequential_admin2.csv
	export MOBILITY_MODEL="gravity" && \
	export MOBILITY_MODEL_TYPE="exp" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/mobility_modelling/radiation_basic/all_pairs_model.rds: ${PWD}/src/train_mobility_models.R \
		${PWD}/data/population/population_admin2.csv \
    ${PWD}/data/distance/distance_matrix_admin2.rds \
    ${PWD}/data/networks/all_pairs_admin2.csv
	export MOBILITY_MODEL="radiation" && \
	export MOBILITY_MODEL_TYPE="basic" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/mobility_modelling/radiation_basic/sequential_model.rds: ${PWD}/src/train_mobility_models.R \
		${PWD}/data/population/population_admin2.csv \
    ${PWD}/data/distance/distance_matrix_admin2.rds \
    ${PWD}/data/networks/sequential_admin2.csv
	export MOBILITY_MODEL="radiation" && \
	export MOBILITY_MODEL_TYPE="basic" && \
	$(R_INTERPRETER) $^ $@

########## MOBILITY MODEL EVALUATION ##########

${PWD}/output/figures/movement_raster_comparison.png: ${PWD}/src/evaluate_mobility_models.R \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/distance/distance_matrix_admin2.rds \
		${PWD}/data/networks/all_pairs_admin2.csv \
		${PWD}/data/networks/sequential_admin2.csv \
		${PWD}/data/mobility_modelling/gravity_power/all_pairs_model.rds \
		${PWD}/data/mobility_modelling/gravity_power/sequential_model.rds \
		${PWD}/data/mobility_modelling/gravity_power/all_pairs_model_predictions.rds \
		${PWD}/data/mobility_modelling/gravity_power/sequential_model_predictions.rds \
		${PWD}/data/geo/admin2.geojson
	$(R_INTERPRETER) $^ $@

${PWD}/output/tables/mobility_model_comparison.csv: ${PWD}/src/compare_mobility_models.R \
		${PWD}/data/mobility_modelling/gravity_basic/all_pairs_model.rds \
		${PWD}/data/mobility_modelling/gravity_exp/all_pairs_model.rds \
		${PWD}/data/mobility_modelling/gravity_power/all_pairs_model.rds \
		${PWD}/data/mobility_modelling/radiation_basic/all_pairs_model.rds \
		${PWD}/data/mobility_modelling/gravity_basic/sequential_model.rds \
		${PWD}/data/mobility_modelling/gravity_exp/sequential_model.rds \
		${PWD}/data/mobility_modelling/gravity_power/sequential_model.rds \
		${PWD}/data/mobility_modelling/radiation_basic/sequential_model.rds
	$(R_INTERPRETER) $^ $@

########## EPI MODELLING ##########

${PWD}/data/epi_modelling/events/gravity_exp/all_pairs_events.rds: ${PWD}/src/prepare_epi_modelling_data.R \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/mobility_modelling/gravity_exp/all_pairs_model_predictions.rds
	export N_MODEL_DATES="1500" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/epi_modelling/events/gravity_exp/sequential_events.rds: ${PWD}/src/prepare_epi_modelling_data.R \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/mobility_modelling/gravity_exp/sequential_model_predictions.rds
	export N_MODEL_DATES="1500" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/epi_modelling/events/gravity_power/all_pairs_events.rds: ${PWD}/src/prepare_epi_modelling_data.R \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/mobility_modelling/gravity_power/all_pairs_model_predictions.rds
	export N_MODEL_DATES="1500" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/epi_modelling/events/gravity_power/sequential_events.rds: ${PWD}/src/prepare_epi_modelling_data.R \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/mobility_modelling/gravity_power/sequential_model_predictions.rds
	export N_MODEL_DATES="1500" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/epi_modelling/events/radiation_basic/all_pairs_events.rds: ${PWD}/src/prepare_epi_modelling_data.R \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/mobility_modelling/radiation_basic/all_pairs_model_predictions.rds
	export N_MODEL_DATES="1500" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/epi_modelling/events/radiation_basic/sequential_events.rds: ${PWD}/src/prepare_epi_modelling_data.R \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/mobility_modelling/radiation_basic/sequential_model_predictions.rds
	export N_MODEL_DATES="1500" && \
	$(R_INTERPRETER) $^ $@

########## EPI MODELLING (5 Focus locations) ##########

${PWD}/data/epi_modelling/results/DONE_FOCUS_LOCS.rds: ${PWD}/src/run_seir_model.R \
		${PWD}/src/seir_model.R \
		${PWD}/data/epi_modelling/population.rds \
		${PWD}/data/epi_modelling/intro_locs_focus.csv \
		${PWD}/data/epi_modelling/events/gravity_exp/all_pairs_events.rds \
    ${PWD}/data/epi_modelling/events/gravity_exp/sequential_events.rds \
    ${PWD}/data/epi_modelling/events/gravity_power/all_pairs_events.rds \
    ${PWD}/data/epi_modelling/events/gravity_power/sequential_events.rds \
    ${PWD}/data/epi_modelling/events/radiation_basic/all_pairs_events.rds \
    ${PWD}/data/epi_modelling/events/radiation_basic/sequential_events.rds
	export N_MODEL_RUNS="10" && \
	$(R_INTERPRETER) $^ $@

########## EPI MODELLING (all introduction locations) ##########

${PWD}/data/epi_modelling/results/DONE_ALL_LOCS.rds: ${PWD}/src/run_seir_model.R \
		${PWD}/src/seir_model.R \
		${PWD}/data/epi_modelling/population.rds \
		${PWD}/data/epi_modelling/intro_locs_all.csv \
		${PWD}/data/epi_modelling/events/gravity_exp/all_pairs_events.rds \
    ${PWD}/data/epi_modelling/events/gravity_exp/sequential_events.rds \
    ${PWD}/data/epi_modelling/events/gravity_power/all_pairs_events.rds \
    ${PWD}/data/epi_modelling/events/gravity_power/sequential_events.rds \
    ${PWD}/data/epi_modelling/events/radiation_basic/all_pairs_events.rds \
    ${PWD}/data/epi_modelling/events/radiation_basic/sequential_events.rds
	export N_MODEL_RUNS="1" && \
	$(R_INTERPRETER) $^ $@

########## EPI MODEL EVALUATION ##########

# combine focus and all locs separately
${PWD}/data/epi_modelling/results/gravity_exp/focus_locs_results.csv: ${PWD}/src/combine_epi_model_results.R \
		${PWD}/data/epi_modelling/results/gravity_exp/**/**/infected_fid029*.rds \
		${PWD}/data/epi_modelling/results/gravity_exp/**/**/infected_fid146*.rds \
		${PWD}/data/epi_modelling/results/gravity_exp/**/**/infected_fid164*.rds \
		${PWD}/data/epi_modelling/results/gravity_exp/**/**/infected_fid207*.rds \
		${PWD}/data/epi_modelling/results/gravity_exp/**/**/infected_fid240*.rds
	$(R_INTERPRETER) $^ $@

${PWD}/data/epi_modelling/results/gravity_power/focus_locs_results.csv: ${PWD}/src/combine_epi_model_results.R \
		${PWD}/data/epi_modelling/results/gravity_power/**/**/infected_fid029*.rds \
		${PWD}/data/epi_modelling/results/gravity_power/**/**/infected_fid146*.rds \
		${PWD}/data/epi_modelling/results/gravity_power/**/**/infected_fid164*.rds \
		${PWD}/data/epi_modelling/results/gravity_power/**/**/infected_fid207*.rds \
		${PWD}/data/epi_modelling/results/gravity_power/**/**/infected_fid240*.rds
	$(R_INTERPRETER) $^ $@

${PWD}/data/epi_modelling/results/radiation_basic/focus_locs_results.csv: ${PWD}/src/combine_epi_model_results.R \
		${PWD}/data/epi_modelling/results/radiation_basic/**/**/infected_fid029*.rds \
		${PWD}/data/epi_modelling/results/radiation_basic/**/**/infected_fid146*.rds \
		${PWD}/data/epi_modelling/results/radiation_basic/**/**/infected_fid164*.rds \
		${PWD}/data/epi_modelling/results/radiation_basic/**/**/infected_fid207*.rds \
		${PWD}/data/epi_modelling/results/radiation_basic/**/**/infected_fid240*.rds
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/peak_infected_proportion_boxplot.png: ${PWD}/src/plot_modelled_epidemic_peak.R \
		${PWD}/data/geo/pcods_admin2.csv \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/epi_modelling/results/focus_locs/epi_model_results_focus_locs.csv
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/modelled_trajectory.png: ${PWD}/src/plot_modelled_epidemic_curve.R \
		${PWD}/data/geo/pcods_admin2.csv \
		${PWD}/data/epi_modelling/results/focus_locs/epi_model_results_focus_locs.csv
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/cell_sites_per_district.png: ${PWD}/src/plot_cell_sites_per_a2.R \
		${PWD}/data/cell_sites/cell_sites_admin2.csv \
		${PWD}/data/geo/admin2.geojson \
		${PWD}/data/population/population_admin2.csv
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/figure_1.png: ${PWD}/src/compare_networks.R \
		${PWD}/data/networks/all_pairs_admin2_timeseries.csv \
		${PWD}/data/networks/sequential_admin2_timeseries.csv \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/cell_sites/cell_sites_admin2.csv \
		${PWD}/data/geo/admin2.geojson \
		${PWD}/data/geo/journey_lines.geojson
	$(R_INTERPRETER) $^ $@
