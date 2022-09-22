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
	${PWD}/data/mobility_modelling/radiation_basic/sequential_model.rds

epi_modelling: \
	epi_modelling_data_preparation \
	${PWD}/data/epi_modelling/results/all_intro_locs/all_pairs/R0_3_infected_fid000_trajectory.rds

epi_modelling_data_preparation: \
	${PWD}/data/epi_modelling/all_pairs_events.rds \
	${PWD}/data/epi_modelling/sequential_events.rds

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

${PWD}/output/figures/movement_raster_comparison.png: ${PWD}/src/evaluate_gravity_models.R \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/distance/distance_matrix_admin2.rds \
		${PWD}/data/networks/all_pairs_admin2.csv \
		${PWD}/data/networks/sequential_admin2.csv \
		${PWD}/output/gravity_modelling/all_pairs_model.rds \
		${PWD}/output/gravity_modelling/sequential_model.rds \
		${PWD}/output/gravity_modelling/all_pairs_model_predictions.rds \
		${PWD}/output/gravity_modelling/sequential_model_predictions.rds
	$(R_INTERPRETER) $^ $@

########## EPI MODELLING ##########

${PWD}/data/epi_modelling/all_pairs_events.rds: ${PWD}/src/prepare_epi_modelling_data.R \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/mobility_modelling/gravity_power/all_pairs_model_predictions.rds
	export N_MODEL_DATES="1000" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/epi_modelling/sequential_events.rds: ${PWD}/src/prepare_epi_modelling_data.R \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/mobility_modelling/gravity_power/sequential_model_predictions.rds
	export N_MODEL_DATES="1000" && \
	$(R_INTERPRETER) $^ $@

${PWD}/data/epi_modelling/results/all_intro_locs/all_pairs/R0_3_infected_fid000_trajectory.rds: ${PWD}/src/run_seir_model_all_intro_locs.R \
		${PWD}/src/seir_model.R \
	  ${PWD}/data/epi_modelling/population.rds \
	  ${PWD}/data/epi_modelling/all_pairs_events.rds
	export MODEL_R0_VALUE="3" && \
	$(R_INTERPRETER) $^ $@

########## EPI MODEL EVALUATION ##########

${PWD}/output/figures/peak_infected_proportion_boxplot.png: ${PWD}/src/plot_modelled_epidemic_peak.R \
		${PWD}/data/modelling/recoded_pcod.rds \
		${PWD}/data/geo/pcods_admin2.csv \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/output/modelling/preliminary/all_pairs/*_sample_1_trajectory.rds \
		${PWD}/output/modelling/preliminary/sequential/*_sample_1_trajectory.rds
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/modelled_trajectory.png: ${PWD}/src/plot_modelled_epidemic_curve.R \
		${PWD}/data/geo/pcods_admin2.csv \
		${PWD}/output/modelling/preliminary/all_pairs/*.rds \
		${PWD}/output/modelling/preliminary/sequential/*.rds
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
