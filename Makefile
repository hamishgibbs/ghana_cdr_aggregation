PYTHON_INTERPRETER = python3
R_INTERPRETER = /usr/local/bin/Rscript

.PHONY: default

default: \
	${PWD}/output/figures/gravity_exp_modelled_trajectory.png \
	${PWD}/output/figures/cell_sites_per_district.png \
	${PWD}/output/figures/figure_1.png \
	data_cleaning \
	mobility_modelling \
	epi_modelling_figures

mobility_modelling: \
	${PWD}/data/mobility_modelling/gravity_power/all_pairs_model.rds \
	${PWD}/data/mobility_modelling/gravity_power/sequential_model.rds \
	${PWD}/data/mobility_modelling/gravity_exp/all_pairs_model.rds \
	${PWD}/data/mobility_modelling/gravity_exp/sequential_model.rds \
	${PWD}/data/mobility_modelling/radiation_basic/all_pairs_model.rds \
	${PWD}/data/mobility_modelling/radiation_basic/sequential_model.rds \
	${PWD}/output/tables/mobility_model_comparison.csv \
	${PWD}/output/figures/movement_raster_comparison_gravity_power.png \
	${PWD}/output/figures/movement_raster_comparison_gravity_exp.png \
	${PWD}/output/figures/movement_raster_comparison_radiation_basic.png \
	${PWD}/output/figures/movement_kernel_comparison.png \
	${PWD}/output/figures/movement_raster_comparison.png

epi_modelling_figures: \
	${PWD}/output/figures/gravity_exp_modelled_trajectory.png \
	${PWD}/output/figures/gravity_power_modelled_trajectory.png \
	${PWD}/output/figures/radiation_basic_modelled_trajectory.png \
	${PWD}/output/figures/gravity_power_time_delay_all_locs.png \
	${PWD}/output/figures/gravity_exp_time_delay_all_locs.png \
	${PWD}/output/figures/radiation_basic_time_delay_all_locs.png \
	${PWD}/output/figures/modelled_trajectories_comparison.png

epi_modelling_results: \
	${PWD}/data/epi_modelling/results/gravity_exp/focus_locs_results_national.csv \
	${PWD}/data/epi_modelling/results/gravity_power/focus_locs_results_national.csv \
	${PWD}/data/epi_modelling/results/radiation_basic/focus_locs_results_national.csv \
	${PWD}/data/epi_modelling/results/gravity_exp/all_locs_results_national.csv \
	${PWD}/data/epi_modelling/results/gravity_power/all_locs_results_national.csv \
	${PWD}/data/epi_modelling/results/radiation_basic/all_locs_results_national.csv

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

${PWD}/data/mobility_modelling/mobility_model_predictions.csv: ${PWD}/src/combine_mobility_model_predictions.R \
		${PWD}/data/distance/distance_matrix_admin2.rds \
		${PWD}/data/networks/all_pairs_admin2.csv \
		${PWD}/data/networks/sequential_admin2.csv \
		${PWD}/data/mobility_modelling/gravity_exp/all_pairs_model_predictions.rds \
		${PWD}/data/mobility_modelling/gravity_exp/sequential_model_predictions.rds \
		${PWD}/data/mobility_modelling/gravity_power/all_pairs_model_predictions.rds \
		${PWD}/data/mobility_modelling/gravity_power/sequential_model_predictions.rds \
		${PWD}/data/mobility_modelling/radiation_basic/all_pairs_model_predictions.rds \
		${PWD}/data/mobility_modelling/radiation_basic/sequential_model_predictions.rds
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/movement_kernel_comparison.png: ${PWD}/src/compare_mobility_model_kernel.R \
		${PWD}/data/mobility_modelling/mobility_model_predictions.csv
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/movement_raster_comparison.png: ${PWD}/src/compare_mobility_model_raster.R \
		${PWD}/data/mobility_modelling/mobility_model_predictions.csv \
		${PWD}/data/geo/admin2.geojson
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/movement_raster_comparison_gravity_power.png: ${PWD}/src/evaluate_mobility_models.R \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/distance/distance_matrix_admin2.rds \
		${PWD}/data/networks/all_pairs_admin2.csv \
		${PWD}/data/networks/sequential_admin2.csv \
		${PWD}/data/mobility_modelling/gravity_power/all_pairs_model.rds \
		${PWD}/data/mobility_modelling/gravity_power/sequential_model.rds \
		${PWD}/data/mobility_modelling/gravity_power/all_pairs_model_predictions.rds \
		${PWD}/data/mobility_modelling/gravity_power/sequential_model_predictions.rds \
		${PWD}/data/geo/admin2.geojson
	export DIFFERENCE_FILL_SCALE_BREAKS="0,10,100,500,1000" && \
	export DIFFERENCE_YAXIS_SCALE_BREAKS="0,10,100,1000,10000" && \
	export PLOT_REGLINE_AND_COR_FOR_PREDICTIONS="0" && \
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/movement_raster_comparison_gravity_exp.png: ${PWD}/src/evaluate_mobility_models.R \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/distance/distance_matrix_admin2.rds \
		${PWD}/data/networks/all_pairs_admin2.csv \
		${PWD}/data/networks/sequential_admin2.csv \
		${PWD}/data/mobility_modelling/gravity_exp/all_pairs_model.rds \
		${PWD}/data/mobility_modelling/gravity_exp/sequential_model.rds \
		${PWD}/data/mobility_modelling/gravity_exp/all_pairs_model_predictions.rds \
		${PWD}/data/mobility_modelling/gravity_exp/sequential_model_predictions.rds \
		${PWD}/data/geo/admin2.geojson
	export DIFFERENCE_FILL_SCALE_BREAKS="0,10,100,500,1000" && \
	export DIFFERENCE_YAXIS_SCALE_BREAKS="-100,-10,0,10,100,1000,10000" && \
	export PLOT_REGLINE_AND_COR_FOR_PREDICTIONS="0" && \
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/movement_raster_comparison_radiation_basic.png: ${PWD}/src/evaluate_mobility_models.R \
		${PWD}/data/population/population_admin2.csv \
		${PWD}/data/distance/distance_matrix_admin2.rds \
		${PWD}/data/networks/all_pairs_admin2.csv \
		${PWD}/data/networks/sequential_admin2.csv \
		${PWD}/data/mobility_modelling/radiation_basic/all_pairs_model.rds \
		${PWD}/data/mobility_modelling/radiation_basic/sequential_model.rds \
		${PWD}/data/mobility_modelling/radiation_basic/all_pairs_model_predictions.rds \
		${PWD}/data/mobility_modelling/radiation_basic/sequential_model_predictions.rds \
		${PWD}/data/geo/admin2.geojson
	export DIFFERENCE_FILL_SCALE_BREAKS="0,10,100,1000,10000" && \
	export DIFFERENCE_YAXIS_SCALE_BREAKS="-5,0,10,100,1000,10000" && \
	export PLOT_REGLINE_AND_COR_FOR_PREDICTIONS="0" && \
	$(R_INTERPRETER) $^ $@

${PWD}/output/tables/mobility_model_comparison.csv: ${PWD}/src/compare_mobility_models.R \
		${PWD}/data/mobility_modelling/gravity_exp/all_pairs_model.rds \
		${PWD}/data/mobility_modelling/gravity_power/all_pairs_model.rds \
		${PWD}/data/mobility_modelling/radiation_basic/all_pairs_model.rds \
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

# combine national results for focus locs
${PWD}/data/epi_modelling/results/gravity_exp/focus_locs_results_national.csv: ${PWD}/src/combine_epi_model_results.R \
		${PWD}/data/epi_modelling/results/gravity_exp/**/**/infected_fid029*.rds \
		${PWD}/data/epi_modelling/results/gravity_exp/**/**/infected_fid146*.rds \
		${PWD}/data/epi_modelling/results/gravity_exp/**/**/infected_fid164*.rds \
		${PWD}/data/epi_modelling/results/gravity_exp/**/**/infected_fid207*.rds \
		${PWD}/data/epi_modelling/results/gravity_exp/**/**/infected_fid240*.rds
	$(R_INTERPRETER) $^ $@

${PWD}/data/epi_modelling/results/gravity_power/focus_locs_results_national.csv: ${PWD}/src/combine_epi_model_results.R \
		${PWD}/data/epi_modelling/results/gravity_power/**/**/infected_fid029*.rds \
		${PWD}/data/epi_modelling/results/gravity_power/**/**/infected_fid146*.rds \
		${PWD}/data/epi_modelling/results/gravity_power/**/**/infected_fid164*.rds \
		${PWD}/data/epi_modelling/results/gravity_power/**/**/infected_fid207*.rds \
		${PWD}/data/epi_modelling/results/gravity_power/**/**/infected_fid240*.rds
	$(R_INTERPRETER) $^ $@

${PWD}/data/epi_modelling/results/radiation_basic/focus_locs_results_national.csv: ${PWD}/src/combine_epi_model_results.R \
		${PWD}/data/epi_modelling/results/radiation_basic/**/**/infected_fid029*.rds \
		${PWD}/data/epi_modelling/results/radiation_basic/**/**/infected_fid146*.rds \
		${PWD}/data/epi_modelling/results/radiation_basic/**/**/infected_fid164*.rds \
		${PWD}/data/epi_modelling/results/radiation_basic/**/**/infected_fid207*.rds \
		${PWD}/data/epi_modelling/results/radiation_basic/**/**/infected_fid240*.rds
	$(R_INTERPRETER) $^ $@

# combine national results for focus locs
${PWD}/data/epi_modelling/results/gravity_exp/all_locs_results_national.csv: ${PWD}/src/combine_epi_model_results.R \
		${PWD}/data/epi_modelling/results/gravity_exp/**/**/*_trajectory_1.rds
	$(R_INTERPRETER) $^ $@

${PWD}/data/epi_modelling/results/gravity_power/all_locs_results_national.csv: ${PWD}/src/combine_epi_model_results.R \
		${PWD}/data/epi_modelling/results/gravity_power/**/**/*_trajectory_1.rds
	$(R_INTERPRETER) $^ $@

${PWD}/data/epi_modelling/results/radiation_basic/all_locs_results_national.csv: ${PWD}/src/combine_epi_model_results.R \
		${PWD}/data/epi_modelling/results/radiation_basic/**/**/*_trajectory_1.rds
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/gravity_exp_modelled_trajectory.png: ${PWD}/src/plot_modelled_epidemic_curve.R \
		${PWD}/data/geo/pcods_admin2.csv \
		${PWD}/data/epi_modelling/results/gravity_exp/focus_locs_results_national.csv
	export MOBILITY_MODEL_TITLE="Gravity Model (Exponential)" && \
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/gravity_power_modelled_trajectory.png: ${PWD}/src/plot_modelled_epidemic_curve.R \
		${PWD}/data/geo/pcods_admin2.csv \
		${PWD}/data/epi_modelling/results/gravity_power/focus_locs_results_national.csv
	export MOBILITY_MODEL_TITLE="Gravity Model (Power)" && \
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/radiation_basic_modelled_trajectory.png: ${PWD}/src/plot_modelled_epidemic_curve.R \
		${PWD}/data/geo/pcods_admin2.csv \
		${PWD}/data/epi_modelling/results/radiation_basic/focus_locs_results_national.csv
	export MOBILITY_MODEL_TITLE="Radiation Model (Basic)" && \
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/modelled_trajectories_comparison.png: ${PWD}/src/compare_modelled_epi_curve.R \
		${PWD}/data/geo/pcods_admin2.csv \
		${PWD}/data/epi_modelling/results/gravity_power/focus_locs_results_national.csv \
		${PWD}/data/epi_modelling/results/gravity_exp/focus_locs_results_national.csv \
		${PWD}/data/epi_modelling/results/radiation_basic/focus_locs_results_national.csv 
	$(R_INTERPRETER) $^ $@

# Plot time difference for all introduction locations

${PWD}/data/mobility_modelling/peak_time_differences.csv: ${PWD}/src/calculate_peak_time_difference.R \
		${PWD}/data/epi_modelling/results/gravity_exp/all_locs_results_national_peaks.csv \
    ${PWD}/data/epi_modelling/results/gravity_power/all_locs_results_national_peaks.csv \
    ${PWD}/data/epi_modelling/results/radiation_basic/all_locs_results_national_peaks.csv
	$(R_INTERPRETER) $^ $@

${PWD}/output/figures/gravity_exp_time_delay_all_locs.png ${PWD}/output/figures/gravity_power_time_delay_all_locs.png ${PWD}/output/figures/radiation_basic_time_delay_all_locs.png output/figures/R0_1.5_time_delay_all_locs.png &: ${PWD}/src/plot_all_introduction_locations.R \
		${PWD}/data/geo/admin2.geojson \
		${PWD}/data/mobility_modelling/peak_time_differences.csv
	export TIME_DIFFERENCE_COLOR_BREAKS="-Inf,-30,-14,-7,0,7,14,30,Inf" && \
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
