PYTHON_INTERPRETER = python3
R_INTERPRETER = /usr/local/bin/Rscript

.PHONY: default

default: \
	${PWD}/output/figures/movement_raster_comparison.png \
	${PWD}/output/figures/peak_infected_proportion_boxplot.png \
	${PWD}/output/figures/modelled_trajectory.png

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
