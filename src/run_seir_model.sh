#!/bin/bash
input_file=$1
echo "Processing $input_file on $(hostname)"
eval "$(conda shell.bash hook)"
conda activate Renv
Rscript ghana_cdr_aggregation/src/run_seir_model.R $input_file