# Define project directory
PROJECT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

# Pull & tag container from GitHub package registry
build:
	docker build . -t ghana_cdr_aggregation

# Start Docker container for development (include volume mounts to data  & output dirs)
up:
	docker run -d -p 8787:8787 \
	--mount type=bind,source=$(PROJECT_DIR),target=/home/rstudio/ghana_cdr_aggregation \
	-w /home/ghana_cdr_aggregation --name ghana_cdr_aggregation -e DISABLE_AUTH=true \
	-e ROOT=true ghana_cdr_aggregation

# Stop and remove Docker container
down:
	docker stop ghana_cdr_aggregation
	docker rm ghana_cdr_aggregation
