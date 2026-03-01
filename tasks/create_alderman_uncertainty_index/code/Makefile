SHELL := bash
include ../../shell_functions.make

PTFE := TRUE
RTFE := TRUE
PORCH := TRUE
CAFE := FALSE
TWO_STAGE := TRUE
STAGE2_WEIGHT := N_PERMITS
VOLUME_CTRL := LAG1
VOLUME_STAGE := BOTH

SPEC := ptfe$(PTFE)_rtfe$(RTFE)_porch$(PORCH)_cafe$(CAFE)_2stage_vol$(VOLUME_CTRL)_$(VOLUME_STAGE)

all: ../output/alderman_uncertainty_index_$(SPEC).csv ../output/uncertainty_index_$(SPEC).pdf ../output/stage1_regression_$(SPEC).tex ../output/stage2_regression_$(SPEC).tex

../output/alderman_uncertainty_index_$(SPEC).csv \
../output/uncertainty_index_$(SPEC).pdf \
../output/stage1_regression_$(SPEC).tex \
../output/stage2_regression_$(SPEC).tex: ../output/.build_$(SPEC).done

../output/.build_$(SPEC).done: create_uncertainty_index.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript create_uncertainty_index.R $(PTFE) $(RTFE) $(PORCH) $(CAFE) $(TWO_STAGE) $(STAGE2_WEIGHT) $(VOLUME_CTRL) $(VOLUME_STAGE)
	touch "$@"

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv

include ../../generic.make
