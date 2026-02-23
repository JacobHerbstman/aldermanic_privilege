SHELL := bash
include ../../shell_functions.make

SCRIPT := create_uncertainty_index.R
INPUT := ../input/permits_for_uncertainty_index.csv

# Active uncertainty specification used throughout slides_uncertainty.
PTFE := FALSE
RTFE := FALSE
PORCH := TRUE
CAFE := FALSE
TWO_STAGE := TRUE
STAGE2_WEIGHT := N_PERMITS
VOLUME_CTRL := NONE
VOLUME_STAGE := STAGE1

SPEC := ptfe$(PTFE)_rtfe$(RTFE)_porch$(PORCH)_cafe$(CAFE)_2stage
CSV_OUT := ../output/alderman_uncertainty_index_$(SPEC).csv
PLOT_OUT := ../output/uncertainty_index_$(SPEC).pdf
STAGE1_OUT := ../output/stage1_regression_$(SPEC).tex
STAGE2_OUT := ../output/stage2_regression_$(SPEC).tex
STAMP := ../output/.build_$(SPEC).done

.DEFAULT_GOAL := all
.PHONY: all link-inputs sweep-fe16-lag1-both

all: $(CSV_OUT) $(PLOT_OUT) $(STAGE1_OUT) $(STAGE2_OUT)

$(CSV_OUT) $(PLOT_OUT) $(STAGE1_OUT) $(STAGE2_OUT): $(STAMP)

$(STAMP): $(SCRIPT) $(INPUT) | ../output
	# arg order: permit_type_fe review_type_fe include_porch ca_fe two_stage stage2_weight volume_ctrl volume_stage
	Rscript $(SCRIPT) \
		$(PTFE) \
		$(RTFE) \
		$(PORCH) \
		$(CAFE) \
		$(TWO_STAGE) \
		$(STAGE2_WEIGHT) \
		$(VOLUME_CTRL) \
		$(VOLUME_STAGE)
	touch "$@"

$(INPUT): ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUT)

sweep-fe16-lag1-both: $(SCRIPT) $(INPUT) | ../output
	@for ptfe in FALSE TRUE; do \
		for rtfe in FALSE TRUE; do \
			for cafe in FALSE TRUE; do \
				for porch in FALSE TRUE; do \
					echo "Running spec: ptfe=$$ptfe rtfe=$$rtfe porch=$$porch cafe=$$cafe volume=LAG1 stage=BOTH"; \
					Rscript $(SCRIPT) "$$ptfe" "$$rtfe" "$$porch" "$$cafe" TRUE N_PERMITS LAG1 BOTH; \
				done; \
			done; \
		done; \
	done

include ../../generic.make
