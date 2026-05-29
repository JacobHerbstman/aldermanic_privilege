PTFE := TRUE
RTFE := TRUE
PORCH := TRUE
CAFE := FALSE
TWO_STAGE := TRUE
STAGE2_WEIGHT := N_PERMITS
VOLUME_CTRL := LAG1
VOLUME_STAGE := BOTH
PAPER_CUTOFF := 2022
POST_2023_CUTOFF := 2026-04

PAPER_SPEC := ptfe$(PTFE)_rtfe$(RTFE)_porch$(PORCH)_cafe$(CAFE)_2stage_vol$(VOLUME_CTRL)_$(VOLUME_STAGE)_through2022
POST_2023_SPEC := ptfe$(PTFE)_rtfe$(RTFE)_porch$(PORCH)_cafe$(CAFE)_2stage_vol$(VOLUME_CTRL)_$(VOLUME_STAGE)_through202604

all: ../output/alderman_uncertainty_index_$(PAPER_SPEC).csv \
	../output/uncertainty_index_$(PAPER_SPEC).pdf \
	../output/stage1_regression_$(PAPER_SPEC).tex \
	../output/alderman_uncertainty_index_$(POST_2023_SPEC).csv

../output/alderman_uncertainty_index_$(PAPER_SPEC).csv: create_uncertainty_index.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	MAX_PERMIT_CUTOFF=$(PAPER_CUTOFF) PAPER_BUNDLE=TRUE Rscript $< $(PTFE) $(RTFE) $(PORCH) $(CAFE) $(TWO_STAGE) $(STAGE2_WEIGHT) $(VOLUME_CTRL) $(VOLUME_STAGE)

../output/uncertainty_index_$(PAPER_SPEC).pdf \
../output/stage1_regression_$(PAPER_SPEC).tex: ../output/alderman_uncertainty_index_$(PAPER_SPEC).csv
	@test -s "$@"

../output/alderman_uncertainty_index_$(POST_2023_SPEC).csv: create_uncertainty_index.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	MAX_PERMIT_CUTOFF=$(POST_2023_CUTOFF) PAPER_BUNDLE=FALSE Rscript $< $(PTFE) $(RTFE) $(PORCH) $(CAFE) $(TWO_STAGE) $(STAGE2_WEIGHT) $(VOLUME_CTRL) $(VOLUME_STAGE)

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv

include ../../generic.make
