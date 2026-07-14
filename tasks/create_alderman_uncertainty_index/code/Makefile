PTFE := TRUE
RTFE := TRUE
PORCH := TRUE
CAFE := FALSE
TWO_STAGE := TRUE
STAGE2_WEIGHT := N_PERMITS
VOLUME_CTRL := LAG1
VOLUME_STAGE := BOTH
PAPER_CUTOFF := 2022
FROZEN_CUTOFF := 2014
POST_2023_CUTOFF := 2026-04

PAPER_CUTOFF_LABEL := through$(subst -,,$(PAPER_CUTOFF))
FROZEN_CUTOFF_LABEL := through$(subst -,,$(FROZEN_CUTOFF))
POST_2023_CUTOFF_LABEL := through$(subst -,,$(POST_2023_CUTOFF))
PAPER_SPEC := ptfe$(PTFE)_rtfe$(RTFE)_porch$(PORCH)_cafe$(CAFE)_2stage_vol$(VOLUME_CTRL)_$(VOLUME_STAGE)_$(PAPER_CUTOFF_LABEL)
FROZEN_SPEC := ptfe$(PTFE)_rtfe$(RTFE)_porch$(PORCH)_cafe$(CAFE)_2stage_vol$(VOLUME_CTRL)_$(VOLUME_STAGE)_$(FROZEN_CUTOFF_LABEL)
POST_2023_SPEC := ptfe$(PTFE)_rtfe$(RTFE)_porch$(PORCH)_cafe$(CAFE)_2stage_vol$(VOLUME_CTRL)_$(VOLUME_STAGE)_$(POST_2023_CUTOFF_LABEL)

all: \
	../output/alderman_uncertainty_index_$(PAPER_SPEC).csv \
	../output/uncertainty_index_$(PAPER_SPEC).pdf \
	../output/stage1_regression_$(PAPER_SPEC).tex \
	../output/alderman_uncertainty_index_$(FROZEN_SPEC).csv \
	../output/alderman_uncertainty_index_$(POST_2023_SPEC).csv

../output/alderman_uncertainty_index_$(PAPER_SPEC).csv: create_uncertainty_index.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript $< $(PTFE) $(RTFE) $(PORCH) $(CAFE) $(TWO_STAGE) $(STAGE2_WEIGHT) $(VOLUME_CTRL) $(VOLUME_STAGE) $(PAPER_CUTOFF) TRUE

../output/uncertainty_index_$(PAPER_SPEC).pdf: create_uncertainty_index.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv ../output/alderman_uncertainty_index_$(PAPER_SPEC).csv | ../output
	@test -s "$@"

../output/stage1_regression_$(PAPER_SPEC).tex: create_uncertainty_index.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv ../output/alderman_uncertainty_index_$(PAPER_SPEC).csv | ../output
	@test -s "$@"

../output/alderman_uncertainty_index_$(FROZEN_SPEC).csv: create_uncertainty_index.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript $< $(PTFE) $(RTFE) $(PORCH) $(CAFE) $(TWO_STAGE) $(STAGE2_WEIGHT) $(VOLUME_CTRL) $(VOLUME_STAGE) $(FROZEN_CUTOFF) FALSE

../output/alderman_uncertainty_index_$(POST_2023_SPEC).csv: create_uncertainty_index.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript $< $(PTFE) $(RTFE) $(PORCH) $(CAFE) $(TWO_STAGE) $(STAGE2_WEIGHT) $(VOLUME_CTRL) $(VOLUME_STAGE) $(POST_2023_CUTOFF) FALSE

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv

include ../../generic.make
