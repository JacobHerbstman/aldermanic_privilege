PTFE := TRUE
RTFE := TRUE
PORCH := TRUE
CAFE := FALSE
TWO_STAGE := TRUE
STAGE2_WEIGHT := N_PERMITS
VOLUME_CTRL := LAG1
VOLUME_STAGE := BOTH
MAX_PERMIT_YEAR ?= 2022
MAX_PERMIT_MONTH ?=

ifeq ($(strip $(MAX_PERMIT_MONTH)),)
CUTOFF_LABEL := $(MAX_PERMIT_YEAR)
else
CUTOFF_LABEL := $(subst -,,$(MAX_PERMIT_MONTH))
endif

SPEC := ptfe$(PTFE)_rtfe$(RTFE)_porch$(PORCH)_cafe$(CAFE)_2stage_vol$(VOLUME_CTRL)_$(VOLUME_STAGE)_through$(CUTOFF_LABEL)

all: ../output/alderman_uncertainty_index_$(SPEC).csv \
	../output/uncertainty_index_$(SPEC).pdf \
	../output/stage1_regression_$(SPEC).tex \
	../output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through202604.csv

../output/alderman_uncertainty_index_$(SPEC).csv: create_uncertainty_index.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	MAX_PERMIT_YEAR=$(MAX_PERMIT_YEAR) MAX_PERMIT_MONTH=$(MAX_PERMIT_MONTH) Rscript create_uncertainty_index.R $(PTFE) $(RTFE) $(PORCH) $(CAFE) $(TWO_STAGE) $(STAGE2_WEIGHT) $(VOLUME_CTRL) $(VOLUME_STAGE)

../output/uncertainty_index_$(SPEC).pdf \
../output/stage1_regression_$(SPEC).tex: ../output/alderman_uncertainty_index_$(SPEC).csv
	@test -s "$@"

../output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through202604.csv: create_uncertainty_index.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	SCORE_ONLY=TRUE MAX_PERMIT_YEAR=2026 MAX_PERMIT_MONTH=2026-04 Rscript create_uncertainty_index.R TRUE TRUE TRUE FALSE TRUE $(STAGE2_WEIGHT) $(VOLUME_CTRL) $(VOLUME_STAGE)

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv

include ../../generic.make
