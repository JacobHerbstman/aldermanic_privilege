SHELL := bash

CASE_BW_M ?= 100
CASE_BW_FT := $(shell Rscript -e 'cat(round($(CASE_BW_M) / 0.3048))')
CASE_BIN_M ?= 10
CASE_BIN_FT := $(shell Rscript -e 'cat(round($(CASE_BIN_M) / 0.3048))')
BORDERS ?= 1_26,11_25

all: ../output/case_study_far_panel_$(CASE_BW_M)m.pdf ../output/case_study_summary_$(CASE_BW_M)m.csv

../output/case_study_far_panel_$(CASE_BW_M)m.pdf: build_case_study_visuals.R ../input/parcels_with_ward_distances.csv | ../output
	# arg order: bw_ft bw_label borders bin_ft axis_units output_pdf output_csv
	Rscript build_case_study_visuals.R \
		$(CASE_BW_FT) \
		$(CASE_BW_M)m \
		$(BORDERS) \
		$(CASE_BIN_FT) \
		meters \
		../output/case_study_far_panel_$(CASE_BW_M)m.pdf \
		../output/case_study_summary_$(CASE_BW_M)m.csv

../output/case_study_summary_$(CASE_BW_M)m.csv: ../output/case_study_far_panel_$(CASE_BW_M)m.pdf
	@test -s "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"


link-inputs: ../input/parcels_with_ward_distances.csv

include ../../shell_functions.make
include ../../generic.make
