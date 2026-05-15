SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
MAIN_BW_M ?= 152.4
MAIN_BW_LABEL ?= 500ft
APPENDIX_BW_M ?= 152.4
APPENDIX_BW_LABEL ?= 500ft
BINS_PER_SIDE ?= 5
YVARS := density_far density_dupac

all: \
	../output/nonparametric_rd_density_linear_display_log_density_far_$(MAIN_BW_LABEL)_all_bins$(BINS_PER_SIDE).pdf \
	../output/nonparametric_rd_density_linear_display_log_density_dupac_$(MAIN_BW_LABEL)_all_bins$(BINS_PER_SIDE).pdf \
	../output/nonparametric_rd_density_linear_display_log_density_far_$(APPENDIX_BW_LABEL)_multifamily_bins$(BINS_PER_SIDE).pdf \
	../output/nonparametric_rd_density_linear_display_log_density_dupac_$(APPENDIX_BW_LABEL)_multifamily_bins$(BINS_PER_SIDE).pdf \
	../output/nonparametric_rd_density_linear_display_4panel_$(MAIN_BW_LABEL)_all_multifamily_bins$(BINS_PER_SIDE).pdf \
	../output/nonparametric_rd_density_linear_display_4panel_levels_$(MAIN_BW_LABEL)_all_multifamily_bins$(BINS_PER_SIDE).pdf

../output/nonparametric_rd_density_linear_display_log_density_far_$(MAIN_BW_LABEL)_all_bins$(BINS_PER_SIDE).pdf: nonparametric_rd_density_linear_display.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	DISTANCE_DISPLAY_UNIT=ft Rscript nonparametric_rd_density_linear_display.R density_far $(MAIN_BW_M) all $(FE_SPEC) $(BINS_PER_SIDE) ../input/parcels_with_ward_distances.csv "$@"

../output/nonparametric_rd_density_linear_display_log_density_dupac_$(MAIN_BW_LABEL)_all_bins$(BINS_PER_SIDE).pdf: nonparametric_rd_density_linear_display.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	DISTANCE_DISPLAY_UNIT=ft Rscript nonparametric_rd_density_linear_display.R density_dupac $(MAIN_BW_M) all $(FE_SPEC) $(BINS_PER_SIDE) ../input/parcels_with_ward_distances.csv "$@"

../output/nonparametric_rd_density_linear_display_log_density_far_$(APPENDIX_BW_LABEL)_multifamily_bins$(BINS_PER_SIDE).pdf: nonparametric_rd_density_linear_display.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	DISTANCE_DISPLAY_UNIT=ft Rscript nonparametric_rd_density_linear_display.R density_far $(APPENDIX_BW_M) multifamily $(FE_SPEC) $(BINS_PER_SIDE) ../input/parcels_with_ward_distances.csv "$@"

../output/nonparametric_rd_density_linear_display_log_density_dupac_$(APPENDIX_BW_LABEL)_multifamily_bins$(BINS_PER_SIDE).pdf: nonparametric_rd_density_linear_display.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	DISTANCE_DISPLAY_UNIT=ft Rscript nonparametric_rd_density_linear_display.R density_dupac $(APPENDIX_BW_M) multifamily $(FE_SPEC) $(BINS_PER_SIDE) ../input/parcels_with_ward_distances.csv "$@"

../output/nonparametric_rd_density_linear_display_4panel_$(MAIN_BW_LABEL)_all_multifamily_bins$(BINS_PER_SIDE).pdf: nonparametric_rd_density_linear_display_combined.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	DISTANCE_DISPLAY_UNIT=ft Rscript nonparametric_rd_density_linear_display_combined.R $(MAIN_BW_M) $(MAIN_BW_LABEL) $(FE_SPEC) $(BINS_PER_SIDE)

../output/nonparametric_rd_density_linear_display_4panel_levels_$(MAIN_BW_LABEL)_all_multifamily_bins$(BINS_PER_SIDE).pdf: nonparametric_rd_density_linear_display_combined.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	DISTANCE_DISPLAY_UNIT=ft Rscript nonparametric_rd_density_linear_display_combined.R $(MAIN_BW_M) $(MAIN_BW_LABEL) $(FE_SPEC) $(BINS_PER_SIDE) level

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
