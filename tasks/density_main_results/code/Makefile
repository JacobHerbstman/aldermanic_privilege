include ../../shared/code/shell_functions.make

all: ../output/density_rd.pdf ../output/density_sample_summary.csv ../output/density_sample_summary.tex

../output/density_rd.pdf: build_density_main_results.R ../input/new_construction_analysis_data.csv | ../output ../report
	$(R) $<

../%/density_sample_summary.csv ../%/density_sample_summary.tex: build_density_summary.R ../input/permit_construction.csv \
	../input/new_construction_analysis_data.csv ../input/boundary_segments_1320ft.gpkg | ../output ../report
	$(R) $<

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@
../input/permit_construction.csv: ../../prepare_permit_construction/output/permit_construction.csv | ../input
	ln -sf $< $@
../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/new_construction_analysis_data.csv ../input/permit_construction.csv ../input/boundary_segments_1320ft.gpkg

include ../../shared/code/generic.make
