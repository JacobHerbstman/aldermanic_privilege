include ../../shared/code/shell_functions.make

all: ../output/rent_pre_scores_full.parquet

../output/rent_pre_scores_full.parquet: calculate_rent_distances.R ../../shared/code/canonical_geometry_helpers.R ../input/chicago_rent_panel.parquet ../input/chicago_rent_panel_location_corrections.parquet ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/chicago_alderman_panel.csv ../input/fred_cpi_cuura207sa0.csv ../../setup_environment/code/packages.R Makefile ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../input/chicago_rent_panel.parquet: ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	ln -sf $< $@

../input/chicago_rent_panel_location_corrections.parquet: ../../renthub_location_corrections/output/chicago_rent_panel_location_corrections.parquet | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/fred_cpi_cuura207sa0.csv: ../../download_fred_cpi/output/fred_cpi_cuura207sa0.csv | ../input
	ln -sf $< $@

link-inputs: \
	../input/chicago_rent_panel.parquet \
	../input/chicago_rent_panel_location_corrections.parquet \
	../input/ward_panel.gpkg \
	../input/ward_pair_boundaries.gpkg \
	../input/chicago_alderman_panel.csv \
	../input/fred_cpi_cuura207sa0.csv

include ../../shared/code/generic.make
