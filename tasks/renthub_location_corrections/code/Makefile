include ../../shared/code/shell_functions.make

START_DATE ?= 2014-01-01
END_DATE ?= 2022-12-31

all: ../output/chicago_rent_panel_location_corrections.parquet

../output/chicago_rent_panel_location_corrections.parquet: build_renthub_location_corrections.R ../input/manual_verified_address_locations.csv ../input/renthub_manifest.csv ../input/chicago_rent_panel.parquet Makefile ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../input/renthub_raw ../report
	$(R) $< $(START_DATE) $(END_DATE)

../input/renthub_raw: | ../../download_rent_data/output/renthub_manifest.csv ../input
	ln -sfn ../../download_rent_data/output $@

../input/renthub_manifest.csv: ../../download_rent_data/output/renthub_manifest.csv | ../input
	ln -sf $< $@

../input/chicago_rent_panel.parquet: ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	ln -sf $< $@

../input/manual_verified_address_locations.csv: ../../renthub_location_hand_adjudications/output/manual_verified_address_locations.csv | ../input
	ln -sf $< $@

link-inputs: ../input/renthub_manifest.csv ../input/renthub_raw ../input/chicago_rent_panel.parquet ../input/manual_verified_address_locations.csv

include ../../shared/code/generic.make
