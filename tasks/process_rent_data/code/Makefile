include ../../shared/code/shell_functions.make

START_DATE ?= 2014-01-01
END_DATE ?= 2022-12-31

all: ../output/chicago_rent_panel.parquet

../output/chicago_rent_panel.parquet: build_renthub_clean_panel.R ../input/renthub_manifest.csv Makefile ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../input/renthub_raw ../report
	$(R) $< $(START_DATE) $(END_DATE)

../input/renthub_manifest.csv: ../../download_rent_data/output/renthub_manifest.csv | ../input
	ln -sf $< $@

../input/renthub_raw: | ../../download_rent_data/output/renthub_manifest.csv ../input
	ln -sfn ../../download_rent_data/output $@

link-inputs: ../input/renthub_manifest.csv ../input/renthub_raw

include ../../shared/code/generic.make
