include ../../shared/code/shell_functions.make

START_DATE ?= 2014-01-01
END_DATE ?= 2022-12-31

all: ../output/renthub_manifest.csv

../output/renthub_manifest.csv: download_rent_data.R ../input/renthub_manifest.csv Makefile $(addprefix ../output/,$(shell tail -n +2 ../sources/renthub_manifest.csv | cut -d, -f2)) | ../output ../report
	$(R) $< $(START_DATE) $(END_DATE)

$(addprefix ../%/,$(shell tail -n +2 ../sources/renthub_manifest.csv | cut -d, -f2)): | download_rent_data.R ../input/renthub_manifest.csv Makefile ../output ../temp ../report
	$(R) download_rent_data.R $(START_DATE) $(END_DATE)

../input/renthub_manifest.csv: ../sources/renthub_manifest.csv | ../input
	ln -sf $< $@

link-inputs: ../input/renthub_manifest.csv

include ../../shared/code/generic.make
