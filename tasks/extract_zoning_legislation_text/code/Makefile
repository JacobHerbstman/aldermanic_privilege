include ../../shared/code/shell_functions.make

all: ../output/zoning_legislation_text.parquet

../output/zoning_legislation_text.parquet: extract_legislation_text.R ../input/elms_zoning_legislation_files.csv \
	../input/zoning_legislation_files | ../output ../report ../temp
	$(R) $<

../input/elms_zoning_legislation_files.csv: ../../download_elms_matters/output/elms_zoning_legislation_files.csv | ../input
	ln -sf $< $@
# The folder of files is complete once the download has written its file record.
../input/zoning_legislation_files: ../../download_elms_matters/output/elms_zoning_legislation_files.csv | ../input
	ln -sfn ../../download_elms_matters/output/zoning_legislation_files $@

link-inputs: ../input/elms_zoning_legislation_files.csv ../input/zoning_legislation_files

include ../../shared/code/generic.make
