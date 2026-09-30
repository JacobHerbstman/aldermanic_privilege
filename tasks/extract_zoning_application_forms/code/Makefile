include ../../shared/code/shell_functions.make

all: ../output/application_form_pages.parquet ../output/attached_application_pages.parquet \
	../output/application_form_fields.csv

../output/application_form_pages.parquet: ocr_application_forms.R ../input/elms_zoning_legislation_files.csv \
	../input/zoning_legislation_text.parquet ../input/zoning_legislation_files | ../output ../report ../temp
	$(R) $<

../output/attached_application_pages.parquet: ocr_attached_applications.R ../input/elms_zoning_application_files.csv \
	../input/zoning_application_files | ../output ../report ../temp
	$(R) $<

../output/application_form_fields.csv: parse_application_forms.R ../output/application_form_pages.parquet \
	../output/attached_application_pages.parquet ../input/zoning_map_amendments.csv \
	../adjudication/application_form_reads.csv | ../output ../report
	$(R) $<

../input/elms_zoning_legislation_files.csv: ../../download_elms_matters/output/elms_zoning_legislation_files.csv | ../input
	ln -sf $< $@
../input/zoning_legislation_text.parquet: ../../extract_zoning_legislation_text/output/zoning_legislation_text.parquet | ../input
	ln -sf $< $@
# The folder of files is complete once the download has written its file record.
../input/zoning_legislation_files: ../../download_elms_matters/output/elms_zoning_legislation_files.csv | ../input
	ln -sfn ../../download_elms_matters/output/zoning_legislation_files $@
../input/zoning_map_amendments.csv: ../../clean_zoning_map_amendments/output/zoning_map_amendments.csv | ../input
	ln -sf $< $@
../input/elms_zoning_application_files.csv: ../../download_elms_matters/output/elms_zoning_application_files.csv \
	| ../input
	ln -sf $< $@
../input/zoning_application_files: ../../download_elms_matters/output/elms_zoning_application_files.csv | ../input
	ln -sfn ../../download_elms_matters/output/zoning_application_files $@

link-inputs: ../input/elms_zoning_legislation_files.csv ../input/zoning_legislation_text.parquet \
	../input/zoning_legislation_files ../input/elms_zoning_application_files.csv ../input/zoning_application_files \
	../input/zoning_map_amendments.csv

include ../../shared/code/generic.make
