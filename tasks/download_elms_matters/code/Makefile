include ../../shared/code/shell_functions.make

all: ../output/elms_matter_index.jsonl ../output/elms_matter_index_requests.csv \
	../output/elms_zoning_matter_details.jsonl ../output/elms_zoning_matter_requests.csv

../%/elms_matter_index.jsonl ../%/elms_matter_index_requests.csv: download_elms_matter_index.R | ../output ../report ../temp
	$(R) $<

../%/elms_zoning_matter_details.jsonl ../%/elms_zoning_matter_requests.csv: download_elms_zoning_details.R \
	../output/elms_matter_index.jsonl | ../output ../report ../temp
	$(R) $<

include ../../shared/code/generic.make
