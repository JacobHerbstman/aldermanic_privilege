include ../../shared/code/shell_functions.make

all: ../output/zoning_refilings.csv

../output/zoning_refilings.csv: link_refilings.R ../input/zoning_map_amendments.csv ../adjudication/refiling_reviews.csv \
	| ../output ../report
	$(R) $<

../input/zoning_map_amendments.csv: ../../clean_zoning_map_amendments/output/zoning_map_amendments.csv | ../input
	ln -sf $< $@

link-inputs: ../input/zoning_map_amendments.csv

include ../../shared/code/generic.make
