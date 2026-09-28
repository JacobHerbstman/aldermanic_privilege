include ../../shared/code/shell_functions.make

all: ../output/alderman_zoning_measures.csv ../output/alderman_zoning_measures_by_half.csv

../%/alderman_zoning_measures.csv ../%/alderman_zoning_measures_by_half.csv: estimate_alderman_zoning_measures.R \
	../input/zoning_map_amendments.csv ../input/zoning_amendment_wards.csv ../input/alderman_terms.csv | ../output ../report
	$(R) $<

../input/zoning_map_amendments.csv: ../../clean_zoning_map_amendments/output/zoning_map_amendments.csv | ../input
	ln -sf $< $@
../input/zoning_amendment_wards.csv: ../../assign_zoning_amendment_wards/output/zoning_amendment_wards.csv | ../input
	ln -sf $< $@
../input/alderman_terms.csv: ../../create_alderman_data/adjudication/alderman_terms.csv | ../input
	ln -sf $< $@

link-inputs: ../input/zoning_map_amendments.csv ../input/zoning_amendment_wards.csv ../input/alderman_terms.csv

include ../../shared/code/generic.make
