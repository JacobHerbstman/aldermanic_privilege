all: ../output/old_zoning_bulk_density_1957_2004.csv

../output/old_zoning_bulk_density_1957_2004.csv: build_old_zoning_lookup.py ../input/chicago_zoning_code1957.txt ../input/zoning_conversion_2004_crosswalk.csv | ../output run.sbatch
	$(PYTHON) $< \
		--in-old-zoning-txt ../input/chicago_zoning_code1957.txt \
		--in-crosswalk-csv ../input/zoning_conversion_2004_crosswalk.csv \
		--out-bulk-csv ../output/old_zoning_bulk_density_1957_2004.csv

../input/chicago_zoning_code1957.txt: ../../../data_raw/chicago_zoning_code1957.txt | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/zoning_conversion_2004_crosswalk.csv: ../../../data_raw/zoning_conversion_2004_crosswalk.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/chicago_zoning_code1957.txt ../input/zoning_conversion_2004_crosswalk.csv

include ../../shell_functions.make
include ../../generic.make
