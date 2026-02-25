all: ../output/ward_permits_by_year.pdf ../output/transition_scan.txt

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	ln -sf "$(abspath $<)" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$(abspath $<)" "$@"

link-inputs: ../input/building_permits_clean.gpkg ../input/chicago_alderman_panel.csv

# ward_summary_table.txt produced as side effect of the same run
../output/ward_permits_by_year.pdf: ward49_development_summary.R ../input/building_permits_clean.gpkg
	Rscript $< \
		--permits ../input/building_permits_clean.gpkg \
		--out_plot ../output/ward_permits_by_year.pdf \
		--out_table ../output/ward_summary_table.txt

../output/transition_scan.txt: scan_transitions.R ../input/building_permits_clean.gpkg ../input/chicago_alderman_panel.csv
	Rscript $< \
		--permits ../input/building_permits_clean.gpkg \
		--panel   ../input/chicago_alderman_panel.csv \
		--out     ../output/transition_scan.txt

../input:
	mkdir -p $@
