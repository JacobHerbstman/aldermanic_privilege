all: ../output/pdf/parcel_land_redistricting_memo.pdf

link-inputs: \
	../input/parcel_land_redistricting_panel.parquet \
	../input/parcel_land_redistricting_support.csv \
	../input/parcel_land_citywide_taxyear_summary.csv \
	../input/parcel_land_geography_taxyear_summary.csv \
	../input/parcel_land_citywide_series_panel.png \
	../input/parcel_land_ward_heatmap_panel.png \
	../input/parcel_land_community_area_heatmap_panel.png

../input:
	mkdir -p ../input

../output:
	mkdir -p ../output

../output/pdf:
	mkdir -p ../output/pdf

../input/parcel_land_redistricting_panel.parquet: ../../create_parcel_land_redistricting_dataset/output/parcel_land_redistricting_panel.parquet | ../input
	ln -sf ../../create_parcel_land_redistricting_dataset/output/parcel_land_redistricting_panel.parquet ../input/parcel_land_redistricting_panel.parquet

../input/parcel_land_redistricting_support.csv: ../../create_parcel_land_redistricting_dataset/output/parcel_land_redistricting_support.csv | ../input
	ln -sf ../../create_parcel_land_redistricting_dataset/output/parcel_land_redistricting_support.csv ../input/parcel_land_redistricting_support.csv

../input/parcel_land_citywide_taxyear_summary.csv: ../../parcel_land_redistricting_sanity_checks/output/parcel_land_citywide_taxyear_summary.csv | ../input
	ln -sf ../../parcel_land_redistricting_sanity_checks/output/parcel_land_citywide_taxyear_summary.csv ../input/parcel_land_citywide_taxyear_summary.csv

../input/parcel_land_geography_taxyear_summary.csv: ../../parcel_land_redistricting_sanity_checks/output/parcel_land_geography_taxyear_summary.csv | ../input
	ln -sf ../../parcel_land_redistricting_sanity_checks/output/parcel_land_geography_taxyear_summary.csv ../input/parcel_land_geography_taxyear_summary.csv

../input/parcel_land_citywide_series_panel.png: ../../parcel_land_redistricting_sanity_checks/output/parcel_land_citywide_series_panel.png | ../input
	ln -sf ../../parcel_land_redistricting_sanity_checks/output/parcel_land_citywide_series_panel.png ../input/parcel_land_citywide_series_panel.png

../input/parcel_land_ward_heatmap_panel.png: ../../parcel_land_redistricting_sanity_checks/output/parcel_land_ward_heatmap_panel.png | ../input
	ln -sf ../../parcel_land_redistricting_sanity_checks/output/parcel_land_ward_heatmap_panel.png ../input/parcel_land_ward_heatmap_panel.png

../input/parcel_land_community_area_heatmap_panel.png: ../../parcel_land_redistricting_sanity_checks/output/parcel_land_community_area_heatmap_panel.png | ../input
	ln -sf ../../parcel_land_redistricting_sanity_checks/output/parcel_land_community_area_heatmap_panel.png ../input/parcel_land_community_area_heatmap_panel.png

../output/memo_stats.tex: build_memo_inputs.R ../input/parcel_land_redistricting_panel.parquet ../input/parcel_land_redistricting_support.csv ../input/parcel_land_citywide_taxyear_summary.csv ../input/parcel_land_geography_taxyear_summary.csv | ../output
	Rscript build_memo_inputs.R ../input/parcel_land_redistricting_panel.parquet ../input/parcel_land_redistricting_support.csv ../input/parcel_land_citywide_taxyear_summary.csv ../input/parcel_land_geography_taxyear_summary.csv ../output/memo_stats.tex

../output/pdf/parcel_land_redistricting_memo.pdf: parcel_land_redistricting_memo.tex ../output/memo_stats.tex ../input/parcel_land_citywide_series_panel.png ../input/parcel_land_ward_heatmap_panel.png ../input/parcel_land_community_area_heatmap_panel.png | ../output/pdf
	pdflatex -interaction=nonstopmode -halt-on-error -output-directory ../output/pdf parcel_land_redistricting_memo.tex
	pdflatex -interaction=nonstopmode -halt-on-error -output-directory ../output/pdf parcel_land_redistricting_memo.tex
