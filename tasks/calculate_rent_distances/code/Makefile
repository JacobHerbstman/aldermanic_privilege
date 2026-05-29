SAMPLE := FALSE

all: ../output/rent_pre_scores_full.parquet

../output/rent_pre_scores_full.parquet: calculate_rent_distances.R ../../_lib/canonical_geometry_helpers.R ../input/chicago_rent_panel.parquet ../input/chicago_rent_panel_quality_flags.parquet ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/chicago_alderman_panel.csv ../input/fred_cpi_cuura207sa0.csv | ../output
	Rscript $< $(SAMPLE)

../input/chicago_rent_panel.parquet: ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_rent_panel_quality_flags.parquet: ../../renthub_quality_flags/output/chicago_rent_panel_quality_flags.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/fred_cpi_cuura207sa0.csv: ../../download_fred_cpi/output/fred_cpi_cuura207sa0.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/chicago_rent_panel.parquet \
	../input/chicago_rent_panel_quality_flags.parquet \
	../input/ward_panel.gpkg \
	../input/ward_pair_boundaries.gpkg \
	../input/chicago_alderman_panel.csv \
	../input/fred_cpi_cuura207sa0.csv

include ../../generic.make
