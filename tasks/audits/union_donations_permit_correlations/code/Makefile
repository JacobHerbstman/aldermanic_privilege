all: \
	../output/union_donations_permit_correlations.csv \
	../output/union_donations_permit_summary_stats.csv \
	../output/union_donations_permit_join.csv \
	../output/union_donations_permit_top_bottom.csv \
	../output/union_donations_permit_unmatched_aldermen.csv \
	../output/union_share_processing_time_scatter.png \
	../output/union_share_permit_counts_scatter.png \
	../output/union_share_units_density_scatter.png \
	../output/union_share_processing_time_scatter.pdf \
	../output/union_share_permit_counts_scatter.pdf \
	../output/union_share_units_density_scatter.pdf

../output/union_donations_permit_correlations.csv: build_union_permit_correlations.R \
	../input/alderman_union_donations_main.csv \
	../input/permits_for_uncertainty_index.csv \
	../input/geocoded_residential_data.gpkg \
	../input/ward_panel.gpkg \
	../input/chicago_alderman_panel.csv | ../output
	Rscript $<

../output/union_donations_permit_summary_stats.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_donations_permit_join.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_donations_permit_top_bottom.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_donations_permit_unmatched_aldermen.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_processing_time_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_permit_counts_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_units_density_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_processing_time_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_permit_counts_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_units_density_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../input/alderman_union_donations_main.csv: ../../../alderman_union_candidate_aggregates_cycles/output/alderman_union_donations_main.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permits_for_uncertainty_index.csv: ../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/geocoded_residential_data.gpkg: ../../../geocode_ccao_data/output/geocoded_residential_data.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/alderman_union_donations_main.csv \
	../input/permits_for_uncertainty_index.csv \
	../input/geocoded_residential_data.gpkg \
	../input/ward_panel.gpkg \
	../input/chicago_alderman_panel.csv

include ../../../generic.make
