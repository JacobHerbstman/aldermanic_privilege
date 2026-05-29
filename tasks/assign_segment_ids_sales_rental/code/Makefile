SEGMENT_LENGTH_FT ?= 1320
SEGMENT_BUFFER_M ?= 457.2

all: \
	../output/sales_pre_scores_with_segments.csv \
	../output/rent_pre_scores_full_with_segments.parquet

../output/rent_pre_scores_full_with_segments.parquet: assign_segment_ids_sales_rental.R ../../_lib/canonical_geometry_helpers.R ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg | ../output
	Rscript $< $(SEGMENT_LENGTH_FT) $(SEGMENT_BUFFER_M)

../output/sales_pre_scores_with_segments.csv: ../output/rent_pre_scores_full_with_segments.parquet
	@test -s "$@"

../input/sales_pre_scores.csv: ../../calculate_sale_distances/output/sales_pre_scores.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/rent_pre_scores_full.parquet: ../../calculate_rent_distances/output/rent_pre_scores_full.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg: ../../border_segment_creation/output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg

include ../../generic.make
