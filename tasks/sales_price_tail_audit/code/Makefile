include ../../shared/code/shell_functions.make

TAIL_PROBABILITY ?= 0.999
AREA_CHANGE_RATIO ?= 1.5
REVIEW_SEED ?= 42
REVIEW_PER_GROUP ?= 4
BANDWIDTHS_FT ?= 500 1000 1500

all: ../output/tail_review.csv ../output/tail_cutoffs.csv ../output/tail_counts.csv ../output/tail_sale_history.csv ../output/tail_improvement_history.csv $(foreach bw,$(BANDWIDTHS_FT),../output/tail_rd_bw$(bw).csv)

../output/tail_review.csv: review_price_tails.R Makefile ../input/quality_records.parquet ../input/clean_sales_with_hedonics_amenities.parquet ../input/parcel_sales_city.csv ../input/residential_improvement_characteristics_full.csv | ../output
	$(R) $< $(TAIL_PROBABILITY) $(AREA_CHANGE_RATIO) $(REVIEW_SEED) $(REVIEW_PER_GROUP)
	touch -r "$@" ../output/tail_cutoffs.csv ../output/tail_counts.csv ../output/tail_sale_history.csv ../output/tail_improvement_history.csv

../output/tail_cutoffs.csv ../output/tail_counts.csv ../output/tail_sale_history.csv ../output/tail_improvement_history.csv: ../output/tail_review.csv
	@test -s "$@"

../output/tail_rd_bw%.csv: estimate_tail_sensitivity.R Makefile ../output/tail_review.csv ../input/quality_records.parquet ../input/price_boundary_property_type_fe_estimates.csv | ../output
	$(R) $< $*

../input/quality_records.parquet: ../../sales_record_quality_audit/output/quality_records.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/clean_sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_sales_city.csv: ../../download_parcel_sales_data/output/parcel_sales_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/residential_improvement_characteristics_full.csv: ../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/price_boundary_property_type_fe_estimates.csv: ../../price_boundary_results/output/price_boundary_property_type_fe_estimates.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/quality_records.parquet ../input/clean_sales_with_hedonics_amenities.parquet ../input/parcel_sales_city.csv ../input/residential_improvement_characteristics_full.csv ../input/price_boundary_property_type_fe_estimates.csv

include ../../shared/code/generic.make
