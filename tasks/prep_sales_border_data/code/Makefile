all: ../output/sales_with_hedonics.parquet

../output/sales_with_hedonics.parquet: prep_sales_data.R ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet | ../output
	Rscript $<

../input/sales_with_ward_distances.csv: ../../merge_event_study_scores/output/sales_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/residential_improvements_panel.parquet: ../../process_residential_improvements_full/output/residential_improvements_panel.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet

include ../../generic.make
