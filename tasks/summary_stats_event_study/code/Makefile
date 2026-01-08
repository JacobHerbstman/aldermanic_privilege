# Makefile for summary_stats_event_study

.PHONY: all clean

all: ../output/summary_stats_event_study.tex

# Input files
INPUT_SALES := ../input/sales_transaction_panel_2012.parquet
INPUT_RENTAL := ../input/rental_listing_panel.parquet

# Link inputs from source tasks
$(INPUT_SALES):
	@mkdir -p ../input
	ln -sf ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2012.parquet $@

$(INPUT_RENTAL):
	@mkdir -p ../input
	ln -sf ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet $@

# Main target
../output/summary_stats_event_study.tex: create_summary_stats.R $(INPUT_SALES) $(INPUT_RENTAL)
	@mkdir -p ../output
	Rscript create_summary_stats.R

clean:
	rm -f ../output/*
	rm -f ../input/*
