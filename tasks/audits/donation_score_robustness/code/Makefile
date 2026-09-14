include ../../../shared/code/shell_functions.make

HISTORY_START := 1994
HISTORY_END := 2026
SAMPLE_START := 2006
SAMPLE_END := 2022
SPLIT_YEAR := 2014
DOLLAR_YEAR := 2022

all: ../output/donation_review.html

../output/donation_review.html: render_donation_review.R Makefile ../output/donation_measures.csv ../output/donation_correlations.csv ../output/donation_stability.csv ../output/donation_rank_gaps.csv ../output/cleaning_counts.csv ../output/donor_classifications.csv ../output/committee_coverage.csv | ../output ../temp
	$(R) $<

../output/donation_measures.csv ../output/donation_correlations.csv ../output/donation_stability.csv ../output/donation_rank_gaps.csv: compare_donations.R Makefile ../output/donation_receipts.parquet ../input/paper_scores.csv ../input/early_scores.csv ../input/chicago_cpi.csv ../input/alderman_months.csv ../sources/legacy_donation_totals.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< $(SAMPLE_START) $(SAMPLE_END) $(SPLIT_YEAR) $(HISTORY_START) $(HISTORY_END) $(DOLLAR_YEAR)

../output/donation_receipts.parquet ../output/cleaning_counts.csv ../output/donor_classifications.csv ../output/committee_coverage.csv: clean_donations.R Makefile ../input/receipts.tsv ../input/alderman_months.csv ../sources/paper_scores.csv ../sources/committee_crosswalk.csv ../sources/committee_registry.tsv ../sources/recorded_union_classifications.csv ../sources/union_name_rules.csv ../sources/sector_name_evidence.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< $(HISTORY_START) $(HISTORY_END)

../input/receipts.tsv: ../../../../data_raw/campaign_finance/Receipts_20260817.txt | ../input
	ln -sf $< $@

../input/alderman_months.csv: ../sources/alderman_months.csv | ../input
	ln -sf $< $@

../input/paper_scores.csv: ../sources/paper_scores.csv | ../input
	ln -sf $< $@

../input/early_scores.csv: ../sources/early_scores.csv | ../input
	ln -sf $< $@

../input/chicago_cpi.csv: ../sources/chicago_cpi.csv | ../input
	ln -sf $< $@

link-inputs: ../input/receipts.tsv ../input/alderman_months.csv ../input/paper_scores.csv ../input/early_scores.csv ../input/chicago_cpi.csv

include ../../../shared/code/generic.make
