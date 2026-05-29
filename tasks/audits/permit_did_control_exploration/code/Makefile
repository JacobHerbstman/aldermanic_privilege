BANDWIDTH_M ?= 250

all: \
	../output/permit_did_control_exploration_$(BANDWIDTH_M)m.csv \
	../output/permit_did_control_exploration_$(BANDWIDTH_M)m.tex \
	../output/permit_event_study_control_exploration_$(BANDWIDTH_M)m.csv \
	../output/permit_event_study_control_pretrends_$(BANDWIDTH_M)m.csv \
	../output/permit_event_study_control_comparison_$(BANDWIDTH_M)m.pdf \
	../output/permit_event_study_control_comparison_$(BANDWIDTH_M)m.png

../output/permit_did_control_exploration_$(BANDWIDTH_M)m.csv: explore_permit_did_controls.R ../input/permit_block_year_panel_2015.parquet ../input/block_parcel_baselines_2014.csv | ../output
	Rscript explore_permit_did_controls.R $(BANDWIDTH_M)
	test -f ../output/permit_did_control_exploration_$(BANDWIDTH_M)m.tex

../output/permit_did_control_exploration_$(BANDWIDTH_M)m.tex: ../output/permit_did_control_exploration_$(BANDWIDTH_M)m.csv
	@test -s "$@"

../output/permit_event_study_control_exploration_$(BANDWIDTH_M)m.csv \
../output/permit_event_study_control_pretrends_$(BANDWIDTH_M)m.csv \
../output/permit_event_study_control_comparison_$(BANDWIDTH_M)m.pdf \
../output/permit_event_study_control_comparison_$(BANDWIDTH_M)m.png: ../output/permit_did_control_exploration_$(BANDWIDTH_M)m.csv
	@test -s "$@"

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_parcel_baselines_2014.csv: ../../event_study_exact_balance/output/block_parcel_baselines_2014.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/block_parcel_baselines_2014.csv

include ../../../generic.make
