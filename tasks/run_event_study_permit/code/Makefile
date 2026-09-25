include ../../shared/code/shell_functions.make

all: ../output/permit_event_study_high_discretion_stable_signed_500ft.pdf \
	../output/permit_event_study_high_discretion_stable_signed_500ft.csv \
	../output/permit_event_study_high_discretion_stable_separate_500ft.pdf \
	../output/permit_event_study_high_discretion_stable_separate_500ft.csv \
	../output/permit_event_study_low_discretion_nosigns_stable_signed_500ft.pdf \
	../output/permit_event_study_low_discretion_nosigns_stable_signed_500ft.csv \
	../output/permit_event_study_high_discretion_all_signed_500ft.pdf \
	../output/permit_event_study_high_discretion_all_signed_500ft.csv \
	../output/permit_event_study_appendix_500ft.tex

../%/permit_event_study_high_discretion_stable_signed_500ft.pdf ../%/permit_event_study_high_discretion_stable_signed_500ft.csv: \
	run_binary_event_study_permit.R ../input/permit_block_year_panel_2015.parquet | ../output ../report
	$(R) $< high_discretion stable signed

../%/permit_event_study_high_discretion_stable_separate_500ft.pdf ../%/permit_event_study_high_discretion_stable_separate_500ft.csv: \
	run_binary_event_study_permit.R ../input/permit_block_year_panel_2015.parquet | ../output ../report
	$(R) $< high_discretion stable separate

../%/permit_event_study_low_discretion_nosigns_stable_signed_500ft.pdf ../%/permit_event_study_low_discretion_nosigns_stable_signed_500ft.csv: \
	run_binary_event_study_permit.R ../input/permit_block_year_panel_2015.parquet | ../output ../report
	$(R) $< low_discretion_nosigns stable signed

../%/permit_event_study_high_discretion_all_signed_500ft.pdf ../%/permit_event_study_high_discretion_all_signed_500ft.csv: \
	run_binary_event_study_permit.R ../input/permit_block_year_panel_2015.parquet | ../output ../report
	$(R) $< high_discretion all signed

../output/permit_event_study_appendix_500ft.tex: build_binary_event_study_table.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $<

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/permit_block_year_panel_2015.parquet

include ../../shared/code/generic.make
