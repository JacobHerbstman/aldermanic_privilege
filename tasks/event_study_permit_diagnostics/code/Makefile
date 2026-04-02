SHELL := bash
include ../../shell_functions.make

all: \
	../output/permit_balance_summary.csv \
	../output/permit_balance_summary.tex \
	../output/permit_balance_joint_summary.csv \
	../output/permit_balance_joint_summary.tex \
	../output/permit_balance_did_shift.csv \
	../output/permit_balance_did_shift.tex \
	../output/permit_balance_love_plot.pdf

../output/permit_balance_summary.csv: diagnose_permit_balance.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	$(R) diagnose_permit_balance.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv ../output/permit_balance_summary.csv ../output/permit_balance_summary.tex ../output/permit_balance_joint_summary.csv ../output/permit_balance_joint_summary.tex ../output/permit_balance_did_shift.csv ../output/permit_balance_did_shift.tex ../output/permit_balance_love_plot.pdf

../output/permit_balance_summary.tex \
../output/permit_balance_joint_summary.csv \
../output/permit_balance_joint_summary.tex \
../output/permit_balance_did_shift.csv \
../output/permit_balance_did_shift.tex \
../output/permit_balance_love_plot.pdf: ../output/permit_balance_summary.csv
	test -f "$@"

../input/permit_block_year_panel.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel.parquet | ../input
	ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../create_block_group_controls/output/block_group_controls.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv

include ../../generic.make
