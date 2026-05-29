BALANCE_BW_M ?= 250

all: \
	../output/permit_balance_summary_$(BALANCE_BW_M)m.csv \
	../output/permit_balance_summary_$(BALANCE_BW_M)m.tex \
	../output/permit_balance_joint_summary_$(BALANCE_BW_M)m.csv \
	../output/permit_balance_joint_summary_$(BALANCE_BW_M)m.tex \
	../output/permit_balance_did_shift_$(BALANCE_BW_M)m.csv \
	../output/permit_balance_did_shift_$(BALANCE_BW_M)m.tex \
	../output/permit_balance_love_plot_$(BALANCE_BW_M)m.pdf

../output/permit_balance_summary_$(BALANCE_BW_M)m.csv: diagnose_permit_balance.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript diagnose_permit_balance.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv ../output/permit_balance_summary_$(BALANCE_BW_M)m.csv ../output/permit_balance_summary_$(BALANCE_BW_M)m.tex ../output/permit_balance_joint_summary_$(BALANCE_BW_M)m.csv ../output/permit_balance_joint_summary_$(BALANCE_BW_M)m.tex ../output/permit_balance_did_shift_$(BALANCE_BW_M)m.csv ../output/permit_balance_did_shift_$(BALANCE_BW_M)m.tex ../output/permit_balance_love_plot_$(BALANCE_BW_M)m.pdf $(BALANCE_BW_M)

../output/permit_balance_summary_$(BALANCE_BW_M)m.tex \
../output/permit_balance_joint_summary_$(BALANCE_BW_M)m.csv \
../output/permit_balance_joint_summary_$(BALANCE_BW_M)m.tex \
../output/permit_balance_did_shift_$(BALANCE_BW_M)m.csv \
../output/permit_balance_did_shift_$(BALANCE_BW_M)m.tex \
../output/permit_balance_love_plot_$(BALANCE_BW_M)m.pdf: ../output/permit_balance_summary_$(BALANCE_BW_M)m.csv
	@test -s "$@"

../input/permit_block_year_panel.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../create_block_group_controls/output/block_group_controls.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv

include ../../generic.make
