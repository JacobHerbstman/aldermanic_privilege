UNCERTAINTY_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022

all: ../output/permit_unit_change_text_audit_summary.csv ../output/permit_unit_change_text_review_sample.csv ../output/permit_unit_change_model_comparison.csv

../output/permit_unit_change_text_audit_summary.csv: audit_permit_unit_changes.R ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../output
	Rscript $< $(UNCERTAINTY_SPEC)

../output/permit_unit_change_text_review_sample.csv: ../output/permit_unit_change_text_audit_summary.csv

../output/permit_unit_change_model_comparison.csv: ../output/permit_unit_change_text_audit_summary.csv

../input/building_permits_text_features.csv.gz: ../../../building_permits_scraping/output/building_permits_text_features.csv.gz | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv

include ../../../generic.make
