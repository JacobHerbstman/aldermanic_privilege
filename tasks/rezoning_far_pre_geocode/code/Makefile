DATE_TAG ?= 20101101_20201231

all: ../output/zoning_matters_far_$(DATE_TAG).csv

../output/zoning_matters_far_$(DATE_TAG).csv: assign_far_pre_geocode.py ../input/zoning_matters_$(DATE_TAG).csv ../input/zoning_far_lookup_clean.csv ../input/rezoning_sample_decisions_20101101_20160831.csv ../input/journal_zoning_code_fills_20101101_20160831.csv ../input/pd_transition_code_corrections_20101101_20160831.csv ../input/destination_code_corrections_20101101_20160831.csv ../input/destination_code_corrections_20160901_20201231.csv ../input/non_scalar_far_decisions_20101101_20201231.csv ../input/section_review_decisions_20101101_20201231.csv ../input/pd_to_pd_far_decisions_20101101_20160831.csv ../input/pd_to_pd_far_decisions_20160901_20201231.csv | ../output run.sbatch
	$(PYTHON) $< $(DATE_TAG)

../input/zoning_matters_$(DATE_TAG).csv: ../../build_zoning_dataset/output/zoning_matters_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/zoning_far_lookup_clean.csv: ../../zoning_data_cleaning/output/zoning_far_lookup_clean.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rezoning_sample_decisions_20101101_20160831.csv: ../../rezoning_hand_adjudications/output/rezoning_sample_decisions_20101101_20160831.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/journal_zoning_code_fills_20101101_20160831.csv: ../../rezoning_hand_adjudications/output/journal_zoning_code_fills_20101101_20160831.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/pd_transition_code_corrections_20101101_20160831.csv: ../../rezoning_hand_adjudications/output/pd_transition_code_corrections_20101101_20160831.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/destination_code_corrections_20101101_20160831.csv: ../../rezoning_hand_adjudications/output/destination_code_corrections_20101101_20160831.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/destination_code_corrections_20160901_20201231.csv: ../../rezoning_hand_adjudications/output/destination_code_corrections_20160901_20201231.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/non_scalar_far_decisions_20101101_20201231.csv: ../../rezoning_hand_adjudications/output/non_scalar_far_decisions_20101101_20201231.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/section_review_decisions_20101101_20201231.csv: ../../rezoning_hand_adjudications/output/section_review_decisions_20101101_20201231.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/pd_to_pd_far_decisions_20101101_20160831.csv: ../../rezoning_hand_adjudications/output/pd_to_pd_far_decisions_20101101_20160831.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/pd_to_pd_far_decisions_20160901_20201231.csv: ../../rezoning_hand_adjudications/output/pd_to_pd_far_decisions_20160901_20201231.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/zoning_matters_$(DATE_TAG).csv ../input/zoning_far_lookup_clean.csv ../input/rezoning_sample_decisions_20101101_20160831.csv ../input/journal_zoning_code_fills_20101101_20160831.csv ../input/pd_transition_code_corrections_20101101_20160831.csv ../input/destination_code_corrections_20101101_20160831.csv ../input/destination_code_corrections_20160901_20201231.csv ../input/non_scalar_far_decisions_20101101_20201231.csv ../input/section_review_decisions_20101101_20201231.csv ../input/pd_to_pd_far_decisions_20101101_20160831.csv ../input/pd_to_pd_far_decisions_20160901_20201231.csv

include ../../shell_functions.make
include ../../generic.make
