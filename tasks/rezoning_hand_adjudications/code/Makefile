all: \
	../output/pd_far_text_review_updates_2011_2020.csv \
	../output/pd_to_pd_far_decisions_20101101_20160831.csv \
	../output/pd_to_pd_far_decisions_20160901_20201231.csv \
	../output/pd_queue_classifications_20160901_20201231.csv \
	../output/journal_zoning_code_fills_20101101_20160831.csv \
	../output/rezoning_sample_decisions_20101101_20160831.csv \
	../output/pd_transition_code_corrections_20101101_20160831.csv \
	../output/destination_code_corrections_20101101_20160831.csv

../output/pd_far_text_review_updates_2011_2020.csv \
../output/pd_to_pd_far_decisions_20101101_20160831.csv \
../output/pd_to_pd_far_decisions_20160901_20201231.csv \
../output/pd_queue_classifications_20160901_20201231.csv \
../output/journal_zoning_code_fills_20101101_20160831.csv \
../output/rezoning_sample_decisions_20101101_20160831.csv \
../output/pd_transition_code_corrections_20101101_20160831.csv \
../output/destination_code_corrections_20101101_20160831.csv:
	@test -f "$@" || { echo "Missing committed hand-adjudicated file: $@"; exit 1; }

link-inputs:
