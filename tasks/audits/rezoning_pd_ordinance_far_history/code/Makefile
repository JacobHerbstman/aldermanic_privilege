all: \
	../output/unresolved_pd_to_pd_queue.csv \
	../output/pd_all_attachment_text.csv \
	../output/pd_all_attachment_fields.csv \
	../output/pd_far_evidence.csv \
	../output/pd_far_review_queue.csv \
	../output/pd_archive_text.csv \
	../output/pd_archive_fields.csv \
	../output/pd_archive_far_evidence.csv \
	../output/pd_archive_review_queue.csv \
	../output/pd_far_decisions_audit.csv \
	../output/pd_far_decision_summary.csv \
	../output/pd_far_unresolved.csv \
	../output/pd_far_khan_comparison_summary.csv \
	../output/post_khan_pd_to_pd_queue_20160901_20201231.csv \
	../output/post_khan_discovery_summary_20160901_20201231.csv \
	../output/post_khan_pd_far_review_queue_20160901_20201231.csv \
	../output/post_khan_pd_archive_review_queue_20160901_20201231.csv \
	../output/post_khan_pd_far_decisions_audit_20160901_20201231.csv \
	../output/post_khan_pd_far_decision_summary_20160901_20201231.csv \
	../output/post_khan_pd_far_unresolved_20160901_20201231.csv \
	../output/pd_far_20101101_20201231_summary.csv

../output/post_khan_matters_20160901_20201231.csv: build_post_khan_candidates.py ../input/matters_20101201_20260212.csv ../input/candidate_seed_ids_20101201_20260212.csv | ../output
	$(PYTHON) $<

../output/post_khan_candidate_seeds_20160901_20201231.csv: ../output/post_khan_matters_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_passed_candidates_20160901_20201231.csv: ../output/post_khan_matters_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_matter_attachments_20160901_20201231.csv: ../../../elms_enrich_matters/code/fetch_matter_children.py ../output/post_khan_matters_20160901_20201231.csv ../output/post_khan_candidate_seeds_20160901_20201231.csv | ../output
	$(PYTHON) $< \
		--in-matters-csv ../output/post_khan_matters_20160901_20201231.csv \
		--in-seed-csv ../output/post_khan_candidate_seeds_20160901_20201231.csv \
		--sleep-seconds 0.1 \
		--max-retries 4 \
		--timeout-seconds 60 \
		--out-sponsors ../output/post_khan_matter_sponsors_20160901_20201231.csv \
		--out-histories ../output/post_khan_matter_histories_20160901_20201231.csv \
		--out-attachments ../output/post_khan_matter_attachments_20160901_20201231.csv \
		--out-candidates ../output/post_khan_candidate_final_20160901_20201231.csv \
		--out-report-json ../output/post_khan_elms_report_20160901_20201231.json

../output/post_khan_matter_sponsors_20160901_20201231.csv: ../output/post_khan_matter_attachments_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_matter_histories_20160901_20201231.csv: ../output/post_khan_matter_attachments_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_candidate_final_20160901_20201231.csv: ../output/post_khan_matter_attachments_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_elms_report_20160901_20201231.json: ../output/post_khan_matter_attachments_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_discovery_text_20160901_20201231.csv: ../../../elms_pdf_processing/code/process_pdfs.py ../../../elms_pdf_processing/code/pdf_parsers.py ../output/post_khan_matter_attachments_20160901_20201231.csv ../output/post_khan_passed_candidates_20160901_20201231.csv | ../output
	$(PYTHON) $< \
		--in-attachments-csv ../output/post_khan_matter_attachments_20160901_20201231.csv \
		--in-candidates-csv ../output/post_khan_passed_candidates_20160901_20201231.csv \
		--pdf-dir ../output/post_khan_discovery_pdfs \
		--sleep-seconds 0.1 \
		--max-retries 4 \
		--max-attachments-per-matter 1 \
		--request-timeout-seconds 120 \
		--parse-timeout-seconds 180 \
		--progress-every 20 \
		--out-text-csv ../output/post_khan_discovery_text_20160901_20201231.csv \
		--out-fields-csv ../output/post_khan_discovery_fields_20160901_20201231.csv

../output/post_khan_discovery_fields_20160901_20201231.csv: ../output/post_khan_discovery_text_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_discovery_fallback_candidates_20160901_20201231.csv: build_post_khan_discovery_fallback.py ../output/post_khan_matters_20160901_20201231.csv ../output/post_khan_discovery_fields_20160901_20201231.csv | ../output
	$(PYTHON) $<

../output/post_khan_discovery_fallback_text_20160901_20201231.csv: ../../../elms_pdf_processing/code/process_pdfs.py ../../../elms_pdf_processing/code/pdf_parsers.py ../output/post_khan_matter_attachments_20160901_20201231.csv ../output/post_khan_discovery_fallback_candidates_20160901_20201231.csv | ../output
	$(PYTHON) $< \
		--in-attachments-csv ../output/post_khan_matter_attachments_20160901_20201231.csv \
		--in-candidates-csv ../output/post_khan_discovery_fallback_candidates_20160901_20201231.csv \
		--pdf-dir ../output/post_khan_discovery_pdfs \
		--sleep-seconds 0.1 \
		--max-retries 4 \
		--max-attachments-per-matter 3 \
		--request-timeout-seconds 120 \
		--parse-timeout-seconds 180 \
		--progress-every 20 \
		--out-text-csv ../output/post_khan_discovery_fallback_text_20160901_20201231.csv \
		--out-fields-csv ../output/post_khan_discovery_fallback_fields_20160901_20201231.csv

../output/post_khan_discovery_fallback_fields_20160901_20201231.csv: ../output/post_khan_discovery_fallback_text_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_pd_to_pd_queue_20160901_20201231.csv: build_post_khan_pd_queue.py ../output/post_khan_matters_20160901_20201231.csv ../output/post_khan_candidate_final_20160901_20201231.csv ../output/post_khan_discovery_text_20160901_20201231.csv ../output/post_khan_discovery_fields_20160901_20201231.csv ../output/post_khan_discovery_fallback_text_20160901_20201231.csv ../output/post_khan_discovery_fallback_fields_20160901_20201231.csv ../input/post_khan_pd_queue_manual_classifications.csv | ../output
	$(PYTHON) $<

../output/post_khan_discovery_summary_20160901_20201231.csv: ../output/post_khan_pd_to_pd_queue_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_pd_all_attachment_text_20160901_20201231.csv: ../../../elms_pdf_processing/code/process_pdfs.py ../../../elms_pdf_processing/code/pdf_parsers.py ../output/post_khan_matter_attachments_20160901_20201231.csv ../output/post_khan_pd_to_pd_queue_20160901_20201231.csv | ../output
	$(PYTHON) $< \
		--in-attachments-csv ../output/post_khan_matter_attachments_20160901_20201231.csv \
		--in-candidates-csv ../output/post_khan_pd_to_pd_queue_20160901_20201231.csv \
		--pdf-dir ../output/post_khan_discovery_pdfs \
		--sleep-seconds 0.1 \
		--max-retries 4 \
		--max-attachments-per-matter 3 \
		--request-timeout-seconds 120 \
		--parse-timeout-seconds 180 \
		--progress-every 20 \
		--out-text-csv ../output/post_khan_pd_all_attachment_text_20160901_20201231.csv \
		--out-fields-csv ../output/post_khan_pd_all_attachment_fields_20160901_20201231.csv

../output/post_khan_pd_all_attachment_fields_20160901_20201231.csv: ../output/post_khan_pd_all_attachment_text_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_pd_far_evidence_20160901_20201231.csv: extract_post_khan_pd_far_evidence.py ../output/post_khan_pd_to_pd_queue_20160901_20201231.csv ../output/post_khan_pd_all_attachment_text_20160901_20201231.csv ../output/post_khan_pd_all_attachment_fields_20160901_20201231.csv | ../output
	$(PYTHON) $<

../output/post_khan_pd_far_review_queue_20160901_20201231.csv: ../output/post_khan_pd_far_evidence_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_pd_archive_text_20160901_20201231.csv: download_post_khan_pd_archives.py ../output/post_khan_pd_to_pd_queue_20160901_20201231.csv ../output/pd_archive_text.csv ../output/pd_archive_fields.csv | ../output ../output/pd_archive_pdfs
	$(PYTHON) $<

../output/post_khan_pd_archive_fields_20160901_20201231.csv: ../output/post_khan_pd_archive_text_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_pd_archive_far_evidence_20160901_20201231.csv: extract_post_khan_pd_archive_evidence.py ../output/post_khan_pd_to_pd_queue_20160901_20201231.csv ../output/post_khan_pd_archive_text_20160901_20201231.csv ../output/post_khan_pd_archive_fields_20160901_20201231.csv | ../output
	$(PYTHON) $<

../output/post_khan_pd_archive_review_queue_20160901_20201231.csv: ../output/post_khan_pd_archive_far_evidence_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_pd_far_decisions_audit_20160901_20201231.csv: summarize_post_khan_pd_far_decisions.py ../output/post_khan_pd_to_pd_queue_20160901_20201231.csv ../output/post_khan_pd_all_attachment_text_20160901_20201231.csv ../output/post_khan_pd_archive_text_20160901_20201231.csv ../output/post_khan_pd_far_review_queue_20160901_20201231.csv ../output/post_khan_pd_archive_review_queue_20160901_20201231.csv ../output/pd_far_decisions_audit.csv ../output/unresolved_pd_to_pd_queue.csv ../input/post_khan_pd_far_manual_decisions.csv | ../output
	$(PYTHON) $<

../output/post_khan_pd_far_decision_summary_20160901_20201231.csv: ../output/post_khan_pd_far_decisions_audit_20160901_20201231.csv
	@test -s "$@"

../output/post_khan_pd_far_unresolved_20160901_20201231.csv: ../output/post_khan_pd_far_decisions_audit_20160901_20201231.csv
	@test -s "$@"

../output/pd_far_20101101_20201231_summary.csv: ../output/post_khan_pd_far_decisions_audit_20160901_20201231.csv
	@test -s "$@"

../output/unresolved_pd_to_pd_queue.csv: build_unresolved_queue.py ../input/khan_reconciled_sample.csv ../input/khan_pd_underlying_evidence.csv | ../output
	$(PYTHON) $<

../output/pd_all_attachment_text.csv: ../../../elms_pdf_processing/code/process_pdfs.py ../../../elms_pdf_processing/code/pdf_parsers.py ../input/matter_attachments_20101101_20160831.csv ../output/unresolved_pd_to_pd_queue.csv | ../output
	$(PYTHON) $< \
		--in-attachments-csv ../input/matter_attachments_20101101_20160831.csv \
		--in-candidates-csv ../output/unresolved_pd_to_pd_queue.csv \
		--pdf-dir ../output/pdfs \
		--sleep-seconds 0.1 \
		--max-retries 4 \
		--max-attachments-per-matter 3 \
		--request-timeout-seconds 120 \
		--parse-timeout-seconds 180 \
		--progress-every 20 \
		--out-text-csv ../output/pd_all_attachment_text.csv \
		--out-fields-csv ../output/pd_all_attachment_fields.csv

../output/pd_all_attachment_fields.csv: ../output/pd_all_attachment_text.csv
	@test -s "$@"

../output/pd_far_evidence.csv: extract_pd_far_evidence.py ../output/unresolved_pd_to_pd_queue.csv ../output/pd_all_attachment_text.csv ../output/pd_all_attachment_fields.csv | ../output
	$(PYTHON) $<

../output/pd_far_review_queue.csv: ../output/pd_far_evidence.csv
	@test -s "$@"

../output/pd_archive_text.csv: download_pd_archives.py ../output/unresolved_pd_to_pd_queue.csv | ../output ../output/pd_archive_pdfs
	$(PYTHON) $<

../output/pd_archive_fields.csv: ../output/pd_archive_text.csv
	@test -s "$@"

../output/pd_archive_far_evidence.csv: extract_pd_archive_evidence.py ../output/unresolved_pd_to_pd_queue.csv ../output/pd_archive_text.csv ../output/pd_archive_fields.csv | ../output
	$(PYTHON) $<

../output/pd_archive_review_queue.csv: ../output/pd_archive_far_evidence.csv
	@test -s "$@"

../output/pd_archive_pdfs: | ../output
	mkdir -p $@

../output/pd_far_decisions_audit.csv: summarize_pd_far_decisions.py ../output/unresolved_pd_to_pd_queue.csv ../output/pd_all_attachment_text.csv ../output/pd_far_review_queue.csv ../output/pd_archive_text.csv ../output/pd_archive_review_queue.csv ../input/khan_reconciled_sample.csv ../input/khan_pd_underlying_evidence.csv ../input/pd_far_manual_decisions.csv | ../output
	$(PYTHON) $<

../output/pd_far_decision_summary.csv: ../output/pd_far_decisions_audit.csv
	@test -s "$@"

../output/pd_far_unresolved.csv: ../output/pd_far_decisions_audit.csv
	@test -s "$@"

../output/pd_far_khan_comparison_summary.csv: ../output/pd_far_decisions_audit.csv
	@test -s "$@"

../input/khan_reconciled_sample.csv: ../../rezoning_khan_comparison/output/khan_reconciled_sample.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/khan_pd_underlying_evidence.csv: ../../rezoning_khan_comparison/input/khan_pd_underlying_evidence.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/matter_attachments_20101101_20160831.csv: ../../../elms_enrich_matters/output/matter_attachments_20101101_20160831.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/matters_20101201_20260212.csv: ../../../elms_fetch_ordinances/output/matters_20101201_20260212.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/candidate_seed_ids_20101201_20260212.csv: ../../../elms_fetch_ordinances/output/candidate_seed_ids_20101201_20260212.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/pd_far_manual_decisions.csv: ../../../rezoning_hand_adjudications/output/pd_to_pd_far_decisions_20101101_20160831.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/post_khan_pd_far_manual_decisions.csv: ../../../rezoning_hand_adjudications/output/pd_to_pd_far_decisions_20160901_20201231.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/post_khan_pd_queue_manual_classifications.csv: ../../../rezoning_hand_adjudications/output/pd_queue_classifications_20160901_20201231.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/khan_reconciled_sample.csv ../input/khan_pd_underlying_evidence.csv ../input/matter_attachments_20101101_20160831.csv ../input/matters_20101201_20260212.csv ../input/candidate_seed_ids_20101201_20260212.csv ../input/pd_far_manual_decisions.csv ../input/post_khan_pd_queue_manual_classifications.csv ../input/post_khan_pd_far_manual_decisions.csv

include ../../../shell_functions.make
include ../../../generic.make
