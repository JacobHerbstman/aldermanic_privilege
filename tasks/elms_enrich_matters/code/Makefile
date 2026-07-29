START_DATE ?= 2010-11-01
END_DATE ?= 2026-02-12
SLEEP_SECONDS ?= 0.2
MAX_RETRIES ?= 4
TIMEOUT_SECONDS ?= 60
DATE_TAG := $(subst -,,$(START_DATE))_$(subst -,,$(END_DATE))

all: \
	../output/matter_sponsors_$(DATE_TAG).csv \
	../output/matter_histories_$(DATE_TAG).csv \
	../output/matter_attachments_$(DATE_TAG).csv \
	../output/candidate_final_ids_$(DATE_TAG).csv

../output/matter_sponsors_$(DATE_TAG).csv: fetch_matter_children_sponsors_histories.R ../temp/matter_children_long_$(DATE_TAG).csv | ../output run.sbatch
	$(R) $< --date-tag $(DATE_TAG)

../output/matter_histories_$(DATE_TAG).csv: ../output/matter_sponsors_$(DATE_TAG).csv

../output/matter_attachments_$(DATE_TAG).csv: fetch_matter_children_attachments_candidates.R ../temp/matter_children_long_$(DATE_TAG).csv | ../output run.sbatch
	$(R) $< --date-tag $(DATE_TAG)

../output/candidate_final_ids_$(DATE_TAG).csv: ../output/matter_attachments_$(DATE_TAG).csv

../temp/matter_children_long_$(DATE_TAG).csv: fetch_matter_children_long.py ../input/matters_$(DATE_TAG).csv ../input/candidate_seed_ids_$(DATE_TAG).csv | ../temp run.sbatch
	$(PYTHON) $< \
		--date-tag $(DATE_TAG) \
		--sleep-seconds $(SLEEP_SECONDS) \
		--max-retries $(MAX_RETRIES) \
		--timeout-seconds $(TIMEOUT_SECONDS)

../input/matters_$(DATE_TAG).csv: ../../elms_fetch_ordinances/output/matters_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/candidate_seed_ids_$(DATE_TAG).csv: ../../elms_fetch_ordinances/output/candidate_seed_ids_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/matters_$(DATE_TAG).csv ../input/candidate_seed_ids_$(DATE_TAG).csv

include ../../shell_functions.make
include ../../generic.make
