SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

START_YEAR ?= 2011
END_YEAR ?= 2020
REVIEW_TAG := $(START_YEAR)_$(END_YEAR)

all: ../output/merge_ready/pd_far_text_review_updates_$(REVIEW_TAG).csv

../output/merge_ready/pd_far_text_review_updates_$(REVIEW_TAG).csv:
	@test -f "$@" || { \
		echo "Missing committed merge-ready PD FAR handoff: $@"; \
		exit 1; \
	}
