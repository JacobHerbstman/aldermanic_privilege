SHELL := bash
SHARED_CODE := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))

.NOTPARALLEL:

../input ../output ../report ../temp slurmlogs:
	mkdir -p $@

run.sbatch: $(SHARED_CODE)/../../setup_environment/code/run.sbatch | slurmlogs
	ln -sf $< $@

.PHONY: FORCE_UPSTREAM
FORCE_UPSTREAM:

.SECONDEXPANSION:
../tasks/% ../../% ../../../% ../../../../%: $$(if $$(or $$(findstring /output/,$$@),$$(findstring /report/,$$@)),FORCE_UPSTREAM)
	@case "$@" in \
		*/output/*) target="$@"; task="$${target%/output/*}"; output="../output/$${target##*/output/}" ;; \
		*/report/*) target="$@"; task="$${target%/report/*}"; output="../report/$${target##*/report/}" ;; \
		*) echo "Missing prerequisite: $@" >&2; exit 1 ;; \
	esac; \
	$(MAKE) -C "$$task/code" "$$output"
