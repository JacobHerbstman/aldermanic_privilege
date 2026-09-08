SHELL := bash

GENERIC_MAKE := $(lastword $(MAKEFILE_LIST))
SHARED_CODE := $(patsubst %/,%,$(dir $(GENERIC_MAKE)))
TASKS_ROOT := $(SHARED_CODE)/../..

../input ../output ../report ../temp slurmlogs:
	mkdir -p $@

run.sbatch: $(TASKS_ROOT)/setup_environment/code/run.sbatch | slurmlogs
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../../shared/code/% ../../../shared/code/%:
	@test -e "$@" || { echo "Missing shared library: $@"; false; }

.PHONY: FORCE_UPSTREAM
FORCE_UPSTREAM:

.SECONDEXPANSION:
../tasks/% ../../% ../../../% ../../../../%: $$(shell bash "$$(SHARED_CODE)/check_upstream_status.sh" "$$@" "$$(MAKE_COMMAND)")
	@case "$@" in \
		../tasks/*/output/*|../tasks/*/report/*) \
			task=$$(printf '%s\n' "$@" | sed -E 's#^\.\./tasks/##; s#/(output|report)/.*##'); \
			output=$$(printf '%s\n' "$@" | sed -E 's#^.*/(output|report)/#../\1/#'); \
			$(MAKE) -C "../tasks/$$task/code" "$$output"; \
			;; \
		../../../*/output/*|../../../*/report/*) \
			task=$$(printf '%s\n' "$@" | sed -E 's#^\.\./\.\./\.\./##; s#/(output|report)/.*##'); \
			output=$$(printf '%s\n' "$@" | sed -E 's#^.*/(output|report)/#../\1/#'); \
			$(MAKE) -C "../../../$$task/code" "$$output"; \
			;; \
		../../*/output/*|../../*/report/*) \
			task=$$(printf '%s\n' "$@" | sed -E 's#^\.\./\.\./##; s#/(output|report)/.*##'); \
			output=$$(printf '%s\n' "$@" | sed -E 's#^.*/(output|report)/#../\1/#'); \
			$(MAKE) -C "../../$$task/code" "$$output"; \
			;; \
		../../../data_raw/*|../../../../data_raw/*) \
			test -e "$@" || { echo "Missing raw root: $@"; false; }; \
			;; \
		*) \
			test -e "$@" || { echo "No generic upstream rule for $@"; false; }; \
			;; \
	esac
