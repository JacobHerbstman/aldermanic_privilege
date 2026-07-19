SHELL := bash

GENERIC_MAKE := $(lastword $(MAKEFILE_LIST))
TASKS_ROOT := $(patsubst %/,%,$(dir $(GENERIC_MAKE)))

../input ../output ../temp slurmlogs:
	mkdir -p $@

run.sbatch: $(TASKS_ROOT)/setup_environment/code/run.sbatch | slurmlogs
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../../_lib/% ../../../_lib/%:
	@test -e "$@" || { echo "Missing shared library: $@"; false; }

.PHONY: FORCE_UPSTREAM
FORCE_UPSTREAM:

.SECONDEXPANSION:
../../% ../../../% ../../../../%: $$(shell bash "$$(TASKS_ROOT)/check_upstream_status.sh" "$$@" "$$(MAKE_COMMAND)")
	@case "$@" in \
		../../../*/output/*) \
			task=$$(printf '%s\n' "$@" | sed 's#^\.\./\.\./\.\./##; s#/output/.*##'); \
			output=$$(printf '%s\n' "$@" | sed 's#^.*/output/#../output/#'); \
			$(MAKE) -C "../../../$$task/code" "$$output"; \
			;; \
		../../*/output/*) \
			task=$$(printf '%s\n' "$@" | sed 's#^\.\./\.\./##; s#/output/.*##'); \
			output=$$(printf '%s\n' "$@" | sed 's#^.*/output/#../output/#'); \
			$(MAKE) -C "../../$$task/code" "$$output"; \
			;; \
		../../../data_raw/*|../../../../data_raw/*) \
			test -e "$@" || { echo "Missing raw root: $@"; false; }; \
			;; \
		*) \
			test -e "$@" || { echo "No generic upstream rule for $@"; false; }; \
			;; \
	esac
