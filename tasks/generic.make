SHELL := bash

GENERIC_MAKE := $(lastword $(MAKEFILE_LIST))
TASKS_ROOT := $(patsubst %/,%,$(dir $(GENERIC_MAKE)))

../input ../output ../temp slurmlogs:
	mkdir -p $@

run.sbatch: $(TASKS_ROOT)/setup_environment/code/run.sbatch | slurmlogs
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

.PHONY: sanitize-numbered-duplicates

sanitize-numbered-duplicates: ../input
	@for dir in ../input ../output; do \
		[ -d "$$dir" ] || continue; \
		find "$$dir" -maxdepth 1 \( -type f -o -type l \) | while IFS= read -r path; do \
			canonical=$$(printf '%s\n' "$$path" | sed -E 's/ [0-9]+(\.[^./]+)$$/\1/'); \
			if [ "$$canonical" != "$$path" ] && { [ -e "$$canonical" ] || [ -L "$$canonical" ]; }; then \
				rm -f "$$path"; \
			fi; \
		done; \
	done

link-inputs: sanitize-numbered-duplicates

../../_lib/% ../../../_lib/%:
	@test -e "$@" || { echo "Missing shared library: $@"; false; }

.PHONY: FORCE_UPSTREAM
FORCE_UPSTREAM:

.SECONDEXPANSION:
../../% ../../../% ../../../../%: $$(shell bash "$$(TASKS_ROOT)/check_upstream_status.sh" "$$@" "$$(MAKE_COMMAND)")
	@case "$@" in \
		../../../*/output/*) \
			task=$$(printf '%s\n' "$@" | sed 's#^\.\./\.\./\.\./##; s#/output/.*##'); \
			output=$$(printf '%s\n' "$@" | sed 's#^\.\./\.\./\.\./[^/]*/output/#../output/#'); \
			$(MAKE) -C "../../../$$task/code" "$$output"; \
			;; \
		../../*/output/*) \
			task=$$(printf '%s\n' "$@" | sed 's#^\.\./\.\./##; s#/output/.*##'); \
			output=$$(printf '%s\n' "$@" | sed 's#^\.\./\.\./[^/]*/output/#../output/#'); \
			$(MAKE) -C "../../$$task/code" "$$output"; \
			;; \
		../../../data_raw/*|../../../../data_raw/*) \
			test -e "$@" || { echo "Missing raw root: $@"; false; }; \
			;; \
		*) \
			test -e "$@" || { echo "No generic upstream rule for $@"; false; }; \
			;; \
	esac
