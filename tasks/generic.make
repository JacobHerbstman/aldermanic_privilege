SHELL := bash

../input ../output ../temp slurmlogs:
	mkdir -p $@

run.sbatch: ../../setup_environment/code/run.sbatch | slurmlogs
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

.PHONY: sanitize-numbered-duplicates FORCE
FORCE:

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

../../_lib/%:
	@test -e "$@" || { echo "Missing shared library: $@"; false; }

../../%: FORCE
	@case "$@" in \
		../../*/output/*) \
			task=$$(printf '%s\n' "$@" | sed 's#^\.\./\.\./##; s#/output/.*##'); \
			output=$$(printf '%s\n' "$@" | sed 's#^\.\./\.\./[^/]*/output/#../output/#'); \
			$(MAKE) -C "../../$$task/code" "$$output"; \
			;; \
		../../../data_raw/*) \
			test -e "$@" || { echo "Missing raw root: $@"; false; }; \
			;; \
		*) \
			test -e "$@" || { echo "No generic upstream rule for $@"; false; }; \
			;; \
	esac
