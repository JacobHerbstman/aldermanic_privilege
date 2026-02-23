# ─────────────────────────────────────────────────────────────────────────────
# generic.make  (to be included from each task’s code/Makefile)
# ─────────────────────────────────────────────────────────────────────────────

SHELL := bash

# ----------------------------------------------------------------------------
# Create the standard folders if they don’t exist
# ----------------------------------------------------------------------------
../input ../output ../temp slurmlogs:
	mkdir -p $@

# ----------------------------------------------------------------------------
# SLURM wrapper (path is still relative to each task’s code/ folder)
# ----------------------------------------------------------------------------
run.sbatch: ../../setup_environment/code/run.sbatch | slurmlogs
	ln -sf $< $@

# ----------------------------------------------------------------------------
# Remove accidental Finder-style duplicates like "file 2.csv" when canonical
# files already exist in the same task folder.
# ----------------------------------------------------------------------------
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

# ----------------------------------------------------------------------------
# Generic upstream rule — This is a powerful but advanced feature.
# For now, we will rely on explicit symbolic linking rules in each Makefile
# to make the workflow as clear as possible.
# ----------------------------------------------------------------------------
 ../../output/%:
	$(MAKE) -C $(subst output/,code/,$(dir $@)) \
	        ../output/$(notdir $@)
