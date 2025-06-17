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
# Generic upstream rule — This is a powerful but advanced feature.
# For now, we will rely on explicit symbolic linking rules in each Makefile
# to make the workflow as clear as possible.
# ----------------------------------------------------------------------------
# ../../output/%:
#	$(MAKE) -C $(subst output/,code/,$(dir $@)) \
#	        ../output/$(notdir $@)