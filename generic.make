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
# Generic upstream rule — *only* for artefacts located in an output folder
# (prevents make from trying to rebuild helper files like generic.make itself)
# ----------------------------------------------------------------------------
../../output/%:
	$(MAKE) -C $(subst output/,code/,$(dir $@)) \
	        ../output/$(notdir $@)

# If you ever need to depend on files created in another task’s input folder,
# uncomment the rule below.
# ../../input/%:
# 	$(MAKE) -C $(subst input/,code/,$(dir $@)) \
# 	        ../input/$(notdir $@)

