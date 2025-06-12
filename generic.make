# ─────────────────────────────────────────────────────────────────────────────
# generic.make  (to be included from each task’s code/Makefile)
# ─────────────────────────────────────────────────────────────────────────────

# 1) detect project root as two levels above whatever code/ subdir we’re in
PROJECT_ROOT := $(abspath $(CURDIR)/../..)
export PROJECT_ROOT

#.PHONY: print-root
#print-root:
#	@echo PROJECT_ROOT = $(PROJECT_ROOT)

# 2) make sure each task has its sibling folders
../output ../temp ../input slurmlogs:
	mkdir -p $@

# 3) link in the sbatch wrapper from setup_environment
#    now using PROJECT_ROOT so we never rely on ../../ in R code either
run.sbatch: $(PROJECT_ROOT)/setup_environment/code/run.sbatch | slurmlogs
	ln -sf $< $@

# 4) generic “build upstream” rule kept exactly as before
../../%:
	$(MAKE) -C $(subst output/,code/,$(dir $@)) \
	         ../output/$(notdir $@)
