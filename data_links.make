# data_link.make   — only bootstraps symlinks from tasks/*/{input,output} → data_link/*_{input,output}
SHELL := bash

# your list of task‐folder basenames
TASKS := \
  clean_building_permits \
  clean_tract_data \
  make_aldermen_panel \
  merge_wards_tracts \
  process_attom_assessor \
  process_attom_assessor_historical 
.PHONY: all $(addsuffix /input,$(TASKS)) $(addsuffix /output,$(TASKS))

all: $(addsuffix /input,$(TASKS)) $(addsuffix /output,$(TASKS))

# for each task X:
#  - remove any old X/input
#  - symlink X/input → ../data_link/X_input
$(addsuffix /input,$(TASKS)): %/input:
	@echo "↳ Linking $$@ → ../data_link/$*_input"
	@rm -rf $@
	@ln -s ../data_link/$*_input $@

# same for X/output → ../data_link/X_output
$(addsuffix /output,$(TASKS)): %/output:
	@echo "↳ Linking $$@ → ../data_link/$*_output"
	@rm -rf $@
	@ln -s ../data_link/$*_output $@
