SHELL := bash
include ../../shell_functions.make

raw_files := $(wildcard ../../download_rent_data/output/*.parquet)
input_links := $(patsubst ../../download_rent_data/output/%,../input/%,$(raw_files))
dates := $(sort $(foreach f,$(raw_files),$(firstword $(subst --, ,$(notdir $(f))))))
temp_csvs := $(patsubst %,../temp/%.csv,$(dates))

all: ../output/chicago_rent_panel.parquet

../output/chicago_rent_panel.parquet: aggregate_rents.R $(temp_csvs) | ../output
	Rscript aggregate_rents.R

define single_day_rule
../temp/$(firstword $(subst --, ,$(notdir $(1)))).csv: $(1) process_rent_data.R | ../temp
	Rscript process_rent_data.R "$$<" "$$@"
endef
$(foreach f,$(input_links),$(eval $(call single_day_rule,$(f))))

../input/%.parquet: ../../download_rent_data/output/%.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: $(input_links)

include ../../generic.make
