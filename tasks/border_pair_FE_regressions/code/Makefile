fe_specs ?= pair_x_year
bws_ft := 250 500 1000
yvars_table := 'log(density_far)' 'log(density_dupac)' 'log(unitscount)'
units_cap ?= 100
output_suffix ?=

table_outputs := $(foreach bw,$(bws_ft),$(foreach fe,$(fe_specs),../output/fe_table_bw$(bw)_$(fe)$(output_suffix).tex))
qc_outputs := ../output/fe_run_qc.csv ../output/fe_run_qc.txt

all: $(table_outputs) $(qc_outputs)

$(qc_outputs): $(firstword $(table_outputs))

define fe_rule
../output/fe_table_bw$(1)_$(2)$(output_suffix).tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript border_pair_FE_regressions.R --bw_ft=$(1) --units_cap=$(units_cap) --fe_spec=$(2) --output="$$@" $(yvars_table)
endef
$(foreach bw,$(bws_ft),$(foreach fe,$(fe_specs),$(eval $(call fe_rule,$(bw),$(fe)))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
