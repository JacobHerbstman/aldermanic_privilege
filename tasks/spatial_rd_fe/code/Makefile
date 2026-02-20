bandwidths := 500
fe_specs := pair_x_year pair_year
yvars := density_far density_dupac unitscount
log_status := true false

log_prefix_true := log_
log_prefix_false :=

define gen_slope_files
$(foreach yvar,$(yvars),$(foreach bw,$(bandwidths),$(foreach fe,$(fe_specs),$(foreach log_stat,$(log_status),../output/rd_fe_plot_$(log_prefix_$(log_stat))$(yvar)_bw$(bw)_$(fe).pdf))))
endef

define gen_level_files
$(foreach yvar,$(yvars),$(foreach bw,$(bandwidths),$(foreach fe,$(fe_specs),$(foreach log_stat,$(log_status),../output/rd_fe_plot_level_$(log_prefix_$(log_stat))$(yvar)_bw$(bw)_$(fe).pdf))))
endef

slope_outputs := $(call gen_slope_files)
level_outputs := $(call gen_level_files)
qc_outputs := ../output/spatial_rd_fe_qc.csv ../output/spatial_rd_fe_qc.txt

all: $(slope_outputs) $(level_outputs) $(qc_outputs)

$(qc_outputs): $(firstword $(slope_outputs))

define slope_rule
../output/rd_fe_plot_$(log_prefix_$(log_stat))$(yvar)_bw$(bw)_$(fe).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript spatial_rd_fe.R $(yvar) $(log_stat) $(bw) $(fe) $$@ slope
endef

$(foreach yvar,$(yvars),$(foreach bw,$(bandwidths),$(foreach fe,$(fe_specs),$(foreach log_stat,$(log_status),$(eval $(slope_rule))))))

define level_rule
../output/rd_fe_plot_level_$(log_prefix_$(log_stat))$(yvar)_bw$(bw)_$(fe).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript spatial_rd_fe.R $(yvar) $(log_stat) $(bw) $(fe) $$@ level
endef

$(foreach yvar,$(yvars),$(foreach bw,$(bandwidths),$(foreach fe,$(fe_specs),$(foreach log_stat,$(log_status),$(eval $(level_rule))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
