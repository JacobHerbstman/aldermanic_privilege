bandwidths := 250 500 1000
kernels := triangular
yvars := density_far density_dupac unitscount
log_status := true false

log_prefix_true := log_
log_prefix_false :=

define gen_filenames
$(foreach yvar,$(yvars),$(foreach bw,$(bandwidths),$(foreach kernel,$(kernels),$(foreach log_stat,$(log_status),../output/rd_plot_$(log_prefix_$(log_stat))$(yvar)_bw$(bw)_$(kernel).pdf))))
endef

rd_plots := $(call gen_filenames)
qc_outputs := ../output/rd_run_qc.csv ../output/rd_run_qc.txt

all: $(rd_plots) $(qc_outputs)

$(qc_outputs): $(firstword $(rd_plots))

define run_rd
../output/rd_plot_$(log_prefix_$(log_stat))$(yvar)_bw$(bw)_$(kernel).pdf: spatial_rd_same_zone_only.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript spatial_rd_same_zone_only.R $(yvar) $(log_stat) $(bw) $(kernel) $$@
endef

$(foreach yvar,$(yvars),$(foreach bw,$(bandwidths),$(foreach kernel,$(kernels),$(foreach log_stat,$(log_status),$(eval $(run_rd))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
