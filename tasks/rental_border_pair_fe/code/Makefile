SHELL := bash
include ../../shell_functions.make

BWS_FT := 500 1000
WINDOWS := pre_covid pre_2021 full drop_mid
SAMPLE_FILTERS := all multifamily_only

OUTPUT_TEX := $(foreach bw,$(BWS_FT),$(foreach w,$(WINDOWS),$(foreach s,$(SAMPLE_FILTERS),../output/fe_table_rental_bw$(bw)_$(w)_$(s).tex)))

all: $(OUTPUT_TEX)

define FE_TEMPLATE
../output/fe_table_rental_bw$(1)_$(2)_$(3).tex: rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_border_pair_fe.R --bw_ft=$(1) --window=$(2) --sample_filter=$(3) --output_tex="$$@" --output_csv="../output/fe_table_rental_bw$(1)_$(2)_$(3).csv" --output_year_diag="../output/year_diagnostics_bw$(1)_$(2)_$(3).csv"
endef

$(foreach bw,$(BWS_FT),$(foreach w,$(WINDOWS),$(foreach s,$(SAMPLE_FILTERS),$(eval $(call FE_TEMPLATE,$(bw),$(w),$(s))))))

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../generic.make
