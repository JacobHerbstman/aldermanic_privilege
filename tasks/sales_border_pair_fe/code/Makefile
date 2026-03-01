SHELL := bash
include ../../shell_functions.make

INPUT := ../input/sales_with_hedonics.parquet
FE_TEX := ../output/fe_table_sales_bw1000_year_quarter.tex
FE_CSV := ../output/fe_table_sales_bw1000_year_quarter.csv
YEAR_DIAG := ../output/year_diagnostics_sales_bw1000_year_quarter.csv
FE_GEOS := segment ward_pair
CLUSTERS := segment ward_pair
ROBUST_FE_OUTPUTS := $(foreach geo,$(FE_GEOS),$(foreach cl,$(CLUSTERS),$(if $(filter segment,$(geo)),$(if $(filter segment,$(cl)),,../output/fe_table_sales_bw1000_year_quarter_geo_$(geo)_clust_$(cl).tex),../output/fe_table_sales_bw1000_year_quarter_geo_$(geo)_clust_$(cl).tex)))

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(FE_TEX) $(ROBUST_FE_OUTPUTS)

$(FE_TEX): sales_border_pair_fe.R $(INPUT) | ../output
	# arg order: input bw_ft fe_time output_tex output_csv output_year_diag fe_geo cluster_level
	Rscript sales_border_pair_fe.R "$(INPUT)" 1000 year_quarter "$@" "$(FE_CSV)" "$(YEAR_DIAG)" segment segment

define FE_ROBUST_RULE
../output/fe_table_sales_bw1000_year_quarter_geo_$(1)_clust_$(2).tex: sales_border_pair_fe.R $(INPUT) | ../output
	Rscript sales_border_pair_fe.R "$(INPUT)" 1000 year_quarter "$$@" "../output/fe_table_sales_bw1000_year_quarter_geo_$(1)_clust_$(2).csv" "../output/year_diagnostics_sales_bw1000_year_quarter_geo_$(1)_clust_$(2).csv" $(1) $(2)
endef
$(foreach geo,$(FE_GEOS),$(foreach cl,$(CLUSTERS),$(if $(and $(filter segment,$(geo)),$(filter segment,$(cl))),,$(eval $(call FE_ROBUST_RULE,$(geo),$(cl))))))

$(INPUT): ../../prep_sales_border_data/output/sales_with_hedonics.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUT)

include ../../generic.make
