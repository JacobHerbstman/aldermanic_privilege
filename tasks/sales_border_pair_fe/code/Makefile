SHELL := bash
include ../../shell_functions.make

FE_GEOS := segment ward_pair
CLUSTERS := segment ward_pair
ROBUST_FE_OUTPUTS := $(foreach geo,$(FE_GEOS),$(foreach cl,$(CLUSTERS),$(if $(filter segment,$(geo)),$(if $(filter segment,$(cl)),,../output/fe_table_sales_bw1000_year_quarter_geo_$(geo)_clust_$(cl).tex),../output/fe_table_sales_bw1000_year_quarter_geo_$(geo)_clust_$(cl).tex)))

all: ../output/fe_table_sales_bw1000_year_quarter.tex $(ROBUST_FE_OUTPUTS)

../output/fe_table_sales_bw1000_year_quarter.tex: sales_border_pair_fe.R ../input/sales_with_hedonics.parquet | ../output
	Rscript sales_border_pair_fe.R ../input/sales_with_hedonics.parquet 1000 year_quarter "$@" ../output/fe_table_sales_bw1000_year_quarter.csv ../output/year_diagnostics_sales_bw1000_year_quarter.csv segment segment

../output/fe_table_sales_bw500_year_quarter.tex: sales_border_pair_fe.R ../input/sales_with_hedonics.parquet | ../output
	Rscript sales_border_pair_fe.R ../input/sales_with_hedonics.parquet 500 year_quarter "$@" ../output/fe_table_sales_bw500_year_quarter.csv ../output/year_diagnostics_sales_bw500_year_quarter.csv segment segment

define FE_ROBUST_RULE
../output/fe_table_sales_bw1000_year_quarter_geo_$(1)_clust_$(2).tex: sales_border_pair_fe.R ../input/sales_with_hedonics.parquet | ../output
	Rscript sales_border_pair_fe.R ../input/sales_with_hedonics.parquet 1000 year_quarter $$@ ../output/fe_table_sales_bw1000_year_quarter_geo_$(1)_clust_$(2).csv ../output/year_diagnostics_sales_bw1000_year_quarter_geo_$(1)_clust_$(2).csv $(1) $(2)
endef
$(foreach geo,$(FE_GEOS),$(foreach cl,$(CLUSTERS),$(if $(and $(filter segment,$(geo)),$(filter segment,$(cl))),,$(eval $(call FE_ROBUST_RULE,$(geo),$(cl))))))

../input/sales_with_hedonics.parquet: ../../prep_sales_border_data/output/sales_with_hedonics.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/sales_with_hedonics.parquet

include ../../generic.make
