include ../../shared/code/shell_functions.make

all: ../output/ward_pair_1-26_500ft.pdf ../output/ward_map_2024.pdf \
	../output/density_rd_slides.pdf ../output/density_placebo_neg1000ft_slides.pdf \
	../output/density_placebo_pos1000ft_slides.pdf ../output/density_donut25ft_slides.pdf \
	../output/density_donut50ft_slides.pdf \
	../output/density_boundary_robustness_slides.tex ../output/density_location_continuity_slides.tex \
	../output/price_boundary_main_slides.pdf ../output/price_boundary_placebos_slides.pdf \
	../output/price_boundary_straight_slides.pdf ../output/price_boundary_donuts_slides.pdf

../output/ward_pair_1-26_500ft.pdf: plot_ward_pair.R ../input/permit_block_year_panel_2015.parquet \
	../input/census_blocks_2010.csv ../input/ward_panel.gpkg | ../output
	$(R) $<

../output/ward_map_2024.pdf: plot_ward_map.R ../input/Wards_2024.geojson | ../output
	$(R) $<

../%/density_rd_slides.pdf ../%/density_placebo_neg1000ft_slides.pdf ../%/density_placebo_pos1000ft_slides.pdf \
	../%/density_donut25ft_slides.pdf ../%/density_donut50ft_slides.pdf: plot_density_slides.R \
	../input/new_construction_analysis_data.csv | ../output
	$(R) $<

../%/density_boundary_robustness_slides.tex ../%/density_location_continuity_slides.tex: build_density_tables_slides.R \
	../input/new_construction_analysis_data.csv ../input/density_boundary_characteristics.csv | ../output
	$(R) $<

../%/price_boundary_main_slides.pdf ../%/price_boundary_placebos_slides.pdf ../%/price_boundary_straight_slides.pdf \
	../%/price_boundary_donuts_slides.pdf: plot_price_slides.R ../input/price_boundary_estimates.csv | ../output
	$(R) $<

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	ln -sf $< $@
../input/census_blocks_2010.csv: ../../download_chicago_spatial_data/output/census_blocks_2010.csv | ../input
	ln -sf $< $@
../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@
../input/Wards_2024.geojson: ../../../data_raw/Wards_2024.geojson | ../input
	ln -sf $< $@
../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@
../input/density_boundary_characteristics.csv: ../../density_boundary_characteristics/output/density_boundary_characteristics.csv | ../input
	ln -sf $< $@
../input/price_boundary_estimates.csv: ../../price_boundary_results/output/price_boundary_estimates.csv | ../input
	ln -sf $< $@

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/census_blocks_2010.csv ../input/ward_panel.gpkg \
	../input/Wards_2024.geojson ../input/new_construction_analysis_data.csv ../input/density_boundary_characteristics.csv \
	../input/price_boundary_estimates.csv

include ../../shared/code/generic.make
