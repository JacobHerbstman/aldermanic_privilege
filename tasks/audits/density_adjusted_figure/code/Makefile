all: ../output/density_model_adjusted_rd.pdf \
	../output/density_model_adjusted_rd.png \
	../output/density_model_adjusted_estimates.csv \
	../output/density_flat_comparison_rd.pdf \
	../output/density_flat_comparison_rd.png \
	../output/density_flat_comparison_estimates.csv \
	../output/density_triangular_rd_500ft.pdf \
	../output/density_triangular_rd_500ft.png \
	../output/density_triangular_rd_500ft_paper_style.pdf \
	../output/density_triangular_rd_500ft_paper_style.png \
	../output/density_triangular_rd_500ft_errorbars.pdf \
	../output/density_triangular_rd_500ft_errorbars.png \
	../output/density_triangular_rd_selected_bandwidth.pdf \
	../output/density_triangular_rd_selected_bandwidth.png \
	../output/density_triangular_rd_estimates.csv \
	../output/density_nonparametric_bins_50ft.pdf \
	../output/density_nonparametric_bins_50ft.png \
	../output/density_nonparametric_bins_100ft.pdf \
	../output/density_nonparametric_bins_100ft.png \
	../output/density_nonparametric_bins_100ft_ribbons.pdf \
	../output/density_nonparametric_bins_100ft_ribbons.png \
	../output/density_nonparametric_bins_250ft_50ft_ribbons.pdf \
	../output/density_nonparametric_bins_250ft_50ft_ribbons.png \
	../output/density_nonparametric_bin_estimates.csv \
	../output/density_nonparametric_bins_100ft_current_no_income_ribbons.pdf \
	../output/density_nonparametric_bins_100ft_current_no_income_ribbons.png \
	../output/density_nonparametric_bins_100ft_education_added_back_ribbons.pdf \
	../output/density_nonparametric_bins_100ft_education_added_back_ribbons.png \
	../output/density_nonparametric_bins_100ft_income_added_back_ribbons.pdf \
	../output/density_nonparametric_bins_100ft_income_added_back_ribbons.png \
	../output/density_nonparametric_bins_100ft_all_covariates_ribbons.pdf \
	../output/density_nonparametric_bins_100ft_all_covariates_ribbons.png \
	../output/density_nonparametric_score_comparison.csv \
	../output/density_nonparametric_placebo_neg1000ft_100ft_ribbons.pdf \
	../output/density_nonparametric_placebo_neg1000ft_100ft_ribbons.png \
	../output/density_nonparametric_placebo_pos1000ft_100ft_ribbons.pdf \
	../output/density_nonparametric_placebo_pos1000ft_100ft_ribbons.png \
	../output/density_nonparametric_placebo_estimates.csv \
	../output/density_nonparametric_donut25ft_100ft_ribbons.pdf \
	../output/density_nonparametric_donut25ft_100ft_ribbons.png \
	../output/density_nonparametric_donut50ft_100ft_ribbons.pdf \
	../output/density_nonparametric_donut50ft_100ft_ribbons.png \
	../output/density_nonparametric_donut_estimates.csv \
	../output/density_nonparametric_boundary_feature_robustness.csv \
	../output/density_segment_classifier_review.csv \
	../output/density_segment_classifier_thresholds.csv \
	../output/density_segment_classifier_recalculation.csv \
	../output/density_segment_classifier_review.png \
	../output/density_segment_classifier_any_contact_review.png \
	../output/density_distance_band_estimates.pdf \
	../output/density_distance_band_estimates.png \
	../output/density_distance_band_estimates.csv \
	../output/density_residualized_binned_means.pdf \
	../output/density_residualized_binned_means.png \
	../output/density_residualized_binned_means.csv \
	../output/density_residualized_bins_with_lines.pdf \
	../output/density_residualized_bins_with_lines.png \
	../output/density_residualized_bins_without_lines.pdf \
	../output/density_residualized_bins_without_lines.png \
	../output/density_raw_bins_with_lines.pdf \
	../output/density_raw_bins_with_lines.png \
	../output/density_descriptive_binned_means.csv \
	../output/density_residualized_bins_without_lines_250ft.pdf \
	../output/density_residualized_bins_without_lines_250ft.png \
	../output/density_descriptive_binned_means_250ft.csv \
	../output/density_residualized_bins_without_lines_250ft_triangular.pdf \
	../output/density_residualized_bins_without_lines_250ft_triangular.png \
	../output/density_descriptive_binned_means_250ft_triangular.csv

../output/density_model_adjusted_rd.pdf: \
	build_density_model_adjusted_figure.R \
	../input/new_construction_analysis_data.csv
	$(R) $<

../output/density_model_adjusted_rd.png: ../output/density_model_adjusted_rd.pdf

../output/density_model_adjusted_estimates.csv: ../output/density_model_adjusted_rd.pdf

../output/density_flat_comparison_rd.pdf: ../output/density_model_adjusted_rd.pdf

../output/density_flat_comparison_rd.png: ../output/density_model_adjusted_rd.pdf

../output/density_flat_comparison_estimates.csv: ../output/density_model_adjusted_rd.pdf

../output/density_triangular_rd_500ft.pdf: \
	build_density_triangular_rd.R \
	../input/new_construction_analysis_data.csv
	$(R) $<

../output/density_triangular_rd_500ft.png: ../output/density_triangular_rd_500ft.pdf

../output/density_triangular_rd_500ft_paper_style.pdf: ../output/density_triangular_rd_500ft.pdf

../output/density_triangular_rd_500ft_paper_style.png: ../output/density_triangular_rd_500ft.pdf

../output/density_triangular_rd_500ft_errorbars.pdf: ../output/density_triangular_rd_500ft.pdf

../output/density_triangular_rd_500ft_errorbars.png: ../output/density_triangular_rd_500ft.pdf

../output/density_triangular_rd_selected_bandwidth.pdf: ../output/density_triangular_rd_500ft.pdf

../output/density_triangular_rd_selected_bandwidth.png: ../output/density_triangular_rd_500ft.pdf

../output/density_triangular_rd_estimates.csv: ../output/density_triangular_rd_500ft.pdf

../output/density_nonparametric_bins_50ft.pdf: \
	build_density_nonparametric_bins.R \
	../input/new_construction_analysis_data.csv
	$(R) $<

../output/density_nonparametric_bins_50ft.png: ../output/density_nonparametric_bins_50ft.pdf

../output/density_nonparametric_bins_100ft.pdf: ../output/density_nonparametric_bins_50ft.pdf

../output/density_nonparametric_bins_100ft.png: ../output/density_nonparametric_bins_50ft.pdf

../output/density_nonparametric_bins_100ft_ribbons.pdf: ../output/density_nonparametric_bins_50ft.pdf

../output/density_nonparametric_bins_100ft_ribbons.png: ../output/density_nonparametric_bins_50ft.pdf

../output/density_nonparametric_bins_250ft_50ft_ribbons.pdf: ../output/density_nonparametric_bins_50ft.pdf

../output/density_nonparametric_bins_250ft_50ft_ribbons.png: ../output/density_nonparametric_bins_50ft.pdf

../output/density_nonparametric_bin_estimates.csv: ../output/density_nonparametric_bins_50ft.pdf

../output/density_nonparametric_bins_100ft_current_no_income_ribbons.pdf: \
	build_density_nonparametric_score_comparison.R \
	../input/new_construction_analysis_data.csv \
	../input/current_income_scores.csv
	$(R) $<

../output/density_nonparametric_bins_100ft_current_no_income_ribbons.png: ../output/density_nonparametric_bins_100ft_current_no_income_ribbons.pdf

../output/density_nonparametric_bins_100ft_education_added_back_ribbons.pdf: ../output/density_nonparametric_bins_100ft_current_no_income_ribbons.pdf

../output/density_nonparametric_bins_100ft_education_added_back_ribbons.png: ../output/density_nonparametric_bins_100ft_current_no_income_ribbons.pdf

../output/density_nonparametric_bins_100ft_income_added_back_ribbons.pdf: ../output/density_nonparametric_bins_100ft_current_no_income_ribbons.pdf

../output/density_nonparametric_bins_100ft_income_added_back_ribbons.png: ../output/density_nonparametric_bins_100ft_current_no_income_ribbons.pdf

../output/density_nonparametric_bins_100ft_all_covariates_ribbons.pdf: ../output/density_nonparametric_bins_100ft_current_no_income_ribbons.pdf

../output/density_nonparametric_bins_100ft_all_covariates_ribbons.png: ../output/density_nonparametric_bins_100ft_current_no_income_ribbons.pdf

../output/density_nonparametric_score_comparison.csv: ../output/density_nonparametric_bins_100ft_current_no_income_ribbons.pdf

../output/density_nonparametric_placebo_neg1000ft_100ft_ribbons.pdf: \
	build_density_nonparametric_placebos.R \
	../input/new_construction_analysis_data.csv \
	../input/commercial_land_adjudication_evidence.csv
	$(R) $<

../output/density_nonparametric_placebo_neg1000ft_100ft_ribbons.png: ../output/density_nonparametric_placebo_neg1000ft_100ft_ribbons.pdf

../output/density_nonparametric_placebo_pos1000ft_100ft_ribbons.pdf: ../output/density_nonparametric_placebo_neg1000ft_100ft_ribbons.pdf

../output/density_nonparametric_placebo_pos1000ft_100ft_ribbons.png: ../output/density_nonparametric_placebo_neg1000ft_100ft_ribbons.pdf

../output/density_nonparametric_placebo_estimates.csv: ../output/density_nonparametric_placebo_neg1000ft_100ft_ribbons.pdf

../output/density_nonparametric_donut25ft_100ft_ribbons.pdf: ../output/density_nonparametric_placebo_neg1000ft_100ft_ribbons.pdf

../output/density_nonparametric_donut25ft_100ft_ribbons.png: ../output/density_nonparametric_placebo_neg1000ft_100ft_ribbons.pdf

../output/density_nonparametric_donut50ft_100ft_ribbons.pdf: ../output/density_nonparametric_placebo_neg1000ft_100ft_ribbons.pdf

../output/density_nonparametric_donut50ft_100ft_ribbons.png: ../output/density_nonparametric_placebo_neg1000ft_100ft_ribbons.pdf

../output/density_nonparametric_donut_estimates.csv: ../output/density_nonparametric_placebo_neg1000ft_100ft_ribbons.pdf

../output/density_nonparametric_boundary_feature_robustness.csv: \
	audit_density_boundary_features.R \
	../input/new_construction_analysis_data.csv \
	../input/segment_classification.csv
	$(R) $<

../output/density_segment_classifier_review.csv: \
	audit_segment_feature_classifier.R \
	../input/new_construction_analysis_data.csv \
	../input/segment_classification.csv \
	../input/boundary_segments_1320ft.gpkg \
	../input/major_streets.geojson \
	../input/gis_osm_roads_free_1.shp ../input/gis_osm_roads_free_1.dbf ../input/gis_osm_roads_free_1.prj ../input/gis_osm_roads_free_1.shx ../input/gis_osm_roads_free_1.cpg \
	../input/gis_osm_landuse_a_free_1.shp ../input/gis_osm_landuse_a_free_1.dbf ../input/gis_osm_landuse_a_free_1.prj ../input/gis_osm_landuse_a_free_1.shx ../input/gis_osm_landuse_a_free_1.cpg \
	../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg \
	../input/gis_osm_waterways_free_1.shp ../input/gis_osm_waterways_free_1.dbf ../input/gis_osm_waterways_free_1.prj ../input/gis_osm_waterways_free_1.shx ../input/gis_osm_waterways_free_1.cpg
	$(R) $<

../output/density_segment_classifier_thresholds.csv: ../output/density_segment_classifier_review.csv

../output/density_segment_classifier_recalculation.csv: ../output/density_segment_classifier_review.csv

../output/density_segment_classifier_review.png: ../output/density_segment_classifier_review.csv

../output/density_segment_classifier_any_contact_review.png: ../output/density_segment_classifier_review.csv

../output/density_distance_band_estimates.pdf: \
	build_density_distance_band_estimates.R \
	../input/new_construction_analysis_data.csv
	$(R) $<

../output/density_distance_band_estimates.png: ../output/density_distance_band_estimates.pdf

../output/density_distance_band_estimates.csv: ../output/density_distance_band_estimates.pdf

../output/density_residualized_binned_means.pdf: ../output/density_distance_band_estimates.pdf

../output/density_residualized_binned_means.png: ../output/density_distance_band_estimates.pdf

../output/density_residualized_binned_means.csv: ../output/density_distance_band_estimates.pdf

../output/density_residualized_bins_with_lines.pdf: \
	build_density_descriptive_binned_plots.R \
	../input/new_construction_analysis_data.csv
	$(R) $<

../output/density_residualized_bins_with_lines.png: ../output/density_residualized_bins_with_lines.pdf

../output/density_residualized_bins_without_lines.pdf: ../output/density_residualized_bins_with_lines.pdf

../output/density_residualized_bins_without_lines.png: ../output/density_residualized_bins_with_lines.pdf

../output/density_raw_bins_with_lines.pdf: ../output/density_residualized_bins_with_lines.pdf

../output/density_raw_bins_with_lines.png: ../output/density_residualized_bins_with_lines.pdf

../output/density_descriptive_binned_means.csv: ../output/density_residualized_bins_with_lines.pdf

../output/density_residualized_bins_without_lines_250ft.pdf: ../output/density_residualized_bins_with_lines.pdf

../output/density_residualized_bins_without_lines_250ft.png: ../output/density_residualized_bins_with_lines.pdf

../output/density_descriptive_binned_means_250ft.csv: ../output/density_residualized_bins_with_lines.pdf

../output/density_residualized_bins_without_lines_250ft_triangular.pdf: ../output/density_residualized_bins_with_lines.pdf

../output/density_residualized_bins_without_lines_250ft_triangular.png: ../output/density_residualized_bins_with_lines.pdf

../output/density_descriptive_binned_means_250ft_triangular.csv: ../output/density_residualized_bins_with_lines.pdf

../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/current_income_scores.csv: ../../income_score_current_results/output/current_income_scores.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/commercial_land_adjudication_evidence.csv: ../../commercial_new_construction_sample_audit/output/commercial_land_adjudication_evidence.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/segment_classification.csv: ../../../border_segment_creation/output/segment_classification.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/boundary_segments_1320ft.gpkg: ../../../border_segment_creation/output/boundary_segments_1320ft.gpkg
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/major_streets.geojson: ../../../download_chicago_spatial_data/output/major_streets.geojson
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/gis_osm_roads_free_1.%: ../../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.%
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/gis_osm_landuse_a_free_1.%: ../../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.%
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/gis_osm_water_a_free_1.%: ../../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.%
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/gis_osm_waterways_free_1.%: ../../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.%
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

link-inputs: ../input/new_construction_analysis_data.csv \
	../input/current_income_scores.csv \
	../input/commercial_land_adjudication_evidence.csv \
	../input/segment_classification.csv \
	../input/boundary_segments_1320ft.gpkg \
	../input/major_streets.geojson \
	../input/gis_osm_roads_free_1.shp ../input/gis_osm_roads_free_1.dbf ../input/gis_osm_roads_free_1.prj ../input/gis_osm_roads_free_1.shx ../input/gis_osm_roads_free_1.cpg \
	../input/gis_osm_landuse_a_free_1.shp ../input/gis_osm_landuse_a_free_1.dbf ../input/gis_osm_landuse_a_free_1.prj ../input/gis_osm_landuse_a_free_1.shx ../input/gis_osm_landuse_a_free_1.cpg \
	../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg \
	../input/gis_osm_waterways_free_1.shp ../input/gis_osm_waterways_free_1.dbf ../input/gis_osm_waterways_free_1.prj ../input/gis_osm_waterways_free_1.shx ../input/gis_osm_waterways_free_1.cpg

include ../../../shell_functions.make
include ../../../generic.make
