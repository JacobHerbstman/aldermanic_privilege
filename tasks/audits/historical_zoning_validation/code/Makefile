all: ../output/historical_zoning_validation_summary.csv \
	../output/historical_zoning_project_comparison.csv \
	../output/historical_zoning_event_coverage.csv \
	../output/historical_zoning_mismatches.csv \
	../output/historical_zoning_matter_parsing.csv \
	../output/historical_zoning_subarea_transitions.csv \
	../output/historical_zoning_event_address_ranges.csv \
	../output/historical_zoning_event_links.csv \
	../output/historical_zoning_linkage_sensitivity.csv \
	../output/historical_zoning_application_validation.csv \
	../output/historical_zoning_application_crosswalk.csv \
	../output/historical_zoning_event_date_parcel_candidates.csv \
	../output/historical_zoning_forward_reconciliation.csv \
	../output/historical_zoning_forward_validation_summary.csv \
	../output/historical_zoning_polygon_lineage.csv \
	../output/historical_zoning_polygon_event_evidence.csv \
	../output/historical_zoning_polygon_reconciliation.csv \
	../output/historical_zoning_polygon_validation_summary.csv \
	../output/historical_zoning_ordinances_2006_2012.csv \
	../output/historical_zoning_2006_project_map.csv \
	../output/historical_zoning_2006_reconstruction_summary.csv \
	../output/historical_zoning_2006_manual_audit.csv \
	../output/historical_zoning_2006_manual_audit_summary.csv \
	../output/historical_zoning_2006_candidate.gpkg \
	../output/historical_zoning_2006_candidate_summary.csv \
	../output/historical_zoning_2006_candidate_unresolved.csv \
	../output/historical_zoning_2006_candidate_project_check.csv \
	../output/historical_zoning_2006_candidate_project_summary.csv \
	../output/historical_zoning_2006_repeat_event_audit.csv \
	../output/historical_zoning_2006_repeat_event_audit_summary.csv \
	../output/historical_zoning_2006_repeat_event_screen_paper_sample.csv \
	../output/historical_zoning_2006_repeat_event_screen_paper_sample_summary.csv \
	../output/historical_zoning_2006_repeat_event_review_paper_sample.csv \
	../output/historical_zoning_2006_repeat_event_project_review_paper_sample.csv \
	../output/historical_zoning_2006_repeat_event_review_paper_sample_summary.csv \
	../output/historical_zoning_2006_project_map_reviewed_paper_sample.csv \
	../output/historical_zoning_2006_project_map_reviewed_paper_sample_summary.csv \
	../output/historical_zoning_paper_sample_history_seeds.csv \
	../output/historical_zoning_paper_sample_history_seed_summary.csv \
	../output/historical_zoning_paper_sample_seed_reviews.csv \
	../output/historical_zoning_a7077_project_review.csv \
	../output/historical_zoning_project_construction_year.csv \
	../output/historical_zoning_project_construction_event_log.csv \
	../output/historical_zoning_project_construction_reviewed_events.csv \
	../output/historical_zoning_project_construction_year_unresolved.csv \
	../output/historical_zoning_project_construction_year_summary.csv \
	../output/historical_zoning_project_construction_year_lookup.csv \
	../output/historical_zoning_paper_sample_validation.csv \
	../output/historical_zoning_paper_sample_validation_summary.csv \
	../output/historical_zoning_construction_spotcheck.csv \
	../output/historical_zoning_construction_spotcheck_documents.csv \
	../output/historical_zoning_construction_spotcheck_summary.csv \
	../output/historical_zoning_density_model_comparison.csv \
	../output/historical_zoning_density_sample_comparison.csv \
	../output/historical_zoning_2016_to_2012_backcast_project_check.csv \
	../output/historical_zoning_2016_to_2012_backcast_mismatches.csv \
	../output/historical_zoning_2016_to_2012_backcast_summary.csv \
	../output/historical_zoning_2016_to_2012_backcast_polygon_check.csv \
	../output/historical_zoning_2016_to_2012_backcast_polygon_summary.csv \
	../output/historical_zoning_2016_to_2014_backcast_project_check.csv \
	../output/historical_zoning_2016_to_2014_backcast_mismatches.csv \
	../output/historical_zoning_2016_to_2014_backcast_summary.csv \
	../output/historical_zoning_2016_to_2014_backcast_polygon_check.csv \
	../output/historical_zoning_2016_to_2014_backcast_polygon_summary.csv

../output/historical_zoning_validation_summary.csv: validate_historical_zoning.R ../input/parcels_with_geometry.gpkg ../input/zoning_matters_far_20101101_20201231.csv ../input/rezoning_parcel_matches_20101101_20201231.csv ../input/pdf_text_20101101_20260212.csv ../input/parcel_addresses_2025_chicago.csv ../input/zoning_nov2012.zip ../input/zoning_sep2014.zip ../input/zoning_jan2016.zip ../input/zoning_sep2025.geojson | ../output
	$(R) $<

../output/historical_zoning_project_comparison.csv \
../output/historical_zoning_event_coverage.csv \
../output/historical_zoning_mismatches.csv \
../output/historical_zoning_matter_parsing.csv \
../output/historical_zoning_subarea_transitions.csv \
../output/historical_zoning_event_address_ranges.csv \
../output/historical_zoning_event_links.csv \
../output/historical_zoning_linkage_sensitivity.csv \
../output/historical_zoning_application_validation.csv \
../output/historical_zoning_application_crosswalk.csv: ../output/historical_zoning_validation_summary.csv
	@test -s "$@"

../output/historical_zoning_event_date_parcel_candidates.csv: reconcile_forward_lineage.R ../output/historical_zoning_validation_summary.csv ../output/historical_zoning_mismatches.csv ../output/historical_zoning_project_comparison.csv ../output/historical_zoning_event_address_ranges.csv ../output/historical_zoning_matter_parsing.csv ../output/historical_zoning_application_crosswalk.csv ../input/rezoning_parcel_matches_20101101_20201231.csv ../input/parcels_with_geometry.gpkg | ../output
	$(R) $<

../output/historical_zoning_forward_reconciliation.csv \
../output/historical_zoning_forward_validation_summary.csv: ../output/historical_zoning_event_date_parcel_candidates.csv
	@test -s "$@"

../output/historical_zoning_polygon_validation_summary.csv: validate_forward_with_parcel_polygons.R ../output/historical_zoning_project_comparison.csv ../output/historical_zoning_matter_parsing.csv ../output/historical_zoning_event_address_ranges.csv ../input/rezoning_parcel_matches_20101101_20201231.csv ../input/parcel_2012.zip ../input/parcel_2013.zip ../input/parcel_2014.zip ../input/parcel_2015.zip ../input/parcel_2016.zip ../input/parcel_2017.zip ../input/parcel_2018.zip ../input/parcel_2019.zip ../input/parcel_2020.zip ../input/parcel_2021.zip ../input/parcel_2022.zip | ../output
	$(R) $<

../output/historical_zoning_polygon_lineage.csv \
../output/historical_zoning_polygon_event_evidence.csv \
../output/historical_zoning_polygon_reconciliation.csv: ../output/historical_zoning_polygon_validation_summary.csv
	@test -s "$@"

../output/historical_zoning_ordinances_2006_2012.csv: extract_historical_ordinances.py ../input/journal_manifest_2006_2012.csv ../input/journals_2006_2012 | ../output
	$(PYTHON) $<

../output/historical_zoning_project_map_grids.csv: assign_zoning_map_grids.R ../output/historical_zoning_project_comparison.csv ../input/zoning_map_index.geojson | ../output
	$(R) $<

../output/historical_zoning_2006_project_map.csv: reconstruct_2006_project_zoning.py ../output/historical_zoning_ordinances_2006_2012.csv ../output/historical_zoning_project_comparison.csv ../output/historical_zoning_matter_parsing.csv ../output/historical_zoning_project_map_grids.csv | ../output
	$(PYTHON) $<

../output/historical_zoning_2006_reconstruction_summary.csv: ../output/historical_zoning_2006_project_map.csv
	@test -s "$@"

../output/historical_zoning_2006_manual_audit.csv: audit_2006_reconstruction_sample.py ../output/historical_zoning_2006_project_map.csv | ../output
	$(PYTHON) $<

../output/historical_zoning_2006_manual_audit_summary.csv: ../output/historical_zoning_2006_manual_audit.csv
	@test -s "$@"

../output/historical_zoning_2006_candidate.gpkg: build_candidate_2006_zoning_map.R ../input/zoning_nov2012.zip ../input/zoning_map_index.geojson ../output/historical_zoning_ordinances_2006_2012.csv ../output/historical_zoning_matter_parsing.csv | ../output
	$(R) $<

../output/historical_zoning_2006_candidate_summary.csv \
../output/historical_zoning_2006_candidate_unresolved.csv: ../output/historical_zoning_2006_candidate.gpkg
	@test -s "$@"

../output/historical_zoning_2006_candidate_project_check.csv: check_candidate_2006_zoning_map.R ../output/historical_zoning_2006_candidate.gpkg ../output/historical_zoning_2006_project_map.csv ../input/parcels_with_geometry.gpkg | ../output
	$(R) $<

../output/historical_zoning_2006_candidate_project_summary.csv: ../output/historical_zoning_2006_candidate_project_check.csv
	@test -s "$@"

../output/historical_zoning_2006_repeat_event_audit.csv: audit_2006_repeat_events.py ../output/historical_zoning_2006_manual_audit.csv ../output/historical_zoning_ordinances_2006_2012.csv | ../output
	$(PYTHON) $<

../output/historical_zoning_2006_repeat_event_audit_summary.csv: ../output/historical_zoning_2006_repeat_event_audit.csv
	@test -s "$@"

../output/historical_zoning_2006_repeat_event_screen_paper_sample.csv: screen_2006_repeat_events.py ../output/historical_zoning_2006_project_map.csv ../output/historical_zoning_ordinances_2006_2012.csv | ../output
	$(PYTHON) $<

../output/historical_zoning_2006_repeat_event_screen_paper_sample_summary.csv: ../output/historical_zoning_2006_repeat_event_screen_paper_sample.csv
	@test -s "$@"

../output/historical_zoning_2006_repeat_event_review_paper_sample.csv: audit_2006_repeat_events.py ../output/historical_zoning_2006_repeat_event_screen_paper_sample.csv ../output/historical_zoning_2006_project_map.csv | ../output
	$(PYTHON) $<

../output/historical_zoning_2006_repeat_event_project_review_paper_sample.csv \
../output/historical_zoning_2006_repeat_event_review_paper_sample_summary.csv \
../output/historical_zoning_2006_project_map_reviewed_paper_sample.csv \
../output/historical_zoning_2006_project_map_reviewed_paper_sample_summary.csv: ../output/historical_zoning_2006_repeat_event_review_paper_sample.csv
	@test -s "$@"

../output/historical_zoning_paper_sample_history_seeds.csv: resolve_paper_sample_history_seeds.py ../output/historical_zoning_2006_project_map_reviewed_paper_sample.csv ../input/parcel_addresses_2025_chicago.csv | ../output
	$(PYTHON) $<

../output/historical_zoning_paper_sample_history_seed_summary.csv \
../output/historical_zoning_paper_sample_seed_reviews.csv \
../output/historical_zoning_a7077_project_review.csv: ../output/historical_zoning_paper_sample_history_seeds.csv
	@test -s "$@"

../output/historical_zoning_project_construction_year.csv: build_project_construction_zoning.py ../output/historical_zoning_paper_sample_history_seeds.csv ../output/historical_zoning_project_comparison.csv ../output/historical_zoning_event_links.csv ../output/historical_zoning_2006_repeat_event_review_paper_sample.csv ../output/historical_zoning_matter_parsing.csv ../output/historical_zoning_ordinances_2006_2012.csv ../input/matters_20101101_20260212.csv ../input/O2017-7056.pdf ../input/SO2017-7051.pdf ../input/SO2018-863.pdf ../input/SO2018-7749.pdf ../input/O2020-2959.pdf ../input/O2024-0008982.pdf ../input/O2024-0010153.pdf ../input/SO2018-4452.pdf ../input/SO2019-4107.pdf ../input/O2022-2464.pdf ../input/SO2023-43.pdf ../input/O2023-1331.pdf | ../output
	$(PYTHON) $<

../output/historical_zoning_project_construction_event_log.csv \
../output/historical_zoning_project_construction_reviewed_events.csv \
../output/historical_zoning_project_construction_year_unresolved.csv \
../output/historical_zoning_project_construction_year_summary.csv: ../output/historical_zoning_project_construction_year.csv
	@test -s "$@" || test "$@" = "../output/historical_zoning_project_construction_year_unresolved.csv"

../output/historical_zoning_project_construction_year_lookup.csv: validate_paper_sample_construction_zoning.py ../output/historical_zoning_project_construction_year.csv ../output/historical_zoning_project_construction_reviewed_events.csv ../output/historical_zoning_paper_sample_seed_reviews.csv ../output/historical_zoning_2016_to_2012_backcast_project_check.csv ../output/historical_zoning_2016_to_2014_backcast_project_check.csv ../input/parcels_with_ward_distances.csv | ../output
	$(PYTHON) $<

../output/historical_zoning_paper_sample_validation.csv \
../output/historical_zoning_paper_sample_validation_summary.csv: ../output/historical_zoning_project_construction_year_lookup.csv
	@test -s "$@"

../output/historical_zoning_construction_spotcheck.csv: audit_construction_zoning_spotcheck.py ../output/historical_zoning_project_construction_year.csv ../output/historical_zoning_project_comparison.csv ../output/historical_zoning_project_construction_event_log.csv ../input/parcel_addresses_2025_chicago.csv ../input/official_ordinance_pdfs | ../output
	$(PYTHON) $<

../output/historical_zoning_construction_spotcheck_documents.csv \
../output/historical_zoning_construction_spotcheck_summary.csv: ../output/historical_zoning_construction_spotcheck.csv
	@test -s "$@"

../output/historical_zoning_density_model_comparison.csv: estimate_density_with_construction_zoning.R ../output/historical_zoning_project_construction_year.csv ../input/parcels_with_ward_distances.csv | ../output
	$(R) $<

../output/historical_zoning_density_sample_comparison.csv: ../output/historical_zoning_density_model_comparison.csv
	@test -s "$@"

../output/historical_zoning_2016_to_2012_backcast_project_check.csv: backcast_2016_to_2012.R ../input/zoning_nov2012.zip ../input/zoning_jan2016.zip ../input/parcels_with_geometry.gpkg ../output/historical_zoning_matter_parsing.csv | ../output
	$(R) $<

../output/historical_zoning_2016_to_2012_backcast_mismatches.csv \
../output/historical_zoning_2016_to_2012_backcast_summary.csv \
../output/historical_zoning_2016_to_2012_backcast_polygon_check.csv \
../output/historical_zoning_2016_to_2012_backcast_polygon_summary.csv: ../output/historical_zoning_2016_to_2012_backcast_project_check.csv
	@test -s "$@"

../output/historical_zoning_2016_to_2014_backcast_project_check.csv: backcast_2016_to_2014.R ../input/zoning_sep2014.zip ../input/zoning_jan2016.zip ../input/parcels_with_geometry.gpkg ../output/historical_zoning_matter_parsing.csv | ../output
	$(R) $<

../output/historical_zoning_2016_to_2014_backcast_mismatches.csv \
../output/historical_zoning_2016_to_2014_backcast_summary.csv \
../output/historical_zoning_2016_to_2014_backcast_polygon_check.csv \
../output/historical_zoning_2016_to_2014_backcast_polygon_summary.csv: ../output/historical_zoning_2016_to_2014_backcast_project_check.csv
	@test -s "$@"

../input/parcels_with_geometry.gpkg: ../../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/zoning_matters_far_20101101_20201231.csv: ../../../rezoning_far_pre_geocode/output/zoning_matters_far_20101101_20201231.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rezoning_parcel_matches_20101101_20201231.csv: ../../../rezoning_geocode_stage1_parcel/output/rezoning_parcel_matches_20101101_20201231.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/pdf_text_20101101_20260212.csv: ../../../elms_pdf_processing/output/pdf_text_20101101_20260212.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/matters_20101101_20260212.csv: ../../../elms_fetch_ordinances/output/matters_20101101_20260212.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/SO2018-4452.pdf: ../../../elms_pdf_processing/output/pdfs_20101101_20260212/matter_SO2018-4452_attachment_elms_cac635ecfa869079.pdf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/SO2019-4107.pdf: ../../../elms_pdf_processing/output/pdfs_20101101_20260212/matter_SO2019-4107_attachment_elms_b8784cf690640969.pdf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/O2022-2464.pdf: ../../../elms_pdf_processing/output/pdfs_20101101_20260212/matter_O2022-2464_attachment_elms_51ab69150565d7b1.pdf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/SO2023-43.pdf: ../../../elms_pdf_processing/output/pdfs_20101101_20260212/matter_SO2023-43_attachment_elms_61703d5764e354b8.pdf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/O2023-1331.pdf: ../../../elms_pdf_processing/output/pdfs_20101101_20260212/matter_O2023-1331_attachment_elms_719441e8a2fb6ce1.pdf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/O2017-7056.pdf: ../../../elms_pdf_processing/output/pdfs_20101101_20260212/matter_O2017-7056_attachment_elms_58301722eb761788.pdf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/SO2017-7051.pdf: ../../../elms_pdf_processing/output/pdfs_20101101_20260212/matter_SO2017-7051_attachment_elms_e1f69a7d1b07f174.pdf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/SO2018-863.pdf: ../../../elms_pdf_processing/output/pdfs_20101101_20260212/matter_SO2018-863_attachment_elms_bffda0ec0f3ecc58.pdf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/SO2018-7749.pdf: ../../../elms_pdf_processing/output/pdfs_20101101_20260212/matter_SO2018-7749_attachment_elms_c61ebd9f98df8a98.pdf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/O2020-2959.pdf: ../../../elms_pdf_processing/output/pdfs_20101101_20260212/matter_O2020-2959_attachment_elms_14e914a5b1910440.pdf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/O2024-0008982.pdf: ../../../elms_pdf_processing/output/pdfs_20101101_20260212/matter_O2024-0008982_attachment_elms_5d958308f85fcfeb.pdf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/O2024-0010153.pdf: ../../../elms_pdf_processing/output/pdfs_20101101_20260212/matter_O2024-0010153_attachment_elms_b706faa6c555dd2c.pdf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_addresses_2025_chicago.csv: ../../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/official_ordinance_pdfs: ../../../elms_pdf_processing/output/pdfs_20101101_20260212 | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -snf "$<" "$@"

../input/zoning_sep2025.geojson: ../../../../data_raw/Boundaries_-_Zoning_Districts_20250910.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/journal_manifest_2006_2012.csv: ../../../clerk_journals_download/output/journal_manifest_2006_2012.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/journals_2006_2012: ../../../clerk_journals_download/output/journals | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -snf "$<" "$@"

../input/zoning_map_index.geojson: | ../input
	curl --fail --get --output "$@.tmp" \
		--data-urlencode 'where=1=1' \
		--data-urlencode 'outFields=ZONE_MAP' \
		--data-urlencode 'returnGeometry=true' \
		--data-urlencode 'outSR=3435' \
		--data-urlencode 'f=geojson' \
		'https://gisapps.chicago.gov/arcgis/rest/services/ExternalApps/Zoning_update/MapServer/22/query'
	mv "$@.tmp" "$@"

../input/zoning_nov2012.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://data.cityofchicago.org/download/p8va-airx/application%2Fzip'
	mv "$@.tmp" "$@"

../input/zoning_sep2014.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://data.cityofchicago.org/api/views/nifi-zqag/files/3gBVikfONpTErJ8ySFbKtWZIRxCsyorpU8rSyy3UgZ8?download=true&filename=Zoning.zip'
	mv "$@.tmp" "$@"

../input/zoning_jan2016.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://data.cityofchicago.org/api/views/xfyf-x4kx/files/f0ee4010-ba7f-4355-9f74-f36e48e9cc52?download=true&filename=zoning_2016_01.zip'
	mv "$@.tmp" "$@"

../input/parcel_2012.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://hub.arcgis.com/api/v3/datasets/bd26024e1c6546d6a86ad384a7a31765_0/downloads/data?format=shp&spatialRefId=3435&where=1%3D1'
	mv "$@.tmp" "$@"

../input/parcel_2013.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://hub.arcgis.com/api/v3/datasets/ea846a11e7a64c6eb7eafcd132c88484_0/downloads/data?format=shp&spatialRefId=3435&where=1%3D1'
	mv "$@.tmp" "$@"

../input/parcel_2014.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://hub.arcgis.com/api/v3/datasets/f2d470e08ab441229f6d310fb8c625ab_0/downloads/data?format=shp&spatialRefId=3435&where=1%3D1'
	mv "$@.tmp" "$@"

../input/parcel_2015.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://hub.arcgis.com/api/v3/datasets/cb0a110357284b1ab23dedc6d0a34c57_0/downloads/data?format=shp&spatialRefId=3435&where=1%3D1'
	mv "$@.tmp" "$@"

../input/parcel_2016.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://hub.arcgis.com/api/v3/datasets/0b86fc37d99c413a8b70a1c2bfc895ba_0/downloads/data?format=shp&spatialRefId=3435&where=1%3D1'
	mv "$@.tmp" "$@"

../input/parcel_2017.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://hub.arcgis.com/api/v3/datasets/a45722101ed8491fb71930fd4c2c64ab_0/downloads/data?format=shp&spatialRefId=3435&where=1%3D1'
	mv "$@.tmp" "$@"

../input/parcel_2018.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://hub.arcgis.com/api/v3/datasets/9539568a52124b99addb042efd0f83b1_0/downloads/data?format=shp&spatialRefId=3435&where=1%3D1'
	mv "$@.tmp" "$@"

../input/parcel_2019.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://hub.arcgis.com/api/v3/datasets/3d3375ac11d147308815d5cf4bb43f4e_0/downloads/data?format=shp&spatialRefId=3435&where=1%3D1'
	mv "$@.tmp" "$@"

../input/parcel_2020.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://maps.cookcountyil.gov/files/opendata/parcels/Parcels2020.gdb.zip'
	mv "$@.tmp" "$@"

../input/parcel_2021.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://hub.arcgis.com/api/v3/datasets/34021b4f3b834a69bf737e6c3344888e_0/downloads/data?format=shp&spatialRefId=3435&where=1%3D1'
	mv "$@.tmp" "$@"

../input/parcel_2022.zip: | ../input
	curl --fail --location --output "$@.tmp" 'https://hub.arcgis.com/api/v3/datasets/d7f375b66b314dfd9f13ead523f615c1_0/downloads/data?format=shp&spatialRefId=3435&where=1%3D1'
	mv "$@.tmp" "$@"

link-inputs: ../input/parcels_with_geometry.gpkg ../input/parcels_with_ward_distances.csv ../input/zoning_matters_far_20101101_20201231.csv ../input/rezoning_parcel_matches_20101101_20201231.csv ../input/pdf_text_20101101_20260212.csv ../input/matters_20101101_20260212.csv ../input/SO2018-4452.pdf ../input/SO2019-4107.pdf ../input/O2022-2464.pdf ../input/SO2023-43.pdf ../input/O2023-1331.pdf ../input/parcel_addresses_2025_chicago.csv ../input/zoning_sep2025.geojson ../input/journal_manifest_2006_2012.csv ../input/journals_2006_2012 ../input/zoning_map_index.geojson

include ../../../shell_functions.make
include ../../../generic.make
