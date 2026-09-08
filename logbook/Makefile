include ../tasks/shared/code/shell_functions.make

all: logbook.pdf

logbook.pdf: entries/2026-09-07-geocode-address-review.tex ../tasks/working_paper_release_audit/report/geocoder_mismatch_review.csv.log ../tasks/working_paper_release_audit/report/geocoding_parcel_history_review.csv.log ../tasks/download_construction_historical_parcels/report/preferred_predecessor_parcel_source.gpkg.log ../tasks/download_construction_historical_parcels/report/preferred_predecessor_source_queries.csv.log ../tasks/new_construction_cleaning/report/preferred_predecessor_reference_points.csv.log ../tasks/new_construction_cleaning/report/preferred_historical_predecessor_resolution.csv.log ../tasks/new_construction_cleaning/report/preferred_historical_predecessor_selected.gpkg.log

logbook.pdf: entries/2026-09-07-address-geocode-replay.tex ../tasks/download_construction_address_geocodes/report/address_geocodes_census.csv.log ../tasks/download_construction_address_geocodes/report/address_geocodes_chicago.csv.log ../tasks/download_construction_historical_parcels/report/preferred_historical_parcel_source.gpkg.log ../tasks/new_construction_cleaning/report/preferred_historical_parcel_coverage.csv.log ../tasks/new_construction_cleaning/report/preferred_address_geocode_requests.csv.log ../tasks/new_construction_cleaning/report/preferred_historical_address_geocodes.csv.log ../tasks/new_construction_cleaning/report/preferred_chicago_address_geocodes.csv.log

logbook.pdf: entries/2026-09-07-predecessor-recovery.tex ../tasks/download_construction_historical_parcels/report/predecessor_parcel_history.csv.log ../tasks/download_construction_historical_parcels/report/historical_predecessor_parcel_source.gpkg.log ../tasks/new_construction_cleaning/report/missing_project_reference_points.csv.log ../tasks/new_construction_cleaning/report/historical_project_predecessor_resolution.csv.log ../tasks/new_construction_cleaning/report/historical_project_year_geometry.gpkg.log

logbook.pdf: entries/2026-09-07-historical-parcel-coverage.tex ../tasks/download_construction_historical_parcels/report/historical_project_parcel_source.gpkg.log ../tasks/download_construction_historical_parcels/report/historical_project_parcel_source_queries.csv.log ../tasks/new_construction_cleaning/report/historical_project_parcel_requests.csv.log ../tasks/new_construction_cleaning/report/historical_project_parcel_coverage.csv.log

logbook.pdf: entries/2026-09-07-city-footprint-reconstruction.tex ../tasks/new_construction_cleaning/report/commercial_city_building_footprints.gpkg.log ../tasks/new_construction_cleaning/report/commercial_ground_up_evidence.csv.log ../tasks/new_construction_cleaning/report/preferred_commercial_projects.csv.log

logbook.pdf: entries/2026-09-07-natchez-b-review.tex ../tasks/working_paper_release_audit/output/natchez_b_land_coverage.csv

logbook.pdf: entries/2026-09-07-natchez-land-resolution.tex ../tasks/working_paper_release_audit/output/natchez_building_land_coverage.csv ../tasks/working_paper_release_audit/output/natchez_parcel_coverage.pdf ../tasks/download_construction_development_records/output/natchez_pd1345.pdf

logbook.pdf: entries/2026-09-07-final-group-decisions.tex ../tasks/new_construction_cleaning/adjudication/commercial_component_overrides.csv

logbook.pdf: entries/2026-09-07-construction-completion-followup.tex ../tasks/download_construction_development_records/output/northmarq_chicago_2025q2.pdf

logbook.pdf: entries/2026-09-07-construction-definition.tex ../tasks/new_construction_cleaning/adjudication/commercial_manual_decisions.csv ../tasks/new_construction_cleaning/adjudication/commercial_cross_family_decisions.csv ../tasks/new_construction_cleaning/adjudication/project_manual_reviews.csv

logbook.pdf: entries/2026-09-07-phased-project-evidence.tex ../tasks/working_paper_release_audit/phased_project_review.md ../tasks/working_paper_release_audit/output/phased_project_permits.csv ../tasks/download_construction_development_records/output/grove_pd1395.pdf

logbook.pdf: entries/2026-09-07-complete-assessor-snapshots.tex entries/2026-09-07-residential-assessor-branch.tex ../tasks/prepare_construction_assessor_history/README.md ../tasks/new_construction_cleaning/output/residential_assessor_project_candidates.csv entries/2026-09-07-construction-chronology.tex ../tasks/new_construction_cleaning/README.md entries/2026-09-07-general-construction-rules.tex ../tasks/new_construction_cleaning/adjudication/multicard_external_web_reviews.csv ../tasks/new_construction_cleaning/adjudication/multicard_manual_episode_decisions.csv ../tasks/new_construction_cleaning/adjudication/residential_unresolved_source_dispositions.csv entries/2026-09-07-manual-dependence.tex ../tasks/working_paper_release_audit/manual_review_inventory.md ../tasks/working_paper_release_audit/output/manual_review_increment.csv ../tasks/working_paper_release_audit/output/manual_episode_increment.csv ../tasks/working_paper_release_audit/output/multicard_parent_coverage.csv ../tasks/working_paper_release_audit/output/multicard_followup_permits.csv ../tasks/working_paper_release_audit/output/density_exclusion_comparison.csv entries/2026-09-07-density-decisions.tex ../tasks/new_construction_cleaning/adjudication/density_denominator_decisions.csv logbook.tex entries/2026-09-06-construction-replication.tex \
	../tasks/working_paper_release_audit/restored_stage_checks.csv \
	../tasks/working_paper_release_audit/current_assessor_temporal_candidate_changes.csv \
	../tasks/working_paper_release_audit/output/multicard_land_scope.csv \
	../tasks/working_paper_release_audit/output/denominator_case_review.csv \
	../tasks/working_paper_release_audit/denominator_review.md ../tasks/working_paper_release_audit/output/denominator_permit_evidence.csv \
	../data_raw/construction_review/SHA256SUMS Makefile
	pdflatex -halt-on-error -interaction=nonstopmode logbook.tex
	pdflatex -halt-on-error -interaction=nonstopmode logbook.tex

include ../tasks/shared/code/generic.make
