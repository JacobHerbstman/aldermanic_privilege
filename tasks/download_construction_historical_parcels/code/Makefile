include ../../shared/code/shell_functions.make

all: ../report/historical_project_parcel_source.gpkg.log ../report/historical_project_parcel_source_queries.csv.log

all: ../report/predecessor_parcel_history.csv.log ../report/predecessor_history_queries.csv.log

all: ../report/historical_predecessor_parcel_source.gpkg.log ../report/historical_predecessor_source_queries.csv.log

all: ../report/preferred_historical_parcel_source.gpkg.log ../report/preferred_historical_parcel_source_queries.csv.log

all: ../report/preferred_predecessor_parcel_source.gpkg.log ../report/preferred_predecessor_source_queries.csv.log

all: ../report/history_reference_parcels.gpkg.log ../report/history_reference_queries.csv.log

../output/history_reference_parcels.gpkg: ../input/history_reference_parcels_complete_2026-09-08.gpkg ../output/history_reference_queries.csv | ../output
	cp $< $@

../output/history_reference_queries.csv: ../input/history_reference_queries_complete_2026-09-08.csv ../input/history_reference_parcels_complete_2026-09-08.gpkg history_reference_snapshot.sha256 | ../output
	shasum -a 256 -c history_reference_snapshot.sha256
	cp $< $@

../report/history_reference_parcels.gpkg.log: ../../shared/code/report.py ../output/history_reference_parcels.gpkg | ../report
	$(PYTHON) $< ../output/history_reference_parcels.gpkg $@ target_year object_id

../report/history_reference_queries.csv.log: ../../shared/code/report.py ../output/history_reference_queries.csv | ../report
	$(PYTHON) $< ../output/history_reference_queries.csv $@ target_year reference_x_3435 reference_y_3435

../output/preferred_predecessor_parcel_source.gpkg: combine_preferred_predecessor_parcels.R ../input/campbell_predecessor_parcels_2026-09-08.gpkg ../input/lake_park_predecessor_parcels_2026-09-08.gpkg ../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_predecessor_parcels_2026-09-07.gpkg ../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg ../output/preferred_predecessor_source_queries.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/preferred_predecessor_source_queries.csv: combine_preferred_predecessor_queries.R ../input/campbell_predecessor_parcels_2026-09-08.gpkg ../input/campbell_predecessor_queries_2026-09-08.csv ../input/lake_park_predecessor_parcels_2026-09-08.gpkg ../input/lake_park_predecessor_queries_2026-09-08.csv ../input/preferred_predecessor_source_queries.csv ../input/preferred_predecessor_queries_2026-09-07.csv ../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_predecessor_parcels_2026-09-07.gpkg ../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg ../input/preferred_predecessor_troy_queries_2026-09-07.csv preferred_predecessor_snapshot.sha256 ../../setup_environment/code/packages.R | ../output
	shasum -a 256 -c preferred_predecessor_snapshot.sha256
	$(R) $<

../report/preferred_predecessor_parcel_source.gpkg.log: ../../shared/code/report.py ../output/preferred_predecessor_parcel_source.gpkg | ../report
	$(PYTHON) $< ../output/preferred_predecessor_parcel_source.gpkg $@ target_year object_id

../report/preferred_predecessor_source_queries.csv.log: ../../shared/code/report.py ../output/preferred_predecessor_source_queries.csv | ../report
	$(PYTHON) $< ../output/preferred_predecessor_source_queries.csv $@ target_year reference_x_3435 reference_y_3435

all: ../report/geocoding_parcel_history.csv.log ../report/geocoding_history_queries.csv.log

../output/geocoding_parcel_history.csv: ../input/geocoding_parcel_history_2026-09-07.csv ../input/geocoding_history_queries_2026-09-07.csv geocoding_history_snapshot.sha256 | ../output
	shasum -a 256 -c geocoding_history_snapshot.sha256
	cp $< $@

../report/geocoding_parcel_history.csv.log: ../../shared/code/report.py ../output/geocoding_parcel_history.csv | ../report
	$(PYTHON) $< ../output/geocoding_parcel_history.csv $@ pin year

../output/geocoding_history_queries.csv: ../input/geocoding_history_queries_2026-09-07.csv ../input/geocoding_parcel_history_2026-09-07.csv geocoding_history_snapshot.sha256 | ../output
	shasum -a 256 -c geocoding_history_snapshot.sha256
	cp $< $@

../report/geocoding_history_queries.csv.log: ../../shared/code/report.py ../output/geocoding_history_queries.csv | ../report
	$(PYTHON) $< ../output/geocoding_history_queries.csv $@ pin

../output/preferred_historical_parcel_source.gpkg: combine_preferred_parcel_sources.R ../input/campbell_parcels_2026-09-08.gpkg ../input/geneva_maud_parcels_2026-09-08.gpkg ../input/east_64th_parcels_2026-09-08.gpkg ../input/lake_park_direct_parcels_2026-09-08.gpkg ../input/preferred_parcel_evidence_2026-09-08.gpkg ../input/preferred_parcel_supplement_2026-09-08.gpkg ../input/preferred_historical_parcel_source.gpkg ../input/preferred_historical_parcel_source_queries.csv ../output/historical_project_parcel_source.gpkg ../input/preferred_parcel_supplement_2026-09-07.gpkg ../output/preferred_historical_parcel_source_queries.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/preferred_historical_parcel_source_queries.csv: combine_preferred_parcel_queries.R ../input/campbell_parcels_2026-09-08.gpkg ../input/campbell_queries_2026-09-08.csv ../input/geneva_maud_parcels_2026-09-08.gpkg ../input/geneva_maud_queries_2026-09-08.csv ../input/east_64th_parcels_2026-09-08.gpkg ../input/east_64th_queries_2026-09-08.csv ../input/lake_park_direct_parcels_2026-09-08.gpkg ../input/lake_park_direct_queries_2026-09-08.csv ../input/preferred_parcel_evidence_2026-09-08.gpkg ../input/preferred_parcel_evidence_queries_2026-09-08.csv ../input/preferred_parcel_supplement_2026-09-08.gpkg ../input/preferred_parcel_supplement_queries_2026-09-08.csv ../input/preferred_historical_parcel_source_queries.csv ../input/preferred_historical_parcel_source.gpkg ../input/preferred_parcel_supplement_queries_2026-09-07.csv ../input/preferred_parcel_supplement_2026-09-07.gpkg ../output/historical_project_parcel_source_queries.csv preferred_snapshot.sha256 ../../setup_environment/code/packages.R | ../output
	shasum -a 256 -c preferred_snapshot.sha256
	$(R) $<

../report/preferred_historical_parcel_source.gpkg.log: ../../shared/code/report.py ../output/preferred_historical_parcel_source.gpkg | ../report
	$(PYTHON) $< ../output/preferred_historical_parcel_source.gpkg $@ target_year object_id

../report/preferred_historical_parcel_source_queries.csv.log: ../../shared/code/report.py ../output/preferred_historical_parcel_source_queries.csv | ../report
	$(PYTHON) $< ../output/preferred_historical_parcel_source_queries.csv $@ target_year pin10

../output/historical_predecessor_parcel_source.gpkg: combine_predecessor_parcels.R ../input/historical_predecessor_parcels_2026-07-27.gpkg ../input/historical_predecessor_parcels_2026-09-07.gpkg ../output/historical_predecessor_source_queries.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/historical_predecessor_source_queries.csv: combine_predecessor_queries.R ../input/historical_predecessor_queries_2026-07-27.csv ../input/historical_predecessor_queries_2026-09-07.csv ../input/historical_predecessor_parcels_2026-07-27.gpkg ../input/historical_predecessor_parcels_2026-09-07.gpkg predecessor_snapshot.sha256 ../../setup_environment/code/packages.R | ../output
	shasum -a 256 -c predecessor_snapshot.sha256
	$(R) $<

../report/historical_predecessor_parcel_source.gpkg.log: ../../shared/code/report.py ../output/historical_predecessor_parcel_source.gpkg | ../report
	$(PYTHON) $< ../output/historical_predecessor_parcel_source.gpkg $@ target_year object_id

../report/historical_predecessor_source_queries.csv.log: ../../shared/code/report.py ../output/historical_predecessor_source_queries.csv | ../report
	$(PYTHON) $< ../output/historical_predecessor_source_queries.csv $@ target_year reference_x_3435 reference_y_3435

../output/predecessor_parcel_history.csv: ../input/predecessor_parcel_history_2026-09-07.csv ../input/predecessor_history_queries_2026-09-07.csv history_snapshot.sha256 | ../output
	shasum -a 256 -c history_snapshot.sha256
	cp $< $@

../output/predecessor_history_queries.csv: ../input/predecessor_history_queries_2026-09-07.csv ../input/predecessor_parcel_history_2026-09-07.csv history_snapshot.sha256 | ../output
	shasum -a 256 -c history_snapshot.sha256
	cp $< $@

../report/predecessor_parcel_history.csv.log: ../../shared/code/report.py ../output/predecessor_parcel_history.csv | ../report
	$(PYTHON) $< ../output/predecessor_parcel_history.csv $@ pin year

../report/predecessor_history_queries.csv.log: ../../shared/code/report.py ../output/predecessor_history_queries.csv | ../report
	$(PYTHON) $< ../output/predecessor_history_queries.csv $@ pin

../output/historical_project_parcel_source.gpkg: combine_parcel_sources.R ../input/historical_project_parcel_source_2026-09-08.gpkg ../input/historical_project_parcel_source_2026-07-27.gpkg ../input/historical_project_parcel_source_2026-09-07.gpkg ../output/historical_project_parcel_source_queries.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/historical_project_parcel_source_queries.csv: combine_parcel_queries.R ../input/historical_project_parcel_queries_2026-09-08.csv ../input/historical_project_parcel_source_2026-09-08.gpkg ../input/historical_project_parcel_queries_2026-07-27.csv ../input/historical_project_parcel_queries_2026-09-07.csv ../input/historical_project_parcel_source_2026-07-27.gpkg ../input/historical_project_parcel_source_2026-09-07.gpkg ../input/historical_project_parcel_layers.csv source_snapshot.sha256 ../../setup_environment/code/packages.R | ../output
	shasum -a 256 -c source_snapshot.sha256
	$(R) $<

../report/historical_project_parcel_source.gpkg.log: ../../shared/code/report.py ../output/historical_project_parcel_source.gpkg | ../report
	$(PYTHON) $< ../output/historical_project_parcel_source.gpkg $@ target_year object_id

../report/historical_project_parcel_source_queries.csv.log: ../../shared/code/report.py ../output/historical_project_parcel_source_queries.csv | ../report
	$(PYTHON) $< ../output/historical_project_parcel_source_queries.csv $@ target_year pin10

../input/lake_park_predecessor_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/lake_park_predecessor_parcels_2026-09-08.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/lake_park_predecessor_queries_2026-09-08.csv: ../../../data_raw/construction_review/lake_park_predecessor_queries_2026-09-08.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/lake_park_direct_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/lake_park_direct_parcels_2026-09-08.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/lake_park_direct_queries_2026-09-08.csv: ../../../data_raw/construction_review/lake_park_direct_queries_2026-09-08.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_parcel_evidence_2026-09-08.gpkg: ../../../data_raw/construction_review/preferred_parcel_evidence_2026-09-08.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_parcel_evidence_queries_2026-09-08.csv: ../../../data_raw/construction_review/preferred_parcel_evidence_queries_2026-09-08.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_parcel_supplement_2026-09-08.gpkg: ../../../data_raw/construction_review/preferred_parcel_supplement_2026-09-08.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_parcel_supplement_queries_2026-09-08.csv: ../../../data_raw/construction_review/preferred_parcel_supplement_queries_2026-09-08.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_project_parcel_source_2026-09-08.gpkg: ../../../data_raw/construction_review/historical_project_parcel_source_2026-09-08.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_project_parcel_queries_2026-09-08.csv: ../../../data_raw/construction_review/historical_project_parcel_queries_2026-09-08.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_project_parcel_source_2026-07-27.gpkg: ../../../data_raw/construction_review/historical_project_parcel_source_2026-07-27.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_project_parcel_source_2026-09-07.gpkg: ../../../data_raw/construction_review/historical_project_parcel_source_2026-09-07.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_project_parcel_queries_2026-07-27.csv: ../../../data_raw/construction_review/historical_project_parcel_queries_2026-07-27.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_project_parcel_queries_2026-09-07.csv: ../../../data_raw/construction_review/historical_project_parcel_queries_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_project_parcel_layers.csv: ../../../data_raw/construction_review/historical_project_parcel_layers_2026-07-27.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/predecessor_parcel_history_2026-09-07.csv: ../../../data_raw/construction_review/predecessor_parcel_history_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/predecessor_history_queries_2026-09-07.csv: ../../../data_raw/construction_review/predecessor_history_queries_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_predecessor_parcels_2026-07-27.gpkg: ../../../data_raw/construction_review/historical_predecessor_parcels_2026-07-27.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_predecessor_parcels_2026-09-07.gpkg: ../../../data_raw/construction_review/historical_predecessor_parcels_2026-09-07.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_predecessor_queries_2026-07-27.csv: ../../../data_raw/construction_review/historical_predecessor_queries_2026-07-27.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_predecessor_queries_2026-09-07.csv: ../../../data_raw/construction_review/historical_predecessor_queries_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_historical_parcel_source.gpkg: ../../../data_raw/construction_review/preferred_historical_parcel_source.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_historical_parcel_source_queries.csv: ../../../data_raw/construction_review/preferred_historical_parcel_source_queries.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_parcel_supplement_2026-09-07.gpkg: ../../../data_raw/construction_review/preferred_parcel_supplement_2026-09-07.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_parcel_supplement_queries_2026-09-07.csv: ../../../data_raw/construction_review/preferred_parcel_supplement_queries_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/preferred_historical_parcel_source.gpkg ../input/preferred_historical_parcel_source_queries.csv ../input/preferred_parcel_supplement_2026-09-07.gpkg ../input/preferred_parcel_supplement_queries_2026-09-07.csv ../input/historical_predecessor_parcels_2026-07-27.gpkg ../input/historical_predecessor_parcels_2026-09-07.gpkg ../input/historical_predecessor_queries_2026-07-27.csv ../input/historical_predecessor_queries_2026-09-07.csv

link-inputs: ../input/predecessor_parcel_history_2026-09-07.csv ../input/predecessor_history_queries_2026-09-07.csv ../input/historical_project_parcel_source_2026-07-27.gpkg ../input/historical_project_parcel_source_2026-09-07.gpkg ../input/historical_project_parcel_queries_2026-07-27.csv ../input/historical_project_parcel_queries_2026-09-07.csv ../input/historical_project_parcel_layers.csv

../input/preferred_predecessor_parcel_source.gpkg: ../../../data_raw/construction_review/preferred_predecessor_parcel_source.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_predecessor_source_queries.csv: ../../../data_raw/construction_review/preferred_predecessor_source_queries.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_predecessor_parcels_2026-09-07.gpkg: ../../../data_raw/construction_review/preferred_predecessor_parcels_2026-09-07.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_predecessor_queries_2026-09-07.csv: ../../../data_raw/construction_review/preferred_predecessor_queries_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_predecessor_source_queries.csv ../input/preferred_predecessor_parcels_2026-09-07.gpkg ../input/preferred_predecessor_queries_2026-09-07.csv

../input/geocoding_parcel_history_2026-09-07.csv: ../../../data_raw/construction_review/geocoding_parcel_history_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/geocoding_history_queries_2026-09-07.csv: ../../../data_raw/construction_review/geocoding_history_queries_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/geocoding_parcel_history_2026-09-07.csv ../input/geocoding_history_queries_2026-09-07.csv

../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg: ../../../data_raw/construction_review/preferred_predecessor_troy_parcels_2026-09-07.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_predecessor_troy_queries_2026-09-07.csv: ../../../data_raw/construction_review/preferred_predecessor_troy_queries_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg ../input/preferred_predecessor_troy_queries_2026-09-07.csv

../input/history_reference_parcels_complete_2026-09-08.gpkg: ../../../data_raw/construction_review/history_reference_parcels_complete_2026-09-08.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/history_reference_queries_complete_2026-09-08.csv: ../../../data_raw/construction_review/history_reference_queries_complete_2026-09-08.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/history_reference_parcels_complete_2026-09-08.gpkg ../input/history_reference_queries_complete_2026-09-08.csv

../input/east_64th_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/east_64th_parcels_2026-09-08.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/east_64th_queries_2026-09-08.csv: ../../../data_raw/construction_review/east_64th_queries_2026-09-08.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/east_64th_parcels_2026-09-08.gpkg ../input/east_64th_queries_2026-09-08.csv

include remaining_review_sources.make

../input/geneva_maud_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/geneva_maud_parcels_2026-09-08.gpkg | ../input
	@if [ "$$(readlink "$@")" != "../../../data_raw/construction_review/geneva_maud_parcels_2026-09-08.gpkg" ]; then ln -sf ../../../data_raw/construction_review/geneva_maud_parcels_2026-09-08.gpkg "$@"; fi

link-inputs: ../input/geneva_maud_parcels_2026-09-08.gpkg

../input/geneva_maud_queries_2026-09-08.csv: ../../../data_raw/construction_review/geneva_maud_queries_2026-09-08.csv | ../input
	@if [ "$$(readlink "$@")" != "../../../data_raw/construction_review/geneva_maud_queries_2026-09-08.csv" ]; then ln -sf ../../../data_raw/construction_review/geneva_maud_queries_2026-09-08.csv "$@"; fi

link-inputs: ../input/geneva_maud_queries_2026-09-08.csv

../input/campbell_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/campbell_parcels_2026-09-08.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/campbell_queries_2026-09-08.csv: ../../../data_raw/construction_review/campbell_queries_2026-09-08.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/campbell_parcels_2026-09-08.gpkg ../input/campbell_queries_2026-09-08.csv

../input/campbell_predecessor_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/campbell_predecessor_parcels_2026-09-08.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/campbell_predecessor_queries_2026-09-08.csv: ../../../data_raw/construction_review/campbell_predecessor_queries_2026-09-08.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/campbell_predecessor_parcels_2026-09-08.gpkg ../input/campbell_predecessor_queries_2026-09-08.csv

include ../../shared/code/generic.make
