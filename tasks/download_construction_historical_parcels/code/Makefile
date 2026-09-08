include ../../shared/code/shell_functions.make

all: ../report/historical_project_parcel_source.gpkg.log ../report/historical_project_parcel_source_queries.csv.log

../output/historical_project_parcel_source.gpkg: combine_parcel_sources.R ../input/historical_project_parcel_source_2026-07-27.gpkg ../input/historical_project_parcel_source_2026-09-07.gpkg ../output/historical_project_parcel_source_queries.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/historical_project_parcel_source_queries.csv: combine_parcel_queries.R ../input/historical_project_parcel_queries_2026-07-27.csv ../input/historical_project_parcel_queries_2026-09-07.csv ../input/historical_project_parcel_source_2026-07-27.gpkg ../input/historical_project_parcel_source_2026-09-07.gpkg ../input/historical_project_parcel_layers.csv source_snapshot.sha256 ../../setup_environment/code/packages.R | ../output
	shasum -a 256 -c source_snapshot.sha256
	$(R) $<

../report/historical_project_parcel_source.gpkg.log: ../../shared/code/report.py ../output/historical_project_parcel_source.gpkg | ../report
	$(PYTHON) $< ../output/historical_project_parcel_source.gpkg $@ target_year object_id

../report/historical_project_parcel_source_queries.csv.log: ../../shared/code/report.py ../output/historical_project_parcel_source_queries.csv | ../report
	$(PYTHON) $< ../output/historical_project_parcel_source_queries.csv $@ target_year pin10

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

link-inputs: ../input/historical_project_parcel_source_2026-07-27.gpkg ../input/historical_project_parcel_source_2026-09-07.gpkg ../input/historical_project_parcel_queries_2026-07-27.csv ../input/historical_project_parcel_queries_2026-09-07.csv ../input/historical_project_parcel_layers.csv

include ../../shared/code/generic.make
