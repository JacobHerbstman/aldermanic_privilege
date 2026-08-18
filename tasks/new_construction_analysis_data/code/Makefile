all: ../output/new_construction_analysis_data.csv ../output/new_construction_project_locations.csv
	@printf '%s  %s\n' '9dc7953e91bdf21a909224d2d68697a8440b56b66f137c7d784bea6137bf8ea4' "../output/new_construction_analysis_data.csv" | shasum -a 256 -c -
	@printf '%s  %s\n' '43a98de01a99111f195881aa8726fecc248c75762af976377d270cc62530a385' "../output/new_construction_project_locations.csv" | shasum -a 256 -c -

../output/new_construction_analysis_data.csv:
	@test -s "$@" || { \
		echo "Missing frozen new-construction analysis data."; \
		false; \
	}

../output/new_construction_project_locations.csv:
	@test -s "$@" || { \
		echo "Missing frozen new-construction project locations."; \
		false; \
	}
