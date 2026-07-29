all: ../output/new_construction_analysis_data.csv
	@printf '%s  %s\n' '9dc7953e91bdf21a909224d2d68697a8440b56b66f137c7d784bea6137bf8ea4' "$<" | shasum -a 256 -c -

../output/new_construction_analysis_data.csv:
	@test -s "$@" || { \
		echo "Missing frozen new-construction analysis data."; \
		false; \
	}
