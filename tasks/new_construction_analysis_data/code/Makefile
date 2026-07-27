all: ../output/new_construction_analysis_data.csv

../output/new_construction_analysis_data.csv:
	@test -s "$@" || { \
		echo "Missing frozen new-construction analysis data."; \
		false; \
	}
