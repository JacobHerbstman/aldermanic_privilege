all: ../output/density_boundary_characteristics.csv
	@printf '%s  %s\n' '6314345aa121c925d3ee24e7aad47a8e431f58ed09ebb59891bcf1a8192eb1d9' "$<" | shasum -a 256 -c -

../output/density_boundary_characteristics.csv:
	@test -s "$@" || { \
		echo "Missing frozen density boundary characteristics."; \
		false; \
	}
