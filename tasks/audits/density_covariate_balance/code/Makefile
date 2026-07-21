all: ../output/density_covariate_balance.csv ../output/density_covariate_balance.tex ../output/density_covariate_joint_tests.csv ../output/density_balance_sur_tests.csv ../output/density_balance_sur_coefficients.csv ../output/density_covariate_balance_report.pdf

../output/density_covariate_balance.csv: build_density_covariate_balance.R ../input/parcels_with_ward_distances.csv | ../output
	$(R) $<

../output/density_covariate_balance.tex \
../output/density_covariate_joint_tests.csv: ../output/density_covariate_balance.csv
	@test -s "$@"

../output/density_balance_sur_tests.csv: run_density_balance_sur.R ../output/density_covariate_balance.csv ../input/parcels_with_ward_distances.csv ../input/parcels_with_geometry.gpkg | ../output
	$(R) $<

../output/density_balance_sur_coefficients.csv: ../output/density_balance_sur_tests.csv
	@test -s "$@"

../output/density_covariate_balance_report.tex: build_density_covariate_balance_report.R ../output/density_covariate_balance.csv ../output/density_covariate_joint_tests.csv ../output/density_balance_sur_tests.csv | ../output
	$(R) $<

../output/density_covariate_balance_report.pdf: ../output/density_covariate_balance_report.tex
	pdflatex -interaction=nonstopmode -halt-on-error -output-directory=../output $<
	@touch $@

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_geometry.gpkg: ../../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/parcels_with_geometry.gpkg

include ../../../shell_functions.make
include ../../../generic.make
