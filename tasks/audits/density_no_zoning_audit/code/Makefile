all: ../output/density_no_zoning_report.pdf

../output/density_no_zoning_report.tex: build_density_no_zoning_audit.R ../input/parcels_with_ward_distances.csv | ../output
	$(R) $<

../output/density_no_zoning_estimates.csv \
../output/density_no_zoning_main_table.tex \
../output/density_no_zoning_plot_estimates.tex \
../output/density_no_zoning_main_4panel.pdf \
../output/density_no_zoning_placebo_lenient_4panel.pdf \
../output/density_no_zoning_placebo_stringent_4panel.pdf \
../output/density_no_zoning_donut25ft_4panel.pdf \
../output/density_no_zoning_donut50ft_4panel.pdf: ../output/density_no_zoning_report.tex
	@test -s "$@"
	@touch "$@"

../output/density_no_zoning_report.pdf: \
	../output/density_no_zoning_report.tex \
	../output/density_no_zoning_estimates.csv \
	../output/density_no_zoning_main_table.tex \
	../output/density_no_zoning_plot_estimates.tex \
	../output/density_no_zoning_main_4panel.pdf \
	../output/density_no_zoning_placebo_lenient_4panel.pdf \
	../output/density_no_zoning_placebo_stringent_4panel.pdf \
	../output/density_no_zoning_donut25ft_4panel.pdf \
	../output/density_no_zoning_donut50ft_4panel.pdf
	pdflatex -interaction=nonstopmode -halt-on-error -output-directory=../output $<
	@touch $@

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../../shell_functions.make
include ../../../generic.make
