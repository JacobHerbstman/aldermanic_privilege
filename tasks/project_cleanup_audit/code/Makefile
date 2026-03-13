SHELL := bash
include ../../shell_functions.make

all: \
	../output/task_dependency_edges.csv \
	../output/active_tex_task_usage.csv \
	../output/stale_output_manifest.csv \
	../output/cleanup_manifest.md \
	../output/task_style_conformance_before.csv \
	../output/task_style_conformance_after.csv \
	../output/task_style_diff.md

../output/task_dependency_edges.csv \
../output/active_tex_task_usage.csv \
../output/stale_output_manifest.csv \
../output/cleanup_manifest.md: build_cleanup_manifest.R | ../output
	Rscript build_cleanup_manifest.R

../output/task_style_conformance_before.csv: build_task_style_conformance.R | ../output
	Rscript build_task_style_conformance.R before "$@"

../output/task_style_conformance_after.csv: build_task_style_conformance.R | ../output
	Rscript build_task_style_conformance.R after "$@"

../output/task_style_diff.md: build_task_style_diff.R ../output/task_style_conformance_before.csv ../output/task_style_conformance_after.csv | ../output
	Rscript build_task_style_diff.R ../output/task_style_conformance_before.csv ../output/task_style_conformance_after.csv "$@"

include ../../generic.make

link-inputs:
