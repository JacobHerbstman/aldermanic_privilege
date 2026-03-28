SHELL := bash
include ../../shell_functions.make

all: \
	../output/task_dependency_edges.csv \
	../output/active_tex_task_usage.csv \
	../output/output_keep_manifest.csv \
	../output/stale_output_manifest.csv \
	../output/task_cleanup_summary.csv \
	../output/cleanup_manifest.md \
	../output/task_style_conformance_before.csv \
	../output/task_style_conformance_after.csv \
	../output/task_style_diff.md \
	../output/paper_pipeline_cleanup_queue.csv \
	../output/paper_pipeline_cleanup_queue.md \
	../output/legacy_task_map.csv \
	../output/legacy_task_map.md \
	../output/task_linearization_priority.csv \
	../output/task_linearization_priority.md

../output/task_dependency_edges.csv \
../output/active_tex_task_usage.csv \
../output/output_keep_manifest.csv \
../output/stale_output_manifest.csv \
../output/task_cleanup_summary.csv \
../output/cleanup_manifest.md: build_cleanup_manifest.R | ../output
	Rscript build_cleanup_manifest.R

../output/task_style_conformance_before.csv: build_task_style_conformance.R | ../output
	Rscript build_task_style_conformance.R before "$@"

../output/task_style_conformance_after.csv: build_task_style_conformance.R | ../output
	Rscript build_task_style_conformance.R after "$@"

../output/task_style_diff.md: build_task_style_diff.R ../output/task_style_conformance_before.csv ../output/task_style_conformance_after.csv | ../output
	Rscript build_task_style_diff.R ../output/task_style_conformance_before.csv ../output/task_style_conformance_after.csv "$@"

../output/paper_pipeline_cleanup_queue.csv ../output/paper_pipeline_cleanup_queue.md: build_paper_cleanup_queue.R ../output/task_dependency_edges.csv ../output/active_tex_task_usage.csv ../output/task_style_conformance_after.csv | ../output
	Rscript build_paper_cleanup_queue.R ../output/task_dependency_edges.csv ../output/active_tex_task_usage.csv ../output/task_style_conformance_after.csv ../output/paper_pipeline_cleanup_queue.csv ../output/paper_pipeline_cleanup_queue.md

../output/legacy_task_map.csv ../output/legacy_task_map.md: build_legacy_task_map.R | ../output
	Rscript build_legacy_task_map.R ../output/legacy_task_map.csv ../output/legacy_task_map.md

../output/task_linearization_priority.csv ../output/task_linearization_priority.md: build_task_linearization_priority.R ../output/active_tex_task_usage.csv ../output/task_style_conformance_after.csv | ../output
	Rscript build_task_linearization_priority.R ../output/active_tex_task_usage.csv ../output/task_style_conformance_after.csv ../output/task_linearization_priority.csv ../output/task_linearization_priority.md

../output/pruned_outputs.csv: prune_stale_outputs.R ../output/output_keep_manifest.csv | ../output
	Rscript prune_stale_outputs.R "$@"

../output/pre_cleanup_snapshot.csv: capture_task_snapshot.R | ../output
	Rscript capture_task_snapshot.R "$@"

../output/post_cleanup_snapshot.csv: capture_task_snapshot.R | ../output
	Rscript capture_task_snapshot.R "$@"

../output/cleanup_delta.md: build_cleanup_delta.R ../output/pre_cleanup_snapshot.csv ../output/post_cleanup_snapshot.csv ../output/task_cleanup_summary.csv | ../output
	Rscript build_cleanup_delta.R ../output/pre_cleanup_snapshot.csv ../output/post_cleanup_snapshot.csv ../output/task_cleanup_summary.csv "$@"

include ../../generic.make

link-inputs:
