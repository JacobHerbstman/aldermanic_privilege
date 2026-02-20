# alderman_data Project Guidelines

## Core Workflow
- Use a task-based pipeline in `tasks/`, where each task has `code/`, `input/`, and `output/`.
- Keep task Makefiles simple and readable.
- Default pattern: `all` target + `link-inputs` target + explicit symlink rules.
- Execute tasks by running `make` inside each task's `code/` folder.
- Keep all paths relative; do not hardcode machine-specific absolute paths.

## File and Pipeline Structure
- `data_raw/`: immutable raw files (never edit in place).
- `tasks/`: all analysis tasks.
- `paper/`: manuscript build.
- `slides/` (optional): presentation build.
- Upstream/downstream dependencies should flow through symlinked inputs so lineage from `data_raw/` to final outputs is traceable.

## Makefile Conventions
- Prefer explicit input symlink rules:
  - `../input/file.ext: ../../upstream_task/output/file.ext | ../input`
  - `\tln -sf "$<" "$@"`
- Parameterized specs should be defined in the Makefile and passed to scripts as CLI args.
- For multi-spec runs, use Make loops/templates to generate output targets.
- Avoid bloated helper targets; keep Makefiles focused.

## Make Incrementality Rules
- Do not call recursive upstream builds inside active task Makefiles (for example, no `$(MAKE) -C ../../...` inside symlink/input rules).
- `link-inputs` should only create symlinks and should not orchestrate upstream task execution.
- Each symlink input should depend on the specific upstream output file path, not on broad/coarse “gate” files when avoidable.
- Prefer narrow dependency edges over single-report anchors that can trigger unnecessary relinking and downstream invalidation.
- Stamp-file workflows can obscure real dependency edges; use them sparingly and only when there is no clearer file-target alternative.
- Before expensive runs, prefer `make -n` to inspect what will rebuild.

## Makefile Readability Rules
- Keep Makefiles minimal, linear, and easy to scan.
- Keep comments brief and structural only.
- Keep default workflow targets limited to essentials (`all`, `link-inputs`, and task-specific essentials only).
- Keep one concise recipe per logical output producer.
- Keep output and input names explicit and traceable.
- Favor readability over clever Make metaprogramming unless scale requires it.

## Makefile Path Style
- Write file paths directly in targets and recipes.
- Do not use path indirection blocks like `*_IN`, `*_UP`, `*_OUT`, or similar path alias variables.
- Keep only scalar/config variables in Makefiles (dates, thresholds, flags, tool executables).
- This style is intentional: it encourages shorter filenames and fewer upstream/downstream artifacts per task.

## Execution Conventions
- Run analysis through scripts and Make, not by manually editing outputs.
- After regenerating tables/figures, recompile paper/slides with `make`.
- Prefer wrapper functions (`R_pc_and_slurm`, `stata_with_flag`) when SLURM support is needed.

## Environment Setup Conventions
- Keep `tasks/setup_environment/code/` as the bootstrap entry point.
- Maintain a package bootstrap script (R/Stata as needed) that logs installed package versions.
- Keep a reusable `run.sbatch` for cluster execution and symlink it into tasks that need it.

## Coding Guardrails
- Preserve reproducibility and deterministic outputs where possible.
- Scripts should accept CLI arguments for key specs and output paths.
- Name outputs to encode key spec choices (bandwidth, FE spec, sample/suffix).
- Never modify raw data; write cleaned/intermediate artifacts to `tasks/*/output/`.

## RStudio Interactive Block Standard
- For every active R script that accepts CLI arguments, include a top-of-file commented interactive block.
- That block must include:
  - a commented `setwd(...)` line to the task `code/` folder
  - one commented valid CLI invocation example line mirroring Makefile arguments
- Do not include bundled commented argument vectors/lists (for example, no `args <- c(...)` block).
- Do not include commented path-variable assignments (for example, no `in_csv <- "../input/..."` lines).
- Interactive examples must mirror current Makefile defaults/paths and run end-to-end when uncommented after `cd` into the task `code/` directory.
- CLI parsing remains canonical for non-interactive runs; interactive blocks are for readability/debugging only.

## Script Example Style
- Use direct per-argument commented examples only.
- Keep example paths explicit and minimal.
- Prefer fewer scripts and fewer task outputs where possible to keep the research flow clear.

## Script Path Style (R + Python)
- Avoid path alias variables for simple I/O handoff (for example, no `in_csv = args.in_csv` / `out_csv <- opt$out_csv` aliases when direct use is clear).
- Prefer direct call-site reads/writes (`read_csv(args.in_csv)`, `write_csv(df, args.out_csv)`, and R equivalents).
- Keep path handling explicit and local to each read/write call unless reuse materially improves clarity.

## JSON Minimalism Policy
- JSON outputs are only for operational checks that gate or audit the pipeline.
- Keep-set (operational JSON):
  - `tasks/rezoning_far_pre_geocode/output/rezoning_far_pre_geocode_summary_<DATE_TAG>.json`
  - `tasks/rezoning_geocode_stage1_parcel/output/far_gate_ok_<DATE_TAG>.json`
  - `tasks/rezoning_geocode_external_merge/input/chicago_geocoder_results_<DATE_TAG>_import_meta.json`
  - `tasks/symlink_graph/output/unused_tasks_audit_<DATE_TAG>.json`
- Default task outputs should otherwise be CSV/PDF (or XLSX where the external tool requires it).
- Optional debug/report JSON flags may exist in scripts, but they should not be part of default `make` targets.

## Terse/Clear Code Preference
- Prefer simpler argument parsing and fewer helper abstractions when they do not improve clarity.
- Keep scripts short, direct, and clearly mapped to Makefile arguments and outputs.
- Avoid gratuitous indirection; future readers should be able to trace input -> transform -> output quickly.

## Modeling Guardrails (Carryover)
- Do not use `log1p`, arcsinh, or similar substitutes for logs unless explicitly requested.
- For logged outcomes with zeros, drop zero observations for that logged specification unless instructed otherwise.

## Collaboration Preferences
- Keep commit messages clear and minimal.
- When changing a figure/table, edit the generating script and rerun the relevant task.
- Prefer incremental, testable changes to large rewrites.
