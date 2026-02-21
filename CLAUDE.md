# Aldermanic Privilege Project Guidelines

## Data Analysis workflow
- This project uses a task-based workflow. Every task in the paper has a dedicated folder in `tasks/` with its own `code/`, `input/`, and `output/` subfolders.
- Each task should have its own makefile. Each makefile should be as clean and simple as possible to make them readable. 
- In makefiles, limit comments and additional targets such as ``clean''. Typical makefiles should only have a default ``all'' target and a ``link-inputs'' target.
- Tasks that run many regression specifications, for example, should always call arguments from the makefile and use loops in the makefile to create target filenames for each specification. 
  Then the R script should read in those arguments from the command line and produce the corresponding output file, named according to the arguments in the makefile.
- Tasks that use output from ``upstream`` tasks should use symlinking and makefiles to connect them together. 
  It should be easy to trace the path out via makefiles from the `data_raw/` folder to final outputs. 
  
  ## Project Structure
- `paper/` - LaTeX paper and sections
- `slides/` - Presentation slides
- `data_raw/` - Raw data files (not to be modified)
- `tasks/` - Analysis tasks, each with `code/`, `input/`, and `output/` subfolders
- R scripts in `tasks/*/code/` generate outputs (tables, figures) used by the paper

## Compiling the Paper
- Always use `make` in the `paper/` folder to compile LaTeX
- Use version control (e.g., Git) to track changes in both code and paper, leave clear and simple commit messages
- Always execute tasks by running `make` from the `code` folder within any task and make sure all paths are relative
- Do NOT use `latexmk` directly

## Workflow
- When modifying tables/figures, edit the R script that generates them, not the output files directly
- After running R scripts that update outputs, recompile the paper with `make` in `paper/`

## Modeling Guardrails
- Do NOT use `log1p`, inverse-hyperbolic-sine (arcsinh), or similar "zero-handling hacks" in place of log transforms.
- If a logged outcome has zeros, handle them by dropping zero observations for the logged specification unless explicitly instructed otherwise.
- For the alderman uncertainty index, keep the current control set unless explicitly asked to change it.

## Makefile Conventions
- Explicit symlink rules: `../input/file.ext: ../../upstream_task/output/file.ext | ../input` + `\tln -sf "$<" "$@"`
- Parameterized specs defined in the Makefile and passed to scripts as CLI args.
- For multi-spec runs, use Make loops/templates to generate output targets.
- Avoid bloated helper targets; keep Makefiles focused.

## Make Incrementality Rules
- Do not call recursive upstream builds inside task Makefiles (no `$(MAKE) -C ../../...` in symlink/input rules).
- `link-inputs` should only create symlinks, not orchestrate upstream task execution.
- Each symlink input should depend on the specific upstream output file, not broad gate files.
- Prefer narrow dependency edges. Avoid stamp-file workflows unless there is no clearer file-target alternative.
- Before expensive runs, prefer `make -n` to inspect what will rebuild.

## Makefile Readability Rules
- Keep Makefiles minimal, linear, and easy to scan.
- Keep comments brief and structural only.
- Keep default workflow targets limited to essentials (`all`, `link-inputs`, task-specific essentials only).
- One concise recipe per logical output producer.

## Makefile Path Style
- Write file paths directly in targets and recipes.
- Do NOT use path alias variables like `*_IN`, `*_UP`, `*_OUT`.
- Only scalar/config variables in Makefiles (dates, thresholds, flags, tool executables).
- This keeps filenames short and the pipeline easy to trace.

## RStudio Interactive Block Standard
- Every R script that accepts CLI arguments must include a top-of-file commented interactive block with:
  - A commented `setwd(...)` line to the task `code/` folder
  - One commented valid CLI invocation example mirroring Makefile defaults
- Do NOT include `args <- c(...)` blocks.
- Do NOT include commented path-variable assignments (e.g., `in_csv <- "../input/..."` lines).
- Interactive examples must mirror current Makefile defaults and run end-to-end when uncommented after `cd` into `code/`.

## Script Path Style (R)
- No path alias variables (e.g., no `in_csv <- opt$in_csv` when the variable is used once).
- Prefer direct call-site reads/writes: `read_csv(opt$in_csv)`, `write_csv(df, opt$out_csv)`.
- Keep path handling explicit and local to each read/write call.

## JSON Minimalism Policy
- JSON outputs only for operational gate/audit files that gate or verify the pipeline.
- Default task outputs should be CSV/PDF.
- Optional debug JSON flags must not be part of default `make` targets.

## Terse/Clear Code Preference
- Keep scripts short, direct, and clearly mapped to Makefile arguments and outputs.
- Avoid gratuitous indirection; readers should be able to trace input → transform → output quickly.
- Fewer scripts and fewer task outputs are better where possible.

## Root-Cause First (No Shortcuts)
- Do not bypass missing dependencies with `$(wildcard ...)`, dummy target hacks, or optionalized prerequisite checks.
- Do not use forwarding wrapper scripts (e.g., an R script that only calls `python3 some_script.py`).
- No silent fallback branches. If a secondary source is needed, make it explicit with reason codes and deterministic unresolved/manual-review outputs.
- Fix upstream root data issues rather than suppressing validation downstream.

## Collaboration Preferences
- Keep commit messages clear and minimal.
- When changing a figure/table, edit the generating script and rerun the relevant task.
- Prefer incremental, testable changes to large rewrites.

## Feedback → File Update Process
- When you flag something you don't like, I will identify the underlying pattern, add a concise rule to CLAUDE.md under the relevant section, and update MEMORY.md if it's worth persisting across sessions.
- I will confirm what was written after each update.
