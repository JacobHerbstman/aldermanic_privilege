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
