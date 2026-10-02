# Density Appendix Results

`build_density_appendix_results.R` repeats the main density specification
(`tasks/shared/code/density_boundary_helpers.R`) at placebo cutoffs 1,000 ft
inside either ward, keeping a 500-ft window around each cutoff, and after
excluding buildings within 25 or 50 ft of the boundary. Each figure has the same
three samples as the main figure and reports the first-band and average
differences. Run `make` in `code/`.
