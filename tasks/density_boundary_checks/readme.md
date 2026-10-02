# Density and location checks at ward boundaries

`build_density_boundary_checks.R` produces two Appendix D tables from the main
density specification (`tasks/shared/code/density_boundary_helpers.R`):

- `density_boundary_robustness.tex` restricts the sample by the geometry of the
  assigned boundary segment, using the classifications from
  `density_boundary_characteristics`.
- `density_location_continuity.tex` uses distances to downtown, the nearest
  school, the nearest park and Lake Michigan as outcomes, with distance bands and
  boundary-segment-by-joint-service fixed effects only.

Run `make` in `code/`.
