# Presentation figures

Figures and tables used only in the slides (`slides/`). The slides report the
average difference only; the estimates are the paper's.

- `plot_ward_pair.R`: ward pair 1–26 around the 2015 remap — event-study blocks
  within 500 ft of the boundary, colored by 2014 ward, 2015 ward and direction of
  reassignment.
- `plot_ward_map.R`: Chicago's 50 wards on the 2024 ward map.
- `plot_density_slides.R`: slide versions of the paper's density figures (main,
  placebo cutoffs, donuts), estimated with the shared density helper.
- `build_density_tables_slides.R`: slide versions of the boundary-geometry and
  location-continuity tables.
- `plot_price_slides.R`: slide versions of the price figures, drawn from the
  estimates saved by `tasks/price_boundary_results`.

Run `make` in `code/`.
