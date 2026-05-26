# rental_rd

Purpose: estimates the paper-facing 2014-2022 listed-rent RD at ward borders.

The default build produces:
- `output/rental_rd_flat_bw500_2014_2022_all_controls.pdf`

The control-rich plot uses `rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet`, which adds the audited hedonic and amenity-distance controls. Rental RD quality diagnostics live in `tasks/audits/rental_rd_quality_audit`.
