# rental_rd_supply

Purpose: Checks whether the RentHub rental RD rent gap is accompanied by differences in advertised rental supply on the stricter side of ward boundaries.

The task treats supply as online RentHub floorplan-month availability, not a count of true vacant housing units.

The default build produces:
- `output/rental_rd_supply_levels_bw500.pdf`
- `output/rental_rd_supply_levels_clean_location_bw500.pdf`

Extended supply diagnostics and pruned-segment robustness plots live in `tasks/audits/rental_rd_supply_audit`.
