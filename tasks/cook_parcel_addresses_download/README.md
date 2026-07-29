# cook_parcel_addresses_download

## Purpose
Downloads Cook County parcel addresses through a reproducible Socrata API pull.

## Inputs
- Live Cook County Socrata API dataset `3723-97qp`

## Outputs
- `tasks/cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv`

## Run
```bash
cd tasks/cook_parcel_addresses_download/code
make
```

## Approx Runtime
- 10-45 minutes

## Notes
- Uses paginated API download with row-count checks.
- Restricts the parcel-year table to 2025 Chicago rows so it matches the
  canonical 2025 Chicago parcel universe used for coordinates.
