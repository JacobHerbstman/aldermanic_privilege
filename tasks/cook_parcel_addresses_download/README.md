# cook_parcel_addresses_download

## Purpose
Downloads Cook County parcel-address and parcel-universe CSVs through reproducible Socrata API pulls.

## Inputs
- Live Cook County Socrata API datasets (default: `3723-97qp`, `pabr-t5kh`)

## Outputs
- `tasks/cook_parcel_addresses_download/output/parcel_addresses_3723-97qp.csv`
- `tasks/cook_parcel_addresses_download/output/parcel_universe_pabr-t5kh.csv`

## Run
```bash
cd tasks/cook_parcel_addresses_download/code
make
```

## Approx Runtime
- 10-45 minutes per dataset

## Notes
- Uses paginated API download with row-count checks.
