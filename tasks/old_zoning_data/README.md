# old_zoning_data

## Purpose
Builds a simplified pre-2004 zoning lookup from targeted sections of the legacy zoning text plus the 2004 conversion crosswalk.

## Inputs
- `data_raw/chicago_zoning_code1957.txt`
- `data_raw/zoning_conversion_2004_crosswalk.csv`

## Outputs
- `tasks/old_zoning_data/output/old_zoning_bulk_density_1957_2004.csv`

## Run
```bash
cd tasks/old_zoning_data/code
make link-inputs
make
```

## Approx Runtime
- <1 minute

## Notes
- This pass uses targeted extraction (FAR, lot area per unit, and unit restrictions) rather than full-book parsing.
- `C5-5` is intentionally left unresolved (`NA`) in this version.
