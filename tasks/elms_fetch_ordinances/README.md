# elms_fetch_ordinances

## Purpose
Fetches ordinance matters from the Chicago ELMS API and builds the initial candidate seed table.

## Inputs
- Live ELMS API (`https://api.chicityclerkelms.chicago.gov/matter`)

## Outputs
- `tasks/elms_fetch_ordinances/output/matters_<DATE_TAG>.csv`
- `tasks/elms_fetch_ordinances/output/candidate_seed_ids_<DATE_TAG>.csv`

## Run
```bash
cd tasks/elms_fetch_ordinances/code
make
```

## Approx Runtime
- 5-60 minutes (depends on date range and API speed)

## Notes
- Requires internet access.
- The default source window is frozen at November 1, 2010 through February 12, 2026 so later substitutes can be linked to ordinances introduced by 2020.
- Uses `recordNumber` as canonical `matter_id` and includes `matter_guid`, `source_system`, and `date_imputed_from_final_action`.
