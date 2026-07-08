# elms_enrich_matters

## Purpose
Fetches sponsors, histories, and attachments from ELMS matter detail endpoints for seeded matters and produces the final candidate set.

## Inputs
- `tasks/elms_fetch_ordinances/output/matters_<DATE_TAG>.csv`
- `tasks/elms_fetch_ordinances/output/candidate_seed_ids_<DATE_TAG>.csv`

## Outputs
- `tasks/elms_enrich_matters/output/matter_sponsors_<DATE_TAG>.csv`
- `tasks/elms_enrich_matters/output/matter_histories_<DATE_TAG>.csv`
- `tasks/elms_enrich_matters/output/matter_attachments_<DATE_TAG>.csv`
- `tasks/elms_enrich_matters/output/candidate_final_ids_<DATE_TAG>.csv`

## Run
```bash
cd tasks/elms_enrich_matters/code
make link-inputs
make
```

## Approx Runtime
- 20-120 minutes

## Notes
- Requires internet access.
