# create_alderman_uncertainty_index

Purpose: Creates alderman uncertainty scores used by the paper and downstream score merges.

The first-stage permit regression includes median household income but omits bachelor's-degree share. It also includes the demographic, geographic, workload, permit-type, review-type, and month controls described in the paper.

Produces:
- the paper score CSV and stage-1 table for the default 2022 cutoff
- the frozen 2014 score CSV used by the permit event study

Approx. runtime: ~1-10 minutes.
