# Checks of the new buildings from the Assessor's records

Checks of `tasks/build_assessor_new_buildings/output/new_buildings.csv`. A
building's site is its parcels, the old parcels under them and the parcels
replacing them (`code/building_sites.R`).

- `check_recall.R` → `recall_against_permits.csv`: of the permit-linked buildings
  of `tasks/prepare_permit_construction` with permits issued 2006–2018 (residential
  cards, condominiums and commercial valuations), the share with a new building on
  the site from one year before to five years after the permit, and the share
  whose building also has the matching type.
- `check_precision.R` → `precision_against_permits.csv`: for samples of new
  buildings of 2012–2022 by type and route, and of parcels of each type with no new
  building, the share with a new-building permit within 100 ft of the site in the
  six years before.
- `check_before_2006.R` → `checks_before_2006.csv`: buildings built 1999–2020 in two
  records not drawn from the assessed values (the Assessor's 2021+ valuations of
  apartment buildings with 7+ units still standing, and City Energy Benchmarking
  multifamily buildings of 50,000+ sq ft) found among the new buildings near their
  reported year built, before and after 2006. Before 2006 there are no permits.
- `compare_census_permits.R` → `census_comparison.csv`: new buildings by year and
  type against the Census Bureau's Chicago permits two years before (apartment class
  counted by development).

`adjudication/` holds two hand checks, one row per sampled building with a verdict
and the reason, read from each building's Assessor history, valuations, parcel
maps, benchmarking and nearby permits:

- `hand_check_first_rules.csv`: 103 events, 5 per type, route and period (before
  2006 and from 2006), drawn from the first version of the rules. It found buildings
  counted twice (condominium buildings first assessed in an apartment class, one
  building on several PINs, a building's later class change), dates off by a year to
  seventeen (two from a minor-improvement value counted as a building), existing
  buildings whose valuation sits on a replacing parcel, and two routes without signal.
  The rules were changed for each, for all cases.
- `hand_check_revised_rules.csv`: 43 buildings and developments from the revised
  rules, weighted to what they changed (apartment developments on several parcels,
  houses dated from their old parcel). It found three more general problems, also
  fixed in the rules: a replacing parcel's valuation describing a neighbouring
  building (Wolf Point), apartment typing reaching too far ahead (841 W Agatite), and
  exempt land taxable for a year or two without a valuation.

Run `make` in `code/`.
