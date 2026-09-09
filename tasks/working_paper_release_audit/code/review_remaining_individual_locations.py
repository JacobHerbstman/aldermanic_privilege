import duckdb
import pandas as pd

cases = pd.read_csv('../reference/individual_location_remaining_cases.csv', dtype=str)
projects = pd.read_csv('../input/preferred_residential_project_candidates.csv', dtype={'component_pins': str})
review = cases.merge(projects[['project_id', 'candidate_status', 'construction_year', 'dwelling_units', 'building_sqft', 'land_sqft']],
    on='project_id', validate='one_to_one')
years = pd.read_csv('../input/residential_reviewed_construction_years.csv')
review = review.merge(years[['project_id', 'reported_year']], on='project_id', how='left', validate='one_to_one')
review['matching_year'] = review.reported_year.fillna(review.construction_year)
con = duckdb.connect()
con.register('review_pins', review[['pin']])
history = con.execute("""SELECT h.pin, h.tax_year, h.card_num, h.class, h.year_built,
    h.building_sqft, h.land_sqft, h.num_apartments, h.row_id
    FROM read_parquet('../input/residential_assessor_history.parquet') h
    JOIN review_pins p ON h.pin=p.pin WHERE h.building_sqft>1""").df()
assert not history.duplicated(['pin', 'tax_year', 'card_num']).any()
history['occupied_cards'] = history.groupby(['pin', 'tax_year']).pin.transform('size')
history['units'] = history.num_apartments
history.loc[history['class'].isin('202 203 204 205 206 207 208 209 210 219 234 278 295'.split()), 'units'] = 1
addresses = pd.read_csv('../input/parcel_addresses_2025_chicago.csv',
    usecols=['pin', 'prop_address_full'], dtype=str)
assert not addresses.pin.duplicated().any()
review = review.merge(addresses, on='pin', how='left', validate='one_to_one')
historical_addresses = pd.read_csv('../input/density_historical_address_records.csv', dtype={'pin': str})
historical_addresses = historical_addresses.sort_values(['pin', 'year']).groupby('pin').tail(1)
review = review.merge(historical_addresses[['pin', 'property_address']], on='pin', how='left', validate='one_to_one')
review['prop_address_full'] = review.prop_address_full.fillna(review.property_address)
review = review.drop(columns='property_address')
points = pd.read_csv('../input/parcel_universe_2025_city.csv',
    usecols=['pin', 'tax_year', 'longitude', 'latitude'], dtype={'pin': str}).rename(
    columns={'tax_year': 'year', 'longitude': 'lon', 'latitude': 'lat'})
for filename in ['predecessor_parcel_history', 'geocoding_parcel_history', 'density_historical_parcel_records']:
    old = pd.read_csv('../input/' + filename + '.csv', dtype={'pin': str}).rename(
        columns={'longitude': 'lon', 'latitude': 'lat'})
    points = pd.concat([points, old[['pin', 'year', 'lon', 'lat']]])
points = points.dropna(subset=['lon', 'lat']).drop_duplicates()
records = []
for row in review.itertuples(index=False):
    h = history[history.pin == row.pin].sort_values(['tax_year', 'card_num'])
    exact = h[(h.occupied_cards == 1) & (h.year_built == row.matching_year) &
        (h.building_sqft == row.building_sqft) & (h.land_sqft == row.land_sqft) & (h.units == row.dwelling_units)]
    p = points[points.pin == row.pin]
    matched = p[p.year.isin(exact.tax_year) & (p.year >= row.construction_year)]
    latest = h.iloc[-1]
    records.append(dict(project_id=row.project_id, last_assessment_year=int(latest.tax_year),
        latest_reported_construction_year=int(latest.year_built), latest_floor_sqft=latest.building_sqft,
        latest_land_sqft=latest.land_sqft, latest_units=latest.units,
        matching_assessment_years='/'.join(map(str, sorted(exact.tax_year.unique()))),
        available_point_years='/'.join(str(int(v)) for v in sorted(p.year.unique())),
        matching_point_years='/'.join(str(int(v)) for v in sorted(matched.year.unique())),
        floor_change_sqft=latest.building_sqft-row.building_sqft,
        year_changed=latest.year_built != row.matching_year))
review = review.merge(pd.DataFrame(records), on='project_id', validate='one_to_one')
scope = pd.read_csv('../input/preferred_project_boundary_scope.csv',
    usecols=['project_id','location_source','distance_to_boundary_ft'])
review = review.merge(scope, on='project_id', validate='one_to_one')
review['location_resolved'] = review.distance_to_boundary_ft.notna() & review.location_source.notna() & review.location_source.ne('former_parcel_centroid_unresolved_individual')
review['finding'] = 'No saved point from an assessment year with matching building measurements'
review.loc[review.matching_point_years.ne(''), 'finding'] = 'Matching assessment and point exist; point does not fit the historical site'
review.loc[review.location_resolved, 'finding'] = 'Individual property has an accepted location in production'
review.loc[review.location_source.eq('same_property_later_exact_parcel_point'), 'finding'] = 'Individual lot located using matching assessment and parcel point'
review.loc[review.location_source.eq('verified_chicago_individual_address_point'), 'finding'] = 'Individual building located by the already verified exact Chicago address match'
review['building_check'] = 'No measurement or construction-year discrepancy in the latest available assessment'
review.loc[review.last_assessment_year.lt(2025), 'building_check'] = 'Property number no longer has a residential assessment in 2025; older record remains available'
review.loc[review.floor_change_sqft.ne(0), 'building_check'] = 'Later assessment changes floor area; preserve selected source measurement pending interpretation'
review.loc[review.year_changed, 'building_check'] = 'Later assessment changes construction year; location evidence does not resolve completion year'
review.loc[review.reported_year.notna(), 'building_check'] = 'Construction year already corrected in the committed decision ledger; preserve approved year'
review['excluded'] = review.candidate_status.str.startswith('exclude')
review.loc[review.excluded, 'finding'] = 'Excluded from construction analysis by the recorded decision; no location repair needed'
assert len(review) == 69 and not review.project_id.duplicated().any()
review.sort_values('project_id').to_csv('../output/remaining_individual_location_findings.csv', index=False)
