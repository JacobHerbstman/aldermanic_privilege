"""Check historical Assessor evidence for the 113 entering second-ward records."""
import csv
import duckdb

with open('../output/entering_home_assignments.csv') as source:
    cohort = [r for r in csv.DictReader(source) if r['ward'] == '2']
with open('../input/current_construction_analysis.csv') as source:
    analysis = list(csv.DictReader(source))
assert len({r['project_id'] for r in analysis}) == len(analysis)
analysis = {r['project_id']: r for r in analysis}
connection = duckdb.connect()
history = connection.execute("""
    SELECT * FROM read_parquet('../input/residential_assessor_history.parquet')
    WHERE pin IN (SELECT unnest(?))
""", [[r['component_pins'] for r in cohort]]).fetchdf().to_dict('records')
addresses = connection.execute("""
    SELECT pin, prop_address_full FROM read_csv(
        '../input/parcel_addresses_2025_chicago.csv', all_varchar=true)
    WHERE pin IN (SELECT unnest(?))
""", [[r['component_pins'] for r in cohort]]).fetchall()
assert len({r[0] for r in addresses}) == len(addresses)
addresses = dict(addresses)
rows = []
for member in cohort:
    project = analysis[member['project_id']]
    records = [r for r in history if r['pin'] == member['component_pins']]
    assert records
    same_measurements = [r for r in records
                         if r['building_sqft'] == float(project['building_sqft'])
                         and r['land_sqft'] == float(project['land_sqft'])
                         and r['building_sqft'] > 0]
    earlier_assessments = [r for r in same_measurements
                           if r['tax_year'] < int(project['construction_year'])]
    historical_years = sorted({int(r['year_built']) for r in records})
    source_ids = project['year_source'].split(':', 1)[1].split('/')
    selected = [r for r in records if r['row_id'] in source_ids]
    assert len(selected) == len(source_ids)
    assert all(r['year_built'] == int(project['construction_year']) for r in selected)
    row = {k: project[k] for k in ['project_id', 'component_pins', 'construction_year',
           'dwelling_units', 'building_sqft', 'land_sqft', 'year_source', 'decision_source']}
    row['address'] = addresses.get(member['component_pins'], '')
    row['historical_years'] = '/'.join(map(str, historical_years))
    row['first_assessment'] = min(r['tax_year'] for r in records)
    row['first_earlier_same_measurement_assessment'] = min(
        [r['tax_year'] for r in earlier_assessments], default='')
    row['same_measurement_historical_years'] = '/'.join(map(str, sorted(
        {int(r['year_built']) for r in same_measurements})))
    row['finding'] = ('same_measurements_recorded_before_selected_construction_year'
                      if earlier_assessments else
                      'conflicting_historical_years' if len(historical_years) > 1
                      else 'consistent_reported_year_not_independent_completion_verification')
    rows.append(row)
with open('../output/ward_two_year_review.csv', 'w', newline='') as target:
    writer = csv.DictWriter(target, fieldnames=list(rows[0]))
    writer.writeheader()
    writer.writerows(rows)
