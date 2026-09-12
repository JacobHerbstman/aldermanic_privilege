"""Compare the draft chronological rule against every saved final project."""
import csv
from collections import Counter
import duckdb
import pandas as pd

with open('../reference/residential_assessments_before_chronology.csv') as source:
    before = list(csv.DictReader(source))
with open('../input/residential_selected_assessments.csv') as source:
    after = list(csv.DictReader(source))
with open('../input/preferred_new_construction_project_ledger.csv') as source:
    ledger = list(csv.DictReader(source))
with open('../input/current_construction_analysis.csv') as source:
    analysis = list(csv.DictReader(source))
for rows in [before, after, ledger, analysis]:
    assert len({r['project_id'] for r in rows}) == len(rows)
before = {r['project_id']: r for r in before}
after = {r['project_id']: r for r in after}
analysis = {r['project_id']: r for r in analysis}
assert before.keys() == after.keys()
for key in before:
    for field in ['dwelling_units', 'building_sqft', 'land_sqft', 'source_row_ids']:
        assert before[key][field] == after[key][field], (key, field)
# Screen all final projects against source-reported parcel-year measurements.
# This catches contradictions; lack of a matching snapshot does not verify a year.
con = duckdb.connect()
con.register("projects", pd.DataFrame(ledger))
con.execute("""
CREATE TEMP TABLE components AS
SELECT project_id, construction_year::INTEGER AS construction_year,
 try_cast(building_sqft AS DOUBLE) AS building_sqft, try_cast(land_sqft AS DOUBLE) AS land_sqft,
 unnest(string_split(component_pins, '/')) AS pin
FROM projects;
CREATE TEMP TABLE measurements AS
SELECT pin, tax_year, sum(building_sqft) AS building_sqft,
 CASE WHEN min(land_sqft)=max(land_sqft) THEN min(land_sqft) END AS land_sqft,
 'residential' AS history_source
FROM read_parquet('../input/residential_assessor_history.parquet')
WHERE building_sqft>0 AND pin IN (SELECT pin FROM components)
GROUP BY pin,tax_year;
INSERT INTO measurements
SELECT regexp_replace(keypin,'[^0-9]','','g'), try_cast(year AS INTEGER),
 coalesce(try_cast(bldgsf AS DOUBLE),try_cast(gross_building_area AS DOUBLE)),
 try_cast(landsf AS DOUBLE), 'commercial'
FROM read_csv('../input/commercial_value_raw.csv',all_varchar=true);
INSERT INTO measurements
SELECT pin,try_cast(year AS INTEGER),try_cast(char_building_sf AS DOUBLE),
 try_cast(char_land_sf AS DOUBLE),'condominium'
FROM read_csv('../input/construction_condominium_history.csv',all_varchar=true);
""")
source_checks = con.execute("""
WITH unique_measurements AS (
 SELECT DISTINCT * FROM measurements WHERE building_sqft>0 AND land_sqft>0
), unambiguous AS (
 SELECT * FROM unique_measurements
 QUALIFY count(*) OVER(PARTITION BY pin,tax_year,history_source)=1
), complete_reports AS (
 SELECT c.project_id,m.tax_year,m.history_source,
 sum(m.building_sqft) AS reported_building_sqft,sum(m.land_sqft) AS reported_land_sqft
 FROM components c JOIN unambiguous m ON c.pin=m.pin
 GROUP BY c.project_id,m.tax_year,m.history_source
 HAVING count(*)=(SELECT count(*) FROM components c2 WHERE c2.project_id=c.project_id)
)
SELECT p.project_id,min(r.tax_year) AS first_matching_source_assessment
FROM projects p JOIN complete_reports r ON p.project_id=r.project_id
 AND try_cast(p.building_sqft AS DOUBLE)=r.reported_building_sqft
 AND try_cast(p.land_sqft AS DOUBLE)=r.reported_land_sqft
GROUP BY p.project_id
""").fetchall()
source_checks = dict(source_checks)
con.close()
rows = []
for project in ledger:
    key = project['project_id']
    old = before.get(key)
    new = after.get(key)
    row = {f: project[f] for f in ['project_id', 'source_family', 'project_kind',
                                  'construction_year', 'year_source']}
    row['within_500ft_before'] = analysis.get(key, {}).get('within_500ft', 'FALSE')
    row['draft_year'] = ''
    row['draft_year_change'] = ''
    row['draft_outside_period'] = ''
    if old is None:
        row['review_status'] = 'not_tested_by_single_card_rule'
    elif project['project_kind'] != 'single_pin_single_card':
        row['review_status'] = 'multiple_component_history_requires_separate_check'
    elif project['year_source'] != old['year_source']:
        row['review_status'] = 'later_recorded_decision_preserved'
    elif new['construction_year'] != old['construction_year']:
        row['draft_year'] = new['construction_year']
        row['draft_year_change'] = int(new['construction_year']) - int(project['construction_year'])
        row['draft_outside_period'] = not 2006 <= int(new['construction_year']) <= 2022
        row['review_status'] = 'draft_correction_requires_population_review'
    else:
        row['review_status'] = 'unchanged_by_single_card_rule_not_independent_verification'
    row['first_matching_source_assessment'] = source_checks.get(key, '')
    row['source_chronology_screen'] = (
        'no_comparable_snapshot_not_verified' if key not in source_checks else
        'matching_measurements_predate_construction_year' if
        source_checks[key] < int(project['construction_year']) else
        'no_direct_date_contradiction_found_not_independent_verification')
    rows.append(row)
with open('../output/population_chronology_review.csv', 'w', newline='') as target:
    writer = csv.DictWriter(target, fieldnames=list(rows[0]))
    writer.writeheader()
    writer.writerows(rows)
print(Counter(r['review_status'] for r in rows))
