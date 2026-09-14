"""Describe the entering non-external-multifamily cohort; do not change analysis data."""
import csv

with open('../output/construction_estimation_project_changes.csv') as source:
    changes = list(csv.DictReader(source))
with open('../input/current_construction_analysis.csv') as source:
    analysis = list(csv.DictReader(source))
with open('../input/terms.csv') as source:
    terms = list(csv.DictReader(source))
with open('../input/scores.csv') as source:
    scores = list(csv.DictReader(source))
assert len({r['project_id'] for r in changes}) == len(changes)
assert len({r['project_id'] for r in analysis}) == len(analysis)
assert len({r['alderman'] for r in scores}) == len(scores)
analysis = {r['project_id']: r for r in analysis}
scores = {r['alderman']: float(r['uncertainty_index']) for r in scores}
cohorts = {}
for outcome in ['far', 'dupac']:
    cohorts[outcome] = {r['project_id'] for r in changes
                        if r[outcome + '_sample_after'] == 'TRUE'
                        and r[outcome + '_sample_before'] != 'TRUE'
                        and r['external_multifamily_after'] == 'FALSE'}
assert cohorts['far'] == cohorts['dupac'], 'Outcome-specific entrants differ'
rows = []
for change in changes:
    if change['project_id'] not in cohorts['far']:
        continue
    project = analysis[change['project_id']]
    for field in ['construction_year', 'ward_pair', 'alderman_own', 'alderman_neighbor']:
        assert project[field] == change[field + '_after'], (project['project_id'], field)
    assert float(project['distance_to_boundary_ft']) <= 500
    for ward_field, alderman_field in [('ward', 'alderman_own'), ('neighbor_ward', 'alderman_neighbor')]:
        serving = [r['alderman'] for r in terms if r['ward'] == project[ward_field]
                   and r['start_date'] <= project['construction_date'] <= r['end_date']]
        assert serving == [project[alderman_field]], (project['project_id'], serving)
    row = {field: project[field] for field in [
        'project_id', 'source_project_ids', 'component_pins', 'project_kind',
        'construction_year', 'construction_date', 'boundary_year', 'ward', 'alderman_own',
        'neighbor_ward', 'alderman_neighbor', 'ward_pair', 'segment_id',
        'distance_to_boundary_ft', 'dwelling_units', 'building_sqft', 'land_sqft',
        'density_far', 'density_dupac', 'external_multifamily', 'multifamily_source']}
    row['own_score'] = scores[project['alderman_own']]
    row['neighbor_score'] = scores[project['alderman_neighbor']]
    assert row['own_score'] != row['neighbor_score']
    row['boundary_side'] = 'higher_score' if row['own_score'] > row['neighbor_score'] else 'lower_score'
    row['present_in_old_data'] = change['present_before']
    rows.append(row)
rows.sort(key=lambda r: (int(r['ward']), r['alderman_own'], r['project_id']))
assert len(rows) == 368, 'The historical decomposition cohort changed; review before interpreting'
with open('../output/entering_home_assignments.csv', 'w', newline='') as target:
    writer = csv.DictWriter(target, fieldnames=list(rows[0]))
    writer.writeheader()
    writer.writerows(rows)
print('Verified 368 entrants, identical across outcomes; all serving-alderman assignments match recorded terms.')
