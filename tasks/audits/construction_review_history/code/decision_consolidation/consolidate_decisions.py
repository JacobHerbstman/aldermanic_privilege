"""Translate committed decisions; never copy the automatically cleaned population."""
import csv
from pathlib import Path
from collections import defaultdict

root = Path('/Users/jacobherbstman/Desktop/aldermanic_privilege')
source = root / 'tasks/new_construction_cleaning/output'
out = Path('/tmp/construction-rewrite-20260912')
changes = {}
conflicts = []
selected_assessments = {r["project_id"]:r for r in csv.DictReader(open(
    out/"before/construction_residential_measurements/output/residential_selected_assessments.csv"))}
inactive = []

def read(name):
    with (source / name).open() as f:
        return [{k: (None if v in ('', 'NA') else v) for k, v in row.items()} for row in csv.DictReader(f)]

def add(project_id, action, sources, values, file, row, reason=None):
    assert project_id
    current = changes.setdefault(project_id, {'project_id': project_id, 'action': action,
        'source_project_ids': sources or project_id, 'decision_references': [], 'decision_reason': []})
    if current['action'] != action:
        conflicts.append((project_id, 'action', current['action'], action, file))
    current['action'] = action
    if sources:
        current['source_project_ids'] = '/'.join(sorted(set((current['source_project_ids']+'/'+sources).split('/'))))
    for k, v in values.items():
        if v is None: continue
        if k == 'evidence_ids' and k in current:
            current[k] = ' | '.join(sorted(set(current[k].split(' | ') + v.split(' | '))))
            continue
        if k in current and current[k] != v:
            conflicts.append((project_id, k, current[k], v, file))
        current[k] = v
    current['decision_references'].append(f'{file}:row{row}')
    if reason and reason not in current['decision_reason']:current['decision_reason'].append(reason)

# Field corrections retain every uncorrected Assessor measurement.
file = 'construction_modifications.csv'
for n, r in enumerate(read(file), 2):
    family = r['source_family']; scope = r['application_scope']
    old = r['source_project_id']; new = r['final_project_id'] or old
    vals = {k:r[k] for k in ['construction_year','dwelling_units','building_sqft','land_sqft','allow_far','allow_dupac']}
    vals['source_family'] = family
    vals['evidence_ids'] = r.get('evidence_ids')
    if scope not in ('construction_year','land_area','selected_fields'):
        for field, flag in [('dwelling_units','units_unusable'),('building_sqft','building_area_unusable')]:
            if r[field] is None: vals[flag]='TRUE'
        if r.get('land_source') in ('construction_year_parcel_union','construction_year_parcel_polygon','construction_year_union_of_2021_components'):
            vals['land_selection']='reported_same_property'; vals['land_sqft']=None
        elif r['land_sqft'] is None: vals['land_area_unusable']='TRUE'
    if scope == 'construction_year':
        action = 'update'; vals = {'source_family':family, 'construction_year':r['construction_year'],
            'reported_construction_year':r['reported_construction_year']}
    elif scope == 'land_area':
        action = 'update'; vals = {'source_family':family, 'land_sqft':r['land_sqft'], 'reported_land_sqft':r['reported_land_sqft']}
    elif scope == 'commercial_measurements':
        action = 'exclude' if (r['action'] or '').startswith('exclude') else 'update'
    else:
        action = 'update' if scope == 'selected_fields' else 'replace'
    add(new,action,old,vals,file,n,r['decision_reason'])

# A reviewed component set refers to the original assessment rows. Measurements
# remain reads and sums of those source rows in production.
file = 'residential_reviewed_building_components.csv'
groups = defaultdict(list)
for n,r in enumerate(read(file),2):groups[r['project_id']].append((n,r))
for pid, group in groups.items():
    rows = [r for _,r in group]
    add(pid,'replace','/'.join(sorted({r['source_project_id'] for r in rows})),
        {'source_family':'residential','assessment_rows':'/'.join(sorted({r['row_id'] for r in rows})),
         'assessment_aggregation':'sum_buildings_distinct_land', 'construction_year':rows[0]['construction_year']},
         file,group[0][0],' | '.join(dict.fromkeys(r['reason'] for r in rows)))
file = 'residential_reviewed_card_selections.csv'
groups = defaultdict(list)
for n,r in enumerate(read(file),2):groups[r['pin']].append((n,r))
for pin,group in groups.items():
    add('residential_multicard_'+pin,'replace',None,{'source_family':'residential',
        'assessment_rows':'/'.join(r['row_id'] for _,r in group),
        'assessment_aggregation':'sum_buildings_distinct_land'},file,group[0][0],group[0][1]['reason'])

for file in ['commercial_component_overrides.csv','commercial_manual_decisions.csv']:
    for n,r in enumerate(read(file),2):
        sources = (r['source_project_ids'] or '').replace(';','/')
        action = r['action']; ids = sources.split('/')
        if action.startswith('exclude'):
            for pid in ids:add(pid,'exclude',pid,{'source_family':'commercial',
                'defer_to_residential':'TRUE' if action=='exclude_commercial_defer_residential' else None},file,n,r['decision_reason'])
            continue
        if action == 'split_to_source_rows':
            # Each named original row becomes one building; its ordinary measurements
            # will be read from the source. The final IDs are the recorded key PINs.
            with (root/'data_raw/construction_review/commercial_valuation_data.csv').open() as f:
                raw=list(csv.DictReader(f))
            for row_id in r['source_rows'].split('/'):
                item=raw[int(row_id)-1]
                pin=''.join(c for c in item['keypin'] if c.isdigit())
                add('commercial_'+pin,'replace',sources,{'source_family':'commercial',
                    'assessment_rows':row_id,'assessment_aggregation':'one_source_row',
                    'allow_far':r['allow_far'],'allow_dupac':r['allow_dupac']},file,n,r['decision_reason'])
            continue
        vals = {new:r.get(old) for old,new in [('final_year','construction_year'),('final_units','dwelling_units'),
            ('final_building_sqft','building_sqft'),('final_land_sqft','land_sqft'),('allow_far','allow_far'),
            ('allow_dupac','allow_dupac'),('retained_component_pins','component_pins')]}
        vals['source_family']='commercial'
        vals['evidence_ids']=r.get('evidence_ids')
        vals['assessment_rows']=r.get('source_rows') or r.get('preferred_source_rows')
        vals['assessment_aggregation']=None
        for field,flag in [('final_units','units_unusable'),('final_building_sqft','building_area_unusable')]:
            if r.get(field) is None: vals[flag]='TRUE'
        if r.get('land_source') in ('construction_year_parcel_union','construction_year_parcel_polygon','construction_year_union_of_2021_components'):
            vals['land_selection']='reported_same_property'; vals['land_sqft']=None
        elif r.get('final_land_sqft') is None: vals['land_area_unusable']='TRUE'
        add(r['final_project_id'] or ids[0],'replace' if r['final_project_id'] not in (None,ids[0]) or action!='retain_field_specific' else 'update',
            sources,vals,file,n,r['decision_reason'])

# Recorded source retirements carry no invented building measurements.
for file,key,why in [('residential_reviewed_source_exclusions.csv','project_id','reason'),
    ('residential_reviewed_source_duplicates.csv','project_id','reason'),
    ('residential_reviewed_home_replacements.csv','project_id','reason')]:
    for n,r in enumerate(read(file),2):
        add(r[key],'exclude',r[key],{'source_family':'residential'},file,n,r[why])
file='eligibility_manual_exceptions.csv'
for n,r in enumerate(read(file),2):add(r['project_id'],'exclude',r['project_id'],{},file,n,r['reason'])
file='residential_source_decisions.csv'
for n,r in enumerate(read(file),2):
    if r['action']=='withhold_density':
        add(r['source_project_id'],'update',None,{'allow_far':'FALSE','allow_dupac':'FALSE'},file,n,r['decision_reason'])
    else:add(r['source_project_id'],'exclude',None,{},file,n,r['decision_reason'])
file='residential_unresolved_source_dispositions.csv'
for n,r in enumerate(read(file),2):
    if r['disposition']=='withhold_density':
        add(r['source_project_id'],'update',None,{'allow_far':'FALSE','allow_dupac':'FALSE'},file,n,r['decision_reason'])
    elif r['disposition'].startswith('exclude'):
        add(r['source_project_id'], 'replace' if changes.get(r['source_project_id'],{}).get('action') == 'replace' else 'exclude',None,{},file,n,r['decision_reason'])
file='residential_overlap_decisions.csv'
for n,r in enumerate(read(file),2):
    if r['overlap_action']=='replace_by_residential_successor' and selected_assessments[r['source_project_id']]['candidate_status']!='review_required':
        inactive.append((r['source_project_id'],file,n,'Original consumer applied this replacement only to unresolved measurements; the complete assessment superseded it.'))
        continue
    if r['overlap_action']=='retain_residential_resolution':
        add(r['source_project_id'],'update',None,{'allow_far':'TRUE','allow_dupac':'TRUE'},file,n,r['decision_reason'])
    else:add(r['source_project_id'],'exclude',None,{},file,n,r['decision_reason'])
file='commercial_reported_land_decisions.csv'
for n,r in enumerate(read(file),2):
    add(r['project_id'],changes.get(r['project_id'],{}).get('action','update'),None,
        {'land_sqft':r['land_sqft'],'allow_far':'FALSE' if r['exclude_far']=='TRUE' else None},file,n,r['decision_reason'])
file='construction_building_types.csv'
for n,r in enumerate(read(file),2):
    add(r['project_id'],changes.get(r['project_id'],{}).get('action','update'),None,
        {'multifamily':r['multifamily']},file,n,r['decision_reason'])
file='residential_class297_component_overrides.csv'
for n,r in enumerate(read(file),2):
    add(r['final_project_id'],changes.get(r['final_project_id'],{}).get('action','update'),None,
        {'component_pins':r['component_pins']},file,n,r['decision_reason'])

# Location reviews become ordinary source references on the building row.
file='residential_reviewed_parcel_locations.csv'
for n,r in enumerate(read(file),2):
    pid=r['project_id']
    add(pid,changes.get(pid,{}).get('action','update'),None,
        {'location_source':r['source'],'location_id':r['pin'],'location_year':r['reference_year'],
         'location_target_year':r['target_year']},file,n,r['reason'])
file='residential_reviewed_permit_locations.csv'
for n,r in enumerate(read(file),2):
    pid=r['project_id']
    add(pid,changes.get(pid,{}).get('action','update'),None,
        {'location_source':'completed_permit','location_id':r['permit'],
         'location_target_year':r['target_year']},file,n,r['evidence'])
file='commercial_reviewed_locations.csv'
for n,r in enumerate(read(file),2):
    pid=r['project_id'];location_source=r['source']
    add(pid,changes.get(pid,{}).get('action','update'),None,
        {'location_source':'parcel_universe_2025' if location_source=='reviewed_current_exact_pin' else location_source,
         'location_id':r['source_id'],'location_year':'2025' if location_source=='reviewed_current_exact_pin' else None,
         'location_target_year':r['construction_year']},file,n,r['reason'])
file='residential_additional_candidate_decisions.csv'
for n,r in enumerate(read(file),2):
    pid=r['candidate_project_id']
    if r['decision']=='replace_by_commercial':
        add(pid,'exclude',None,{},file,n,r['decision_reason'])
    else:
        inactive.append((pid,file,n,'Historical review entry; the adopted consumer uses this table only for replacement by commercial records.'))
file='historical_address_corrections.csv'
for n,r in enumerate(read(file),2):
    pid='residential_'+r['pin']
    add(pid,changes.get(pid,{}).get('action','update'),None,
        {'corrected_address':r['corrected_address']},file,n,r['reason'])
file='historical_address_matches.csv'
for n,r in enumerate(read(file),2):
    pid='residential_'+r['pin']
    add(pid,changes.get(pid,{}).get('action','update'),None,
        {'historical_location_pin':r['matched_pin']},file,n,r['decision_reason'])
file='historical_coordinate_year_corrections.csv'
for n,r in enumerate(read(file),2):
    pid='residential_'+r['pin']
    add(pid,changes.get(pid,{}).get('action','update'),None,
        {'historical_location_year':r['matched_construction_year']},file,n,r['reason'])
file='corrected_year_zoning_decisions.csv'
for n,r in enumerate(read(file),2):
    pid=r['project_id']
    add(pid,changes.get(pid,{}).get('action','update'),None,
        {'zoning_group':r['construction_zone_group'],'zoning_year':r['construction_year'],
         'zoning_source':r['decision_source'],'zoning_note':r['decision_note']},file,n,r['decision_note'])

fields=['project_id','action','source_project_ids','source_family','assessment_rows','assessment_aggregation',
    'defer_to_residential','component_pins','construction_year','dwelling_units','building_sqft','land_sqft','allow_far','allow_dupac',
    'multifamily','units_unusable','building_area_unusable','land_area_unusable','land_selection','reported_construction_year','reported_land_sqft','evidence_ids','location_source','location_id','location_year','location_target_year',
    'corrected_address','historical_location_pin','historical_location_year',
    'zoning_group','zoning_year','zoning_source','zoning_note','decision_references','decision_reason']
with (out/'recorded_building_changes_DRAFT.csv').open('w') as f:
    writer=csv.DictWriter(f,fields);writer.writeheader()
    for pid,r in sorted(changes.items()):
        writer.writerow({**r,'decision_references':' | '.join(r['decision_references']),
            'decision_reason':' | '.join(r['decision_reason'])})
with (out/'decision_conflicts.csv').open('w') as f:
    w=csv.writer(f);w.writerow(['project_id','field','earlier','later','later_source']);w.writerows(conflicts)
print(len(changes),'draft change records;',len(conflicts),'overlaps to check against the existing application order')

with (out/'inactive_decisions.csv').open('w') as f:
    w=csv.writer(f);w.writerow(['project_id','source_file','source_row','reason']);w.writerows(inactive)
