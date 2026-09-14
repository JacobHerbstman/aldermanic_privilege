import csv,re
from pathlib import Path
root=Path('/Users/jacobherbstman/Desktop/aldermanic_privilege')
archive=root/'tasks/audits/construction_review_history/records/production_decisions_before_consolidation'
p=root/'tasks/new_construction_cleaning/output/recorded_building_changes.csv'
changes=list(csv.DictReader(p.open()));by_id={r['project_id']:r for r in changes}
raw=list(csv.DictReader((root/'data_raw/construction_review/commercial_valuation_data.csv').open()))
def pins(row):
    result=[]
    for text in row['pins'].split(','):
        pin=re.sub(r'\D','',text)
        if len(pin)==13:pin+='0'
        assert len(pin)==14,(text,pin)
        result.append(pin)
    return result
for r in csv.DictReader((archive/'commercial_component_overrides.csv').open()):
    if r['action']!='split_to_source_rows':continue
    for index in r['source_rows'].split('/'):
        source=raw[int(index)-1]
        pid='commercial_'+re.sub(r'\D','',source['keypin'])
        by_id[pid]['component_pins']='/'.join(sorted(set(pins(source))))
for r in csv.DictReader((archive/'commercial_manual_decisions.csv').open()):
    if r['action']!='merge_source_projects' or not r['preferred_source_rows']:continue
    sources=[raw[int(i)-1] for i in r['preferred_source_rows'].split('/')]
    by_id[r['final_project_id']]['component_pins']='/'.join(sorted({pin for row in sources for pin in pins(row)}))
with p.open('w') as f:
    w=csv.DictWriter(f,changes[0]);w.writeheader();w.writerows(changes)
