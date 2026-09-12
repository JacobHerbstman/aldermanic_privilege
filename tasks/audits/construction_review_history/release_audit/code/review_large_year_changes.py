"""Record the bounded 26-case review without changing production decisions."""
import csv
import duckdb
with open('../output/population_chronology_review.csv') as source:
    cohort = [r for r in csv.DictReader(source) if r['draft_year_change']
              and abs(int(r['draft_year_change'])) > 3]
with open('../reference/large_year_change_recommendations.csv') as source:
    decisions = list(csv.DictReader(source))
assert len(cohort) == len(decisions) == 26
assert len({r['project_id'] for r in decisions}) == 26
assert {r['project_id'] for r in cohort} == {r['project_id'] for r in decisions}
decisions = {r['project_id']: r for r in decisions}
con = duckdb.connect()
addresses = con.execute("""SELECT pin,prop_address_full FROM read_csv(
 '../input/parcel_addresses_2025_chicago.csv',all_varchar=true)
 WHERE pin IN (SELECT unnest(?))""", [[r['project_id'].split('_')[-1] for r in cohort]]).fetchall()
assert len({r[0] for r in addresses}) == len(addresses)
addresses = dict(addresses)
rows = []
for r in cohort:
    d = decisions[r['project_id']]
    rows.append(dict(project_id=r['project_id'],
        address=addresses.get(r['project_id'].split('_')[-1], ''),
        current_year=r['construction_year'], draft_year=r['draft_year'],
        within_500ft=r['within_500ft_before'],
        **{k:v for k,v in d.items() if k != 'project_id'}))
with open('../output/large_year_change_review.csv','w',newline='') as target:
    writer=csv.DictWriter(target,fieldnames=list(rows[0]))
    writer.writeheader()
    writer.writerows(rows)
