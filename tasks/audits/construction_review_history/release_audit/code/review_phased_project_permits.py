# Run from tasks/working_paper_release_audit/code.
import duckdb

con = duckdb.connect()
# These are evidence searches, not accepted project membership or cleaning rules.
# Preserve all permit types, revisions, and source dates within the search.
con.execute("""
COPY (
    SELECT CASE
        WHEN street_name LIKE '%COTTAGE GROVE%' THEN '4400 Grove'
        WHEN street_name LIKE '%ARTHINGTON%' THEN 'Roosevelt Square: Arthington'
        ELSE 'Natchez and neighboring development' END AS review_site,
        id, permit_, permit_type, permit_status, permit_milestone,
        application_start_date, issue_date,
        street_number, street_direction, street_name, work_description, pin_list
    FROM read_csv('../input/building_permits_full.csv', all_varchar=true)
    WHERE (street_direction='S' AND street_name LIKE '%COTTAGE GROVE%'
           AND try_cast(street_number AS INTEGER) BETWEEN 4400 AND 4500)
       OR (street_direction='W' AND street_name LIKE '%ARTHINGTON%'
           AND try_cast(street_number AS INTEGER) BETWEEN 1200 AND 1230)
       OR (street_direction='N' AND street_name LIKE '%NATCHEZ%'
           AND try_cast(street_number AS INTEGER) BETWEEN 2000 AND 2160)
       OR (street_direction='N' AND street_name LIKE '%NASHVILLE%'
           AND try_cast(street_number AS INTEGER) BETWEEN 2100 AND 2160)
       OR (street_direction='W' AND regexp_matches(street_name, 'DICKENS|SHAKESPEARE')
           AND try_cast(street_number AS INTEGER) BETWEEN 6530 AND 6550)
       OR regexp_matches(pin_list, '1331214001|1331205023|1331205024|133120508[1234]')
    ORDER BY review_site, issue_date, id
) TO '../output/phased_project_permits.csv' (HEADER)
""")
assert con.execute("""
SELECT count(*)=count(DISTINCT id) AND count(*)=count(id)
FROM read_csv('../output/phased_project_permits.csv', all_varchar=true)
""").fetchone()[0]
con.close()
