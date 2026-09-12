# Run from tasks/working_paper_release_audit/code.
import duckdb

con = duckdb.connect()
# Address ranges are search areas, not automatic project matches. Retain revisions
# and non-construction permits so a reviewer can distinguish later work.
con.execute("""
COPY (
    SELECT CASE
        WHEN street_name LIKE 'FLETCHER%' OR street_name LIKE 'CAMPBELL%' THEN 'Fletcher'
        WHEN regexp_matches(street_name, '50TH|51ST') THEN 'Park Place'
        ELSE 'Eastgate' END AS review_site,
        id, permit_, permit_type, application_start_date, issue_date,
        street_number, street_direction, street_name, work_description, pin_list
    FROM read_csv('../input/building_permits_full.csv', all_varchar=true)
    WHERE (street_direction='W' AND street_name LIKE 'FLETCHER%'
           AND try_cast(street_number AS INTEGER) BETWEEN 2422 AND 2456)
       OR (street_direction='N' AND street_name LIKE 'CAMPBELL%'
           AND try_cast(street_number AS INTEGER) BETWEEN 3139 AND 3141)
       OR street_name LIKE '%EASTGATE%'
       OR (street_direction='S' AND regexp_matches(street_name, 'CALUMET|KING')
           AND try_cast(street_number AS INTEGER) BETWEEN 2500 AND 2535)
       OR (street_direction='W' AND regexp_matches(street_name, '51ST|50TH')
           AND try_cast(street_number AS INTEGER) BETWEEN 3600 AND 3655)
       OR (street_direction='E' AND street_name LIKE '25TH%'
           AND try_cast(street_number AS INTEGER) BETWEEN 340 AND 352)
    ORDER BY review_site, street_name, try_cast(street_number AS INTEGER), issue_date, id
) TO '../output/denominator_permit_evidence.csv' (HEADER)
""")
assert con.execute("""
SELECT count(*)=count(DISTINCT id) AND count(*)=count(id)
FROM read_csv('../output/denominator_permit_evidence.csv', all_varchar=true)
""").fetchone()[0]
# Verify that the key newly identified permits have survived the search.
assert con.execute("""
SELECT count(*)=7 FROM read_csv('../output/denominator_permit_evidence.csv', all_varchar=true)
WHERE permit_ IN ('100125952','100125948','100109482','100109481',
                 '100110249','100110230','100109483')
""").fetchone()[0]
con.close()
