# Run from tasks/working_paper_release_audit/code.
import duckdb

con = duckdb.connect()
# Broad address search areas retain all permit types and dates. These rows are
# evidence for manual review, not automatic assignments to construction projects.
con.execute("""
COPY (
 SELECT CASE WHEN street_name LIKE 'LENOX%' THEN 'Lenox'
             WHEN street_name LIKE 'ROCKWELL%' THEN 'Rockwell'
             ELSE '33rd and Prairie' END AS review_site,
        id, permit_, permit_type, application_start_date, issue_date,
        street_number, street_direction, street_name, work_description, pin_list
 FROM read_csv('../input/building_permits_full.csv', all_varchar=true)
 WHERE (street_direction='N' AND street_name LIKE 'LENOX%'
        AND try_cast(street_number AS INTEGER) BETWEEN 6230 AND 6238)
    OR (street_direction='N' AND street_name LIKE 'ROCKWELL%'
        AND try_cast(street_number AS INTEGER) BETWEEN 2417 AND 2421)
    OR (street_direction='E' AND street_name LIKE '33RD%'
        AND try_cast(street_number AS INTEGER) BETWEEN 239 AND 245)
    OR (street_direction='S' AND street_name LIKE 'PRAIRIE%'
        AND try_cast(street_number AS INTEGER) BETWEEN 3301 AND 3303)
 ORDER BY review_site, street_name, try_cast(street_number AS INTEGER), issue_date, id
) TO '../output/multicard_followup_permits.csv' (HEADER)
""")
assert con.execute("""
SELECT count(*)=count(DISTINCT id) AND count(*)=count(id)
FROM read_csv('../output/multicard_followup_permits.csv', all_varchar=true)
""").fetchone()[0]
assert con.execute("""
SELECT count(*)=6 FROM read_csv('../output/multicard_followup_permits.csv', all_varchar=true)
WHERE permit_ IN ('100163191','100163192','100784138','100784145','100076635','100076611')
""").fetchone()[0]
con.close()
