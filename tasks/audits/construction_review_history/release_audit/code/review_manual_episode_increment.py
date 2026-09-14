# Run from tasks/working_paper_release_audit/code.
# Numerical agreement does not establish that a successor suppression is valid.
import duckdb

con = duckdb.connect()
con.execute("""
CREATE TABLE decisions AS SELECT * FROM read_csv(
    '../reference/multicard_manual_episode_decisions.csv', all_varchar=true);
CREATE TABLE adjudication AS SELECT * FROM read_csv(
    '../input/multicard_final_adjudication.csv', all_varchar=true);
""")
for table in ['decisions', 'adjudication']:
    assert con.execute(f"""
        SELECT count(*)=count(DISTINCT project_id)
          AND count(*)=count(project_id) FROM {table}
    """).fetchone()[0], f'{table} must be unique by project'
assert con.execute("""
    SELECT count(*) FROM decisions d ANTI JOIN adjudication a USING(project_id)
""").fetchone()[0] == 0

con.execute("""
COPY (
    SELECT d.project_id, d.disposition, d.successor_rule,
        a.rule_disposition, a.final_disposition,
        try_cast(a.rule_units AS DOUBLE) AS rule_units,
        try_cast(a.final_units AS DOUBLE) AS final_units,
        try_cast(a.rule_building_sqft AS DOUBLE) AS rule_building_sqft,
        try_cast(a.final_building_sqft AS DOUBLE) AS final_building_sqft,
        a.rule_disposition = a.final_disposition
            AND try_cast(a.rule_units AS DOUBLE) IS NOT DISTINCT FROM
                try_cast(a.final_units AS DOUBLE)
            AND try_cast(a.rule_building_sqft AS DOUBLE) IS NOT DISTINCT FROM
                try_cast(a.final_building_sqft AS DOUBLE)
            AS disposition_and_values_already_computed,
        d.evidence
    FROM decisions d JOIN adjudication a USING(project_id)
    ORDER BY d.project_id
) TO '../output/manual_episode_increment.csv' (HEADER);
""")
con.close()
