# Run from tasks/working_paper_release_audit/code.
# Compare the external review layer with its already-adjudicated input.
import duckdb

con = duckdb.connect()
con.execute("""
CREATE TABLE reviews AS SELECT * FROM read_csv(
    '../reference/multicard_external_review_queue.csv', all_varchar=true);
CREATE TABLE projects AS SELECT * FROM read_csv(
    '../reference/multicard_adjudicated_density_model_input.csv', all_varchar=true);
""")
for table in ['reviews', 'projects']:
    assert con.execute(f"""
        SELECT count(*)=count(DISTINCT project_id)
          AND count(*)=count(project_id) FROM {table}
    """).fetchone()[0], f'{table} must be unique by project'

con.execute("""
COPY (
    WITH comparison AS (
        SELECT r.project_id, r.review_status, r.building_type_rule,
            CASE r.building_type_rule
                WHEN 'single_family_buildings' THEN 'exclude'
                WHEN 'multifamily_building' THEN 'include'
                WHEN 'suppressed' THEN 'suppress'
                ELSE 'unresolved' END AS prior_type_disposition,
            r.multifamily_disposition AS reviewed_type_disposition,
            p.project_id IS NOT NULL AS present_before_external_review,
            try_cast(p.dwelling_units AS DOUBLE) AS prior_units,
            try_cast(r.external_unit_count AS DOUBLE) AS reviewed_units,
            try_cast(p.building_sqft AS DOUBLE) AS prior_building_sqft,
            try_cast(r.external_building_sqft AS DOUBLE) AS reviewed_building_sqft,
            r.internal_evidence, r.google_visual_type, r.reviewer_notes,
            r.source_1_url, r.source_2_url
        FROM reviews r LEFT JOIN projects p USING(project_id)
    ), differences AS (
        SELECT *, prior_type_disposition <> reviewed_type_disposition AS type_differs,
            present_before_external_review
                AND reviewed_type_disposition IN ('include', 'exclude')
                AND reviewed_units IS NOT NULL
                AND reviewed_units IS DISTINCT FROM prior_units AS units_differ,
            present_before_external_review
                AND reviewed_type_disposition IN ('include', 'exclude')
                AND reviewed_building_sqft IS NOT NULL
                AND reviewed_building_sqft IS DISTINCT FROM prior_building_sqft
                AS building_sqft_differs
        FROM comparison
    )
    SELECT *, CASE WHEN type_differs OR units_differ OR building_sqft_differs
        THEN 'explain_incremental_review_decision'
        ELSE 'agrees_with_preceding_adjudicated_input' END AS comparison_status
    FROM differences ORDER BY project_id
) TO '../output/manual_review_increment.csv' (HEADER);
""")
con.close()
