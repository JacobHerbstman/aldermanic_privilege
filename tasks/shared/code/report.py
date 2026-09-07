"""Read-only CSV reports, adapted from Jacob's project-template report.py.

DuckDB computes the saved-file summary. Distinct counts are exact up to 100,000 rows and approximate above that;
row counts, non-missing counts, declared-key checks, and file SHA-256 are exact.
CSV column types are inferred by DuckDB, not a schema stored in the CSV.
"""
import hashlib
import sys
from pathlib import Path

import duckdb


def quote_identifier(value):
    return '"' + value.replace('"', '""') + '"'


def write_summary(source_file, report_file, keys):
    source = "read_csv('" + source_file.replace("'", "''") + "', sample_size=-1, nullstr=['', 'NA'])"
    con = duckdb.connect()
    con.execute("SET threads=1")
    summary = con.execute(f"SUMMARIZE SELECT * FROM {source}").fetchdf()
    columns = summary['column_name'].tolist()
    row_count = int(summary['count'].iloc[0])
    if not set(keys).issubset(columns):
        raise ValueError('Declared key column is absent from saved data')
    if keys:
        # Check identifiers as source text to preserve leading zeros and precision.
        key_source = source[:-1] + ', all_varchar=true)'
        key_columns = ', '.join(quote_identifier(key) for key in keys)
        missing = ' OR '.join(quote_identifier(key) + ' IS NULL' for key in keys)
        missing_keys = con.execute(f'SELECT count(*) FROM {key_source} WHERE {missing}').fetchone()[0]
        duplicate_keys = con.execute(
            f'SELECT count(*) FROM (SELECT {key_columns} FROM {key_source} '
            f'GROUP BY {key_columns} HAVING count(*) > 1)'
        ).fetchone()[0]
        if missing_keys or duplicate_keys:
            raise ValueError(f'Declared key fails: {missing_keys} missing rows, {duplicate_keys} duplicated keys')
    non_missing = con.execute(
        'SELECT ' + ', '.join('count(' + quote_identifier(column) + ')' for column in columns)
        + f' FROM {source}'
    ).fetchone()
    distinct_method = 'approximate'
    if row_count <= 100000:
        summary['approx_unique'] = con.execute(
            'SELECT ' + ', '.join('count(DISTINCT ' + quote_identifier(column) + ')' for column in columns)
            + f' FROM {source}'
        ).fetchone()
        distinct_method = 'exact'
    summary['non_missing'] = non_missing
    summary = summary[['column_name', 'column_type', 'non_missing', 'approx_unique', 'min', 'max', 'avg', 'std']]
    summary.columns = ['column', 'inferred_type', 'non_missing', 'distinct', 'min', 'max', 'mean', 'sd']
    # Text extrema can be long free-form review notes; numeric statistics are sufficient.
    numeric = summary['inferred_type'].str.match(r'^(U?TINYINT|U?SMALLINT|U?INTEGER|U?BIGINT|U?HUGEINT|FLOAT|REAL|DOUBLE|DECIMAL)')
    summary.loc[~numeric, ['min', 'max', 'mean', 'sd']] = None
    digest = hashlib.sha256()
    with open(source_file, 'rb') as file:
        for chunk in iter(lambda: file.read(1024 * 1024), b''):
            digest.update(chunk)
    text = (
        f'File: {Path(source_file).name}\nRows: {row_count}\nColumns: {len(columns)}\n'
        f'SHA-256 (saved bytes): {digest.hexdigest()}\n'
        f'Key: {", ".join(keys) if keys else "not declared in this report"}\n'
        'Types: DuckDB inference from the complete saved CSV.\n'
        f'Distinct counts: {distinct_method} (exact up to 100,000 rows).\n'
        'Row counts, non-missing counts, and declared-key checks: exact.\n\n'
        + summary.astype(object).fillna("-").to_string(index=False) + '\n'
    )
    con.close()
    # A failed computation must leave the previous report intact.
    temporary = Path(report_file + '.tmp')
    temporary.write_text(text)
    temporary.replace(report_file)


if __name__ == '__main__':
    if len(sys.argv) < 3:
        raise SystemExit('Usage: report.py source.csv report.csv.log [key columns ...]')
    write_summary(sys.argv[1], sys.argv[2], sys.argv[3:])
