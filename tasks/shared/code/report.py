"""Read-only CSV, Parquet, and single-layer GeoPackage attribute reports.

DuckDB computes the saved-file summary. Distinct counts are exact up to 100,000 rows and approximate above that;
row counts, non-missing counts, declared-key checks, and file SHA-256 are exact.
CSV column types are inferred by DuckDB, not a schema stored in the CSV.
"""
import hashlib
import sqlite3
import sys
from pathlib import Path

import duckdb
import pandas as pd


def quote_identifier(value):
    return '"' + value.replace('"', '""') + '"'


def write_summary(source_file, report_file, keys):
    con = duckdb.connect()
    con.execute("SET threads=1")
    if Path(source_file).suffix == '.gpkg':
        # Summarize saved attributes without loading an extension or interpreting
        # geometry blobs. The producing spatial script owns geometry validation.
        with sqlite3.connect(Path(source_file).resolve().as_uri() + '?mode=ro', uri=True) as spatial:
            layers = spatial.execute('SELECT table_name, column_name FROM gpkg_geometry_columns').fetchall()
            if len(layers) != 1:
                raise ValueError('A GeoPackage report requires exactly one spatial layer')
            table, geometry = layers[0]
            attributes = [row[1] for row in spatial.execute('PRAGMA table_info(' + quote_identifier(table) + ')')
                          if row[1] != geometry]
            frame = pd.read_sql_query('SELECT ' + ', '.join(map(quote_identifier, attributes))
                                      + ' FROM ' + quote_identifier(table), spatial)
        con.register('saved_attributes', frame)
        source = key_source = 'saved_attributes'
        type_description = 'Types: SQLite attributes read through pandas; geometry excluded.\n'
    elif Path(source_file).suffix == '.parquet':
        source = "read_parquet('" + source_file.replace("'", "''") + "')"
        key_source = source
        type_description = 'Types: Stored Parquet schema.\n'
    else:
        source = "read_csv('" + source_file.replace("'", "''") + "', sample_size=-1, nullstr=['', 'NA'])"
        key_source = source[:-1] + ', all_varchar=true)'
        type_description = 'Types: DuckDB inference from the complete saved CSV.\n'
    summary = con.execute(f"SUMMARIZE SELECT * FROM {source}").fetchdf()
    columns = summary['column_name'].tolist()
    row_count = int(summary['count'].iloc[0])
    if not set(keys).issubset(columns):
        raise ValueError('Declared key column is absent from saved data')
    if keys:
        # Check identifiers as source text to preserve leading zeros and precision.
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
        f'{type_description}'
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
        raise SystemExit('Usage: report.py source.{csv,parquet,gpkg} report.log [key columns ...]')
    write_summary(sys.argv[1], sys.argv[2], sys.argv[3:])
