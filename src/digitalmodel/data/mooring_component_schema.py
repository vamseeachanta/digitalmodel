"""Schema and provenance validation for curated mooring component CSVs.

Legacy custom directories without a sidecar remain explicitly unverified.
"""
import copy
import hashlib
import json
import re
from datetime import datetime

import numpy as np
import pandas as pd

TABLE_COLUMNS = {
    'chain': ['diameter', 'grade', 'link_type', 'mbl', 'stiffness',
              'weight_air', 'weight_water', 'bend_radius', 'fatigue_category',
              'certification', 'manufacturer', 'part_number', 'notes'],
    'wire': ['diameter', 'construction', 'grade', 'mbl', 'stiffness',
             'weight_air', 'weight_water'],
    'line': ['diameter', 'material', 'mbl', 'stiffness', 'weight_air', 'weight_water'],
}
IDENTITY_FIELDS = {
    'chain': ['diameter', 'grade', 'link_type'],
    'wire': ['diameter', 'construction', 'grade'],
    'line': ['diameter', 'material'],
}
UNITS = {'diameter': 'mm', 'mbl': 'kN', 'stiffness': 'kN',
         'weight_air': 'kg/m', 'weight_water': 'kg/m', 'bend_radius': 'm'}
SOURCE_FIELDS = ['source_id', 'name', 'url', 'commit', 'file', 'locator',
                 'retrieved_at', 'license', 'terms', 'terms_url', 'sha256']


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def row_identity(kind, row):
    return {key: float(row[key]) if key == 'diameter' else row[key]
            for key in IDENTITY_FIELDS[kind]}


def validate_table(kind, table):
    """Reject malformed properties before returning a curated table."""
    filename = f'{kind}_properties.csv'
    expected = set(TABLE_COLUMNS[kind])
    if set(table.columns) != expected:
        raise ValueError(f'{filename}: column schema mismatch; '
                         f'missing {sorted(expected - set(table.columns))}; '
                         f'extra {sorted(set(table.columns) - expected)}')
    for column in table.columns:
        if column in UNITS:
            values = pd.to_numeric(table[column], errors='coerce').astype(float)
            bad = ~np.isfinite(values) | (values <= 0)
        else:
            values = table[column].astype(str)
            bad = values.str.strip().eq('') if column != 'notes' else values.isna()
        if bad.any():
            raise ValueError(f'{filename}: row {bad[bad].index[0]} column '
                             f'{column} requires a finite positive number '
                             'or a nonempty schema string')
        table[column] = values
    if table.duplicated(IDENTITY_FIELDS[kind]).any():
        raise ValueError(f'{filename}: duplicate component identity')
    families = IDENTITY_FIELDS[kind][1:]
    for _, rows in table.groupby(families):
        rows = rows.sort_values('diameter')
        for column in ['mbl', 'weight_air']:
            if (rows[column].diff().dropna() <= 0).any():
                raise ValueError(f'{filename}: {column} must increase with '
                                 'diameter within each family')


def _validate_sources(sources, fields=None):
    fields = SOURCE_FIELDS if fields is None else fields
    if not isinstance(sources, dict) or not sources:
        raise ValueError('provenance: missing sources')
    for source in sources.values():
        if any(not isinstance(source.get(key), str) or not source[key].strip()
               for key in fields):
            raise ValueError('provenance: incomplete source or reuse terms')
        if source['terms'].strip().lower() == 'terms not stated':
            raise ValueError('provenance: terms not stated denies reuse')
        if not re.fullmatch(r'[a-f0-9]{40}', source['commit']):
            raise ValueError('provenance: invalid source commit')
        if not re.fullmatch(r'[a-f0-9]{64}', source['sha256']):
            raise ValueError('provenance: invalid source digest')
        date = datetime.fromisoformat(source['retrieved_at'].replace('Z', '+00:00'))
        if date.utcoffset() is None or date.utcoffset().total_seconds() != 0:
            raise ValueError('provenance: retrieval timestamp must use UTC')


def validate_provenance(data_dir, tables, provenance):
    """Bind every curated row, its units and local source excerpt to bytes."""
    try:
        if provenance['schema_version'] != 1 or provenance['units'] != UNITS:
            raise ValueError('provenance: unsupported schema or units')
        _validate_sources(provenance['sources'])
        sampling_fields = ['owner_repo'] + [
            key for key in SOURCE_FIELDS if key not in ['source_id', 'name']]
        _validate_sources({'sampling': provenance['diameter_source']}, sampling_fields)
        excerpt = data_dir / 'sources' / 'moorpy_coefficients.json'
        if sha256(excerpt) != provenance['excerpt_sha256']:
            raise ValueError('provenance: source excerpt digest mismatch')
        for kind, table in tables.items():
            entry = provenance['tables'][kind]
            if entry['row_count'] != len(table):
                raise ValueError(f'provenance: {kind} manifest row count mismatch')
            if entry['sha256'] != sha256(data_dir / f'{kind}_properties.csv'):
                raise ValueError(f'provenance: {kind} CSV digest mismatch')
            records = provenance['rows'][kind]
            if len(records) != len(table):
                raise ValueError(f'provenance: {kind} row coverage mismatch')
            for record, (_, row) in zip(records, table.iterrows()):
                if record['identity'] != row_identity(kind, row):
                    raise ValueError(f'provenance: {kind} identity mismatch')
                if record['source'] not in provenance['sources']:
                    raise ValueError('provenance: unknown row source')
                if record['kind'] not in ['computed', 'catalogue', 'reference-model']:
                    raise ValueError('provenance: invalid row kind')
                if record['kind'] == 'computed' and (
                        not record['formula'] or not record['coefficients']):
                    raise ValueError('provenance: computed row lacks derivation')
    except (KeyError, TypeError, AttributeError, OSError) as exc:
        raise ValueError(f'provenance: incomplete sidecar: {exc}') from exc


def load_tables(data_dir):
    """Load historical CSVs or a fail-closed curated library with a sidecar."""
    sidecar = data_dir / 'provenance.json'
    try:
        provenance = json.loads(sidecar.read_text()) if sidecar.exists() else None
    except json.JSONDecodeError as exc:
        raise ValueError(f'provenance: malformed JSON: {exc.msg}') from exc
    tables = {}
    for kind in TABLE_COLUMNS:
        path = data_dir / f'{kind}_properties.csv'
        if path.exists():
            table = pd.read_csv(path, keep_default_na=provenance is None)
            if provenance is not None:
                validate_table(kind, table)
        elif provenance is not None:
            raise ValueError(f'provenance: required {path.name} is missing')
        else:
            table = pd.DataFrame()
        tables[kind] = table
    if provenance is not None:
        validate_provenance(data_dir, tables, provenance)
    return tables, provenance


def find_provenance(provenance, kind, diameter, specification):
    if provenance is None:
        raise ValueError('No provenance sidecar; this library is unverified')
    matches = [record for record in provenance['rows'].get(kind, [])
               if record['identity']['diameter'] == diameter
               and all(record['identity'].get(key) == value
                       for key, value in specification.items())]
    if len(matches) != 1:
        raise ValueError('Provenance not found or ambiguous for specification')
    result = copy.deepcopy(matches[0])
    result['source'] = copy.deepcopy(provenance['sources'][result['source']])
    result['diameter_source'] = copy.deepcopy(provenance.get('diameter_source'))
    return result
