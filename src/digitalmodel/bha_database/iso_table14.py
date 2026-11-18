"""Nominal reference loader for ISO 10424-1:2004, Table 14.

The 23 supplied rows contain designation, OD, bore, permitted nominal lengths,
bevel diameter and reference bending strength ratio. The source bundle records
138 compared cells; a fresh PDF comparison is not established by this release.
The historical edition does not establish current design qualification.
Stored weight is an assumed computation, not a published Table 14 value.
Rows are separate references, not assembly components or equipment ratings.
"""
from __future__ import annotations

import argparse
import json
import math
from pathlib import Path

from .loader import BhaDatabaseError, read_yaml

DEFAULT_PATH = Path(__file__).resolve().parents[3] / 'data/bha_standards/drill_collars_std.yml'
SOURCE_KEY = 'ISO10424-1-2004-T14'
WEIGHT_KEY = 'SYNTHETIC-STEEL-WEIGHT-ASSUMPTION'
UNITS = {'designation': 'dimensionless', 'od': 'mm', 'id': 'mm',
         'length': 'm', 'bevel_diameter': 'mm', 'reference_bsr': 'dimensionless',
         'weight_per_length': 'lbf/ft'}


def require(condition, reason):
    if not condition:
        raise BhaDatabaseError(f'Table 14: {reason}')


def positive_number(value):
    return type(value) in (float, int) and math.isfinite(value) and value > 0


def validate_quantity(field, quantity):
    weight = field == 'weight_per_length'
    require(quantity['unit'] == UNITS[field], f'{field} unit')
    require(quantity['source_key'] == (WEIGHT_KEY if weight else SOURCE_KEY),
            f'{field} source_key')
    require(quantity['basis'] == ('computed' if weight else 'published'), f'{field} basis')
    if 'verification' in quantity:
        status = quantity['verification']
        require(status in ('verified', 'pending'), f'{field} verification')
        context = ('verification_reason', 'verification_issue') if status == 'pending' else (
            'verification_locator',)
        for key in context:
            require(isinstance(quantity.get(key), str) and quantity[key].strip(),
                    f'{field} {key}')
    for key in ('as_published', 'as_published_on'):
        require(isinstance(quantity[key], str) and quantity[key].strip(), f'{field} {key}')
    if weight:
        require(quantity['formula'] == '2.67 * ((od_mm / 25.4)**2 - (id_mm / 25.4)**2)',
                'weight formula')
    value = quantity['value']
    if field == 'designation':
        require(isinstance(value, str) and value.strip(), 'designation')
    elif field == 'length':
        require(isinstance(value, list) and value and all(map(positive_number, value)),
                'nominal lengths')
    else:
        require(positive_number(value), f'{field} positive finite value')


def validate_document(data):
    require(type(data['schema_version']) is int and data['schema_version'] == 1,
            'schema version')
    sources = {source['key']: source for source in data['sources']}
    require(len(sources) == len(data['sources']) == 2, 'source identities')
    require(set(sources) == {SOURCE_KEY, WEIGHT_KEY}, 'source keys')
    standard = sources[SOURCE_KEY]
    require(standard['type'] == 'standard' and standard['edition'] == '2004',
            'source edition/type')
    require(standard['title'] == 'BS EN ISO 10424-1:2004 Rotary drill stem elements',
            'source standard')
    require(standard['locator'] == 'Table 14; printed pages 40–41; PDF pages 50–51',
            'source locator')
    require(standard['sha256'] ==
            'bc874fc24aebded53d76de12869a7bb2239cb5a871382bcfd3f2f3db7d6062ce',
            'source PDF digest')
    rows = data['components']
    require(isinstance(rows, list) and len(rows) == 23, 'expected 23 rows')
    identities, component_ids = set(), set()
    for row in rows:
        require(row['type'] == 'drill_collar' and row['data_class'] == 'STANDARD-NOMINAL',
                'nominal drill collar required')
        for field in UNITS:
            validate_quantity(field, row[field])
        identity = (row['designation']['value'], row['id']['value'])
        require(identity not in identities, 'duplicate designation/bore')
        require(row['component_id'] not in component_ids, 'duplicate component_id')
        identities.add(identity)
        component_ids.add(row['component_id'])
        require(row['id']['value'] < row['od']['value'], 'bore must be below OD')
        require(row['bevel_diameter']['value'] < row['od']['value'], 'bevel must be below OD')


def load_table14(path: Path | str = DEFAULT_PATH):
    """Load cited ISO 10424-1:2004, Table 14 rows and resolving source records.

    Values are as transcribed in the supplied bundle, whose record reports a
    138-cell comparison. A fresh PDF comparison is not established here.
    Validation covers structure and provenance labels, not PDF authenticity or
    per-value equality; numeric regression tests pin the supplied table. Source
    checked date, digest and locator are inherited, not freshly verified.
    Six table fields retain supplied units and as_published metadata. Stored
    lbf/ft weight uses an assumed coefficient; it is not a standard rating.
    Defaults to source-checkout data; installed consumers pass a YAML path.

    Optional cell verification accepts only pending or verified. Pending cells
    require verification_reason and verification_issue; verified cells require
    verification_locator recording the comparison's printed page. These labels
    do not authenticate a source comparison. An absent status remains unspecified,
    not implicitly verified. The returned pending_cells list identifies each
    pending component_id, field, source_key, reason and issue. It is recomputed
    from cell metadata on every load, including when reading a saved result.
    """
    try:
        _, data = read_yaml(Path(path))
        validate_document(data)
        data['pending_cells'] = [
            {'component_id': row['component_id'], 'field': field,
             'source_key': row[field]['source_key'],
             'reason': row[field]['verification_reason'],
             'issue': row[field]['verification_issue']}
            for row in data['components'] for field in UNITS
            if row[field].get('verification') == 'pending'
        ]
        return data
    except BhaDatabaseError:
        raise
    except FileNotFoundError:
        raise BhaDatabaseError('Table 14 data missing; pass an explicit YAML path') from None
    except Exception as exc:
        raise BhaDatabaseError(f'Table 14 validation failed ({type(exc).__name__})') from None


def audit_table14(path: Path | str = DEFAULT_PATH):
    """Report comparison coverage for every published Table 14 field.

    Issue #2629 requires every row to be compared with the licensed
    ISO 10424-1:2004 Table 14 and its printed page recorded before correction.
    Pending and unmarked cells are NOT_EVALUATED. Only an explicit verified
    cell with a locator is COMPARISON_RECORDED; this records supplied metadata,
    not source access, authenticity, equality or design qualification.
    Inherited source locators cannot supply missing cell comparison records.
    Computed weight is outside Table 14 and excluded from coverage.

    The table is validated and coverage recomputed on every call. No file is
    written and no quantity is changed. A complete set of comparison records
    still requires review of the actual source evidence by the maintainer.
    """
    data = load_table14(path)
    counts = dict(total=0, recorded=0, pending=0, unspecified=0)
    cells = []
    for row in data['components']:
        for field in UNITS:
            if field == 'weight_per_length':
                continue
            quantity = row[field]
            verification = quantity.get('verification', 'unspecified')
            recorded = verification == 'verified'
            counts['total'] += 1
            counts['recorded' if recorded else verification] += 1
            cells.append({
                'component_id': row['component_id'],
                'field': field,
                'source_key': quantity['source_key'],
                'verification': verification,
                'status': 'COMPARISON_RECORDED' if recorded else 'NOT_EVALUATED',
                'locator': quantity['verification_locator'] if recorded else None,
                'reason': (
                    'Comparison metadata recorded; source evidence requires review.'
                    if recorded else quantity.get(
                        'verification_reason', 'No cell comparison record supplied.')
                ),
                'issue': quantity.get('verification_issue'),
            })
    return {
        'source_key': SOURCE_KEY,
        'status': ('COMPARISON_RECORDED' if counts['recorded'] == counts['total']
                   else 'NOT_EVALUATED'),
        'counts': counts,
        'cells': cells,
    }


def main(argv: list[str] | None = None) -> int:
    """Print a JSON comparison-coverage report without editing source data.

    Exit success means the report was generated, including NOT_EVALUATED.
    Invalid or missing input is an error; callers inspect status for coverage.
    """
    parser = argparse.ArgumentParser(description=main.__doc__)
    parser.add_argument('path', nargs='?', type=Path, default=DEFAULT_PATH,
                        help='Table 14 YAML; defaults to source-checkout data')
    args = parser.parse_args(argv)
    try:
        report = audit_table14(args.path)
    except BhaDatabaseError as exc:
        parser.error(str(exc))
    print(json.dumps(report, indent=2, allow_nan=False))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
