"""Regenerate the public component seed offline from recorded source excerpts."""
import csv
import json
import math
import shutil
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'src'))
from digitalmodel.data.mooring_component_schema import (  # noqa: E402
    IDENTITY_FIELDS, TABLE_COLUMNS, UNITS, row_identity, sha256,
)

DATA = ROOT / 'src/digitalmodel/data/marine_engineering/mooring_components'
SOURCE = DATA / 'sources/moorpy_coefficients.json'
FORMULA = (
    'd = diameter / mm_per_m [m]; mass = mass_d2 * d**2 [kg/m]; '
    'MBL_N = MBL_d2 * d**2 + MBL_d3 * d**3; '
    'EA_N = EA_d2 * d**2 + EA_MBL * MBL_N; '
    'mbl = MBL_N / n_per_kn [kN]; stiffness = EA_N / n_per_kn [kN]; '
    'weight_water = mass * (1 - water_density / density) for fibre, '
    'or mass - pi/4 * (dvol_dnom * d)**2 * water_density for wire [kg/m]. '
    'Unprovided polynomial coefficients are zero, per loadLineProps.'
)


def properties(diameter, coefficients, source):
    """Evaluate only licensed, non-standard-derived property coefficients."""
    d = diameter / source['mm_per_m']
    mass = coefficients['mass_d2'] * d**2
    mbl = coefficients['MBL_d2'] * d**2 + coefficients.get('MBL_d3', 0) * d**3
    stiffness = (coefficients.get('EA_d2', 0) * d**2
                 + coefficients.get('EA_MBL', 0) * mbl)
    rho = source['water_density']
    if 'density' in coefficients:
        wet = mass * (1 - rho / coefficients['density'])
    else:
        wet = mass - math.pi / 4 * (coefficients['dvol_dnom'] * d)**2 * rho
    return {'diameter': diameter, 'mbl': mbl / source['n_per_kn'],
            'stiffness': stiffness / source['n_per_kn'],
            'weight_air': mass, 'weight_water': wet}


def generate(output_dir=DATA):
    """Emit three CSVs and their byte-bound provenance manifest."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    source = json.loads(SOURCE.read_text())
    if output_dir.resolve() != DATA.resolve():
        (output_dir / 'sources').mkdir(exist_ok=True)
        for name in ['moorpy_coefficients.json', 'MoorPy-LICENSE.txt']:
            shutil.copyfile(DATA / 'sources' / name, output_dir / 'sources' / name)
    provenance = {
        'schema_version': 1, 'owner_repo': 'digitalmodel', 'units': UNITS,
        'intended_use': 'offline coefficient examples; not certified component selection',
        'readiness': 'partial', 'sources': {'moorpy': source['source']},
        'excerpt_sha256': sha256(SOURCE), 'rows': {}, 'tables': {},
        'excluded_sources': source['excluded_sources'],
        'retained_large_discovery_file': source['retained_large_discovery_file'],
        'diameter_source': source['diameter_source'],
        'schema': {kind: {'columns': columns, 'identity': IDENTITY_FIELDS[kind]}
                   for kind, columns in TABLE_COLUMNS.items()},
    }
    for kind, columns in TABLE_COLUMNS.items():
        rows, records = [], []
        for family, coefficients in source.get(kind, {}).items():
            for diameter in source[f'{kind}_diameters_mm']:
                row = properties(diameter, coefficients, source)
                row.update({'construction': family, 'grade': 'not specified'}
                           if kind == 'wire' else {'material': family})
                rows.append(row)
                records.append({
                    'identity': row_identity(kind, row), 'source': 'moorpy',
                    'kind': 'computed', 'formula': FORMULA,
                    'coefficients': coefficients,
                    'water_density_kg_m3': source['water_density'],
                    'mm_per_m': source['mm_per_m'], 'n_per_kn': source['n_per_kn'],
                    'valid_diameter_range': 'not established; fixture sampling only',
                })
        path = output_dir / f'{kind}_properties.csv'
        with path.open('w', newline='', encoding='utf-8') as stream:
            writer = csv.DictWriter(stream, fieldnames=columns, lineterminator='\n')
            writer.writeheader()
            writer.writerows(rows)
        provenance['rows'][kind] = records
        provenance['tables'][kind] = {'sha256': sha256(path), 'row_count': len(rows)}
    (output_dir / 'provenance.json').write_text(
        json.dumps(provenance, indent=2, allow_nan=False) + '\n', encoding='utf-8')


if __name__ == '__main__':
    generate()
    (ROOT / 'MANIFEST.in').write_text(
        'recursive-include src/digitalmodel/data/marine_engineering/mooring_components '
        '*.csv *.json *.html *.txt\n', encoding='utf-8')
