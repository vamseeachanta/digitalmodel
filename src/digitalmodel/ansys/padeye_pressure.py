"""Preparation-only native pressure probe; deliberately has no solution path."""
import hashlib
import json
from pathlib import Path

from digitalmodel.ansys.padeye_pressure_mesh import (
    MESH_LEVELS, build_pressure_mesh, pressure_geometry,
)


def generate_pressure_preparation(geometry):
    """Emit explicit mapped blocks and a native database pressure snapshot."""
    mesh = build_pressure_mesh(geometry)
    lines = [
        '! Prescribed-pressure preparation only; NO STRESS QUALIFICATION',
        'FINISH', '/CLEAR,NOSTART', '/TITLE,Padeye pressure preparation',
        '/UNITS,MPA', '/PREP7', 'SHPP,DEFAULT', 'SHPP,ON',
        'ET,1,PLANE182', 'KEYOPT,1,3,3', 'R,1,8',
        'MP,EX,1,205000', 'MP,PRXY,1,0.3', 'TYPE,1', 'MAT,1', 'REAL,1',
    ]
    lines.extend(f"N,{n['id']},{n['x_mm']:.16g},{n['y_mm']:.16g},0"
                 for n in mesh['nodes'])
    lines.extend('EN,'+str(e['id'])+','+','.join(map(str, e['nodes']))
                 for e in mesh['elements'])
    lines += ['NSEL,S,LOC,Y,0', 'D,ALL,ALL,0', 'ALLSEL,ALL', 'CSYS,0',
              f"SFGRAD,PRES,0,Y,220,{mesh['pressure_slope_mpa_per_mm']:.16g}"]
    lines.extend(f"SFE,{row['element']},4,PRES,1,0,0,0,0"
                 for row in mesh['pressures'])
    lines += ['SFGRAD', 'ALLSEL,ALL', 'CHECK', 'SHPP,STATUS', 'SHPP,SUMMARY',
              '/OUTPUT,pressure_faces,txt', 'SFELIST,ALL,PRES', '/OUTPUT',
              'CDWRITE,DB,pressure_native,cdb', 'FINISH', '/EXIT,NOSAVE']
    return '\n'.join(lines)+'\n'


def prepare_pressure_study(output_dir):
    """Create fresh, separate input/intent bundles with explicit LF encoding."""
    root = Path(output_dir)
    paths = tuple(root / f'mesh-{size:g}mm' / 'pressure_prepare.inp'
                  for size in MESH_LEVELS)
    if any(path.parent.exists() for path in paths):
        raise FileExistsError('pressure-study destination already exists')
    for size, path in zip(MESH_LEVELS, paths):
        geom = pressure_geometry(size)
        content = generate_pressure_preparation(geom).encode('utf-8')
        intent = build_pressure_mesh(geom)
        intent['input_sha256'] = hashlib.sha256(content).hexdigest()
        path.parent.mkdir(parents=True, exist_ok=False)
        path.write_bytes(content)
        manifest = path.with_name('pressure_intent.json')
        manifest.write_text(json.dumps(intent, indent=2, allow_nan=False)+'\n',
                            encoding='utf-8', newline='\n')
        if path.read_bytes() != content or json.loads(manifest.read_text()) != intent:
            raise OSError('prepared pressure bundle failed readback')
    return paths
