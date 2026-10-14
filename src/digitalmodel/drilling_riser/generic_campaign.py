# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Prepare a generic connected riser offset sweep without a licensed solver.

Input ownership stays with the generic specification. The analytical outputs
are approximate riser-column screens, not assembly acceptance calculations.
"""
from __future__ import annotations

import argparse
import copy
import csv
import hashlib
import json
import math
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.drilling_riser.operability import watch_circle_radius_m
from digitalmodel.drilling_riser.riser_response import solve_static_response
from digitalmodel.drilling_riser.section import section_properties
from digitalmodel.drilling_riser.stackup import top_tension_required

SPEC = Path('docs/domains/orcaflex/library/templates/drilling_riser/spec.yml')
DEST = Path('docs/domains/orcaflex/library/campaigns/generic-drilling-riser')
METHOD = ('vessel offset applied as a static mean position; no prescribed vessel '
          'motion; wave-frequency response is outside this sweep')
ASSUMED = 'assumed, based on previous experience'


def digest(path: Path) -> str:
    """Hash the exact bytes of an artifact."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def source_digest(path: Path) -> str:
    """Hash canonical UTF-8/LF source text, independent of checkout CRLF."""
    return hashlib.sha256(path.read_text(encoding='utf-8').encode('utf-8')).hexdigest()


def calculation_fingerprints(directory: Path | None = None) -> dict[str, str]:
    """Bind the screen adapter and every imported numerical implementation."""
    folder = directory or Path(__file__).parent
    names = ('generic_campaign.py', 'operability.py', 'riser_response.py', 'section.py', 'stackup.py')
    return {name: source_digest(folder / name) for name in names}


def write_text(path: Path, text: str) -> None:
    """Write a portable UTF-8 artifact with LF line endings."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text.rstrip('\n') + '\n', encoding='utf-8', newline='\n')


def tagged(value: Any, source: str = ASSUMED, unit: str = '1') -> dict[str, Any]:
    """Attach the unit and source to a value."""
    return {'value': value, 'unit': unit, 'source': source}


def read_spec(root: Path) -> dict:
    """Read the owning generic specification."""
    return dict(yaml.safe_load((root / SPEC).read_text(encoding='utf-8')))


def sourced_tree(node: Any, value: Any) -> Any:
    """Preserve every source leaf and its actual YAML source line."""
    if isinstance(node, yaml.MappingNode):
        return {key.value: sourced_tree(child, value[key.value]) for key, child in node.value}
    if isinstance(node, yaml.SequenceNode):
        return [sourced_tree(child, item) for child, item in zip(node.value, value)]
    return tagged(value, f'{SPEC.as_posix()}:{node.start_mark.line + 1}', 'as specified')


def column_basis(root: Path) -> dict:
    """Calculate the approximate main-column weight and section properties."""
    spec = read_spec(root)
    pipe = spec['generic']['line_types'][0]
    line = spec['generic']['lines'][0]
    od, bore = pipe['outer_diameter'], pipe['inner_diameter']
    length = line['properties']['LineType, Length, TargetSegmentLength'][0][1]
    section = section_properties(od, bore, float(pipe['properties']['E']) * 1000,
                                 pipe['properties']['PoissonRatio'])
    # Hollow-section area: section.py. Densities: the source specification.
    mass = (section.area * pipe['properties']['MaterialDensity']
            + math.pi / 4 * bore**2 * line['properties']['ContentsDensity']
            - math.pi / 4 * od**2 * spec['environment']['water']['density'])
    # Gravity is an explicit engineering assumption, not a standards value.
    weight_kn = mass * length * 9.81
    return {'od': od, 'length': length, 'section': section, 'weight_kn': weight_kn,
            'min_tension_kn': top_tension_required(weight_kn, 1.25),
            'rho': spec['environment']['water']['density'] * 1000,
            'cd': pipe['properties']['Cdn']}


def case_table(root: Path) -> dict:
    """Define the fixed sourced grid and explicitly assumed criteria."""
    spec = read_spec(root)
    text = (root / SPEC).read_text(encoding='utf-8')
    source = sourced_tree(yaml.compose(text), spec)
    depth = spec['environment']['water']['depth']
    basis = column_basis(root)
    cases = []
    for offset in range(6):
        for index, speed in enumerate((0, 0.5, 1.0, 1.5)):
            cases.append({
                'id': f'o{offset}-c{index}',
                'offset_pct': tagged(offset, unit='%'),
                'offset_m': tagged(watch_circle_radius_m(offset, depth),
                    f'{ASSUMED}; offset_pct times water depth / 100', 'm'),
                'current_mps': tagged(speed, unit='m/s'),
                'current_direction_deg': source['environment']['current']['direction'],
                'current_profile': tagged([[0, 1], [depth, 1]], unit='m, 1'),
                'wave_height_m': source['environment']['waves']['height'],
                'wave_period_s': source['environment']['waves']['period'],
                'stages_s': tagged(spec['simulation']['stages'], f'{SPEC.as_posix()}:54', 's'),
            })
    return {
        'schema_version': 1,
        'method': METHOD,
        'template_sha256': source_digest(root / SPEC),
        'template_digest_basis': 'UTF-8 source text normalized to LF',
        'basis': {key: source[key] for key in ('environment', 'simulation', 'generic')},
        'profile_description': 'Uniform current from surface to seabed; assumed, based on previous experience',
        'criteria': {
            'mean_angle_deg': tagged(2.0, unit='degrees'),
            'max_angle_deg': tagged(4.0, unit='degrees'),
            'min_top_tension_kn': tagged(basis['min_tension_kn'],
                f'{ASSUMED}; 1.25 times approximate riser-column submerged weight; '
                'src/digitalmodel/drilling_riser/stackup.py:28', 'kN'),
            'stroke_min_m': tagged(-5.0, unit='m'),
            'stroke_max_m': tagged(5.0, unit='m'),
            'stress_limit_mpa': tagged(448.0 * (2 / 3), unit='MPa'),
        },
        'assumptions': {
            'gravity_mps2': tagged(9.81, unit='m/s²'),
            'top_tension_factor': tagged(1.25),
            'yield_strength_mpa': tagged(448.0, unit='MPa'),
            'stress_design_factor': tagged(2 / 3),
            'stroke_reference': tagged('vertical ring displacement from initial elevation relative to fixed vessel'),
            'stroke_capacity_basis': tagged('independent assumed range; model line length is not equipment capacity'),
        },
        'screen_limitations': 'Approximate riser-column screen; constant tension, uniform drag, pinned ends; '
            'attached members, nonlinear tensioner response, pressure stress and wave loading omitted. '
            'Geometric extension is not mechanical tensioner stroke. No assembly acceptance is established.',
        'cases': cases,
    }


def validate_model(folder: Path) -> None:
    """Check generated YAML with both licence-free validators."""
    from digitalmodel.solvers.orcaflex.yaml_validator import OrcaFlexYAMLValidator
    from digitalmodel.solvers.orcaflex.modular_generator.post_validator import PostGenerationValidator
    result = OrcaFlexYAMLValidator().validate_directory(folder)
    findings = PostGenerationValidator().validate_directory(folder / 'includes')
    errors = [str(issue) for issue in result.errors]
    errors.extend(item.message for item in findings if item.level == 'error')
    if errors:
        raise ValueError('; '.join(errors))


def build_models(root: Path, destination: Path, table: dict) -> None:
    """Generate and validate every case without importing a solver interface."""
    from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
    if source_digest(root / SPEC) != table['template_sha256']:
        raise ValueError('Source specification digest mismatch')
    if table != case_table(root):
        raise ValueError('Unsupported case table; use the fixed sourced sweep')
    for case in table['cases']:
        spec = copy.deepcopy(read_spec(root))
        vessel = spec['generic']['vessels'][0]
        vessel['initial_position'] = [-case['offset_m']['value'], 0, 0]
        props = vessel['properties']
        for key in list(props):
            if key.startswith('PrescribedMotion'):
                del props[key]
        props['PrimaryMotion'] = 'None'
        props['SuperimposedMotion'] = 'None'
        spec['environment']['current'].update(
            speed=case['current_mps']['value'], profile=case['current_profile']['value'])
        folder = destination / case['id']
        ModularModelGenerator.from_spec(ProjectInputSpec(**spec)).generate(folder)
        # Normalize only newly generated files; existing generator remains unchanged.
        for path in folder.rglob('*.yml'):
            text = path.read_text(encoding='utf-8')
            if path.name == 'master.yml':
                text = text.replace('# Type: Model', '# Type: Model\n# ' + METHOD)
            write_text(path, text)
        validate_model(folder)


def screen_rows(root: Path, table: dict) -> list[dict]:
    """Calculate approximate static column responses for the case grid."""
    basis = column_basis(root)
    rows = []
    for case in table['cases']:
        offset, speed = case['offset_m']['value'], case['current_mps']['value']
        tension = basis['min_tension_kn'] * 1000
        response = solve_static_response(length_m=basis['length'], top_offset_m=offset,
            tension_n=tension, ei_nm2=basis['section'].bending_stiffness,
            current_load_n_per_m=0.5 * basis['rho'] * basis['cd'] * basis['od'] * speed**2)
        stress = (tension / basis['section'].area + response.max_bending_moment_nm
                  * basis['od'] / 2 / basis['section'].moment_of_inertia) / 1e6
        rows.append({'case_id': case['id'], 'offset_pct': case['offset_pct']['value'],
            'current_mps': speed, 'submerged_weight_kn': basis['weight_kn'],
            'required_top_tension_kn': basis['min_tension_kn'],
            'lower_angle_deg': abs(response.angle_lower_deg),
            'upper_angle_deg': abs(response.angle_upper_deg),
            'axial_plus_bending_stress_mpa': stress,
            'geometric_extension_m': math.hypot(basis['length'], offset) - basis['length'],
            'scope': 'approximate riser-column screen'})
    return rows


def prepare(root: Path, destination: Path) -> None:
    """Write the sourced case table, generated models and analytical screens."""
    table = case_table(root)
    write_text(destination / 'case-table.yml', yaml.safe_dump(table, sort_keys=False, allow_unicode=True))
    build_models(root, destination / 'models', table)
    rows = screen_rows(root, table)
    path = destination / 'screens/column-screen.csv'
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open('w', encoding='utf-8', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]), lineterminator='\n')
        writer.writeheader()
        writer.writerows(rows)
    write_text(destination / 'screens/basis.json', json.dumps({
        'template_sha256': table['template_sha256'], 'limitations': table['screen_limitations'],
        'case_table_sha256': digest(destination / 'case-table.yml'),
        'screen_csv_sha256': digest(path),
        'calculation_fingerprints': calculation_fingerprints(),
        'criteria': table['criteria'], 'assumptions': table['assumptions'],
        'units': 'Column suffixes: kn = kN, mpa = MPa, m = m, deg = degrees, mps = m/s, pct = %',
    }, indent=2, ensure_ascii=False))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=Path.cwd())
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    prepare(args.root, args.output or args.root / DEST)
