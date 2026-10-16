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
          'motion; no wave is applied: this sweep covers vessel offset and current only')
ASSUMED = 'assumed, based on previous experience'
REFERENCES = {
    '16Q': 'API RP 16Q, Design, Selection, Operation and Maintenance of Marine Drilling '
           'Riser Systems, second edition, April 2017',
    '5L': 'API Spec 5L, Line Pipe, 46th edition, April 2018',
    '2RD': 'API RP 2RD, Design of Risers for Floating Production Systems (FPSs) and '
           'Tension-Leg Platforms (TLPs), first edition, June 1998; historical background only, '
           'retained to match the cited public errata; no acceptance criterion uses this edition',
}
RUNTIME_UNITS = {'mean_angle_deg': 'degrees', 'max_angle_deg': 'degrees',
                 'yield_strength_mpa': 'MPa', 'stress_design_factor': '1'}


def horizontal_shift(offset: float, direction: float) -> list[float]:
    """Resolve offset along current bearing by Cartesian sine/cosine projection."""
    angle = math.radians(direction)
    return [round(offset * math.cos(angle), 10) + 0.0, round(offset * math.sin(angle), 10) + 0.0]


def load_criteria(path: Path | None, table: dict) -> tuple[dict, str]:
    """Require externally supplied criteria; never infer licensed numerical values.

    Stress limit = supplied yield strength times supplied design factor. This
    is an assumed uniaxial screening criterion, not a code compliance equation.
    The external file is private; only its form and bibliographic references
    belong with the prepared inputs. Units must match the named quantities.
    Identity hashes the parsed UTF-8 snapshot with normalized LF newlines.
    """
    if path is None:
        raise ValueError('An external criteria file is required')
    snapshot = path.read_text(encoding='utf-8')
    data = json.loads(snapshot)
    if not isinstance(data, dict) or set(data) != set(RUNTIME_UNITS):
        raise ValueError('Unexpected runtime criteria fields')
    for key, unit in RUNTIME_UNITS.items():
        item = data[key]
        if not isinstance(item, dict):
            raise ValueError('Criteria entries must be sourced quantities')
        value = item.get('value')
        if (isinstance(value, bool) or not isinstance(value, (int, float))
                or not math.isfinite(value) or value <= 0):
            raise ValueError('Criteria must be finite and positive')
        if item.get('unit') != unit or not isinstance(item.get('source'), str) or not item['source'].strip():
            raise ValueError('Criteria require the expected unit and a source')
    if data['mean_angle_deg']['value'] > data['max_angle_deg']['value']:
        raise ValueError('Mean angle criterion exceeds maximum')
    if data['stress_design_factor']['value'] > 1:
        raise ValueError('Stress design factor exceeds unity')
    criteria = copy.deepcopy(table['criteria'])
    for key in ('mean_angle_deg', 'max_angle_deg'):
        criteria[key] = data[key]
    strength, factor = data['yield_strength_mpa'], data['stress_design_factor']
    product = strength['value'] * factor['value']
    if not math.isfinite(product) or product <= 0:
        raise ValueError('Derived stress limit must be finite and positive')
    criteria['stress_limit_mpa'] = tagged(product,
        'assumed uniaxial criterion: supplied yield strength times supplied design factor; '
        + strength['source'] + '; ' + factor['source'], 'MPa')
    return criteria, hashlib.sha256(snapshot.encode('utf-8')).hexdigest()


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
                'wave_applied': tagged(False, f'{SPEC.as_posix()}:37-41; specification wave is not applied; '
                    'offset and current only by scope'),
                'ring_initial_position_m': tagged(
                    horizontal_shift(watch_circle_radius_m(offset, depth), spec['environment']['current']['direction'])
                    + [next(b for b in spec['generic']['buoys_6d'] if b['name'] == 'Tensioner Ring')['initial_position'][2]],
                    'assumed; same horizontal shift as vessel, offset_m times cosine/sine of current direction; '
                    'initial elevation retained from specification', 'm'),
                'stages_s': tagged(spec['simulation']['stages'], f'{SPEC.as_posix()}:55-57', 's'),
            })
    return {
        'schema_version': 1,
        'method': METHOD,
        'template_sha256': source_digest(root / SPEC),
        'template_digest_basis': 'UTF-8 source text normalized to LF',
        'line_contact_source': 'In-repository program example specification: '
            'docs/domains/orcaflex/library/model_library/b01_drilling_riser/spec.yml; '
            'generic.line_contact_data copied unchanged; native statics not established',
        'basis': {key: source[key] for key in ('environment', 'simulation', 'generic')},
        'wave_basis_note': 'Specification wave data are retained for provenance only; no wave is applied.',
        'profile_description': 'Uniform current from surface to seabed; assumed, based on previous experience',
        'tension_acceptance_basis': (
            'The stored minimum top tension is retained as an assumed criterion. '
            f"The minimum remains {basis['min_tension_kn']:,.1f} kN. "
            'Native pre-flight statics on the earlier models, reported on #2469, give the main-riser '
            'top effective tension below this criterion in every converged case; '
            'numerical reconciliation remains pending access to the diagnostic records. '
            'The four links share their vertical load between the riser, choke and kill lines '
            'and the slip joint; total tensioner force is not main-riser top effective tension. '
            'No operating window is established. Requalify the staged method case by case '
            'before production; the prior pre-flight does not qualify the changed method.'),
        'statics_method': tagged(
            'At the specified vessel offset, solve zero-current statics, retain calculated positions, '
            'then solve at the specified current. Zero-current cases solve once. '
            'Both stages must reach InStaticState before dynamics. Native convergence remains unverified.',
            'src/digitalmodel/drilling_riser/campaign.py::_statics_path; '
            'scripts/orcaflex/run_generic_drilling_riser_campaign.py::calculate_staged_statics'),
        'drill_string_current_basis': tagged(
            'Normal drag is retained from the source specification. Shielding by the containing riser '
            'has not been established by native readback; no drag reduction is applied. '
            'The contribution to contact loads and angles requires qualification.',
            f'{SPEC.as_posix()}; generic.line_types and generic.line_contact_data'),
        'criteria': {
            'mean_angle_deg': tagged(None, 'runtime criteria file; ' + REFERENCES['16Q'], 'degrees'),
            'max_angle_deg': tagged(None, 'runtime criteria file; ' + REFERENCES['16Q'], 'degrees'),
            'min_top_tension_kn': tagged(basis['min_tension_kn'],
                f'{ASSUMED}; 1.25 times approximate riser-column submerged weight; '
                'src/digitalmodel/drilling_riser/stackup.py:28', 'kN'),
            'stroke_min_m': tagged(-5.0, unit='m'),
            'stroke_max_m': tagged(5.0, unit='m'),
            'stress_limit_mpa': tagged(None, 'runtime yield strength times design factor; ' + REFERENCES['16Q'], 'MPa'),
        },
        'assumptions': {
            'gravity_mps2': tagged(9.81, unit='m/s²'),
            'top_tension_factor': tagged(1.25),
            'yield_strength_mpa': tagged(None, 'runtime criteria file; ' + REFERENCES['5L'], 'MPa'),
            'stress_design_factor': tagged(None, 'runtime criteria file; ' + REFERENCES['16Q']),
            'stroke_reference': tagged('vertical ring displacement from initial elevation relative to fixed vessel'),
            'stroke_capacity_basis': tagged('independent assumed range; model line length is not equipment capacity'),
        },
        'screen_limitations': 'Approximate riser-column screen; constant assumed top tension, uniform drag, pinned ends; '
            'lower joint angle is not established because effective tension varies along the column. '
            'Both joint-angle comparisons are omitted: constant tension can introduce an error of about a factor of two. '
            'Weight treats the above-water 20 m as submerged, underestimating column weight by about 1.0 %. '
            'Stress retains the constant-tension approximation. '
            'Screen tension is an assumed criterion value, not the modelled tension. '
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
    for case in table['cases']:
        for key in case:
            if key.startswith('PrescribedMotion'):
                raise ValueError(f'Unsupported vessel motion: {key}')
    if table != case_table(root):
        raise ValueError('Unsupported case table; use the fixed sourced sweep')
    for case in table['cases']:
        spec = copy.deepcopy(read_spec(root))
        vessel = spec['generic']['vessels'][0]
        shift = horizontal_shift(case['offset_m']['value'], case['current_direction_deg']['value'])
        vessel['initial_position'] = shift + [0]
        ring = next(b for b in spec['generic']['buoys_6d'] if b['name'] == 'Tensioner Ring')
        ring['initial_position'] = case['ring_initial_position_m']['value']
        drill = next(line for line in spec['generic']['lines'] if line['name'] == 'Drill String')
        connections = next(value for key, value in drill['properties'].items()
                           if key.startswith('Connection, '))
        connections[0][1:3] = shift
        # Calm-water precedent: library/templates/installation_pull_in/spec.yml:35-36.
        spec['environment']['waves'].update(type='Airy', height=0)
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
                text = text.replace('# Type: Model', '# Type: Model\n# ' + METHOD
                    + '\n# Tensioner ring initial position (m): '
                    + str(case['ring_initial_position_m']['value'])
                    + '; assumed, same horizontal shift as vessel from offset and current direction; '
                    'initial elevation retained.')
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
            'axial_plus_bending_stress_mpa': stress,
            'geometric_extension_m': math.hypot(basis['length'], offset) - basis['length'],
            'scope': 'approximate riser-column screen'})
    return rows


def prepare(root: Path, destination: Path) -> None:
    """Write the sourced case table, generated models and analytical screens."""
    table = case_table(root)
    write_text(destination / 'case-table.yml', yaml.safe_dump(table, sort_keys=False, allow_unicode=True))
    write_text(destination / 'criteria-form.json', json.dumps({key: {
        'value': None, 'unit': unit, 'source': 'Supply a qualified source at run time; '
        + REFERENCES['5L' if key == 'yield_strength_mpa' else '16Q']}
        for key, unit in RUNTIME_UNITS.items()}, indent=2))
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
        'tension_acceptance_basis': table['tension_acceptance_basis'],
        'units': 'Column suffixes: kn = kN, mpa = MPa, m = m, deg = degrees, mps = m/s, pct = %',
    }, indent=2, ensure_ascii=False))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=Path.cwd())
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    prepare(args.root, args.output or args.root / DEST)
