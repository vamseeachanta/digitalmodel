"""Calculate traced diagnostic buoy hydrodynamics; never launch an analysis."""
from __future__ import annotations

import copy
from dataclasses import asdict
import hashlib
import json
import math
import re
from pathlib import Path

import yaml

from digitalmodel.citations.schema import Citation, validate_citation
from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader, orcaflex_dump


def _finite(value, name, positive=False):
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f'{name} must be numeric')
    if not math.isfinite(value) or (positive and value <= 0):
        raise ValueError(f'Invalid {name}')
    return value


def _validate_inputs(inputs, body, expected):
    required = ('Mass', 'Volume', 'Height', 'CentreOfMass', 'CentreOfVolume',
                'MomentsOfInertia')
    if not all(key in expected for key in required):
        raise ValueError('Expected source properties must pin solid/body properties')
    if any(body.get(key) != value for key, value in expected.items()):
        raise ValueError('Source property precondition mismatch')
    if (body.get('BuoyType') != 'Lumped buoy'
            or body.get('LumpedBuoyAddedMassMethod') != 'Diagonal values'):
        raise ValueError('Only diagonal lumped-buoy inputs are supported')
    for key in ('m_air', 'l', 'w', 'h'):
        _finite(inputs[key], key, positive=True)
    _finite(inputs['m_water'], 'm_water')
    if inputs['m_air'] <= inputs['m_water']:
        raise ValueError('Displaced mass must be positive')
    if inputs['m_air'] / 1000 != body['Mass']:
        raise ValueError('Calculated and source dry mass differ')
    if abs(inputs['h'] - body['Height']) > .001:
        raise ValueError('Calculated and source height differ beyond 1 mm rounding')
    for axis in 'xyz':
        value = _finite(inputs['perforation_ratio'][axis], 'perforation')
        if not 0 <= value <= .5:
            raise ValueError('Perforation ratio must be in [0,0.5] correction scope')
        for field in ('cog', 'cov'):
            _finite(inputs[field][axis], field)



def _basis_record(source, config):
    reference = config.get('geometry_basis', {})
    if not isinstance(reference, dict) or not reference.get('path'):
        raise ValueError('Pinned geometry basis required')
    path = (source.parent / reference['path']).resolve(strict=True)
    raw = path.read_bytes()
    digest = hashlib.sha256(raw).hexdigest()
    if digest != reference.get('sha256'):
        raise ValueError('Geometry basis digest mismatch')
    record = json.loads(raw)
    if (record.get('schema_version') != 1 or record.get('units') != 'm'
            or record.get('status') not in ('project_assumption', 'source_evidence')):
        raise ValueError('Unsupported geometry basis schema, units or status')
    provenance = record.get('provenance')
    if not isinstance(provenance, list) or not provenance:
        raise ValueError('Geometry source provenance required')
    for item in provenance:
        value = item.get('sha256', '')
        if (len(value) != 64 or any(c not in '0123456789abcdef' for c in value)
                or not item.get('source_id') or not item.get('locator')):
            raise ValueError('Geometry source digest and locator required')
    return path, {'sha256': digest, 'record': record}


def _validate_geometry(inputs, body, record):
    tolerance = _finite(record.get('rounding_tolerance_m'), 'rounding tolerance', True)
    if tolerance > .001:
        raise ValueError('Geometry rounding tolerance exceeds 1 mm')
    if not all(isinstance(record.get(k), str) and record[k].strip()
               for k in ('input_datum', 'model_datum')):
        raise ValueError('Input and model datum declarations required')
    offset = record.get('translation_m')
    if not isinstance(offset, list) or len(offset) != 3:
        raise ValueError('Three-component datum translation required')
    for value in offset:
        _finite(value, 'datum translation')
    dimensions = record.get('dimensions_m', {})
    for key in ('l', 'w', 'h'):
        value = _finite(dimensions.get(key), 'sourced dimension', True)
        if not math.isclose(inputs[key], value, rel_tol=0, abs_tol=1e-12):
            raise ValueError('Input dimension differs from pinned geometry basis')
    centres = record.get('centres_m', {})
    for field, native in [('cog', 'CentreOfMass'), ('cov', 'CentreOfVolume')]:
        for i, axis in enumerate('xyz'):
            value = _finite(centres.get(field, {}).get(axis), 'sourced centre')
            if not math.isclose(inputs[field][axis], value, rel_tol=0, abs_tol=1e-12):
                raise ValueError('Input centre differs from pinned geometry basis')
            target = _finite(body[native][i], 'model centre')
            if abs(value + offset[i] - target) > tolerance:
                raise ValueError('Transformed centre differs from source model')


def _calculate(inputs):
    import inspect
    from digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_rectangular import (
        DNVRPH103_hydrodynamics_rectangular,
    )
    lookup = Path(__file__).resolve().parents[1] / 'base_configs/modules/code_dnvrph103/code_dnvrph103.yml'
    raw = lookup.read_bytes()
    cfg = yaml.safe_load(raw)
    cfg['inputs'] = copy.deepcopy(inputs)
    calculator = DNVRPH103_hydrodynamics_rectangular()
    calculator.cfg = cfg
    properties = {'translational': calculator.get_translational_properties()}
    properties['translational']['cd'].pop('splash')
    code = Path(inspect.getfile(DNVRPH103_hydrodynamics_rectangular)).read_bytes()
    return properties, cfg['rho_water'], hashlib.sha256(raw).hexdigest(), hashlib.sha256(code).hexdigest()


def _citations(config, wiki_root):
    citations = [Citation(**item) for item in config.get('citations', [])]
    if not citations or any((c.code_id, c.publisher, c.revision)
                            != ('dnv-rp-h103', 'DNV', '2011') for c in citations):
        raise ValueError('Calculation requires DNV-RP-H103 2011 citations')
    sections = set(re.findall(r'(?<![\w.])(?:[AB]-\d+|\d+(?:\.\d+)+)(?![\w.])',
                              ';'.join(c.section for c in citations)))
    if not {'A-2', 'B-2', '4.6.3.3', '4.6.4.1'} <= sections:
        raise ValueError('Missing citation coverage for a calculation component')
    for citation in citations:
        validate_citation(citation, repo_root=wiki_root)
    return citations


def _hydro_fields(properties, displaced_mass_t):
    trans = properties['translational']
    vector = lambda values: [float(values[axis]) for axis in 'xyz']
    fields = {'DragArea': vector(trans['area_drag']),
              'DragForceCoefficient': vector(trans['cd']['deep']),
              'AddedMassCoefficient': vector(trans['ca']),
              'HydrodynamicMass': [displaced_mass_t] * 3}
    for field, values in fields.items():
        for value in values:
            _finite(value, field)
            if value < 0:
                raise ValueError(f'Negative calculated {field}')
    return fields


def _model(source_bytes, config):
    try:
        text = source_bytes.decode('utf-8-sig')
    except UnicodeDecodeError as error:
        raise ValueError('Source model requires UTF-8 encoding (optional BOM)') from error
    model = yaml.load(text, Loader=OrcaFlexLoader)
    if model.get('General', {}).get('UnitsSystem') != 'SI':
        raise ValueError('Only SI source models are supported')
    bodies = model.get('6DBuoys', [])
    matches = [i for i, body in enumerate(bodies)
               if body.get('Name') == config['body_name']]
    if len(matches) != 1:
        raise ValueError('Expected one named 6D buoy')
    index = matches[0]
    _validate_inputs(config['inputs'], bodies[index], config['expected_source_properties'])
    return model, index


def _apply(before, index, fields):
    after = copy.deepcopy(before)
    body = after['6DBuoys'][index]
    diff = [{'field': key, 'present_before': key in body,
             'before': body.get(key), 'after': value} for key, value in fields.items()]
    body.update(fields)
    restored = copy.deepcopy(after)
    for entry in diff:
        target = restored['6DBuoys'][index]
        if entry['present_before']:
            target[entry['field']] = entry['before']
        else:
            target.pop(entry['field'])
    if restored != before:
        raise ValueError('Change outside hydrodynamic allowlist')
    return after, diff


def _write(output, after, receipt):
    output.mkdir(parents=True, exist_ok=False)
    target = output / 'master.yml'
    orcaflex_dump(after, target)
    if yaml.load(target.read_bytes(), Loader=OrcaFlexLoader) != after:
        raise ValueError('Model semantic readback mismatch')
    receipt['master_sha256'] = hashlib.sha256(target.read_bytes()).hexdigest()
    serialized = json.dumps(receipt, indent=2, allow_nan=False) + '\n'
    metadata = output / 'properties.json'
    metadata.write_text(serialized, encoding='utf-8')
    if json.loads(metadata.read_text(encoding='utf-8')) != receipt:
        raise ValueError('Property receipt readback mismatch')
    return receipt


def _unchanged(path, digest, label):
    if hashlib.sha256(path.read_bytes()).hexdigest() != digest:
        raise ValueError(f'{label} changed during preparation')


def build_candidate(source, expectedsha, output, config, *, wiki_root):
    """Calculate translation; citation/basis checks do not establish qualification."""
    if wiki_root is None:
        raise ValueError('Explicit wiki_root required')
    source, output = Path(source).resolve(strict=True), Path(output).absolute()
    if output.exists():
        raise FileExistsError(output)
    raw = source.read_bytes()
    if hashlib.sha256(raw).hexdigest() != expectedsha:
        raise ValueError('Source digest mismatch')
    before, index = _model(raw, config)
    basis_path, basis = _basis_record(source, config)
    _validate_geometry(config['inputs'], before['6DBuoys'][index], basis['record'])
    citations = _citations(config, wiki_root)
    props, rho, lookupsha, codesha = _calculate(config['inputs'])
    density = _finite(before.get('Environment', {}).get('Density'), 'source density', True)
    if not math.isclose(density * 1000, rho, rel_tol=1e-12):
        raise ValueError('Source density differs from calculation density')
    mass_kg = config['inputs']['m_air'] - config['inputs']['m_water']
    after, diff = _apply(before, index, _hydro_fields(props, mass_kg / 1000))
    receipt = {'schema_version': 1, 'state': 'diagnostic_candidate_not_run',
               'analysis_executed': False, 'acceptance_established': False,
               'source_sha256': expectedsha, 'geometry_basis': basis, 'lookup_sha256': lookupsha,
               'legacy_calculator_sha256': codesha, 'rho_water_kg_m3': rho,
               'units': {'added_mass': 'kg', 'area_drag': 'm2', 'ca': 'dimensionless',
                         'cd': 'dimensionless', 'HydrodynamicMass': 't'},
               'config_sha256': hashlib.sha256(json.dumps(config, sort_keys=True,
                   allow_nan=False).encode()).hexdigest(),
               'workflow_sha256': hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
               'inputs': config['inputs'], 'body_name': config['body_name'],
               'source_solid_inertia_preserved_unqualified': True,
               'source_displaced_volume_m3': before['6DBuoys'][index]['Volume'],
               'calculated_displaced_volume_m3': mass_kg / rho,
               'hydrodynamic_reference_mass_kg': mass_kg,
               'source_rotational_hydrodynamics_preserved_unqualified': True,
               'limitations': [
                   'Legacy lookup interpolation includes approximated plate table points.',
                   'Plate drag ratio 50 represents the finite approximation of an infinite plate.',
                   'Heave correction is permuted across axes by the legacy calculator.',
                   'Citation resolution does not establish full standards conformance.',
                   'Plan dimensions require source review; native wireframe is not a design drawing.',
                   'Deep-zone coefficients only; calculated splash values are not applied.',
                   'Comparison to an N103 workbook crosses editions and is not an exact standard reproduction.',
                   'Source volume is retained; explicit hydrodynamic mass uses dry minus wet mass.',
               ],
               'properties': props, 'semantic_diff': diff,
               'citations': [asdict(citation) for citation in citations]}
    _unchanged(source, expectedsha, 'Source')
    _unchanged(basis_path, basis['sha256'], 'Geometry basis')
    return _write(output, after, receipt)


def main(argv=None):
    """Run property calculation and new-model preparation, without a solver."""
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, required=True)
    parser.add_argument('--config', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--source-sha256', required=True)
    parser.add_argument('--wiki-root', type=Path, required=True)
    args = parser.parse_args(argv)
    cfg = yaml.safe_load(args.config.read_bytes())
    result = build_candidate(args.source, args.source_sha256, args.output, cfg,
                             wiki_root=args.wiki_root)
    print(json.dumps({'state': result['state'], 'master_sha256': result['master_sha256']}))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
