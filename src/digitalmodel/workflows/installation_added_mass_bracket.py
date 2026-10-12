"""Prepare a pinned one-at-a-time vertical-added-mass bracket; never solve."""
import argparse
from hashlib import sha256
import json
import math
import os
from pathlib import Path

import yaml

from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader, orcaflex_dump
from digitalmodel.workflows.installation_seastates import _source, _settings, _change_payload, _json


def code_hashes():
    names = ('installation_added_mass_bracket.py', 'installation_seastates.py',
             'installation_added_mass_materialize.py', 'installation_trace_extract.py')
    return {name: sha256(Path(__file__).with_name(name).read_bytes()).hexdigest() for name in names}


def integration_basis(model, matrix):
    general, settings = model['General'], matrix['settings']
    fixed = general.get('ImplicitUseVariableTimeStep') in (False, 'No')
    declared = settings.get('fixed_time_step_s')
    if fixed != (declared is not None):
        raise ValueError('Matrix/source integration mode mismatch')
    if fixed and positive(general['ImplicitConstantTimeStep'], 'source timestep') != declared:
        raise ValueError('Matrix/source fixed timestep mismatch')


def read_pinned(path, digest):
    path = Path(path).resolve(strict=True)
    raw = path.read_bytes()
    if sha256(raw).hexdigest() != digest:
        raise ValueError('Input digest mismatch')
    return path, raw


def positive(value, label):
    if type(value) not in (int, float) or not math.isfinite(value) or value <= 0:
        raise ValueError(f'Positive finite numeric {label} required')
    return float(value)


def named_body(model, name):
    matches = [b for b in model.get('6DBuoys', []) if b.get('Name') == name]
    if len(matches) != 1:
        raise ValueError('Exactly one named buoy required')
    return matches[0]


def case_settings(matrix, index):
    common, case = matrix['settings'], matrix['cases'][index]
    names = ('buildup_s', 'duration_s', 'sample_interval_s', 'gamma', 'components', 'max_time_step_s')
    settings = _settings(case['hs_m'], case['tp_s'], case['seed'], *[common[k] for k in names])
    if 'fixed_time_step_s' in common:
        settings['fixed_time_step_s'] = positive(common['fixed_time_step_s'], 'fixed timestep')
        if settings['fixed_time_step_s'] > settings['max_time_step_s']:
            raise ValueError('Fixed timestep exceeds cap')
    settings['wave_reference'] = common['wave_reference']
    return settings


def _basis(model, receipt, name, baseline):
    if model.get('General', {}).get('UnitsSystem') != 'SI':
        raise ValueError('SI source required')
    body = named_body(model, name)
    if body.get('BuoyType') != 'Lumped buoy' or body.get('LumpedBuoyAddedMassMethod') != 'Diagonal values':
        raise ValueError('Diagonal lumped buoy required')
    if (receipt.get('body_name') != name or receipt.get('units', {}).get('added_mass') != 'kg'
            or receipt.get('units', {}).get('HydrodynamicMass') != 't'):
        raise ValueError('Property receipt body/units mismatch')
    mass = positive(receipt['properties']['translational']['added_mass']['z'], 'receipt mass') / 1000
    if not math.isclose(positive(baseline, 'baseline mass'), mass, rel_tol=1e-12, abs_tol=1e-12):
        raise ValueError('Declared baseline differs from property receipt')
    for field in ('AddedMassCoefficient', 'HydrodynamicMass'):
        if len(body[field]) != 3:
            raise ValueError('Three hydrodynamic axes required')
        for value in body[field]:
            positive(value, field)
    reference = positive(body['HydrodynamicMass'][2], 'reference mass')
    displaced = (receipt['inputs']['m_air'] - receipt['inputs']['m_water']) / 1000
    rho = model.get('Environment', {}).get('Density')
    if not math.isclose(positive(rho, 'density') * 1000, receipt['rho_water_kg_m3'], rel_tol=1e-12):
        raise ValueError('Source/receipt density mismatch')
    if not math.isclose(reference, displaced, rel_tol=1e-12) or not math.isclose(reference*1000, receipt['hydrodynamic_reference_mass_kg'], rel_tol=1e-12):
        raise ValueError('Source/receipt reference mass mismatch')
    ca = positive(receipt['properties']['translational']['ca']['z'], 'receipt CaZ')
    if not math.isclose(ca, mass/reference, rel_tol=1e-12):
        raise ValueError('Receipt mass/coefficient inconsistent')
    return body, ca


def expected_change(settings, wave_name, body_name, source_ca, coefficient_z):
    payload = _change_payload(settings, wave_name)
    # Named mapping edits the existing object; list syntax replaces it.
    payload['6DBuoys'] = {body_name: {'AddedMassCoefficient': list(source_ca[:2]) + [coefficient_z]}}
    return payload


def _selection(matrix, factors, indices):
    if not factors or len(set(factors)) != len(factors):
        raise ValueError('Unique nonempty factors required')
    for factor in factors:
        positive(factor, 'factor')
    if not indices or any(type(i) is not int or not 0 <= i < len(matrix['cases']) for i in indices):
        raise ValueError('Valid integer case indices required')
    if len(set(indices)) != len(indices):
        raise ValueError('Unique case indices required')


def _write_changes(output, matrix, factors, indices, body, ca_z, baseline, wave_name):
    rows = []
    for factor_index, factor in enumerate(factors):
        for index in indices:
            settings = case_settings(matrix, index)
            identity = f'case_{index:03d}_factor_{factor_index:02d}'
            payload = expected_change(settings, wave_name, body['Name'], body['AddedMassCoefficient'], factor*ca_z)
            path = output / 'changes' / f'{identity}.yml'
            orcaflex_dump(payload, path)
            if yaml.load(path.read_text(encoding='utf-8'), Loader=OrcaFlexLoader) != payload:
                raise ValueError('Change readback mismatch')
            rows.append(dict(id=identity, source_case_index=index, factor=factor, settings=settings,
                added_mass_t=factor*baseline, coefficient_z=factor*ca_z,
                change_file=path.relative_to(output).as_posix(), change_sha256=sha256(path.read_bytes()).hexdigest(),
                status='prepared_not_native_verified'))
    return rows


def prepare_bracket(source, source_sha256, matrix, matrix_sha256, receipt, receipt_sha256,
                    output, *, body_name, baseline_added_mass_t, factors, case_indices):
    output = Path(output).resolve()
    if output.exists():
        raise FileExistsError('New bracket output required')
    source = _source(source, source_sha256)
    paths, raws = {}, {}
    for name, path, digest in [('source', source, source_sha256), ('matrix', matrix, matrix_sha256), ('receipt', receipt, receipt_sha256)]:
        paths[name], raws[name] = read_pinned(path, digest)
    model = yaml.load(raws['source'].decode('utf-8-sig'), Loader=OrcaFlexLoader)
    matrix, receipt = json.loads(raws['matrix']), json.loads(raws['receipt'])
    if matrix.get('master_sha256') != source_sha256 or receipt.get('source_sha256') != source_sha256:
        raise ValueError('Matrix/property receipt source identity mismatch')
    _selection(matrix, factors, case_indices)
    integration_basis(model, matrix)
    body, ca_z = _basis(model, receipt, body_name, baseline_added_mass_t)
    baseline_added_mass_t = receipt['properties']['translational']['added_mass']['z'] / 1000
    waves = model.get('Environment', {}).get('WaveTrains', [])
    if len(waves) != 1 or not isinstance(waves[0].get('Name'), str):
        raise ValueError('One named wave train required')
    for index in case_indices:
        _change_payload(case_settings(matrix, index), waves[0]['Name'])
    output.mkdir(parents=True, exist_ok=False)
    (output/'.incomplete').write_text('Preparation has not completed.\n')
    (output/'master.yml').write_bytes(raws['source'])
    rows = _write_changes(output, matrix, factors, case_indices, body, ca_z, baseline_added_mass_t, waves[0]['Name'])
    result = dict(schema_version=1, status='prepared_not_native_verified', engineering_acceptance='NOT EVALUATED',
        body_name=body_name, baseline_added_mass_t=baseline_added_mass_t, source_ca=body['AddedMassCoefficient'],
        reference_mass_t=body['HydrodynamicMass'][2], wave_name=waves[0]['Name'],
        master_file='master.yml', master_sha256=source_sha256, cases=rows,
        generator_sha256=sha256(Path(__file__).read_bytes()).hexdigest(),
        code_sha256=code_hashes(),
        sources={name: dict(path=os.path.relpath(path, output).replace('\\', '/'), sha256=sha256(raws[name]).hexdigest()) for name, path in paths.items()},
        limitations=['One-at-a-time Z added-mass diagnostic. Original master XY, drag and all other non-wave properties are retained; the full recalculated candidate is not adopted.',
                     'Bracket factors are project diagnostic assumptions, not standards-derived bounds or operating limits.',
                     f'Factor 1 uses receipt CaZ {ca_z!r}; original master CaZ is {body["AddedMassCoefficient"][2]!r}.'])
    _json(output/'manifest.json', result)
    if (output/'master.yml').read_bytes() != raws['source']:
        raise ValueError('Master readback mismatch')
    for name, path in paths.items():
        if path.read_bytes() != raws[name]:
            raise ValueError('Input changed during preparation')
    (output/'.incomplete').unlink()
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('source', 'matrix', 'receipt'):
        parser.add_argument('--'+name, required=True, type=Path)
        parser.add_argument('--'+name+'-sha256', required=True)
    parser.add_argument('--output', required=True, type=Path)
    parser.add_argument('--body-name', required=True)
    parser.add_argument('--baseline-added-mass-t', required=True, type=float)
    parser.add_argument('--factors', nargs='+', required=True, type=float)
    parser.add_argument('--case-indices', nargs='+', required=True, type=int)
    print(json.dumps({'status': prepare_bracket(**vars(parser.parse_args()))['status']}))


if __name__ == '__main__':
    main()
