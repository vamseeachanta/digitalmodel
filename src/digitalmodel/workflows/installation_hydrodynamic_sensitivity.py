"""Prepare bounded property perturbations; no statics, dynamics or acceptance."""
from __future__ import annotations

import copy
import hashlib
import html
import json
import math
import os
from pathlib import Path

import yaml

from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader, orcaflex_dump
from digitalmodel.workflows.structure_hydrodynamic_candidate import (
    _calculate, _citations, _hydro_fields,
)


def _digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def _source(receipt, expected_sha, wiki_root):
    path = Path(receipt).resolve()
    if _digest(path) != expected_sha:
        raise ValueError('Property receipt digest mismatch')
    data = json.loads(path.read_text(encoding='utf-8'))
    master = path.parent / 'master.yml'
    if _digest(master) != data['master_sha256']:
        raise ValueError('Master digest mismatch')
    _citations(data, wiki_root)
    calc, rho, lookup_sha, code_sha = _calculate(data['inputs'])
    if (lookup_sha != data['lookup_sha256'] or code_sha != data['legacy_calculator_sha256']
            or rho != data['rho_water_kg_m3'] or calc != data['properties']):
        raise ValueError('Source calculator provenance or properties mismatch')
    model = yaml.load(master.read_text(encoding='utf-8-sig'), Loader=OrcaFlexLoader)
    if 'BaseFile' in model or 'IncludeFile' in model:
        raise ValueError('Source master must be self-contained; no BaseFile/IncludeFile')
    bodies = [b for b in model['6DBuoys'] if b['Name'] == data['body_name']]
    if len(bodies) != 1:
        raise ValueError('Expected one named body')
    ref = (data['inputs']['m_air'] - data['inputs']['m_water']) / 1000
    fields = _hydro_fields(calc, ref)
    if any(bodies[0].get(key) != value for key, value in fields.items()):
        raise ValueError('Master hydrodynamic properties mismatch')
    return data, master, model, fields


def _factors(data):
    from digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_rectangular import (
        DNVRPH103_hydrodynamics_rectangular,
    )
    calculator = DNVRPH103_hydrodynamics_rectangular()
    calculator.cfg = {'inputs': data['inputs'], 'rho_water': data['rho_water_kg_m3']}
    factors, coefficients = [], []
    for axis in 'xyz':
        d = getattr(calculator, f'get_{axis}_dimensions')()
        lam = math.sqrt(d['a'] * d['b']) / (d['c'] + math.sqrt(d['a'] * d['b']))
        factor = 1 + math.sqrt((1 - lam**2) / (2 * (1 + lam**2)))
        # Endpoint experiment is restricted to the nonperforated receipt.
        if data['inputs']['perforation_ratio'][axis] != 0:
            raise ValueError('Sensitivity requires zero perforation on every axis')
        mass = data['properties']['translational']['added_mass'][axis]
        plate = math.pi / 4 * data['rho_water_kg_m3'] * d['a']**2 * d['b']
        factors.append(factor)
        coefficients.append(mass / factor / plate)
    return factors, coefficients


def _scenarios(fields, factors, plate_coefficients):
    flat = copy.deepcopy(fields['AddedMassCoefficient'])
    flat[:2] = [flat[i] / factors[i] for i in range(2)]
    endpoint = copy.deepcopy(fields['AddedMassCoefficient'])
    endpoint[1] /= plate_coefficients[1]
    return [
        ('baseline', 'Unchanged reference; physical applicability remains unqualified.', {}),
        ('horizontal_flat_plate', 'X/Y finite-height correction removed; diagnostic ablation, not an alternate DNV recommendation. Z is retained.',
         {'AddedMassCoefficient': flat}),
        ('y_asymptotic_endpoint', 'Y plate coefficient set to 1.0 as an asymptotic sensitivity endpoint, not a standard-prescribed value at this aspect ratio.',
         {'AddedMassCoefficient': endpoint}),
        ('cd_080', 'All translational Cd multiplied by 0.8; illustrative perturbation, not a sourced uncertainty bound.',
         {'DragForceCoefficient': [v * .8 for v in fields['DragForceCoefficient']]}),
        ('cd_120', 'All translational Cd multiplied by 1.2; illustrative perturbation, not a sourced uncertainty bound.',
         {'DragForceCoefficient': [v * 1.2 for v in fields['DragForceCoefficient']]})]


def _case_files(output, data, model, fields, scenarios):
    cases = []
    for name, rationale, overrides in scenarios:
        if not set(overrides) <= {'AddedMassCoefficient', 'DragForceCoefficient'}:
            raise ValueError('Unexpected sensitivity override')
        variant = copy.deepcopy(model)
        body = next(b for b in variant['6DBuoys'] if b['Name'] == data['body_name'])
        original = {key: copy.deepcopy(body[key]) for key in overrides}
        body.update(overrides)
        applied = copy.deepcopy(body)
        body.update(original)
        if variant != model:
            raise ValueError('Nonselected model field changed')
        payload = {'BaseFile': '../master.yml'}
        if overrides:
            # In variation models list syntax replaces objects and breaks connections.
            # Named-object mapping edits the existing body in place.
            payload['6DBuoys'] = {data['body_name']: overrides}
        change = output / 'changes' / f'{name}.yml'
        orcaflex_dump(payload, change)
        if yaml.load(change.read_text(), Loader=OrcaFlexLoader) != payload:
            raise ValueError('Change readback mismatch')
        masses = [ca * hm * 1000 for ca, hm in zip(
            applied['AddedMassCoefficient'], applied['HydrodynamicMass'])]
        baseline = [fields['AddedMassCoefficient'][i] * fields['HydrodynamicMass'][i] * 1000 for i in range(3)]
        cases.append({'id': name, 'rationale': rationale, 'overrides': overrides,
                      'change_file': f'changes/{name}.yml', 'change_sha256': _digest(change),
                      'added_mass_kg': masses, 'added_mass_change_percent': [
                          100 * (m / b - 1) for m, b in zip(masses, baseline)],
                      'ca': applied['AddedMassCoefficient'], 'cd': applied['DragForceCoefficient']})
    return cases


def _report(result):
    rows = ''.join('<tr><td>' + html.escape(c['id']) + '</td><td>' +
                   '</td><td>'.join(', '.join(f'{v:.6g}' for v in c[key]) for key in
                                   ('added_mass_kg', 'added_mass_change_percent', 'cd')) +
                   '</td><td>' + html.escape(c['rationale']) + '</td></tr>' for c in result['cases'])
    return ('<!doctype html><html lang="en"><meta charset="utf-8"><title>Hydrodynamic sensitivity</title>'
            '<style>body{font:16px Arial;max-width:1300px;margin:2em auto}table{border-collapse:collapse}'
            'td,th{border:1px solid #aaa;padding:.6em;text-align:left}</style>'
            '<h1>Mudmat hydrodynamic property sensitivity</h1><h2>Scope and limitations</h2>'
            '<p>Five property scenarios are prepared. No statics or dynamics were executed. '
            'Response changes and operating limits are not established. Z finite-height correction, '
            'geometry, rigging, inertia and rotational hydrodynamics are preserved in every scenario. '
            'Cd perturbations affect translation only; rotational drag remains fixed. '
            'The baseline retains the qualification gaps recorded in its source receipt. '
            'Wave settings are inherited without modification; these property variants do not '
            'establish an irregular-wave response study. Review inherited_wave_trains in the JSON '
            'before preparing a separate matched JONSWAP load-case layer.</p>'
            '<p>Inherited wave settings: ' + html.escape(json.dumps(result['inherited_wave_trains'])) + '</p>'
            '<table><tr><th>Case</th><th>Added mass X,Y,Z (kg)</th><th>Added mass change X,Y,Z (%)</th>'
            '<th>Cd X,Y,Z (-)</th><th>Interpretation</th></tr>' + rows + '</table>'
            '<p>Table 1. Property perturbations relative to the unchanged baseline.</p>'
            '<h2>Reproduction and next checkpoint</h2><p>Run the digitalmodel '
            'installation_hydrodynamic_sensitivity module using the receipt hash in '
            '<a href="sensitivity.json">sensitivity.json</a>. Native loading is a separate checkpoint. '
            'Before response studies, use a matched sea state, wave realization and extraction setup '
            'for all scenarios; compare loads, slack and motions under the same conditions.</p></html>')


def prepare_sensitivity(receipt, receipt_sha256, output_dir, *, wiki_root):
    """Generate a hashed master and five lean change files; never invoke OrcaFlex."""
    output = Path(output_dir).resolve()
    if output.exists():
        raise FileExistsError(output)
    data, master, model, fields = _source(receipt, receipt_sha256, wiki_root)
    factors, coefficients = _factors(data)
    scenarios = _scenarios(fields, factors, coefficients)
    if _digest(master) != data['master_sha256'] or _digest(receipt) != receipt_sha256:
        raise ValueError('Source drift before output creation')
    output.mkdir(parents=True, exist_ok=False)
    (output / 'changes').mkdir()
    (output / 'master.yml').write_bytes(master.read_bytes())
    result = {'schema_version': 1, 'state': 'prepared_not_native_verified',
              'analysis_executed': False, 'acceptance_established': False,
              'receipt_file': os.path.relpath(Path(receipt).resolve(), output).replace('\\', '/'),
              'receipt_sha256': receipt_sha256, 'master_sha256': _digest(output / 'master.yml'),
              'generator_sha256': _digest(__file__), 'body_name': data['body_name'],
              'inherited_wave_trains': model.get('Environment', {}).get('WaveTrains', []),
              'finite_height_factors_xyz': factors, 'plate_coefficients_xyz': coefficients,
              'units': {'added_mass': 'kg', 'ca': 'dimensionless', 'cd': 'dimensionless'},
              'baseline_limitations': data.get('limitations', []), 'citations': data['citations'],
              'cases': _case_files(output, data, model, fields, scenarios)}
    path = output / 'sensitivity.json'
    path.write_text(json.dumps(result, indent=2) + '\n', encoding='utf-8')
    (output / 'report.html').write_text(_report(result), encoding='utf-8')
    if (json.loads(path.read_text()) != result
            or _digest(master) != data['master_sha256']
            or _digest(output / 'master.yml') != data['master_sha256']
            or _digest(receipt) != receipt_sha256):
        raise ValueError('Output readback/source preservation failure')
    return result


def main():
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--receipt', required=True)
    parser.add_argument('--receipt-sha256', required=True)
    parser.add_argument('--output', required=True)
    parser.add_argument('--wiki-root', required=True)
    args = parser.parse_args()
    prepare_sensitivity(args.receipt, args.receipt_sha256, args.output, wiki_root=args.wiki_root)


if __name__ == '__main__':
    main()
