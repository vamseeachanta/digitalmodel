# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline input binding using invented parity data, never licensed tables.

Physical numbers are the synthetic example in
examples/workflows/on-bottom-stability-e305/input.yml. The added mass, gravity
and velocity perturbations below are invented sensitivity inputs. Digest
expectations use literal UTF-8 JSON bytes, independently of the evidence helper.
Schema versions and collection sizes are protocol values, not design constants.
"""

import copy
import hashlib
import importlib
import json
from pathlib import Path

import pytest

from digitalmodel.subsea.on_bottom_stability.e305_workflow import BASENAME, router

PIPE_BYTES = (
    b'{"absorption_mass_fraction":0.0,"added_mass_kg_m":0.0,'
    b'"concrete_density_kg_m3":20.0,"concrete_m":0.1,'
    b'"contents_density_kg_m3":0.0,"corrosion_allowance_m":0.0,'
    b'"corrosion_coating_density_kg_m3":0.0,"corrosion_coating_m":0.0,'
    b'"field_joint_density_kg_m3":0.0,"field_joint_fraction":0.0,'
    b'"field_joint_m":0.0,"marine_growth_density_kg_m3":0.0,'
    b'"marine_growth_m":0.0,"steel_density_kg_m3":8.0,'
    b'"steel_od_m":1.0,"wall_m":0.1,"water_density_kg_m3":1.0}'
)
CURRENT_BYTES = (
    b'{"Fw":3.0,"R":0.5,"cd":3.0,"cl":2.0,"cm":0.0,'
    b'"current_m_s":2.0,"duplicate_current":false,"gamma_st":1.4,'
    b'"mu":0.5,"period_s":8.0,"phase_count":720,"phase_rad":0.0,'
    b'"reduction_application":"wave_velocity","role":"extreme_current",'
    b'"target_safety_factor":1.25,"wave_velocity_m_s":1.0}'
)
WAVE_BYTES = (
    b'{"Fw":3.0,"R":0.5,"cd":3.0,"cl":2.0,"cm":0.0,'
    b'"current_m_s":1.0,"duplicate_current":false,"gamma_st":1.4,'
    b'"mu":0.5,"period_s":4.0,"phase_count":720,"phase_rad":0.0,'
    b'"reduction_application":"wave_velocity","role":"extreme_wave",'
    b'"target_safety_factor":1.25,"wave_velocity_m_s":2.0}'
)
# phase_count is the existing Case numerical resolution, not a standard value.
WEIGHT_BYTES = (
    b'{"added_mass":"extra structural mass multiplied by gravity",'
    b'"buoyancy":"local displaced-water volume with length-weighted field joints",'
    b'"method":"e305_simplified.weight_build_up","units":"SI"}'
)
EXPECTED_BASIS_BYTES = (
    b'{"g_m_s2":10.0,"mode":"parity","schema_version":1,"segments":[{'
    b'"cases":[' + CURRENT_BYTES + b',' + WAVE_BYTES + b'],"id":"synthetic_a",'
    b'"pipe":' + PIPE_BYTES + b'}],"weight_basis":' + WEIGHT_BYTES + b'}'
)
OUTPUT_NAMES = ('e305.json', 'e305.csv', 'e305.manifest.json')
SIDECAR = 'e305.evidence.json'


def evidence():
    return importlib.import_module('digitalmodel.subsea.on_bottom_stability.e305_evidence')


@pytest.fixture
def cfg(tmp_path):
    return {BASENAME: {
        'mode': 'parity', 'g': 10.0,
        'outputs': {'directory': str(tmp_path / 'results')},
        'segments': [{'id': 'synthetic_a', 'pipe': json.loads(PIPE_BYTES),
                      'cases': [json.loads(WAVE_BYTES), json.loads(CURRENT_BYTES)]}],
    }}


def run_outputs(cfg):
    router(copy.deepcopy(cfg))
    directory = Path(cfg[BASENAME]['outputs']['directory'])
    pins = {name: hashlib.sha256((directory / name).read_bytes()).hexdigest()
            for name in OUTPUT_NAMES}
    return directory, pins


def write_evidence(cfg):
    directory, pins = run_outputs(cfg)
    basis = evidence().canonical_input_basis(cfg)
    evidence().write_evidence(directory, expected_basis=basis, retained_output_sha256=pins)
    return directory, pins, basis


def verify(directory, pins, basis):
    return evidence().verify_evidence(
        directory, expected_basis=basis, retained_output_sha256=pins)


def test_existing_manifest_has_only_output_identity(cfg):
    directory, pins = run_outputs(cfg)
    manifest = json.loads((directory / 'e305.manifest.json').read_bytes())
    assert manifest == {'schema_version': 1, 'outputs': {
        name: pins[name] for name in ('e305.json', 'e305.csv')}}
    assert not (directory / SIDECAR).exists()


def test_literal_basis_and_digest(cfg):
    basis = evidence().canonical_input_basis(cfg)
    assert basis == json.loads(EXPECTED_BASIS_BYTES)
    assert evidence().basis_digest(basis) == hashlib.sha256(EXPECTED_BASIS_BYTES).hexdigest()


@pytest.mark.parametrize('change', ['gravity', 'weight', 'case', 'second_segment'])
def test_changed_input_changes_basis_digest(cfg, change):
    before = evidence().basis_digest(evidence().canonical_input_basis(cfg))
    params = cfg[BASENAME]
    if change == 'gravity':
        params['g'] = 8.0
    elif change == 'weight':
        params['segments'][0]['pipe']['added_mass_kg_m'] = 1.0
    elif change == 'case':
        params['segments'][0]['cases'][0]['current_m_s'] = 2.0
    else:
        other = copy.deepcopy(params['segments'][0])
        other['id'] = 'synthetic_b'
        params['segments'].append(other)
    assert evidence().basis_digest(evidence().canonical_input_basis(cfg)) != before


def test_reordered_mapping_same_identity(cfg):
    def reverse(value):
        if isinstance(value, dict):
            return {key: reverse(item) for key, item in reversed(list(value.items()))}
        if isinstance(value, list):
            return [reverse(item) for item in value]
        return value

    basis = evidence().canonical_input_basis(cfg)
    assert evidence().basis_digest(evidence().canonical_input_basis(reverse(cfg))) == (
        hashlib.sha256(EXPECTED_BASIS_BYTES).hexdigest())
    assert evidence().basis_digest(reverse(basis)) == evidence().basis_digest(basis)


def test_destination_does_not_change_basis(cfg, tmp_path):
    directory, pins, basis = write_evidence(cfg)
    cfg[BASENAME]['outputs']['directory'] = str(tmp_path / 'relocated')
    cfg['_config_dir_path'] = str(tmp_path / 'unrelated')
    other, other_pins, other_basis = write_evidence(cfg)
    assert other_basis == basis
    assert other_pins == pins
    assert (directory / SIDECAR).read_bytes() == (other / SIDECAR).read_bytes()


def test_round_trip_preserves_outputs_and_binds_manifest(cfg):
    directory, pins = run_outputs(cfg)
    original = {name: (directory / name).read_bytes() for name in OUTPUT_NAMES}
    basis = json.loads(EXPECTED_BASIS_BYTES)  # Independently supplied input record.
    result = evidence().write_evidence(
        directory, expected_basis=basis, retained_output_sha256=pins)
    assert result['input_basis'] == basis
    assert result['input_basis_sha256'] == hashlib.sha256(EXPECTED_BASIS_BYTES).hexdigest()
    assert result['output_manifest_sha256'] == hashlib.sha256(
        original['e305.manifest.json']).hexdigest()
    assert result['output_sha256'] == pins
    assert verify(directory, pins, basis) == result
    assert all((directory / name).read_bytes() == raw for name, raw in original.items())


@pytest.mark.parametrize('missing', ['input_basis', 'input_basis_sha256', 'weight_basis'])
def test_missing_basis_rejected(cfg, missing):
    directory, pins, basis = write_evidence(cfg)
    sidecar = json.loads((directory / SIDECAR).read_bytes())
    target = sidecar['input_basis'] if missing == 'weight_basis' else sidecar
    target.pop(missing)
    (directory / SIDECAR).write_text(json.dumps(sidecar))
    with pytest.raises(ValueError, match='basis'):
        verify(directory, pins, basis)


@pytest.mark.parametrize('empty', ['segments', 'cases'])
def test_empty_case_sets_rejected_before_write(cfg, empty):
    directory, pins = run_outputs(cfg)
    basis = json.loads(EXPECTED_BASIS_BYTES)
    target = basis if empty == 'segments' else basis['segments'][0]
    target[empty] = []
    with pytest.raises(ValueError, match='segments|cases|roles'):
        evidence().write_evidence(directory, expected_basis=basis, retained_output_sha256=pins)
    assert not (directory / SIDECAR).exists()


@pytest.mark.parametrize('name', OUTPUT_NAMES)
def test_tampered_output_rejected(cfg, name):
    directory, pins, basis = write_evidence(cfg)
    with (directory / name).open('ab') as stream:
        stream.write(b' ')
    with pytest.raises(ValueError, match='digest'):
        verify(directory, pins, basis)


def test_altered_result_with_rehashed_archive_rejected(cfg):
    directory, pins, basis = write_evidence(cfg)
    payload = json.loads((directory / 'e305.json').read_bytes())
    payload['segments'][0]['cases'][0]['required_weight_n_m'] = 1.0
    (directory / 'e305.json').write_text(json.dumps(payload))
    forged = {name: hashlib.sha256((directory / name).read_bytes()).hexdigest()
              for name in OUTPUT_NAMES}
    manifest = {'schema_version': 1, 'outputs': {
        name: forged[name] for name in ('e305.json', 'e305.csv')}}
    (directory / 'e305.manifest.json').write_text(json.dumps(manifest))
    forged['e305.manifest.json'] = hashlib.sha256(
        (directory / 'e305.manifest.json').read_bytes()).hexdigest()
    sidecar = json.loads((directory / SIDECAR).read_bytes())
    sidecar['output_sha256'] = forged
    sidecar['output_manifest_sha256'] = forged['e305.manifest.json']
    (directory / SIDECAR).write_text(json.dumps(sidecar))
    with pytest.raises(ValueError, match='digest'):
        verify(directory, pins, basis)  # Original external pins remain authoritative.


def test_expected_basis_is_independent(cfg):
    directory, pins, basis = write_evidence(cfg)
    changed = copy.deepcopy(basis)
    changed['g_m_s2'] = 8.0
    with pytest.raises(ValueError, match='basis'):
        verify(directory, pins, changed)


@pytest.mark.parametrize('change', ['gravity', 'pipe', 'case', 'id', 'weight'])
def test_mismatched_identity_rejected_before_write(cfg, change):
    directory, pins = run_outputs(cfg)
    basis = json.loads(EXPECTED_BASIS_BYTES)
    payload = json.loads((directory / 'e305.json').read_bytes())
    segment = payload['segments'][0]
    if change == 'gravity':
        payload['g'] = 8.0
    elif change == 'pipe':
        segment['pipe']['added_mass_kg_m'] = 1.0
    elif change == 'case':
        segment['cases'][0]['inputs']['current_m_s'] = 2.0
    elif change == 'id':
        segment['id'] = 'different'
    else:
        segment['weight']['g_m_s2'] = 8.0
    (directory / 'e305.json').write_text(json.dumps(payload))
    # Fresh pins cannot make a mismatched physical identity valid.
    pins['e305.json'] = hashlib.sha256((directory / 'e305.json').read_bytes()).hexdigest()
    (directory / 'e305.manifest.json').write_text(json.dumps({
        'schema_version': 1, 'outputs': {name: pins[name]
                                      for name in ('e305.json', 'e305.csv')}}))
    pins['e305.manifest.json'] = hashlib.sha256(
        (directory / 'e305.manifest.json').read_bytes()).hexdigest()
    with pytest.raises(ValueError, match='basis|identity|weight'):
        evidence().write_evidence(directory, expected_basis=basis, retained_output_sha256=pins)
    assert not (directory / SIDECAR).exists()


def test_all_segments_and_case_roles_are_checked(cfg):
    other = copy.deepcopy(cfg[BASENAME]['segments'][0])
    other['id'] = 'synthetic_b'
    cfg[BASENAME]['segments'].append(other)
    directory, pins, basis = write_evidence(cfg)
    reordered = copy.deepcopy(basis)
    reordered['segments'].reverse()
    for segment in reordered['segments']:
        segment['cases'].reverse()
    assert verify(directory, pins, reordered)
    reordered['segments'][0]['cases'][0]['current_m_s'] = 8.0
    with pytest.raises(ValueError, match='basis'):
        verify(directory, pins, reordered)


@pytest.mark.parametrize('invalid', ['missing_g', 'duplicate_segment', 'duplicate_role', 'tables'])
def test_invalid_input_basis_rejected(cfg, invalid):
    params = cfg[BASENAME]
    if invalid == 'missing_g':
        params.pop('g')
    elif invalid == 'duplicate_segment':
        params['segments'].append(copy.deepcopy(params['segments'][0]))
    elif invalid == 'duplicate_role':
        params['segments'][0]['cases'][1] = copy.deepcopy(params['segments'][0]['cases'][0])
    else:
        params['mode'] = 'tables'
    with pytest.raises(ValueError):
        evidence().canonical_input_basis(cfg)


def test_independent_output_pins_required(cfg):
    directory, pins, basis = write_evidence(cfg)
    with pytest.raises(ValueError, match='digest'):
        verify(directory, {}, basis)
    pins.pop('e305.manifest.json')
    with pytest.raises(ValueError, match='digest'):
        verify(directory, pins, basis)


@pytest.mark.parametrize('invalid', [None, {}, {'g_m_s2': 10.0}])
def test_missing_expected_basis_rejected(cfg, invalid):
    directory, pins = run_outputs(cfg)
    with pytest.raises(ValueError, match='basis'):
        evidence().write_evidence(directory, expected_basis=invalid, retained_output_sha256=pins)
    assert not (directory / SIDECAR).exists()


@pytest.mark.parametrize('value', [None, True, 0.0, float('nan'), float('inf')])
def test_invalid_gravity_rejected(cfg, value):
    cfg[BASENAME]['g'] = value
    with pytest.raises(ValueError):
        evidence().canonical_input_basis(cfg)


def test_relabelled_archive_basis_rejected(cfg):
    directory, pins, basis = write_evidence(cfg)
    sidecar = json.loads((directory / SIDECAR).read_bytes())
    sidecar['input_basis']['g_m_s2'] = 8.0
    sidecar['input_basis_sha256'] = hashlib.sha256(
        EXPECTED_BASIS_BYTES.replace(b'"g_m_s2":10.0', b'"g_m_s2":8.0')).hexdigest()
    (directory / SIDECAR).write_text(json.dumps(sidecar))
    with pytest.raises(ValueError, match='basis'):
        verify(directory, pins, basis)


@pytest.mark.parametrize('missing', ['e305.json', 'e305.csv', 'e305.manifest.json', SIDECAR])
def test_missing_retained_file_rejected(cfg, missing, tmp_path):
    directory, pins, basis = write_evidence(cfg)
    incomplete = tmp_path / 'incomplete'
    incomplete.mkdir()
    for name in (*OUTPUT_NAMES, SIDECAR):
        if name != missing:
            (incomplete / name).write_bytes((directory / name).read_bytes())
    with pytest.raises(ValueError, match='digest|basis'):
        verify(incomplete, pins, basis)


@pytest.mark.parametrize('raw', [b'{}', b'null', b'{"input_basis":{},"input_basis":{}}',
                                 b'{"input_basis":NaN}', b'broken JSON'])
def test_malformed_supplement_rejected(cfg, raw):
    directory, pins, basis = write_evidence(cfg)
    (directory / SIDECAR).write_bytes(raw)
    with pytest.raises(ValueError):
        verify(directory, pins, basis)


def test_manifest_must_bind_retained_outputs_before_write(cfg):
    directory, pins = run_outputs(cfg)
    basis = json.loads(EXPECTED_BASIS_BYTES)
    manifest = json.loads((directory / 'e305.manifest.json').read_bytes())
    manifest['outputs']['e305.json'] = hashlib.sha256(b'unrelated synthetic bytes').hexdigest()
    (directory / 'e305.manifest.json').write_text(json.dumps(manifest))
    pins['e305.manifest.json'] = hashlib.sha256(
        (directory / 'e305.manifest.json').read_bytes()).hexdigest()
    with pytest.raises(ValueError, match='manifest digest'):
        evidence().write_evidence(directory, expected_basis=basis, retained_output_sha256=pins)
    assert not (directory / SIDECAR).exists()


def test_sidecar_schema_and_output_binding_rejected(cfg):
    directory, pins, basis = write_evidence(cfg)
    original = (directory / SIDECAR).read_bytes()
    for field in ('schema_version', 'output_manifest_sha256', 'output_sha256'):
        sidecar = json.loads(original)
        sidecar.pop(field)
        (directory / SIDECAR).write_text(json.dumps(sidecar))
        with pytest.raises(ValueError, match='digest|schema'):
            verify(directory, pins, basis)


def test_writer_is_idempotent_and_preserves_existing_evidence(cfg):
    directory, pins, basis = write_evidence(cfg)
    original = (directory / SIDECAR).read_bytes()
    evidence().write_evidence(directory, expected_basis=basis, retained_output_sha256=pins)
    assert (directory / SIDECAR).read_bytes() == original
    different = b'{"different":"retained evidence"}\n'
    (directory / SIDECAR).write_bytes(different)
    with pytest.raises(ValueError, match='existing supplemental'):
        evidence().write_evidence(directory, expected_basis=basis, retained_output_sha256=pins)
    assert (directory / SIDECAR).read_bytes() == different


def test_evidence_does_not_evaluate_workflow_or_stability(cfg, monkeypatch):
    directory, pins = run_outputs(cfg)
    helper = evidence()
    basis = json.loads(EXPECTED_BASIS_BYTES)

    def unexpected(*args, **kwargs):
        pytest.fail('offline evidence must not evaluate stability or the workflow')

    monkeypatch.setattr('digitalmodel.subsea.on_bottom_stability.e305_workflow.router', unexpected)
    monkeypatch.setattr('digitalmodel.subsea.on_bottom_stability.e305_simplified.evaluate_case', unexpected)
    helper.write_evidence(directory, expected_basis=basis, retained_output_sha256=pins)
    assert verify(directory, pins, basis)
