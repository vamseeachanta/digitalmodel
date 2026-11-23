"""Derived studies differ from their source only by declared master fields or the wave seed."""
import json

import pytest
import yaml

from digitalmodel.workflows import installation_seastates as sea
from digitalmodel.workflows import installation_variant_study as variant


@pytest.fixture
def source_study(tmp_path):
    master = tmp_path / 'master-source.yml'
    master.write_text(yaml.safe_dump({
        'General': {'StageDuration': [10, 10], 'TargetLogSampleInterval': .1,
                    'ImplicitUseVariableTimeStep': 'No', 'ImplicitConstantTimeStep': .05},
        'Environment': {'WaveTrains': [{'Name': 'Wave1', 'WaveDirection': 165,
                                        'WaveOrigin': [0, 0], 'WaveTimeOrigin': 0}]},
        'Lines': [{'Name': 'rope', 'EA': 42.}]}))
    study = tmp_path / 'inputs'
    sea.prepare_matrix(master, sea.compute_hash(master), study, gamma=1.5, components=80, max_time_step=.05)
    return study


def test_timestep_variant_changes_one_master_field_only(source_study, tmp_path):
    out = tmp_path / 'dt025'
    record = variant.prepare_variant_study(source_study, out, general={'ImplicitConstantTimeStep': 0.025},
                                           purpose='time-step diagnostic')
    before = yaml.safe_load((source_study / 'master.yml').read_text())
    after = yaml.safe_load((out / 'master.yml').read_text())
    assert after['General'].pop('ImplicitConstantTimeStep') == 0.025
    before['General'].pop('ImplicitConstantTimeStep')
    assert after == before
    source_matrix = json.loads((source_study / 'matrix.json').read_text())
    matrix = json.loads((out / 'matrix.json').read_text())
    assert matrix['settings']['fixed_time_step_s'] == 0.025
    for a, b in zip(source_matrix['cases'], matrix['cases']):
        assert (source_study / a['change_file']).read_bytes() == (out / b['change_file']).read_bytes()
    handover = json.loads((out / 'handover.json').read_text())
    assert handover['matrix_sha256'] == sea.compute_hash(out / 'matrix.json')
    assert len(handover['cases']) == 156 and {c['status'] for c in handover['cases']} == {'MISSING'}
    assert record['master_deltas'] == {'General.ImplicitConstantTimeStep': {'before': 0.05, 'after': 0.025}}
    assert record['engineering_acceptance'] == 'NOT EVALUATED'
    assert json.loads((out / 'variant.json').read_text()) == record
    assert not list(tmp_path.glob('*.source-master.yml'))


def test_seed_variant_changes_wave_seed_only(source_study, tmp_path):
    out = tmp_path / 'seed2'
    variant.prepare_variant_study(source_study, out, seed=20260916, purpose='boundary seeds')
    assert (out / 'master.yml').read_bytes() == (source_study / 'master.yml').read_bytes()
    source_matrix = json.loads((source_study / 'matrix.json').read_text())
    matrix = json.loads((out / 'matrix.json').read_text())
    for a, b in zip(source_matrix['cases'], matrix['cases']):
        old = yaml.safe_load((source_study / a['change_file']).read_text())
        new = yaml.safe_load((out / b['change_file']).read_text())
        assert new['Environment']['WaveTrains'][0].pop('WaveSeed') == 20260916
        old['Environment']['WaveTrains'][0].pop('WaveSeed')
        assert old == new and b['seed'] == 20260916


@pytest.mark.parametrize('kwargs', [
    {}, {'general': {'WaveHs': 1}}, {'general': {'ImplicitConstantTimeStep': 0.05}},
    {'general': {'ImplicitConstantTimeStep': float('nan')}}, {'seed': 20260915}, {'seed': True}])
def test_invalid_or_empty_variant_rejected_before_output(source_study, tmp_path, kwargs):
    out = tmp_path / 'bad'
    with pytest.raises(ValueError):
        variant.prepare_variant_study(source_study, out, purpose='x', **kwargs)
    assert not out.exists()


def test_existing_output_or_changed_source_rejected(source_study, tmp_path):
    out = tmp_path / 'exists'
    out.mkdir()
    with pytest.raises(FileExistsError):
        variant.prepare_variant_study(source_study, out, seed=1, purpose='x')
    change = next((source_study / 'changes').iterdir())
    change.write_text(change.read_text() + '# tampered\n')
    with pytest.raises(ValueError):
        variant.prepare_variant_study(source_study, tmp_path / 'fresh', seed=1, purpose='x')


def _rewrite_matrix(study, mutate):
    path = study / 'matrix.json'
    data = json.loads(path.read_text())
    mutate(data)
    path.write_text(json.dumps(data))


def test_truncated_source_matrix_rejected(source_study, tmp_path):
    _rewrite_matrix(source_study, lambda d: d.update(cases=d['cases'][:1]))
    with pytest.raises(ValueError):
        variant.prepare_variant_study(source_study, tmp_path / 'out', seed=7, purpose='x')
    assert not (tmp_path / 'out').exists() and not list(tmp_path.glob('out*'))


def test_inconsistent_source_settings_rejected(source_study, tmp_path):
    _rewrite_matrix(source_study, lambda d: d['settings'].update(fixed_time_step_s=0.025))
    with pytest.raises(ValueError):
        variant.prepare_variant_study(source_study, tmp_path / 'out', seed=7, purpose='x')
    assert not list(tmp_path.glob('out*'))


def test_failed_verification_publishes_nothing(source_study, tmp_path, monkeypatch):
    def boom(*args): raise ValueError('forced')
    monkeypatch.setattr(variant, '_verify_changes', boom)
    with pytest.raises(ValueError):
        variant.prepare_variant_study(source_study, tmp_path / 'out', seed=7, purpose='x')
    assert not list(tmp_path.glob('out*'))
