"""Generation contracts with synthetic API doubles; no licensed solver calls."""
import copy
import json
from types import SimpleNamespace as NS

import pytest
import yaml

from digitalmodel.workflows import installation_seastates as sea


@pytest.mark.parametrize('cap', [.05, .1])
@pytest.mark.parametrize('mode', ['No', False])
def test_fixed_timestep_master_preserves_mode_and_skips_inactive_setting(setup, cap, mode):
    api, source, _, output, extraction = setup
    payload = yaml.safe_load(source.read_text())
    payload['General'].update(ImplicitUseVariableTimeStep=mode, ImplicitConstantTimeStep=.05)
    source.write_text(yaml.safe_dump(payload))
    manifest = sea.prepare_matrix(source, sea.compute_hash(source), output, max_time_step=cap)
    change = yaml.safe_load((output / manifest['cases'][0]['change_file']).read_text())
    assert 'ImplicitVariableMaxTimeStep' not in change['General']
    assert manifest['settings']['fixed_time_step_s'] == .05
    result = sea.materialize_case(api, output, 0, output.parent / 'fixed-case', extraction=extraction)
    actual = yaml.safe_load((output.parent / 'fixed-case/model.yml').read_text())
    assert actual['General']['ImplicitUseVariableTimeStep'] == mode
    assert actual['General']['ImplicitConstantTimeStep'] == .05
    assert 'ImplicitVariableMaxTimeStep' not in actual['General']
    assert result['settings']['fixed_time_step_s'] == .05


@pytest.mark.parametrize('fixed', [0, -.1, .2, float('nan'), float('inf')])
def test_invalid_fixed_timestep_rejected_before_matrix_write(setup, fixed):
    _, source, _, output, _ = setup
    payload = yaml.safe_load(source.read_text())
    payload['General'].update(ImplicitUseVariableTimeStep='No', ImplicitConstantTimeStep=fixed)
    source.write_text(yaml.safe_dump(payload))
    with pytest.raises(ValueError, match='Fixed integration timestep'):
        sea.prepare_matrix(source, sea.compute_hash(source), output, max_time_step=.1)
    assert not output.exists()


def test_standalone_fixed_case_records_actual_step_separately(setup):
    api, source, _, output, extraction = setup
    payload = yaml.safe_load(source.read_text())
    payload['General'].update(ImplicitUseVariableTimeStep='No', ImplicitConstantTimeStep=.05)
    source.write_text(yaml.safe_dump(payload))
    result = sea.generate_case(api, source, sea.compute_hash(source), output, extraction=extraction)
    assert result['settings']['fixed_time_step_s'] == .05
    assert result['settings']['max_time_step_s'] == .1


def test_fixed_metadata_rejects_variable_mode_readback(setup):
    api, source, _, _, _ = setup
    model = api.Model(); model.LoadData(source)
    settings = sea._settings(1, 8, 1, 80, 600, .1, 1.5, 80)
    reference = sea._configure(model, settings)
    settings['fixed_time_step_s'] = .05
    model.general.ImplicitUseVariableTimeStep = 'Yes'
    with pytest.raises(ValueError, match='mode'):
        sea._verify(model, settings, reference)


class Environment(NS):
    def __setattr__(self, key, value):
        super().__setattr__(key, value)
        if key == 'WaveType' and hasattr(self, 'WaveDirection'):
            super().__setattr__('WaveDirection', 180.)
        if key == 'WaveOrigin':
            super().__setattr__('WaveOriginX', value[0])
            super().__setattr__('WaveOriginY', value[1])
        if key == 'WaveGamma' and hasattr(self, 'WaveTp'):
            super().__setattr__('WaveTp', self.WaveTp * .9)


class Model:
    def LoadData(self, path):
        self.data = yaml.safe_load(open(path))
        if 'BaseFile' in self.data:
            from pathlib import Path
            delta = self.data
            self.LoadData(Path(path).parent / delta['BaseFile'])
            self.general.__dict__.update(delta['General'])
            for key, value in delta['Environment'].items():
                if key == 'WaveTrains':
                    for name, val in value[0].items():
                        if name != 'Name': setattr(self.environment, name, val)
                else: setattr(self.environment, key, value)
            return
        self.general = NS(**self.data['General'])
        self.environment = Environment(**self.data['Environment'])

    def payload(self):
        return {**self.data, 'General': vars(self.general), 'Environment': vars(self.environment)}

    def SaveDataMem(self, kind):
        return yaml.safe_dump(self.payload()).encode()

    def SaveData(self, path):
        with open(path, 'w') as stream:
            yaml.safe_dump(self.payload(), stream)


@pytest.fixture
def setup(tmp_path):
    source = tmp_path / 'synthetic.yml'
    source.write_text(yaml.safe_dump({'General': {'StageDuration': [10, 10, 1], 'TargetLogSampleInterval': .1},
        'Environment': {'NumberOfWaveTrains': 1, 'WaveType': 'Dean stream', 'WaveDirection': 180,
                        'WaveOriginX': 0., 'WaveOriginY': 0., 'WaveTimeOrigin': 0., 'WaterDepth': 2000},
        'Lines': [{'Name': 'test_rope', 'EA': 42.}]}))
    api = NS(Model=lambda **kw: Model(), DataFileType=NS(Text=1))
    extraction = {'period': [0., 600.], 'time_histories': [{'object': 'rope'}], 'range_graphs': [{'object': 'jumper'}]}
    return api, source, sea.compute_hash(source), tmp_path / 'case', extraction


def test_case_preserves_source_and_structure_and_full_stages(setup):
    api, source, digest, output, extraction = setup
    result = sea.generate_case(api, source, digest, output, extraction=extraction)
    assert sea.compute_hash(source) == digest
    model = yaml.safe_load((output / 'model.yml').read_text())
    assert model['Lines'] == yaml.safe_load(source.read_text())['Lines']
    assert model['General']['StageDuration'] == [80., 600.]
    assert result['settings']['tp_s'] == 8.
    assert result['settings']['seed'] == 20260915
    request = yaml.safe_load((output / 'request.yml').read_text())
    assert request['model_sha256'] == sea.compute_hash(output / 'model.yml')
    assert request['extraction'] == extraction
    assert request['timeout_seconds'] == 1800
    assert json.loads((output / 'generation.json').read_text()) == result


def test_wave_conversion_preserves_nondefault_heading(setup):
    api, source, _, output, extraction = setup
    payload = yaml.safe_load(source.read_text())
    payload['Environment']['WaveDirection'] = 165.
    payload['Environment'].update(WaveOriginX=12., WaveOriginY=-7., WaveTimeOrigin=3.)
    source.write_text(yaml.safe_dump(payload))
    digest = sea.compute_hash(source)
    sea.generate_case(api, source, digest, output, extraction=extraction)
    matrix = output.parent / 'matrix'
    sea.prepare_matrix(source, digest, matrix)
    sea.materialize_case(api, matrix, 43, output.parent / 'pilot', extraction=extraction)


@pytest.mark.parametrize('reference', [
    {'WaveDirection': 165., 'WaveOrigin': [0., 0.], 'WaveTimeOrigin': 0., 'WaveHs': 999},
    {'WaveDirection': float('nan'), 'WaveOrigin': [0., 0.], 'WaveTimeOrigin': 0.},
    {'WaveDirection': 165., 'WaveOrigin': [0.], 'WaveTimeOrigin': 0.},
    {'WaveDirection': 165., 'WaveOrigin': [0., float('inf')], 'WaveTimeOrigin': 0.},
])
def test_wave_reference_rejects_unbounded_overrides(reference):
    settings = sea._settings(1, 8, 1, 80, 600, .1, 1.5, 80)
    settings['wave_reference'] = reference
    with pytest.raises(ValueError, match='Wave reference'):
        sea._change_payload(settings, 'Wave1')


def test_refuses_overwrite_or_wrong_source_hash(setup):
    api, source, digest, output, extraction = setup
    with pytest.raises(ValueError, match='digest'):
        sea.generate_case(api, source, '0'*64, output, extraction=extraction)
    assert not output.exists()
    output.mkdir()
    with pytest.raises(FileExistsError):
        sea.generate_case(api, source, digest, output, extraction=extraction)


def test_changed_structure_rejected(setup):
    api, source, digest, output, extraction = setup
    class Tampering(Model):
        def SaveData(self, path):
            self.data['Lines'][0]['EA'] = 999.
            super().SaveData(path)
    api.Model = lambda **kw: Tampering()
    with pytest.raises(ValueError, match='structural'):
        sea.generate_case(api, source, digest, output, extraction=extraction)
    assert not (output / 'request.yml').exists()


@pytest.mark.parametrize('settings', [{'hs': 0}, {'tp': float('nan')}, {'seed': 1.5}, {'duration': -1}])
def test_invalid_inputs_fail_before_creation(setup, settings):
    api, source, digest, output, extraction = setup
    with pytest.raises(ValueError):
        sea.generate_case(api, source, digest, output, extraction=extraction, **settings)
    assert not output.exists()


def test_manifest_has_all_156_unexecuted_cells(setup):
    _, source, digest, output, _ = setup
    result = sea.prepare_matrix(source, digest, output)
    cases = result['cases']
    assert len(cases) == len({(c['hs_m'], c['tp_s']) for c in cases}) == 156
    assert {c['hs_m'] for c in cases} == {i/4 for i in range(1, 13)}
    assert {c['tp_s'] for c in cases} == set(range(4, 17))
    assert all(c['status'] == 'not_run' for c in cases)
    assert result['excluded_cases'] == []
    assert not list(output.glob('*.sim'))


def test_matrix_is_one_master_and_small_hashed_change_files(setup):
    _, source, digest, output, _ = setup
    result = sea.prepare_matrix(source, digest, output)
    assert (output / 'master.yml').read_bytes() == source.read_bytes()
    case = result['cases'][0]
    change = output / case['change_file']
    assert sea.compute_hash(change) == case['change_sha256']
    payload = yaml.safe_load(change.read_text())
    assert payload['BaseFile'] == '../master.yml'
    assert 'Lines' not in payload
    assert len(change.read_bytes()) < 2000


def test_selected_case_materialization_verifies_dependencies(setup):
    api, source, digest, output, extraction = setup
    manifest = sea.prepare_matrix(source, digest, output)
    case_index = next(i for i, c in enumerate(manifest['cases']) if c['hs_m'] == 2 and c['tp_s'] == 8)
    prepared = sea.materialize_case(api, output, case_index, output.parent / 'selected', extraction=extraction)
    assert prepared['settings']['tp_s'] == 8
    assert prepared['dependencies']['master_sha256'] == digest
    change = output / manifest['cases'][case_index]['change_file']
    change.write_text(change.read_text() + '\n# tampered\n')
    with pytest.raises(ValueError, match='digest'):
        sea.materialize_case(api, output, case_index, output.parent / 'reject', extraction=extraction)
    assert not (output.parent / 'reject').exists()


def test_materialization_does_not_save_variation_as_standalone(setup):
    api, source, digest, output, extraction = setup
    class VariationModel(Model):
        def LoadData(self, path):
            super().LoadData(path)
            self.is_variation = 'BaseFile' in yaml.safe_load(open(path))
        def SaveData(self, path):
            if self.is_variation:
                raise AssertionError('Variation SaveData retains BaseFile')
            super().SaveData(path)
    api.Model = lambda **kw: VariationModel()
    sea.prepare_matrix(source, digest, output)
    sea.materialize_case(api, output, 95, output.parent / 'selected', extraction=extraction)


def test_sensitivity_settings_are_persisted_and_materialized(setup):
    api, source, digest, output, extraction = setup
    sea.prepare_matrix(source, digest, output, buildup=160., duration=1200., sample_interval=.05, components=400, max_time_step=.05)
    extraction['period'] = [0., 1200.]
    result = sea.materialize_case(api, output, 95, output.parent/'selected', extraction=extraction)
    assert result['settings']['buildup_s'] == 160.
    assert result['settings']['duration_s'] == 1200.
    assert result['settings']['sample_interval_s'] == .05
    assert result['settings']['components'] == 400
    model = yaml.safe_load((output.parent/'selected/model.yml').read_text())
    assert model['General']['ImplicitVariableMaxTimeStep'] == .05
    assert sea.compute_hash(source) == digest


def test_materialization_accepts_extended_timeout_without_changing_inputs(setup):
    api, source, digest, output, extraction = setup
    sea.prepare_matrix(source, digest, output)
    before = {p: p.read_bytes() for p in output.rglob('*') if p.is_file()}
    selected = output.parent / 'selected'
    sea.materialize_case(api, output, 95, selected, extraction=extraction, timeout_seconds=14400)
    assert yaml.safe_load((selected / 'request.yml').read_text())['timeout_seconds'] == 14400
    assert all(p.read_bytes() == content for p, content in before.items())


@pytest.mark.parametrize('timeout', [0, -1, float('nan'), float('inf'), True, None, 'bad'])
def test_invalid_timeout_fails_before_materialization_writes(setup, timeout):
    api, source, digest, output, extraction = setup
    sea.prepare_matrix(source, digest, output)
    selected = output.parent / 'selected'
    with pytest.raises(ValueError, match='timeout_seconds'):
        sea.materialize_case(api, output, 95, selected, extraction=extraction, timeout_seconds=timeout)
    assert not selected.exists()
