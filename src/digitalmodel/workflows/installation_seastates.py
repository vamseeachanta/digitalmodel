"""Prepare immutable irregular-wave cases for the existing reproduction runner.

No analysis is executed here. Supply an already configured OrcFxAPI facade.
Run a generated request with digitalmodel.workflows.orcaflex_reproduce.
"""
from __future__ import annotations

import copy
import json
import math
import re
import shutil
from pathlib import Path

import yaml

from digitalmodel.infrastructure.persistence.provenance import compute_hash


def _settings(hs, tp, seed, buildup, duration, sample_interval, gamma, components, max_time_step=.1):
    values = {'hs_m': hs, 'tp_s': tp, 'buildup_s': buildup, 'duration_s': duration,
              'sample_interval_s': sample_interval, 'gamma': gamma, 'max_time_step_s': max_time_step}
    if any(not math.isfinite(float(v)) or float(v) <= 0 for v in values.values()):
        raise ValueError('Sea-state and duration parameters must be finite and positive')
    if isinstance(seed, bool) or not isinstance(seed, int) or seed < 0:
        raise ValueError('seed must be a nonnegative integer')
    if isinstance(components, bool) or not isinstance(components, int) or components < 1:
        raise ValueError('components must be a positive integer')
    return {**{k: float(v) for k, v in values.items()}, 'seed': seed, 'components': components}


def _source(source, digest):
    source = Path(source).resolve(strict=True)
    if not isinstance(digest, str) or len(digest) != 64 or compute_hash(source) != digest:
        raise ValueError('Source digest mismatch')
    if re.search(r'(?im)^\s*(?:BaseFile|IncludeFile)\s*:', source.read_text(encoding='utf-8-sig')):
        raise ValueError('Master must be self-contained; unresolved dependencies are prohibited')
    return source


def _snapshot(api, model):
    from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader
    text = bytes(model.SaveDataMem(api.DataFileType.Text)).decode('utf-8-sig')
    return yaml.load(text, Loader=OrcaFlexLoader)


def _structure(snapshot):
    """Compare all structural data plus non-wave environment and general data."""
    result = copy.deepcopy(snapshot)
    general = result.get('General', {})
    for key in ('StageDuration', 'TargetLogSampleInterval', 'ImplicitVariableMaxTimeStep'):
        general.pop(key, None)
    env = result.get('Environment', {})
    for key in list(env):
        if key.startswith('Wave') or key in ('UserSpecifiedRandomWaveSeeds', 'KinematicStretchingMethod'):
            env.pop(key)
    return result


def _wave_reference(env):
    return {key: getattr(env, key) for key in
            ('WaveDirection', 'WaveOriginX', 'WaveOriginY', 'WaveTimeOrigin')}


def _fixed_mode(value):
    """Accept native API strings and YAML's boolean representation of No."""
    return value is False or value == 'No'


def _validated_wave_reference(reference):
    if not isinstance(reference, dict) or set(reference) != {'WaveDirection', 'WaveOrigin', 'WaveTimeOrigin'}:
        raise ValueError('Wave reference requires exactly direction, origin and time')
    origin = reference['WaveOrigin']
    if not isinstance(origin, (list, tuple)) or len(origin) != 2:
        raise ValueError('Wave reference origin must contain two coordinates')
    values = [reference['WaveDirection'], reference['WaveTimeOrigin'], *origin]
    if any(isinstance(v, bool) or not isinstance(v, (int, float)) or not math.isfinite(v) for v in values):
        raise ValueError('Wave reference values must be finite numbers')
    return copy.deepcopy(reference)


def _configure(model, settings):
    env, general = model.environment, model.general
    if int(env.NumberOfWaveTrains) != 1:
        raise ValueError('Exactly one wave train is required')
    reference = _wave_reference(env)
    general.StageDuration = [settings['buildup_s'], settings['duration_s']]
    general.TargetLogSampleInterval = settings['sample_interval_s']
    if not _fixed_mode(getattr(general, 'ImplicitUseVariableTimeStep', 'Yes')):
        general.ImplicitVariableMaxTimeStep = settings['max_time_step_s']
    else:
        settings.setdefault('fixed_time_step_s', float(general.ImplicitConstantTimeStep))
    env.WaveType = 'JONSWAP'
    env.WaveJONSWAPParameters = 'Partially specified'
    env.UserSpecifiedRandomWaveSeeds = 'Yes'
    env.WaveFrequencySpectrumDiscretisationMethod = 'Equal energy'
    env.WaveNumberOfSpectralDirections = 1
    env.WaveHs = settings['hs_m']
    # OrcaFlex preserves Tz when gamma changes; set Tp AFTER gamma.
    env.WaveGamma = settings['gamma']
    env.WaveTp = settings['tp_s']
    env.WaveSeed = settings['seed']
    env.WaveNumberOfComponents = settings['components']
    for key, value in reference.items():
        setattr(env, key, value)
    return reference


def _verify(model, settings, reference):
    env = model.environment
    expected = {'WaveHs': settings['hs_m'], 'WaveTp': settings['tp_s'],
                'WaveGamma': settings['gamma'], 'WaveSeed': settings['seed'],
                'WaveNumberOfComponents': settings['components'], 'WaveNumberOfSpectralDirections': 1}
    for key, value in expected.items():
        if not math.isclose(float(getattr(env, key)), value, rel_tol=1e-10, abs_tol=1e-10):
            raise ValueError(f'Readback mismatch: {key}')
    for key, value in {'WaveType': 'JONSWAP', 'WaveJONSWAPParameters': 'Partially specified',
                       'UserSpecifiedRandomWaveSeeds': 'Yes',
                       'WaveFrequencySpectrumDiscretisationMethod': 'Equal energy'}.items():
        if getattr(env, key) != value:
            raise ValueError(f'Readback mismatch: {key}')
    if list(model.general.StageDuration) != [settings['buildup_s'], settings['duration_s']]:
        raise ValueError('Stage-duration readback mismatch')
    if not math.isclose(float(model.general.TargetLogSampleInterval), settings['sample_interval_s']):
        raise ValueError('Logging interval readback mismatch')
    mode = getattr(model.general, 'ImplicitUseVariableTimeStep', 'Yes')
    if 'fixed_time_step_s' in settings and not _fixed_mode(mode):
        raise ValueError('Fixed integration mode readback mismatch')
    if _fixed_mode(mode):
        fixed = float(model.general.ImplicitConstantTimeStep)
        if not math.isfinite(fixed) or not 0 < fixed <= settings['max_time_step_s']:
            raise ValueError('Fixed integration timestep exceeds requested cap')
        if 'fixed_time_step_s' in settings and fixed != settings['fixed_time_step_s']:
            raise ValueError('Fixed integration timestep readback mismatch')
    elif not math.isclose(float(model.general.ImplicitVariableMaxTimeStep), settings['max_time_step_s']):
        raise ValueError('Integration maximum timestep readback mismatch')
    if _wave_reference(env) != reference:
        raise ValueError('Wave heading/origin changed')


def _json(path, payload):
    path.write_text(json.dumps(payload, indent=2, allow_nan=False), encoding='utf-8')
    if json.loads(path.read_text(encoding='utf-8')) != payload:
        raise ValueError('JSON readback mismatch')


def _validate_timeout(timeout_seconds):
    if (isinstance(timeout_seconds, bool) or not isinstance(timeout_seconds, (int, float))
            or not math.isfinite(timeout_seconds) or timeout_seconds <= 0):
        raise ValueError('timeout_seconds must be finite and positive')
    return timeout_seconds


def _request(output, extraction, settings, solver_version, timeout_seconds=1800):
    timeout_seconds = _validate_timeout(timeout_seconds)
    config = {'model': 'model.yml', 'model_sha256': compute_hash(output / 'model.yml'),
              'solver_version': solver_version, 'timeout_seconds': timeout_seconds,
              'extraction': copy.deepcopy(extraction), 'limitations': [
                  'Irregular-wave screening case; no engineering acceptance limits established.',
                  'One fixed heading, loading condition and random seed; not an operational envelope.',
                  'Prescribed synthetic waves; future values are not causal forecasts.',
                  f"Stationary extraction excludes {settings['buildup_s']:g} s build-up."]}
    path = output / 'request.yml'
    path.write_text(yaml.safe_dump(config, sort_keys=False), encoding='utf-8')
    if yaml.safe_load(path.read_text(encoding='utf-8')) != config:
        raise ValueError('Request readback mismatch')


def generate_case(api, source, source_sha256, output_dir, *, extraction,
                  hs=2., tp=8., seed=20260915, buildup=80., duration=600.,
                  sample_interval=.1, gamma=3.3, components=200, solver_version='11.6c'):
    """Save one verified JONSWAP model/request; never overwrite or execute it."""
    settings = _settings(hs, tp, seed, buildup, duration, sample_interval, gamma, components)
    source = _source(source, source_sha256)
    if extraction.get('period') != [0., float(duration)]:
        raise ValueError('Extraction period must cover stationary interval [0, duration]')
    output = Path(output_dir).resolve()
    output.mkdir(parents=True, exist_ok=False)
    try:
        model = api.Model(threadCount=1)
        model.LoadData(str(source))
        before = _snapshot(api, model)
        reference = _configure(model, settings)
        _verify(model, settings, reference)
        model.SaveData(str(output / 'model.yml'))
        readback = api.Model(threadCount=1)
        readback.LoadData(str(output / 'model.yml'))
        _verify(readback, settings, reference)
        if _structure(before) != _structure(_snapshot(api, readback)):
            raise ValueError('Generated model changed structural/non-wave inputs')
        _request(output, extraction, settings, solver_version)
        receipt = {'source': str(source), 'source_sha256': source_sha256,
                   'model_sha256': compute_hash(output / 'model.yml'), 'settings': settings,
                   'wave_reference': reference, 'status': 'prepared_not_run',
                   'request': str(output / 'request.yml')}
        _json(output / 'generation.json', receipt)
        return receipt
    finally:
        if compute_hash(source) != source_sha256:
            raise ValueError('Source digest changed during generation')


def prepare_matrix(source, source_sha256, output_dir, *, seed=20260915,
                   buildup=80., duration=600., sample_interval=.1, gamma=3.3, components=200, max_time_step=.1):
    """Prepare exactly 156 unrun cells; exclude no cell by an assumed criterion."""
    source = _source(source, source_sha256)
    common = _settings(2, 8, seed, buildup, duration, sample_interval, gamma, components, max_time_step)
    from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader, orcaflex_dump
    master = yaml.load(source.read_text(encoding='utf-8-sig'), Loader=OrcaFlexLoader)
    if _fixed_mode(master.get('General', {}).get('ImplicitUseVariableTimeStep')):
        fixed = float(master['General']['ImplicitConstantTimeStep'])
        if not math.isfinite(fixed) or not 0 < fixed <= max_time_step:
            raise ValueError('Fixed integration timestep exceeds requested cap')
        common['fixed_time_step_s'] = fixed
    trains = master['Environment'].get('WaveTrains', [{'Name': 'Wave1'}])
    if len(trains) != 1:
        raise ValueError('Exactly one source wave train required')
    wave = trains[0] if 'WaveTrains' in master['Environment'] else master['Environment']
    common['wave_reference'] = {
        'WaveDirection': wave.get('WaveDirection', 180.),
        'WaveOrigin': wave.get('WaveOrigin', [wave.get('WaveOriginX', 0.), wave.get('WaveOriginY', 0.)]),
        'WaveTimeOrigin': wave.get('WaveTimeOrigin', 0.)}
    _validated_wave_reference(common['wave_reference'])
    output = Path(output_dir).resolve()
    output.mkdir(parents=True, exist_ok=False)
    shutil.copyfile(source, output / 'master.yml')
    _source(output / 'master.yml', source_sha256)
    (output / 'changes').mkdir()
    cases = [{'hs_m': index / 4, 'tp_s': period, 'seed': seed, 'status': 'not_run'}
             for index in range(1, 13) for period in range(4, 17)]
    for index, case in enumerate(cases):
        change = output / 'changes' / f'case_{index:03d}.yml'
        settings = {**common, 'hs_m': case['hs_m'], 'tp_s': case['tp_s']}
        orcaflex_dump(_change_payload(settings, trains[0]['Name']), change)
        case.update(change_file=change.relative_to(output).as_posix(), change_sha256=compute_hash(change))
    result = {'source': str(source), 'source_sha256': source_sha256, 'status': 'manifest_only',
              'master_file': 'master.yml', 'master_sha256': source_sha256,
              'settings': {k: v for k, v in common.items() if k not in ('hs_m', 'tp_s', 'seed')},
              'cases': cases, 'excluded_cases': [], 'execution': 'awaits pilot review'}
    _json(output / 'matrix.json', result)
    return result


def _change_payload(settings, wave_name):
    result = {'BaseFile': '../master.yml', 'General': {
        'StageDuration': [settings['buildup_s'], settings['duration_s']],
        'ImplicitVariableMaxTimeStep': settings['max_time_step_s'],
        'TargetLogSampleInterval': settings['sample_interval_s']}, 'Environment': {
        'UserSpecifiedRandomWaveSeeds': 'Yes', 'WaveFrequencySpectrumDiscretisationMethod': 'Equal energy',
        'WaveTrains': [{'Name': wave_name, 'WaveType': 'JONSWAP',
            'WaveNumberOfSpectralDirections': 1, 'WaveJONSWAPParameters': 'Partially specified',
            'WaveHs': settings['hs_m'], 'WaveGamma': settings['gamma'], 'WaveTp': settings['tp_s'],
            'WaveSeed': settings['seed'], 'WaveNumberOfComponents': settings['components']}]}}
    if 'fixed_time_step_s' in settings:
        result['General'].pop('ImplicitVariableMaxTimeStep')
    if 'wave_reference' in settings:
        result['Environment']['WaveTrains'][0].update(_validated_wave_reference(settings['wave_reference']))
    return result


def materialize_case(api, study_dir, case_index, output_dir, *, extraction,
                     solver_version='11.6c', timeout_seconds=1800):
    """Resolve ONE hashed master/change pair into the runner's standalone input."""
    timeout_seconds = _validate_timeout(timeout_seconds)
    study = Path(study_dir).resolve()
    manifest = json.loads((study / 'matrix.json').read_text(encoding='utf-8'))
    case = manifest['cases'][case_index]
    master = _source(study / manifest['master_file'], manifest['master_sha256'])
    change = (study / case['change_file']).resolve()
    if not change.is_relative_to(study) or compute_hash(change) != case['change_sha256']:
        raise ValueError('Change dependency digest/path mismatch')
    payload = yaml.safe_load(change.read_text(encoding='utf-8'))
    if payload.get('BaseFile') != '../master.yml' or (change.parent / payload['BaseFile']).resolve() != master:
        raise ValueError('Unexpected BaseFile dependency')
    common = manifest.get('settings', {})
    settings = _settings(case['hs_m'], case['tp_s'], case['seed'], common.get('buildup_s', 80),
                         common.get('duration_s', 600), common.get('sample_interval_s', .1),
                         common.get('gamma', 3.3), common.get('components', 200), common.get('max_time_step_s', .1))
    if 'fixed_time_step_s' in common:
        settings['fixed_time_step_s'] = common['fixed_time_step_s']
    if 'wave_reference' in common:
        settings['wave_reference'] = common['wave_reference']
    wave_name = payload.get('Environment', {}).get('WaveTrains', [{}])[0].get('Name')
    expected_change = _change_payload(settings, wave_name)
    if 'max_time_step_s' not in common and 'ImplicitVariableMaxTimeStep' not in payload.get('General', {}):
        expected_change['General'].pop('ImplicitVariableMaxTimeStep')
    if payload != expected_change:
        raise ValueError('Change file contains undeclared overrides')
    if extraction.get('period') != [0., settings['duration_s']]:
        raise ValueError('Extraction must cover stationary interval')
    output = Path(output_dir).resolve()
    output.mkdir(parents=True, exist_ok=False)
    baseline = api.Model(threadCount=1)
    baseline.LoadData(str(master))
    reference, before = _wave_reference(baseline.environment), _snapshot(api, baseline)
    model = api.Model(threadCount=1)
    model.LoadData(str(change))
    _verify(model, settings, reference)
    _configure(baseline, settings)
    model = baseline
    _verify(model, settings, reference)
    model.SaveData(str(output / 'model.yml'))
    model.LoadData(str(output / 'model.yml'))
    _verify(model, settings, reference)
    if _structure(before) != _structure(_snapshot(api, model)):
        raise ValueError('Generated model changed structural/non-wave inputs')
    if compute_hash(master) != manifest['master_sha256'] or compute_hash(change) != case['change_sha256']:
        raise ValueError('Dependency digest changed during materialization')
    _request(output, extraction, settings, solver_version, timeout_seconds)
    receipt = {'status': 'prepared_not_run', 'settings': settings,
               'model_sha256': compute_hash(output / 'model.yml'), 'wave_reference': reference,
               'request': str(output / 'request.yml'), 'dependencies': {
                   'master_sha256': manifest['master_sha256'], 'change_sha256': case['change_sha256'],
                   'change_file': str(change), 'master_file': str(master)}}
    _json(output / 'generation.json', receipt)
    return receipt
