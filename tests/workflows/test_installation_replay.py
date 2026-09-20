import json
from hashlib import sha256

import pytest

from digitalmodel.workflows import installation_replay as replay


def manifest(tmp_path):
    entries = {}
    for name in replay.REQUIRED_INPUTS:
        file = tmp_path / f'{name}.json'
        file.write_text('{}')
        entries[name] = dict(path=file.name, sha256=sha256(file.read_bytes()).hexdigest())
    path = tmp_path / 'replay.json'
    path.write_text(json.dumps(dict(version=1, case_index=97, inputs=entries,
        comparison=dict(absolute_tolerance=1e-6, relative_tolerance=1e-6),
        retention=dict(simulation=False), code={}, runtime={})))
    return path


@pytest.mark.parametrize('defect', ['missing', 'tampered'])
def test_missing_or_tampered_input_stops_before_output(tmp_path, defect):
    path = manifest(tmp_path)
    target = tmp_path / 'master.json'
    target.unlink() if defect == 'missing' else target.write_text('changed')
    with pytest.raises(ValueError, match='input'):
        replay.validate_manifest(path, tmp_path / 'fresh', check_environment=False)
    assert not (tmp_path / 'fresh').exists()


def test_existing_output_root_rejected_even_empty(tmp_path):
    path = manifest(tmp_path)
    output = tmp_path / 'existing'
    output.mkdir()
    with pytest.raises(FileExistsError):
        replay.validate_manifest(path, output, check_environment=False)


def test_comparison_requires_identical_channels_and_explicit_tolerances():
    fresh = dict(channels={'load':dict(units='kN', minimum=1, maximum=2)})
    assert replay.compare_metrics(fresh, fresh, dict(absolute_tolerance=0, relative_tolerance=0))['passed']
    with pytest.raises(ValueError, match='channels'):
        replay.compare_metrics(fresh, {'channels': {}}, dict(absolute_tolerance=1, relative_tolerance=1))
    changed = dict(channels={'load':dict(units='kN', minimum=1, maximum=3)})
    assert not replay.compare_metrics(fresh, changed, dict(absolute_tolerance=0, relative_tolerance=0))['passed']


def test_retention_cannot_delete_outside_fresh_run(tmp_path):
    outside = tmp_path / 'historical.sim'
    outside.write_bytes(b'evidence')
    with pytest.raises(ValueError):
        replay.dispose_simulation(outside, tmp_path / 'fresh')
    assert outside.read_bytes() == b'evidence'


def test_lineage_records_verified_bytes(tmp_path):
    output = tmp_path / 'result.json'
    output.write_text('{}')
    record = replay.artifact_record(output, tmp_path)
    assert record == {'path': 'result.json', 'sha256': sha256(output.read_bytes()).hexdigest(), 'bytes': 2}


def test_native_composition_never_requests_resume(monkeypatch, tmp_path):
    from digitalmodel.workflows import installation_seastates, installation_campaign, orcaflex_reproduce
    import psutil
    dll = tmp_path / 'test.dll'
    dll.write_bytes(b'test')
    extraction = tmp_path / 'extraction.yml'
    extraction.write_text('period: [0, 600]')
    captured = {}
    monkeypatch.setattr(psutil, 'Process', lambda: type('P', (), {'cpu_affinity': lambda self, value: None})())
    monkeypatch.setattr(orcaflex_reproduce, '_load_api', lambda c: (object(),
        dict(resolved_version='test', resolved_lib_path=str(dll))))
    monkeypatch.setattr(installation_seastates, 'materialize_case', lambda *a, **k: captured.update(materialized=True))
    monkeypatch.setattr(orcaflex_reproduce, 'reproduce', lambda *a, **k: captured.update(k) or {'status': 'completed'})
    monkeypatch.setattr(installation_campaign, '_verify_run', lambda *a: None)
    monkeypatch.setattr(installation_campaign, '_ensure_traces', lambda *a: None)
    replay._native(tmp_path, {'extraction': extraction}, {},
                   {'case_index': 97, 'timeout_seconds': 14400, 'runtime': {'cpu_affinity': [0], 'solver_version': 'test',
                    'solver_library_sha256': sha256(dll.read_bytes()).hexdigest()}})
    assert captured == {'materialized': True, 'postprocess_only': False}


@pytest.mark.parametrize('failure', [False, True])
def test_fresh_pipeline_retention_and_lineage(monkeypatch, tmp_path, failure):
    from digitalmodel.workflows import installation_replay_report
    source = tmp_path / 'manifest.json'
    source.write_text('{}')
    output = tmp_path / 'new'
    data = dict(case_index=97, runtime={}, comparison={})
    source.write_text(json.dumps(data))
    monkeypatch.setattr(replay, 'validate_manifest', lambda *a, **k: (data, {}))
    monkeypatch.setattr(replay, '_snapshot_inputs', lambda *a: ({}, {}))
    monkeypatch.setattr(replay, '_environment', lambda *a: None)
    def native(root, *args):
        assert not (root / 'run').exists()
        (root / 'run').mkdir()
        (root / 'run/model.sim').write_bytes(b'new simulation')
        return {'resolved_version': 'test'}
    def reports(root, *args):
        if failure:
            raise ValueError('comparison failed')
        return dict(comparison={'passed': True}, artifacts=[])
    monkeypatch.setattr(replay, '_native', native)
    monkeypatch.setattr(installation_replay_report, 'build_reports', reports)
    if failure:
        with pytest.raises(ValueError, match='comparison failed'):
            replay.run_replay(source, output)
        assert (output / 'run/model.sim').exists()
    else:
        result = replay.run_replay(source, output)
        assert result['force_fresh'] is True and result['status'] == 'completed'
        assert not (output / 'run/model.sim').exists()
    lineage = json.loads((output / 'lineage.json').read_bytes())
    assert lineage['status'] == ('failed' if failure else 'completed')
    assert lineage['source_campaign_mutation'] is False


def test_static_and_event_metrics_are_compared():
    channel = dict(units='kN', minimum=1, maximum=2, static_tension_kN=1.5,
                   events={'low_tension': {'total_duration_s': 2, 'event_count': 1}})
    fresh = {'channels': {'load': channel}}
    old = json.loads(json.dumps(fresh))
    old['channels']['load']['events']['low_tension']['total_duration_s'] = 3
    result = replay.compare_metrics(fresh, old, {'absolute_tolerance': 0, 'relative_tolerance': 0})
    assert not result['passed']
    assert {'static_tension_kN', 'events.low_tension.total_duration_s'} <= {r['metric'] for r in result['metrics']}


def test_clean_source_checks_untracked_and_clears_inherited_git_bindings(monkeypatch, tmp_path):
    captured = {}
    monkeypatch.setenv('GIT_DIR', 'unrelated')
    def run(args, **kwargs):
        captured.update(args=args, **kwargs)
        return type('Result', (), {'stdout': '?? src/unpinned.py'})()
    monkeypatch.setattr(replay.subprocess, 'run', run)
    with pytest.raises(ValueError, match='dirty'):
        replay._clean_source(tmp_path, ['src', 'config'])
    assert '--untracked-files=all' in captured['args']
    assert 'GIT_DIR' not in captured['env']


@pytest.mark.parametrize('defect', ['import_origin', 'missing_solver_digest'])
def test_environment_rejects_unpinned_runtime(monkeypatch, tmp_path, defect):
    code_root = replay.Path(replay.__file__).resolve().parents[3]
    source = code_root / 'src/digitalmodel/workflows/installation_replay.py'
    monkeypatch.setattr(replay, '_git', lambda root: 'revision')
    monkeypatch.setattr(replay, '_clean_source', lambda *args: None)
    origin = tmp_path / ('other/__init__.py' if defect == 'import_origin' else 'src/assetutilities/__init__.py')
    monkeypatch.setattr(replay, 'find_spec', lambda name: type('Spec', (), {'origin': str(origin)})())
    data = dict(code={'git_revision': 'revision', 'files': {source.relative_to(code_root).as_posix(): sha256(source.read_bytes()).hexdigest()}},
                runtime={'python_version': replay.platform.python_version(), 'dependencies': {},
                         'assetutilities_root': '.', 'assetutilities_git_revision': 'revision',
                         'cpu_affinity': [0]})
    with pytest.raises(ValueError, match='Imported assetutilities|solver_library_sha256'):
        replay._environment(data, tmp_path)


@pytest.mark.parametrize('timeout', [None, True, 0, -1, '14400', float('inf')])
def test_execution_requires_explicit_typed_timeout(timeout):
    with pytest.raises(ValueError, match='timeout'):
        replay._validate_execution({'timeout_seconds': timeout, 'runtime': {'solver_version': '11.6c'}})


@pytest.mark.parametrize('solver', [None, '', 11.6, True])
def test_execution_requires_solver_version(solver):
    with pytest.raises(ValueError, match='solver_version'):
        replay._validate_execution({'timeout_seconds': 14400, 'runtime': {'solver_version': solver}})


@pytest.mark.parametrize('channel', [{'units': 'kN'}, {'units': 'kN', 'minimum': 1},
                                    {'units': 'kN', 'minimum': '1', 'maximum': '2'}])
def test_comparison_rejects_vacuous_or_missing_extrema(channel):
    row = {'channels': {'load': channel}}
    with pytest.raises(ValueError, match='minimum.*maximum'):
        replay.compare_metrics(row, row, {'absolute_tolerance': 0, 'relative_tolerance': 0})
