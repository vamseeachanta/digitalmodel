"""Bounded campaign ownership, resource and evidence contracts."""
import copy
import json
from pathlib import Path

import pytest

from digitalmodel.workflows import installation_parallel_campaign as parallel

_REAL_EXECUTE = parallel.campaign._execute
_REAL_PROCESS = parallel.psutil.Process
_REAL_POOL_METHOD = parallel.InstallationCasePool.process_files_parallel


@pytest.fixture
def setup(tmp_path, monkeypatch):
    study = tmp_path / 'inputs'
    study.mkdir()
    (study / 'master.yml').write_text('master')
    cases = []
    for index in range(7):
        change = study / f'case_{index}.yml'
        change.write_text(str(index))
        cases.append(dict(hs_m=index / 4 + .25, tp_s=8, seed=42,
                          change_file=change.name,
                          change_sha256=parallel.campaign.compute_hash(change)))
    matrix = dict(master_file='master.yml', cases=cases,
                  master_sha256=parallel.campaign.compute_hash(study / 'master.yml'))
    (study / 'matrix.json').write_text(json.dumps(matrix))
    source = tmp_path / 'frozen.json'
    summary = parallel.campaign._summary(matrix, study, tmp_path / 'old')
    source.write_text(json.dumps(summary))
    affinity = [0, 1, 2, 3]
    class Process:
        def cpu_affinity(self, values=None):
            if values is not None:
                affinity[:] = values
            return affinity[:]
    monkeypatch.setattr(parallel.psutil, 'Process', lambda: Process())
    calls = []
    def execute(index, manifest, root, study, extraction, api, version, row, timeout):
        assert api is None
        assert timeout == 14400
        calls.append(index)
        row.update(status='COMPLETED', run_dir=str(root / 'runs' / f'case_{index:03d}'),
                   generation_file=str(root / 'prepared' / f'case_{index:03d}' / 'generation.json'))
    monkeypatch.setattr(parallel.campaign, '_execute', execute)
    monkeypatch.setattr(parallel.campaign, '_verify_run', lambda *args: {})
    monkeypatch.setattr(parallel.campaign, '_ensure_traces', lambda *args: None)
    def inline(self, files, config):
        return {'results': [self.process_single_file({'file_path': f, 'config': config}) for f in files]}
    monkeypatch.setattr(parallel.InstallationCasePool, 'process_files_parallel', inline)
    return dict(study=study, root=tmp_path / 'new', source=source,
                calls=calls, affinity=affinity, summary=summary)


def run(data, **kwargs):
    return parallel.run_parallel_campaign(data['study'], data['root'],
        source_campaign=data['source'], workers=3, cpus=[0, 1, 2],
        extraction={}, timeout_seconds=14400, **kwargs)


def test_bounded_waves_single_writer_and_resource_restore(setup, monkeypatch):
    snapshots = []
    original = parallel.campaign._save
    def save(root, summary):
        snapshots.append(copy.deepcopy(summary))
        original(root, summary)
    monkeypatch.setattr(parallel.campaign, '_save', save)
    monkeypatch.setenv('OMP_NUM_THREADS', '9')
    result = run(setup)
    assert setup['calls'] == list(range(7))
    assert result['status'] == 'selected_cases_complete'
    assert max(sum(r['status'] == 'RUNNING' for r in s['cases']) for s in snapshots) == 3
    assert setup['affinity'] == [0, 1, 2, 3]
    assert parallel.os.environ['OMP_NUM_THREADS'] == '9'
    assert not (setup['root'] / 'campaign.lock').exists()
    assert result['execution_policy']['native_threads_per_model'] == 1


def test_failure_drains_wave_and_stops_new_submissions(setup, monkeypatch):
    original = parallel.campaign._execute
    def execute(*args):
        if args[0] == 1:
            raise RuntimeError('native failed')
        return original(*args)
    monkeypatch.setattr(parallel.campaign, '_execute', execute)
    result = run(setup)
    assert setup['calls'] == [0, 2]
    assert [r['status'] for r in result['cases']] == [
        'COMPLETED', 'FAILED', 'COMPLETED', 'MISSING', 'MISSING', 'MISSING', 'MISSING']
    assert result['status'] == 'stopped'


def test_completed_source_evidence_verified_and_never_resubmitted(setup, monkeypatch):
    row = setup['summary']['cases'][0]
    row.update(status='COMPLETED', run_dir='retained-run', generation_file='retained-generation')
    setup['source'].write_text(json.dumps(setup['summary']))
    verified = []
    monkeypatch.setattr(parallel.campaign, '_verify_run', lambda *args: verified.append(args[3]) or {})
    result = run(setup)
    assert verified == [0]
    assert 0 not in setup['calls']
    assert result['cases'][0]['run_dir'] == 'retained-run'


def test_changed_completed_evidence_blocks_before_any_solve(setup, monkeypatch):
    setup['summary']['cases'][0].update(status='COMPLETED', run_dir='old', generation_file='gen')
    setup['source'].write_text(json.dumps(setup['summary']))
    def reject(*args):
        raise ValueError('Changed simulation')
    monkeypatch.setattr(parallel.campaign, '_verify_run', reject)
    with pytest.raises(ValueError, match='Changed simulation'):
        run(setup)
    assert not setup['calls']


@pytest.mark.parametrize('cpus,workers', [([0, 0], 2), ([0], 3), ([99], 1), ([True], 1), ([0], True)])
def test_invalid_resource_selection_rejected_before_output(setup, cpus, workers):
    with pytest.raises(ValueError):
        parallel.run_parallel_campaign(setup['study'], setup['root'], source_campaign=setup['source'],
                                       workers=workers, cpus=cpus, extraction={})
    assert not setup['root'].exists()


def test_lock_rejects_second_owner_without_touching_content(setup):
    setup['root'].mkdir()
    lock = setup['root'] / 'campaign.lock'
    lock.write_text('another owner')
    with pytest.raises(FileExistsError):
        run(setup)
    assert lock.read_text() == 'another owner'
    assert not setup['calls']


def test_unfrozen_running_source_rejected(setup):
    setup['summary']['cases'][0]['status'] = 'RUNNING'
    setup['source'].write_text(json.dumps(setup['summary']))
    with pytest.raises(ValueError, match='frozen'):
        run(setup)
    assert not setup['calls']


def test_source_change_blocks_resume(setup):
    run(setup)
    setup['source'].write_text(setup['source'].read_text() + '\n')
    with pytest.raises(ValueError, match='source'):
        run(setup)
    assert setup['calls'] == list(range(7))


def test_stop_marker_prevents_first_wave(setup):
    setup['root'].mkdir()
    (setup['root'] / 'STOP_AFTER_WAVE').write_text('pause')
    result = run(setup)
    assert result['status'] == 'paused'
    assert not setup['calls']


def test_incomplete_old_attempt_is_preserved_but_not_reused(setup):
    setup['summary']['cases'][0]['previous_attempt'] = {'run_dir': 'old-partial'}
    setup['source'].write_text(json.dumps(setup['summary']))
    result = run(setup)
    assert result['cases'][0]['previous_attempt'] == {'run_dir': 'old-partial'}
    assert str(setup['root']) in result['cases'][0]['run_dir']


def test_incomplete_new_root_attempt_is_not_overwritten(setup, monkeypatch):
    attempt = setup['root'] / 'runs' / 'case_000'
    attempt.mkdir(parents=True)
    receipt = attempt / 'run.json'
    receipt.write_text('{"status":"solving"}')
    before = receipt.read_bytes()
    monkeypatch.setattr(parallel.campaign, '_execute', _REAL_EXECUTE)
    def reject(*args):
        raise ValueError('Existing run or extraction is not complete')
    monkeypatch.setattr(parallel.campaign, '_verify_run', reject)
    result = run(setup)
    assert result['cases'][0]['status'] == 'FAILED'
    assert receipt.read_bytes() == before
    assert result['cases'][3]['status'] == 'MISSING'


def test_verified_completed_resume_does_not_execute(setup):
    run(setup)
    setup['calls'].clear()
    result = run(setup)
    assert not setup['calls']
    assert result['status'] == 'selected_cases_complete'


def test_missing_worker_result_cannot_be_completed(setup, monkeypatch):
    monkeypatch.setattr(parallel.InstallationCasePool, 'process_files_parallel',
                        lambda *args: {'results': []})
    result = run(setup)
    assert result['status'] == 'stopped'
    assert [r['status'] for r in result['cases'][:3]] == ['FAILED'] * 3
    assert result['cases'][3]['status'] == 'MISSING'


def test_misordered_source_rows_rejected(setup):
    setup['summary']['cases'].reverse()
    setup['source'].write_text(json.dumps(setup['summary']))
    with pytest.raises(ValueError, match='coordinates/order'):
        run(setup)
    assert not setup['calls']


def test_windows_spawn_transports_failed_receipts_without_loading_solver(setup, monkeypatch):
    """Real child processes reject incomplete evidence before native API loading."""
    monkeypatch.setattr(parallel.psutil, 'Process', _REAL_PROCESS)
    monkeypatch.setattr(parallel.InstallationCasePool, 'process_files_parallel', _REAL_POOL_METHOD)
    monkeypatch.setattr(parallel.campaign, '_execute', _REAL_EXECUTE)
    cpus = _REAL_PROCESS().cpu_affinity()[:3]
    for index in range(len(cpus)):
        run_dir = setup['root'] / 'runs' / f'case_{index:03d}'
        prepared = setup['root'] / 'prepared' / f'case_{index:03d}'
        run_dir.mkdir(parents=True)
        prepared.mkdir(parents=True)
        (run_dir / 'run.json').write_text('{"status":"solving"}')
        (prepared / 'generation.json').write_text('{}')
    result = parallel.run_parallel_campaign(setup['study'], setup['root'],
        source_campaign=setup['source'], workers=len(cpus), cpus=cpus,
        extraction={}, timeout_seconds=14400)
    assert result['status'] == 'stopped'
    for row in result['cases'][:len(cpus)]:
        assert row['status'] == 'FAILED'
        assert 'dependencies' in row['error']
        assert row['worker_cpu_affinity'] == cpus
        assert json.loads((Path(row['run_dir']) / 'run.json').read_text()) == {'status': 'solving'}
    assert result['cases'][len(cpus)]['status'] == 'MISSING'


def test_explicit_subset_preserves_failed_case(setup):
    run(setup)
    path = setup['root'] / 'campaign.json'
    summary = json.loads(path.read_text())
    summary['cases'][0].update(status='FAILED', error='retained instability')
    for index in (4, 5, 6):
        summary['cases'][index]['status'] = 'MISSING'
    path.write_text(json.dumps(summary))
    setup['calls'].clear()
    result = run(setup, case_indices=[4, 5])
    assert setup['calls'] == [4, 5]
    assert result['cases'][0] == summary['cases'][0]
    assert result['cases'][6]['status'] == 'MISSING'
    assert result['selected_indices'] == [4, 5]
    assert result['status'] == 'selected_cases_complete'


@pytest.mark.parametrize('indices', [[], [1, 1], [-1], [7], [True], ['1']])
def test_invalid_case_subset_rejected_before_output(setup, indices):
    with pytest.raises(ValueError):
        run(setup, case_indices=indices)
    assert not setup['root'].exists()
    assert not setup['calls']


def test_completed_subset_is_not_resubmitted(setup):
    run(setup)
    setup['calls'].clear()
    result = run(setup, case_indices=[1, 3])
    assert not setup['calls']
    assert result['selected_indices'] == []


def test_selected_failure_stops_remaining_subset(setup, monkeypatch):
    original = parallel.campaign._execute
    def execute(*args):
        if args[0] == 3:
            raise RuntimeError('selected unstable case')
        return original(*args)
    monkeypatch.setattr(parallel.campaign, '_execute', execute)
    result = run(setup, case_indices=[2, 3, 4, 5])
    assert setup['calls'] == [2, 4]
    assert result['status'] == 'stopped'
    assert result['cases'][3]['status'] == 'FAILED'
    assert result['cases'][5]['status'] == 'MISSING'
    assert result['cases'][0]['status'] == 'MISSING'
