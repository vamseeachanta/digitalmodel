"""Priority dispatch must not overlap a live baseline or alter frozen inputs."""
import json
from contextlib import nullcontext

import pytest
import yaml

from digitalmodel.workflows import installation_priority_requests as priority


@pytest.fixture
def prepared(tmp_path, monkeypatch):
    source = tmp_path / 'source'
    source.mkdir()
    rows, pins = [], {}
    for index in range(5):
        name = f'case_{index}'
        model = source / f'{name}.yml'
        model.write_text('General: {}')
        request = source / f'{name}_request.yml'
        digest = priority.compute_hash(model)
        request.write_text(yaml.safe_dump(dict(model=model.name, model_sha256=digest)))
        rows.append(dict(id=name, request=request.name, model_sha256=digest))
        pins[name] = priority.compute_hash(request)
    manifest = source / 'matched-pilot.json'
    manifest.write_text(json.dumps(dict(cases=rows)))
    artifacts = source / 'manifest.json'
    artifacts.write_text(json.dumps({'files': [dict(path=p.name, sha256=priority.compute_hash(p))
                                             for p in source.iterdir()]}))
    baseline = tmp_path / 'baseline'
    baseline.mkdir()
    (baseline / 'campaign.json').write_text('{"status":"paused"}')
    monkeypatch.setattr(priority.parallel, '_validate_resources', lambda *a: None)
    monkeypatch.setattr(priority.parallel, '_resources', lambda *a: nullcontext())
    monkeypatch.setattr(priority.parallel, '_set_affinity', lambda cpus: cpus)
    monkeypatch.setattr(priority, '_live_identity', lambda identity: None)
    calls = []
    def reproduce(request, output):
        calls.append(str(request))
        output.mkdir(parents=True)
        return {'status': 'completed'}
    monkeypatch.setattr(priority, 'reproduce', reproduce)
    def traces(run, receipt):
        folder = run / 'installation_traces'
        folder.mkdir()
        (folder / 'metadata.json').write_text(json.dumps({
            'trace_sha256': 'a' * 64, 'supplemental_profile_sha256': 'b' * 64}))
    monkeypatch.setattr(priority.parallel.campaign, '_ensure_traces', traces)
    def inline(self, files, config):
        return {'results': [self.process_single_file(dict(file_path=f, config=config)) for f in files]}
    monkeypatch.setattr(priority.PriorityRequestPool, 'process_files_parallel', inline)
    return dict(manifest_path=manifest, manifest_sha256=priority.compute_hash(manifest),
                artifact_manifest=artifacts, artifact_manifest_sha256=priority.compute_hash(artifacts),
                baseline_root=baseline,
                identities=[dict(pid=101, create_time=100.0)], output_root=tmp_path / 'out',
                cpus=[60, 61, 62], calls=calls)


def run(data, **extra):
    args = {k: v for k, v in data.items() if k != 'calls'}
    return priority.run_priority_requests(**dict(args, **extra))


def test_five_requests_in_bounded_waves_preserve_inputs(prepared):
    source = prepared['manifest_path'].parent
    before = {p.name: p.read_bytes() for p in source.iterdir()}
    result = run(prepared)
    assert result['status'] == 'completed'
    assert len(prepared['calls']) == 5
    assert all(r['status'] == 'completed' for r in result['cases'])
    assert before == {p.name: p.read_bytes() for p in source.iterdir()}
    assert not (prepared['output_root'] / 'campaign.lock').exists()
    assert result['execution_policy']['native_threads_per_model'] == 1
    assert all(r['trace_sha256'] == 'a' * 64 for r in result['cases'])
    assert all(r['supplemental_profile_sha256'] == 'b' * 64 for r in result['cases'])


@pytest.mark.parametrize('which', ['manifest', 'request', 'model'])
def test_changed_frozen_input_rejected_before_output(prepared, which):
    root = prepared['manifest_path'].parent
    path = {'manifest': prepared['manifest_path'], 'request': root / 'case_0_request.yml',
            'model': root / 'case_0.yml'}[which]
    path.write_text(path.read_text() + '\n')
    with pytest.raises(ValueError, match='digest'):
        run(prepared)
    assert not prepared['calls']
    assert not prepared['output_root'].exists()


@pytest.mark.parametrize('state,lock', [('running', False), ('paused', True)])
def test_capacity_gate_waits_then_times_out(prepared, state, lock):
    root = prepared['baseline_root']
    (root / 'campaign.json').write_text(json.dumps({'status': state}))
    if lock:
        (root / 'campaign.lock').write_text('owner')
    with pytest.raises(TimeoutError):
        run(prepared, wait_timeout_seconds=0)
    assert not prepared['calls']
    assert json.loads((prepared['output_root'] / 'campaign.json').read_text())['status'] == 'failed'


def test_live_identity_blocks_dispatch(prepared, monkeypatch):
    class Live:
        def children(self, recursive):
            return []
    monkeypatch.setattr(priority, '_live_identity', lambda identity: Live())
    with pytest.raises(TimeoutError):
        run(prepared, wait_timeout_seconds=0)
    assert not prepared['calls']


def test_failed_wave_prevents_second_wave(prepared, monkeypatch):
    original = priority.reproduce
    def fail(request, output):
        if 'case_1' in str(request):
            raise RuntimeError('solver failure')
        return original(request, output)
    monkeypatch.setattr(priority, 'reproduce', fail)
    result = run(prepared)
    assert result['status'] == 'failed'
    assert [r['status'] for r in result['cases']] == [
        'completed', 'failed', 'completed', 'waiting', 'waiting']
    assert len(prepared['calls']) == 2


def test_pid_reuse_is_rejected(monkeypatch):
    class Reused:
        def create_time(self):
            return 200.0
    monkeypatch.setattr(priority.psutil, 'Process', lambda pid: Reused())
    with pytest.raises(RuntimeError, match='reused'):
        priority._live_identity({'pid': 101, 'create_time': 100.0})


def test_descendant_retained_after_parent_exit(prepared, monkeypatch):
    class Child:
        pid = 202
        def create_time(self):
            return 200.0
    class Parent:
        def children(self, recursive):
            return [Child()]
    seen = []
    def live(identity):
        seen.append(identity['pid'])
        return Parent() if identity['pid'] == 101 else None
    monkeypatch.setattr(priority, '_live_identity', live)
    summary = {'tracked_processes': prepared['identities'].copy()}
    assert not priority._capacity_ready(prepared['baseline_root'], summary)
    assert {'pid': 202, 'create_time': 200.0} in summary['tracked_processes']
    assert 202 in seen


def test_output_overlap_and_missing_pins_fail(prepared):
    with pytest.raises(ValueError):
        run(prepared, output_root=prepared['manifest_path'].parent / 'outputs')
    with pytest.raises(ValueError):
        run(prepared, artifact_manifest_sha256='0' * 64)
    with pytest.raises(ValueError):
        run(prepared, workers=4)


def test_path_escape_rejected(prepared):
    path = prepared['manifest_path']
    data = json.loads(path.read_text())
    data['cases'][0]['request'] = '../escaped.yml'
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError):
        run(prepared, manifest_sha256=priority.compute_hash(path))


def test_waiting_transitions_to_running_only_after_capacity_release(prepared, monkeypatch):
    original = priority._capacity_ready
    attempts = []
    snapshots = []
    save = priority.parallel.campaign._save
    def ready(*args):
        attempts.append(1)
        return len(attempts) > 1 and original(*args)
    def record(root, summary):
        snapshots.append(json.loads(json.dumps(summary)))
        save(root, summary)
    monkeypatch.setattr(priority, '_capacity_ready', ready)
    monkeypatch.setattr(priority.time, 'sleep', lambda _: None)
    monkeypatch.setattr(priority.parallel.campaign, '_save', record)
    run(prepared)
    assert snapshots[0]['status'] == 'waiting'
    assert max(sum(r['status'] == 'running' for r in s['cases']) for s in snapshots) == 3
    assert snapshots[-1]['status'] == 'completed'


def test_access_denied_fails_closed_without_solve(prepared, monkeypatch):
    def reject(identity):
        raise priority.psutil.AccessDenied(identity['pid'])
    monkeypatch.setattr(priority, '_live_identity', reject)
    with pytest.raises(priority.psutil.AccessDenied):
        run(prepared)
    assert not prepared['calls']


def test_input_change_while_waiting_is_rejected(prepared, monkeypatch):
    original = priority._wait
    def wait(*args):
        original(*args)
        (prepared['manifest_path'].parent / 'case_0_request.yml').write_text('tampered')
    monkeypatch.setattr(priority, '_wait', wait)
    with pytest.raises(ValueError, match='digest'):
        run(prepared)
    assert not prepared['calls']


def test_missing_worker_result_fails_without_second_wave(prepared, monkeypatch):
    monkeypatch.setattr(priority.PriorityRequestPool, 'process_files_parallel',
                        lambda *args: {'results': []})
    result = run(prepared)
    assert result['status'] == 'failed'
    assert all(r['status'] == 'waiting' for r in result['cases'][3:])


def test_existing_root_is_not_overwritten(prepared):
    prepared['output_root'].mkdir()
    retained = prepared['output_root'] / 'previous'
    retained.write_text('retained')
    with pytest.raises(FileExistsError):
        run(prepared)
    assert retained.read_text() == 'retained'


def test_trace_extraction_failure_stops_next_wave(prepared, monkeypatch):
    original = priority.parallel.campaign._ensure_traces
    def fail(run, receipt):
        if run.name == 'case_1':
            raise ValueError('Trace profile mismatch')
        original(run, receipt)
    monkeypatch.setattr(priority.parallel.campaign, '_ensure_traces', fail)
    result = run(prepared)
    assert result['status'] == 'failed'
    assert result['cases'][1]['status'] == 'failed'
    assert 'Trace profile mismatch' in result['cases'][1]['error']
    assert len(prepared['calls']) == 3
    assert all(r['status'] == 'waiting' for r in result['cases'][3:])
