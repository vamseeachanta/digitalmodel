import json
from hashlib import sha256

import pytest

from digitalmodel.workflows import vessel_capability_postcampaign as workflow


def fixture_config(tmp_path):
    matrix = tmp_path / 'matrix.json'
    matrix.write_text('{"cases": []}')
    digest = sha256(matrix.read_bytes()).hexdigest()
    source = tmp_path / 'prior.json'
    source.write_text(json.dumps(dict(matrix_sha256=digest, created_utc='prior',
                                      design_basis={'dry_mass_t': 5.97})))
    campaign = tmp_path / 'campaign.json'
    campaign.write_text(json.dumps(dict(status='selected_cases_complete',
                                        matrix_sha256=digest, cases=[])))
    return dict(source_report=str(source), source_report_sha256=sha256(source.read_bytes()).hexdigest(),
                campaign=str(campaign), matrix=str(matrix), output=str(tmp_path / 'new.html'),
                pid=100, create_time=20.0, poll_seconds=1, wait_timeout=5)


def fake_report(campaign, matrix, output, basis):
    result = dict(counts={'MISSING': 1}, design_basis=basis,
                  campaign_sha256=sha256(campaign.read_bytes()).hexdigest())
    output.write_text('report')
    output.with_suffix('.json').write_text(json.dumps(result))
    return result


def test_completed_wait_preserves_basis_and_records_partial(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    monkeypatch.setattr(workflow, 'generate_report', fake_report)
    receipt = workflow.run(config)
    assert receipt['status'] == 'PARTIAL'
    assert receipt['source_snapshot_utc'] == 'prior'
    assert json.loads((tmp_path / 'new.json').read_text())['design_basis']['dry_mass_t'] == 5.97
    assert (tmp_path / 'new.campaign-snapshot.json').exists()


@pytest.mark.parametrize('change,match', [('lock', 'lock'), ('running', 'terminal'),
                                        ('row', 'RUNNING'), ('pid', 'identity'),
                                        ('source', 'digest')])
def test_guards_fail_without_report(monkeypatch, tmp_path, change, match):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    if change == 'lock': (tmp_path / 'campaign.lock').touch()
    if change in ('running', 'row'):
        path = tmp_path / 'campaign.json'
        data = json.loads(path.read_text())
        if change == 'running': data['status'] = 'running'
        else: data['cases'] = [{'status': 'RUNNING'}]
        path.write_text(json.dumps(data))
    if change == 'pid': monkeypatch.setattr(workflow, '_process_identity', lambda pid: 21)
    if change == 'source': (tmp_path / 'prior.json').write_text('{}')
    with pytest.raises(ValueError, match=match): workflow.run(config)
    assert not (tmp_path / 'new.html').exists()
    assert json.loads((tmp_path / 'new.completion.json').read_text())['status'] == 'FAILED'


def test_timeout(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: 20)
    ticks = iter([0, 6])
    monkeypatch.setattr(workflow.time, 'monotonic', lambda: next(ticks))
    with pytest.raises(TimeoutError): workflow.run(config)


def test_changed_campaign_during_generation_never_publishes(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    def mutate(*args):
        result = fake_report(*args)
        (tmp_path / 'campaign.json').write_text('{}')
        return result
    monkeypatch.setattr(workflow, 'generate_report', mutate)
    with pytest.raises(ValueError, match='changed'): workflow.run(config)
    assert not (tmp_path / 'new.html').exists()


@pytest.mark.parametrize('key,value', [('poll_seconds', 31), ('wait_timeout', float('inf')),
                                      ('pid', 0), ('create_time', -1)])
def test_invalid_config(tmp_path, key, value):
    config = fixture_config(tmp_path)
    config[key] = value
    with pytest.raises(ValueError): workflow.run(config)


def test_real_generator_keeps_missing_grid_rows(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    matrix = tmp_path / 'matrix.json'
    matrix.write_text(json.dumps({'cases': [dict(hs_m=1, tp_s=8, seed=1)]}))
    digest = sha256(matrix.read_bytes()).hexdigest()
    for name in ('prior.json', 'campaign.json'):
        path = tmp_path / name
        content = json.loads(path.read_text())
        content['matrix_sha256'] = digest
        path.write_text(json.dumps(content))
    config['source_report_sha256'] = sha256((tmp_path / 'prior.json').read_bytes()).hexdigest()
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    receipt = workflow.run(config)
    assert receipt['status'] == 'PARTIAL'
    assert receipt['counts'] == {'MISSING': 1}
    assert json.loads((tmp_path / 'new.json').read_text())['cases'][0]['status'] == 'MISSING'


def test_wait_checks_identity_until_exit(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    identities = iter([20, 20, None])
    sleeps = []
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: next(identities))
    monkeypatch.setattr(workflow.time, 'sleep', sleeps.append)
    workflow._wait(config)
    assert sleeps == [1, 1]


@pytest.mark.parametrize('target', ['prior.json', 'matrix.json', 'campaign.lock'])
def test_postgeneration_input_guard(monkeypatch, tmp_path, target):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    def mutate(*args):
        result = fake_report(*args)
        (tmp_path / target).write_text('{}')
        return result
    monkeypatch.setattr(workflow, 'generate_report', mutate)
    with pytest.raises(ValueError): workflow.run(config)
    assert not (tmp_path / 'new.html').exists()


def test_existing_receipt_refuses_without_overwrite(tmp_path):
    config = fixture_config(tmp_path)
    receipt = tmp_path / 'new.completion.json'
    receipt.write_text('preserve')
    with pytest.raises(FileExistsError): workflow.run(config)
    assert receipt.read_text() == 'preserve'


def test_mutated_snapshot_never_publishes(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    def mutate(campaign, *args):
        result = fake_report(campaign, *args)
        campaign.write_text('{}')
        return result
    monkeypatch.setattr(workflow, 'generate_report', mutate)
    with pytest.raises(ValueError): workflow.run(config)
    assert not (tmp_path / 'new.html').exists()


def test_report_campaign_hash_mismatch_never_publishes(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    def corrupt(*args):
        result = fake_report(*args)
        result['campaign_sha256'] = 'incorrect'
        return result
    monkeypatch.setattr(workflow, 'generate_report', corrupt)
    with pytest.raises(ValueError): workflow.run(config)
    assert not (tmp_path / 'new.html').exists()


def test_code_mutation_never_publishes(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    source = tmp_path / 'dependency.py'
    source.write_text('original')
    monkeypatch.setattr(workflow, '_code_paths', lambda: [source])
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    def mutate(*args):
        result = fake_report(*args)
        source.write_text('changed')
        return result
    monkeypatch.setattr(workflow, 'generate_report', mutate)
    with pytest.raises(ValueError): workflow.run(config)
    assert not (tmp_path / 'new.html').exists()


@pytest.mark.parametrize('counts,planned,expected', [({'VERIFIED': 0},0,False),({'VERIFIED': 1},2,False),({'VERIFIED': 2,'FAILED':0},2,True)])
def test_execution_complete_requires_full_nonempty_grid(counts, planned, expected):
    assert workflow._execution_complete(counts, planned) is expected


def test_case_evidence_hash_detects_trace_mutation(tmp_path):
    run = tmp_path / 'run'
    traces = run / 'installation_traces'
    traces.mkdir(parents=True)
    trace = traces / 'traces.npz'
    trace.write_bytes(b'original')
    snapshot = {'cases': [{'status': 'COMPLETED', 'run_dir': str(run),
                          'generation_file': str(tmp_path / 'generation.json')}]}
    before = workflow._evidence_hashes(snapshot)
    trace.write_bytes(b'changed')
    assert workflow._evidence_hashes(snapshot) != before


@pytest.mark.parametrize('git,status', [
    ({'head': 'a' * 40, 'dirty': []}, 'PINNED'),
    ({'head': 'a' * 40, 'dirty': ['src/digitalmodel/workflows/vessel_capability_report.py']}, 'UNPINNED_CODE'),
    (None, 'UNKNOWN')])
def test_receipt_records_git_code_identity(monkeypatch, tmp_path, git, status):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    monkeypatch.setattr(workflow, 'generate_report', fake_report)
    def fake_git(paths):
        if git is None: raise OSError('git unavailable')
        return git
    monkeypatch.setattr(workflow, '_git_state', fake_git)
    receipt = workflow.run(config)
    identity = receipt['code_identity']
    assert identity['status'] == status
    assert identity['git_head'] == (None if git is None else 'a' * 40)
    assert identity['dirty_paths'] == ([] if git is None else git['dirty'])
    assert json.loads((tmp_path / 'new.completion.json').read_text())['code_identity'] == identity


def test_real_git_state_lists_only_pinned_paths():
    state = workflow._git_state(workflow._code_paths())
    assert len(state['head']) == 40 and isinstance(state['dirty'], list)


def _git_repo(tmp_path, monkeypatch):
    import subprocess
    for key in ('GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR', 'GIT_INDEX_FILE'):
        monkeypatch.delenv(key, raising=False)
    repo = tmp_path / 'repo with space'
    (repo / 'pkg').mkdir(parents=True)
    git = lambda *a: subprocess.run(['git', '-C', str(repo), *a], check=True, capture_output=True)
    git('init', '-q'); git('config', 'user.email', 't@t'); git('config', 'user.name', 't')
    for name in ('pinned.py', 'other.py', 'moved.py'):
        (repo / 'pkg' / name).write_text('x = 1\n')
    git('add', '.'); git('commit', '-qm', 'init')
    return repo, git


def test_git_state_detects_dirty_pinned_file_from_subdirectory(tmp_path, monkeypatch):
    repo, git = _git_repo(tmp_path, monkeypatch)
    pinned, other, moved = (repo / 'pkg' / n for n in ('pinned.py', 'other.py', 'moved.py'))
    assert workflow._git_state([pinned, moved], start=repo / 'pkg')['dirty'] == []
    pinned.write_text('x = 2\n'); other.write_text('x = 3\n')
    git('mv', 'pkg/moved.py', 'pkg/renamed.py')
    state = workflow._git_state([pinned, moved], start=repo / 'pkg')
    assert state['dirty'] == ['pkg/moved.py', 'pkg/pinned.py']
    assert len(state['head']) == 40


def test_git_identity_change_during_run_marks_unpinned(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    monkeypatch.setattr(workflow, 'generate_report', fake_report)
    heads = iter(['a' * 40, 'b' * 40])
    monkeypatch.setattr(workflow, '_git_state', lambda paths: {'head': next(heads), 'dirty': []})
    receipt = workflow.run(config)
    assert receipt['code_identity']['status'] == 'UNPINNED_CODE'
    assert receipt['code_identity']['changed_during_run'] is True
    assert receipt['code_identity_end']['git_head'] == 'b' * 40


def test_staging_change_during_run_is_detected(tmp_path, monkeypatch):
    repo, git = _git_repo(tmp_path, monkeypatch)
    pinned = repo / 'pkg' / 'pinned.py'
    pinned.write_text('x = 2\n')
    before = workflow._git_state([pinned], start=repo)
    git('add', 'pkg/pinned.py')
    after = workflow._git_state([pinned], start=repo)
    assert before['dirty'] == after['dirty'] and before['dirty_status'] != after['dirty_status']


def test_failed_run_records_end_identity(monkeypatch, tmp_path):
    config = fixture_config(tmp_path)
    monkeypatch.setattr(workflow, '_process_identity', lambda pid: None)
    heads = iter(['a' * 40, 'b' * 40])
    monkeypatch.setattr(workflow, '_git_state', lambda paths: {'head': next(heads), 'dirty': []})
    def fail(*args): raise ValueError('Input changed during report generation')
    monkeypatch.setattr(workflow, 'generate_report', fail)
    with pytest.raises(ValueError): workflow.run(config)
    receipt = json.loads((tmp_path / 'new.completion.json').read_text())
    assert receipt['status'] == 'FAILED'
    assert receipt['code_identity_end']['git_head'] == 'b' * 40
    assert receipt['code_identity']['changed_during_run'] is True
