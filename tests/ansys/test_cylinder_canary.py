"""Synthetic operator protocols; launch is always replaced and never native."""
import copy
import json
from pathlib import Path

import pytest

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys.cylinder_canary import run_canary
from digitalmodel.ansys import cylinder_canary
from digitalmodel.ansys.cylinder_benchmark import build_case


ORDER = ['ocv-zero-t60-n16', 'ocv-t60-p10-n4',
         'ocv-t60-p10-n8', 'ocv-t60-p10-n16']


def source_inventory():
    directory = Path(cylinder_canary.__file__).resolve().parent
    paths = sorted(directory.glob('*.py'))
    return [{'path': 'src/digitalmodel/ansys/' + path.name,
             'sha256': digest_bytes(path.read_bytes())} for path in paths]


def reapprove_manifest(bundle, approval, manifest):
    raw = canonical_bytes(manifest)
    (bundle / 'manifest.json').write_bytes(raw)
    approval['manifest_sha256'] = digest_bytes(raw)


@pytest.fixture
def protocol(tmp_path):
    bundle = tmp_path / 'bundle'
    bundle.mkdir()
    artifacts = []
    for name in ['reference.json', *[f'{case}.inp' for case in ORDER]]:
        data = b'synthetic; never a native engineering input'
        if name.endswith('.inp'):
            data = build_case(Path(name).stem)['deck_bytes']
        (bundle / name).write_bytes(data)
        artifacts.append({'path': name, 'sha256': digest_bytes(data)})
    manifest = {'schema': 'cylinder-b1-1', 'case_order': ORDER,
                'artifacts': artifacts, 'reference': 'reference.json',
                'runtime_sources': source_inventory(),
                'cases': [{'case_id': case, 'deck': f'{case}.inp'} for case in ORDER]}
    (bundle / 'manifest.json').write_bytes(canonical_bytes(manifest))
    approval = {'approval_id': 'synthetic-b2', 'operator_id': 'operator',
                'checker_id': 'independent-checker',
                'ledger_directory': str(tmp_path / 'authority-ledger'),
                'execution_host': 'synthetic-host',
                'manifest_sha256': digest_bytes(canonical_bytes(manifest)),
                'capture_allowance_bytes': 10000000, 'reserve_bytes': 1024,
                'executable_sha256': 'a' * 64,
                'profile': {'cores': 1, 'parallel': 'smp', 'timeout_seconds': 300}}
    calls = []
    def launch(case, directory, timeout):
        calls.append(case['case_id'])
        assert (directory.parent / f"attempt-{len(calls)}.json").is_file()
        assert timeout == 300
        return {'return_code': 0, 'timed_out': False, 'owned_processes_remaining': 0,
                'stdout': b'', 'stderr': b'', 'duration_seconds': '1',
                'containment_verified': True, 'evidence_complete': True,
                'settlement_required': False, 'streams_finalized': True}
    callbacks = {
        'verify_authority': lambda a: a['approval_id'] == 'synthetic-b2',
        'preflight': lambda a: {'license_query': 'synthetic licensed seat observation',
            'exclusive_seat_owner': a['operator_id'], 'process_inventory': [],
            'free_bytes': 20000000, 'source_rights': 'synthetic private-retention decision',
            'execution_host': a['execution_host'],
            'executable_sha256': a['executable_sha256'], 'profile': a['profile']},
        'launch': launch, 'extract': lambda case, path, result: {'case_id': case['case_id']},
        'assess': lambda records: {'status': 'PASS', 'checks': []},
        'adjudicate': lambda receipt, a: {'checker_id': a['checker_id'],
                                       'receipt_sha256': digest_bytes(canonical_bytes(receipt))},
        'verify_reference': lambda path: True,
    }
    return bundle, tmp_path / 'run', approval, callbacks, calls


def test_four_attempts_are_journaled_before_launch_and_no_repeat(protocol):
    bundle, root, approval, callbacks, calls = protocol
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'PASS'
    assert calls == ORDER and result['unattempted'] == []
    with pytest.raises(FileExistsError):
        run_canary(bundle, root, approval, **callbacks)
    assert len(calls) == 4


@pytest.mark.parametrize('failure', ['authority', 'reference', 'hash', 'storage', 'seat'])
def test_preflight_failure_consumes_no_attempt(protocol, failure):
    bundle, root, approval, callbacks, calls = protocol
    if failure == 'authority':
        callbacks['verify_authority'] = lambda a: False
    elif failure == 'reference':
        callbacks['verify_reference'] = lambda path: False
    elif failure == 'hash':
        (bundle / 'reference.json').write_bytes(b'changed')
    elif failure == 'storage':
        approval['capture_allowance_bytes'] = 99999999
    else:
        original = callbacks['preflight']
        callbacks['preflight'] = lambda a: {**original(a), 'exclusive_seat_owner': 'other'}
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'INCOMPLETE' and not calls
    assert result['unattempted'] == ORDER
    assert not list(root.glob('attempt-*.json'))


@pytest.mark.parametrize('failure', ['launch', 'timeout', 'streams', 'process', 'extract'])
def test_evidence_failure_stops_after_consumed_attempt(protocol, failure):
    bundle, root, approval, callbacks, calls = protocol
    original = callbacks['launch']
    def faulty(case, directory, timeout):
        result = original(case, directory, timeout)
        if failure == 'launch':
            raise OSError('mock launch failure')
        key = {'timeout': 'timed_out', 'streams': 'stdout',
               'process': 'owned_processes_remaining'}.get(failure)
        if key:
            result[key] = b'unexpected' if failure == 'streams' else 1
        return result
    callbacks['launch'] = faulty
    if failure == 'extract':
        callbacks['extract'] = lambda *a: (_ for _ in ()).throw(ValueError('missing evidence'))
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'INCOMPLETE' and calls == ORDER[:1]
    assert result['unattempted'] == ORDER[1:]


def test_failed_control_stops_but_coarse_diagnostics_continue(protocol):
    bundle, root, approval, callbacks, calls = protocol
    callbacks['assess'] = lambda records: {'status': 'FAIL', 'checks': ['zero-control']}
    assert run_canary(bundle, root, approval, **callbacks)['status'] == 'FAIL'
    assert calls == ORDER[:1]


def test_time_budget_reserves_full_attempt_and_missing_adjudication_refuses(protocol):
    bundle, root, approval, callbacks, calls = protocol
    times = iter([0, 1600])
    result = run_canary(bundle, root, approval, clock=lambda: next(times), **callbacks)
    assert result['status'] == 'INCOMPLETE' and not calls


def test_failed_final_adjudication_cannot_be_pass(protocol):
    bundle, root, approval, callbacks, calls = protocol
    callbacks['adjudicate'] = lambda *a: None
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'INCOMPLETE' and len(calls) == 4


def test_tamper_after_preflight_refuses_next_launch(protocol):
    bundle, root, approval, callbacks, calls = protocol
    original = callbacks['launch']
    def launch(*args):
        result = original(*args)
        (bundle / f'{ORDER[1]}.inp').write_bytes(b'tampered')
        return result
    callbacks['launch'] = launch
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'INCOMPLETE' and len(calls) == 1


def test_current_deck_tamper_during_preflight_cannot_launch(protocol):
    bundle, root, approval, callbacks, calls = protocol
    original = callbacks['preflight']
    def tamper(a):
        (bundle / f'{ORDER[0]}.inp').write_bytes(b'changed during preflight')
        return original(a)
    callbacks['preflight'] = tamper
    assert run_canary(bundle, root, approval, **callbacks)['status'] == 'INCOMPLETE'
    assert not calls


def test_approval_cannot_run_four_more_in_another_directory(protocol):
    bundle, root, approval, callbacks, calls = protocol
    assert run_canary(bundle, root, approval, **callbacks)['status'] == 'PASS'
    result = run_canary(bundle, root.with_name('another-run'), approval, **callbacks)
    assert result['status'] == 'INCOMPLETE' and len(calls) == 4


def test_failed_streams_and_execution_metadata_are_retained(protocol):
    bundle, root, approval, callbacks, calls = protocol
    original = callbacks['launch']
    callbacks['launch'] = lambda *a: {**original(*a), 'stdout': b'\xff raw failure'}
    assert run_canary(bundle, root, approval, **callbacks)['status'] == 'INCOMPLETE'
    assert (root / ORDER[0] / 'stdout.bin').read_bytes() == b'\xff raw failure'
    assert (root / ORDER[0] / 'execution.json').is_file()


def test_runtime_failure_after_journal_retains_incomplete_outcome(protocol):
    bundle, root, approval, callbacks, calls = protocol
    callbacks['launch'] = lambda *a: (_ for _ in ()).throw(RuntimeError('supervisor failed'))
    assert run_canary(bundle, root, approval, **callbacks)['status'] == 'INCOMPLETE'
    assert (root / 'attempt-1.json').is_file() and (root / 'outcome.json').is_file()


def test_final_processing_deadline_overrun_cannot_pass(protocol):
    bundle, root, approval, callbacks, calls = protocol
    now = [0]
    original = callbacks['extract']
    def extract(*args):
        if len(calls) == 4:
            now[0] = 1801
        return original(*args)
    callbacks['extract'] = extract
    result = run_canary(bundle, root, approval, clock=lambda: now[0], **callbacks)
    assert result['status'] == 'INCOMPLETE'


@pytest.mark.parametrize('field,value', [('containment_verified', False),
    ('evidence_complete', False), ('settlement_required', True), ('streams_finalized', False)])
def test_unqualified_containment_or_cleanup_cannot_pass(protocol, field, value):
    bundle, root, approval, callbacks, calls = protocol
    original = callbacks['launch']
    callbacks['launch'] = lambda *a: {**original(*a), field: value}
    assert run_canary(bundle, root, approval, **callbacks)['status'] == 'INCOMPLETE'
    assert len(calls) == 1


def test_missing_stream_still_retains_supervisor_settlement_token(protocol):
    import json
    bundle, root, approval, callbacks, calls = protocol
    original = callbacks['launch']
    callbacks['launch'] = lambda *a: {**original(*a), 'stdout': None,
                                    'retained_supervisor_token': 'synthetic-owned-handle'}
    assert run_canary(bundle, root, approval, **callbacks)['status'] == 'INCOMPLETE'
    execution = json.loads((root / ORDER[0] / 'execution.json').read_bytes())
    assert execution['retained_supervisor_token'] == 'synthetic-owned-handle'
    assert execution['stdout_available'] is False


def test_runner_wall_duration_is_retained_as_decimal_text(protocol):
    import json
    bundle, root, approval, callbacks, calls = protocol
    original = callbacks['launch']
    callbacks['launch'] = lambda *a: {**original(*a), 'duration_seconds': 0.125}
    assert run_canary(bundle, root, approval, **callbacks)['status'] == 'PASS'
    execution = json.loads((root / ORDER[0] / 'execution.json').read_bytes())
    assert execution['duration_seconds'] == '0.125'


@pytest.mark.parametrize('fault', ['missing', 'incomplete', 'digest', 'alternate_root'])
def test_runtime_source_inventory_is_required(protocol, fault):
    bundle, root, approval, callbacks, calls = protocol
    manifest = json.loads((bundle / 'manifest.json').read_bytes())
    if fault == 'missing':
        del manifest['runtime_sources']
    elif fault == 'incomplete':
        manifest['runtime_sources'].pop()
    elif fault == 'digest':
        manifest['runtime_sources'][0]['sha256'] = '0' * 64
    else:
        manifest['runtime_sources'][0]['path'] = '../alternate.py'
    reapprove_manifest(bundle, approval, manifest)
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'INCOMPLETE' and not calls


@pytest.mark.parametrize('when', ['preflight', 'after_first'])
def test_runtime_source_drift_stops_before_next_attempt(protocol, monkeypatch, when):
    bundle, root, approval, callbacks, calls = protocol
    original_read = Path.read_bytes
    source = Path(cylinder_canary.__file__).resolve()
    def install_drift():
        monkeypatch.setattr(Path, 'read_bytes', lambda path: original_read(path) + b'\n# drift'
                            if path.resolve() == source else original_read(path))
    if when == 'preflight':
        original = callbacks['preflight']
        def preflight(a):
            result = original(a)
            install_drift()
            return result
        callbacks['preflight'] = preflight
    else:
        original = callbacks['launch']
        def launch(*args):
            result = original(*args)
            install_drift()
            return result
        callbacks['launch'] = launch
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'INCOMPLETE'
    assert len(calls) == (0 if when == 'preflight' else 1)


def test_capture_allowance_exceeded_stops_and_preserves_bytes(protocol):
    bundle, root, approval, callbacks, calls = protocol
    original = callbacks['launch']
    def launch(case, directory, timeout):
        (directory / 'oversized.rst').write_bytes(b'x' * 10000001)
        return original(case, directory, timeout)
    callbacks['launch'] = launch
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'INCOMPLETE' and len(calls) == 1
    assert (root / ORDER[0] / 'oversized.rst').stat().st_size == 10000001
    assert result['retained_bytes'] > approval['capture_allowance_bytes']


def test_mismatched_stream_preserves_supervisor_metadata(protocol):
    bundle, root, approval, callbacks, calls = protocol
    original = callbacks['launch']
    def launch(case, directory, timeout):
        (directory / 'stdout.bin').write_bytes(b'changed capture')
        return {**original(case, directory, timeout), 'retained_supervisor_token': 'owned-token'}
    callbacks['launch'] = launch
    assert run_canary(bundle, root, approval, **callbacks)['status'] == 'INCOMPLETE'
    execution = json.loads((root / ORDER[0] / 'execution.json').read_bytes())
    assert execution['retained_supervisor_token'] == 'owned-token'
    assert execution['stdout_readback_matches'] is False


def test_receipt_writes_call_fsync(protocol, monkeypatch):
    import os
    calls = []
    monkeypatch.setattr(os, 'fsync', lambda descriptor: calls.append(descriptor))
    bundle, root, approval, callbacks, _ = protocol
    assert run_canary(bundle, root, approval, **callbacks)['status'] == 'PASS'
    assert len(calls) >= 13


def test_loaded_dependency_from_alternate_root_refuses(protocol, monkeypatch):
    from digitalmodel.ansys import analysis_records
    bundle, root, approval, callbacks, calls = protocol
    monkeypatch.setattr(analysis_records, '__file__', str(root / 'alternate.py'))
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'INCOMPLETE' and not calls


def test_remaining_allowance_is_recorded_and_free_space_rechecked(protocol):
    bundle, root, approval, callbacks, calls = protocol
    original = callbacks['preflight']
    def preflight(a):
        return {**original(a), 'free_bytes': 20000000 if not calls else 1024}
    callbacks['preflight'] = preflight
    result = run_canary(bundle, root, approval, **callbacks)
    assert result['status'] == 'INCOMPLETE' and len(calls) == 1
    first = json.loads((root / 'preflight-1.json').read_bytes())
    assert first['remaining_capture_allowance_bytes'] == approval['capture_allowance_bytes']


def test_final_adjudication_cannot_bind_changed_capture_summary(protocol):
    bundle, root, approval, callbacks, calls = protocol
    original = callbacks['adjudicate']
    def adjudicate(receipt, a):
        proof = original(receipt, a)
        (root / 'late-evidence.bin').write_bytes(b'new unadjudicated evidence')
        return proof
    callbacks['adjudicate'] = adjudicate
    assert run_canary(bundle, root, approval, **callbacks)['status'] == 'INCOMPLETE'
