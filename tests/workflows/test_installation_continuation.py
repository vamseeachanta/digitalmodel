"""Continuation contracts for installation waves; no native solver or processes."""
import copy
from contextlib import nullcontext
from dataclasses import asdict
import json
import sys

import pytest

from digitalmodel.workflows import installation_parallel_campaign as parallel
from digitalmodel.workflows.campaign_state import StopDisposition, classify_stop


@pytest.fixture
def waves(tmp_path, monkeypatch):
    calls, snapshots = [], []
    failures = set()
    after_wave = []

    class Pool:
        def __init__(self, num_threads, use_processes):
            assert num_threads == 2
            assert use_processes is True

        def process_files_parallel(self, files, config):
            calls.append(list(files))
            results = []
            for file_path in files:
                index = int(file_path)
                row = copy.deepcopy(config['rows'][index])
                failed = index in failures
                row['status'] = 'FAILED' if failed else 'COMPLETED'
                results.append(dict(file_path=file_path, row=row,
                                    status='failed' if failed else 'success',
                                    error='fake case failure' if failed else None))
            for callback in after_wave:
                callback()
            return {'results': results}

    monkeypatch.setattr(parallel, 'InstallationCasePool', Pool)
    monkeypatch.setattr(parallel.campaign, '_save',
                        lambda root, summary: snapshots.append(copy.deepcopy(summary)))
    summary = dict(status='prepared', cases=[
        dict(index=index, status='MISSING') for index in range(6)])
    return dict(root=tmp_path, summary=summary, calls=calls,
                snapshots=snapshots, failures=failures, after_wave=after_wave)


def run_waves(data, **kwargs):
    return parallel._waves(data['root'], data['summary'], {}, 2, **kwargs)


def assert_stop_record(data, disposition, reason, failed, untouched, flag):
    record = data['summary']['stop_record']
    assert set(record) == {'disposition', 'reason', 'failed_ids', 'untouched_ids',
                           'clears_when', 'auto_resume_allowed', 'continue_untouched'}
    assert record['disposition'] == disposition.value
    assert record['reason'] == reason
    assert record['failed_ids'] == failed
    assert record['untouched_ids'] == untouched
    assert record['continue_untouched'] is flag
    assert record['auto_resume_allowed'] is (
        disposition == StopDisposition.FAILED_CASES_ISOLATED)
    assert all(type(case_id) is str for case_id in failed + untouched)
    statuses = {'COMPLETED': 'completed', 'FAILED': 'failed', 'RUNNING': 'running'}
    expected = asdict(classify_stop(reason, [
        dict(id=str(row['index']), status=statuses.get(row['status'], 'unrun'))
        for row in data['summary']['cases']]))
    expected['disposition'] = expected['disposition'].value
    assert record == dict(expected, continue_untouched=flag)
    assert json.loads(json.dumps(record)) == record
    assert data['snapshots'][-1]['stop_record'] == record


def test_default_failure_stops_before_later_waves(waves):
    waves['failures'].add(1)
    result = run_waves(waves)
    assert result['status'] == 'stopped'
    assert waves['calls'] == [['0', '1']]
    assert [row['status'] for row in result['cases']] == [
        'COMPLETED', 'FAILED', 'MISSING', 'MISSING', 'MISSING', 'MISSING']
    assert_stop_record(waves, StopDisposition.FAILED_CASES_ISOLATED, None,
                       ['1'], ['2', '3', '4', '5'], False)


def test_isolated_failures_continue_through_later_waves(waves):
    waves['failures'].update({1, 4})
    result = run_waves(waves, continue_untouched=True)
    assert result['status'] == 'completed_with_isolated_failures'
    assert waves['calls'] == [['0', '1'], ['2', '3'], ['4', '5']]
    assert [row['status'] for row in result['cases']] == [
        'COMPLETED', 'FAILED', 'COMPLETED', 'COMPLETED', 'FAILED', 'COMPLETED']
    assert all(snapshot['status'] == 'running'
               for snapshot in waves['snapshots'][:-1])
    # classify_stop uses COMPLETE when no untouched cases remain, even with failures.
    assert_stop_record(waves, StopDisposition.COMPLETE, None, ['1', '4'], [], True)


@pytest.mark.parametrize('flag', [False, True])
def test_all_failed_wave_stops_for_diagnosis(waves, flag):
    waves['failures'].update({2, 3})
    result = run_waves(waves, continue_untouched=flag)
    assert result['status'] == 'stopped'
    assert waves['calls'] == [['0', '1'], ['2', '3']]
    assert [row['status'] for row in result['cases']] == [
        'COMPLETED', 'COMPLETED', 'FAILED', 'FAILED', 'MISSING', 'MISSING']
    assert_stop_record(waves, StopDisposition.NEEDS_DIAGNOSIS,
                       'every case in wave 2 failed (possible shared failure)',
                       ['2', '3'], ['4', '5'], flag)


@pytest.mark.parametrize('flag', [False, True])
def test_successful_completion_records_stop(waves, flag):
    result = run_waves(waves, continue_untouched=flag)
    assert result['status'] == 'selected_cases_complete'
    assert waves['calls'] == [['0', '1'], ['2', '3'], ['4', '5']]
    assert_stop_record(waves, StopDisposition.COMPLETE, None, [], [], flag)


def test_already_completed_cases_record_stop_without_launch(waves):
    for row in waves['summary']['cases']:
        row['status'] = 'COMPLETED'
    result = run_waves(waves)
    assert result['status'] == 'selected_cases_complete'
    assert waves['calls'] == []
    assert_stop_record(waves, StopDisposition.COMPLETE, None, [], [], False)


@pytest.mark.parametrize('flag', [False, True])
def test_pause_before_first_wave_records_stop(waves, flag):
    (waves['root'] / 'STOP_AFTER_WAVE').write_text('pause', encoding='utf-8')
    result = run_waves(waves, continue_untouched=flag)
    assert result['status'] == 'paused'
    assert waves['calls'] == []
    assert_stop_record(waves, StopDisposition.NEEDS_DIAGNOSIS,
                       'paused by STOP_AFTER_WAVE', [],
                       ['0', '1', '2', '3', '4', '5'], flag)


def test_pause_after_isolated_failure_prevents_next_wave(waves):
    waves['failures'].add(1)
    waves['after_wave'].append(lambda: (waves['root'] / 'STOP_AFTER_WAVE').write_text(
        'pause', encoding='utf-8'))
    result = run_waves(waves, continue_untouched=True)
    assert result['status'] == 'paused'
    assert waves['calls'] == [['0', '1']]
    assert_stop_record(waves, StopDisposition.NEEDS_DIAGNOSIS,
                       'paused by STOP_AFTER_WAVE', ['1'], ['2', '3', '4', '5'], True)


def test_stop_record_maps_all_case_statuses_and_string_ids(waves):
    statuses = ['COMPLETED', 'FAILED', 'RUNNING', 'MISSING', 'PENDING', 'OTHER']
    for row, status in zip(waves['summary']['cases'], statuses):
        row['status'] = status
    result = run_waves(waves, case_indices=[0], continue_untouched=True)
    assert result['status'] == 'selected_cases_complete'
    assert waves['calls'] == []
    assert_stop_record(waves, StopDisposition.FAILED_CASES_ISOLATED, None,
                       ['1'], ['3', '4', '5'], True)


@pytest.mark.parametrize('flag', [False, True])
def test_run_parallel_campaign_forwards_continuation(tmp_path, monkeypatch, flag):
    monkeypatch.setattr(parallel, '_validate_resources', lambda *args: None)
    monkeypatch.setattr(parallel.campaign, '_validate_timeout', lambda value: value)
    monkeypatch.setattr(parallel.campaign, '_read', lambda path: {'cases': [{}, {}]})
    monkeypatch.setattr(parallel.campaign, '_manifest', lambda *args: {})
    monkeypatch.setattr(parallel, '_resources', lambda cpus: nullcontext())
    monkeypatch.setattr(parallel, '_ownership', lambda root: nullcontext())
    summary = {'cases': []}
    monkeypatch.setattr(parallel, '_initialize', lambda *args: summary)
    for key in parallel._THREAD_VARIABLES:
        monkeypatch.setenv(key, '1')

    class Process:
        def cpu_affinity(self):
            return [0, 1]

    monkeypatch.setattr(parallel.psutil, 'Process', Process)
    received = []

    def fake_waves(root, result, config, workers, case_indices=None,
                   continue_untouched=False):
        received.append((workers, case_indices, continue_untouched))
        return result

    monkeypatch.setattr(parallel, '_waves', fake_waves)
    kwargs = {'continue_untouched': True} if flag else {}
    result = parallel.run_parallel_campaign(
        tmp_path / 'study', tmp_path / 'output', source_campaign=tmp_path / 'frozen.json',
        workers=2, cpus=[0, 1], extraction={}, case_indices=[1], **kwargs)
    assert result is summary
    assert received == [(2, [1], flag)]


@pytest.mark.parametrize('flag', [False, True])
@pytest.mark.parametrize('status,exit_code', [
    ('selected_cases_complete', 0), ('paused', 0), ('stopped', 1),
    ('completed_with_isolated_failures', 2)])
def test_main_parses_continuation_and_returns_exit_code(
        tmp_path, monkeypatch, capsys, flag, status, exit_code):
    extraction = tmp_path / 'extraction.yml'
    extraction.write_text('{}', encoding='utf-8')
    argv = ['installation-parallel', str(tmp_path / 'study'), str(tmp_path / 'output'),
            '--source-campaign', str(tmp_path / 'frozen.json'), '--cpus', '0,1',
            '--extraction-config', str(extraction)]
    if flag:
        argv.append('--continue-untouched')
    monkeypatch.setattr(sys, 'argv', argv)
    received = []

    def fake_run(*args, **kwargs):
        received.append(kwargs)
        return {'status': status}

    monkeypatch.setattr(parallel, 'run_parallel_campaign', fake_run)
    assert parallel.main() == exit_code
    assert len(received) == 1
    assert received[0]['continue_untouched'] is flag
    assert json.loads(capsys.readouterr().out)['status'] == status
