"""Run pinned requests after a baseline coordinator releases its CPU capacity.

The input manifest is immutable. Failed waves drain without further dispatch;
the baseline is never resumed by this adapter. No engineering acceptance is set.
"""
from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
import re
import time

import psutil
import yaml

from digitalmodel.workflows import installation_parallel_campaign as parallel
from digitalmodel.workflows.orcaflex_reproduce import compute_hash, reproduce


def _read(path):
    return json.loads(Path(path).read_text(encoding='utf-8-sig'))


def _digest(path, expected):
    if not isinstance(expected, str) or not re.fullmatch(r'[0-9a-f]{64}', expected):
        raise ValueError('Expected a pinned SHA-256 digest')
    if compute_hash(path) != expected:
        raise ValueError(f'Input digest mismatch: {path}')


def _within(root, relative):
    value = Path(relative)
    path = (root / value).resolve()
    if value.is_absolute() or not path.is_relative_to(root) or path == root:
        raise ValueError('Input path must remain within artifact root')
    return path


def _inputs(manifest_path, manifest_sha256, artifact_manifest, artifact_manifest_sha256):
    manifest, artifacts = Path(manifest_path).resolve(), Path(artifact_manifest).resolve()
    _digest(artifacts, artifact_manifest_sha256)
    _digest(manifest, manifest_sha256)
    root = artifacts.parent
    pinned = {}
    for entry in _read(artifacts)['files']:
        path = _within(root, entry['path'])
        if path in pinned:
            raise ValueError('Duplicate artifact entry')
        pinned[path] = entry['sha256']
    if pinned.get(manifest) != manifest_sha256:
        raise ValueError('Matched manifest must belong to pinned artifact manifest')
    rows = []
    for case in _read(manifest)['cases']:
        name = case['id']
        if not isinstance(name, str) or not re.fullmatch(r'[A-Za-z0-9_-]+', name):
            raise ValueError('Unsafe case identifier')
        request = _within(root, case['request'])
        _digest(request, pinned.get(request))
        config = yaml.safe_load(request.read_text(encoding='utf-8-sig'))
        model = _within(request.parent, config['model'])
        expected = case['model_sha256']
        if config['model_sha256'] != expected or pinned.get(model) != expected:
            raise ValueError('Model digest declarations differ')
        _digest(model, expected)
        rows.append(dict(id=name, request=str(request), request_sha256=pinned[request],
                         model=str(model), model_sha256=expected, status='waiting'))
    if len(rows) != 5 or len({row['id'] for row in rows}) != 5:
        raise ValueError('Exactly five unique matched cases are required')
    return rows


def _live_identity(identity):
    try:
        process = psutil.Process(identity['pid'])
        if abs(process.create_time() - identity['create_time']) > 1e-3:
            raise RuntimeError(f"Tracked PID reused: {identity['pid']}")
        return process
    except psutil.NoSuchProcess:
        return None


def _capacity_ready(baseline, summary):
    """Retain discovered descendants even after their original parent exits."""
    identities = summary['tracked_processes']
    active = False
    for identity in identities:
        process = _live_identity(identity)
        if process is None:
            continue
        active = True
        try:
            children = process.children(recursive=True)
        except psutil.NoSuchProcess:
            continue
        for child in children:
            try:
                found = dict(pid=child.pid, create_time=child.create_time())
            except psutil.NoSuchProcess:
                continue
            if found not in identities:
                identities.append(found)
    state = _read(baseline / 'campaign.json')
    return not active and state.get('status') == 'paused' and not (baseline / 'campaign.lock').exists()


def _wait(baseline, root, summary, timeout, poll):
    started = time.monotonic()
    while True:
        ready = _capacity_ready(baseline, summary)
        summary['waiting_seconds'] = time.monotonic() - started
        parallel.campaign._save(root, summary)
        if ready:
            return
        if timeout is not None and summary['waiting_seconds'] >= timeout:
            raise TimeoutError('Baseline has not released capacity')
        time.sleep(poll)


class PriorityRequestPool(parallel.OrcaFlexParallelAnalysis):
    """Keep existing process-pool scheduling and standalone reproduction."""

    def process_single_file(self, file_info):
        started = time.perf_counter()
        config = file_info['config']
        row = dict(config['rows'][file_info['file_path']])
        result = dict(file_path=file_info['file_path'], status='failed', row=row)
        try:
            row['worker_cpu_affinity'] = parallel._set_affinity(config['cpus'])
            _digest(Path(row['request']), row['request_sha256'])
            _digest(Path(row['model']), row['model_sha256'])
            receipt = reproduce(Path(row['request']), Path(row['run_dir']))
            if receipt.get('status') != 'completed':
                raise ValueError('Reproduction did not complete')
            run = Path(row['run_dir'])
            parallel.campaign._ensure_traces(run, receipt)
            metadata_path = run / 'installation_traces' / 'metadata.json'
            metadata = _read(metadata_path)
            row.update(trace_sha256=metadata['trace_sha256'],
                       supplemental_profile_sha256=metadata.get('supplemental_profile_sha256'),
                       trace_metadata_sha256=compute_hash(metadata_path))
            _digest(Path(row['request']), row['request_sha256'])
            _digest(Path(row['model']), row['model_sha256'])
            row['status'] = 'completed'
            result['status'] = 'success'
        except Exception as error:
            row.update(status='failed', error=f'{type(error).__name__}: {error}')
            result['error'] = row['error']
        result['duration'] = time.perf_counter() - started
        row['duration_seconds'] = result['duration']
        return result


def _merge(summary, selected, results):
    by_id = {}
    for result in results:
        name = result['file_path']
        if name not in selected or name in by_id:
            raise ValueError('Unexpected or duplicate worker result')
        by_id[name] = result
    for index, row in enumerate(summary['cases']):
        if row['id'] not in selected:
            continue
        result = by_id.get(row['id'], {})
        updated = result.get('row', row)
        if result.get('status') != 'success' or updated.get('status') != 'completed':
            updated.update(status='failed', error=result.get('error', 'Missing worker result'))
            summary['status'] = 'failed'
        summary['cases'][index] = updated


def _dispatch(root, baseline, summary, workers, cpus, inputs):
    for offset in range(0, len(summary['cases']), workers):
        if not _capacity_ready(baseline, summary):
            raise RuntimeError('Baseline capacity changed before dispatch')
        _inputs(**inputs)
        rows = summary['cases'][offset:offset + workers]
        summary['status'] = 'running'
        for row in rows:
            row.update(status='running', run_dir=str(root / 'runs' / row['id']))
        parallel.campaign._save(root, summary)
        config = dict(rows={row['id']: row for row in rows}, cpus=cpus)
        pool = PriorityRequestPool(num_threads=workers, use_processes=True)
        results = pool.process_files_parallel(list(config['rows']), config)
        _merge(summary, config['rows'], results.get('results', []))
        parallel.campaign._save(root, summary)
        if summary['status'] == 'failed':
            return
    summary['status'] = 'completed'


def _validate_options(root, baseline, inputs, identities, workers, cpus, poll, timeout):
    if type(workers) is not int or not 1 <= workers <= 3 or cpus != [60, 61, 62]:
        raise ValueError('Use at most three workers and explicit CPUs [60, 61, 62]')
    parallel._validate_resources(workers, cpus)
    if not math.isfinite(poll) or not 0 < poll <= 30:
        raise ValueError('poll_seconds must be in (0, 30]')
    if timeout is not None and (not math.isfinite(timeout) or timeout < 0):
        raise ValueError('Wait timeout must be nonnegative and finite')
    if not identities:
        raise ValueError('Coordinator identity is required')
    for item in identities:
        if (type(item['pid']) is not int or item['pid'] <= 0 or
                not math.isfinite(item['create_time']) or item['create_time'] <= 0):
            raise ValueError('Invalid process identity')
    for source in (baseline, Path(inputs['artifact_manifest']).resolve().parent):
        if root.is_relative_to(source) or source.is_relative_to(root):
            raise ValueError('Output root must be disjoint from baseline and input tree')
    if root.exists():
        raise FileExistsError('Priority output root already exists')


def run_priority_requests(manifest_path, output_root, *, manifest_sha256,
                          artifact_manifest, artifact_manifest_sha256, baseline_root,
                          identities, workers=3, cpus, poll_seconds=15,
                          wait_timeout_seconds=None):
    """Wait for a pinned baseline process tree, then run five immutable requests."""
    root, baseline = Path(output_root).resolve(), Path(baseline_root).resolve()
    inputs = dict(manifest_path=str(Path(manifest_path).resolve()), manifest_sha256=manifest_sha256,
                  artifact_manifest=str(Path(artifact_manifest).resolve()),
                  artifact_manifest_sha256=artifact_manifest_sha256)
    _validate_options(root, baseline, inputs, identities, workers, cpus,
                      poll_seconds, wait_timeout_seconds)
    rows = _inputs(**inputs)
    summary = dict(status='waiting', cases=rows, pinned_inputs=inputs,
                   baseline_root=str(baseline), tracked_processes=[dict(i) for i in identities],
                   engineering_acceptance='NOT EVALUATED',
                   execution_policy=dict(workers=workers, cpu_ids=cpus,
                       native_threads_per_model=1, inner_batch_workers=1, auto_resume_baseline=False))
    with parallel._ownership(root):
        try:
            _wait(baseline, root, summary, wait_timeout_seconds, poll_seconds)
            with parallel._resources(cpus):
                _dispatch(root, baseline, summary, workers, cpus, inputs)
        except BaseException as error:
            summary.update(status='failed', error=f'{type(error).__name__}: {error}')
            raise
        finally:
            parallel.campaign._save(root, summary)
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('config', type=Path, help='JSON keyword arguments for run_priority_requests')
    args = parser.parse_args()
    config = _read(args.config)
    result = run_priority_requests(**config)
    print(json.dumps({'status': result['status'], 'output_root': config['output_root']}))
    return 0 if result['status'] == 'completed' else 1


if __name__ == '__main__':
    raise SystemExit(main())
