"""Bounded installation waves with one registry writer and an explicit CPU set.

The old launcher and descendants must be stopped before this adapter is used.
A frozen handover records completed evidence and preserves incomplete attempts.
"""
from __future__ import annotations

import argparse
from contextlib import contextmanager
import copy
import hashlib
import json
import os
from pathlib import Path
import time

import psutil
import yaml

from digitalmodel.solvers.orcaflex.orcaflex_parallel_analysis import OrcaFlexParallelAnalysis
from digitalmodel.workflows import installation_campaign as campaign


_THREAD_VARIABLES = ('OMP_NUM_THREADS', 'OPENBLAS_NUM_THREADS',
                     'MKL_NUM_THREADS', 'NUMEXPR_NUM_THREADS')


def _validate_resources(workers, cpus):
    if type(workers) is not int or workers < 1:
        raise ValueError('workers must be a positive integer')
    if not cpus or any(type(cpu) is not int or cpu < 0 for cpu in cpus):
        raise ValueError('cpus must be explicit nonnegative integer CPU identifiers')
    if len(set(cpus)) != len(cpus) or workers > len(cpus):
        raise ValueError('Unique CPUs and workers <= CPU count are required')
    if not set(cpus).issubset(psutil.Process().cpu_affinity()):
        raise ValueError('Requested CPUs are outside current process affinity')


def _set_affinity(cpus):
    process = psutil.Process()
    process.cpu_affinity(list(cpus))
    observed = process.cpu_affinity()
    if set(observed) != set(cpus):
        raise RuntimeError('Requested process affinity was not applied')
    return observed


@contextmanager
def _resources(cpus):
    original_cpus = psutil.Process().cpu_affinity()
    original_env = {key: os.environ.get(key) for key in _THREAD_VARIABLES}
    try:
        _set_affinity(cpus)
        for key in _THREAD_VARIABLES:
            os.environ[key] = '1'
        yield
    finally:
        psutil.Process().cpu_affinity(original_cpus)
        for key, value in original_env.items():
            if value is None:
                os.environ.pop(key, None)
            else:
                os.environ[key] = value


@contextmanager
def _ownership(root):
    root.mkdir(parents=True, exist_ok=True)
    lock = root / 'campaign.lock'
    with lock.open('x', encoding='utf-8') as stream:
        stream.write(json.dumps({'pid': os.getpid(), 'created_unix': time.time()}))
    try:
        yield
    finally:
        lock.unlink()


def _validate_rows(summary, manifest):
    rows = summary.get('cases', [])
    if len(rows) != len(manifest['cases']):
        raise ValueError('Campaign case count differs from matrix')
    for index, (row, case) in enumerate(zip(rows, manifest['cases'])):
        if row.get('index') != index or any(row.get(k) != case[k] for k in ('hs_m', 'tp_s', 'seed')):
            raise ValueError('Campaign case coordinates/order differ from matrix')


def _verify_completed(summary, manifest, solver_version):
    for row in summary['cases']:
        if row['status'] != 'COMPLETED':
            continue
        run, generation = Path(row['run_dir']), Path(row['generation_file'])
        receipt = campaign._verify_run(run, generation, manifest, row['index'], solver_version)
        campaign._ensure_traces(run, receipt)


def _initialize(study, root, source, manifest, solver_version):
    source_bytes = source.read_bytes()
    frozen = json.loads(source_bytes)
    digest = hashlib.sha256(source_bytes).hexdigest()
    matrix_hash = campaign.compute_hash(study / 'matrix.json')
    if frozen.get('matrix_sha256') != matrix_hash:
        raise ValueError('Frozen source belongs to a different matrix revision')
    _validate_rows(frozen, manifest)
    if any(row.get('status') not in ('COMPLETED', 'MISSING') for row in frozen['cases']):
        raise ValueError('Source must be frozen with COMPLETED or MISSING rows')
    _verify_completed(frozen, manifest, solver_version)
    if (root / 'campaign.json').exists():
        summary = campaign._summary(manifest, study, root)
        if summary.get('source_campaign_sha256') != digest:
            raise ValueError('Frozen source changed since campaign creation')
        _validate_rows(summary, manifest)
        _verify_completed(summary, manifest, solver_version)
        return summary
    summary = copy.deepcopy(frozen)
    summary.update(status='prepared', source_campaign=str(source),
                   source_campaign_sha256=digest, engineering_acceptance='NOT EVALUATED')
    for row in summary['cases']:
        if row['status'] == 'MISSING':
            for key in ('run_dir', 'generation_file', 'disposition', 'error'):
                if key in row:
                    raise ValueError('Frozen missing rows must move old run references into previous_attempt')
    return summary


class InstallationCasePool(OrcaFlexParallelAnalysis):
    """Reuse the existing process executor while retaining case-level receipts."""

    def process_single_file(self, file_info):
        started = time.perf_counter()
        config = file_info['config']
        index = int(file_info['file_path'])
        row = copy.deepcopy(config['rows'][index])
        result = {'file_path': file_info['file_path'], 'status': 'failed', 'row': row}
        try:
            result['observed_cpu_affinity'] = _set_affinity(config['cpus'])
            campaign._execute(index, config['manifest'], Path(config['root']),
                Path(config['study']), config['extraction'], None,
                config['solver_version'], row, config['timeout_seconds'])
            result['status'] = 'success'
        except Exception as error:
            row.update(status='FAILED', error=f'{type(error).__name__}: {error}')
            result['error'] = row['error']
        result['duration'] = time.perf_counter() - started
        row['last_operation_seconds'] = result['duration']
        row['storage_bytes'] = campaign._bytes(Path(row['run_dir'])) if row.get('run_dir') else 0
        return result


def _merge_wave(summary, indices, results):
    by_index = {}
    for result in results:
        index = int(result['file_path'])
        if index not in indices or index in by_index:
            raise ValueError('Executor returned unexpected or duplicate case results')
        by_index[index] = result
    for index in indices:
        result = by_index.get(index, {})
        row = result.get('row', summary['cases'][index])
        if result.get('status') != 'success' or row.get('status') != 'COMPLETED':
            row.update(status='FAILED', error=result.get('error', 'Missing or failed worker result'))
            summary['status'] = 'stopped'
        row['worker_cpu_affinity'] = result.get('observed_cpu_affinity')
        summary['cases'][index] = row


def _waves(root, summary, config, workers, case_indices=None):
    selected = set(case_indices) if case_indices is not None else None
    pending = [row['index'] for row in summary['cases']
               if row['status'] != 'COMPLETED'
               and (selected is None or row['index'] in selected)]
    summary['selected_indices'] = pending
    started = time.perf_counter()
    for offset in range(0, len(pending), workers):
        if (root / 'STOP_AFTER_WAVE').exists():
            summary['status'] = 'paused'
            break
        indices = pending[offset:offset + workers]
        summary['status'] = 'running'
        for index in indices:
            summary['cases'][index]['status'] = 'RUNNING'
        campaign._save(root, summary)
        wave_config = dict(config, rows={i: summary['cases'][i] for i in indices})
        pool = InstallationCasePool(num_threads=workers, use_processes=True)
        results = pool.process_files_parallel([str(i) for i in indices], wave_config)
        _merge_wave(summary, indices, results.get('results', []))
        summary['invocation_seconds'] = time.perf_counter() - started
        campaign._save(root, summary)
        if summary['status'] == 'stopped':
            break
    else:
        summary['status'] = 'selected_cases_complete'
    campaign._save(root, summary)
    return summary


def run_parallel_campaign(study_dir, output_root, *, source_campaign, workers=3,
                          cpus, extraction, solver_version='11.6c', timeout_seconds=14400,
                          case_indices=None):
    """Run only unfinished cases after a separately verified serial handover."""
    _validate_resources(workers, cpus)
    timeout = campaign._validate_timeout(timeout_seconds)
    study, root = Path(study_dir).resolve(), Path(output_root).resolve()
    source = Path(source_campaign).resolve()
    if source.is_relative_to(root) or study.is_relative_to(root) or root.is_relative_to(study):
        raise ValueError('Output root must be separate from frozen source and input study')
    matrix = campaign._read(study / 'matrix.json')
    indices = list(range(len(matrix['cases']))) if case_indices is None else case_indices
    manifest = campaign._manifest(study, indices)
    with _resources(cpus), _ownership(root):
        summary = _initialize(study, root, source, manifest, solver_version)
        summary['execution_policy'] = {'workers': workers, 'cpu_ids': list(cpus),
            'observed_coordinator_affinity': psutil.Process().cpu_affinity(),
            'native_threads_per_model': 1, 'timeout_seconds': timeout,
            'inner_batch_workers': 1, 'thread_environment': {k: os.environ[k] for k in _THREAD_VARIABLES}}
        config = dict(manifest=manifest, root=str(root), study=str(study), cpus=list(cpus),
                      extraction=extraction, solver_version=solver_version, timeout_seconds=timeout)
        return _waves(root, summary, config, workers, case_indices)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('study_dir', type=Path)
    parser.add_argument('output_root', type=Path)
    parser.add_argument('--source-campaign', required=True, type=Path)
    parser.add_argument('--case-indices', help='Optional explicit comma-separated subset; other rows remain unchanged')
    parser.add_argument('--workers', type=int, default=3)
    parser.add_argument('--cpus', required=True, help='Explicit comma-separated logical CPU IDs')
    parser.add_argument('--extraction-config', required=True, type=Path)
    parser.add_argument('--timeout-seconds', type=float, default=14400)
    parser.add_argument('--solver-version', default='11.6c')
    args = parser.parse_args()
    extraction = yaml.safe_load(args.extraction_config.read_text(encoding='utf-8'))
    if not isinstance(extraction, dict):
        parser.error('extraction-config must contain the extraction mapping')
    result = run_parallel_campaign(args.study_dir, args.output_root,
        source_campaign=args.source_campaign, workers=args.workers,
        cpus=[int(cpu) for cpu in args.cpus.split(',')], extraction=extraction,
        solver_version=args.solver_version, timeout_seconds=args.timeout_seconds,
        case_indices=None if args.case_indices is None else [int(i) for i in args.case_indices.split(',')])
    print(json.dumps({'status': result['status'], 'campaign': str(args.output_root / 'campaign.json')}))
    return 1 if result['status'] == 'stopped' else 0


if __name__ == '__main__':
    raise SystemExit(main())
