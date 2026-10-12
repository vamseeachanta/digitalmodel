"""Explicit, serial installation case execution using the existing runner.

This module never selects a whole matrix implicitly and never assigns GO.
Existing failed runs are preserved. Any missing/invalid evidence stops the
requested sequence; another invocation can select different untouched cases.
"""
from __future__ import annotations

import argparse
import copy
import json
from pathlib import Path
import time

import yaml

from digitalmodel.infrastructure.persistence.provenance import compute_hash
from digitalmodel.workflows.installation_seastates import materialize_case, _validate_timeout
from digitalmodel.workflows.orcaflex_reproduce import reproduce, _load_api


def _read(path):
    return json.loads(Path(path).read_text(encoding='utf-8'))


def _hash(path, expected):
    if not expected or not Path(path).is_file() or compute_hash(path) != expected:
        raise ValueError(f'Missing or changed evidence: {path}')


def _manifest(study, indices):
    data = _read(study / 'matrix.json')
    cases = data['cases']
    if not indices or any(type(i) is not int or i < 0 or i >= len(cases) for i in indices):
        raise ValueError('Explicit nonempty in-range case indices required')
    if len(set(indices)) != len(indices):
        raise ValueError('Duplicate case indices')
    master = (study / data['master_file']).resolve()
    if not master.is_relative_to(study):
        raise ValueError('Master escapes study directory')
    _hash(master, data['master_sha256'])
    for case in cases:
        change = (study / case['change_file']).resolve()
        if not change.is_relative_to(study):
            raise ValueError('Change escapes study directory')
        _hash(change, case['change_sha256'])
    return data


def _verify_run(run, generation_path, manifest, index, solver_version):
    generation, receipt = _read(generation_path), _read(run / 'run.json')
    dep, case = generation['dependencies'], manifest['cases'][index]
    if any(generation.get('settings', {}).get(k) != case[k] for k in ('hs_m', 'tp_s', 'seed')):
        raise ValueError('Generation settings differ from selected case')
    if dep['master_sha256'] != manifest['master_sha256'] or dep['change_sha256'] != case['change_sha256']:
        raise ValueError('Run dependencies differ from selected case')
    if receipt.get('status') != 'completed' or receipt.get('extraction', {}).get('status') != 'complete':
        raise ValueError('Existing run or extraction is not complete')
    if receipt.get('solver_version') != solver_version:
        raise ValueError('Existing solver version differs')
    expected = generation['model_sha256']
    if receipt.get('model_sha256') != expected:
        raise ValueError('Run model differs from generated case')
    _hash(run / 'source/model.yml', expected)
    _hash(run / 'batch_runs/sims/model.sim', receipt.get('simulation_sha256'))
    _hash(run / 'request.yml', receipt.get('request_sha256'))
    return receipt


def _extract_traces(run):
    from digitalmodel.workflows.installation_trace_extract import extract
    return extract(run)


def _ensure_traces(run, receipt):
    directory = run / 'installation_traces'
    if not directory.exists():
        _extract_traces(run)
    metadata = _read(directory / 'metadata.json')
    from digitalmodel.workflows.installation_trace_extract import verify_profile_metadata
    verify_profile_metadata(run, receipt, metadata)
    if metadata.get('simulation_sha256') != receipt['simulation_sha256'] or not metadata.get('channels'):
        raise ValueError('Supplemental trace metadata does not identify the simulation/channels')
    _hash(directory / 'traces.npz', metadata.get('trace_sha256'))
    from digitalmodel.workflows.installation_trace_extract import verify_profile_arrays
    verify_profile_arrays(directory / 'traces.npz', receipt, metadata)


def _summary(manifest, study, root):
    digest = compute_hash(study / 'matrix.json')
    path = root / 'campaign.json'
    if path.exists():
        old = _read(path)
        if old.get('matrix_sha256') != digest:
            raise ValueError('Existing campaign belongs to a different matrix revision')
        return old
    return {'status': 'prepared', 'matrix_sha256': digest, 'master_sha256': manifest['master_sha256'],
            'engineering_acceptance': 'NOT EVALUATED', 'cases': [
                {'index': i, 'hs_m': c['hs_m'], 'tp_s': c['tp_s'], 'seed': c['seed'],
                 'status': 'MISSING', 'engineering_acceptance': 'NOT EVALUATED'}
                for i, c in enumerate(manifest['cases'])]}


def _save(root, summary):
    path = root / 'campaign.json'
    temporary = root / 'campaign.pending.json'
    temporary.write_text(json.dumps(summary, indent=2, allow_nan=False), encoding='utf-8')
    if _read(temporary) != summary:
        raise ValueError('Campaign readback mismatch')
    temporary.replace(path)


def _bytes(root):
    return sum(p.stat().st_size for p in root.rglob('*') if p.is_file()) if root.exists() else 0


def _pilot(pilot_run, pilot_generation, manifest, solver_version):
    if pilot_run is None:
        return None
    if pilot_generation is None:
        raise ValueError('Pilot reuse requires its generation.json dependency record')
    generation = _read(pilot_generation)
    matches = [i for i, c in enumerate(manifest['cases'])
               if c['change_sha256'] == generation['dependencies']['change_sha256']]
    if len(matches) != 1:
        raise ValueError('Pilot does not identify one matrix case')
    receipt = _verify_run(Path(pilot_run), Path(pilot_generation), manifest, matches[0], solver_version)
    _ensure_traces(Path(pilot_run), receipt)
    return matches[0]


def _execute(index, manifest, root, study, extraction, api, solver_version, row, timeout_seconds):
    if row.get('disposition') == 'verified_pilot':
        receipt = _verify_run(Path(row['run_dir']), Path(row['generation_file']), manifest, index, solver_version)
        _ensure_traces(Path(row['run_dir']), receipt)
        row['status'] = 'COMPLETED'
        return api
    prepared, run = root / 'prepared' / f'case_{index:03d}', root / 'runs' / f'case_{index:03d}'
    row.update(run_dir=str(run), generation_file=str(prepared / 'generation.json'))
    if run.exists():
        receipt = _verify_run(run, prepared / 'generation.json', manifest, index, solver_version)
        _ensure_traces(run, receipt)
        row.update(status='COMPLETED', disposition='verified_existing')
        return api
    if prepared.exists():
        raise ValueError('Prepared directory without completed run is preserved; select a fresh campaign root')
    if api is None:
        api, identity = _load_api({'solver_version': solver_version})
        if identity['resolved_version'] != solver_version:
            raise ValueError('Resolved solver version differs from campaign request')
    extraction = copy.deepcopy(extraction)
    case = manifest['cases'][index]
    extraction['title'] = f"Installation screening: Hs {case['hs_m']:g} m, Tp {case['tp_s']:g} s, seed {case['seed']}"
    materialize_case(api, study, index, prepared, extraction=extraction,
                     solver_version=solver_version, timeout_seconds=timeout_seconds)
    reproduce(prepared / 'request.yml', run)
    receipt = _verify_run(run, prepared / 'generation.json', manifest, index, solver_version)
    _ensure_traces(run, receipt)
    row.update(status='COMPLETED', disposition='executed', warnings=receipt.get('warnings', []))
    return api


def run_campaign(study_dir, output_root, case_indices, *, extraction, api=None,
                 pilot_run=None, pilot_generation=None, solver_version='11.6c', timeout_seconds=1800):
    """Execute only selected cases serially; stop on first failed evidence check."""
    timeout_seconds = _validate_timeout(timeout_seconds)
    study, root = Path(study_dir).resolve(), Path(output_root).resolve()
    manifest = _manifest(study, case_indices)
    summary = _summary(manifest, study, root)
    pilot_index = _pilot(pilot_run, pilot_generation, manifest, solver_version)
    root.mkdir(parents=True, exist_ok=True)
    summary.update(status='running', selected_indices=list(case_indices))
    _save(root, summary)
    started = time.perf_counter()
    for index in case_indices:
        row, start = summary['cases'][index], time.perf_counter()
        row['status'] = 'RUNNING'
        _save(root, summary)
        try:
            if index == pilot_index:
                row.update(status='COMPLETED', disposition='verified_pilot',
                           run_dir=str(Path(pilot_run).resolve()), generation_file=str(Path(pilot_generation).resolve()))
            else:
                api = _execute(index, manifest, root, study, extraction, api, solver_version, row, timeout_seconds)
        except Exception as error:
            row.update(status='FAILED', error=f'{type(error).__name__}: {error}')
            summary['status'] = 'stopped'
        row['last_operation_seconds'] = time.perf_counter() - start
        row['storage_bytes'] = _bytes(Path(row['run_dir'])) if row.get('run_dir') else 0
        summary['invocation_seconds'] = time.perf_counter() - started
        _save(root, summary)
        if summary['status'] == 'stopped':
            return summary
    summary['status'] = 'selected_cases_complete'
    _save(root, summary)
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('study_dir', type=Path)
    parser.add_argument('output_root', type=Path)
    parser.add_argument('--case-indices', required=True, help='Explicit comma-separated indices')
    parser.add_argument('--extraction-config', type=Path)
    parser.add_argument('--pilot-run', type=Path)
    parser.add_argument('--pilot-generation', type=Path)
    parser.add_argument('--solver-version', default='11.6c')
    parser.add_argument('--timeout-seconds', type=float, default=1800,
                        help='Positive finite solver timeout per new case (default: 1800)')
    args = parser.parse_args()
    if args.extraction_config:
        extraction = yaml.safe_load(args.extraction_config.read_text(encoding='utf-8'))
    elif args.pilot_run:
        extraction = yaml.safe_load((args.pilot_run / 'request.yml').read_text(encoding='utf-8'))['extraction']
    else:
        parser.error('--extraction-config or a verified --pilot-run is required')
    result = run_campaign(args.study_dir, args.output_root,
        [int(i) for i in args.case_indices.split(',')], extraction=extraction,
        pilot_run=args.pilot_run, pilot_generation=args.pilot_generation,
        solver_version=args.solver_version, timeout_seconds=args.timeout_seconds)
    print(json.dumps({'status': result['status'], 'campaign': str(args.output_root / 'campaign.json')}))
    return 1 if result['status'] == 'stopped' else 0


if __name__ == '__main__':
    raise SystemExit(main())
