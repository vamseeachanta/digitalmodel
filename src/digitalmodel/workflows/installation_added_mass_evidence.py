"""Verify completed bracket evidence without invoking OrcaFlex."""
import copy
from hashlib import sha256
import io
import json
import math
from pathlib import Path

import numpy as np
import yaml

from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader
from digitalmodel.infrastructure.persistence.provenance import compute_hash
from digitalmodel.workflows.installation_priority_requests import _inputs
from digitalmodel.workflows.vessel_capability_report import _audit_profile


def pinned_json(path, digest):
    raw = Path(path).read_bytes()
    if sha256(raw).hexdigest() != digest:
        raise ValueError('Pinned JSON digest mismatch')
    return json.loads(raw)


def verify_generation(generation, case, bracket_sha, master_sha):
    if generation.get('status') != 'native_verified_not_run' or generation.get('case_id') != case['id']:
        raise ValueError('Native generation identity/status mismatch')
    for key in ('factor', 'coefficient_z', 'added_mass_t', 'settings'):
        if generation.get(key) != case[key]:
            raise ValueError(f'Native generation differs from bracket: {key}')
    expected = dict(manifest_sha256=bracket_sha, master_sha256=master_sha, change_sha256=case['change_sha256'])
    if any(generation.get('dependencies', {}).get(key) != value for key, value in expected.items()):
        raise ValueError('Native generation dependency mismatch')


def audit_row(identity, run, receipt, metadata, metadata_sha):
    return dict(index=identity, run_dir=str(run), metadata_sha256=metadata_sha,
                simulation_sha256=receipt['simulation_sha256'], trace_sha256=metadata['trace_sha256'],
                channels=metadata['channels'])


def _file(path, digest, evidence, large=False):
    path = Path(path).resolve()
    before = path.stat()
    raw = None if large else path.read_bytes()
    actual = compute_hash(path) if large else sha256(raw).hexdigest()
    after = path.stat()
    if actual != digest or (before.st_size, before.st_mtime_ns) != (after.st_size, after.st_mtime_ns):
        raise ValueError(f'Changed evidence digest: {path.name}')
    evidence.append(dict(path=str(path), sha256=actual, bytes=after.st_size,
                         mtime_ns=after.st_mtime_ns, large_file=large))
    return raw


def _snapshot(path, evidence):
    raw = Path(path).read_bytes()
    return _file(path, sha256(raw).hexdigest(), evidence)


def _batch_rows(sequence, evidence):
    if sequence.get('status') != 'completed' or len(sequence.get('jobs', [])) != 3:
        raise ValueError('Three completed batches required')
    results = sequence.get('batch_results', [])
    if len(results) != 3 or {r['index'] for r in results} != {0, 1, 2}:
        raise ValueError('Complete unique batch indices required')
    rows = []
    for entry in results:
        config = sequence['jobs'][entry['index']]['config_args']
        if (Path(config['output_root'])/'campaign.lock').exists():
            raise ValueError('Campaign coordinator lock still present')
        campaign = json.loads(_snapshot(Path(config['output_root'])/'campaign.json', evidence))
        if campaign != entry['result'] or campaign.get('status') != 'completed':
            raise ValueError('Live campaign differs from pinned completed sequence')
        if len(campaign['cases']) != 5 or len({r['id'] for r in campaign['cases']}) != 5:
            raise ValueError('Five unique completed cases required per batch')
        inputs = {k: config[k] for k in ('manifest_path', 'manifest_sha256', 'artifact_manifest', 'artifact_manifest_sha256')}
        verified = {r['id']: r for r in _inputs(**inputs)}
        artifacts = json.loads(_file(config['artifact_manifest'], config['artifact_manifest_sha256'], evidence))
        _file(config['manifest_path'], config['manifest_sha256'], evidence)
        root = Path(config['artifact_manifest']).resolve().parent
        pins = {(root/r['path']).resolve(): r['sha256'] for r in artifacts['files']}
        for row in campaign['cases']:
            expected = verified.get(row['id'])
            if row.get('status') != 'completed' or expected is None:
                raise ValueError('Unverified or incomplete campaign case')
            if any(row[k] != expected[k] for k in ('request', 'request_sha256', 'model', 'model_sha256')):
                raise ValueError('Campaign and artifact identity differ')
            rows.append((row, pins))
    if len({Path(r['run_dir']).resolve() for r, _ in rows}) != len(rows):
        raise ValueError('Duplicate native run paths')
    return rows


def _run_inputs(row, pins, case, bracket, bracket_sha, evidence):
    prepared = Path(row['request']).resolve().parent
    generation_path = prepared/'generation.json'
    if generation_path not in pins:
        raise ValueError('Generation receipt is not pinned by artifact manifest')
    generation = json.loads(_file(generation_path, pins[generation_path], evidence))
    verify_generation(generation, case, bracket_sha, bracket['master_sha256'])
    run = Path(row['run_dir'])
    receipt = json.loads(_snapshot(run/'run.json', evidence))
    if not isinstance(receipt.get('warnings'), list) or not all(isinstance(w, str) for w in receipt['warnings']):
        raise ValueError('Explicit solver warning list required')
    if receipt.get('status') != 'completed' or receipt.get('extraction', {}).get('status') != 'complete':
        raise ValueError('Completed native run and extraction required')
    for key in ('model_sha256', 'request_sha256'):
        if generation.get(key) != row[key] or receipt.get(key) != row[key]:
            raise ValueError('Generation/campaign/run identity mismatch')
    if receipt.get('solver_version') != generation['solver']['resolved_version']:
        raise ValueError('Native solver version mismatch')
    _file(row['request'], row['request_sha256'], evidence)
    request = yaml.safe_load(_file(run/'request.yml', row['request_sha256'], evidence))
    _file(row['model'], row['model_sha256'], evidence)
    model = yaml.load(_file(run/'source/model.yml', row['model_sha256'], evidence), Loader=OrcaFlexLoader)
    _file(run/'batch_runs/sims/model.sim', receipt['simulation_sha256'], evidence, large=True)
    return run, receipt, request, model, generation


def _trace(row, run, receipt, evidence):
    directory = run/'installation_traces'
    raw = _file(directory/'metadata.json', row['trace_metadata_sha256'], evidence)
    metadata = json.loads(raw)
    if metadata['simulation_sha256'] != receipt['simulation_sha256'] or metadata['trace_sha256'] != row['trace_sha256']:
        raise ValueError('Trace/run identity mismatch')
    profile = metadata.get('supplemental_profile', {})
    if not profile.get('geometry_lines'):
        raise ValueError('Explicit retained geometry profile required')
    audit = _audit_profile(audit_row(row['id'], run, receipt, metadata, sha256(raw).hexdigest()))
    if audit['status'] != 'VERIFIED':
        raise ValueError(f'Profile/event audit failed: {audit}')
    trace_raw = _file(directory/'traces.npz', metadata['trace_sha256'], evidence)
    with np.load(io.BytesIO(trace_raw), allow_pickle=False) as arrays:
        saved = {key: arrays[key].copy() for key in arrays.files}
    for key, channel in metadata['channels'].items():
        if float(saved[key].min()) != channel['minimum'] or float(saved[key].max()) != channel['maximum']:
            raise ValueError('Channel extrema disagree with retained arrays')
    return metadata, saved, audit


def read_cases(sequence, bracket, bracket_sha, evidence):
    cases = {row['id']: row for row in bracket['cases']}
    verified = []
    for row, pins in _batch_rows(sequence, evidence):
        if row['id'] not in cases:
            raise ValueError('Campaign case missing from bracket')
        case = cases[row['id']]
        run, receipt, request, model, generation = _run_inputs(row, pins, case, bracket, bracket_sha, evidence)
        metadata, arrays, audit = _trace(row, run, receipt, evidence)
        if not np.isclose(receipt['actual_logging_interval'], .1, rtol=0, atol=1e-10):
            raise ValueError('Actual logging interval must match the declared 0.1 s filter')
        normalized = copy.deepcopy(model)
        body = [b for b in normalized['6DBuoys'] if b['Name'] == bracket['body_name']]
        if len(body) != 1 or not math.isclose(body[0]['AddedMassCoefficient'][2],case['coefficient_z'],rel_tol=1e-12,abs_tol=1e-12):
            raise ValueError('Saved model CaZ differs from bracket')
        body[0]['AddedMassCoefficient'][2] = None
        context = dict(logging_interval_s=receipt['actual_logging_interval'], extraction=request['extraction'],
            solver=generation['solver'], extractor_sha256=metadata['extractor_sha256'],
            metrics_sha256=metadata['metrics_sha256'], profile=metadata['supplemental_profile'], model=normalized)
        verified.append(dict(case=case, metadata=metadata, arrays=arrays, context=context, audit=audit,
                             warnings=receipt['warnings']))
    return verified


def reverify(evidence):
    for entry in evidence:
        path = Path(entry['path']); stat = path.stat()
        if (stat.st_size, stat.st_mtime_ns) != (entry['bytes'], entry['mtime_ns']):
            raise ValueError('Source evidence changed during comparison')
        if compute_hash(path) != entry['sha256']:
            raise ValueError('Source evidence digest changed during comparison')
