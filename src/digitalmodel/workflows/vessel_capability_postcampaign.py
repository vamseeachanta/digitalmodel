"""Wait for a pinned coordinator and audit a frozen campaign without solves."""
import argparse
from datetime import datetime, timezone
from hashlib import sha256
import json
import math
from pathlib import Path
import time

import psutil

from digitalmodel.workflows.vessel_capability_report import generate_report


def _utc():
    return datetime.now(timezone.utc).isoformat()


def _code_paths():
    names = ('vessel_capability_postcampaign', 'vessel_capability_report',
             'vessel_capability_layout', 'installation_report_layout',
             'installation_partial_report', 'installation_seastate_report',
             'installation_report_sections', 'installation_event_audit',
             'installation_trace_extract', 'installation_response_metrics',
             'orcaflex_reproduce')
    root = Path(__file__).resolve().parent
    return [root / (name + '.py') for name in names] + [
        root.parent / 'infrastructure/persistence/provenance.py']


def _process_identity(pid):
    try:
        return psutil.Process(pid).create_time()
    except psutil.NoSuchProcess:
        return None


def _wait(config):
    started = time.monotonic()
    while True:
        identity = _process_identity(config['pid'])
        if identity is None:
            return
        if identity != config['create_time']:
            raise ValueError('Living PID identity differs from pinned coordinator')
        remaining = config['wait_timeout'] - (time.monotonic() - started)
        if remaining <= 0:
            raise TimeoutError('Coordinator wait timeout')
        time.sleep(min(config['poll_seconds'], remaining))


def _validate(config):
    for key in ('pid', 'create_time', 'poll_seconds', 'wait_timeout'):
        value = config[key]
        if isinstance(value, bool) or not isinstance(value, (int, float)):
            raise ValueError(f'{key} must be numeric')
        if not math.isfinite(value) or value <= 0:
            raise ValueError(f'{key} must be finite and positive')
    if not isinstance(config['pid'], int) or config['poll_seconds'] > 30:
        raise ValueError('Integer PID and poll_seconds <= 30 required')
    output = Path(config['output']).resolve()
    if output.suffix != '.html':
        raise ValueError('HTML output required')
    paths = dict(html=output, summary=output.with_suffix('.json'),
                 receipt=output.with_suffix('.completion.json'),
                 snapshot=output.with_suffix('.campaign-snapshot.json'),
                 staging=output.with_suffix('.partial.html'))
    paths['staging_summary'] = paths['staging'].with_suffix('.json')
    if any(path.exists() for path in paths.values()):
        raise FileExistsError('All output, snapshot and partial paths must be new')
    return paths


def _save(path, value):
    raw = json.dumps(value, indent=2, allow_nan=False).encode('utf-8')
    with path.open('xb') as stream:
        stream.write(raw)
    if path.read_bytes() != raw:
        raise ValueError('Artifact readback failed')


def _inputs(config):
    source = Path(config['source_report']).resolve()
    matrix = Path(config['matrix']).resolve()
    raw, matrix_raw = source.read_bytes(), matrix.read_bytes()
    if sha256(raw).hexdigest() != config['source_report_sha256']:
        raise ValueError('Prior report digest mismatch')
    prior = json.loads(raw)
    if prior['matrix_sha256'] != sha256(matrix_raw).hexdigest():
        raise ValueError('Prior report matrix digest mismatch')
    if not isinstance(prior['design_basis'], dict) or not prior['design_basis']:
        raise ValueError('Frozen design basis required')
    return prior, {source: raw, matrix: matrix_raw}


def _capture(config):
    campaign = Path(config['campaign']).resolve()
    if (campaign.parent / 'campaign.lock').exists():
        raise ValueError('Campaign lock remains present')
    raw = campaign.read_bytes()
    snapshot = json.loads(raw)
    terminal = {'completed', 'selected_cases_complete', 'stopped', 'paused'}
    if snapshot.get('status') not in terminal:
        raise ValueError('Campaign status is not terminal')
    if any(row.get('status', '').upper() == 'RUNNING' for row in snapshot['cases']):
        raise ValueError('Campaign still contains RUNNING cases')
    return campaign, raw, snapshot


def _verify_unchanged(config, pinned):
    for path, raw in pinned.items():
        if path.read_bytes() != raw:
            raise ValueError(f'Input changed during report generation: {path.name}')
    _capture(config)
    identity = _process_identity(config['pid'])
    if identity is not None:
        raise ValueError('Coordinator PID identity present after wait')


def _execution_complete(counts, planned):
    return (planned > 0 and counts.get('VERIFIED', 0) == planned
            and sum(counts.values()) == planned)


def _evidence_hashes(snapshot):
    from digitalmodel.infrastructure.persistence.provenance import compute_hash
    hashes = {}
    for row in snapshot['cases']:
        if row['status'] != 'COMPLETED':
            continue
        run, generation = Path(row['run_dir']), Path(row['generation_file'])
        paths = [generation, generation.parent / 'model.yml', run / 'run.json',
                 run / 'request.yml', run / 'source/model.yml',
                 run / 'installation_traces/metadata.json',
                 run / 'installation_traces/traces.npz']
        paths.extend((run / 'batch_runs/sims').glob('*.sim'))
        hashes.update({str(path): compute_hash(path) if path.is_file() else None
                       for path in paths})
    return hashes


def _generate(config, paths, record):
    prior, pinned = _inputs(config)
    code = {path: path.read_bytes() for path in _code_paths()}
    record['dependency_sha256'] = {str(path): sha256(raw).hexdigest() for path, raw in code.items()}
    pinned.update(code)
    _wait(config)
    campaign, raw, snapshot = _capture(config)
    pinned[campaign] = raw
    record.update(source_snapshot_utc=prior['created_utc'], campaign_captured_utc=_utc(),
                  campaign_status=snapshot['status'],
                  input_sha256={str(path): sha256(data).hexdigest() for path, data in pinned.items()})
    if snapshot['matrix_sha256'] != prior['matrix_sha256']:
        raise ValueError('Campaign matrix differs from frozen design basis report')
    with paths['snapshot'].open('xb') as stream:
        stream.write(raw)
    if paths['snapshot'].read_bytes() != raw:
        raise ValueError('Frozen campaign readback failed')
    pinned[paths['snapshot']] = raw
    evidence = _evidence_hashes(snapshot)
    record['case_evidence_sha256'] = evidence
    summary = generate_report(paths['snapshot'], config['matrix'], paths['staging'], prior['design_basis'])
    if summary.get('campaign_sha256') != sha256(raw).hexdigest():
        raise ValueError('Generated report campaign digest differs from frozen snapshot')
    _verify_unchanged(config, pinned)
    if _evidence_hashes(snapshot) != evidence:
        raise ValueError('Per-case evidence changed during report generation')
    counts = summary['counts']
    planned = len(json.loads(pinned[Path(config['matrix']).resolve()])['cases'])
    complete = _execution_complete(counts, planned)
    record.update(status='COMPLETE' if complete else 'PARTIAL', counts=counts,
                  engineering_acceptance='NOT EVALUATED', completed_utc=_utc(),
                  output_sha256={paths[key].name: sha256(paths[stage].read_bytes()).hexdigest()
                                 for key, stage in [('html', 'staging'), ('summary', 'staging_summary')]})
    if paths['html'].exists() or paths['summary'].exists():
        raise FileExistsError('Final report destination appeared during audit')
    paths['staging'].rename(paths['html'])
    paths['staging_summary'].rename(paths['summary'])


def run(config):
    """Return an audit receipt; partial coverage is never reported as complete."""
    paths = _validate(config)
    paths['receipt'].parent.mkdir(parents=True, exist_ok=True)
    record = dict(schema_version=1, status='FAILED', started_utc=_utc(), config=config,
                  engineering_acceptance='NOT EVALUATED',
                  module_sha256=sha256(Path(__file__).read_bytes()).hexdigest())
    try:
        _generate(config, paths, record)
    except Exception as error:
        record.update(status='FAILED', completed_utc=_utc(), error=f'{type(error).__name__}: {error}')
        _save(paths['receipt'], record)
        raise
    _save(paths['receipt'], record)
    return record


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--config', type=Path, required=True)
    args = parser.parse_args()
    config = json.loads(args.config.read_text(encoding='utf-8'))
    print(json.dumps(run(config), indent=2))


if __name__ == '__main__':
    main()
