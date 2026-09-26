"""Recompute every jumper tension-event dictionary from verified saved traces."""
from __future__ import annotations

import argparse
from concurrent.futures import ThreadPoolExecutor
from hashlib import sha256
from html import escape
import io
import json
from pathlib import Path

import numpy as np

from digitalmodel.workflows.installation_response_metrics import tension_event_metrics

EXPECTED_CHANNELS = tuple(f'{name}_end_{end}' for name in
                          ['Cranewire#1', 'Cranewire#2'] + [f'Sling#{i}' for i in range(1, 7)]
                          for end in ('A', 'B'))


def audit_case(row, expected_channels=EXPECTED_CHANNELS):
    result = dict(index=row['index'], status='FAILED', channels_verified=0, errors=[])
    try:
        directory = Path(row['run_dir']) / 'installation_traces'
        metadata_path, trace_path = directory / 'metadata.json', directory / 'traces.npz'
        raw, trace = metadata_path.read_bytes(), trace_path.read_bytes()
        metadata = json.loads(raw)
        if sha256(raw).hexdigest() != row['metadata_sha256']:
            raise ValueError('Metadata digest differs from verified report')
        if sha256(trace).hexdigest() != metadata['trace_sha256']:
            raise ValueError('Trace digest mismatch')
        for key in ('trace_sha256', 'simulation_sha256'):
            if metadata[key] != row[key]:
                raise ValueError(f'{key} differs from verified report')
        with np.load(io.BytesIO(trace), allow_pickle=False) as arrays:
            for name in expected_channels:
                try:
                    channel = metadata['channels'][name]
                    if channel['variable'] != 'Effective tension' or channel['units'] != 'kN':
                        raise ValueError('Expected effective tension in kN')
                    actual = tension_event_metrics(arrays['time'], arrays[name], units='kN')
                    expected = channel['events']
                    if json.dumps(actual, sort_keys=True, allow_nan=False) != json.dumps(expected, sort_keys=True, allow_nan=False):
                        raise ValueError('Complete event dictionary mismatch')
                    result['channels_verified'] += 1
                except (KeyError, ValueError, TypeError) as error:
                    result['errors'].append(f'{name}: {error}')
        if sha256(metadata_path.read_bytes()).digest() != sha256(raw).digest():
            raise ValueError('Metadata changed during audit')
        if sha256(trace_path.read_bytes()).digest() != sha256(trace).digest():
            raise ValueError('Trace changed during audit')
        result.update(metadata_sha256=sha256(raw).hexdigest(), trace_sha256=sha256(trace).hexdigest())
        if not result['errors'] and result['channels_verified'] == len(expected_channels):
            result['status'] = 'VERIFIED'
    except (OSError, ValueError, KeyError, TypeError) as error:
        result['errors'].append(str(error))
    return result


def _coverage(report):
    rows = report['cases']
    planned = report['campaign_snapshot']['cases']
    expected = {row['index']: row for row in planned}
    actual = {row['index']: row for row in rows}
    if len(expected) != len(planned) or len(actual) != len(rows) or set(actual) != set(expected):
        raise ValueError('Report coverage differs from campaign snapshot')
    for index, row in actual.items():
        if any(row[key] != expected[index][key] for key in ('hs_m', 'tp_s', 'seed')):
            raise ValueError('Report coordinates differ from campaign snapshot')
    return len(planned)


def generate_audit(report_path, output, *, workers=1):
    if isinstance(workers, bool) or not isinstance(workers, int) or workers < 1:
        raise ValueError('workers must be a positive integer')
    report_path, output = Path(report_path), Path(output)
    html_path = output.with_suffix('.html')
    if output.suffix != '.json' or output.exists() or html_path.exists():
        raise ValueError('New JSON and HTML output paths required')
    raw = report_path.read_bytes()
    report = json.loads(raw)
    planned_count = _coverage(report)
    rows = report['cases']
    if not rows or any(row['status'] != 'VERIFIED' for row in rows):
        raise ValueError('Every source case must have verified execution evidence')
    if len({row['index'] for row in rows}) != len(rows):
        raise ValueError('Duplicate case indices')
    with ThreadPoolExecutor(max_workers=workers) as executor:
        cases = list(executor.map(audit_case, rows))
    if report_path.read_bytes() != raw:
        raise ValueError('Source report changed during audit')
    summary = dict(report_sha256=sha256(raw).hexdigest(), workers=workers,
                   planned_cases=planned_count, audited_cases=len(cases),
                   expected_channel_count=planned_count * len(EXPECTED_CHANNELS),
                   verified_channel_count=sum(row['channels_verified'] for row in cases),
                   expected_channels=list(EXPECTED_CHANNELS), cases=cases,
                   verified_cases=sum(row['status'] == 'VERIFIED' for row in cases),
                   failed_cases=sum(row['status'] != 'VERIFIED' for row in cases),
                   engineering_acceptance='NOT EVALUATED')
    payload = json.dumps(summary, indent=2, allow_nan=False)
    html = ('<!doctype html><html lang="en"><meta charset="utf-8"><title>Full tension-event audit</title>'
            '<h1>Full tension-event audit</h1><p>Signed tension diagnostics do not establish geometric slack '
            'or an operating window. All 16 expected tension channels are checked for each case; failures remain explicit.</p>'
            '<pre>' + escape(payload) + '</pre></html>')
    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open('x', encoding='utf-8') as stream:
        stream.write(payload)
    with html_path.open('x', encoding='utf-8') as stream:
        stream.write(html)
    if output.read_text(encoding='utf-8') != payload or html_path.read_text(encoding='utf-8') != html:
        raise ValueError('Audit readback mismatch')
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--workers', type=int, default=1)
    args = parser.parse_args()
    result = generate_audit(args.report, args.output, workers=args.workers)
    print(json.dumps({key: result[key] for key in ('verified_cases', 'failed_cases', 'workers')}))
    if result['failed_cases']:
        raise SystemExit(1)


if __name__ == '__main__':
    main()
