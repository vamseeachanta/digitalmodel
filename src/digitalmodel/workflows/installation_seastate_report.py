"""Evidence-checked sea-state response matrix; no implicit acceptance criteria."""
from __future__ import annotations

import argparse
from html import escape
import io
import json
from pathlib import Path

import numpy as np

from digitalmodel.infrastructure.persistence.provenance import compute_hash
from digitalmodel.workflows.installation_response_metrics import summarize_grid, tension_event_metrics


def _read(path):
    return json.loads(Path(path).read_text(encoding='utf-8'))


def _paths(index, mapping):
    if isinstance(mapping, dict):
        return Path(mapping['run_dir']), Path(mapping['generation_file'])
    run = Path(mapping)
    return run, run.parent.parent / 'prepared' / f'case_{index:03d}' / 'generation.json'


def _generation(index, mapping, case, matrix):
    run, path = _paths(index, mapping)
    generation = _read(path)
    settings = generation['settings']
    if any(settings[key] != case[key] for key in ('hs_m', 'tp_s', 'seed')):
        raise ValueError('Generation coordinate/seed mismatch')
    dependencies = generation.get('dependencies', {})
    if dependencies.get('master_sha256') != matrix['master_sha256']:
        raise ValueError('Generation master digest mismatch')
    if dependencies.get('change_sha256') != case['change_sha256']:
        raise ValueError('Generation change digest mismatch')
    if compute_hash(path.parent / 'model.yml') != generation['model_sha256']:
        raise ValueError('Generated model digest mismatch')
    return run, generation


def _verified_sim(run, generation):
    receipt = _read(run / 'run.json')
    if receipt.get('model_sha256') != generation['model_sha256']:
        raise ValueError('Run/generated model identity mismatch')
    if receipt.get('status') in ('started', 'preflight', 'solving', 'postprocessing'):
        return receipt
    if receipt.get('status') != 'completed':
        raise ValueError(f"Run status: {receipt.get('status', 'missing')}")
    sims = list((run / 'batch_runs/sims').glob('*.sim'))
    if len(sims) != 1 or compute_hash(sims[0]) != receipt.get('simulation_sha256'):
        raise ValueError('Saved simulation digest mismatch')
    if receipt.get('simulation_stop') != generation['settings']['duration_s']:
        raise ValueError('Simulation duration mismatch')
    return receipt


def _traces(run, receipt):
    directory = run / 'installation_traces'
    metadata = _read(directory / 'metadata.json')
    path = directory / 'traces.npz'
    if metadata.get('simulation_sha256') != receipt['simulation_sha256']:
        raise ValueError('Trace simulation identity mismatch')
    if compute_hash(path) != metadata.get('trace_sha256'):
        raise ValueError('Trace digest mismatch')
    with np.load(io.BytesIO(path.read_bytes()), allow_pickle=False) as data:
        arrays = {key: data[key].copy() for key in data.files}
    times = arrays['time']
    if times.ndim != 1 or len(times) < 2 or not np.isfinite(times).all():
        raise ValueError('Invalid time array')
    if not np.isclose(times[0], 0) or not np.isclose(times[-1], receipt['simulation_stop']):
        raise ValueError('Incomplete trace coverage')
    for key, channel in metadata['channels'].items():
        values = arrays[key]
        if values.shape != times.shape or not np.isfinite(values).all():
            raise ValueError(f'Invalid trace: {key}')
        if not np.isclose(channel['minimum'], values.min()) or not np.isclose(channel['maximum'], values.max()):
            raise ValueError(f'Metadata extrema mismatch: {key}')
    return arrays, metadata


def _responses(arrays, metadata):
    rows = []
    for key, channel in metadata['channels'].items():
        if channel.get('variable') != 'Effective tension':
            continue
        if channel.get('units') != 'kN':
            raise ValueError('Tension units must be kN')
        event = tension_event_metrics(arrays['time'], arrays[key], units='kN')
        duration = event['low_tension']['total_duration_s']
        stored = channel['events']['low_tension']['total_duration_s']
        if not np.isclose(duration, stored):
            raise ValueError('Low-tension duration metadata mismatch')
        rows.append({'channel': key, 'minimum_kN': float(channel['minimum']),
                     'peak_kN': float(channel['maximum']), 'low_tension_duration_s': duration})
    if not rows:
        raise ValueError('No signed-tension channels supplied')
    return {'minimum_signed_tension_kN': min(r['minimum_kN'] for r in rows),
            'peak_tension_kN': max(r['peak_kN'] for r in rows),
            'maximum_low_tension_duration_s': max(r['low_tension_duration_s'] for r in rows),
            'tension_channels': rows}


def _case(index, case, mapping, matrix):
    result = {key: case[key] for key in ('hs_m', 'tp_s', 'seed')}
    result.update(index=index, status='MISSING', reason='No run mapped')
    if mapping is None:
        return result
    try:
        run, generation = _generation(index, mapping, case, matrix)
        result.update(run_dir=str(run), settings=generation['settings'],
                      heading_degrees=generation['wave_reference']['WaveDirection'])
        receipt = _verified_sim(run, generation)
        if receipt['status'] != 'completed':
            result.update(status='RUNNING', reason=f"Recorded run state: {receipt['status']}; "
                          'completion and engineering acceptance not established')
            return result
        result.update(status='NOT EVALUATED', simulation_sha256=receipt['simulation_sha256'],
                      reason='Component capacities and slack/snap criteria unverified')
        if not (run / 'installation_traces/metadata.json').exists():
            result['reason'] = 'Solve verified; trace extraction missing; criteria unverified'
            return result
        arrays, metadata = _traces(run, receipt)
        result.update(_responses(arrays, metadata), trace_sha256=metadata['trace_sha256'])
    except (OSError, ValueError, KeyError, TypeError) as error:
        result.update(status='FAILED', reason=f'Evidence verification failed: {error}')
    return result


def _format(value):
    return 'Not evaluated' if value is None else f'{value:.4g}'


def _matrix_table(cases):
    hs = sorted({row['hs_m'] for row in cases})
    periods = sorted({row['tp_s'] for row in cases})
    lookup = {(row['hs_m'], row['tp_s']): row for row in cases}
    rows = ['<tr><th>Hs (m) / Tp (s)</th>' + ''.join(f'<th>{p:g}</th>' for p in periods) + '</tr>']
    for height in hs:
        cells = []
        for period in periods:
            row = lookup.get((height, period), {'status': 'MISSING', 'reason': 'No planned cell'})
            title = escape(row['reason'], quote=True)
            detail = ''
            if 'peak_tension_kN' in row:
                detail = (f"<br>min {_format(row['minimum_signed_tension_kN'])} kN"
                          f"<br>peak {_format(row['peak_tension_kN'])} kN"
                          f"<br>low {_format(row['maximum_low_tension_duration_s'])} s")
            cells.append(f'<td title="{title}">{row["status"]}{detail}</td>')
        rows.append(f'<tr><th>{height:g}</th>' + ''.join(cells) + '</tr>')
    return '<table>' + ''.join(rows) + '</table>'


def _details(cases):
    rows = []
    for case in cases:
        settings = case.get('settings', {})
        scope = (f"seed {case['seed']}; heading {_format(case.get('heading_degrees'))} deg; "
                 f"build-up {_format(settings.get('buildup_s'))} s; "
                 f"dynamic duration {_format(settings.get('duration_s'))} s")
        rows.append(f"<li>Case {case['index']}: Hs {case['hs_m']:g} m, Tp {case['tp_s']:g} s; "
                    f"{scope}; {escape(case['status'])}. {escape(case['reason'])}</li>")
    return '<ul>' + ''.join(rows) + '</ul>'


def _html(summary):
    return f'''<!doctype html><html lang="en"><meta charset="utf-8">
<title>Installation sea-state response readiness</title>
<style>body{{font:15px sans-serif;margin:2rem;color:#173042}}table{{border-collapse:collapse}}
td,th{{border:1px solid #abb;padding:.5rem;font-size:12px}}th{{background:#e9f0f5}}</style>
<h1>SIMULATED installation sea-state response matrix</h1>
<p>{len(summary['cases'])} planned cells. Engineering acceptance: NOT EVALUATED.
No operating window has been established. Missing and failed evidence remains explicit.</p>
{_matrix_table(summary['cases'])}
<p>Table 1. Each cell lists the minimum signed tension, peak tension and greatest total
low-tension duration across extracted crane-wire/sling end channels where verified.
The three values can govern at different channels. Low tension means tension at or below zero;
it is not geometric slack distance. Component-level metrics are retained in the JSON sidecar.</p>
<h2>Scope by case</h2>{_details(summary['cases'])}
<h2>Unresolved acceptance evidence</h2><p>Component capacities and applicable DNV
edition/clause criteria remain unverified. Geometry-based slack, re-tension/snap-load
convergence, startup sensitivity, duration, multiple seeds and heading coverage require
assessment. No historical hoist ratio or illustrative pamphlet threshold is applied here.</p>
<p>Source matrix SHA-256: {escape(summary['matrix_sha256'])}. Reusable engineering results
are owned by private digitalmodel-data. Working copies do not establish remote backup.</p></html>'''


def generate_report(matrix_path, run_map, output):
    """Write HTML plus JSON; mapping values are paths or run_dir/generation_file dicts."""
    output, matrix_path = Path(output), Path(matrix_path)
    sidecar = output.with_suffix('.json')
    if output.exists() or sidecar.exists() or output == sidecar:
        raise FileExistsError('Report/sidecar destination already exists or collides')
    matrix = _read(matrix_path)
    mapping = {int(key): value for key, value in run_map.items()}
    if any(index < 0 or index >= len(matrix['cases']) for index in mapping):
        raise ValueError('Run-map index outside matrix')
    cases = [_case(index, case, mapping.get(index), matrix) for index, case in enumerate(matrix['cases'])]
    summary = {'matrix_sha256': compute_hash(matrix_path), 'engineering_acceptance': 'NOT EVALUATED',
               'cases': cases, 'grid': summarize_grid(cases)}
    html = _html(summary)
    payload = json.dumps(summary, indent=2, allow_nan=False)
    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open('x', encoding='utf-8') as stream:
        stream.write(html)
    with sidecar.open('x', encoding='utf-8') as stream:
        stream.write(payload)
    if output.read_text(encoding='utf-8') != html or _read(sidecar) != summary:
        raise ValueError('Report readback mismatch')
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--matrix', required=True, type=Path)
    parser.add_argument('--run-map', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    generate_report(args.matrix, _read(args.run_map), args.output)
    print(args.output)


if __name__ == '__main__':
    main()
