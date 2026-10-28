# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Export seeded soil probabilities, bound tornadoes and reusable named inputs.

Usage: PYTHONPATH=src python examples/domains/geotechnical/jackup_soil_sensitivity.py OUTPUT
The CLI requires at least 10000 samples per case. Completed cases are verified
and reused by digest, so a restarted study does not repeat completed work.
Plotly figure specifications require no network, browser or Plotly installation.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
from pathlib import Path

from digitalmodel.geotechnical import soil_sensitivity, spudcan
from digitalmodel.geotechnical.soil_sensitivity import monte_carlo, tornado, validate_case

INPUTS = Path(__file__).resolve().parents[2] / 'data' / 'jackup_soil_sensitivity'
DEFAULT_SEED = 20261028


def write_json(path: Path, value: dict) -> None:
    """Serialize finite values with stable keys and portable UTF-8 encoding."""
    path.write_text(json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + '\n', encoding='utf-8')


def digest(path: Path) -> str:
    """Hash committed inputs or generated sample bytes without path metadata."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def fingerprint(case: dict, n: int, seed: int) -> str:
    """Bind a completed case to its inputs, RNG settings and executing sources."""
    manifest = {'case': case, 'n': n, 'seed': seed, 'sources': {
        'soil_sensitivity.py': digest(Path(soil_sensitivity.__file__)),
        'spudcan.py': digest(Path(spudcan.__file__)),
        'example.py': digest(Path(__file__)),
    }}
    return hashlib.sha256(json.dumps(manifest, sort_keys=True, allow_nan=False).encode()).hexdigest()


def write_csv(path: Path, rows: list[dict]) -> None:
    """Write ordered dictionaries using the first row's named columns."""
    with path.open('w', newline='', encoding='utf-8') as output:
        writer = csv.DictWriter(output, fieldnames=sorted(rows[0]), lineterminator='\n')
        writer.writeheader()
        writer.writerows(rows)


def probability_figure(summaries: list[dict]) -> dict:
    """Create per-profile bars with asymmetric Wilson sampling intervals."""
    results = [summary['result'] for summary in summaries]
    return {'data': [{
        'type': 'bar', 'name': 'Descending peak reached',
        'x': [summary['name'] for summary in summaries],
        'y': [100 * result['probability'] for result in results],
        'error_y': {'type': 'data', 'symmetric': False,
                    'array': [100 * (r['interval_95'][1] - r['probability']) for r in results],
                    'arrayminus': [100 * (r['probability'] - r['interval_95'][0]) for r in results]},
        'hovertemplate': '%{x}<br>%{y:.1f}%<extra></extra>',
    }], 'layout': {
        'title': {'text': 'Conditional punch-through probability at assumed preload'},
        'yaxis': {'title': {'text': 'Probability (%)'}, 'range': [0, 100]},
        'meta': {'basis': 'assumed, based on previous experience', 'interval': '95% Wilson score',
                 'sample_count_per_case': results[0]['n'], 'seed': results[0]['seed'],
                 'interpretation': 'Per-leg model probability; excludes model and distribution uncertainty.'},
    }}


def tornado_figure(rows: list[dict]) -> dict:
    """Rank bound effects and mark undefined margins separately from zero effects."""
    def span(row):
        values = [abs(row[f'{bound}_delta_kn']) for bound in ('low', 'high')
                  if row[f'{bound}_delta_kn'] is not None]
        return max(values, default=0)
    ordered = sorted(rows, key=lambda row: (-span(row), row['profile'], row['parameter']))
    labels = [f"{r['profile']}: {r['parameter']}" for r in ordered]
    traces = []
    for bound in ('low', 'high'):
        traces.append({'type': 'bar', 'orientation': 'h', 'name': f'{bound.capitalize()} bound',
                       'y': labels, 'x': [None if r[f'{bound}_delta_kn'] is None
                                          else r[f'{bound}_delta_kn'] / 1000 for r in ordered],
                       'hovertemplate': '%{y}<br>%{x:.1f} MN<extra>%{fullData.name}</extra>'})
    undefined = [label for label, row in zip(labels, ordered)
                 if row['low_delta_kn'] is None or row['high_delta_kn'] is None]
    return {'data': traces, 'layout': {
        'title': {'text': 'One-at-a-time punch-through margin changes'}, 'barmode': 'overlay',
        'xaxis': {'title': {'text': 'Change in margin from nominal (MN)'}, 'zeroline': True},
        'yaxis': {'autorange': 'reversed', 'categoryorder': 'array', 'categoryarray': labels},
        'margin': {'l': 340},
        'annotations': [{'xref': 'paper', 'yref': 'paper', 'x': 0, 'y': -0.15,
                         'showarrow': False, 'text': 'No descending branch: ' + '; '.join(undefined)}]
                       if undefined else [],
        'meta': {'basis': 'assumed, based on previous experience',
                 'undefined_margin_inputs': undefined,
                 'positive_margin': 'Descending capacity threshold minus preload; no loss is null.'},
    }}


def write_study(destination: Path, *, n: int = 10000, seed: int = DEFAULT_SEED) -> None:
    """Write one resumable, serial study; production minimum is enforced by CLI.

    Lower counts are accepted here for inexpensive export integration tests.
    Each final summary is the completion record. A partial sample CSV without a
    summary is restarted. A completed record with a mismatched digest is refused.
    """
    destination.mkdir(parents=True, exist_ok=True)
    summaries, probability_rows, tornado_rows = [], [], []
    inputs = sorted(INPUTS.glob('*.json'))
    if not inputs:
        raise ValueError('no named soil sensitivity inputs found')
    for source in inputs:
        case = json.loads(source.read_text(encoding='utf-8'))
        validate_case(case)
        name = case['name']
        key = fingerprint(case, n, seed)
        sample_path = destination / f'{name}.samples.csv'
        summary_path = destination / f'{name}.summary.json'
        if summary_path.exists():
            summary = json.loads(summary_path.read_text(encoding='utf-8'))
            if summary['fingerprint'] != key:
                raise ValueError(f'{name}: completed input/source digest mismatch')
            for filename, expected in summary['artifacts'].items():
                path = destination / filename
                if not path.is_file() or digest(path) != expected:
                    raise ValueError(f'{name}: completed artifact digest mismatch')
        else:
            result = monte_carlo(case, n=n, seed=seed, sample_path=sample_path)
            rows, cases = tornado(case)
            case_path = destination / f'{name}.cases.json'
            tornado_path = destination / f'{name}.tornado.csv'
            write_json(case_path, {'schema_version': 1, 'name': name, 'basis': case['basis'],
                                  'parameters': case['parameters'], 'cases': cases,
                                  'sources': case['sources'], 'standards': case['standards'],
                                  'limits': case['limits']})
            write_csv(tornado_path, rows)
            summary = {'schema_version': 1, 'name': name, 'basis': case['basis'],
                       'fingerprint': key, 'result': result, 'tornado': rows,
                       'preload_kn': case['input']['preload_kn'],
                       'artifacts': {path.name: digest(path) for path in (sample_path, case_path, tornado_path)}}
            write_json(summary_path, summary)
        summaries.append(summary)
        r = summary['result']
        probability_rows.append({'profile': name, 'n': r['n'], 'seed': r['seed'],
                                 'preload_kn': summary['preload_kn'],
                                 'punch_through_count': r['punch_through_count'],
                                 'probability': r['probability'], 'ci95_low': r['interval_95'][0],
                                 'ci95_high': r['interval_95'][1],
                                 'unsupported_count': r['unsupported_count'],
                                 'no_loss_count': r['no_loss_count']})
        tornado_rows.extend(summary['tornado'])
    write_csv(destination / 'probabilities.csv', probability_rows)
    write_csv(destination / 'tornado.csv', tornado_rows)
    write_json(destination / 'probability.figure.json', probability_figure(summaries))
    write_json(destination / 'tornado.figure.json', tornado_figure(tornado_rows))


def main(argv: list[str] | None = None) -> None:
    """Parse production sample settings and export the study without networking."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output', type=Path)
    parser.add_argument('--samples', type=int, default=10000)
    parser.add_argument('--seed', type=int, default=DEFAULT_SEED)
    args = parser.parse_args(argv)
    if args.samples < 10000:
        parser.error('at least 10000 samples per case are required')
    write_study(args.output, n=args.samples, seed=args.seed)
    print('Soil sensitivity probabilities, named cases and offline figures written.')


if __name__ == '__main__':
    main()
