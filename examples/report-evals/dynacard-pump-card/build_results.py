"""Generate the report-eval contract from the public-case calculation."""
import json
from pathlib import Path

from report_contract import METRICS, TITLE, REPO, FOLDER, PULL_REQUEST

HERE = Path(__file__).resolve().parent
# Calculation metadata is reproducible to 12 decimal places across numeric
# libraries. Result values retain their separate display tolerances; strings
# (including identifiers and source digests) are never rounded or rewritten.
METADATA_DECIMAL_PLACES = 12


def canonical_metadata(value):
    """Bound insignificant floating-point variation in nested metadata."""
    if isinstance(value, float):
        return round(value, METADATA_DECIMAL_PLACES)
    if isinstance(value, dict):
        return {key: canonical_metadata(item) for key, item in value.items()}
    if isinstance(value, list):
        return [canonical_metadata(item) for item in value]
    return value


def numerical_results(data):
    """Values and display digits graded for arithmetic reproduction only."""
    rows = [(key, label, value, unit, dp) for key, (label, unit, dp) in METRICS.items()
            for value in [data['key_results'][key]]]
    rows += [(key, key.replace('_', ' '), value, '%', 3)
             for key, value in data['solver_cross_check'].items() if value is not None]
    for row in data['sensitivity']:
        for step in row['steps']:
            tag = ('minus' if step['input_change_pct'] < 0 else 'plus') + str(abs(step['input_change_pct']))
            for output, value in step['output_change_pct'].items():
                key = f"sensitivity_{row['input']}_{tag}_{output}"
                rows.append((key, key.replace('_', ' '), value, '%', 2))
        for size in (1, 5):
            for output, value in row[f'factor_{size}pct'].items():
                key = f"factor_{size}pct_{row['input']}_{output}"
                rows.append((key, key.replace('_', ' '), value, '% per %', 2))
    return rows


def manifest(data):
    pin = data['commit']
    source = 'Retained public example well; fixed repository inputs'
    results = [{'id': key, 'label': label, 'value': int(round(value)) if digits == 0 else round(value, digits), 'unit': unit,
                'period': 'Public example well, one measured stroke', 'source': source,
                'method': FOLDER + '/analysis.py run(); package calculations and default solver',
                'repo': REPO, 'commit': pin, 'tolerance': {'abs': 0.5 * 10 ** -digits},
                'basis': 'fixed', 'limits': ['Arithmetic reproduction only; derived pump quantities depend on the ASSUMED inputs recorded in the analysis and report.']}
               for key, label, value, unit, digits in numerical_results(data)]
    inputs = [{'id': f'source_{index}', 'description': row['what'], 'source': row['source'],
               'url': row['url'], 'licence': row['licence'], 'version_or_date': row['date'],
               'sha256_published': row['sha256'], 'updates': 'never',
               'supplied_by': 'repository', 'required': True}
              for index, row in enumerate(data['sources'])]
    metadata = {key: data[key] for key in ('inputs', 'assumptions', 'settings', 'shape_hypothesis',
                                          'validation', 'sensitivity_basis', 'geometry_basis')}
    metadata['numeric_metadata_decimal_places'] = METADATA_DECIMAL_PLACES
    inputs.append({'id': 'calculation_basis', 'description': json.dumps(canonical_metadata(metadata), ensure_ascii=False),
                   'source': 'Analysis output; includes every prompt number, model hypothesis and assumption; no engineering acceptance is graded.',
                   'supplied_by': 'repository', 'required': True})
    return {'$schema': 'https://www.aceengineer.com/assets/data/report-evals/results.schema.json',
            'schema_version': '1.0',
            'report': {'id': 'dynacard-pump-card', 'title': TITLE,
                       'url': 'https://www.aceengineer.com/newsletter/', 'published': '', 'family': 'newsletter'},
            'code': {'repo': REPO, 'commit': pin,
                     'paths': ['src/digitalmodel/marine_ops/artificial_lift/dynacard',
                               'tests/marine_ops/artificial_lift/test_data/cleansed_well_001.json',
                               'tests/marine_ops/artificial_lift/dynacard/testdata'],
                     'python': 'Python 3.11 or later', 'install': 'uv sync --extra test; run with PYTHONPATH=src. The article URL and publication date are set at publication. The recorded pin predates the endpoint correction; use candidate mode at refs/pull/' + str(PULL_REQUEST) + '/head until the corrected revision is pinned.'},
            'inputs': inputs, 'missing_data': data['missing_data'], 'cannot_do': data['cannot_do'],
            'eval': {'command': 'PYTHONPATH=src uv run --no-sync python ' + FOLDER + '/eval.py',
                     'prompt': FOLDER + '/PROMPT.md', 'cadence': 'on-change', 'network': False,
                     'ref': 'main'}, 'results': results}


def main():
    from analysis import run
    result = manifest(run())
    (HERE / 'results.json').write_text(json.dumps(result, indent=1, ensure_ascii=False, allow_nan=False) + '\n', encoding='utf-8')


if __name__ == '__main__':
    main()
