# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Resumable connected-riser execution; native entry point is run_case only.

Extraction contract: main stage, absolute connection rotations at the compliant
ends, effective top tension, ring vertical excursion relative to initial
position, and maximum main-riser von Mises stress. Native channel qualification
remains necessary before engineering acceptance.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import time
from typing import Any, Callable

import yaml

METRICS = ('lower_mean_deg', 'lower_max_deg', 'upper_mean_deg', 'upper_max_deg',
           'top_min_kn', 'top_max_kn', 'stroke_min_m', 'stroke_max_m', 'stress_max_mpa')
EXTRACTION_VERSION = 'connection-angles-vertical-ring-1'
LICENCE_EXIT = 12


class LicenceUnavailable(RuntimeError):
    """No licensed execution can begin; retry is controlled externally."""


def write_json(path: Path, data: dict[str, Any]) -> None:
    """Publish one complete JSON record by replacing a temporary file."""
    temporary = path.with_suffix(path.suffix + '.tmp')
    temporary.write_text(json.dumps(data, indent=2, sort_keys=True, allow_nan=False) + '\n',
                         encoding='utf-8', newline='\n')
    temporary.replace(path)


def input_digest(campaign: Path, case_id: str) -> str:
    """Bind model components, criteria and extraction implementation."""
    files = [campaign / 'case-table.yml', Path(__file__)]
    files.extend(sorted((campaign / 'models' / case_id).rglob('*.yml')))
    if len(files) < 5:
        raise ValueError('Incomplete generated model')
    identity = hashlib.sha256(EXTRACTION_VERSION.encode('ascii'))
    for path in files:
        label = path.name if path == Path(__file__) else path.relative_to(campaign).as_posix()
        identity.update(label.encode('utf-8') + b'\0' + path.read_bytes() + b'\0')
    return identity.hexdigest()


def validate_result(result: dict[str, Any]) -> None:
    """Reject missing, nonfinite or inconsistent native summary values."""
    if result.get('native_state') != 'SimulationStopped' or not result.get('solver_version'):
        raise ValueError('Completed native state and solver version are required')
    for name in (*METRICS, 'wall_seconds'):
        value = result.get(name)
        if isinstance(value, bool) or not isinstance(value, (int, float)) or not math.isfinite(value):
            raise ValueError(f'Missing or nonfinite result: {name}')
    for low, high in [('lower_mean_deg', 'lower_max_deg'), ('upper_mean_deg', 'upper_max_deg'),
                      ('top_min_kn', 'top_max_kn'), ('stroke_min_m', 'stroke_max_m')]:
        if result[low] > result[high]:
            raise ValueError('Reversed result bounds')
    if min(result['lower_mean_deg'], result['upper_mean_deg'], result['stress_max_mpa'], result['wall_seconds']) < 0:
        raise ValueError('Negative magnitude or elapsed time')


def criterion_rows(result: dict[str, Any], criteria: dict[str, Any]) -> list[tuple]:
    """Criterion, component, demand, limit, unit, utilization; all checks active."""
    limit = {key: entry['value'] for key, entry in criteria.items()}
    for key in ('mean_angle_deg', 'max_angle_deg', 'min_top_tension_kn', 'stress_limit_mpa'):
        if isinstance(limit[key], bool) or not math.isfinite(limit[key]) or limit[key] <= 0:
            raise ValueError('Criteria must be finite and positive')
    if not (not isinstance(limit['stroke_min_m'], bool) and not isinstance(limit['stroke_max_m'], bool)
            and math.isfinite(limit['stroke_min_m']) and math.isfinite(limit['stroke_max_m'])
            and limit['stroke_min_m'] < 0 < limit['stroke_max_m']):
        raise ValueError('Stroke bounds must be finite and span zero')
    rows = []
    for joint in ('lower', 'upper'):
        for stat in ('mean', 'max'):
            demand = result[f'{joint}_{stat}_deg']
            bound = limit[f'{stat}_angle_deg']
            rows.append((f'{stat} angle', joint + ' flex joint', demand, bound, 'degrees', demand / bound))
    tension = result['top_min_kn']
    rows.append(('minimum tension', 'riser top', tension, limit['min_top_tension_kn'], 'kN',
                 limit['min_top_tension_kn'] / tension if tension > 0 else math.inf))
    for stat in ('min', 'max'):
        demand, bound = result[f'stroke_{stat}_m'], limit[f'stroke_{stat}_m']
        ratio = demand / bound
        rows.append((f'{stat} vertical stroke', 'tensioner ring', demand, bound, 'm', max(0, ratio)))
    rows.append(('maximum stress', 'main riser', result['stress_max_mpa'], limit['stress_limit_mpa'],
                 'MPa', result['stress_max_mpa'] / limit['stress_limit_mpa']))
    return rows


def run_case(model_path: Path, simulation_path: Path) -> dict[str, Any]:
    """Native execution only. Never invoked by preparation or fake tests.

    Channel/API precedent: drilling_riser/global_model/orcaflex_run.py.
    Connection topology: generic specification, lines 314-350 and 517-550.
    SI model stress is kN/m²; output is MPa. Vertical stroke is an explicitly
    assumed demand definition, not individual spring-link extension.
    """
    started = time.perf_counter()
    try:
        import OrcFxAPI
    except ImportError as exc:
        raise LicenceUnavailable('Solver interface unavailable') from exc
    try:
        model = OrcFxAPI.Model(str(model_path))
        model.CalculateStatics()
        if getattr(model.state, 'name', '') != 'InStaticState':
            raise ValueError('Statics did not complete')
        model.RunSimulation()
        if getattr(model.state, 'name', '') != 'SimulationStopped':
            raise ValueError('Dynamics did not complete normally')
        if float(model.simulationTimeStatus.CurrentTime) < float(model.simulationStopTime):
            raise ValueError('Dynamics ended before the requested stop time')
        period = OrcFxAPI.Period(1)

        def series(obj: str, variable: str, end: Any = None) -> list[float]:
            arguments = (variable, period) if end is None else (variable, period, end)
            values = [float(v) for v in model[obj].TimeHistory(*arguments)]
            if not values or not all(math.isfinite(v) for v in values):
                raise ValueError('Empty or nonfinite native channel')
            return values

        lower = [abs(v) for v in series('Flex Joint Rigid Piece', 'Ez-Angle', OrcFxAPI.oeEndB)]
        upper = [abs(v) for v in series('Riser Slip Joint', 'Ez-Angle', OrcFxAPI.oeEndA)]
        tension = series('Riser', 'Effective tension', OrcFxAPI.oeEndA)
        # The vessel is fixed in this sweep, so its vertical displacement is zero.
        stroke = [v - 20.0 for v in series('Tensioner Ring', 'Z')]
        stress = [float(v) / 1000 for v in model['Riser'].RangeGraph('Max von Mises stress', period).Max]
        if not stress or not all(math.isfinite(v) for v in stress):
            raise ValueError('Empty or nonfinite stress channel')
        result = {'lower_mean_deg': sum(lower) / len(lower), 'lower_max_deg': max(lower),
            'upper_mean_deg': sum(upper) / len(upper), 'upper_max_deg': max(upper),
            'top_min_kn': min(tension), 'top_max_kn': max(tension),
            'stroke_min_m': min(stroke), 'stroke_max_m': max(stroke), 'stress_max_mpa': max(stress),
            'solver_version': str(OrcFxAPI.DLLVersion()), 'native_state': 'SimulationStopped',
            'wall_seconds': time.perf_counter() - started}
        validate_result(result)
        model.SaveSimulation(str(simulation_path))
        return result
    except Exception as exc:
        if 'licence' in str(exc).lower() or 'license' in str(exc).lower():
            raise LicenceUnavailable('Solver licence unavailable') from exc
        raise


def retain_governing(output: Path, summaries: list[dict[str, Any]], criteria: dict[str, Any]) -> None:
    """Remove only this driver's non-governing native files after all complete."""
    governing: dict[float, tuple[float, str]] = {}
    for summary in summaries:
        current = summary['current_mps']
        score = max(row[-1] for row in criterion_rows(summary, criteria))
        old = governing.get(current)
        if old is None or score > old[0]:
            governing[current] = (score, summary['case_id'])
    retained = {value[1] for value in governing.values()}
    for case_id in retained:
        if not (output / f'{case_id}.sim').is_file():
            raise ValueError('Governing native artifact is missing')
    for summary in summaries:
        simulation = output / f"{summary['case_id']}.sim"
        if summary['case_id'] not in retained and simulation.is_file():
            simulation.unlink()
        summary['simulation_retained'] = summary['case_id'] in retained
        write_json(output / f"{summary['case_id']}.summary.json", summary)
    write_json(output / 'retention.json', {'governing_cases': sorted(retained),
        'basis': 'maximum normalized demand over every criterion; first case resolves ties'})


def run_campaign(campaign: Path, output: Path, *,
                 criteria_file: Path | None = None,
                 case_runner: Callable[[Path, Path], dict[str, Any]] | None = None) -> int:
    """Resume matching cases and retain native evidence for governing cases."""
    runner = run_case if case_runner is None else case_runner
    if output.resolve().is_relative_to(campaign.resolve()):
        raise ValueError('Results must be outside the prepared input directory')
    table = yaml.safe_load((campaign / 'case-table.yml').read_text(encoding='utf-8'))
    from digitalmodel.drilling_riser.generic_campaign import SPEC, load_criteria, source_digest
    source = Path(__file__).resolve().parents[2] / SPEC
    manifest = json.loads((campaign / 'manifest.campaign.json').read_text(encoding='utf-8'))
    expected = manifest['env']['SOURCE_SHA256']
    if (expected != source_digest(source) or table['template_sha256'] != expected
            or os.environ.get('SOURCE_SHA256', expected) != expected):
        raise ValueError('Source specification digest mismatch')
    criteria, criteria_identity = load_criteria(criteria_file, table)
    criterion_rows({name: 1.0 for name in METRICS}, criteria)
    # Retention depends on criteria; a new set requires a complete new output set.
    for summary in output.glob('*.summary.json'):
        if json.loads(summary.read_text(encoding='utf-8')).get('criteria_sha256') != criteria_identity:
            raise ValueError('Criteria changed; use a new output directory for a complete rerun')
    cases = table['cases']
    if len(cases) != 24 or len({c['id'] for c in cases}) != 24:
        raise ValueError('Exactly 24 unique cases are required')
    output.mkdir(parents=True, exist_ok=True)
    completed: list[dict[str, Any]] = []
    failures = 0
    version: str | None = None
    for case in cases:
        case_id = case['id']
        if not case_id.replace('-', '').isalnum():
            raise ValueError('Invalid case identifier')
        identity = input_digest(campaign, case_id)
        summary_path = output / f'{case_id}.summary.json'
        result = None
        if summary_path.is_file():
            saved = json.loads(summary_path.read_text(encoding='utf-8'))
            if saved.get('input_sha256') == identity and saved.get('case_id') == case_id and saved.get('status') == 'complete':
                validate_result(saved)
                simulation = output / f'{case_id}.sim'
                if saved.get('simulation_retained') is False or (simulation.is_file() and
                        hashlib.sha256(simulation.read_bytes()).hexdigest() == saved.get('simulation_sha256')):
                    result = saved
        if result is None:
            # Preserve stale evidence outside the progress glob before retrying.
            if summary_path.is_file():
                archive = output / 'superseded'
                archive.mkdir(exist_ok=True)
                old_digest = hashlib.sha256(summary_path.read_bytes()).hexdigest()
                summary_path.replace(archive / f'{case_id}-{old_digest}.json')
            try:
                result = runner(campaign / 'models' / case_id / 'master.yml', output / f'{case_id}.sim')
                validate_result(result)
                if not (output / f'{case_id}.sim').is_file():
                    raise ValueError('Native artifact was not written')
                if version is not None and version != result['solver_version']:
                    raise ValueError('Solver version changed; use a new output directory for a complete rerun')
                result.update(case_id=case_id, input_sha256=identity, status='complete',
                              current_mps=case['current_mps']['value'], extraction_version=EXTRACTION_VERSION,
                              criteria_sha256=criteria_identity,
                              simulation_retained=True, simulation_sha256=hashlib.sha256(
                                  (output / f'{case_id}.sim').read_bytes()).hexdigest())
                write_json(summary_path, result)
            except LicenceUnavailable:
                write_json(output / f'{case_id}.failure.json', {'case_id': case_id, 'status': 'licence-unavailable'})
                return LICENCE_EXIT
            except Exception as exc:
                write_json(output / f'{case_id}.failure.json', {'case_id': case_id,
                           'status': 'incomplete', 'error_type': type(exc).__name__})
                failures += 1
                continue
        if version is not None and version != result['solver_version']:
            raise ValueError('Mixed solver versions; use a new output directory for a complete rerun')
        if not completed:
            write_json(output / 'timing.json', {'case_id': case_id, 'input_sha256': identity,
                'wall_seconds': result['wall_seconds'], 'projected_seconds': result['wall_seconds'] * len(cases),
                'basis': 'first completed case times case count; estimate'})
        version = result['solver_version']
        completed.append(result)
    if failures:
        return 1
    retain_governing(output, completed, criteria)
    return 0


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--campaign', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--criteria', type=Path, required=True)
    args = parser.parse_args()
    raise SystemExit(run_campaign(args.campaign, args.output, criteria_file=args.criteria))
