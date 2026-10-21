# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline Case 2.10 diagnostics; missing source data never admits a run."""
from __future__ import annotations

import csv
import hashlib
import io
import json
import math
from pathlib import Path

import numpy as np

from .kcs_step1 import digest, three_grid_gci, _diagnostics

SIGNALS = ('ct_x1000', 'heave_over_a', 'pitch_over_ka')
UNITS = {key: '1' for key in ('time_over_Te', *SIGNALS, 'wave_over_lpp')}
QUANTITIES = {f'{q}_{kind}{n}': ('rad' if kind == 'phase' else '1')
              for q in SIGNALS for kind, ns in [('a', range(5)), ('phase', range(1, 5))] for n in ns}
QUANTITIES['wave_over_lpp_a1'] = '1'
POLICY_SHA256 = 'bb9c481d16e31f529bf6a58ef2a1fcd38259eaac11dc88731e50844b831d914e'

CONDITION_SHA256 = {'C0': 'cc535717fe99826d5865adaffa2b6b8dd7735775ff0e44c194514df0a81bdd79', 'C1': 'b78de3330990056c4fc22ee44e30020f61169df15c37d2093d8f49325f8ad58d', 'C2': '225b8f6fa082a1037e681ca224afa133f65d6ab5b68251a80bef2f955aee69a6', 'C3': 'fddc7fd84a8ad860ad16af77c684bf83672a9f5507f63a7a2af405282ad8a937', 'C4': '3593a8090424128d694321c9e5640f0a388ebaff41246bd5e681687cfcdaa65c', 'C5': '170d3fd903ef6d58549b71d36be23c9c717c5d53f3a00ec5ed1ec1f5a9eb46ce'}


def _validate_condition(row: dict) -> None:
    try:
        actual = digest({k: row[k] for k in ("conditions", "conventions")})
        if row["id"] == "C0" or actual != CONDITION_SHA256.get(row["id"]):
            raise ValueError("Case 2.10 condition or convention mismatch")
    except (KeyError, TypeError) as exc:
        raise ValueError("condition basis incomplete") from exc


def validate_policy(policy: dict) -> None:
    """Pin predeclared tolerances and convergence criteria to this revision."""
    if digest(policy) != POLICY_SHA256:
        raise ValueError('criteria missing, malformed or changed; reviewed revision required')


def load_policy(path: Path) -> dict:
    try:
        policy = json.loads(Path(path).read_text(encoding='utf-8'))
        validate_policy(policy)
        return policy
    except (OSError, TypeError, json.JSONDecodeError) as exc:
        raise ValueError('criteria missing or malformed') from exc


def load_reference(path: Path) -> dict:
    try:
        reference = json.loads(Path(path).read_text(encoding='utf-8'))
        rows = reference['reference_rows']
        if reference['case'] != '2.10' or len(rows) != 6 or {r['id'] for r in rows} != {f'C{i}' for i in range(6)}:
            raise ValueError('reference requires C0 through C5 exactly once')
        for row in rows:
            expected = set(QUANTITIES) if row['id'] != 'C0' else {'ct_x1000_mean', 'sinkage_over_lpp_mean', 'trim_deg_mean'}
            if set(row['responses']) != expected:
                raise ValueError('reference response set is incomplete')
        return reference
    except (OSError, KeyError, TypeError, json.JSONDecodeError) as exc:
        raise ValueError('reference missing or malformed') from exc


def _sha(value) -> bool:
    return isinstance(value, str) and len(value) == 64 and all(c in '0123456789abcdef' for c in value)


def reference_is_qualified(row: dict) -> bool:
    """Check readback evidence, not the authenticity of external declarations."""
    try:
        _validate_condition(row)
        if row['id'] == 'C0' or set(row['responses']) != set(QUANTITIES):
            return False
        evidence = row['readback']
        if (evidence['status'] != 'verified' or not _sha(evidence['source_sha256'])
                or evidence['table_sha256'] != digest(row['responses'])):
            return False
        for key, cell in row['responses'].items():
            if (cell['status'] != 'verified' or cell['units'] != QUANTITIES[key]
                    or type(cell['value']) not in (int, float) or not math.isfinite(cell['value'])
                    or cell['froude'] != row['conditions']['froude']
                    or cell.get('condition_id') != row['id']
                    or cell.get('wave_conditions') != row['id'] + ': see conditions'):
                return False
            if not all(cell.get(k) for k in ('sheet', 'cell', 'source_document', 'source_url', 'page', 'published_data_file', 'licence_note', 'wave_conditions')):
                return False
        return True
    except (KeyError, TypeError, ValueError):
        return False


def _basis(metadata: dict, row: dict, policy: dict) -> None:
    if row.get('id') == 'C0':
        raise ValueError('C0 requires a separate calm-water reader; wave normalization is undefined')
    _validate_condition(row)
    try:
        if row['id'] not in {f'C{i}' for i in range(1, 6)} or metadata['condition_id'] != row['id']:
            raise ValueError('wrong Case 2.10 condition')
        if metadata['mesh'] not in ('coarse', 'medium', 'fine') or metadata['evidence_kind'] not in ('native', 'synthetic'):
            raise ValueError('unknown mesh or evidence kind')
        if metadata['reference_sha256'] != digest(row) or metadata['criteria_sha256'] != digest(policy):
            raise ValueError('reference or criteria digest mismatch')
        if metadata['conditions'] != row['conditions'] or metadata['units'] != UNITS or metadata['conventions'] != row['conventions']:
            raise ValueError('condition, unit or convention mismatch')
        conditions = row['conditions']
        if (conditions['froude'] != .261 or conditions['body_condition'] != 'free_to_heave_and_pitch'
                or conditions['appendages'] != 'rudder' or conditions['heading'] != 'head'
                or conditions['wave_height_m'] <= 0 or conditions['wavelength_m'] <= 0):
            raise ValueError('invalid wave condition')
        if set(row['responses']) != set(QUANTITIES):
            raise ValueError('reference table is incomplete')
        for key, cell in row['responses'].items():
            if type(cell['value']) not in (int, float) or not math.isfinite(cell['value']) or cell['units'] != QUANTITIES[key]:
                raise ValueError('reference response is NOT_EVALUATED or has invalid units')
    except (KeyError, TypeError) as exc:
        raise ValueError('reference or run basis is incomplete') from exc


def _windows(rows: list[dict], policy: dict) -> tuple[np.ndarray, np.ndarray]:
    if not rows or any(set(r) != set(UNITS) for r in rows):
        raise ValueError('history requires exactly the declared columns')
    try:
        data = np.array([[r[k] for k in UNITS] for r in rows], dtype=float)
    except (TypeError, ValueError) as exc:
        raise ValueError('history contains invalid numeric values') from exc
    if not np.isfinite(data).all() or len(data) < 2:
        raise ValueError('history is nonfinite or too short')
    dt = np.diff(data[:, 0])
    limits = policy['history']
    if dt[0] <= 0 or not np.allclose(dt, dt[0], rtol=limits['sampling_relative_tolerance'], atol=1e-12):
        raise ValueError('history needs uniform increasing encounter time')
    samples = round(1 / dt[0])
    if samples < limits['minimum_samples_per_cycle'] or not math.isclose(samples * dt[0], 1, rel_tol=1e-6):
        raise ValueError('history needs an integer sample count per encounter cycle')
    window = limits['window_cycles'] * samples
    required = (limits['discard_cycles'] + 2 * limits['window_cycles']) * samples
    if len(data) < required or len(data) % samples:
        raise ValueError('history needs discard cycles and two complete windows without a duplicate endpoint')
    return data[-2 * window:-window], data[-window:]


def _wrap(angle: float) -> float:
    return math.atan2(math.sin(angle), math.cos(angle))


def _harmonics(window: np.ndarray, floor: float) -> dict:
    t = window[:, 0]
    design = np.column_stack([np.ones(len(t)), *[f(2 * math.pi * n * t) for n in range(1, 5) for f in (np.cos, np.sin)]])
    coefficients, _, rank, _ = np.linalg.lstsq(design, window[:, 1:], rcond=None)
    if rank != 9:
        raise ValueError('harmonic fit is rank deficient')
    wave_phase = math.atan2(-coefficients[2, 3], coefficients[1, 3])
    wave_amplitude = math.hypot(coefficients[1, 3], coefficients[2, 3])
    if wave_amplitude < floor:
        raise ValueError('incident fundamental wave amplitude is below phase floor')
    residual = window[:, 1:] - design @ coefficients
    rms = np.sqrt(np.mean(residual ** 2, axis=0))
    result = {'wave_over_lpp_a1': wave_amplitude,
              '_fit_residual_rms': dict(zip((*SIGNALS, 'wave_over_lpp'), map(float, rms)))}
    for column, quantity in enumerate(SIGNALS):
        result[quantity + '_a0'] = float(2 * coefficients[0, column])
        for n in range(1, 5):
            a, b = coefficients[2*n-1:2*n+1, column]
            amplitude = math.hypot(a, b)
            result[f'{quantity}_a{n}'] = amplitude
            result[f'{quantity}_phase{n}'] = (_wrap(math.atan2(-b, a) - n * wave_phase) if amplitude >= floor else None)
    return result


def compare_rows(rows: list[dict], metadata: dict, row: dict, policy: dict) -> dict:
    """Compare two complete encounter-period windows, without rounding verdicts.

    Input signals are already normalized by the workshop definitions. The
    adapter supplying them owns conversion from native solver output; this
    function does not claim to read a solver-specific force-file dialect.
    """
    validate_policy(policy)
    _basis(metadata, row, policy)
    windows = _windows(rows, policy)
    previous, last = [_harmonics(w, policy['history']['phase_amplitude_floor']) for w in windows]
    quantities = {}
    for key, spec in policy['tolerances'].items():
        actual, before, expected = last[key], previous[key], row['responses'][key]['value']
        circular = spec['kind'] == 'absolute_circular'
        difference = lambda a, b: abs(_wrap(a-b)) if circular else abs(a-b)
        evaluated = actual is not None and before is not None
        error = difference(actual, expected) if evaluated else None
        drift = difference(actual, before) if evaluated else None
        quantities[key] = dict(computed=actual, reference=expected, units=spec['units'],
                               absolute_error=error, absolute_tolerance=spec['limit'],
                               window_drift=drift, status='EVALUATED' if evaluated else 'NOT_EVALUATED',
                               passed=bool(evaluated and error <= spec['limit'] and drift <= spec['maximum_window_drift']))
    fit = {key: max(part['_fit_residual_rms'][key] for part in (previous, last))
           for key in (*SIGNALS, 'wave_over_lpp')}
    fit_passed = all(value <= policy['history']['maximum_fit_residual_rms'][key] for key, value in fit.items())
    passed = all(q['passed'] for q in quantities.values()) and fit_passed
    # In-memory calculations are diagnostics only; file hashes and observed
    # mesh/residual diagnostics are checked by compare_run before smoke PASS.
    return dict(schema_version=1, comparison_passed=passed, coarse_smoke_passed=False,
                qualified=False, evidence_kind=metadata['evidence_kind'], mesh=metadata['mesh'],
                condition_id=row['id'], reference_sha256=digest(row), criteria_sha256=digest(policy),
                quantities=quantities, history_fit={'passed': fit_passed, 'maximum_window_rms': fit},
                grid_uncertainty='NOT_EVALUATED')


def compare_run(history: Path, metadata: dict, row: dict, policy: dict) -> dict:
    data = Path(history).read_bytes()
    sha = hashlib.sha256(data).hexdigest()
    if metadata.get('history_sha256') != sha:
        raise ValueError('history digest mismatch')
    reader = csv.DictReader(io.StringIO(data.decode('utf-8')))
    if reader.fieldnames is None or len(reader.fieldnames) != len(UNITS) or set(reader.fieldnames) != set(UNITS):
        raise ValueError('history column mismatch')
    try:
        rows = [{k: float(v) for k, v in r.items()} for r in reader]
    except (TypeError, ValueError) as exc:
        raise ValueError('history has malformed rows') from exc
    report = compare_rows(rows, metadata, row, policy)
    diagnostics = _diagnostics(metadata, policy, {'values': {'lpp': {'value': row['conditions']['lpp_m']}}})
    report.update(history_sha256=sha, diagnostics=diagnostics)
    report['comparison_passed'] = report['comparison_passed'] and diagnostics['passed']
    report['coarse_smoke_passed'] = bool(report['comparison_passed'] and reference_is_qualified(row)
                                          and metadata['evidence_kind'] == 'native' and metadata['mesh'] == 'coarse')
    return report


def require_run_admission(next_mesh: str, reference_path: Path, criteria_path: Path,
                          coarse_history: Path | None = None, coarse_metadata: Path | None = None,
                          *, condition_id: str | None = None) -> dict:
    """Admission evidence only; a future launcher must enforce this at launch."""
    if next_mesh not in ('coarse', 'medium', 'fine'):
        raise ValueError('unknown mesh')
    reference = load_reference(reference_path)
    matches = [r for r in reference['reference_rows'] if r['id'] == condition_id]
    if len(matches) == 1:
        _validate_condition(matches[0])
    if len(matches) != 1 or not reference_is_qualified(matches[0]):
        raise ValueError('reference readback is required for the requested condition')
    row = matches[0]
    policy = load_policy(criteria_path)
    result = dict(admitted=True, mesh=next_mesh, condition_id=condition_id,
                  reference_sha256=digest(row), criteria_sha256=digest(policy))
    if next_mesh == 'coarse':
        return result
    if coarse_history is None or coarse_metadata is None:
        raise ValueError('passing coarse smoke evidence is required')
    try:
        metadata = json.loads(Path(coarse_metadata).read_text(encoding='utf-8'))
        report = compare_run(coarse_history, metadata, row, policy)
    except (OSError, KeyError, TypeError, json.JSONDecodeError) as exc:
        raise ValueError('coarse smoke evidence missing or malformed') from exc
    if not report['coarse_smoke_passed']:
        raise ValueError('coarse smoke comparison failed')
    result['coarse_history_sha256'] = report['history_sha256']
    return result


def amplitude_gci(h: list[float], values: list[float]) -> dict:
    """Reuse step 1 GCI; phases and signed/zero means are outside this method."""
    return three_grid_gci(h, values)


def compare_triplet(histories: list[Path], metadata: list[dict], row: dict, policy: dict) -> dict:
    """GCI for positive harmonics only; no phase or signed-mean qualification."""
    validate_policy(policy)
    try:
        if len(histories) != 3 or len(metadata) != 3 or {m['mesh'] for m in metadata} != {'fine', 'medium', 'coarse'}:
            raise ValueError('three distinct meshes are required')
        order = {'fine': 0, 'medium': 1, 'coarse': 2}
        pairs = sorted(zip(histories, metadata), key=lambda pair: order[pair[1]['mesh']])
        metas = [m for _, m in pairs]
        for key in ('mesh_family', 'numerics'):
            if not metas[0][key] or len({m[key] for m in metas}) != 1:
                raise ValueError('mesh family and numerics must match')
        if any(m['refinement_axes'] != 'xyz' for m in metas):
            raise ValueError('systematic xyz refinement required')
        h = [m['h_m'] for m in metas]
        for a, b in zip(metas, metas[1:]):
            for key in ('dx_m', 'dz_m'):
                if a['free_surface'][key] <= 0 or b['free_surface'][key] <= 0 or not math.isclose(b['free_surface'][key] / a['free_surface'][key], b['h_m'] / a['h_m'], rel_tol=1e-6):
                    raise ValueError('surface spacing is not systematically refined')
        reports = [compare_run(path, meta, row, policy) for path, meta in pairs]
        if not all(r['comparison_passed'] for r in reports):
            raise ValueError('every grid requires passing comparison and convergence')
        keys = [k for k in QUANTITIES if '_a' in k and not k.endswith('a0')]
        gci = {k: amplitude_gci(h, [r['quantities'][k]['computed'] for r in reports]) for k in keys}
    except (KeyError, TypeError, ZeroDivisionError) as exc:
        raise ValueError('systematic mesh evidence is incomplete') from exc
    return dict(qualified=False, amplitude_gci_passed=all(g['gci_fine_pct'] <= policy['gci']['maximum_fine_percent'] for g in gci.values()),
                gci=gci, runs=reports, limitations='Phase and signed zero-harmonic grid uncertainty NOT_EVALUATED; no overall engineering qualification.')
