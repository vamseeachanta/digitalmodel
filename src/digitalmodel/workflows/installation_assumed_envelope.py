"""Conditional sampled envelopes under explicit, externally supplied assumptions.

No engineering standard or capacity is embedded here. Passing a configured
screen never establishes operational acceptance or an unsampled boundary.
"""
from __future__ import annotations

import math


def _number(value):
    return type(value) in (int, float) and math.isfinite(value)


def _positive(value):
    return _number(value) and value > 0


def _safe(value):
    if isinstance(value, dict):
        return {key: _safe(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_safe(item) for item in value]
    if isinstance(value, float) and not math.isfinite(value):
        return None
    return value


def _unknown(identifier, reason):
    return dict(id=identifier, status='NOT_EVALUATED', utilization=None,
                governing_channel=None, reason=reason)


def _combine(results):
    if any(row['status'] == 'FAIL' for row in results):
        return 'FAIL'
    if not results or any(row['status'] != 'PASS' for row in results):
        return 'NOT_EVALUATED'
    return 'PASS'


def _governing(results):
    failures = [row for row in results if row['status'] == 'FAIL']
    unbounded = [row for row in failures if row.get('utilization') is None]
    measured = [row for row in (failures or results) if _number(row.get('utilization'))]
    if unbounded:
        return unbounded[0]
    return max(measured, key=lambda row: row['utilization']) if measured else None


def _limit(check, criteria):
    if (criteria.get('status') != 'project_assumption' or not criteria.get('provenance')
            or not check.get('provenance') or not _positive(check.get('limit'))):
        return None
    kind, unit = check.get('kind'), check.get('value_units')
    if kind == 'minimum_static_ratio' and unit == '1':
        return float(check['limit'])
    if kind != 'maximum_tension' or unit not in ('Te', 'kN'):
        return None
    if unit == 'kN':
        return float(check['limit'])
    conversion = criteria.get('force_conversion', {})
    if (not isinstance(conversion, dict) or not conversion.get('provenance')
            or not _positive(conversion.get('kN_per_Te'))):
        return None
    limit = check['limit'] * conversion['kN_per_Te']
    return float(limit) if _positive(limit) else None


def _channel_check(channel, values, check, limit):
    unknown = _unknown(check['id'], f'Missing or invalid channel: {channel}')
    if (not isinstance(values, dict) or values.get('units') != 'kN'
            or values.get('variable') != 'Effective tension'):
        return unknown
    kind = check['kind']
    field = 'maximum' if kind == 'maximum_tension' else 'minimum'
    demand = values.get(field)
    if not _number(demand):
        return unknown
    result = dict(id=check['id'], governing_channel=channel, demand_kN=demand)
    if kind == 'maximum_tension':
        utilization = max(0., demand) / limit
        result.update(limit_kN=limit, demand_definition='maximum recorded axial tension')
    else:
        static = values.get('static_tension_kN')
        if not _positive(static):
            return _unknown(check['id'], f'Missing or nonpositive static tension: {channel}')
        ratio = demand / static
        utilization = limit / ratio if ratio > 0 else None
        result.update(static_tension_kN=static, minimum_static_ratio=ratio, required_ratio=limit)
    if utilization is not None and not math.isfinite(utilization):
        return _unknown(check['id'], f'Nonfinite calculated utilization: {channel}')
    result.update(status='PASS' if utilization is not None and utilization <= 1 else 'FAIL',
                  utilization=utilization, reason='Conditional project-assumption screen')
    return result


def _check(case, check, criteria):
    identifier = check.get('id')
    limit = _limit(check, criteria)
    names = check.get('channels')
    if (limit is None or not isinstance(names, list) or not names
            or any(not isinstance(name, str) or not name for name in names)
            or len(set(names)) != len(names)):
        return _unknown(identifier, 'Unknown criterion, missing provenance or invalid limit/channels')
    channels = case.get('channels', {})
    if not isinstance(channels, dict):
        return _unknown(identifier, 'Missing channel mapping')
    results = [_channel_check(name, channels.get(name), check, limit) for name in names]
    chosen = _governing(results)
    result = dict(chosen) if chosen else _unknown(identifier, 'No channel can be evaluated')
    result.update(status=_combine(results), channel_checks=results)
    return result


def _validate_case_identity(planned, evidence):
    expected, coordinates = {}, set()
    for row in planned:
        index = row.get('index')
        hs, tp = row.get('hs_m'), row.get('tp_s')
        if (type(index) is not int or index < 0 or index in expected
                or not _positive(hs) or not _positive(tp) or (hs, tp) in coordinates):
            raise ValueError('Invalid or duplicate planned case identity')
        expected[index] = row
        coordinates.add((hs, tp))
    if not expected:
        raise ValueError('Explicit planned grid is required')
    hs_values = {row['hs_m'] for row in planned}
    tp_values = {row['tp_s'] for row in planned}
    if len(coordinates) != len(hs_values) * len(tp_values):
        raise ValueError('Planned Hs-Tp grid must include every cell explicitly')
    found = {}
    for case in evidence:
        index = case.get('index')
        if type(index) is not int or index not in expected or index in found:
            raise ValueError('Unexpected or duplicate evidence case identity')
        if any(case.get(key) != expected[index].get(key) for key in ('hs_m', 'tp_s', 'seed')):
            raise ValueError('Evidence case coordinates or seed differ from planned case')
        found[index] = case
    return found


def _cell(row, evidence, criteria):
    case = evidence.get(row['index'])
    result = dict(index=row['index'], hs_m=row['hs_m'], tp_s=row['tp_s'])
    if case is None or case.get('status') != 'VERIFIED' or row.get('status') != 'COMPLETED':
        checks = []
        reason = 'Completed verified case evidence unavailable'
    else:
        checks = [_check(case, check, criteria) for check in criteria.get('checks', [])]
        reason = 'Conditional criteria only; unresolved operational checks remain separate'
    chosen = _governing(checks)
    evaluated = [check['utilization'] for check in checks if _number(check.get('utilization'))]
    result.update(status=_combine(checks), checks=checks, reason=reason,
                  governing_check=chosen['id'] if chosen else None,
                  max_utilization=max(evaluated) if evaluated else None)
    return result


def _boundary(tp, cells):
    row = sorted((cell for cell in cells if cell['tp_s'] == tp), key=lambda cell: cell['hs_m'])
    islands, current = [], []
    highest, first_nonpass, seen_nonpass, nonmonotonic = None, None, False, False
    seen_failure = False
    for cell in row:
        if cell['status'] == 'PASS':
            current.append(cell['hs_m'])
            if not seen_nonpass:
                highest = cell['hs_m']
            nonmonotonic = nonmonotonic or seen_failure
        else:
            if current:
                islands.append(current)
                current = []
            if first_nonpass is None:
                first_nonpass = cell
            seen_nonpass = True
            seen_failure = seen_failure or cell['status'] == 'FAIL'
    if current:
        islands.append(current)
    return dict(tp_s=tp, lowest_planned_hs_m=row[0]['hs_m'],
                highest_contiguous_pass_hs_m=highest, pass_islands=islands,
                first_nonpass_hs_m=first_nonpass['hs_m'] if first_nonpass else None,
                first_nonpass_status=first_nonpass['status'] if first_nonpass else None,
                upper_edge_censored=row[-1]['status'] == 'PASS',
                contiguous_upper_edge_censored=first_nonpass is None,
                nonmonotonic_observed=nonmonotonic,
                interpretation='Sampled conditional passes only; no interpolation or extrapolation')


def build_envelope(summary, criteria):
    """Evaluate endpoint assumptions while preserving a distinct unresolved release."""
    if not isinstance(criteria, dict) or not isinstance(criteria.get('checks', []), list):
        raise ValueError('Criteria must contain a list of explicitly identified checks')
    checks = criteria.get('checks', [])
    ids = [check.get('id') for check in checks if isinstance(check, dict)]
    if (len(ids) != len(checks) or any(not isinstance(key, str) or not key for key in ids)
            or len(set(ids)) != len(ids)):
        raise ValueError('Criteria check identifiers must be unique nonempty strings')
    planned = summary.get('campaign_snapshot', {}).get('cases', [])
    evidence = _validate_case_identity(planned, summary.get('cases', []))
    cells = [_cell(row, evidence, criteria) for row in sorted(planned, key=lambda r: (r['hs_m'], r['tp_s']))]
    return dict(schema_version=1, assessment_basis='project_assumption', criteria=_safe(criteria),
                operational_acceptance='NOT_EVALUATED', cells=cells,
                boundaries=[_boundary(tp, cells) for tp in sorted({row['tp_s'] for row in planned})],
                counts={status: sum(cell['status'] == status for cell in cells)
                        for status in ('PASS', 'FAIL', 'NOT_EVALUATED')},
                unresolved_operational_checks=_safe(criteria.get('unresolved_operational_checks', [])),
                limitations=['Endpoint screens do not establish whole-line or component-interior demand.',
                             'Crane axial proxies do not establish hoist-rope strength or crane certification.',
                             'Passing sampled assumptions does not qualify intentional slack or operating release.',
                             'No interpolation, extrapolation or monotonic-response assumption is applied.'])
