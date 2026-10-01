"""Compose a report summary from a base audit plus pinned single-cell substitutions.

A substitution replaces one cell with a VERIFIED row from another audited campaign, for example the
same sea state solved at a smaller time step. The substituted row records the step it was solved at,
so the report can flag it; every other cell and the base provenance are unchanged.
"""
from __future__ import annotations

import argparse
from collections import Counter
import copy
from hashlib import sha256
import json
import math
from pathlib import Path

from digitalmodel.workflows.installation_partial_report import component_envelopes
from digitalmodel.workflows.vessel_capability_report import critical_periods


def _pinned(path, digest):
    raw = Path(path).read_bytes()
    if sha256(raw).hexdigest() != digest:
        raise ValueError(f'Digest mismatch for {path}')
    return json.loads(raw)


def _verify_variant(sub, summary, other, step):
    """The model equivalence proof is the variant-study record: same source master, time step the only delta."""
    if not sub.get('variant') or not sub.get('variant_sha256'):
        raise ValueError('Substitution requires the pinned variant-study record')
    variant = _pinned(sub['variant'], sub['variant_sha256'])
    base_master = summary.get('campaign_snapshot', {}).get('master_sha256')
    other_master = other.get('campaign_snapshot', {}).get('master_sha256')
    if variant.get('source_master_sha256') != base_master or variant.get('master_sha256') != other_master:
        raise ValueError('Variant record does not link the base and substituted campaigns')
    if variant.get('seed') is not None:
        raise ValueError('Variant record changes the wave seed')
    deltas = variant.get('master_deltas') or {}
    if set(deltas) != {'General.ImplicitConstantTimeStep'} or deltas['General.ImplicitConstantTimeStep'].get('after') != float(step):
        raise ValueError('Variant record must change only the implicit time step, to the declared value')


def build_composite(base, base_sha256, substitutions):
    summary = copy.deepcopy(_pinned(base, base_sha256))
    seen, applied = set(), []
    for sub in substitutions:
        index, step = sub['index'], sub['time_step_s']
        if index in seen:
            raise ValueError(f'Cell {index} substituted twice')
        if isinstance(step, bool) or not isinstance(step, (int, float)) or not math.isfinite(step) or step <= 0:
            raise ValueError('time_step_s must be a positive finite number')
        seen.add(index)
        other = _pinned(sub['summary'], sub['sha256'])
        if other.get('design_basis') != summary.get('design_basis'):
            raise ValueError(f'Substitution for cell {index} uses a different design basis')
        rows = [r for r in other['cases'] if r['index'] == index]
        if len(rows) != 1 or rows[0]['status'] != 'VERIFIED':
            raise ValueError(f'Substitution for cell {index} must be one VERIFIED row')
        current = summary['cases'][index]
        if current['index'] != index or any(current.get(k) != rows[0].get(k) for k in ('hs_m', 'tp_s', 'seed')):
            raise ValueError(f'Substitution coordinates or seed differ for cell {index}')
        settings = dict(rows[0].get('settings') or {})
        if settings.pop('fixed_time_step_s', None) != float(step):
            raise ValueError(f'Substituted row for cell {index} was not solved at the declared time step')
        if current.get('settings'):
            reference = dict(current['settings']); reference.pop('fixed_time_step_s', None)
            if settings != reference:
                raise ValueError(f'Substituted row for cell {index} differs in analysis settings beyond the time step')
        _verify_variant(sub, summary, other, step)
        planned = [r for r in other.get('campaign_snapshot', {}).get('cases', []) if r.get('index') == index]
        if len(planned) != 1 or planned[0].get('status') != 'COMPLETED':
            raise ValueError(f'Substitution for cell {index} needs its COMPLETED campaign row')
        provenance = dict(summary=str(sub['summary']), sha256=sub['sha256'], reason=sub['reason'],
                          replaced_status=current['status'], variant=str(sub['variant']), variant_sha256=sub['variant_sha256'])
        summary['cases'][index] = dict(copy.deepcopy(rows[0]), solved_time_step_s=float(step), substituted_from=provenance)
        snapshot_rows = summary['campaign_snapshot']['cases']
        position = next(i for i, r in enumerate(snapshot_rows) if r['index'] == index)
        snapshot_rows[position] = dict(copy.deepcopy(planned[0]), solved_time_step_s=float(step), substituted_from=provenance)
        applied.append(dict(index=index, time_step_s=float(step), reason=sub['reason'],
                            summary=str(sub['summary']), sha256=sub['sha256'], replaced_status=current['status']))
    summary['counts'] = dict(Counter(row['status'] for row in summary['cases']))
    summary['envelopes'] = component_envelopes(summary['cases'])
    summary['critical_periods'] = critical_periods(summary['cases'])
    summary['composite'] = dict(base=str(base), base_sha256=base_sha256, substitutions=applied,
                                note='campaign_snapshot and cases carry substituted rows; campaign_sha256 describes the base campaign only')
    summary['engineering_acceptance'] = 'NOT EVALUATED'
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--base', required=True, type=Path)
    parser.add_argument('--base-sha256', required=True)
    parser.add_argument('--substitution', action='append', required=True,
                        help='JSON: {"summary": path, "sha256": hex, "index": n, "time_step_s": s, "reason": text}')
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    if args.output.exists():
        raise FileExistsError('Composite output must be new')
    result = build_composite(args.base, args.base_sha256, [json.loads(s) for s in args.substitution])
    raw = json.dumps(result, indent=2, allow_nan=False)
    with args.output.open('x', encoding='utf-8') as stream:
        stream.write(raw)
    print(args.output, sha256(raw.encode('utf-8')).hexdigest())


if __name__ == '__main__':
    main()
