# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Export assumed preload comparisons as CSV and offline Plotly JSON.

Usage: PYTHONPATH=src python examples/domains/geotechnical/jackup_preload_methods.py OUTPUT
Optional --tide-range LOW HIGH changes water levels relative to the fixed hull.
No Plotly dependency, network access, browser or licensed solver is required.
"""

from __future__ import annotations

import argparse
import csv
from collections.abc import Sequence
from dataclasses import asdict
import json
from pathlib import Path
import runpy
from typing import Any, TypedDict

from digitalmodel.geotechnical.spudcan import finite, penetration_curve
from digitalmodel.marine_ops.preload_sequence import (
    HullGeometry, PreloadMethodCheck, compare_preload_methods, preload_method_stages,
)

BASIS = 'assumed, based on previous experience'
STANDARDS = [
    {'document': 'SNAME T&RB 5-5A', 'edition': 'Rev 3 (2008)', 'clause': 'TO CONFIRM'},
    {'document': 'ISO 19905-1', 'edition': '2012', 'clause': 'TO CONFIRM'},
]
# Every numerical scenario input carries an independent sensitivity range.
# They are illustrative bounds, not a measured regional or equipment envelope.
class AssumedInput(TypedDict):
    """One illustrative value with independent sensitivity bounds."""

    value: float
    range: list[float]


ASSUMPTIONS: dict[str, AssumedInput] = {
    'hull_length_m': {'value': 40.0, 'range': [30.0, 70.0]},
    'hull_breadth_m': {'value': 36.0, 'range': [30.0, 66.0]},
    'hull_depth_m': {'value': 6.0, 'range': [5.0, 10.0]},
    'leg_longitudinal_spacing_m': {'value': 40.0, 'range': [30.0, 50.0]},
    'leg_transverse_spacing_m': {'value': 44.0, 'range': [36.0, 54.0]},
    'water_weight_kn_m3': {'value': 10.0, 'range': [9.8, 10.2]},
    'reserve_per_leg_kn': {'value': 12000.0, 'range': [6000.0, 18000.0]},
    'maximum_total_weight_kn': {'value': 72000.0, 'range': [54000.0, 108000.0]},
    'minimum_reference_draft_m': {'value': 1.0, 'range': [0.5, 1.5]},
    'elevated_reference_air_gap_m': {'value': 1.0, 'range': [0.3, 1.5]},
    'early_target_fraction': {'value': 0.6, 'range': [0.4, 0.8]},
    'prescribed_settlement_m': {'value': 1.0, 'range': [0.25, 2.0]},
    'tide_offset_m': {'value': 0.0, 'range': [-1.0, 1.0]},
    'sea_state_significant_height_m': {'value': 0.0, 'range': [0.0, 2.0]},
    'ballast_weight_kn': {'value': 36000.0, 'range': [18000.0, 72000.0]},
}

PUBLIC_SOURCES = [
    {'title': 'Looking back at successful jack-up preloading operations: Managing and applying key data',
     'year': 2021, 'locator': 'Example Rig Move 1, page 5',
     'url': 'https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2021-13.pdf',
     'use': 'A 2006 operation used six leg-by-leg stages, the first four with hull in water.'},
    {'title': 'Regional coastal wetland Ramsar information sheet', 'year': 2024,
     'locator': 'Section 4.4.4, page 12',
     'url': 'https://rsis.ramsar.org/RISapp/files/RISrep/GA1652RIS_2412_fr.pdf',
     'use': 'Reported tidal excursion about 1.0 m neaps and 2.0 m springs; not a site tide prediction.'},
]

LIMITS = [
    'Static required reactions are not proof that the soil can achieve or hold them.',
    'Settlement is prescribed; no dynamic plunge or buoyancy arrest is predicted.',
    'Negative reactions invalidate the assumed three-support contact state.',
    'No penetration memory, remoulding, rebound, lateral loading or structural stiffness.',
    'Sea state is an unresolved input range; wave-induced forces are excluded.',
    'A 2026 public industry article on preload draft is context only; no event input is known.',
    'Six stages and first four in water follow the public case; loads and within-stage sweeps are assumed.',
    'Regional spring excursion is interpreted as a 2.0 m low-to-high range; symmetric offsets are assumed.',
    'Method clauses and required standards edition remain to be confirmed.',
]


def _write_json(path: Path, value: dict) -> None:
    """Write finite values with stable formatting and no external links to load."""
    path.write_text(json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + '\n', encoding='utf-8')


def _summary(rows: Sequence[PreloadMethodCheck], method: str, tide: float) -> dict[str, Any]:
    selected = [r for r in rows if r.method == method and r.tide_m == tide]
    nominal = [r for r in selected if r.condition == 'nominal']
    faults = [r for r in selected if r.condition != 'nominal']
    wet = [r for r in nominal if r.reference_draft_m > 0]
    wet_max = max((r.reaction_kn for r in wet), default=None)
    wet_peak = min((r.peak_headroom_kn for r in wet), default=None)
    troughs = [r.post_peak_reserve_kn for r in selected if r.post_peak_reserve_kn is not None]
    return {
        'method': method,
        'tide_m': tide,
        'in_water_maximum_nominal_reaction_kn': wet_max,
        'in_water_minimum_nominal_peak_headroom_kn': wet_peak,
        'maximum_nominal_reaction_per_leg_kn': [max(r.reaction_kn for r in nominal if r.leg == i)
                                                for i in range(3)],
        'minimum_peak_headroom_kn': min(r.peak_headroom_kn for r in selected),
        'minimum_post_peak_reserve_kn': min(troughs) if troughs else None,
        'maximum_absolute_trim_deg': max(abs(r.trim_deg) for r in faults),
        'maximum_absolute_heel_deg': max(abs(r.heel_deg) for r in faults),
        'maximum_absolute_redistribution_kn': max(abs(r.reaction_change_kn) for r in faults),
        'all_contact_states_valid': all(r.contact_valid for r in selected),
        'all_demands_within_evaluated_capacity': all(r.within_evaluated_capacity for r in selected),
    }


def figure_spec(rows: Sequence[PreloadMethodCheck], profile: str,
                tides: tuple[float, ...]) -> dict:
    """Compare reaction, reserve, rotation and redistribution without a renderer."""
    traces = []
    methods = tuple(dict.fromkeys(r.method for r in rows))
    for method in methods:
        summaries = [_summary(rows, method, tide) for tide in tides]
        metrics = [
            ('maximum_nominal_reaction_per_leg_kn', 'Best achieved nominal reaction', 1000),
            ('minimum_peak_headroom_kn', 'Least peak headroom including trial loss', 1000),
            ('maximum_absolute_trim_deg', 'Trial-loss trim', 1),
            ('maximum_absolute_redistribution_kn', 'Trial-loss redistribution magnitude', 1000),
        ]
        for index, (key, label, scale) in enumerate(metrics, 1):
            values = [min(s[key]) if isinstance(s[key], list) else s[key] for s in summaries]
            axis = '' if index == 1 else str(index)
            traces.append({'type': 'scatter', 'mode': 'lines+markers',
                           'name': method + ': ' + label,
                           'x': list(tides), 'y': [value / scale for value in values],
                           'xaxis': 'x' + axis, 'yaxis': 'y' + axis,
                           'hovertemplate': '%{x:.1f} m<br>%{y:.1f}<extra>%{fullData.name}</extra>'})
        if any(s['in_water_maximum_nominal_reaction_kn'] is not None for s in summaries):
            for key, label, axis in (
                ('in_water_maximum_nominal_reaction_kn', 'In-water best nominal reaction', ''),
                ('in_water_minimum_nominal_peak_headroom_kn', 'In-water least nominal peak headroom', '2'),
            ):
                traces.append({'type': 'scatter', 'mode': 'lines+markers',
                               'name': method + ': ' + label,
                               'x': list(tides), 'y': [s[key] / 1000 for s in summaries],
                               'xaxis': 'x' + axis, 'yaxis': 'y' + axis,
                               'line': {'dash': 'dash'},
                               'hovertemplate': '%{x:.1f} m<br>%{y:.1f} MN<extra>%{fullData.name}</extra>'})
        traces.append({'type': 'scatter', 'mode': 'lines+markers',
                       'name': method + ': Trial-loss heel', 'x': list(tides),
                       'y': [s['maximum_absolute_heel_deg'] for s in summaries],
                       'xaxis': 'x3', 'yaxis': 'y3', 'line': {'dash': 'dot'},
                       'hovertemplate': '%{x:.1f} m<br>%{y:.1f} degrees<extra>%{fullData.name}</extra>'})
        troughs = [s['minimum_post_peak_reserve_kn'] for s in summaries]
        if any(value is not None for value in troughs):
            traces.append({'type': 'scatter', 'mode': 'lines+markers',
                           'name': method + ': Least post-peak reserve including trial loss',
                           'x': list(tides), 'y': [None if v is None else v / 1000 for v in troughs],
                           'xaxis': 'x2', 'yaxis': 'y2', 'line': {'dash': 'dot'},
                           'hovertemplate': '%{x:.1f} m<br>%{y:.1f} MN<extra>%{fullData.name}</extra>'})
    layout: dict[str, Any] = {
        'title': {'text': profile.replace('_', ' ').capitalize() + ' — assumed preload comparison'},
        'grid': {'rows': 2, 'columns': 2, 'pattern': 'independent'},
        'meta': {'basis': BASIS, 'standards_by_method': {m: STANDARDS for m in methods},
                 'limits': LIMITS, 'assumptions': ASSUMPTIONS, 'public_sources': PUBLIC_SOURCES},
    }
    for index, title in enumerate(('Per-leg nominal reaction (MN)', 'Signed capacity margin (MN)',
                                   'Absolute trim or heel (degrees)', 'Absolute reaction change (MN)'), 1):
        axis = '' if index == 1 else str(index)
        layout['xaxis' + axis] = {'title': {'text': 'Tide offset at fixed hull position (m)'}}
        layout['yaxis' + axis] = {'title': {'text': title}}
    return {'data': traces, 'layout': layout}


def write_examples(destination: Path, *, tide_levels_m: tuple[float, ...] = (-1.0, 0.0, 1.0)) -> None:
    """Write three profile comparisons and adjacent offline figure specifications."""
    if not tide_levels_m:
        raise ValueError('at least one tide level is required')
    for tide in tide_levels_m:
        finite('tide level', tide, minimum=-float('inf'))
    tide_levels_m = tuple(sorted(set(tide_levels_m) | {0.0}))
    # Reuse the preceding example's range-validated profile loader without
    # requiring the examples directory to be installed as a Python package.
    soil_example = runpy.run_path(str(Path(__file__).with_name('jackup_layered_penetration.py')))
    values = {name: item['value'] for name, item in ASSUMPTIONS.items()}
    longitudinal = values['leg_longitudinal_spacing_m']
    transverse = values['leg_transverse_spacing_m']
    hull = HullGeometry(values['hull_length_m'], values['hull_breadth_m'], values['hull_depth_m'],
                        ((0, 2 * longitudinal / 3), (-transverse / 2, -longitudinal / 3),
                         (transverse / 2, -longitudinal / 3)), values['water_weight_kn_m3'])
    destination.mkdir(parents=True, exist_ok=True)
    assumptions = {'basis': BASIS, 'inputs': ASSUMPTIONS, 'tide_levels_m': tide_levels_m,
                   'tide_basis': 'Regional Ramsar record, section 4.4.4: about 1.0 m neaps and 2.0 m springs; symmetric offsets assumed',
                   'public_sources': PUBLIC_SOURCES,
                   'standards': STANDARDS, 'limits': LIMITS,
                   'ballast_convention': 'maximum total weight equals three reserves plus available ballast'}
    _write_json(destination / 'assumptions.json', assumptions)
    output_rows = []
    summary: dict[str, Any] = {'basis': BASIS, 'profiles': {}, 'tide_sensitivity': {}, 'limits': LIMITS}
    for source in sorted(soil_example['INPUTS'].glob('*.json')):
        data = json.loads(source.read_text(encoding='utf-8'))
        profile, spudcan, depths, target = soil_example['from_input'](data)
        curve = penetration_curve(profile, spudcan, depths, preload_kn=target)
        stages = preload_method_stages(
            target_kn=target, reserve_kn=values['reserve_per_leg_kn'],
            maximum_weight_kn=values['maximum_total_weight_kn'],
            wet_draft_m=values['minimum_reference_draft_m'],
            elevated_air_gap_m=values['elevated_reference_air_gap_m'],
            early_fraction=values['early_target_fraction'])
        rows = compare_preload_methods((curve,) * 3, hull, stages, tide_levels_m=tide_levels_m,
                                       target_kn=target, settlement_m=values['prescribed_settlement_m'])
        methods = tuple(dict.fromkeys(r.method for r in rows))
        name = data['name']
        for row in rows:
            output_rows.append({'profile': name, **asdict(row)})
        summary['profiles'][name] = {method: _summary(rows, method, 0.0) for method in methods}
        summary['tide_sensitivity'][name] = {method: [_summary(rows, method, t) for t in tide_levels_m]
                                             for method in methods}
        _write_json(destination / f'{name}.input.json', {'soil': data, 'hull': asdict(hull),
                                                       'stages': [asdict(s) for s in stages],
                                                       'assumptions': assumptions})
        _write_json(destination / f'{name}.figure.json', figure_spec(rows, name, tide_levels_m))
    with (destination / 'comparison.csv').open('w', newline='', encoding='utf-8') as stream:
        writer = csv.DictWriter(stream, fieldnames=list(output_rows[0]))
        writer.writeheader()
        writer.writerows(output_rows)
    _write_json(destination / 'summary.json', summary)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output', type=Path)
    parser.add_argument('--tide-range', nargs=2, type=float, metavar=('LOW', 'HIGH'), default=(-1.0, 1.0))
    args = parser.parse_args()
    low, high = args.tide_range
    if low > high:
        parser.error('tide range must be increasing')
    # Zero is always included so reference-state comparisons remain explicit.
    write_examples(args.output, tide_levels_m=tuple(sorted({low, 0.0, high})))
