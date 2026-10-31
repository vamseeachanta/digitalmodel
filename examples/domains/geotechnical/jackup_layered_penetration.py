# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Write portable layered-soil inputs, reactions and offline Plotly specifications.

Usage: PYTHONPATH=src python examples/domains/geotechnical/jackup_layered_penetration.py OUTPUT
The three generic scenarios use assumptions with ranges, not site measurements.
A report can embed each figure JSON without fetching data or rerunning a solver.
No Plotly installation, browser or network connection is needed by this script.
"""

from __future__ import annotations

import csv
import json
import math
import sys
from dataclasses import asdict, replace
from pathlib import Path

from digitalmodel.geotechnical.spudcan import (
    LayeredPenetrationCurve,
    LayeredProfile,
    SoilLayer,
    Spudcan,
    finite,
    penetration_curve,
)

INPUTS = Path(__file__).resolve().parents[2] / 'data' / 'jackup_layered_penetration'


def from_input(data: dict) -> tuple[LayeredProfile, Spudcan, list[float], float]:
    """Build the callable API inputs from a portable, range-labelled JSON case.

    Ranges describe independent sensitivity bounds, not valid combinations of
    layers. The model also checks contiguity and method limits for each case.
    """
    for name, bounds in data['parameter_ranges'].items():
        value = data
        for part in name.split('.'):
            value = value[int(part)] if isinstance(value, list) else value[part]
        finite(name, value)
        if len(bounds) != 2 or not bounds[0] <= value <= bounds[1]:
            raise ValueError(f'{name} is outside its declared assumption range')
    profile_data = data['profile']
    layers = tuple(SoilLayer(**layer) for layer in profile_data['layers'])
    profile = LayeredProfile(layers, profile_data['sand_method'], profile_data['spread_ratio'])
    spudcan = Spudcan(**data['spudcan'])
    grid = data['bearing_depth_grid_m']
    finite('grid start', grid['start'])
    finite('grid stop', grid['stop'], minimum=grid['start'])
    finite('grid step', grid['step'], strict=True)
    count = math.floor((grid['stop'] - grid['start']) / grid['step'])
    depths = [grid['start'] + i * grid['step'] + spudcan.tip_to_bearing_m for i in range(count + 1)]
    depths.append(grid['stop'] + spudcan.tip_to_bearing_m)
    return profile, spudcan, sorted(set(depths)), data['preload_kn']


def figure_spec(curves: dict[str, LayeredPenetrationCurve], preload: float, data: dict) -> dict:
    """Build a Plotly figure with reactions, descending peaks, troughs and preload."""
    traces = []
    for method, curve in curves.items():
        traces.append({
            'type': 'scatter', 'mode': 'lines', 'name': method,
            'x': [point.depth_m for point in curve.points],
            'y': [point.load_kn / 1000 for point in curve.points],
            'hovertemplate': 'Tip %{x:.1f} m<br>Reaction %{y:.1f} MN<extra>%{fullData.name}</extra>',
        })
        markers = [('Peak', curve.peak, 'triangle-up')]
        for index, event in enumerate(curve.events, 1):
            markers.append((f'Trough {index}', event.trough, 'triangle-down'))
            traces.append({
                'type': 'scatter', 'mode': 'lines+text',
                'name': f'{method}: capacity loss {event.drop_kn / 1000:.1f} MN',
                'x': [event.peak.depth_m, event.trough.depth_m],
                'y': [event.peak.load_kn / 1000, event.trough.load_kn / 1000],
                'line': {'dash': 'dot'}, 'text': ['', f'{event.drop_percent:.1f}% loss'],
                'textposition': 'bottom center',
            })
        for label, point, symbol in markers:
            traces.append({
                'type': 'scatter', 'mode': 'markers', 'name': f'{method}: {label}',
                'x': [point.depth_m], 'y': [point.load_kn / 1000],
                'marker': {'symbol': symbol, 'size': 10},
            })
    first = next(iter(curves.values()))
    traces.append({
        'type': 'scatter', 'mode': 'lines', 'name': 'Preload reaction',
        'x': list(first.evaluated_tip_interval_m), 'y': [preload / 1000, preload / 1000],
        'line': {'dash': 'dash', 'color': 'black'},
    })
    return {
        'data': traces,
        'layout': {
            'title': {'text': data['name'].replace('_', ' ').capitalize() + ' — assumed soil profile'},
            'xaxis': {'title': {'text': 'Tip penetration below mudline (m)'}},
            'yaxis': {'title': {'text': 'Vertical reaction per leg (MN)'}},
            'meta': {'basis': data['basis'], 'standards': data['standards'], 'limits': data['limits']},
        },
    }


def _write_json(path: Path, value: dict) -> None:
    """Serialize finite portable values with stable formatting."""
    path.write_text(json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + '\n', encoding='utf-8')


def write_examples(destination: Path) -> None:
    """Write all three JSON inputs, CSV curves, summaries and figure specs together."""
    destination.mkdir(parents=True, exist_ok=True)
    for source in sorted(INPUTS.glob('*.json')):
        data = json.loads(source.read_text(encoding='utf-8'))
        profile, spudcan, depths, preload = from_input(data)
        curve = penetration_curve(profile, spudcan, depths, preload_kn=preload)
        curves = {profile.sand_method if profile.layers[0].kind == 'sand' else 'clay_envelope': curve}
        if profile.layers[0].kind == 'sand':
            comparison = replace(profile, sand_method='punching_shear',
                                 punching_coefficient=data['punching_comparison']['coefficient'])
            curves['punching_shear'] = penetration_curve(comparison, spudcan, depths, preload_kn=preload)
        name = data['name']
        _write_json(destination / f'{name}.input.json', data)
        with (destination / f'{name}.curve.csv').open('w', newline='', encoding='utf-8') as output:
            writer = csv.writer(output)
            writer.writerow(('method', 'tip_depth_m', 'bearing_plane_depth_m', 'pressure_kpa',
                             'leg_reaction_kn', 'preload_kn', 'branch'))
            for method, result in curves.items():
                for point in result.points:
                    writer.writerow((method, point.depth_m, point.depth_m - spudcan.tip_to_bearing_m,
                                     point.pressure_kpa, point.load_kn, preload, point.branch))
        summary = asdict(curve)
        summary.update({'standards': data['standards'], 'basis': data['basis'],
                        'limits': data['limits'], 'method_comparison':
                        {method: asdict(result) for method, result in curves.items()}})
        _write_json(destination / f'{name}.summary.json', summary)
        _write_json(destination / f'{name}.figure.json', figure_spec(curves, preload, data))


if __name__ == '__main__':
    write_examples(Path(sys.argv[1]))
    print('Three assumed soil profiles, reaction curves and figure specifications written.')
