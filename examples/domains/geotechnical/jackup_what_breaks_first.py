# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Export assumed failure rankings and one offline interactive Plotly figure.

Usage: PYTHONPATH=src python examples/domains/geotechnical/jackup_what_breaks_first.py OUTPUT
OUTPUT receives inputs, numerical results and the figure specification together.
Generation uses the standard library, without a browser, network or Plotly.
A report embeds the JSON using its own locally bundled Plotly renderer.
"""

from __future__ import annotations

import json
import math
import sys
from dataclasses import asdict
from pathlib import Path

from digitalmodel.geotechnical.spudcan import LayeredProfile, SoilLayer, Spudcan
from digitalmodel.marine_ops.jackup_failure_sequence import (
    ASSUMED, JackupFailureScreen, JackupInputs, screen_jackup,
)

SOIL_INPUTS = Path(__file__).resolve().parents[2] / 'data' / 'jackup_layered_penetration'
PATHS = (('preload', 'all_legs'), ('preload', 'single_leg'), ('storm', 'all_legs'))
CONDITIONAL = ('post_punch_', 'buoyancy_reserve')


def soil_cases() -> list[dict]:
    """Use all three step-1 topologies, each with low/base/high soil strengths.

    A separate named sensitivity set is not present in this branch. Strength
    endpoints come from the step-1 declared assumption bounds, with unchanged
    layer geometry and unit weights. Sand friction angle varies with strength.
    These are controlled sensitivities, not quantified regional probabilities.
    """
    result = []
    for source in sorted(SOIL_INPUTS.glob('*.json')):
        data = json.loads(source.read_text(encoding='utf-8'))
        for level in ('low', 'base', 'high'):
            layers = [dict(layer) for layer in data['profile']['layers']]
            for index, layer in enumerate(layers):
                key = 'su_kpa' if layer['kind'] == 'clay' else 'friction_deg'
                bounds = data['parameter_ranges'][f'profile.layers.{index}.{key}']
                if level != 'base':
                    layer[key] = bounds[0 if level == 'low' else 1]
            result.append({'name': data['name']+'/'+level, 'layers': layers,
                           'spudcan': data['spudcan'], 'basis': ASSUMED,
                           'parameter_ranges': data['parameter_ranges'],
                           'sand_method': data['profile']['sand_method'],
                           'spread_ratio': data['profile']['spread_ratio'],
                           'tip_interval_m': [data['spudcan']['tip_to_bearing_m'],
                                              data['bearing_depth_grid_m']['stop']+data['spudcan']['tip_to_bearing_m']],
                           'standards': data['standards'], 'sources': data['sources']})
    return result


def evaluate(case: dict, p: JackupInputs, phase: str, pattern: str, state: str, gap: float):
    """Run the same pure screen for a selected soil, load path and hull elevation."""
    return screen_jackup(
        profile=LayeredProfile(tuple(SoilLayer(**layer) for layer in case['layers']),
                               case['sand_method'], case['spread_ratio']),
        spudcan=Spudcan(**case['spudcan']), tip_depths_m=case['tip_interval_m'],
        inputs=p, phase=phase, hull_state=state, air_gap_m=gap, preload_pattern=pattern)


def intact_governing(screen) -> tuple[str, ...]:
    """Exclude conditional consequences when naming the intact governing mode."""
    rows = [r for r in screen.sequence.ordered if not r.name.startswith(CONDITIONAL)]
    if not rows:
        return ()
    first = rows[0].first_failure_factor
    return tuple(row.name for row in rows if math.isclose(row.first_failure_factor, first,
                                                         rel_tol=1e-7, abs_tol=1e-7))


def gap_changes(case: dict, p: JackupInputs, phase: str, pattern: str) -> tuple[list[dict], list[dict]]:
    """Bracket observed governing changes on a declared 1 m grid, then refine.

    The sweep is 1 to 20 m of elevated underside height. It cannot exclude
    additional changes between sampled gaps. Brackets are refined to 0.1 m
    and printed outward, retaining their lower and upper governing sets.
    The separate in-water state is not interpolated into this elevated sweep.
    """
    result, samples = [], []

    def sample(gap):
        screen = evaluate(case, p, phase, pattern, 'elevated', gap)
        modes = intact_governing(screen)
        row = next((r for r in screen.sequence.ordered if r.name in modes), None)
        samples.append({'gap_m': gap, 'governing_modes': modes,
                        'first_failure_factor': row.first_failure_factor if row else None})
        return modes

    lower, lower_modes = 1.0, sample(1.0)
    for gap in range(2, 21):
        upper_modes = sample(float(gap))
        if lower_modes != upper_modes:
            lo, hi = lower, float(gap)
            while hi-lo > 0.05:
                middle = (lo+hi)/2
                if intact_governing(evaluate(case, p, phase, pattern, 'elevated', middle)) == lower_modes:
                    lo = middle
                else:
                    hi = middle
            result.append({'soil': case['name'], 'phase': phase, 'preload_pattern': pattern,
                           'lower_gap_m': math.floor(lo*10)/10,
                           'upper_gap_m': math.ceil(hi*10)/10,
                           'lower_modes': lower_modes, 'upper_modes': upper_modes})
        lower, lower_modes = float(gap), upper_modes
    return result, samples


def figure_spec(results: list[tuple[str, JackupFailureScreen]], gap_sweeps: dict) -> dict:
    """Build one utilization chart with a selector for every soil/path/state."""
    traces, ranges = [], []
    for index, (name, screen) in enumerate(results):
        start = len(traces)
        for row in screen.sequence.modes:
            label = row.name.replace('_', ' ')
            traces.append({'type': 'scatter', 'mode': 'lines', 'name': label,
                           'x': [round(f, 6) for f in screen.sequence.load_factors],
                           'y': [round(v, 6) for v in row.utilizations],
                           'visible': index == 0,
                           'line': {'dash': 'dot' if row.name.startswith(CONDITIONAL) else 'solid'},
                           'hovertemplate': 'Load factor %{x:.1f}<br>Utilization %{y:.1f}<extra>%{fullData.name}</extra>'})
            if row.first_failure_factor is not None:
                traces.append({'type': 'scatter', 'mode': 'markers', 'name': label+' threshold',
                               'x': [round(row.first_failure_factor, 6)],
                               'y': [round(row.failure_utilization, 6)],
                               'showlegend': False, 'visible': index == 0,
                               'marker': {'symbol': 'x', 'size': 7},
                               'hovertemplate': 'Threshold %{x:.1f}<br>Utilization %{y:.1f}<extra>%{fullData.name}</extra>'})
        samples = gap_sweeps.get(name)
        if samples is not None:
            xs = [s['gap_m'] for s in samples]
            ys = [round(s['first_failure_factor'], 6) if s['first_failure_factor'] is not None else None
                  for s in samples]
            labels = [', '.join(s['governing_modes']).replace('_', ' ') for s in samples]
        else:
            rows = [r for r in screen.sequence.ordered if r.name in intact_governing(screen)]
            xs, ys = [0], [round(rows[0].first_failure_factor, 6) if rows else None]
            labels = [', '.join(intact_governing(screen)).replace('_', ' ')]
        traces.append({'type': 'scatter', 'mode': 'lines+markers' if samples else 'markers',
                       'name': 'First intact threshold across air gap', 'x': xs, 'y': ys,
                       'xaxis': 'x2', 'yaxis': 'y2', 'customdata': labels,
                       'visible': index == 0, 'showlegend': False,
                       'hovertemplate': 'Gap %{x:.1f} m<br>First factor %{y:.1f}<br>%{customdata}<extra></extra>'})
        ranges.append((start, len(traces)))
    buttons = []
    for (name, screen), (start, end) in zip(results, ranges):
        buttons.append({'label': name, 'method': 'update',
                        'args': [{'visible': [start <= i < end for i in range(len(traces))]},
                                 {'title.text': name+' — assumed inputs',
                                  'xaxis.range': [0, screen.sequence.load_factors[-1]]}]})
    return {'data': traces, 'layout': {
        'title': {'text': results[0][0]+' — assumed inputs'},
        'height': 850,
        'xaxis': {'title': {'text': 'Synthetic action load factor'}, 'anchor': 'y'},
        'yaxis': {'title': {'text': 'Demand / capacity'}, 'range': [0, 2], 'domain': [0.45, 1]},
        'xaxis2': {'title': {'text': 'Elevated air gap (m); zero is the separate in-water state'},
                   'range': [0, 20], 'anchor': 'y2'},
        'yaxis2': {'title': {'text': 'First intact load factor'}, 'domain': [0, 0.25], 'anchor': 'x2'},
        'shapes': [{'type': 'line', 'xref': 'paper', 'x0': 0, 'x1': 1, 'y0': 1, 'y1': 1,
                    'line': {'dash': 'dash', 'color': 'black'}}],
        'updatemenus': [{'buttons': buttons, 'direction': 'down', 'x': 0, 'y': 1.2}],
        'meta': {'basis': ASSUMED, 'dotted_lines': 'Conditional consequences after the first soil peak',
                 'limits': 'Independent thresholds beyond first failure; not a collapse simulation. '
                           'Censored modes remain explicit in summary.json. Initial y range clips large '
                           'post-punch ratios; use Plotly autoscale to inspect them.'}}}


def _write_json(path: Path, value: dict, *, compact: bool = False) -> None:
    """Write portable finite JSON; compact figure specs avoid redundant whitespace."""
    path.write_text(json.dumps(value, indent=None if compact else 2, sort_keys=True,
                               separators=(',', ':') if compact else None,
                               allow_nan=False)+'\n', encoding='utf-8', newline='\n')


def write_examples(destination: Path) -> None:
    """Write assumptions, all thresholds, gap changes and one interactive chart."""
    destination.mkdir(parents=True, exist_ok=True)
    p, soils = JackupInputs(), soil_cases()
    outputs, cases, changes, sweeps, chart_sweeps = [], [], [], [], {}
    for soil in soils:
        for phase, pattern in PATHS:
            for state, gap in (('in_water', 0.0), ('elevated', 10.0)):
                screen = evaluate(soil, p, phase, pattern, state, gap)
                name = f'{soil["name"]} | {phase} | {pattern} | {state}'
                outputs.append((name, screen))
                cases.append({'name': name, 'air_gap_m': gap,
                              'governing_intact_modes': intact_governing(screen),
                              'governing_including_conditional': screen.sequence.governing,
                              'ordered_thresholds': [
                                  {'mode': row.name, 'load_factor': row.first_failure_factor,
                                   'status': row.status, 'utilization_at_threshold': row.failure_utilization,
                                   'conditional': row.name.startswith(CONDITIONAL)}
                                  for row in screen.sequence.ordered],
                              'not_reached': [row.name for row in screen.sequence.modes
                                              if row.first_failure_factor is None],
                              'not_applicable': screen.not_applicable,
                              'load_factor_limit': screen.sequence.load_factors[-1],
                              'consequence_at_first_soil_peak': asdict(screen.consequence),
                              'capacity_basis': screen.capacities, 'nominal_actions': screen.actions,
                              'reactions_at_one_kn': screen.reactions_at_one_kn,
                              'ballast_at_one_kn': screen.ballast_at_one_kn,
                              'ballast_to_match_dry_preload_at_one_kn': screen.ballast_to_match_dry_preload_at_one_kn,
                              'soil_first_peak_kn': screen.curve.peak.load_kn,
                              'soil_maximum_kn': screen.curve.maximum.load_kn,
                              'soil_capacity_loss_kn': screen.curve.post_peak_drop_kn})
            brackets, samples = gap_changes(soil, p, phase, pattern)
            changes.extend(brackets)
            sweeps.append({'soil': soil['name'], 'phase': phase, 'preload_pattern': pattern, 'samples': samples})
            chart_sweeps[name] = samples
    _write_json(destination/'inputs.json', {
        'basis': ASSUMED, 'unit_type': 'generic three-leg independent-leg jack-up',
        'inputs': p.assumptions(), 'soil_cases': soils,
        'air_gap_assumption': {'elevated_base_m': 10, 'range_m': [1, 20],
                               'sweep_step_m': 1, 'basis': ASSUMED},
        'preload_patterns': ['all_legs', 'single_leg'],
        'unknown_inputs': ['air gap', 'draft', 'sea state', 'tide', 'ballast', 'preload pattern']})
    _write_json(destination/'summary.json', {
        'basis': ASSUMED, 'cases': cases, 'air_gap_changes': changes, 'air_gap_sweeps': sweeps,
        'limits': ['Thresholds after first failure are independent extrapolations, not a collapse order.',
                   'Buoyancy balance is quasi-static; a dynamic plunge or arrest is not predicted.',
                   'Air-gap changes are observed brackets on the stated grid, not an exhaustive search.',
                   'Member and holding capacities are unqualified assumptions; standards clauses remain to confirm.',
                   'No measured regional soil profile or event-specific input is represented.']})
    _write_json(destination/'utilization.figure.json', figure_spec(outputs, chart_sweeps), compact=True)


if __name__ == '__main__':
    write_examples(Path(sys.argv[1]))
    print('Assumed inputs, failure rankings, air-gap brackets and offline figure specification written.')
