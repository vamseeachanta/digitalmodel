# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Generic three-leg independent-leg jack-up preload holding analysis.

Run with an explicit output directory outside the checkout. Each leg is an
identical independent illustrative case; multiply neither its capacity nor
load before passing it to the per-leg sequence screen. There is no network
access and no Plotly dependency: figures are plain Plotly JSON specifications.
"""

from __future__ import annotations

import argparse
import json
import math
from dataclasses import asdict, replace
from pathlib import Path

from digitalmodel.geotechnical.preload_consolidation import (
    ClayConsolidation,
    ConsolidationPreload,
    holding_response,
    recommend_minimum_hold,
)
from digitalmodel.geotechnical.spudcan import PenetrationPoint
from digitalmodel.marine_ops.preload_sequence import (
    PreloadCriteria,
    PreloadSample,
    PreloadSequenceScreen,
    PreloadStage,
    screen_preload_sequence,
)


def _screen_case(
    model: ConsolidationPreload, ramp_h: float, drainage_ramp_h: float,
    scheduled_hold_h: float, criteria: PreloadCriteria,
) -> PreloadSequenceScreen:
    # Existing sequence logic owns stage order, hold duration, absolute travel
    # in the trailing window, planned-depth comparisons and hazard flags.
    half = replace(model, target_load_kn=model.target_load_kn / 2)
    midway = holding_response(half, 0, ramp_duration_h=drainage_ramp_h / 2)
    times = sorted({0.0, scheduled_hold_h / 2,
                    scheduled_hold_h - criteria.settlement_window_h, scheduled_hold_h})
    held = tuple(PreloadSample(
        ramp_h + time, model.target_load_kn,
        model.initial_penetration_m + holding_response(
            model, time, ramp_duration_h=drainage_ramp_h).settlement_m,
    ) for time in times)
    stages = (
        PreloadStage('preparation', (PreloadSample(0, 0, model.initial_penetration_m),)),
        PreloadStage('preload', (PreloadSample(
            ramp_h / 2, model.target_load_kn / 2,
            model.initial_penetration_m + midway.settlement_m),)),
        PreloadStage('holding', held),
        PreloadStage('discharge', (PreloadSample(
            ramp_h + scheduled_hold_h + 1, 0, held[-1].penetration_m),)),
    )
    initial_capacity = holding_response(model, 0).capacity_kn
    # Illustrative linear loading reference ONLY for exercising the existing
    # depth comparison. This is not a penetration solution for the layered bed.
    curve = (PenetrationPoint(0, 0, 0, 'illustrative'), PenetrationPoint(
        6, initial_capacity / model.area_m2, initial_capacity, 'illustrative'))
    return screen_preload_sequence(
        stages, target_preload_per_leg_kn=model.target_load_kn,
        planned_curve=curve, criteria=criteria)


def run_example() -> dict:
    """Return full-precision calculation data, assumptions and sequence screens.

    The sand/soft-clay/stiffer-clay profile is a generic regional-style
    sensitivity scenario, assumed, based on previous experience. No measured
    regional distribution, site match or installation acceptance is inferred.
    Ranges in the accompanying note are assumptions, not confidence intervals.
    """
    clay = ClayConsolidation(4, 4, 0.02, 0.001, 80, 0.25)
    model = ConsolidationPreload(clay, 160, 18000, 0.25, 1)
    criteria = PreloadCriteria()
    times = (0, 0.25, 0.5, 1, 2, 4, 8, 16, 24, 48, 96, 168, 336, 672, 1344, 2400)
    cases = []
    for label, ramp_h, drainage_ramp_h in (
        ('Rapid preload (undrained ramp)', 0.1, 0.0),
        ('24 h linear preload', 24.0, 24.0),
        ('240 h linear preload', 240.0, 240.0),
    ):
        recommendation = recommend_minimum_hold(
            model, ramp_duration_h=drainage_ramp_h, criteria=criteria, max_hold_h=2400)
        if recommendation.holding_time_h is None:
            raise ValueError('example holding criterion was not reached within its horizon')
        # Round upward to the next tenth of an hour, strictly above the root,
        # so decimal sample conversion does not change a strict rate decision.
        scheduled = (math.floor(recommendation.holding_time_h * 10) + 1) / 10
        rows = [asdict(holding_response(model, time, ramp_duration_h=drainage_ramp_h))
                for time in times]
        screen = _screen_case(model, ramp_h, drainage_ramp_h, scheduled, criteria)
        cases.append({
            'name': label,
            'ramp_duration_h': ramp_h,
            'drainage_ramp_duration_h': drainage_ramp_h,
            'load_rate_kn_h': model.target_load_kn / ramp_h,
            'rapid_ramp_drainage_neglected': drainage_ramp_h == 0,
            # Initial capacity exceeds target in this example. The full ramp
            # is supported even before assigning any consolidation credit.
            'ramp_capacity_supported': holding_response(model, 0).capacity_kn >= model.target_load_kn,
            'responses': rows,
            'recommendation': asdict(recommendation),
            'scheduled_hold_h': scheduled,
            'sequence_screen': asdict(screen),
        })
    return {
        'summary': 'Generic preload holding analysis; site applicability and standard compliance unqualified.',
        'revision_history': [{'revision': 0, 'date': '2026-10-28',
                              'change': 'Add three loading rates and consolidation sensitivities.'}],
        'assumption_basis': 'assumed, based on previous experience',
        'profile': [
            {'top_m': 0, 'base_m': 1, 'material': 'permeable sand blanket',
             'model_role': 'drained upper boundary; no settlement computed'},
            {'top_m': 1, 'base_m': 5, 'material': 'normally consolidated soft clay',
             'model_role': 'single-drained equivalent compressible slab'},
            {'top_m': 5, 'base_m': None, 'material': 'stiffer low-permeability clay',
             'model_role': 'impermeable rigid base approximation; no settlement computed'},
        ],
        'model': asdict(model),
        'criteria': asdict(criteria),
        'standards': {
            'sname_edition': 'SNAME 5-5A Rev 3; publication year TO CONFIRM',
            'sname_holding_clause': 'clause: TO CONFIRM',
            'iso_edition': 'ISO 19905-1:2023',
            'iso_holding_clause': 'clause: TO CONFIRM',
            'compliance': 'NOT_ESTABLISHED',
        },
        'cases': cases,
        'sources': {
            'diffusion_and_settlement': 'MIT 1.361 (2004), Part V-2, sections 1.1-1.2, pages 2-3',
            'normalized_strength': 'FHWA NHI-06-088 (2006), section 5.5.6.1',
            'time_scaling': 'FHWA NHI-06-088 (2006), section 7.5.3, equation 7-8',
            'holding_criterion': 'marine_ops.preload_sequence.PreloadCriteria; illustrative assumption',
            'input_ranges': 'docs/domains/geotechnical/jackup-preload-holding.md',
        },
        'limits': [
            'No site investigation or verified regional parameter distribution.',
            'Only the equivalent soft-clay layer contributes settlement and gained strength.',
            'No radial drainage, creep, remoulding, unloading rebound or coupled penetration.',
            'Rapid ramp neglects drainage; finite linear ramps retain partial drainage.',
            'Low settlement rate alone establishes neither sufficient capacity nor installation acceptance.',
            'An unsupported load ramp invalidates a stable held-load prediction.',
            'Planned depth curve is an illustrative sequence-screen input, not a layered-soil solution.',
        ],
    }


def _figure(result: dict, quantity: str) -> dict:
    settlement = quantity == 'settlement'
    field, scale = ('settlement_m', 1000) if settlement else ('capacity_kn', 0.001)
    unit = 'Primary settlement since unloaded state (mm)' if settlement else 'Undrained capacity reference (MN)'
    return {
        'data': [{
            'type': 'scatter', 'mode': 'lines+markers', 'name': case['name'],
            'x': [row['holding_time_h'] for row in case['responses']],
            'y': [row[field] * scale for row in case['responses']],
            'hovertemplate': '%{x:.1f} h<br>%{y:.1f}<extra>%{fullData.name}</extra>',
        } for case in result['cases']],
        'layout': {
            'title': {'text': f'Generic jack-up preload holding: {quantity}'},
            'xaxis': {'title': {'text': 'Holding time (h)'}, 'type': 'linear', 'tickformat': '.1f'},
            'yaxis': {'title': {'text': unit}, 'tickformat': '.1f'},
            'meta': {'standard_compliance': 'NOT_ESTABLISHED',
                     'basis': result['assumption_basis'],
                     'holding_time_origin': 'first attainment of full load'},
        },
    }


def write_example(output_directory: Path) -> None:
    """Write calculation data and two adjacent offline Plotly figure specs."""
    result = run_example()
    output_directory.mkdir(parents=True, exist_ok=True)
    payloads = {'results.json': result,
                'settlement.figure.json': _figure(result, 'settlement'),
                'capacity.figure.json': _figure(result, 'capacity')}
    for filename, payload in payloads.items():
        (output_directory / filename).write_text(
            json.dumps(payload, indent=2, allow_nan=False) + '\n', encoding='utf-8')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output_directory', type=Path)
    write_example(parser.parse_args().output_directory)
