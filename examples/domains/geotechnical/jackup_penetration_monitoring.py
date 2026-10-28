# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Export a synthetic penetration-monitoring record and offline Plotly JSON.

Usage: PYTHONPATH=src python examples/domains/geotechnical/jackup_penetration_monitoring.py OUTPUT
Only the generator is retained in the repository. OUTPUT receives the record
and a neighbouring figure specification for embedding without runtime network
access. No browser, CDN, external service or licensed solver is used.

The record represents one leg of a generic three-leg independent-leg jack-up.
The other two legs are not evaluated. Every curve value, reading, trigger limit
and context range is synthetic: assumed, based on previous experience. The
curve only illustrates a layered response; it is not a regional soil model or
an observed installation history. Its depth bounds are illustrative ranges,
not a statistically calibrated confidence interval. No incident is reconstructed.

Context from a 2026 public industry article on preload draft supplies no known
air gap, draft, sea state, tide, ballast or preload mode. The example retains
these as assumed input ranges only. They do not determine the trigger limits
or demonstrate the feasibility of returning the hull to the water.

Standard context and public operational source clauses are documented in
digitalmodel.marine_ops.penetration_monitoring. All Plan B actions require an
independently agreed procedure; the output supplies no installation acceptance.
"""

import json
import sys
from collections.abc import Sequence
from dataclasses import asdict
from pathlib import Path

import plotly.graph_objects as go

from digitalmodel.marine_ops.penetration_monitoring import (
    Measurement,
    MonitoringCriteria,
    MonitoringResult,
    PenetrationMonitor,
    PredictionPoint,
    Trigger,
)


def synthetic_inputs() -> tuple[tuple[PredictionPoint, ...], MonitoringCriteria, tuple[Measurement, ...]]:
    """Return assumed curve bounds, thresholds and a deliberately flagged stream."""
    curve = tuple(PredictionPoint(*row) for row in (
        (0, 0, 0, 0.5), (1, 100, 0.5, 1.5), (2, 160, 1.5, 2.5),
        (3, 180, 2.5, 3.5), (4, 140, 3.5, 4.5), (5, 220, 4.5, 5.5),
    ))
    criteria = MonitoringCriteria(0.5, 0.015, 1)
    record = tuple(Measurement(*row) for row in (
        (0, 0, 0), (10, 100, 1), (20, 140, 2), (30, 150, 2.1),
        (40, 150, 2.3), (50, 150, 2.6), (60, 150, 2.6),
    ))
    return curve, criteria, record


def unpublished_input_ranges() -> dict[str, dict[str, object]]:
    """Preserve unknown conditions as assumptions, including both preload modes."""
    ranges: dict[str, dict[str, object]] = {
        'air_gap_m': {'range': [0, 2], 'unit': 'm'},
        'draft_m': {'range': [0, 6], 'unit': 'm'},
        'significant_wave_height_m': {'range': [0, 2], 'unit': 'm'},
        'tide_m': {'range': [-1, 1], 'unit': 'm relative to a chosen datum'},
        'ballast_t': {'range': [0, 1000], 'unit': 't'},
        'preload_mode': {'choices': ['single-leg', 'all-leg'], 'unit': 'categorical'},
    }
    return {name: {**item, 'basis': 'assumed, based on previous experience',
                   'readiness': 'illustration_only'} for name, item in ranges.items()}


def make_figure(curve: Sequence[PredictionPoint], record: Sequence[Measurement],
                results: Sequence[MonitoringResult]) -> go.Figure:
    """Plot nominal response, uncertainty and individual trigger entry points."""
    figure = go.Figure()
    loads = [p.load_kn for p in curve]
    figure.add_trace(go.Scatter(
        x=[p.lower_penetration_m for p in curve], y=loads,
        name='Assumed lower penetration bound', mode='lines',
        line={'color': '#aabbd0', 'dash': 'dot'},
    ))
    figure.add_trace(go.Scatter(
        x=[p.upper_penetration_m for p in curve], y=loads,
        name='Assumed upper penetration bound', mode='lines', fill='tonextx',
        line={'color': '#aabbd0', 'dash': 'dot'},
        fillcolor='rgba(80, 120, 180, 0.12)',
    ))
    figure.add_trace(go.Scatter(
        x=[p.penetration_m for p in curve], y=loads,
        name='Synthetic nominal curve', mode='lines', line={'color': '#28649b'},
    ))
    figure.add_trace(go.Scatter(
        x=[p.penetration_m for p in record], y=[p.load_kn for p in record],
        customdata=[p.elapsed_s for p in record], name='Synthetic measured record',
        mode='lines+markers', line={'color': '#282828'},
        hovertemplate='Depth %{x:.1f} m; load %{y:.1f} kN; time %{customdata:.1f} s<extra></extra>',
    ))
    styles = (
        (Trigger.STIFFNESS_DROP, 'Stiffness drop entry', 'diamond', '#e59020'),
        (Trigger.RAPID_PENETRATION, 'Rapid penetration entry', 'triangle-up', '#b63740'),
        (Trigger.OUTSIDE_BAND, 'Outside band entry', 'x', '#753faa'),
    )
    for trigger, name, symbol, color in styles:
        selected = [r.sample for r in results if any(a.trigger == trigger for a in r.alerts)]
        figure.add_trace(go.Scatter(
            x=[p.penetration_m for p in selected], y=[p.load_kn for p in selected],
            name=name, mode='markers', marker={'symbol': symbol, 'size': 13, 'color': color},
        ))
    figure.update_layout(
        title='Synthetic predicted and measured penetration monitoring',
        xaxis_title='Penetration (m)', yaxis_title='Leg load (kN)',
        template='plotly_white',
        meta={'synthetic': True, 'qualification': 'illustration_only'},
    )
    return figure


def run_example(output: str | Path) -> None:
    """Write the replayable synthetic record and a neighbouring Plotly JSON spec."""
    curve, criteria, record = synthetic_inputs()
    monitor = PenetrationMonitor(curve, criteria)
    results = [monitor.update(sample) for sample in record]
    payload = {
        'schema_version': 1, 'synthetic': True, 'qualification': 'illustration_only',
        'basis': 'assumed, based on previous experience',
        'scope': 'one leg of a generic three-leg independent-leg jack-up',
        'other_legs': 'not_evaluated',
        'units': {'load': 'kN', 'penetration': 'm', 'elapsed_time': 's'},
        'depth_datum': 'common assumed seabed datum; positive downward',
        'prediction': [asdict(point) for point in curve],
        'criteria': asdict(criteria), 'measurements': [asdict(p) for p in record],
        'assumed_criteria_ranges': {
            'stiffness_drop_fraction': [0.3, 0.7],
            'rapid_penetration_rate_m_s': [0.005, 0.03],
            'constant_load_tolerance_kn': [0, 2],
        },
        'unpublished_input_ranges': unpublished_input_ranges(),
        'limitations': [
            'No soil profile or installation acceptance is established.',
            'The input ranges are illustrative and are not known site conditions.',
            'Several curve equilibria may exist; reachability is not established.',
            'No reviewed fallback stage or buoyancy-arrest capability is established.',
            'Rate is a sampled average and cannot resolve unsampled transients.',
        ],
        'results': [asdict(result) for result in results],
    }
    output = Path(output)
    output.mkdir(parents=True, exist_ok=True)
    (output / 'penetration-monitoring.json').write_text(
        json.dumps(payload, indent=2, allow_nan=False) + '\n', encoding='utf-8')
    make_figure(curve, record, results).write_json(
        output / 'penetration-monitoring.plotly.json', pretty=True, remove_uids=True)


if __name__ == '__main__':
    if len(sys.argv) != 2:
        raise SystemExit('Usage: jackup_penetration_monitoring.py OUTPUT')
    run_example(sys.argv[1])
