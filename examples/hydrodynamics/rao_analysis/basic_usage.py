# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Compare analytic oscillator response curves; see README.md, 'Analytic basis'.

These analytic amplitudes illustrate comparison and establish no physical
shielding qualification. Importing or building the figure writes nothing.
Only the executable entrypoint exports HTML, to the required --output path.
"""

import argparse
from pathlib import Path

import plotly.graph_objects as go
from plotly.subplots import make_subplots

from digitalmodel.hydrodynamics.rao_analysis import RAOAnalysis


def build_figure() -> go.Figure:
    """Return an in-memory comparison using the documented hand calculation."""
    # Lamar University, Mechanical Vibrations, Examples 5-6; see README.md.
    mass = 3.0  # kg
    stiffness = 75.0  # N/m
    damping = 90.0  # N s/m
    forcing_frequency = 5.0  # rad/s
    # Static limit and integer harmonics of the published forcing frequency.
    frequencies = tuple(n * forcing_frequency for n in range(4))
    # Divide |F / (k - m*w**2 + i*c*w)| by static deflection F/k.
    # The published forcing amplitude cancels; no reduction factor is imposed.
    reference = tuple(1.0 for _ in frequencies)
    comparison = tuple(
        stiffness / abs(complex(stiffness - mass * omega**2, damping * omega))
        for omega in frequencies
    )

    # One subplot is required by the row/column trace-admission API.
    figure = make_subplots(rows=1, cols=1)
    analysis = RAOAnalysis()
    for name, amplitudes, color in (
        ("Static reference", reference, "blue"),
        ("Damped oscillator", comparison, "orange"),
    ):
        analysis.add_trace_to_fig(
            figure,
            {
                "x": frequencies,
                "y": amplitudes,
                "name": name,
                "line_color": color,
                "legendgroup": name,
                "showlegend": True,
                "mode": "lines+markers",
                "row": 1,
                "col": 1,
            },
        )
    figure.update_layout(
        title="Oscillator comparison — no physical shielding qualification",
        xaxis_title="Angular frequency (rad/s)",
        yaxis_title="Displacement / static deflection (m/m)",
    )
    return figure


def main() -> None:
    """Write standalone HTML only to the caller's explicit output path."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output", type=Path, required=True, help="HTML destination; parent must exist"
    )
    args = parser.parse_args()
    build_figure().write_html(args.output, include_plotlyjs=True, auto_open=False)


if __name__ == "__main__":
    main()
