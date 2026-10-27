# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Compare analytic RAO curves; see README.md, 'Analytic basis'.

These synthetic amplitudes illustrate comparison and establish no physical
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
    # All curve constants are synthetic choices in README.md, 'Analytic basis'.
    periods = (4.0, 8.0, 12.0, 16.0)
    reference = tuple(1.0 / (1.0 + (period / 8.0 - 1.0) ** 2) for period in periods)
    comparison = tuple(0.75 * amplitude for amplitude in reference)

    # One subplot is required by the row/column trace-admission API.
    figure = make_subplots(rows=1, cols=1)
    analysis = RAOAnalysis()
    for name, amplitudes, color in (
        ("Synthetic reference", reference, "blue"),
        ("Synthetic scaled comparison", comparison, "orange"),
    ):
        analysis.add_trace_to_fig(
            figure,
            {
                "x": periods,
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
        title="Synthetic RAO comparison — no physical shielding qualification",
        xaxis_title="Period (s)",
        yaxis_title="Amplitude RAO (m/m)",
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
