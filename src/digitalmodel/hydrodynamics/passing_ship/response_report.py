# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""HTML sections for computed quasi-static passing-ship response histories."""

from __future__ import annotations

from datetime import date
from html import escape
import json

import numpy as np

from .analysis import AnalysisResult

# Exact SI boundary specified by docs/UNITS.md, "The rule".
_KN_TO_N = 1000.0


def _plot(identifier, time, values, label):
    """Encode data as JSON with HTML-sensitive characters escaped."""

    def encode(value):
        return (
            json.dumps(value, allow_nan=False)
            .replace("<", r"\u003c")
            .replace(">", r"\u003e")
            .replace("&", r"\u0026")
        )

    traces = [
        {
            "type": "scatter",
            "mode": "lines",
            "x": time.tolist(),
            "y": np.asarray(values).tolist(),
            "name": label,
            "hovertemplate": "%{x:.1f} s, %{y:.1f}<extra></extra>",
        }
    ]
    layout = {
        "xaxis": {"title": "Time (s)", "tickformat": ".1f"},
        "yaxis": {"title": label, "tickformat": ".1f"},
    }
    return (
        f'<div id="{identifier}"></div><script>Plotly.newPlot('
        f"{encode(identifier)},{encode(traces)},{encode(layout)});</script>"
    )


def response_sections(results: list[AnalysisResult]) -> str:
    """Return computed envelopes and histories, or an explicit absence notice."""
    if not results:
        return '<section id="response"><h2>Quasi-static response</h2><p>Response results not supplied.</p></section>'
    sections = [
        '<section id="response-summary"><h2>Quasi-static response analysis</h2>'
        "<p>Sampled equilibrium with supplied horizontal line stiffnesses and "
        "compression-only fenders. Dynamic amplification, line strength and "
        "operability acceptance are NOT_EVALUATED.</p></section>",
        "<section><h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th>"
        "<th>Change and reason</th></tr><tr><td>1</td>"
        f"<td>{date.today().isoformat()}</td><td>Initial computed response analysis "
        "to expose tensions and motions.</td></tr></table></section>",
    ]
    for index, case in enumerate(results):
        sections.append(
            f"<section><h2>{escape(case.name)}</h2><h3>Tension envelopes</h3>"
        )
        if case.line_tensions_n is None:
            sections.append("<p>Not requested.</p>")
        else:
            sections.append(
                "<table><tr><th>Line</th><th>Minimum (kN)</th><th>Maximum (kN)</th></tr>"
            )
            for name, tension in case.line_tensions_n.items():
                sections.append(
                    f"<tr><td>{escape(name)}</td><td>{np.min(tension) / _KN_TO_N:.1f}</td>"
                    f"<td>{np.max(tension) / _KN_TO_N:.1f}</td></tr>"
                )
            sections.append("</table>")
        sections.append("<h3>Motion time histories</h3>")
        if case.motions is None:
            sections.append("<p>Not requested.</p>")
        else:
            for dof, label in enumerate(["Surge (m)", "Sway (m)", "Yaw (rad)"]):
                sections.append(
                    _plot(
                        f"response-{index}-{dof}",
                        case.time,
                        case.motions[:, dof],
                        label,
                    )
                )
        if case.fender_loads_n is not None:
            sections.append("<h3>Fender loads</h3>")
            for number, (name, force) in enumerate(case.fender_loads_n.items()):
                sections.append(f"<h4>{escape(name)}</h4>")
                sections.append(
                    _plot(
                        f"fender-{index}-{number}",
                        case.time,
                        force / _KN_TO_N,
                        "Fender load (kN)",
                    )
                )
        if case.forces is not None:
            sections.append("<h3>Combined force time histories</h3>")
            for dof, (values, label) in enumerate(
                [
                    (case.forces.surge, "Surge (N)"),
                    (case.forces.sway, "Sway (N)"),
                    (case.forces.yaw, "Yaw moment (N m)"),
                ]
            ):
                sections.append(_plot(f"force-{index}-{dof}", case.time, values, label))
        sections.append("</section>")
    return "\n".join(sections)
