# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""HTML sections for computed quasi-static passing-ship response histories."""

from __future__ import annotations

from html import escape
import json

import numpy as np

from .analysis import AnalysisResult

# Exact SI boundary specified by docs/UNITS.md, "Rule 1".
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
            "hovertemplate": "%{x:.3g} s, %{y:.3g}<extra></extra>",
        }
    ]
    layout = {
        "xaxis": {"title": "Time (s)", "tickformat": ".3g"},
        "yaxis": {"title": label, "tickformat": ".3g"},
    }
    return (
        f'<div id="{identifier}"></div><script>Plotly.newPlot('
        f"{encode(identifier)},{encode(traces)},{encode(layout)});</script>"
    )


def _applicability(case: AnalysisResult) -> str:
    """Compare time scales without inventing a dynamic acceptance criterion."""
    text = (
        "<h3>Quasi-static applicability</h3><p>NOT_EVALUATED: "
        "No acceptance criterion supplied for the period/passage-duration ratio. "
    )
    period, duration = case.surge_natural_period_s, case.passage_duration_s
    if period is None:
        return text + "Response period estimate not supplied.</p>"
    if not np.isfinite(period) or period <= 0:
        raise ValueError("surge period must be finite and positive")
    text += f"Uncoupled surge period estimate: {period:.3g} s. "
    if case.applicability_basis is None:
        text += "Numerical basis inputs not supplied; the estimate is unverified. "
    else:
        for key, label, unit in (
            ("mass_kg", "Displacement mass", "kg"),
            ("surge_stiffness_n_m", "Bare-line Kxx", "N/m"),
            ("moored_length_m", "Moored hull length", "m"),
            ("passing_length_m", "Passing hull length", "m"),
            ("speed_m_s", "Case speed", "m/s"),
        ):
            value = case.applicability_basis[key]
            if not np.isfinite(value) or value <= 0:
                raise ValueError("applicability basis must be finite and positive")
            text += f"{label}: {value:.3g} {unit}. "
    if duration is None:
        text += "Passage duration and ratio are not applicable: no passage load. "
    else:
        if not np.isfinite(duration) or duration <= 0:
            raise ValueError("passage duration must be finite and positive")
        text += (f"Passage duration: {duration:.3g} s; "
                 f"period/passage-duration ratio: {period / duration:.3g}. ")
    return text + (
        "T=2*pi*sqrt(m/Kxx), using displacement mass in kg and bare-line surge "
        "stiffness in N/m (OpenStax, University Physics Volume 1, 2016, section "
        "15.1, Simple Harmonic Motion); duration=(L1+L2)/U uses both hull lengths "
        "and the case speed. Added mass, coupling and fender contact stiffness "
        "are excluded from the period estimate; no dynamic pass is established.</p>"
    )


def response_sections(
    results: list[AnalysisResult], *,
    revision_history: list[dict[str, str]] | None = None,
) -> str:
    """Return computed envelopes and histories, or an explicit absence notice."""
    if not results:
        return '<section id="response"><h2>Quasi-static response</h2><p>Response results not supplied.</p></section>'
    sections = [
        '<section id="response-summary"><h2>Quasi-static response analysis</h2>'
        "<p>Sampled equilibrium with supplied horizontal line stiffnesses and "
        "compression-only fenders. Dynamic amplification, line strength and "
        "operability acceptance are NOT_EVALUATED.</p></section>",
        "<section><h2>Revision history</h2>",
    ]
    if revision_history:
        sections.append("<table><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr>")
        for row in revision_history:
            cells = "".join(f"<td>{escape(row[key])}</td>" for key in ("revision", "date", "change"))
            sections.append(f"<tr>{cells}</tr>")
        sections.append("</table>")
    else:
        sections.append("<p>No revision record supplied; issue history is not established.</p>")
    sections.append("</section>")
    for index, case in enumerate(results):
        sections.append(
            f"<section><h2>{escape(case.name)}</h2>{_applicability(case)}<h3>Tension envelopes</h3>"
        )
        if case.line_tensions_n is None:
            sections.append("<p>Not requested.</p>")
        else:
            sections.append(
                "<table><tr><th>Line</th><th>Minimum (N)</th><th>Maximum (N)</th></tr>"
            )
            for name, tension in case.line_tensions_n.items():
                sections.append(
                    f"<tr><td>{escape(name)}</td><td>{np.min(tension):.9g}</td>"
                    f"<td>{np.max(tension):.9g}</td></tr>"
                )
            sections.append("</table>")
        sections.append("<h3>Motion time histories</h3>")
        if case.motions is None:
            sections.append("<p>Not requested.</p>")
        else:
            for dof, label in enumerate(["Surge (m)", "Sway (m)", "Yaw (mrad)"]):
                sections.append(
                    _plot(
                        f"response-{index}-{dof}",
                        case.time,
                        case.motions[:, dof] * (1000 if dof == 2 else 1),
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
