"""Section 3: Dynamic time series — Plotly line charts for key variables."""
from __future__ import annotations

from ._common import lines, plotly_chart, vessels


def build_time_series(model, config) -> str:
    """Build time series section HTML with Plotly charts."""
    import OrcFxAPI

    line_objs = lines(model)
    vessel_objs = vessels(model)

    charts = []
    chart_id = 0

    def _make_chart(title: str, x_data: list, y_data: list, ylabel: str) -> str:
        nonlocal chart_id
        div_id = f"ts_chart_{chart_id}"
        chart_id += 1
        trace = {"x": x_data, "y": y_data, "mode": "lines", "name": title,
                 "line": {"width": 1.5}}
        layout = {
            "title": {"text": title, "font": {"size": 13}},
            "xaxis": {"title": "Time (s)"},
            "yaxis": {"title": ylabel},
            "margin": {"l": 50, "r": 20, "t": 40, "b": 40},
            "height": 250,
        }
        return plotly_chart(div_id, [trace], layout)

    # Vessel DOF time series
    for vessel in vessel_objs[:2]:  # max 2 vessels
        for dof, label, unit in [("X", "Surge", "m"), ("Z", "Heave", "m"), ("Ry", "Pitch", "°")]:
            try:
                th = vessel.TimeHistory(dof)
                t = list(model.SampleTimes())
                if len(t) == len(th):
                    charts.append(_make_chart(
                        f"{vessel.name} — {label}", list(t), list(th), unit
                    ))
            except Exception:
                pass

    # Line end tension time series
    for line in line_objs[:3]:  # max 3 lines
        try:
            th = line.TimeHistory("Effective Tension", OrcFxAPI.oeEndA)
            t = list(model.SampleTimes())
            if len(t) == len(th):
                charts.append(_make_chart(
                    f"{line.name} — Top Tension", list(t), list(th), "kN"
                ))
        except Exception:
            pass

    if not charts:
        return "<p>No dynamic time series available (model may be static-only).</p>"

    return "\n".join(charts)
