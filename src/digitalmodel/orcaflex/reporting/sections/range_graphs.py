"""Section 4: Range graphs — arclength vs min/max envelope heatmaps."""
from __future__ import annotations
import json


def build_range_graphs(model, config) -> str:
    """Build range graph section HTML with Plotly charts."""
    import OrcFxAPI

    lines = [o for o in model.objects if isinstance(o, OrcFxAPI.Line)]
    if not lines:
        return "<p>No line objects found for range graph extraction.</p>"

    charts = []
    chart_id = 0

    for line in lines[:4]:  # max 4 lines
        for variable, unit in [("Effective Tension", "kN"), ("Bend Moment", "kN.m")]:
            try:
                rg = line.RangeGraph(variable)
                if not rg.Min or not rg.Max:
                    continue

                arc = list(rg.X)
                div_id = f"rg_chart_{chart_id}"
                chart_id += 1

                traces = [
                    {"x": arc, "y": list(rg.Max), "mode": "lines", "name": "Max",
                     "line": {"color": "#dc3545", "width": 1.5}, "fill": "tonexty",
                     "fillcolor": "rgba(220,53,69,0.15)"},
                    {"x": arc, "y": list(rg.Min), "mode": "lines", "name": "Min",
                     "line": {"color": "#0d6efd", "width": 1.5}},
                ]
                if hasattr(rg, "Mean") and rg.Mean:
                    traces.append({
                        "x": arc, "y": list(rg.Mean), "mode": "lines", "name": "Mean",
                        "line": {"color": "#198754", "width": 1, "dash": "dash"},
                    })

                layout = {
                    "title": {"text": f"{line.name} — {variable}", "font": {"size": 13}},
                    "xaxis": {"title": "Arc length (m)"},
                    "yaxis": {"title": unit},
                    "margin": {"l": 50, "r": 20, "t": 40, "b": 40},
                    "height": 250,
                    "legend": {"orientation": "h"},
                }
                charts.append(
                    f'<div id="{div_id}" style="width:100%;margin-bottom:1rem;"></div>\n'
                    f'<script>Plotly.newPlot("{div_id}", '
                    f'{json.dumps(traces)}, {json.dumps(layout)}, '
                    f'{{responsive:true, displayModeBar:false}});</script>'
                )
            except Exception:
                pass

    if not charts:
        return "<p>No range graph data available (model may be static-only or variables absent).</p>"

    return "\n".join(charts)
