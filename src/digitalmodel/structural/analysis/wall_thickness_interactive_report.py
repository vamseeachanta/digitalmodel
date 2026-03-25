# ABOUTME: Interactive HTML report builder with Tom Select code selector and Plotly charts
# ABOUTME: Multi-code overlay charts with toggleable visibility per design code

"""Interactive Multi-Code Wall Thickness Report.

Generates an HTML report with:
- Tom Select searchable multi-select for design code toggling
- Utilisation vs wall thickness overlay chart (one trace per code x check)
- Phase comparison grouped bar chart
- All charts respond to code selection changes via Plotly.restyle()
"""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional

import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY
from digitalmodel.structural.analysis.wall_thickness_parametric import (
    API_5L_GRADES,
    ParametricSweep,
    SweepConfig,
)
from digitalmodel.structural.analysis.wall_thickness_phases import (
    PhaseAnalysisRunner,
    PhaseComparisonResult,
    PipeDefinition,
    create_standard_phases,
)

logger = logging.getLogger(__name__)

# Consistent color palette for codes
CODE_COLORS = {
    "DNV-ST-F101": "#1f77b4",
    "API-RP-1111": "#ff7f0e",
    "PD-8010-2": "#2ca02c",
    "ASME-B31.8": "#d62728",
    "ISO-13623": "#9467bd",
}

# Dash styles for check types
CHECK_DASHES = {
    "pressure_containment": "solid",
    "hoop_stress": "solid",
    "burst": "solid",
    "collapse": "dash",
    "propagation_buckling": "dot",
    "propagation": "dot",
    "combined_loading": "dashdot",
}


class InteractiveReportBuilder:
    """Build an interactive HTML report with multi-code toggling."""

    def __init__(self):
        self._charts: List[Dict] = []
        self._code_values: List[str] = []

    def add_utilisation_vs_wt_chart(
        self,
        geometry: PipeGeometry,
        material: PipeMaterial,
        internal_pressure: float,
        external_pressure: float,
        codes: List[DesignCode],
        safety_class: SafetyClass = SafetyClass.MEDIUM,
        wt_min: float = 0.010,
        wt_max: float = 0.040,
        wt_steps: int = 15,
    ):
        """Add a utilisation vs wall thickness overlay chart.

        One trace per (code, check) combination.
        Color = code, dash pattern = check type.
        """
        self._code_values = [c.value for c in codes]
        fig = go.Figure()

        wt_values = np.linspace(wt_min, wt_max, wt_steps)
        factors = DesignFactors(safety_class=safety_class)

        for code in codes:
            code_color = CODE_COLORS.get(code.value, "#999")

            # Run sweep for this code
            check_data: Dict[str, List[float]] = {}
            valid_wts: List[float] = []

            for wt in wt_values:
                if wt >= geometry.outer_diameter / 2 or wt <= 0:
                    continue
                try:
                    geom = PipeGeometry(
                        outer_diameter=geometry.outer_diameter,
                        wall_thickness=wt,
                        corrosion_allowance=geometry.corrosion_allowance,
                        fabrication_tolerance=geometry.fabrication_tolerance,
                    )
                    loads = DesignLoads(
                        internal_pressure=internal_pressure,
                        external_pressure=external_pressure,
                    )
                    analyzer = WallThicknessAnalyzer(geom, material, loads, factors, code)
                    result = analyzer.perform_analysis()

                    if not valid_wts or wt != valid_wts[-1]:
                        valid_wts.append(wt)
                    for check_name, util_val in result.checks.items():
                        check_data.setdefault(check_name, []).append(util_val)
                except (ValueError, ZeroDivisionError):
                    continue

            wt_mm = [w * 1000 for w in valid_wts]
            for check_name, utils in check_data.items():
                dash = CHECK_DASHES.get(check_name, "solid")
                fig.add_trace(go.Scatter(
                    x=wt_mm,
                    y=utils,
                    mode="lines+markers",
                    name=f"{code.value} — {check_name}",
                    line=dict(color=code_color, dash=dash),
                    marker=dict(size=4),
                    meta=dict(code=code.value),
                ))

        fig.add_hline(y=1.0, line_dash="dash", line_color="red",
                      annotation_text="Utilisation = 1.0")

        fig.update_layout(
            title="Utilisation vs Wall Thickness (Multi-Code)",
            xaxis_title="Wall Thickness (mm)",
            yaxis_title="Utilisation Ratio",
            template="plotly_white",
            hovermode="x unified",
            height=600,
        )

        self._charts.append({
            "id": "chart-util-vs-wt",
            "title": "Utilisation vs Wall Thickness",
            "fig": fig,
        })

    def add_phase_comparison_chart(
        self,
        comparison: PhaseComparisonResult,
    ):
        """Add a phase comparison bar chart with per-code grouping."""
        codes = comparison.codes
        self._code_values = list(set(self._code_values) | {c.value for c in codes})

        df = comparison.to_dataframe()
        phase_names = [p.name for p in comparison.phases]

        fig = go.Figure()

        for code in codes:
            code_color = CODE_COLORS.get(code.value, "#999")
            code_df = df[df["code"] == code.value]
            checks = sorted(code_df["check"].unique())

            for check in checks:
                check_df = code_df[code_df["check"] == check]
                check_df = check_df.set_index("phase").reindex(phase_names).reset_index()

                fig.add_trace(go.Bar(
                    x=check_df["phase"],
                    y=check_df["utilisation"],
                    name=f"{code.value} — {check}",
                    marker_color=code_color,
                    opacity=0.7 + 0.3 * (checks.index(check) / max(len(checks) - 1, 1)),
                    meta=dict(code=code.value),
                ))

        fig.add_hline(y=1.0, line_dash="dash", line_color="red",
                      annotation_text="Utilisation = 1.0")

        fig.update_layout(
            title="Phase Utilisation Comparison (Multi-Code)",
            xaxis_title="Phase",
            yaxis_title="Utilisation",
            barmode="group",
            template="plotly_white",
            height=600,
        )

        self._charts.append({
            "id": "chart-phase-comparison",
            "title": "Phase Comparison",
            "fig": fig,
        })

    def build(self, output_path: str, title: str = "Interactive Wall Thickness Report") -> str:
        """Build and write the interactive HTML report.

        Args:
            output_path: File path for the HTML output.
            title: Report title.

        Returns:
            The HTML string.
        """
        # Build chart HTML divs
        chart_html_parts = []
        for i, chart in enumerate(self._charts):
            include_js = "cdn" if i == 0 else False
            chart_div = chart["fig"].to_html(
                full_html=False,
                include_plotlyjs=include_js,
                div_id=chart["id"],
            )
            chart_html_parts.append(f"""
                <div class="chart-section">
                    <h2>{chart["title"]}</h2>
                    {chart_div}
                </div>
            """)

        charts_combined = "\n".join(chart_html_parts)

        # Tom Select options
        code_options = "\n".join(
            f'<option value="{cv}" selected>{cv}</option>'
            for cv in sorted(set(self._code_values))
        )

        # Build chart IDs for JS
        chart_ids_json = json.dumps([c["id"] for c in self._charts])

        html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>{title}</title>
    <link href="https://cdn.jsdelivr.net/npm/tom-select@2.3.1/dist/css/tom-select.css" rel="stylesheet">
    <script src="https://cdn.jsdelivr.net/npm/tom-select@2.3.1/dist/js/tom-select.complete.min.js"></script>
    <style>
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            margin: 0; padding: 0; color: #333; background: #f5f6fa;
        }}
        .header {{
            background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
            color: white; padding: 2em; margin-bottom: 2em;
        }}
        .header h1 {{ margin: 0; font-size: 1.8em; }}
        .header p {{ margin: 0.5em 0 0; opacity: 0.8; }}
        .container {{ max-width: 1200px; margin: 0 auto; padding: 0 2em 2em; }}
        .controls {{
            background: white; border-radius: 8px; padding: 1.5em;
            box-shadow: 0 2px 8px rgba(0,0,0,0.1); margin-bottom: 2em;
        }}
        .controls label {{ font-weight: 600; display: block; margin-bottom: 0.5em; }}
        .chart-section {{
            background: white; border-radius: 8px; padding: 1.5em;
            box-shadow: 0 2px 8px rgba(0,0,0,0.1); margin-bottom: 2em;
        }}
        .chart-section h2 {{ color: #16213e; margin-top: 0; }}
        .footer {{
            text-align: center; padding: 1em; color: #888; font-size: 0.85em;
        }}
    </style>
</head>
<body>
    <div class="header">
        <h1>{title}</h1>
        <p>Toggle design codes below to compare results</p>
    </div>
    <div class="container">
        <div class="controls">
            <label for="code-select">Select Design Codes:</label>
            <select id="code-select" multiple placeholder="Search codes...">
                {code_options}
            </select>
        </div>
        {charts_combined}
    </div>
    <div class="footer">
        Generated by digitalmodel — wall thickness multi-code analysis
    </div>
    <script>
        // Initialize Tom Select
        var codeSelect = new TomSelect('#code-select', {{
            plugins: ['remove_button'],
            maxItems: null,
        }});

        var chartIds = {chart_ids_json};

        codeSelect.on('change', function(values) {{
            var selectedCodes = values ? values.split(',') : [];
            chartIds.forEach(function(chartId) {{
                var plotDiv = document.getElementById(chartId);
                if (!plotDiv || !plotDiv.data) return;
                var visibility = [];
                for (var i = 0; i < plotDiv.data.length; i++) {{
                    var trace = plotDiv.data[i];
                    if (trace.meta && trace.meta.code) {{
                        visibility.push(selectedCodes.indexOf(trace.meta.code) >= 0);
                    }} else {{
                        visibility.push(true);
                    }}
                }}
                Plotly.restyle(plotDiv, {{'visible': visibility}});
            }});
        }});
    </script>
</body>
</html>"""

        path = Path(output_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        with open(path, "w", encoding="utf-8") as f:
            f.write(html)

        logger.info("Interactive report written to %s", output_path)
        return html
