"""
Extreme Results section builder.
"""
import plotly.graph_objects as go
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_extreme_results_html(report: OrcaFlexAnalysisReport, include_plotlyjs: str = "cdn") -> str:
    """Builds the extreme results section with MPM bar charts."""
    if not report.extreme_results or not report.extreme_results.mpm_values:
        return "" # Optional/Conditional

    res = report.extreme_results
    
    # MPM Comparison Chart
    chart_mpm_html = ""
    mpm_vals = res.mpm_values
    if mpm_vals:
        labels = [_escape(v.get("label", "Unknown")) for v in mpm_vals]
        values = [v.get("value", 0.0) for v in mpm_vals]
        
        fig = go.Figure(go.Bar(x=labels, y=values, marker_color='#3498db'))
        fig.update_layout(
            title="Most Probable Maximum (MPM) Values",
            xaxis_title="Variable",
            yaxis_title="MPM Value",
            height=400,
            template="plotly_white"
        )
        chart_mpm_html = fig.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="extreme-mpm-chart")

    body_html = f"""
    <div id="extreme-governing">
        <p><strong>Governing Load Case:</strong> {_escape(res.governing_load_case)}</p>
        <p><strong>Governing Location:</strong> {f"{res.governing_location_arc_m:.1f} m" if res.governing_location_arc_m is not None else "N/A"}</p>
    </div>
    
    <h3 id="extreme-mpm-chart">13a. MPM Comparison Across Load Cases</h3>
    {chart_mpm_html if chart_mpm_html else '<p class="no-data">MPM chart data not available.</p>'}
    """
    
    return wrap_section("extreme-results", "13. Extreme Results", body_html)
