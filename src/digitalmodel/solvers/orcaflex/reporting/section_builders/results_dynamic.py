"""
Dynamic Results section builder.
"""
import plotly.graph_objects as go
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_dynamic_results_html(report: OrcaFlexAnalysisReport, include_plotlyjs: str = "cdn") -> str:
    """Builds the dynamic analysis results section."""
    if not report.dynamic_results:
        return ""  # Omitted if static-only analysis

    res = report.dynamic_results
    
    # Time History Plots
    ts_html = ""
    for ts in res.time_series:
        escaped_label = _escape(ts.label)
        escaped_units = _escape(ts.units)
        escaped_id = _escape(ts.id)
        
        fig = go.Figure(go.Scatter(x=ts.t, y=ts.values, mode='lines', line=dict(width=1)))
        
        # Highlight ramp period
        fig.add_vrect(x0=min(ts.t), x1=res.ramp_end_time_s, 
                      fillcolor="gray", opacity=0.1, line_width=0,
                      annotation_text="Ramp", annotation_position="top left")
        
        fig.update_layout(
            title=f"{escaped_label} Time History",
            xaxis_title="Time [s]",
            yaxis_title=f"{escaped_label} [{escaped_units}]",
            height=400,
            template="plotly_white"
        )
        ts_html += f'<h3 id="dynamic-{escaped_id}">{escaped_label} Time History</h3>'
        ts_html += fig.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id=f"dynamic-{escaped_id}")

    # Envelope Plots
    env_html = ""
    for env in res.envelopes:
        escaped_label = _escape(env.label)
        escaped_units = _escape(env.units)
        escaped_id = _escape(env.id)
        
        fig = go.Figure()
        fig.add_trace(go.Scatter(x=env.arc_length, y=env.max_values, name="Max", line=dict(color='#e74c3c')))
        fig.add_trace(go.Scatter(x=env.arc_length, y=env.min_values, name="Min", line=dict(color='#3498db')))
        
        fig.update_layout(
            title=f"{escaped_label} Envelope",
            xaxis_title="Arc Length [m]",
            yaxis_title=f"{escaped_label} [{escaped_units}]",
            height=400,
            template="plotly_white"
        )
        env_html += f'<h3 id="dynamic-{escaped_id}">{escaped_label} Envelope</h3>'
        env_html += fig.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id=f"dynamic-{escaped_id}")

    # Stats Table
    stats_table_html = ""
    if res.statistical_summary:
        headers = [_escape(h) for h in res.statistical_summary[0].keys()]
        rows = [[_escape(str(item.get(h_raw, "-"))) for h_raw in res.statistical_summary[0].keys()] 
                for item in res.statistical_summary]
        stats_table_html = build_table(headers, rows)

    body_html = f"""
    <div id="dynamic-time-series">
        {ts_html if ts_html else '<p class="no-data">No time history data provided.</p>'}
    </div>
    
    <div id="dynamic-envelopes">
        {env_html if env_html else '<p class="no-data">No envelope data provided.</p>'}
    </div>
    
    <h3 id="dynamic-stats-table">12f. Statistical Summary Table</h3>
    {stats_table_html if stats_table_html else '<p class="no-data">Statistical summary not provided.</p>'}
    """
    
    return wrap_section("dynamic-results", "12. Dynamic Results", body_html)
