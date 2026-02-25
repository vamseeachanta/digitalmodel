"""
Static Results section builder.
"""
import plotly.graph_objects as go
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_static_results_html(report: OrcaFlexAnalysisReport, include_plotlyjs: str = "cdn") -> str:
    """Builds the static equilibrium results section."""
    if not report.static_results:
        return ""

    res = report.static_results
    
    # Tension Profile
    chart_te_html = ""
    if res.tension_profile:
        s = res.tension_profile.get("arc_length", [])
        te = res.tension_profile.get("tension", [])
        if s and te:
            fig_te = go.Figure(go.Scatter(x=s, y=te, mode='lines', line=dict(color='#e67e22', width=3)))
            fig_te.update_layout(
                title="Static Effective Tension Profile",
                xaxis_title="Arc Length [m]",
                yaxis_title="Effective Tension [kN]",
                height=400,
                template="plotly_white"
            )
            chart_te_html = fig_te.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="static-tension-profile")

    # BM Profile
    chart_bm_html = ""
    if res.bm_profile:
        s = res.bm_profile.get("arc_length", [])
        bm = res.bm_profile.get("bm", [])
        if s and bm:
            fig_bm = go.Figure(go.Scatter(x=s, y=bm, mode='lines', line=dict(color='#9b59b6', width=3)))
            fig_bm.update_layout(
                title="Static Bending Moment Profile",
                xaxis_title="Arc Length [m]",
                yaxis_title="Bending Moment [kN·m]",
                height=400,
                template="plotly_white"
            )
            chart_bm_html = fig_bm.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="static-bm-profile")

    # Summary Table
    summary_rows = []
    for k, v in res.end_tensions_kn.items():
        summary_rows.append([f"End Tension: {_escape(k)}", f"{v:.1f} kN"])
    if res.tdp_position_m is not None:
        summary_rows.append(["Static TDP Position", f"{res.tdp_position_m:.1f} m"])
    if res.minimum_clearance_m is not None:
        summary_rows.append(["Minimum Clearance", f"{res.minimum_clearance_m:.2f} m"])
    
    summary_table_html = build_table(["Parameter", "Value"], summary_rows)

    body_html = f"""
    <div id="static-summary">
        {summary_table_html}
    </div>
    
    <h3 id="static-tension-profile">11a. Static Tension Profile</h3>
    {chart_te_html if chart_te_html else '<p class="no-data">Tension profile not provided.</p>'}
    
    <h3 id="static-bm-profile">11b. Static BM Profile</h3>
    {chart_bm_html if chart_bm_html else '<p class="no-data">Bending moment profile not provided.</p>'}
    """
    
    return wrap_section("static-results", "11. Static Results", body_html)
