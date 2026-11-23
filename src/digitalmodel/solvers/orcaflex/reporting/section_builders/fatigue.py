"""
Fatigue section builder.
"""
import plotly.graph_objects as go
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_fatigue_html(report: OrcaFlexAnalysisReport, include_plotlyjs: str = "cdn") -> str:
    """Builds the fatigue results section."""
    if not report.fatigue:
        return "" # Optional section

    fat = report.fatigue
    
    # Damage Profile Chart
    chart_dmg_html = ""
    if fat.damage_per_node:
        s = fat.damage_per_node.get("arc_length", [])
        dmg = fat.damage_per_node.get("damage_yr", [])
        if s and dmg:
            fig = go.Figure(go.Scatter(x=s, y=dmg, mode='lines+markers', line=dict(color='#e74c3c')))
            fig.update_layout(
                title="Annual Fatigue Damage Profile",
                xaxis_title="Arc Length [m]",
                yaxis_title="Damage [1/yr]",
                height=400,
                template="plotly_white"
            )
            chart_dmg_html = fig.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="fatigue-damage-profile")

    body_html = f"""
    <div id="fatigue-metadata">
        <p><strong>Method:</strong> {_escape(fat.method)}</p>
        <p><strong>S-N Curve:</strong> {_escape(fat.sn_curve)}</p>
        <p><strong>SCF:</strong> {fat.scf:.2f}</p>
        <p><strong>Design Life:</strong> {fat.design_life_yrs:.1f} years</p>
    </div>
    
    <h3 id="fatigue-damage-profile">15a. Damage Profile</h3>
    {chart_dmg_html if chart_dmg_html else '<p class="no-data">Damage profile not provided.</p>'}
    """
    
    return wrap_section("fatigue", "15. Fatigue", body_html)
