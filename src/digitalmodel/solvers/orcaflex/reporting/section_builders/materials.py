"""
Materials section builder.
"""
import plotly.graph_objects as go
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_materials_html(report: OrcaFlexAnalysisReport, include_plotlyjs: str = "cdn") -> str:
    """Builds the materials and line types section."""
    if not report.materials or not report.materials.line_types:
        return ""

    mats = report.materials
    
    # Line Type Table
    lt_headers = ["Name", "OD [mm]", "ID [mm]", "WT [mm]", "Grade", "E [GPa]", "Density [kg/m³]"]
    lt_rows = [
        [_escape(lt.name), f"{lt.od*1000:.1f}", f"{lt.id*1000:.1f}", f"{lt.wt*1000:.1f}", 
         _escape(lt.grade), f"{(lt.youngs_modulus_mpa/1000):.1f}" if lt.youngs_modulus_mpa else "-", 
         f"{lt.density_kg_m3:.0f}"]
        for lt in mats.line_types
    ]
    lt_table_html = build_table(lt_headers, lt_rows)

    # 5a. Section Properties Chart (EA, EI)
    names = [_escape(lt.name) for lt in mats.line_types]
    ea_values = [lt.ea_kn if lt.ea_kn else 0.0 for lt in mats.line_types]
    ei_values = [lt.ei_knm2 if lt.ei_knm2 else 0.0 for lt in mats.line_types]
    
    fig_props = go.Figure()
    fig_props.add_trace(go.Bar(x=names, y=ea_values, name="EA [kN]", yaxis="y"))
    fig_props.add_trace(go.Bar(x=names, y=ei_values, name="EI [kN·m²]", yaxis="y2"))
    
    fig_props.update_layout(
        title="Computed Section Properties",
        yaxis=dict(title="Axial Stiffness EA [kN]", side="left"),
        yaxis2=dict(title="Bending Stiffness EI [kN·m²]", overlaying="y", side="right"),
        barmode='group',
        height=400,
        template="plotly_white",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1)
    )
    chart_props_html = fig_props.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="materials-section-props")

    # 5b. Submerged Weight Profile
    chart_ws_html = ""
    if mats.submerged_weight_profile:
        s = mats.submerged_weight_profile.get("arc_length", [])
        ws = mats.submerged_weight_profile.get("w_s", [])
        if s and ws:
            fig_ws = go.Figure(go.Scatter(x=s, y=ws, mode='lines+markers', line=dict(color='#2ecc71')))
            fig_ws.update_layout(
                title="Submerged Weight Profile",
                xaxis_title="Arc Length [m]",
                yaxis_title="Submerged Weight w_s [kN/m]",
                height=400,
                template="plotly_white"
            )
            chart_ws_html = fig_ws.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="materials-submerged-weight")

    body_html = f"""
    <h3 id="materials-linetype-table">Line Type Properties</h3>
    {lt_table_html}
    
    <h3 id="materials-section-props">5a. Section Properties</h3>
    {chart_props_html}
    
    <h3 id="materials-submerged-weight">5b. Submerged Weight Profile</h3>
    {chart_ws_html if chart_ws_html else '<p class="no-data">Submerged weight profile not provided.</p>'}
    """
    
    return wrap_section("materials", "5. Materials", body_html)
