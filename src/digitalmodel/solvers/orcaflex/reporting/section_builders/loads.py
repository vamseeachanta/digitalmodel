"""
Loads and Environment section builder.
"""
import plotly.graph_objects as go
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_loads_html(report: OrcaFlexAnalysisReport, include_plotlyjs: str = "cdn") -> str:
    """Builds the loads and environmental conditions section."""
    if not report.loads or not report.loads.load_cases:
        return ""

    loads = report.loads
    
    # 9c. Load Case Table
    lc_headers = ["ID", "Hs [m]", "Tp [s]", "γ", "U_curr [m/s]", "Dir [deg]", "Pi [MPa]"]
    lc_rows = [
        [_escape(lc.case_id), f"{lc.hs_m:.2f}" if lc.hs_m is not None else "-", 
         f"{lc.tp_s:.1f}" if lc.tp_s is not None else "-", 
         f"{lc.gamma:.2f}" if lc.gamma is not None else "-",
         f"{lc.current_velocity_m_s:.2f}" if lc.current_velocity_m_s is not None else "-",
         f"{lc.current_direction_deg:.1f}" if lc.current_direction_deg is not None else "-",
         f"{lc.internal_pressure_mpa:.1f}" if lc.internal_pressure_mpa is not None else "-"]
        for lc in loads.load_cases
    ]
    lc_table_html = build_table(lc_headers, lc_rows)

    # 9a. Current Profile
    chart_curr_html = ""
    if loads.current_profile:
        z = loads.current_profile.get("depth", [])
        u = loads.current_profile.get("velocity", [])
        if z and u:
            fig_curr = go.Figure(go.Scatter(x=u, y=z, mode='lines+markers', line=dict(color='#3498db')))
            fig_curr.update_layout(
                title="Design Current Profile",
                xaxis_title="Velocity [m/s]",
                yaxis_title="Z-elevation [m]",
                height=400,
                template="plotly_white"
            )
            chart_curr_html = fig_curr.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="loads-current-profile")

    # Hydro Coefficients Table
    hydro_table_html = ""
    if loads.hydrodynamic_coefficients:
        h_headers = ["Line Type", "Cd Normal", "Cd Axial", "Ca Normal", "Ca Axial"]
        h_rows = [
            [_escape(h.line_type_name), f"{h.cd_normal:.2f}", f"{h.cd_axial:.2f}" if h.cd_axial is not None else "-",
             f"{h.ca_normal:.2f}", f"{h.ca_axial:.2f}" if h.ca_axial is not None else "-"]
            for h in loads.hydrodynamic_coefficients
        ]
        hydro_table_html = build_table(h_headers, h_rows)

    body_html = f"""
    <h3 id="loads-case-table">9c. Load Case Matrix</h3>
    {lc_table_html}
    
    <h3 id="loads-current-profile">9a. Current Profile</h3>
    {chart_curr_html if chart_curr_html else '<p class="no-data">Current profile not provided.</p>'}
    
    <h3 id="loads-hydro-table">Hydrodynamic Coefficients</h3>
    {hydro_table_html if hydro_table_html else '<p class="no-data">Hydrodynamic coefficients not provided.</p>'}
    """
    
    return wrap_section("loads", "9. Loads & Environment", body_html)
