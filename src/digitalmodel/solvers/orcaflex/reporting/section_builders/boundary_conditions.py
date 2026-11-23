"""
Boundary Conditions section builder.
"""
import plotly.graph_objects as go
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_boundary_conditions_html(report: OrcaFlexAnalysisReport, include_plotlyjs: str = "cdn") -> str:
    """Builds the boundary conditions and seabed interaction section."""
    if not report.boundary_conditions:
        return ""

    bc = report.boundary_conditions
    
    # End BCs Table
    bc_rows = []
    if bc.end_a:
        e = bc.end_a
        bc_rows.append([_escape(e.name), _escape(e.type), f"({e.x:.1f}, {e.y:.1f}, {e.z:.1f})", 
                        _escape(e.connected_to or "Seabed/Fixed"), _escape(e.dof_fixity or "All Fixed")])
    if bc.end_b:
        e = bc.end_b
        bc_rows.append([_escape(e.name), _escape(e.type), f"({e.x:.1f}, {e.y:.1f}, {e.z:.1f})", 
                        _escape(e.connected_to or "Seabed/Fixed"), _escape(e.dof_fixity or "All Fixed")])
    
    bc_table_html = build_table(["Location", "Type", "Position (X, Y, Z)", "Connected To", "DOF Fixity"], bc_rows)

    # Seabed Table
    seabed_table_html = ""
    if bc.seabed:
        sb = bc.seabed
        sb_rows = [
            ["Model Type", _escape(sb.type)],
            ["Normal Stiffness [kN/m²]", f"{sb.stiffness_kn_m2:.1f}" if sb.stiffness_kn_m2 is not None else "-"],
            ["Axial Friction μ_ax", f"{sb.friction_axial:.2f}" if sb.friction_axial is not None else "-"],
            ["Lateral Friction μ_lat", f"{sb.friction_lateral:.2f}" if sb.friction_lateral is not None else "-"],
            ["Slope [deg]", f"{sb.slope_deg:.1f}" if sb.slope_deg is not None else "0.0"]
        ]
        seabed_table_html = build_table(["Property", "Value"], sb_rows)

    # 6a. BC Annotation Overlay
    chart_bc_html = ""
    if report.geometry and report.geometry.line_profile:
        prof = report.geometry.line_profile
        fig = go.Figure(go.Scatter(x=prof.x, y=prof.z, mode='lines', line=dict(color='#bdc3c7', width=1), name="Structure"))
        
        # Add End A marker
        if bc.end_a:
             fig.add_trace(go.Scatter(x=[bc.end_a.x], y=[bc.end_a.z], mode='markers', 
                                     marker=dict(size=12, symbol='square', color='#e74c3c'), name="End A"))
        # Add End B marker
        if bc.end_b:
             fig.add_trace(go.Scatter(x=[bc.end_b.x], y=[bc.end_b.z], mode='markers', 
                                     marker=dict(size=12, symbol='circle', color='#2ecc71'), name="End B"))
        
        fig.update_layout(
            title="Boundary Condition Locations",
            xaxis_title="X [m]",
            yaxis_title="Z [m]",
            height=400,
            template="plotly_white"
        )
        chart_bc_html = fig.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="bc-annotation")

    body_html = f"""
    <h3 id="bc-end-table">End Boundary Conditions</h3>
    {bc_table_html}
    
    <h3 id="bc-seabed-table">Seabed Model</h3>
    {seabed_table_html if seabed_table_html else '<p class="no-data">Seabed model details not provided.</p>'}
    
    <h3 id="bc-annotation">6a. BC Annotation Overlay</h3>
    {chart_bc_html if chart_bc_html else '<p class="no-data">Visual representation not available.</p>'}
    """
    
    return wrap_section("boundary-conditions", "6. Boundary Conditions", body_html)
