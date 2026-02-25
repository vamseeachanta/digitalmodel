"""
Geometry section builder.
"""
import plotly.graph_objects as go
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_geometry_html(report: OrcaFlexAnalysisReport, include_plotlyjs: str = "cdn") -> str:
    """Builds the structural geometry section with 3D and 2D profiles."""
    if not report.geometry or not report.geometry.line_profile:
        return ""

    geom = report.geometry
    prof = geom.line_profile
    
    # 4a. 3D Line Profile
    fig3d = go.Figure(go.Scatter3d(
        x=prof.x, y=prof.y, z=prof.z,
        mode='lines',
        line=dict(color='#3498db', width=5),
        name="Static shape",
    ))
    
    # Add seabed surface if water depth available
    if geom.water_depth_m is not None:
        depth = -geom.water_depth_m
        fig3d.add_surface(
            x=[min(prof.x)-50, max(prof.x)+50],
            y=[min(prof.y)-50, max(prof.y)+50],
            z=[[depth, depth], [depth, depth]],
            opacity=0.3,
            showscale=False,
            colorscale=[[0, '#bdc3c7'], [1, '#bdc3c7']]
        )
        
    fig3d.update_layout(
        title="3D Static Configuration",
        scene=dict(
            xaxis_title="X [m]",
            yaxis_title="Y [m]",
            zaxis_title="Z [m]",
            aspectmode='data'
        ),
        height=600,
        template="plotly_white"
    )
    chart3d_html = fig3d.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="geometry-3d")

    # 4b. 2D Vertical Profile (XZ Plane)
    fig2dxz = go.Figure(go.Scatter(
        x=prof.x, y=prof.z,
        mode='lines',
        line=dict(color='#3498db', width=3),
        name="XZ shape",
    ))
    
    if geom.water_depth_m is not None:
        fig2dxz.add_hline(y=-geom.water_depth_m, line_dash="dash", line_color="#bdc3c7", annotation_text="Seabed")

    fig2dxz.update_layout(
        title="Vertical Profile (XZ Plane)",
        xaxis_title="X [m]",
        yaxis_title="Z [m]",
        height=500,
        template="plotly_white"
    )
    chart2dxz_html = fig2dxz.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="geometry-2d")

    # 4c. Plan View (XY Plane)
    fig2dxy = go.Figure(go.Scatter(
        x=prof.x, y=prof.y,
        mode='lines',
        line=dict(color='#3498db', width=3),
        name="Plan view",
    ))
    
    fig2dxy.update_layout(
        title="Horizontal Profile (Plan View)",
        xaxis_title="X [m]",
        yaxis_title="Y [m]",
        height=500,
        template="plotly_white"
    )
    chart2dxy_html = fig2dxy.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="geometry-plan")

    # Key Points Table
    kp_rows = [[_escape(kp.label), f"{kp.arc_length_m:.1f}", f"{kp.x:.1f}" if kp.x is not None else "-", 
                 f"{kp.y:.1f}" if kp.y is not None else "-", f"{kp.z:.1f}" if kp.z is not None else "-"] 
                for kp in geom.key_points]
    kp_table_html = build_table(["Label", "Arc Length [m]", "X [m]", "Y [m]", "Z [m]"], kp_rows) if kp_rows else ""

    body_html = f"""
    <div id="geometry-overview">
        <p><strong>Coordinate System:</strong> {_escape(geom.coordinate_system or "MSL")}</p>
        <p><strong>Water Depth:</strong> {geom.water_depth_m if geom.water_depth_m is not None else "-"} m</p>
    </div>
    
    <h3 id="geometry-3d">4a. 3D Line Profile</h3>
    {chart3d_html}
    
    <h3 id="geometry-2d">4b. 2D Vertical Profile</h3>
    {chart2dxz_html}
    
    <h3 id="geometry-plan">4c. Plan View</h3>
    {chart2dxy_html}
    
    <h3 id="geometry-key-points">Key Points Table</h3>
    {kp_table_html}
    """
    
    return wrap_section("geometry", "4. Geometry", body_html)
