"""
Mesh section builder.
"""
import plotly.graph_objects as go
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_mesh_html(report: OrcaFlexAnalysisReport, include_plotlyjs: str = "cdn") -> str:
    """Builds the mesh discretization and quality section."""
    if not report.mesh:
        return ""

    mesh = report.mesh
    
    # 7a. Segment Length Profile
    chart_len_html = ""
    if mesh.segments:
        s = [seg.arc_length_m for seg in mesh.segments]
        l = [seg.length_m for seg in mesh.segments]
        
        fig_len = go.Figure(go.Bar(x=s, y=l, marker_color='#3498db'))
        fig_len.update_layout(
            title="Segment Length Profile",
            xaxis_title="Arc Length [m]",
            yaxis_title="Segment Length [m]",
            height=400,
            template="plotly_white"
        )
        chart_len_html = fig_len.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="mesh-segment-lengths")

    # 7b. Adjacent Ratio Profile
    chart_ratio_html = ""
    if mesh.quality and mesh.quality.adjacent_ratios:
        ratios = mesh.quality.adjacent_ratios
        indices = list(range(len(ratios)))
        
        fig_ratio = go.Figure(go.Scatter(x=indices, y=ratios, mode='lines+markers', line=dict(color='#e67e22')))
        fig_ratio.add_hline(y=3.0, line_dash="dash", line_color="red", annotation_text="Limit (3.0)")
        
        fig_ratio.update_layout(
            title="Adjacent Segment Length Ratio",
            xaxis_title="Segment Interface Index",
            yaxis_title="Ratio Max(L_i, L_i+1) / Min(L_i, L_i+1)",
            height=400,
            template="plotly_white"
        )
        chart_ratio_html = fig_ratio.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="mesh-adjacent-ratio")

    # 7c. Quality Table
    quality_table_html = ""
    if mesh.quality:
        q = mesh.quality
        verdict_badge = "badge-pass" if q.verdict == "PASS" else "badge-warning" if q.verdict == "WARNING" else "badge-fail"
        q_rows = [
            ["Total Segment Count", str(mesh.total_segment_count)],
            ["Max Adjacent Ratio", f"{q.max_adjacent_ratio:.2f}"],
            ["Worst Ratio Location [m]", f"{q.worst_ratio_arc_length_m:.1f}"],
            ["Quality Verdict", f'<span class="badge {verdict_badge}">{_escape(q.verdict)}</span>']
        ]
        quality_table_html = build_table(["Metric", "Value"], q_rows)

    body_html = f"""
    <h3 id="mesh-segment-lengths">7a. Segment Length Profile</h3>
    {chart_len_html if chart_len_html else '<p class="no-data">Segment details not provided.</p>'}
    
    <h3 id="mesh-adjacent-ratio">7b. Adjacent Ratio Profile</h3>
    {chart_ratio_html if chart_ratio_html else '<p class="no-data">Adjacent ratio data not available.</p>'}
    
    <h3 id="mesh-quality-table">7c. Quality Table</h3>
    {quality_table_html if quality_table_html else '<p class="no-data">Quality metrics not available.</p>'}
    """
    
    return wrap_section("mesh", "7. Mesh", body_html)
