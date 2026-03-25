"""
Design Checks section builder.
"""
import plotly.graph_objects as go
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_design_checks_html(report: OrcaFlexAnalysisReport, include_plotlyjs: str = "cdn") -> str:
    """Builds the design check results section with UC heatmap."""
    if not report.design_checks or not report.design_checks.checks:
        return ""

    checks = report.design_checks.checks
    
    # Check Summary Table
    dc_headers = ["Check Name", "Value", "Allowable", "UC", "Load Case", "Status"]
    dc_rows = []
    for c in checks:
        status_badge = "badge-pass" if c.pass_fail else "badge-fail"
        status_text = "PASS" if c.pass_fail else "FAIL"
        
        # Conflict detection
        conflict_badge = ""
        if c.has_conflict:
            conflict_badge = '<span class="badge badge-warning" title="Conflict with derived UC verdict">⚠️ CONFLICT</span>'
        
        dc_rows.append([
            _escape(c.name), 
            f"{c.value:.2f}" if c.value is not None else "-", 
            f"{c.allowable:.2f}" if c.allowable is not None else "-", 
            f"{c.uc:.3f}", 
            _escape(c.load_case), 
            f'<span class="badge {status_badge}">{status_text}</span> {conflict_badge}'
        ])
    dc_table_html = build_table(dc_headers, dc_rows)

    # 14a. UC Heatmap or Bar Chart
    # Determine if we have enough data for a heatmap (multiple locations per check or multiple checks across same locations)
    locations = sorted(list(set(c.location_arc_m for c in checks if c.location_arc_m is not None)))
    check_types = sorted(list(set(c.name for c in checks)))
    
    if len(locations) > 1 and len(check_types) >= 1:
        # Build UC matrix for heatmap
        z = []
        for ct in check_types:
            row = []
            for loc in locations:
                # Find check matching type and location
                match = next((c for c in checks if c.name == ct and c.location_arc_m == loc), None)
                row.append(match.uc if match else None)
            z.append(row)
            
        fig = go.Figure(data=go.Heatmap(
            z=z,
            x=locations,
            y=[_escape(ct) for ct in check_types],
            colorscale=[[0, 'green'], [0.8, 'yellow'], [1.0, 'red']],
            zmin=0,
            zmax=1.2,
            colorbar=dict(title="UC")
        ))
        
        fig.update_layout(
            title="Design Check Utilization Heatmap",
            xaxis_title="Arc Length [m]",
            yaxis_title="Check Category",
            height=300 + (len(check_types) * 40),
            template="plotly_white"
        )
    else:
        # Fallback to bar chart for worst-case only
        fig = go.Figure(go.Bar(
            x=[c.uc for c in checks],
            y=[_escape(c.name) for c in checks],
            orientation='h',
            marker_color=['#27ae60' if c.uc < 0.8 else '#f39c12' if c.uc <= 1.0 else '#e74c3c' for c in checks]
        ))
        
        fig.add_vline(x=1.0, line_dash="dash", line_color="red")
        
        fig.update_layout(
            title="Design Check Utilization (Worst-case)",
            xaxis_title="Utilization Ratio (UC)",
            yaxis_title="Check Category",
            height=300 + (len(checks) * 30),
            margin=dict(l=200),
            template="plotly_white"
        )
    
    chart_html = fig.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="dc-heatmap")

    body_html = f"""
    <div id="design-checks-code">
        <p><strong>Design Code:</strong> {_escape(report.design_checks.code)}</p>
    </div>
    
    <h3 id="dc-summary-table">14b. Summary Table</h3>
    {dc_table_html}
    
    <h3 id="dc-heatmap">14a. UC Heatmap / Worst-case Chart</h3>
    {chart_html}
    """
    
    return wrap_section("design-checks", "14. Design Checks", body_html)
