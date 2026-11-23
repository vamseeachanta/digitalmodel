"""
Executive Summary section builder.
"""
import plotly.graph_objects as go
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_executive_summary_html(report: OrcaFlexAnalysisReport, include_plotlyjs: str = "cdn") -> str:
    """Builds the executive summary with overall verdict and worst-case UC."""
    if not report.design_checks or not report.design_checks.checks:
        return ""

    checks = report.design_checks.checks
    names = [_escape(c.name) for c in checks]
    ucs = [c.uc for c in checks]
    
    # Create bar chart
    colors = ['#27ae60' if uc < 0.8 else '#f39c12' if uc <= 1.0 else '#e74c3c' for uc in ucs]
    
    fig = go.Figure(go.Bar(
        x=ucs,
        y=names,
        orientation='h',
        marker_color=colors,
    ))
    
    fig.add_vline(x=1.0, line_dash="dash", line_color="red", annotation_text="Limit")
    
    fig.update_layout(
        title="Design Check Utilization Summary",
        xaxis_title="Utilization Ratio (UC)",
        yaxis_title="Check Category",
        height=300 + (len(names) * 30),
        margin=dict(l=200),
        template="plotly_white"
    )
    
    chart_html = fig.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="exec-uc-chart")

    # Worst case info
    worst_check = max(checks, key=lambda x: x.uc)
    status = report.overall_pass
    if status is True:
        verdict_badge, verdict_text = "badge-pass", "ALL PASS"
    elif status is False:
        verdict_badge, verdict_text = "badge-fail", "FAIL"
    else:
        verdict_badge, verdict_text = "badge-warning", "NO CHECKS"

    # Conflict detection
    has_any_conflict = any(c.has_conflict for c in checks)
    conflict_warning = ""
    if has_any_conflict:
        conflict_warning = '<div class="badge badge-warning">⚠️ CONFLICT DETECTED IN CHECKS</div>'

    # Build recommendations list
    recs_list = "".join([f"<li>{_escape(r)}</li>" for r in report.recommendations]) if report.recommendations else "<li>No specific recommendations at this time.</li>"
    
    compliance_text = "COMPLIANT" if report.overall_pass is True else ("NON-COMPLIANT" if report.overall_pass is False else "NOT EVALUATED")

    body_html = f"""
    <div class="grid">
        <div class="stat-card">
            <div class="stat-label">Overall Verdict</div>
            <div class="stat-value">
                <span class="badge {verdict_badge}" style="font-size: 1.5rem;">{verdict_text}</span>
                {conflict_warning}
            </div>
        </div>
        <div class="stat-card">
            <div class="stat-label">Max Utilization</div>
            <div class="stat-value" style="color: {colors[ucs.index(worst_check.uc)]}">{worst_check.uc:.3f}</div>
            <div class="stat-label">{_escape(worst_check.name)}</div>
        </div>
        <div class="stat-card">
            <div class="stat-label">Governing Load Case</div>
            <div class="stat-value" style="font-size: 1.2rem;">{_escape(worst_check.load_case)}</div>
        </div>
    </div>

    <h3 id="exec-uc-chart">UC Summary Chart</h3>
    {chart_html}
    
    <div id="exec-verdict">
        <h3>Verdict & Recommendations</h3>
        <p>The analysis indicates that the structure is <strong>{compliance_text}</strong> with the stated design codes.</p>
        <ul>
            {recs_list}
        </ul>
    </div>
    """
    
    return wrap_section("executive-summary", "2. Executive Summary", body_html)
