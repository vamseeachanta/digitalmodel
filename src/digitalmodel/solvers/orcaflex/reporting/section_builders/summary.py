"""
Summary and Recommendations section builder.
"""
from .utils import wrap_section, _escape
from ..models import OrcaFlexAnalysisReport


def _build_summary_html(report: OrcaFlexAnalysisReport) -> str:
    """Builds the final summary and recommendations section."""
    escaped_notes = _escape(report.summary_notes or "The analysis was completed successfully. See individual sections for detailed results.")
    
    recommendations_html = ""
    if report.recommendations:
        recommendations_html = "".join([f"<li>{_escape(r)}</li>" for r in report.recommendations])
    else:
        recommendations_html = "<li>No specific recommendations.</li>"

    body_html = f"""
    <div id="summary-notes">
        <h3>Analysis Findings</h3>
        <p>{escaped_notes}</p>
    </div>
    
    <div id="summary-recommendations">
        <h3>Recommendations</h3>
        <ul>
            {recommendations_html}
        </ul>
    </div>
    """
    
    return wrap_section("summary", "16. Summary & Recommendations", body_html)
