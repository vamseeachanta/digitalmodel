"""
Model Overview section builder.
"""
from .utils import wrap_section, build_table, _escape
from ..models import OrcaFlexAnalysisReport


def _build_model_overview_html(report: OrcaFlexAnalysisReport) -> str:
    """Builds the Model Overview section with structure purpose and design codes."""
    # This section is mandatory
    rows = [
        ["Structure ID", _escape(report.structure_id)],
        ["Structure Type", _escape(report.structure_type.upper())],
        ["Analysis Reference", _escape(report.analysis_ref)],
        ["Analysis Date", report.date.strftime("%Y-%m-%d")],
        ["Analyst", _escape(report.analyst)],
        ["Design Codes", _escape(", ".join(report.design_codes)) if report.design_codes else "N/A"],
        ["OrcaFlex Version", _escape(report.orcaflex_version)]
    ]
    
    table_html = build_table(["Field", "Value"], rows)
    
    body_html = f"""
    <div id="model-overview-metadata">
        {table_html}
    </div>
    
    <div id="model-overview-description" style="margin-top: 1.5rem;">
        <h3>Structure Purpose</h3>
        <p>{_escape(report.summary_notes or "Standard analysis of " + report.structure_type + " structure.")}</p>
    </div>
    """
    
    return wrap_section("model-overview", "3. Model Overview", body_html)
