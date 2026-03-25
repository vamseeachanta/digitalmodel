"""
Report header and navigation builder.
"""
from ..models import OrcaFlexAnalysisReport
from .utils import _escape


def _build_header_html(report: OrcaFlexAnalysisReport) -> str:
    """Builds the report header and sticky navigation bar."""
    status = report.overall_pass
    if status is True:
        pass_badge, pass_text = "badge-pass", "ALL PASS"
    elif status is False:
        pass_badge, pass_text = "badge-fail", "FAIL"
    else:
        pass_badge, pass_text = "badge-warning", "NO CHECKS"
    
    escaped_project = _escape(report.project_name)
    escaped_id = _escape(report.structure_id)
    escaped_ref = _escape(report.analysis_ref)
    escaped_analyst = _escape(report.analyst)
    escaped_version = _escape(report.orcaflex_version)
    escaped_type = _escape(report.structure_type.upper())
    date_str = report.date.strftime("%Y-%m-%d")
    
    return f"""
<header id="header">
  <div class="header-content">
    <div class="header-main">
      <div class="header-title">
        {escaped_project} - {escaped_id}
        <span class="badge badge-info">{escaped_type}</span>
        <span class="badge {pass_badge}">{pass_text}</span>
      </div>
      <div class="header-meta">
        Ref: {escaped_ref} | Date: {date_str} | Analyst: {escaped_analyst} | Software: OrcaFlex {escaped_version}
      </div>
    </div>
    <nav class="nav-links">
      <a href="#executive-summary">Summary</a>
      <a href="#model-overview">Overview</a>
      <a href="#geometry">Geometry</a>
      <a href="#materials">Materials</a>
      <a href="#boundary-conditions">BCs</a>
      <a href="#mesh">Mesh</a>
      <a href="#loads">Loads</a>
      <a href="#analysis-setup">Setup</a>
      <a href="#static-results">Static</a>
      <a href="#dynamic-results">Dynamic</a>
      <a href="#extreme-results">Extreme</a>
      <a href="#design-checks">Checks</a>
      <a href="#fatigue">Fatigue</a>
      <a href="#appendices">Appendices</a>
    </nav>
  </div>
</header>
"""
