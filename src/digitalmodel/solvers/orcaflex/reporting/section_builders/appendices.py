"""
Appendices section builder.
"""
from .utils import wrap_section
from ..models import OrcaFlexAnalysisReport


def _build_appendices_html(report: OrcaFlexAnalysisReport) -> str:
    """Builds the appendices section."""
    body_html = """
    <div id="appendix-a">
        <h3>Appendix A: Notation and Units</h3>
        <table>
            <thead><tr><th>Symbol</th><th>Description</th><th>Unit</th></tr></thead>
            <tbody>
                <tr><td>Te</td><td>Effective Tension</td><td>kN</td></tr>
                <tr><td>BM</td><td>Resultant Bending Moment</td><td>kN·m</td></tr>
                <tr><td>UC</td><td>Utilization Ratio</td><td>-</td></tr>
                <tr><td>KP</td><td>Kilometer Point</td><td>m</td></tr>
                <tr><td>TDP</td><td>Touchdown Point</td><td>-</td></tr>
            </tbody>
        </table>
    </div>
    """
    
    return wrap_section("appendices", "17. Appendices", body_html)
