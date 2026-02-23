from typing import List, Dict, Any
import plotly.graph_objects as go
from .base import BaseRenderer
from ..models import OrcaFlexAnalysisReport
from ..section_builders.utils import build_table, _escape


class MooringRenderer(BaseRenderer):
    """Renderer strategy for Mooring Systems."""

    def get_section_config(self) -> List[Dict[str, Any]]:
        config = super().get_section_config()
        for item in config:
            if item["id"] == "geometry":
                original_builder = item["builder"]
                item["builder"] = lambda r, include_plotlyjs, b=original_builder: self._build_mooring_geometry(r, include_plotlyjs, b)
            elif item["id"] == "static-results":
                original_builder = item["builder"]
                item["builder"] = lambda r, include_plotlyjs, b=original_builder: self._build_mooring_static_results(r, include_plotlyjs, b)
        return config

    def _build_mooring_geometry(self, report: OrcaFlexAnalysisReport, include_plotlyjs: str, base_builder) -> str:
        base_html = base_builder(report, include_plotlyjs)
        # Injection for #geometry-spider - Placeholder for actual spider plot logic
        spider_html = """
        <h3 id="geometry-spider">Mooring Spread Diagram (Spider Plot)</h3>
        <div class="plotly-chart" id="mooring-spider-chart">
            <p class="no-data">Mooring layout visualization placeholder.</p>
        </div>
        """
        if '<h3 id="geometry-key-points">' in base_html:
            return base_html.replace('<h3 id="geometry-key-points">', spider_html + '<h3 id="geometry-key-points">')
        return base_html

    def _build_mooring_static_results(self, report: OrcaFlexAnalysisReport, include_plotlyjs: str, base_builder) -> str:
        base_html = base_builder(report, include_plotlyjs)
        if not base_html or not report.static_results or not report.static_results.per_line_tensions:
            return base_html
            
        tensions = report.static_results.per_line_tensions
        headers = ["Line ID", "Pretension [kN]"]
        rows = [[_escape(str(k)), f"{v:.1f}"] for k, v in tensions.items()]
        
        per_line_html = f"""
        <h3 id="static-per-line-tensions">Per-Line Tension Table</h3>
        {build_table(headers, rows)}
        """
        
        if '<div id="static-summary">' in base_html:
            return base_html.replace('<div id="static-summary">', per_line_html + '<div id="static-summary">')
        return base_html
