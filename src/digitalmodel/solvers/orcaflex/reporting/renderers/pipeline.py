from typing import List, Dict, Any
from .base import BaseRenderer
from ..models import OrcaFlexAnalysisReport
from ..section_builders.utils import build_table


class PipelineRenderer(BaseRenderer):
    """Renderer strategy for Subsea Pipelines."""

    def get_section_config(self) -> List[Dict[str, Any]]:
        config = super().get_section_config()
        
        for item in config:
            if item["id"] == "geometry":
                original_builder = item["builder"]
                item["builder"] = lambda r, include_plotlyjs, b=original_builder: self._build_pipeline_geometry(r, include_plotlyjs, b)
            elif item["id"] == "design-checks":
                original_builder = item["builder"]
                item["builder"] = lambda r, include_plotlyjs, b=original_builder: self._build_pipeline_design_checks(r, include_plotlyjs, b)
        
        return config

    def _build_pipeline_geometry(self, report: OrcaFlexAnalysisReport, include_plotlyjs: str, base_builder) -> str:
        base_html = base_builder(report, include_plotlyjs)
        if not base_html or not report.geometry or not report.geometry.kp_chainage_table:
            return base_html
            
        geom = report.geometry
        headers = ["KP [m]", "Easting [m]", "Northing [m]", "Water Depth [m]"]
        rows = [
            [f"{row.get('KP', 0):.1f}", f"{row.get('E', 0):.1f}", 
             f"{row.get('N', 0):.1f}", f"{row.get('WD', 0):.1f}"]
            for row in geom.kp_chainage_table
        ]
        kp_table_html = f"""
        <h3 id="geometry-kp-chainage">KP Chainage Table</h3>
        {build_table(headers, rows)}
        """
        
        # Inject after key points table
        if '<h3 id="geometry-key-points">' in base_html:
            return base_html.replace('<h3 id="geometry-key-points">', kp_table_html + '<h3 id="geometry-key-points">')
        else:
            return base_html.replace('</div>\n  </div>\n</section>', kp_table_html + '</div>\n  </div>\n</section>')

    def _build_pipeline_design_checks(self, report: OrcaFlexAnalysisReport, include_plotlyjs: str, base_builder) -> str:
        base_html = base_builder(report, include_plotlyjs)
        # Upheaval check logic would go here
        return base_html
