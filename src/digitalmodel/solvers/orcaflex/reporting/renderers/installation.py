from typing import List, Dict, Any
from .base import BaseRenderer
from ..models import OrcaFlexAnalysisReport


class InstallationRenderer(BaseRenderer):
    """Renderer strategy for Installation analyses."""

    def get_section_config(self) -> List[Dict[str, Any]]:
        config = super().get_section_config()
        for item in config:
            if item["id"] == "geometry":
                original_builder = item["builder"]
                item["builder"] = lambda r, include_plotlyjs, b=original_builder: self._build_installation_geometry(r, include_plotlyjs, b)
            elif item["id"] == "design-checks":
                original_builder = item["builder"]
                item["builder"] = lambda r, include_plotlyjs, b=original_builder: self._build_installation_design_checks(r, include_plotlyjs, b)
        return config

    def _build_installation_geometry(self, report: OrcaFlexAnalysisReport, include_plotlyjs: str, base_builder) -> str:
        base_html = base_builder(report, include_plotlyjs)
        # Injection for #geometry-stinger
        return base_html

    def _build_installation_design_checks(self, report: OrcaFlexAnalysisReport, include_plotlyjs: str, base_builder) -> str:
        base_html = base_builder(report, include_plotlyjs)
        # Injection for #dc-overbend
        return base_html
