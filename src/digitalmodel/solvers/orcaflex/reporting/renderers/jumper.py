from typing import List, Dict, Any
from .base import BaseRenderer
from ..models import OrcaFlexAnalysisReport


class JumperRenderer(BaseRenderer):
    """Renderer strategy for Rigid Jumpers."""

    def get_section_config(self) -> List[Dict[str, Any]]:
        config = super().get_section_config()
        for item in config:
            if item["id"] == "dynamic-results":
                original_builder = item["builder"]
                item["builder"] = lambda r, include_plotlyjs, b=original_builder: self._build_jumper_dynamic_results(r, include_plotlyjs, b)
        return config

    def _build_jumper_dynamic_results(self, report: OrcaFlexAnalysisReport, include_plotlyjs: str, base_builder) -> str:
        base_html = base_builder(report, include_plotlyjs)
        # Injection for #dynamic-end-rotations
        return base_html
