from typing import List, Dict, Any
from .base import BaseRenderer
from ..models import OrcaFlexAnalysisReport


class JumperRenderer(BaseRenderer):
    """Renderer strategy for Rigid Jumpers."""

    def render(self, report: OrcaFlexAnalysisReport) -> str:
        if report.jumper_installation is None:
            return super().render(report)
        from importlib.resources import files
        from jinja2 import Environment, StrictUndefined
        import plotly.graph_objects as go
        from ..section_builders.header import _build_header_html

        payload = report.jumper_installation
        weights, sections = payload["weight_items"], payload["orcaflex_sections"]
        weight_chart = go.Figure(go.Pie(labels=list(weights), values=list(weights.values())))
        weight_chart.update_layout(title="Dry lift weight [kg]", paper_bgcolor="#f7fafc")
        length_chart = go.Figure(go.Bar(x=[s["name"] for s in sections],
                                        y=[s["length_m"] for s in sections], marker_color="#2c5282"))
        length_chart.update_layout(xaxis_title="Section", yaxis_title="Length [m]")
        charts = {name: chart.to_html(full_html=False, include_plotlyjs=False, div_id=name)
                  for name, chart in (("jumper-weight", weight_chart), ("jumper-lengths", length_chart))}
        template = files("digitalmodel").joinpath(
            "solvers/orcaflex/reporting/templates/jumper_installation.html.j2"
        ).read_text(encoding="utf-8")
        body = Environment(autoescape=True, undefined=StrictUndefined).from_string(template).render(
            report=report, p=payload, charts=charts,
        )
        return _build_header_html(report) + body

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
