"""
Base renderer strategy for OrcaFlex reports.
"""
from typing import List, Dict, Any, Union
from ..models import OrcaFlexAnalysisReport
from ..section_builders import (
    _build_header_html,
    _build_executive_summary_html,
    _build_model_overview_html,
    _build_geometry_html,
    _build_materials_html,
    _build_boundary_conditions_html,
    _build_mesh_html,
    _build_other_structures_html,
    _build_loads_html,
    _build_analysis_setup_html,
    _build_static_results_html,
    _build_dynamic_results_html,
    _build_extreme_results_html,
    _build_design_checks_html,
    _build_fatigue_html,
    _build_summary_html,
    _build_appendices_html
)


class BaseRenderer:
    """Base class for report rendering strategies."""

    def __init__(self, include_plotlyjs: Union[str, bool] = "cdn"):
        self.include_plotlyjs = include_plotlyjs

    def get_section_config(self) -> List[Dict[str, Any]]:
        """
        Returns the canonical section configuration.
        Each dict contains:
        - id: anchor ID
        - title: full section title (e.g. "4. Geometry")
        - builder: the function to call
        - mandatory: bool
        """
        return [
            {"id": "executive-summary", "title": "2. Executive Summary", "builder": _build_executive_summary_html, "mandatory": True},
            {"id": "model-overview", "title": "3. Model Overview", "builder": _build_model_overview_html, "mandatory": True},
            {"id": "geometry", "title": "4. Geometry", "builder": _build_geometry_html, "mandatory": True},
            {"id": "materials", "title": "5. Materials", "builder": _build_materials_html, "mandatory": True},
            {"id": "boundary-conditions", "title": "6. Boundary Conditions", "builder": _build_boundary_conditions_html, "mandatory": True},
            {"id": "mesh", "title": "7. Mesh", "builder": _build_mesh_html, "mandatory": True},
            {"id": "other-structures", "title": "8. Other Structures", "builder": _build_other_structures_html, "mandatory": False},
            {"id": "loads", "title": "9. Loads & Environment", "builder": _build_loads_html, "mandatory": True},
            {"id": "analysis-setup", "title": "10. Analysis Setup", "builder": _build_analysis_setup_html, "mandatory": True},
            {"id": "static-results", "title": "11. Static Results", "builder": _build_static_results_html, "mandatory": True},
            {"id": "dynamic-results", "title": "12. Dynamic Results", "builder": _build_dynamic_results_html, "mandatory": False},
            {"id": "extreme-results", "title": "13. Extreme Results", "builder": _build_extreme_results_html, "mandatory": False},
            {"id": "design-checks", "title": "14. Design Checks", "builder": _build_design_checks_html, "mandatory": True},
            {"id": "fatigue", "title": "15. Fatigue", "builder": _build_fatigue_html, "mandatory": False},
            {"id": "summary", "title": "16. Summary & Recommendations", "builder": _build_summary_html, "mandatory": True},
            {"id": "appendices", "title": "17. Appendices", "builder": _build_appendices_html, "mandatory": True}
        ]

    def render(self, report: OrcaFlexAnalysisReport) -> str:
        """Renders the full report to HTML."""
        header_html = _build_header_html(report)
        
        sections_html = []
        for config in self.get_section_config():
            builder = config["builder"]
            is_mandatory = config["mandatory"]
            anchor = config["id"]
            title = config["title"]
            
            try:
                # Check if we should call with include_plotlyjs
                import inspect
                sig = inspect.signature(builder)
                if "include_plotlyjs" in sig.parameters:
                    content = builder(report, self.include_plotlyjs)
                else:
                    content = builder(report)
                
                if content:
                    sections_html.append(content)
                elif is_mandatory:
                    # Mandatory-section placeholder ownership
                    from ..section_builders.utils import wrap_section
                    sections_html.append(wrap_section(anchor, title, "", empty=True))
                # Conditional sections remain absent when builder returns ""
            except Exception as e:
                sections_html.append(f'<section id="{anchor}"><div class="section-card badge-fail">Error rendering section {anchor}: {e}</div></section>')
        
        return header_html + '<div class="container">' + "".join(sections_html) + '</div>'
