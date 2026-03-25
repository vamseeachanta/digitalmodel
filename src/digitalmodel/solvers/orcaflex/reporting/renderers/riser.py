"""
Riser-specific report renderer.
"""
from typing import List, Dict, Any
import plotly.graph_objects as go
from .base import BaseRenderer
from ..models import OrcaFlexAnalysisReport


class RiserRenderer(BaseRenderer):
    """Renderer strategy for SCRs and Flexible Risers."""
    
    def render(self, report: OrcaFlexAnalysisReport) -> str:
        """Renders the riser report, injecting riser-specific subsections."""
        # We'll use the base render but we need to inject content into specific sections
        # To do this cleanly without rewriting the whole render, 
        # we can wrap the builders or modify the config.
        return super().render(report)

    def get_section_config(self) -> List[Dict[str, Any]]:
        config = super().get_section_config()
        
        # We need to find the dynamic results and design checks builders and wrap them
        for item in config:
            if item["id"] == "dynamic-results":
                original_builder = item["builder"]
                item["builder"] = lambda r, include_plotlyjs, b=original_builder: self._build_riser_dynamic_results(r, include_plotlyjs, b)
            elif item["id"] == "design-checks":
                original_builder = item["builder"]
                item["builder"] = lambda r, include_plotlyjs, b=original_builder: self._build_riser_design_checks(r, include_plotlyjs, b)
        
        return config

    def _build_riser_dynamic_results(self, report: OrcaFlexAnalysisReport, include_plotlyjs: str, base_builder) -> str:
        base_html = base_builder(report, include_plotlyjs)
        if not base_html or not report.dynamic_results or not report.dynamic_results.tdp_excursion_history:
            return base_html
            
        res = report.dynamic_results
        ts = res.tdp_excursion_history
        
        fig = go.Figure(go.Scatter(x=ts.t, y=ts.values, mode='lines', line=dict(color='#e67e22')))
        fig.add_vrect(x0=min(ts.t), x1=res.ramp_end_time_s, fillcolor="gray", opacity=0.1, line_width=0)
        
        fig.update_layout(
            title="TDP X-Excursion Time History",
            xaxis_title="Time [s]",
            yaxis_title=f"X-Position [{ts.units}]",
            height=400,
            template="plotly_white"
        )
        
        tdp_html = f"""
        <h3 id="dynamic-tdp-excursion">12e. TDP Excursion Time History</h3>
        {fig.to_html(full_html=False, include_plotlyjs=include_plotlyjs, div_id="dynamic-tdp-excursion-chart")}
        """
        
        # Inject before the stats table
        if '<h3 id="dynamic-stats-table">' in base_html:
            return base_html.replace('<h3 id="dynamic-stats-table">', tdp_html + '<h3 id="dynamic-stats-table">')
        else:
            return base_html.replace('</div>\n    \n    </section>', tdp_html + '</div>\n    \n    </section>')

    def _build_riser_design_checks(self, report: OrcaFlexAnalysisReport, include_plotlyjs: str, base_builder) -> str:
        base_html = base_builder(report, include_plotlyjs)
        # Placeholder for VIV susceptibility check if we have data for it
        # For now, just return base
        return base_html
