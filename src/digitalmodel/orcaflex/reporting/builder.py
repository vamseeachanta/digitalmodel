"""OrcaFlex HTML report builder."""
from __future__ import annotations

import os
import sys
from pathlib import Path

from .config import ReportConfig
from .sections import (
    build_model_summary,
    build_static_config,
    build_time_series,
    build_range_graphs,
    build_code_check,
    build_mooring_loads,
    build_modal_analysis,
    build_qa_summary,
)

_HTML_TEMPLATE = """\
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{title}</title>
  <link rel="stylesheet"
    href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css"
    integrity="sha384-T3c6CoIi6uLrA9TneNEoa7RxnatzjcDSCmG1MXxSR1GAsXEV/Dwwykc2MPK8M2HN"
    crossorigin="anonymous">
  <script src="{plotlyjs_src}" {plotlyjs_attrs}></script>
  <style>
    body {{ font-family: "Segoe UI", system-ui, sans-serif; background: #f8f9fa; }}
    .report-header {{ background: #0d6efd; color: white; padding: 2rem; margin-bottom: 2rem; }}
    .section-card {{ background: white; border-radius: 8px; padding: 1.5rem;
                    margin-bottom: 1.5rem; box-shadow: 0 1px 4px rgba(0,0,0,.08); }}
    .section-disabled {{ color: #adb5bd; font-style: italic; }}
    table.ofx {{ width: 100%; border-collapse: collapse; font-size: .9rem; }}
    table.ofx th {{ background: #e9ecef; padding: .4rem .7rem; text-align: left; }}
    table.ofx td {{ padding: .35rem .7rem; border-top: 1px solid #dee2e6; }}
    .pass {{ color: #198754; font-weight: 600; }}
    .fail {{ color: #dc3545; font-weight: 600; }}
    .skip {{ color: #6c757d; }}
  </style>
</head>
<body>
<div class="report-header container-fluid">
  <h1>{title}</h1>
  <p class="mb-0 opacity-75">OrcaFlex simulation report &mdash; {sim_name}</p>
</div>
<div class="container-fluid px-4">
{sections}
</div>
<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"
  integrity="sha384-C6RzsynM9kWDrMNeT87bh95OGNyZPhcTNXj1NW7RuBCsyN/o0jlpcV8Qyq46cDfL"
  crossorigin="anonymous"></script>
</body>
</html>
"""


def _load_orcfxapi() -> None:
    api_path = os.environ.get(
        "ORCAFLEX_API_PATH",
        r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcFxAPI\Python",
    )
    if api_path not in sys.path:
        sys.path.insert(0, api_path)


class OrcaFlexReportBuilder:
    """Build an HTML report from an OrcaFlex .sim file."""

    def __init__(self, sim_path: Path, config: ReportConfig):
        self.sim_path = Path(sim_path)
        self.config = config
        self._model = None

    def _load_simulation(self):
        if self._model is not None:
            return self._model
        _load_orcfxapi()
        try:
            import OrcFxAPI
        except ImportError as exc:
            raise ImportError(
                "OrcFxAPI not found. Set ORCAFLEX_API_PATH env var or install OrcaFlex."
            ) from exc
        model = OrcFxAPI.Model()
        model.LoadSimulation(str(self.sim_path))
        self._model = model
        return model

    @staticmethod
    def _section_card(heading: str, body: str) -> str:
        return (
            f'<div class="section-card">\n'
            f'  <h2 class="h4 mb-3">{heading}</h2>\n'
            f'  {body}\n'
            f'</div>\n'
        )

    @staticmethod
    def _disabled_card(heading: str) -> str:
        return (
            f'<div class="section-card section-disabled">\n'
            f'  <h2 class="h4 mb-1">{heading}</h2>\n'
            f'  <p class="mb-0">Section disabled in report config.</p>\n'
            f'</div>\n'
        )

    def build(self) -> str:
        """Build and return the complete HTML string."""
        model = self._load_simulation()
        cfg = self.config
        parts = []

        section_defs = [
            ("1. Model Summary",    cfg.model_summary,   lambda: build_model_summary(model, cfg.model_summary)),
            ("2. Static Config",    cfg.static_config,   lambda: build_static_config(model, cfg.static_config)),
            ("3. Time Series",      cfg.time_series,      lambda: build_time_series(model, cfg.time_series)),
            ("4. Range Graphs",     cfg.range_graphs,     lambda: build_range_graphs(model, cfg.range_graphs)),
            ("5. Code Check",       cfg.code_check,       lambda: build_code_check(model, cfg.code_check)),
            ("6. Mooring Loads",    cfg.mooring_loads,    lambda: build_mooring_loads(model, cfg.mooring_loads)),
            ("7. Modal Analysis",   cfg.modal_analysis,   lambda: build_modal_analysis(model, cfg.modal_analysis)),
            ("8. QA Summary",       cfg.qa_summary,       lambda: build_qa_summary(model, cfg.qa_summary)),
        ]

        for heading, section_cfg, build_fn in section_defs:
            if not section_cfg.enabled:
                parts.append(self._disabled_card(heading))
            else:
                try:
                    body = build_fn()
                    parts.append(self._section_card(heading, body))
                except Exception as exc:
                    error_body = f'<p class="text-danger">Error building section: {exc}</p>'
                    parts.append(self._section_card(heading, error_body))

        plotlyjs_src = "https://cdn.plot.ly/plotly-2.27.0.min.js"
        plotlyjs_attrs = ""

        html = _HTML_TEMPLATE.format(
            title=cfg.title,
            sim_name=self.sim_path.name,
            sections="\n".join(parts),
            plotlyjs_src=plotlyjs_src,
            plotlyjs_attrs=plotlyjs_attrs,
        )
        return html
