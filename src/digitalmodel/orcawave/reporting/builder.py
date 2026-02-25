"""OrcaWave HTML report builder — orchestrates all sections."""
from __future__ import annotations

import logging
from pathlib import Path

from .config import ReportConfig
from .sections import (
    model_summary,
    rao_plots,
    hydro_matrices,
    mean_drift,
    panel_pressures,
    multi_body,
    qtf_heatmap,
    qa_summary,
)

logger = logging.getLogger(__name__)

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
  <style>
    body {{ font-family: Arial, sans-serif; padding: 20px; }}
    h1 {{ margin-bottom: 30px; }}
    h2 {{ margin-top: 40px; border-bottom: 2px solid #dee2e6; padding-bottom: 8px; }}
    .section-placeholder {{ color: #6c757d; font-style: italic; padding: 12px; }}
    .tab-content {{ padding-top: 16px; }}
  </style>
</head>
<body>
  <div class="container-fluid">
    <h1>{title}</h1>
    {sections}
  </div>
  <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"
    integrity="sha384-C6RzsynM9kWDrMNeT87bh95OGNyZPhcTNXj1NW7RuBCsyN/o0jlpcV8Qyq46cDfL"
    crossorigin="anonymous"></script>
</body>
</html>
"""


class OrcaWaveReportBuilder:
    """Builds a self-contained HTML report from an OrcaWave .owr result file."""

    def __init__(self, owr_path: str | Path, config: ReportConfig) -> None:
        self.owr_path = Path(owr_path)
        self.config = config

    def _load_diffraction(self):
        """Load OrcFxAPI.Diffraction from the .owr file."""
        try:
            import OrcFxAPI  # type: ignore
        except ImportError as exc:
            raise ImportError(
                "OrcFxAPI is required to generate OrcaWave reports."
            ) from exc
        diff = OrcFxAPI.Diffraction(str(self.owr_path))
        return diff

    def build(self) -> str:
        """Build and return the full self-contained HTML report string."""
        diff = self._load_diffraction()
        cfg = self.config
        section_html_parts: list[str] = []

        _build_section(
            section_html_parts,
            "1. Model Summary",
            cfg.model_summary.enabled,
            lambda: model_summary.build_model_summary(diff, cfg.model_summary),
        )
        _build_section(
            section_html_parts,
            "2. RAO Plots",
            cfg.rao_plots.enabled,
            lambda: rao_plots.build_rao_plots(diff, cfg.rao_plots, cfg.include_plotlyjs),
        )
        _build_section(
            section_html_parts,
            "3. Hydrodynamic Matrices",
            cfg.hydro_matrices.enabled,
            lambda: hydro_matrices.build_hydro_matrices(diff, cfg.hydro_matrices),
        )
        _build_section(
            section_html_parts,
            "4. Mean Drift",
            cfg.mean_drift.enabled,
            lambda: mean_drift.build_mean_drift(diff, cfg.mean_drift),
        )
        _build_section(
            section_html_parts,
            "5. Panel Pressures",
            cfg.panel_pressures.enabled,
            lambda: panel_pressures.build_panel_pressures(diff, cfg.panel_pressures),
        )
        _build_section(
            section_html_parts,
            "6. Multi-Body Coupling",
            cfg.multi_body.enabled,
            lambda: multi_body.build_multi_body(diff, cfg.multi_body),
        )
        _build_section(
            section_html_parts,
            "7. QTF Heatmap",
            cfg.qtf_heatmap.enabled,
            lambda: qtf_heatmap.build_qtf_heatmap(diff, cfg.qtf_heatmap),
        )
        _build_section(
            section_html_parts,
            "8. QA Summary",
            cfg.qa_summary.enabled,
            lambda: qa_summary.build_qa_summary(diff, cfg.qa_summary),
        )

        return _HTML_TEMPLATE.format(
            title=cfg.title,
            sections="\n".join(section_html_parts),
        )


def _build_section(
    parts: list[str],
    heading: str,
    enabled: bool,
    build_fn,
) -> None:
    """Attempt to build one section; append result or skip/error placeholder."""
    if not enabled:
        return
    try:
        content = build_fn()
        parts.append(f'<h2>{heading}</h2>\n{content}')
    except Exception:
        logger.exception("Failed to build section '%s'", heading)
        parts.append(
            f'<h2>{heading}</h2>\n'
            f'<p class="section-placeholder text-danger">'
            f'Section failed to render — see logs for details.</p>'
        )
