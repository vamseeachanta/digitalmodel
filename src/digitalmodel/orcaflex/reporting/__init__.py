"""OrcaFlex HTML report generation.

Entry point::

    from digitalmodel.orcaflex.reporting import generate_orcaflex_report

    generate_orcaflex_report(
        sim_path="path/to/model.sim",
        output_html="path/to/report.html",
    )
"""
from __future__ import annotations

from pathlib import Path

from .config import ReportConfig
from .builder import OrcaFlexReportBuilder

__all__ = ["generate_orcaflex_report", "ReportConfig", "OrcaFlexReportBuilder"]


def generate_orcaflex_report(
    sim_path: str | Path,
    output_html: str | Path,
    config: ReportConfig | None = None,
    config_yml: str | Path | None = None,
) -> Path:
    """Generate a self-contained HTML report from an OrcaFlex .sim file.

    Args:
        sim_path: Path to the OrcaFlex simulation file (.sim).
        output_html: Destination path for the HTML report.
        config: Pre-built ReportConfig instance (mutually exclusive with config_yml).
        config_yml: Path to a YAML config file (mutually exclusive with config).

    Returns:
        Path to the written HTML file.
    """
    if config is not None and config_yml is not None:
        raise ValueError("Provide either config or config_yml, not both")

    if config_yml is not None:
        cfg = ReportConfig.from_yaml(config_yml)
    elif config is not None:
        cfg = config
    else:
        cfg = ReportConfig()

    builder = OrcaFlexReportBuilder(sim_path=Path(sim_path), config=cfg)
    html = builder.build()

    out = Path(output_html)
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(html, encoding="utf-8")
    return out
