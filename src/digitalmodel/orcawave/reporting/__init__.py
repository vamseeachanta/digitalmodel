"""OrcaWave HTML report generation."""
from __future__ import annotations

from pathlib import Path

from .config import ReportConfig
from .builder import OrcaWaveReportBuilder

__all__ = ["generate_orcawave_report", "ReportConfig", "OrcaWaveReportBuilder"]


def generate_orcawave_report(
    owr_path: str | Path,
    output_html: str | Path,
    config: ReportConfig | None = None,
    config_yml: str | Path | None = None,
) -> Path:
    """Generate a self-contained HTML report from an OrcaWave .owr result file.

    Args:
        owr_path: Path to the .owr result file.
        output_html: Path for the output HTML file.
        config: ReportConfig instance (mutually exclusive with config_yml).
        config_yml: Path to a YAML config file (mutually exclusive with config).

    Returns:
        Path to the generated HTML file.
    """
    if config is not None and config_yml is not None:
        raise ValueError("Provide either config or config_yml, not both.")

    if config is None and config_yml is None:
        config = ReportConfig()
    elif config_yml is not None:
        config = ReportConfig.from_yaml(str(config_yml))

    builder = OrcaWaveReportBuilder(owr_path, config)
    html = builder.build()

    output_path = Path(output_html)
    output_path.write_text(html, encoding="utf-8")
    return output_path
