"""Report configuration models for OrcaFlex HTML reports."""
from __future__ import annotations

from pathlib import Path

try:
    from pydantic import BaseModel, Field
    _PYDANTIC = True
except ImportError:
    from dataclasses import dataclass as _dc, field as _f
    BaseModel = object
    _PYDANTIC = False


def _section(enabled: bool = True):
    return Field(default_factory=lambda: {"enabled": enabled}) if _PYDANTIC else {}


class SectionConfig:
    """Base section configuration."""
    def __init__(self, enabled: bool = True):
        self.enabled = enabled


class ModelSummaryConfig(SectionConfig):
    """Model summary: object counts, environment, analysis type."""
    pass


class StaticConfigSectionConfig(SectionConfig):
    """Static shape plots: line profiles, plan view."""
    pass


class TimeSeriesConfig(SectionConfig):
    """Dynamic time series: vessel motion, line end tensions."""
    max_variables: int = 10


class RangeGraphsConfig(SectionConfig):
    """Range graph heatmaps: max/min across arclength."""
    percentiles: list = None

    def __init__(self, enabled: bool = True):
        super().__init__(enabled)
        self.percentiles = [5, 50, 95]


class CodeCheckConfig(SectionConfig):
    """Code check utilization table (requires code check in .sim)."""
    pass


class MooringLoadsConfig(SectionConfig):
    """Fairlead tensions and watch circle (moored systems)."""
    pass


class ModalAnalysisConfig(SectionConfig):
    """Natural frequencies and mode shapes (if modal data present)."""
    pass


class QASummaryConfig(SectionConfig):
    """QA pass/fail summary from orcaflex_example_qa checks."""
    pass


class ReportConfig:
    """Top-level report configuration.

    All sections default to enabled. Disable individual sections by
    setting their ``enabled`` attribute to False::

        cfg = ReportConfig()
        cfg.code_check.enabled = False
        cfg.modal_analysis.enabled = False
    """
    def __init__(
        self,
        title: str = "OrcaFlex Analysis Report",
        include_plotlyjs: str = "cdn",
    ):
        self.title = title
        self.include_plotlyjs = include_plotlyjs
        self.model_summary = ModelSummaryConfig()
        self.static_config = StaticConfigSectionConfig()
        self.time_series = TimeSeriesConfig()
        self.range_graphs = RangeGraphsConfig()
        self.code_check = CodeCheckConfig(enabled=False)  # off by default — needs code check data
        self.mooring_loads = MooringLoadsConfig(enabled=False)  # off by default — moored systems only
        self.modal_analysis = ModalAnalysisConfig(enabled=False)  # off by default — needs modal data
        self.qa_summary = QASummaryConfig()

    @classmethod
    def from_yaml(cls, path: str | Path) -> "ReportConfig":
        """Load configuration from a YAML file.

        Expected YAML structure::

            title: "My OrcaFlex Report"
            sections:
              model_summary: true
              static_config: true
              time_series: true
              range_graphs: true
              code_check: false
              mooring_loads: false
              modal_analysis: false
              qa_summary: true
        """
        try:
            import yaml
        except ImportError as exc:
            raise ImportError("pyyaml is required to load config from YAML") from exc

        with open(path) as f:
            data = yaml.safe_load(f)

        cfg = cls(title=data.get("title", "OrcaFlex Analysis Report"))
        sections = data.get("sections", {})
        for attr, key in [
            ("model_summary", "model_summary"),
            ("static_config", "static_config"),
            ("time_series", "time_series"),
            ("range_graphs", "range_graphs"),
            ("code_check", "code_check"),
            ("mooring_loads", "mooring_loads"),
            ("modal_analysis", "modal_analysis"),
            ("qa_summary", "qa_summary"),
        ]:
            if key in sections:
                getattr(cfg, attr).enabled = bool(sections[key])
        return cfg
