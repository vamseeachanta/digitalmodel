"""Tests for orcaflex.reporting.config — ReportConfig and section configs.

ABOUTME: Tests all config dataclasses/classes including defaults,
from_yaml loading, and section enable/disable logic.
"""
from __future__ import annotations

import textwrap
from pathlib import Path

import pytest

from digitalmodel.orcaflex.reporting.config import (
    CodeCheckConfig,
    ModalAnalysisConfig,
    ModelSummaryConfig,
    MooringLoadsConfig,
    QASummaryConfig,
    RangeGraphsConfig,
    ReportConfig,
    SectionConfig,
    StaticConfigSectionConfig,
    TimeSeriesConfig,
)


class TestSectionConfig:
    """Test base SectionConfig and subclasses."""

    def test_default_enabled(self):
        """SectionConfig defaults to enabled=True."""
        cfg = SectionConfig()
        assert cfg.enabled is True

    def test_explicit_disabled(self):
        """SectionConfig can be created disabled."""
        cfg = SectionConfig(enabled=False)
        assert cfg.enabled is False

    def test_model_summary_inherits(self):
        """ModelSummaryConfig inherits from SectionConfig."""
        cfg = ModelSummaryConfig()
        assert isinstance(cfg, SectionConfig)
        assert cfg.enabled is True

    def test_static_config_inherits(self):
        """StaticConfigSectionConfig is a SectionConfig."""
        cfg = StaticConfigSectionConfig()
        assert cfg.enabled is True

    def test_code_check_inherits(self):
        """CodeCheckConfig inherits and can be disabled."""
        cfg = CodeCheckConfig(enabled=False)
        assert cfg.enabled is False


class TestTimeSeriesConfig:
    """Test TimeSeriesConfig with max_variables."""

    def test_default_max_variables(self):
        """Default max_variables is 10."""
        cfg = TimeSeriesConfig()
        assert cfg.max_variables == 10
        assert cfg.enabled is True

    def test_custom_max_variables(self):
        """max_variables can be set."""
        cfg = TimeSeriesConfig()
        cfg.max_variables = 5
        assert cfg.max_variables == 5


class TestRangeGraphsConfig:
    """Test RangeGraphsConfig with percentiles."""

    def test_default_percentiles(self):
        """Default percentiles are [5, 50, 95]."""
        cfg = RangeGraphsConfig()
        assert cfg.percentiles == [5, 50, 95]

    def test_custom_percentiles(self):
        """Percentiles can be overridden."""
        cfg = RangeGraphsConfig()
        cfg.percentiles = [10, 90]
        assert cfg.percentiles == [10, 90]

    def test_enabled_by_default(self):
        """Range graphs enabled by default."""
        cfg = RangeGraphsConfig()
        assert cfg.enabled is True


class TestReportConfig:
    """Test top-level ReportConfig."""

    def test_default_title(self):
        """Default title is 'OrcaFlex Analysis Report'."""
        cfg = ReportConfig()
        assert cfg.title == "OrcaFlex Analysis Report"

    def test_custom_title(self):
        """Title can be set at construction."""
        cfg = ReportConfig(title="Custom Report")
        assert cfg.title == "Custom Report"

    def test_default_plotlyjs(self):
        """Default include_plotlyjs is 'cdn'."""
        cfg = ReportConfig()
        assert cfg.include_plotlyjs == "cdn"

    def test_default_section_states(self):
        """Check default enabled/disabled for all sections."""
        cfg = ReportConfig()
        assert cfg.model_summary.enabled is True
        assert cfg.static_config.enabled is True
        assert cfg.time_series.enabled is True
        assert cfg.range_graphs.enabled is True
        assert cfg.code_check.enabled is False  # off by default
        assert cfg.mooring_loads.enabled is False  # off by default
        assert cfg.modal_analysis.enabled is False  # off by default
        assert cfg.qa_summary.enabled is True

    def test_disable_section(self):
        """Sections can be disabled by setting .enabled = False."""
        cfg = ReportConfig()
        cfg.model_summary.enabled = False
        assert cfg.model_summary.enabled is False

    def test_enable_code_check(self):
        """Code check can be enabled after construction."""
        cfg = ReportConfig()
        cfg.code_check.enabled = True
        assert cfg.code_check.enabled is True


class TestReportConfigFromYAML:
    """Test ReportConfig.from_yaml loading."""

    def test_loads_title(self, tmp_path: Path):
        """Title is read from YAML."""
        yml = tmp_path / "config.yaml"
        yml.write_text(textwrap.dedent("""\
            title: "My Custom Report"
            sections:
              model_summary: true
        """))
        cfg = ReportConfig.from_yaml(yml)
        assert cfg.title == "My Custom Report"

    def test_sections_from_yaml(self, tmp_path: Path):
        """Section enable/disable states are read from YAML."""
        yml = tmp_path / "config.yaml"
        yml.write_text(textwrap.dedent("""\
            title: "Test"
            sections:
              model_summary: true
              static_config: false
              time_series: true
              range_graphs: false
              code_check: true
              mooring_loads: true
              modal_analysis: false
              qa_summary: false
        """))
        cfg = ReportConfig.from_yaml(yml)
        assert cfg.model_summary.enabled is True
        assert cfg.static_config.enabled is False
        assert cfg.code_check.enabled is True
        assert cfg.qa_summary.enabled is False

    def test_missing_sections_key_uses_defaults(self, tmp_path: Path):
        """Missing 'sections' key falls back to defaults."""
        yml = tmp_path / "config.yaml"
        yml.write_text("title: Minimal\n")
        cfg = ReportConfig.from_yaml(yml)
        assert cfg.title == "Minimal"
        assert cfg.model_summary.enabled is True

    def test_partial_sections_preserves_defaults(self, tmp_path: Path):
        """Only specified sections are changed; others keep defaults."""
        yml = tmp_path / "config.yaml"
        yml.write_text(textwrap.dedent("""\
            sections:
              code_check: true
        """))
        cfg = ReportConfig.from_yaml(yml)
        assert cfg.code_check.enabled is True
        assert cfg.mooring_loads.enabled is False  # unchanged default

    def test_requires_pyyaml(self, tmp_path: Path, monkeypatch):
        """ImportError if pyyaml is not available."""
        yml = tmp_path / "config.yaml"
        yml.write_text("title: Test\n")
        monkeypatch.setitem(__import__("sys").modules, "yaml", None)
        with pytest.raises((ImportError, TypeError)):
            ReportConfig.from_yaml(yml)
