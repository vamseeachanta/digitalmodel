"""Tests for OrcaWave reporting Pydantic config models.

Validates all config models with valid and invalid data, default values,
serialization round-trips, and edge cases.
"""
from __future__ import annotations

import pytest
from pydantic import ValidationError

from digitalmodel.orcawave.reporting.config import (
    HydroMatricesConfig,
    MeanDriftConfig,
    ModelSummaryConfig,
    MultiBodyConfig,
    PanelPressuresConfig,
    QASummaryConfig,
    QTFHeatmapConfig,
    RAOPlotsConfig,
    ReportConfig,
    SectionConfig,
)


# ---------------------------------------------------------------------------
# SectionConfig base
# ---------------------------------------------------------------------------


class TestSectionConfig:
    """Tests for the SectionConfig base model."""

    def test_default_enabled_true(self):
        cfg = SectionConfig()
        assert cfg.enabled is True

    def test_explicit_disabled(self):
        cfg = SectionConfig(enabled=False)
        assert cfg.enabled is False


# ---------------------------------------------------------------------------
# ModelSummaryConfig
# ---------------------------------------------------------------------------


class TestModelSummaryConfig:
    """Tests for ModelSummaryConfig."""

    def test_inherits_section_config(self):
        cfg = ModelSummaryConfig()
        assert isinstance(cfg, SectionConfig)
        assert cfg.enabled is True

    def test_disable(self):
        cfg = ModelSummaryConfig(enabled=False)
        assert cfg.enabled is False


# ---------------------------------------------------------------------------
# RAOPlotsConfig
# ---------------------------------------------------------------------------


class TestRAOPlotsConfig:
    """Tests for RAOPlotsConfig with DOF and heading options."""

    def test_defaults(self):
        cfg = RAOPlotsConfig()
        assert cfg.enabled is True
        assert cfg.headings is None  # None = all headings
        assert cfg.include_phase is False
        assert len(cfg.dofs) == 6
        assert "surge" in cfg.dofs

    def test_custom_headings(self):
        cfg = RAOPlotsConfig(headings=[0.0, 90.0, 180.0])
        assert cfg.headings == [0.0, 90.0, 180.0]

    def test_custom_dofs_subset(self):
        cfg = RAOPlotsConfig(dofs=["heave", "pitch"])
        assert cfg.dofs == ["heave", "pitch"]

    def test_include_phase_true(self):
        cfg = RAOPlotsConfig(include_phase=True)
        assert cfg.include_phase is True

    def test_empty_dofs_allowed(self):
        cfg = RAOPlotsConfig(dofs=[])
        assert cfg.dofs == []


# ---------------------------------------------------------------------------
# HydroMatricesConfig
# ---------------------------------------------------------------------------


class TestHydroMatricesConfig:
    """Tests for HydroMatricesConfig."""

    def test_defaults(self):
        cfg = HydroMatricesConfig()
        assert cfg.frequencies_to_show is None  # None = all

    def test_explicit_frequencies(self):
        cfg = HydroMatricesConfig(frequencies_to_show=[0.1, 0.5, 1.0])
        assert cfg.frequencies_to_show == [0.1, 0.5, 1.0]


# ---------------------------------------------------------------------------
# MeanDriftConfig
# ---------------------------------------------------------------------------


class TestMeanDriftConfig:
    """Tests for MeanDriftConfig."""

    def test_default_polar_included(self):
        cfg = MeanDriftConfig()
        assert cfg.include_polar is True

    def test_polar_disabled(self):
        cfg = MeanDriftConfig(include_polar=False)
        assert cfg.include_polar is False


# ---------------------------------------------------------------------------
# QTFHeatmapConfig
# ---------------------------------------------------------------------------


class TestQTFHeatmapConfig:
    """Tests for QTFHeatmapConfig."""

    def test_default_max_delta_omegas(self):
        cfg = QTFHeatmapConfig()
        assert cfg.max_delta_omegas == 3

    def test_custom_max_delta_omegas(self):
        cfg = QTFHeatmapConfig(max_delta_omegas=5)
        assert cfg.max_delta_omegas == 5


# ---------------------------------------------------------------------------
# Simple section configs (pass-through)
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "config_cls",
    [PanelPressuresConfig, MultiBodyConfig, QASummaryConfig],
    ids=["PanelPressures", "MultiBody", "QASummary"],
)
class TestSimpleSectionConfigs:
    """Tests for section configs that only inherit enabled."""

    def test_default_enabled(self, config_cls):
        cfg = config_cls()
        assert cfg.enabled is True

    def test_explicit_disabled(self, config_cls):
        cfg = config_cls(enabled=False)
        assert cfg.enabled is False


# ---------------------------------------------------------------------------
# ReportConfig (top-level)
# ---------------------------------------------------------------------------


class TestReportConfig:
    """Tests for the top-level ReportConfig orchestrator."""

    def test_default_title(self):
        cfg = ReportConfig()
        assert cfg.title == "OrcaWave Analysis Report"

    def test_custom_title(self):
        cfg = ReportConfig(title="My Custom Report")
        assert cfg.title == "My Custom Report"

    def test_default_plotlyjs(self):
        cfg = ReportConfig()
        assert cfg.include_plotlyjs == "cdn"

    def test_all_sections_present(self):
        cfg = ReportConfig()
        assert isinstance(cfg.model_summary, ModelSummaryConfig)
        assert isinstance(cfg.rao_plots, RAOPlotsConfig)
        assert isinstance(cfg.hydro_matrices, HydroMatricesConfig)
        assert isinstance(cfg.mean_drift, MeanDriftConfig)
        assert isinstance(cfg.panel_pressures, PanelPressuresConfig)
        assert isinstance(cfg.multi_body, MultiBodyConfig)
        assert isinstance(cfg.qtf_heatmap, QTFHeatmapConfig)
        assert isinstance(cfg.qa_summary, QASummaryConfig)

    def test_all_sections_enabled_by_default(self):
        cfg = ReportConfig()
        assert cfg.model_summary.enabled is True
        assert cfg.rao_plots.enabled is True
        assert cfg.hydro_matrices.enabled is True
        assert cfg.mean_drift.enabled is True
        assert cfg.panel_pressures.enabled is True
        assert cfg.multi_body.enabled is True
        assert cfg.qtf_heatmap.enabled is True
        assert cfg.qa_summary.enabled is True

    def test_nested_override(self):
        cfg = ReportConfig(rao_plots=RAOPlotsConfig(enabled=False, dofs=["heave"]))
        assert cfg.rao_plots.enabled is False
        assert cfg.rao_plots.dofs == ["heave"]

    def test_serialization_round_trip(self):
        original = ReportConfig(
            title="Test Report",
            rao_plots=RAOPlotsConfig(headings=[0.0, 180.0], include_phase=True),
        )
        data = original.model_dump()
        restored = ReportConfig(**data)
        assert restored.title == "Test Report"
        assert restored.rao_plots.headings == [0.0, 180.0]
        assert restored.rao_plots.include_phase is True

    def test_from_yaml_method_exists(self):
        """Verify ReportConfig.from_yaml is callable."""
        assert callable(getattr(ReportConfig, "from_yaml", None))

    def test_from_yaml_with_empty_data(self, tmp_path):
        """ReportConfig.from_yaml should handle an empty YAML file gracefully."""
        yml = tmp_path / "empty.yml"
        yml.write_text("")
        cfg = ReportConfig.from_yaml(str(yml))
        assert cfg.title == "OrcaWave Analysis Report"

    def test_from_yaml_with_overrides(self, tmp_path):
        """ReportConfig.from_yaml should pick up field overrides from YAML."""
        yml = tmp_path / "custom.yml"
        yml.write_text("title: 'YAML Title'\ninclude_plotlyjs: 'false'\n")
        cfg = ReportConfig.from_yaml(str(yml))
        assert cfg.title == "YAML Title"
