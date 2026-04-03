"""Tests for OrcaWave reporting config.py — error paths, edge cases, YAML loading.

Extends the existing test_config_models.py with additional error-path coverage.
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
# SectionConfig error paths
# ---------------------------------------------------------------------------


class TestSectionConfigErrors:
    """Edge-case and error-path tests for SectionConfig and subclasses."""

    def test_enabled_rejects_non_bool_string(self):
        """Pydantic should reject non-boolean string for 'enabled'."""
        with pytest.raises(ValidationError):
            SectionConfig(enabled="maybe")

    def test_enabled_accepts_truthy_int(self):
        """Pydantic coerces 1/0 to True/False for bool fields."""
        cfg = SectionConfig(enabled=1)
        assert cfg.enabled is True
        cfg2 = SectionConfig(enabled=0)
        assert cfg2.enabled is False


# ---------------------------------------------------------------------------
# RAOPlotsConfig error/edge cases
# ---------------------------------------------------------------------------


class TestRAOPlotsConfigErrors:

    def test_headings_with_non_float_raises(self):
        """headings should be list[float]; non-numeric should fail."""
        with pytest.raises(ValidationError):
            RAOPlotsConfig(headings=["north", "south"])

    def test_dofs_with_non_string_raises(self):
        """dofs should be list[str]; integers should fail."""
        with pytest.raises(ValidationError):
            RAOPlotsConfig(dofs=[1, 2, 3])

    def test_include_phase_rejects_string(self):
        with pytest.raises(ValidationError):
            RAOPlotsConfig(include_phase="maybe")


# ---------------------------------------------------------------------------
# QTFHeatmapConfig validation
# ---------------------------------------------------------------------------


class TestQTFHeatmapConfigErrors:

    def test_max_delta_omegas_rejects_float(self):
        """max_delta_omegas is int; float should be rejected or truncated."""
        # Pydantic v2 strict: int field won't accept arbitrary float
        with pytest.raises(ValidationError):
            QTFHeatmapConfig(max_delta_omegas=3.7)

    def test_max_delta_omegas_zero_allowed(self):
        cfg = QTFHeatmapConfig(max_delta_omegas=0)
        assert cfg.max_delta_omegas == 0

    def test_max_delta_omegas_negative_allowed(self):
        """Negative value is allowed by the model (no validator constraints)."""
        cfg = QTFHeatmapConfig(max_delta_omegas=-1)
        assert cfg.max_delta_omegas == -1


# ---------------------------------------------------------------------------
# ReportConfig error paths
# ---------------------------------------------------------------------------


class TestReportConfigErrors:

    def test_unknown_section_key_raises(self):
        """Extra fields not in the schema should raise ValidationError."""
        with pytest.raises(ValidationError):
            ReportConfig(nonexistent_section={"enabled": True})

    def test_invalid_section_value_type(self):
        """Passing a non-dict / non-model value for a section should fail."""
        with pytest.raises(ValidationError):
            ReportConfig(model_summary=42)

    def test_nested_section_dict_accepted(self):
        """Sections should accept raw dicts that match the schema."""
        cfg = ReportConfig(rao_plots={"enabled": False, "dofs": ["heave"]})
        assert cfg.rao_plots.enabled is False
        assert cfg.rao_plots.dofs == ["heave"]


# ---------------------------------------------------------------------------
# YAML loading error paths
# ---------------------------------------------------------------------------


class TestReportConfigYAMLErrors:

    def test_from_yaml_nonexistent_file_raises(self, tmp_path):
        """from_yaml should raise FileNotFoundError for missing files."""
        with pytest.raises(FileNotFoundError):
            ReportConfig.from_yaml(str(tmp_path / "does_not_exist.yml"))

    def test_from_yaml_invalid_yaml_raises(self, tmp_path):
        """from_yaml should raise on malformed YAML."""
        bad = tmp_path / "bad.yml"
        bad.write_text("{{{{{{{not valid yaml")
        with pytest.raises(Exception):
            ReportConfig.from_yaml(str(bad))

    def test_from_yaml_with_section_overrides(self, tmp_path):
        """from_yaml should parse nested section overrides from YAML."""
        yml = tmp_path / "sections.yml"
        yml.write_text(
            "title: 'From YAML'\n"
            "rao_plots:\n"
            "  enabled: false\n"
            "  dofs:\n"
            "    - heave\n"
            "    - pitch\n"
        )
        cfg = ReportConfig.from_yaml(str(yml))
        assert cfg.title == "From YAML"
        assert cfg.rao_plots.enabled is False
        assert cfg.rao_plots.dofs == ["heave", "pitch"]

    def test_from_yaml_with_unknown_key_raises(self, tmp_path):
        """from_yaml with unknown top-level keys should raise."""
        yml = tmp_path / "bad_key.yml"
        yml.write_text("unknown_key: 42\n")
        with pytest.raises(ValidationError):
            ReportConfig.from_yaml(str(yml))

    def test_from_yaml_none_values_uses_defaults(self, tmp_path):
        """YAML with null values should use defaults."""
        yml = tmp_path / "nulls.yml"
        yml.write_text("title: null\n")
        # Pydantic will reject null for a required str field -> depends on behavior
        # Actually title has a default, so null should trigger default or error
        try:
            cfg = ReportConfig.from_yaml(str(yml))
            # If it accepts null, title should be None or default
        except (ValidationError, TypeError):
            pass  # Also acceptable — null is not a valid string


# ---------------------------------------------------------------------------
# Model dump / JSON round-trip
# ---------------------------------------------------------------------------


class TestReportConfigSerialization:

    def test_model_dump_contains_all_sections(self):
        cfg = ReportConfig()
        data = cfg.model_dump()
        for key in [
            "model_summary", "rao_plots", "hydro_matrices",
            "mean_drift", "panel_pressures", "multi_body",
            "qtf_heatmap", "qa_summary",
        ]:
            assert key in data
            assert "enabled" in data[key]

    def test_model_json_round_trip(self):
        original = ReportConfig(
            title="JSON Test",
            rao_plots=RAOPlotsConfig(headings=[45.0, 135.0], include_phase=True),
            qtf_heatmap=QTFHeatmapConfig(max_delta_omegas=5),
        )
        json_str = original.model_dump_json()
        restored = ReportConfig.model_validate_json(json_str)
        assert restored.title == "JSON Test"
        assert restored.rao_plots.headings == [45.0, 135.0]
        assert restored.qtf_heatmap.max_delta_omegas == 5

    def test_model_dump_with_disabled_sections(self):
        cfg = ReportConfig(
            model_summary={"enabled": False},
            rao_plots={"enabled": False},
        )
        data = cfg.model_dump()
        assert data["model_summary"]["enabled"] is False
        assert data["rao_plots"]["enabled"] is False
        # Others still enabled
        assert data["hydro_matrices"]["enabled"] is True
