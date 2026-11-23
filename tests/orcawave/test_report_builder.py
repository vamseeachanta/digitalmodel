"""Tests for OrcaWaveReportBuilder — init, section generation, HTML output.

All tests mock OrcFxAPI so no actual solver is required.
"""
from __future__ import annotations

from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from digitalmodel.orcawave.reporting.config import (
    RAOPlotsConfig,
    ReportConfig,
)
from digitalmodel.orcawave.reporting.builder import (
    OrcaWaveReportBuilder,
    _build_section,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def default_config():
    return ReportConfig()


@pytest.fixture
def all_disabled_config():
    return ReportConfig(
        model_summary={"enabled": False},
        rao_plots={"enabled": False},
        hydro_matrices={"enabled": False},
        mean_drift={"enabled": False},
        panel_pressures={"enabled": False},
        multi_body={"enabled": False},
        qtf_heatmap={"enabled": False},
        qa_summary={"enabled": False},
    )


@pytest.fixture
def builder(tmp_path, default_config):
    owr_path = tmp_path / "test.owr"
    owr_path.touch()
    return OrcaWaveReportBuilder(owr_path, default_config)


# ---------------------------------------------------------------------------
# OrcaWaveReportBuilder.__init__
# ---------------------------------------------------------------------------


class TestBuilderInit:
    """Tests for OrcaWaveReportBuilder initialization."""

    def test_init_stores_path(self, builder, tmp_path):
        assert builder.owr_path == tmp_path / "test.owr"

    def test_init_stores_config(self, builder, default_config):
        assert builder.config is default_config

    def test_owr_path_is_pathlib(self, default_config):
        b = OrcaWaveReportBuilder("/some/path.owr", default_config)
        assert isinstance(b.owr_path, Path)
        assert b.owr_path == Path("/some/path.owr")

    def test_string_path_accepted(self, default_config, tmp_path):
        owr = tmp_path / "test.owr"
        owr.touch()
        b = OrcaWaveReportBuilder(str(owr), default_config)
        assert b.owr_path == owr


# ---------------------------------------------------------------------------
# _build_section helper
# ---------------------------------------------------------------------------


class TestBuildSection:
    """Tests for the _build_section helper function."""

    def test_skips_when_disabled(self):
        parts: list[str] = []
        _build_section(parts, "Test Section", enabled=False, build_fn=lambda: "html")
        assert len(parts) == 0

    def test_appends_content_when_enabled(self):
        parts: list[str] = []
        _build_section(
            parts, "Test Section", enabled=True, build_fn=lambda: "<p>content</p>"
        )
        assert len(parts) == 1
        assert "Test Section" in parts[0]
        assert "<p>content</p>" in parts[0]

    def test_catches_exceptions_gracefully(self):
        parts: list[str] = []

        def failing_fn():
            raise RuntimeError("oops")

        _build_section(parts, "Failing Section", enabled=True, build_fn=failing_fn)
        assert len(parts) == 1
        assert "failed to render" in parts[0].lower() or "text-danger" in parts[0]

    def test_heading_wrapped_in_h2(self):
        parts: list[str] = []
        _build_section(parts, "My Heading", enabled=True, build_fn=lambda: "body")
        assert "<h2>My Heading</h2>" in parts[0]


# ---------------------------------------------------------------------------
# OrcaWaveReportBuilder._load_diffraction
# ---------------------------------------------------------------------------


class TestLoadDiffraction:
    """Tests for _load_diffraction import and error handling."""

    def test_raises_import_error_without_orcfxapi(self, builder):
        """Without OrcFxAPI installed, _load_diffraction should raise ImportError."""
        with pytest.raises(ImportError, match="OrcFxAPI"):
            builder._load_diffraction()


# ---------------------------------------------------------------------------
# OrcaWaveReportBuilder.build (mocked diffraction)
# ---------------------------------------------------------------------------


class TestBuild:
    """Tests for .build() with a fully mocked Diffraction object."""

    def test_build_all_disabled_returns_html_skeleton(
        self, tmp_path, all_disabled_config
    ):
        """With all sections disabled, build should return valid HTML with no sections."""
        owr = tmp_path / "test.owr"
        owr.touch()
        b = OrcaWaveReportBuilder(owr, all_disabled_config)

        # Mock _load_diffraction so it doesn't need OrcFxAPI
        b._load_diffraction = MagicMock(return_value=MagicMock())
        html = b.build()

        assert "<!DOCTYPE html>" in html
        assert "<title>" in html
        assert "OrcaWave Analysis Report" in html

    def test_build_custom_title(self, tmp_path):
        cfg = ReportConfig(
            title="My Analysis",
            model_summary={"enabled": False},
            rao_plots={"enabled": False},
            hydro_matrices={"enabled": False},
            mean_drift={"enabled": False},
            panel_pressures={"enabled": False},
            multi_body={"enabled": False},
            qtf_heatmap={"enabled": False},
            qa_summary={"enabled": False},
        )
        owr = tmp_path / "test.owr"
        owr.touch()
        b = OrcaWaveReportBuilder(owr, cfg)
        b._load_diffraction = MagicMock(return_value=MagicMock())
        html = b.build()
        assert "My Analysis" in html

    def test_build_contains_bootstrap_css(self, tmp_path, all_disabled_config):
        owr = tmp_path / "test.owr"
        owr.touch()
        b = OrcaWaveReportBuilder(owr, all_disabled_config)
        b._load_diffraction = MagicMock(return_value=MagicMock())
        html = b.build()
        assert "bootstrap" in html.lower()


# ---------------------------------------------------------------------------
# generate_orcawave_report convenience function
# ---------------------------------------------------------------------------


class TestGenerateReport:
    """Tests for the top-level generate_orcawave_report function."""

    def test_raises_on_both_config_and_yml(self, tmp_path):
        from digitalmodel.orcawave.reporting import generate_orcawave_report

        with pytest.raises(ValueError, match="not both"):
            generate_orcawave_report(
                owr_path=tmp_path / "x.owr",
                output_html=tmp_path / "out.html",
                config=ReportConfig(),
                config_yml=tmp_path / "config.yml",
            )

    def test_default_config_when_none_provided(self, tmp_path):
        """When neither config nor config_yml is given, default ReportConfig is used."""
        from digitalmodel.orcawave.reporting import generate_orcawave_report

        owr = tmp_path / "test.owr"
        owr.touch()
        out = tmp_path / "out.html"

        # Patch the builder to avoid needing OrcFxAPI
        with patch(
            "digitalmodel.orcawave.reporting.OrcaWaveReportBuilder"
        ) as MockBuilder:
            mock_instance = MockBuilder.return_value
            mock_instance.build.return_value = "<html>test</html>"
            result = generate_orcawave_report(owr, out)

        assert result == out
        assert out.read_text() == "<html>test</html>"
