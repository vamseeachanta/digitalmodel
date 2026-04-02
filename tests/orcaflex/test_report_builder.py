"""Tests for OrcaFlexReportBuilder — HTML report assembly.

ABOUTME: Tests the builder class that loads a .sim file (mocked) and
assembles an HTML report from section builders.  All OrcFxAPI calls
are mocked.
"""
from __future__ import annotations

import sys
import types
from pathlib import Path
from unittest.mock import patch, MagicMock

import pytest

from digitalmodel.orcaflex.reporting.config import ReportConfig
from digitalmodel.orcaflex.reporting.builder import OrcaFlexReportBuilder, _load_orcfxapi


class TestLoadOrcfxapi:
    """Test the OrcFxAPI path loader."""

    def test_adds_api_path_to_sys_path(self, monkeypatch):
        """_load_orcfxapi adds the API path to sys.path."""
        monkeypatch.setenv("ORCAFLEX_API_PATH", "/fake/orcfxapi/path")
        _load_orcfxapi()
        assert "/fake/orcfxapi/path" in sys.path
        sys.path.remove("/fake/orcfxapi/path")

    def test_does_not_duplicate_path(self, monkeypatch):
        """Calling _load_orcfxapi twice doesn't duplicate the path."""
        monkeypatch.setenv("ORCAFLEX_API_PATH", "/fake/path")
        _load_orcfxapi()
        count1 = sys.path.count("/fake/path")
        _load_orcfxapi()
        count2 = sys.path.count("/fake/path")
        assert count1 == count2
        sys.path.remove("/fake/path")

    def test_uses_default_path_when_env_unset(self, monkeypatch):
        """Falls back to default Windows path when env var is unset."""
        monkeypatch.delenv("ORCAFLEX_API_PATH", raising=False)
        _load_orcfxapi()
        # Default path should be in sys.path now
        default = r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcFxAPI\Python"
        assert default in sys.path
        sys.path.remove(default)


class TestOrcaFlexReportBuilder:
    """Test builder construction and HTML assembly."""

    def test_construction(self, tmp_path: Path):
        """Builder stores sim_path and config."""
        sim = tmp_path / "model.sim"
        sim.write_text("placeholder")
        cfg = ReportConfig()
        builder = OrcaFlexReportBuilder(sim, cfg)
        assert builder.sim_path == sim
        assert builder.config is cfg
        assert builder._model is None

    def test_section_card_html(self):
        """_section_card produces valid HTML structure."""
        html = OrcaFlexReportBuilder._section_card("Test Heading", "<p>body</p>")
        assert 'class="section-card"' in html
        assert "Test Heading" in html
        assert "<p>body</p>" in html

    def test_disabled_card_html(self):
        """_disabled_card produces disabled section HTML."""
        html = OrcaFlexReportBuilder._disabled_card("Disabled Section")
        assert "section-disabled" in html
        assert "Disabled Section" in html
        assert "Section disabled" in html

    def test_build_produces_full_html(self, tmp_path: Path, fake_orcfxapi, fake_model):
        """build() produces complete HTML with title and plotly script."""
        sim = tmp_path / "test.sim"
        sim.write_text("placeholder")

        cfg = ReportConfig(title="Test Report Title")
        builder = OrcaFlexReportBuilder(sim, cfg)
        builder._model = fake_model  # bypass _load_simulation

        html = builder.build()
        assert "<!DOCTYPE html>" in html
        assert "Test Report Title" in html
        assert "plotly" in html.lower()
        assert "test.sim" in html

    def test_disabled_sections_show_disabled_card(
        self, tmp_path: Path, fake_orcfxapi, fake_model
    ):
        """Disabled sections produce 'section-disabled' cards."""
        sim = tmp_path / "test.sim"
        sim.write_text("placeholder")

        cfg = ReportConfig()
        # code_check, mooring_loads, modal_analysis are disabled by default
        builder = OrcaFlexReportBuilder(sim, cfg)
        builder._model = fake_model

        html = builder.build()
        assert html.count("section-disabled") >= 3

    def test_enabled_sections_produce_content(
        self, tmp_path: Path, fake_orcfxapi, fake_model
    ):
        """Enabled sections produce section-card (not disabled) content."""
        sim = tmp_path / "test.sim"
        sim.write_text("placeholder")

        cfg = ReportConfig()
        builder = OrcaFlexReportBuilder(sim, cfg)
        builder._model = fake_model

        html = builder.build()
        assert 'class="section-card"' in html
        # model summary, static config, time series, range graphs, qa_summary → 5 enabled
        # code_check, mooring_loads, modal_analysis → 3 disabled
        # 8 section divs total, but "section-card" also appears in the CSS style block
        # Count only the div occurrences
        div_count = html.count('class="section-card"') + html.count('class="section-card section-disabled"')
        assert div_count == 8  # 5 enabled + 3 disabled

    def test_error_in_section_gracefully_handled(
        self, tmp_path: Path, fake_orcfxapi, fake_model
    ):
        """Section build errors produce error card instead of crash."""
        sim = tmp_path / "test.sim"
        sim.write_text("placeholder")

        cfg = ReportConfig()
        cfg.code_check.enabled = True
        builder = OrcaFlexReportBuilder(sim, cfg)

        # Sabotage the model to cause errors
        fake_model.codeChecks = None
        builder._model = fake_model

        # Should not raise — errors are caught and shown in HTML
        html = builder.build()
        assert isinstance(html, str)
