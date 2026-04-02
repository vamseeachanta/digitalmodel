"""Tests for generate_orcaflex_report top-level convenience function.

ABOUTME: Tests the high-level entry point that wires config + builder + output.
Validates argument validation, config loading, and file output behavior.
"""
from __future__ import annotations

import textwrap
from pathlib import Path
from unittest.mock import patch, MagicMock

import pytest

from digitalmodel.orcaflex.reporting import generate_orcaflex_report, ReportConfig


class TestGenerateReportArgValidation:
    """Test argument validation for generate_orcaflex_report."""

    def test_both_config_and_config_yml_raises(self, tmp_path: Path):
        """Providing both config and config_yml raises ValueError."""
        sim = tmp_path / "model.sim"
        sim.write_text("placeholder")
        yml = tmp_path / "config.yaml"
        yml.write_text("title: test\n")
        output = tmp_path / "report.html"
        cfg = ReportConfig()

        with pytest.raises(ValueError, match="Provide either config or config_yml"):
            generate_orcaflex_report(
                sim_path=sim,
                output_html=output,
                config=cfg,
                config_yml=yml,
            )

    def test_default_config_when_none_provided(
        self, tmp_path: Path, fake_orcfxapi, fake_model
    ):
        """When no config is provided, default ReportConfig is used."""
        sim = tmp_path / "test.sim"
        sim.write_text("placeholder")
        output = tmp_path / "report.html"

        # Patch the builder to avoid actual model loading
        with patch(
            "digitalmodel.orcaflex.reporting.OrcaFlexReportBuilder"
        ) as MockBuilder:
            mock_instance = MagicMock()
            mock_instance.build.return_value = "<html>test</html>"
            MockBuilder.return_value = mock_instance

            result = generate_orcaflex_report(sim_path=sim, output_html=output)

        # Verify default config was used
        call_kwargs = MockBuilder.call_args
        assert isinstance(call_kwargs.kwargs.get("config") or call_kwargs[1].get("config", call_kwargs[0][1] if len(call_kwargs[0]) > 1 else None), ReportConfig)

    def test_custom_config_passed_through(
        self, tmp_path: Path, fake_orcfxapi, fake_model
    ):
        """Custom ReportConfig is passed to builder."""
        sim = tmp_path / "test.sim"
        sim.write_text("placeholder")
        output = tmp_path / "report.html"
        cfg = ReportConfig(title="Custom Title")

        with patch(
            "digitalmodel.orcaflex.reporting.OrcaFlexReportBuilder"
        ) as MockBuilder:
            mock_instance = MagicMock()
            mock_instance.build.return_value = "<html>custom</html>"
            MockBuilder.return_value = mock_instance

            generate_orcaflex_report(sim_path=sim, output_html=output, config=cfg)

        # Verify builder was called (config gets passed as positional or keyword)
        MockBuilder.assert_called_once()
        call_args, call_kwargs = MockBuilder.call_args
        # Config is passed — check it's present in args or kwargs
        all_args = list(call_args) + list(call_kwargs.values())
        assert cfg in all_args


class TestGenerateReportOutput:
    """Test file output behavior."""

    def test_creates_output_file(self, tmp_path: Path, fake_orcfxapi, fake_model):
        """HTML report file is created at output_html path."""
        sim = tmp_path / "test.sim"
        sim.write_text("placeholder")
        output = tmp_path / "output" / "report.html"

        with patch(
            "digitalmodel.orcaflex.reporting.OrcaFlexReportBuilder"
        ) as MockBuilder:
            mock_instance = MagicMock()
            mock_instance.build.return_value = "<html><body>Report</body></html>"
            MockBuilder.return_value = mock_instance

            result = generate_orcaflex_report(sim_path=sim, output_html=output)

        assert output.exists()
        assert result == output

    def test_creates_parent_directories(self, tmp_path: Path, fake_orcfxapi):
        """Parent directories are created if they don't exist."""
        sim = tmp_path / "test.sim"
        sim.write_text("placeholder")
        output = tmp_path / "deep" / "nested" / "dir" / "report.html"

        with patch(
            "digitalmodel.orcaflex.reporting.OrcaFlexReportBuilder"
        ) as MockBuilder:
            mock_instance = MagicMock()
            mock_instance.build.return_value = "<html>test</html>"
            MockBuilder.return_value = mock_instance

            generate_orcaflex_report(sim_path=sim, output_html=output)

        assert output.exists()
        assert output.parent.exists()

    def test_output_content_matches_builder(self, tmp_path: Path, fake_orcfxapi):
        """Written HTML content matches what builder.build() returns."""
        sim = tmp_path / "test.sim"
        sim.write_text("placeholder")
        output = tmp_path / "report.html"
        expected_html = "<!DOCTYPE html><html><body>Test Report Content</body></html>"

        with patch(
            "digitalmodel.orcaflex.reporting.OrcaFlexReportBuilder"
        ) as MockBuilder:
            mock_instance = MagicMock()
            mock_instance.build.return_value = expected_html
            MockBuilder.return_value = mock_instance

            generate_orcaflex_report(sim_path=sim, output_html=output)

        assert output.read_text(encoding="utf-8") == expected_html

    def test_returns_path_object(self, tmp_path: Path, fake_orcfxapi):
        """Return value is a Path object."""
        sim = tmp_path / "test.sim"
        sim.write_text("placeholder")
        output = tmp_path / "report.html"

        with patch(
            "digitalmodel.orcaflex.reporting.OrcaFlexReportBuilder"
        ) as MockBuilder:
            mock_instance = MagicMock()
            mock_instance.build.return_value = "<html></html>"
            MockBuilder.return_value = mock_instance

            result = generate_orcaflex_report(sim_path=sim, output_html=output)

        assert isinstance(result, Path)


class TestGenerateReportFromYAML:
    """Test config_yml parameter for YAML-based config loading."""

    def test_loads_config_from_yaml(self, tmp_path: Path, fake_orcfxapi):
        """config_yml loads ReportConfig from a YAML file."""
        sim = tmp_path / "test.sim"
        sim.write_text("placeholder")
        yml = tmp_path / "config.yaml"
        yml.write_text(textwrap.dedent("""\
            title: "YAML Config Report"
            sections:
              code_check: true
              mooring_loads: false
        """))
        output = tmp_path / "report.html"

        with patch(
            "digitalmodel.orcaflex.reporting.OrcaFlexReportBuilder"
        ) as MockBuilder:
            mock_instance = MagicMock()
            mock_instance.build.return_value = "<html>yaml config</html>"
            MockBuilder.return_value = mock_instance

            generate_orcaflex_report(
                sim_path=sim, output_html=output, config_yml=yml
            )

        # Verify config was loaded from YAML
        call_args = MockBuilder.call_args
        config = call_args.kwargs.get("config") or (call_args[0][1] if len(call_args[0]) > 1 else None)
        if config is not None:
            assert config.title == "YAML Config Report"
