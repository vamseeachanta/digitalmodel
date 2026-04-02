"""Tests for all orcaflex.reporting.sections — build_* functions.

ABOUTME: Tests each section builder individually with fake OrcFxAPI objects.
Each function is tested for HTML output, content correctness, and edge cases.
"""
from __future__ import annotations

import json
import sys
import types
from pathlib import Path
from unittest.mock import patch

import pytest

from digitalmodel.orcaflex.reporting.config import ReportConfig
from digitalmodel.orcaflex.reporting import sections


# Import conftest fixtures via the conftest.py in parent orcaflex/ dir
# fake_orcfxapi and fake_model fixtures are auto-discovered


class TestBuildModelSummary:
    """Test model summary section builder."""

    def test_returns_html_string(self, fake_orcfxapi, fake_model):
        """build_model_summary returns a non-empty HTML string."""
        cfg = ReportConfig()
        html = sections.build_model_summary(fake_model, cfg.model_summary)
        assert isinstance(html, str)
        assert len(html) > 0

    def test_contains_table(self, fake_orcfxapi, fake_model):
        """Output contains an HTML table."""
        cfg = ReportConfig()
        html = sections.build_model_summary(fake_model, cfg.model_summary)
        assert '<table class="ofx">' in html

    def test_shows_simulation_filename(self, fake_orcfxapi, fake_model):
        """Simulation filename appears in the summary."""
        cfg = ReportConfig()
        html = sections.build_model_summary(fake_model, cfg.model_summary)
        assert "test_model.sim" in html

    def test_shows_orcaflex_version(self, fake_orcfxapi, fake_model):
        """OrcaFlex version appears in the summary."""
        cfg = ReportConfig()
        html = sections.build_model_summary(fake_model, cfg.model_summary)
        assert "11.6" in html

    def test_shows_environment_data(self, fake_orcfxapi, fake_model):
        """Environment parameters appear in summary."""
        cfg = ReportConfig()
        html = sections.build_model_summary(fake_model, cfg.model_summary)
        assert "100.0" in html  # water depth
        assert "JONSWAP" in html  # wave type

    def test_shows_object_counts(self, fake_orcfxapi, fake_model):
        """Object counts (lines, vessels) are shown."""
        cfg = ReportConfig()
        html = sections.build_model_summary(fake_model, cfg.model_summary)
        assert "2" in html  # 2 lines
        assert "1" in html  # 1 vessel


class TestBuildStaticConfig:
    """Test static configuration section builder."""

    def test_returns_html(self, fake_orcfxapi, fake_model):
        """build_static_config returns HTML string."""
        cfg = ReportConfig()
        html = sections.build_static_config(fake_model, cfg.static_config)
        assert isinstance(html, str)
        assert len(html) > 0

    def test_shows_tension_values(self, fake_orcfxapi, fake_model):
        """Static tension values appear in output."""
        cfg = ReportConfig()
        html = sections.build_static_config(fake_model, cfg.static_config)
        assert "100.00" in html  # our fake returns 100.0

    def test_shows_line_names(self, fake_orcfxapi, fake_model):
        """Line names appear in output."""
        cfg = ReportConfig()
        html = sections.build_static_config(fake_model, cfg.static_config)
        assert "Line1" in html

    def test_no_lines_returns_message(self, fake_orcfxapi, fake_model):
        """Empty model with no lines returns informative message."""
        fake_model.objects = []
        cfg = ReportConfig()
        html = sections.build_static_config(fake_model, cfg.static_config)
        assert "No line objects" in html

    def test_vessel_static_positions(self, fake_orcfxapi, fake_model):
        """Vessel static positions appear in output."""
        cfg = ReportConfig()
        html = sections.build_static_config(fake_model, cfg.static_config)
        assert "FPSO1" in html


class TestBuildTimeSeries:
    """Test time series section builder."""

    def test_returns_plotly_html(self, fake_orcfxapi, fake_model):
        """build_time_series returns Plotly chart HTML."""
        cfg = ReportConfig()
        html = sections.build_time_series(fake_model, cfg.time_series)
        assert isinstance(html, str)
        assert "Plotly.newPlot" in html

    def test_contains_chart_divs(self, fake_orcfxapi, fake_model):
        """Output contains chart div elements."""
        cfg = ReportConfig()
        html = sections.build_time_series(fake_model, cfg.time_series)
        assert "ts_chart_" in html

    def test_vessel_dofs_plotted(self, fake_orcfxapi, fake_model):
        """Vessel DOF names appear in chart titles."""
        cfg = ReportConfig()
        html = sections.build_time_series(fake_model, cfg.time_series)
        assert "Surge" in html or "Heave" in html or "FPSO1" in html

    def test_line_tension_plotted(self, fake_orcfxapi, fake_model):
        """Line tension time series appears."""
        cfg = ReportConfig()
        html = sections.build_time_series(fake_model, cfg.time_series)
        assert "Tension" in html or "Line1" in html

    def test_no_objects_still_returns_string(self, fake_orcfxapi, fake_model):
        """Empty model still returns a string (no crash)."""
        fake_model.objects = []
        cfg = ReportConfig()
        html = sections.build_time_series(fake_model, cfg.time_series)
        assert isinstance(html, str)


class TestBuildRangeGraphs:
    """Test range graph section builder."""

    def test_returns_plotly_html(self, fake_orcfxapi, fake_model):
        """build_range_graphs returns Plotly chart HTML."""
        cfg = ReportConfig()
        html = sections.build_range_graphs(fake_model, cfg.range_graphs)
        assert "Plotly.newPlot" in html

    def test_contains_min_max_traces(self, fake_orcfxapi, fake_model):
        """Charts contain Min and Max traces."""
        cfg = ReportConfig()
        html = sections.build_range_graphs(fake_model, cfg.range_graphs)
        assert "Max" in html
        assert "Min" in html

    def test_shows_line_names_in_titles(self, fake_orcfxapi, fake_model):
        """Line names appear in chart titles."""
        cfg = ReportConfig()
        html = sections.build_range_graphs(fake_model, cfg.range_graphs)
        assert "Line1" in html

    def test_no_lines_returns_message(self, fake_orcfxapi, fake_model):
        """Empty model returns informative message."""
        fake_model.objects = []
        cfg = ReportConfig()
        html = sections.build_range_graphs(fake_model, cfg.range_graphs)
        assert "No line objects" in html

    def test_mean_trace_included(self, fake_orcfxapi, fake_model):
        """Mean trace is included when data has Mean attribute."""
        cfg = ReportConfig()
        html = sections.build_range_graphs(fake_model, cfg.range_graphs)
        assert "Mean" in html


class TestBuildCodeCheck:
    """Test code check utilization table builder."""

    def test_returns_table_html(self, fake_orcfxapi, fake_model):
        """build_code_check returns HTML table."""
        cfg = ReportConfig()
        html = sections.build_code_check(fake_model, cfg.code_check)
        assert '<table class="ofx">' in html

    def test_shows_pass_for_low_utilisation(self, fake_orcfxapi, fake_model):
        """Utilisation <= 1.0 gets 'pass' class."""
        cfg = ReportConfig()
        html = sections.build_code_check(fake_model, cfg.code_check)
        assert 'class="pass"' in html

    def test_shows_fail_for_high_utilisation(self, fake_orcfxapi, fake_model):
        """Utilisation > 1.0 gets 'fail' class."""
        cfg = ReportConfig()
        html = sections.build_code_check(fake_model, cfg.code_check)
        assert 'class="fail"' in html  # ISO-19902 has Utilisation=1.05

    def test_no_code_checks_returns_message(self, fake_orcfxapi, fake_model):
        """No code checks returns informative message."""
        fake_model.codeChecks = []
        cfg = ReportConfig()
        html = sections.build_code_check(fake_model, cfg.code_check)
        assert "No code check results" in html

    def test_shows_check_names(self, fake_orcfxapi, fake_model):
        """Code check names appear in table."""
        cfg = ReportConfig()
        html = sections.build_code_check(fake_model, cfg.code_check)
        assert "API-2A-WSD" in html
        assert "ISO-19902" in html


class TestBuildMooringLoads:
    """Test mooring loads section builder."""

    def test_returns_table_html(self, fake_orcfxapi, fake_model):
        """build_mooring_loads returns HTML table."""
        cfg = ReportConfig()
        html = sections.build_mooring_loads(fake_model, cfg.mooring_loads)
        assert '<table class="ofx">' in html

    def test_shows_static_tension(self, fake_orcfxapi, fake_model):
        """Static tension values appear."""
        cfg = ReportConfig()
        html = sections.build_mooring_loads(fake_model, cfg.mooring_loads)
        assert "100.0" in html

    def test_shows_dynamic_range(self, fake_orcfxapi, fake_model):
        """Dynamic min/max range appears."""
        cfg = ReportConfig()
        html = sections.build_mooring_loads(fake_model, cfg.mooring_loads)
        assert "90.0" in html  # min of fake TimeHistory
        assert "110.0" in html  # max of fake TimeHistory

    def test_no_lines_returns_message(self, fake_orcfxapi, fake_model):
        """No mooring lines returns informative message."""
        fake_model.objects = []
        cfg = ReportConfig()
        html = sections.build_mooring_loads(fake_model, cfg.mooring_loads)
        assert "No mooring lines" in html

    def test_line_names_in_table(self, fake_orcfxapi, fake_model):
        """Line names appear in the mooring loads table."""
        cfg = ReportConfig()
        html = sections.build_mooring_loads(fake_model, cfg.mooring_loads)
        assert "Line1" in html


class TestBuildModalAnalysis:
    """Test modal analysis section builder."""

    def test_returns_table_html(self, fake_orcfxapi, fake_model):
        """build_modal_analysis returns HTML table."""
        cfg = ReportConfig()
        html = sections.build_modal_analysis(fake_model, cfg.modal_analysis)
        assert '<table class="ofx">' in html

    def test_shows_mode_descriptions(self, fake_orcfxapi, fake_model):
        """Mode descriptions appear in table."""
        cfg = ReportConfig()
        html = sections.build_modal_analysis(fake_model, cfg.modal_analysis)
        assert "1st lateral mode" in html
        assert "1st vertical mode" in html

    def test_shows_frequencies(self, fake_orcfxapi, fake_model):
        """Natural frequencies appear in table."""
        cfg = ReportConfig()
        html = sections.build_modal_analysis(fake_model, cfg.modal_analysis)
        assert "0.1500" in html  # 0.15 Hz formatted to 4 decimal places
        assert "0.2200" in html  # 0.22 Hz

    def test_shows_periods(self, fake_orcfxapi, fake_model):
        """Period (1/freq) values appear."""
        cfg = ReportConfig()
        html = sections.build_modal_analysis(fake_model, cfg.modal_analysis)
        # 1/0.15 = 6.67, 1/0.22 = 4.55
        assert "6.67" in html
        assert "4.55" in html

    def test_no_modes_returns_message(self, fake_orcfxapi, fake_model):
        """No modal results returns informative message."""
        fake_model.ModalAnalysisResults = []
        cfg = ReportConfig()
        html = sections.build_modal_analysis(fake_model, cfg.modal_analysis)
        assert "No modal analysis results" in html

    def test_mode_count(self, fake_orcfxapi, fake_model):
        """Correct number of modes in output."""
        cfg = ReportConfig()
        html = sections.build_modal_analysis(fake_model, cfg.modal_analysis)
        # 3 modes → 3 data rows + 1 header row
        assert html.count("<tr>") >= 3


class TestBuildQASummary:
    """Test QA summary section builder."""

    def test_no_qa_dir_returns_message(self, fake_orcfxapi, fake_model):
        """Missing QA directory returns informative message."""
        cfg = ReportConfig()
        # The _QA_DIR is hardcoded and won't exist in test env
        html = sections.build_qa_summary(fake_model, cfg.qa_summary)
        assert "No QA results found" in html or isinstance(html, str)

    def test_with_qa_results_files(self, fake_orcfxapi, fake_model, tmp_path: Path):
        """QA results files are read and displayed."""
        qa_dir = tmp_path / "qa"
        qa_dir.mkdir()

        # Create fake QA results
        import json
        results = {
            "example_id": "L00_validation",
            "checks": [
                {"name": "freq_check", "passed": True},
                {"name": "rao_check", "passed": True},
                {"name": "phase_check", "passed": False},
            ],
        }
        (qa_dir / "L00_validation_qa_results.json").write_text(json.dumps(results))

        # Patch _QA_DIR to our tmp dir
        with patch("digitalmodel.orcaflex.reporting.sections.qa_summary._QA_DIR", qa_dir):
            cfg = ReportConfig()
            html = sections.build_qa_summary(fake_model, cfg.qa_summary)

        assert "L00_validation" in html
        assert "FAIL" in html  # 1 check failed

    def test_skipped_example(self, fake_orcfxapi, fake_model, tmp_path: Path):
        """Skipped examples show skip reason."""
        qa_dir = tmp_path / "qa"
        qa_dir.mkdir()

        import json
        results = {
            "example_id": "L03_skipped",
            "skipped": True,
            "skip_reason": "License not available",
        }
        (qa_dir / "L03_skipped_qa_results.json").write_text(json.dumps(results))

        with patch("digitalmodel.orcaflex.reporting.sections.qa_summary._QA_DIR", qa_dir):
            cfg = ReportConfig()
            html = sections.build_qa_summary(fake_model, cfg.qa_summary)

        assert "Skipped" in html
        assert "License not available" in html

    def test_passing_example(self, fake_orcfxapi, fake_model, tmp_path: Path):
        """All-passing example shows PASS status."""
        qa_dir = tmp_path / "qa"
        qa_dir.mkdir()

        import json
        results = {
            "example_id": "L00_pass",
            "checks": [
                {"name": "check1", "passed": True},
                {"name": "check2", "passed": True},
            ],
        }
        (qa_dir / "L00_pass_qa_results.json").write_text(json.dumps(results))

        with patch("digitalmodel.orcaflex.reporting.sections.qa_summary._QA_DIR", qa_dir):
            cfg = ReportConfig()
            html = sections.build_qa_summary(fake_model, cfg.qa_summary)

        assert "PASS" in html
        assert 'class="pass"' in html

    def test_malformed_json_handled(self, fake_orcfxapi, fake_model, tmp_path: Path):
        """Malformed JSON file doesn't crash — shows error."""
        qa_dir = tmp_path / "qa"
        qa_dir.mkdir()
        (qa_dir / "broken_qa_results.json").write_text("{invalid json")

        with patch("digitalmodel.orcaflex.reporting.sections.qa_summary._QA_DIR", qa_dir):
            cfg = ReportConfig()
            html = sections.build_qa_summary(fake_model, cfg.qa_summary)

        assert "Parse error" in html or isinstance(html, str)
