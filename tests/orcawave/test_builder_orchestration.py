"""Tests for OrcaWave report builder — section orchestration, HTML assembly.

Extends test_report_builder.py with deeper orchestration tests: verifying
each section is called/skipped correctly, section ordering, error isolation.
"""
from __future__ import annotations

from pathlib import Path
from unittest.mock import MagicMock, patch, call

import pytest

from digitalmodel.orcawave.reporting.config import (
    ReportConfig,
    RAOPlotsConfig,
    ModelSummaryConfig,
    HydroMatricesConfig,
    MeanDriftConfig,
    PanelPressuresConfig,
    MultiBodyConfig,
    QTFHeatmapConfig,
    QASummaryConfig,
)
from digitalmodel.orcawave.reporting.builder import (
    OrcaWaveReportBuilder,
    _build_section,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def owr_path(tmp_path):
    p = tmp_path / "test_model.owr"
    p.touch()
    return p


@pytest.fixture
def all_enabled_config():
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


def _make_builder(owr_path, config):
    b = OrcaWaveReportBuilder(owr_path, config)
    b._load_diffraction = MagicMock(return_value=MagicMock())
    return b


# ---------------------------------------------------------------------------
# Section orchestration — section calls
# ---------------------------------------------------------------------------


class TestSectionOrchestration:
    """Verify the builder calls each section builder when enabled."""

    @patch("digitalmodel.orcawave.reporting.builder.model_summary")
    def test_model_summary_called_when_enabled(self, mock_mod, owr_path):
        mock_mod.build_model_summary.return_value = "<p>summary</p>"
        cfg = ReportConfig(
            rao_plots={"enabled": False},
            hydro_matrices={"enabled": False},
            mean_drift={"enabled": False},
            panel_pressures={"enabled": False},
            multi_body={"enabled": False},
            qtf_heatmap={"enabled": False},
            qa_summary={"enabled": False},
        )
        b = _make_builder(owr_path, cfg)
        html = b.build()
        mock_mod.build_model_summary.assert_called_once()
        assert "summary" in html

    @patch("digitalmodel.orcawave.reporting.builder.rao_plots")
    def test_rao_plots_called_when_enabled(self, mock_mod, owr_path):
        mock_mod.build_rao_plots.return_value = "<p>rao</p>"
        cfg = ReportConfig(
            model_summary={"enabled": False},
            hydro_matrices={"enabled": False},
            mean_drift={"enabled": False},
            panel_pressures={"enabled": False},
            multi_body={"enabled": False},
            qtf_heatmap={"enabled": False},
            qa_summary={"enabled": False},
        )
        b = _make_builder(owr_path, cfg)
        html = b.build()
        mock_mod.build_rao_plots.assert_called_once()
        assert "rao" in html

    @patch("digitalmodel.orcawave.reporting.builder.model_summary")
    def test_model_summary_not_called_when_disabled(self, mock_mod, owr_path):
        cfg = ReportConfig(model_summary={"enabled": False})
        b = _make_builder(owr_path, cfg)
        # Patch remaining sections to not fail
        with patch("digitalmodel.orcawave.reporting.builder.rao_plots") as m1, \
             patch("digitalmodel.orcawave.reporting.builder.hydro_matrices") as m2, \
             patch("digitalmodel.orcawave.reporting.builder.mean_drift") as m3, \
             patch("digitalmodel.orcawave.reporting.builder.panel_pressures") as m4, \
             patch("digitalmodel.orcawave.reporting.builder.multi_body") as m5, \
             patch("digitalmodel.orcawave.reporting.builder.qtf_heatmap") as m6, \
             patch("digitalmodel.orcawave.reporting.builder.qa_summary") as m7:
            for m in [m1, m2, m3, m4, m5, m6, m7]:
                getattr(m, list(vars(m).keys())[0] if vars(m) else "build_rao_plots", m).return_value = "<p>ok</p>"
            html = b.build()
        mock_mod.build_model_summary.assert_not_called()

    def test_all_disabled_produces_empty_sections(self, owr_path, all_disabled_config):
        b = _make_builder(owr_path, all_disabled_config)
        html = b.build()
        assert "<h2>" not in html
        assert "<!DOCTYPE html>" in html

    def test_all_enabled_produces_all_section_headings(self, owr_path):
        """When all sections enabled but their builders fail, we still get
        error placeholders with all 8 headings."""
        cfg = ReportConfig()
        b = _make_builder(owr_path, cfg)
        # All section modules will fail because mock diff has no real attributes
        # But _build_section catches exceptions, so build() should not raise
        html = b.build()
        for heading_text in [
            "1. Model Summary",
            "2. RAO Plots",
            "3. Hydrodynamic Matrices",
            "4. Mean Drift",
            "5. Panel Pressures",
            "6. Multi-Body Coupling",
            "7. QTF Heatmap",
            "8. QA Summary",
        ]:
            assert heading_text in html


# ---------------------------------------------------------------------------
# Section ordering
# ---------------------------------------------------------------------------


class TestSectionOrdering:
    """Verify sections appear in the correct order in the output."""

    def test_sections_appear_in_order(self, owr_path):
        cfg = ReportConfig()
        b = _make_builder(owr_path, cfg)
        html = b.build()
        positions = []
        for heading in [
            "1. Model Summary",
            "2. RAO Plots",
            "3. Hydrodynamic Matrices",
            "4. Mean Drift",
            "5. Panel Pressures",
            "6. Multi-Body Coupling",
            "7. QTF Heatmap",
            "8. QA Summary",
        ]:
            pos = html.find(heading)
            assert pos >= 0, f"Section '{heading}' not found in HTML"
            positions.append(pos)
        # Verify strictly increasing order
        assert positions == sorted(positions), "Sections are not in order"


# ---------------------------------------------------------------------------
# Error isolation
# ---------------------------------------------------------------------------


class TestErrorIsolation:
    """Verify that a failing section does not prevent other sections."""

    @patch("digitalmodel.orcawave.reporting.builder.model_summary")
    @patch("digitalmodel.orcawave.reporting.builder.qa_summary")
    def test_failing_section_doesnt_prevent_others(
        self, mock_qa, mock_model, owr_path
    ):
        """If model_summary raises, qa_summary should still render."""
        mock_model.build_model_summary.side_effect = RuntimeError("boom")
        mock_qa.build_qa_summary.return_value = "<p>qa ok</p>"

        cfg = ReportConfig(
            rao_plots={"enabled": False},
            hydro_matrices={"enabled": False},
            mean_drift={"enabled": False},
            panel_pressures={"enabled": False},
            multi_body={"enabled": False},
            qtf_heatmap={"enabled": False},
        )
        b = _make_builder(owr_path, cfg)
        html = b.build()

        # Model summary should show error placeholder
        assert "failed to render" in html.lower() or "text-danger" in html
        # QA summary should have rendered successfully
        assert "qa ok" in html


# ---------------------------------------------------------------------------
# _build_section edge cases
# ---------------------------------------------------------------------------


class TestBuildSectionEdgeCases:

    def test_empty_content_from_builder(self):
        parts = []
        _build_section(parts, "Empty", enabled=True, build_fn=lambda: "")
        assert len(parts) == 1
        assert "<h2>Empty</h2>" in parts[0]

    def test_html_content_preserved(self):
        parts = []
        html = '<div class="card"><p>Rich content</p></div>'
        _build_section(parts, "Rich", enabled=True, build_fn=lambda: html)
        assert html in parts[0]

    def test_multiple_sections_accumulated(self):
        parts = []
        _build_section(parts, "A", enabled=True, build_fn=lambda: "a")
        _build_section(parts, "B", enabled=True, build_fn=lambda: "b")
        _build_section(parts, "C", enabled=False, build_fn=lambda: "c")
        assert len(parts) == 2
        assert "A" in parts[0]
        assert "B" in parts[1]

    def test_exception_message_not_leaked_to_html(self):
        """The error placeholder should NOT contain the raw exception message."""
        parts = []
        _build_section(
            parts, "Leaky", enabled=True,
            build_fn=lambda: (_ for _ in ()).throw(ValueError("secret internal error")),
        )
        assert len(parts) == 1
        assert "secret internal error" not in parts[0]


# ---------------------------------------------------------------------------
# HTML output structure
# ---------------------------------------------------------------------------


class TestHTMLOutputStructure:

    def test_html_has_doctype(self, owr_path, all_disabled_config):
        b = _make_builder(owr_path, all_disabled_config)
        html = b.build()
        assert html.strip().startswith("<!DOCTYPE html>")

    def test_html_has_closing_tags(self, owr_path, all_disabled_config):
        b = _make_builder(owr_path, all_disabled_config)
        html = b.build()
        assert "</html>" in html
        assert "</body>" in html
        assert "</head>" in html

    def test_title_appears_in_h1_and_title_tag(self, owr_path):
        cfg = ReportConfig(
            title="Test Report Title",
            model_summary={"enabled": False},
            rao_plots={"enabled": False},
            hydro_matrices={"enabled": False},
            mean_drift={"enabled": False},
            panel_pressures={"enabled": False},
            multi_body={"enabled": False},
            qtf_heatmap={"enabled": False},
            qa_summary={"enabled": False},
        )
        b = _make_builder(owr_path, cfg)
        html = b.build()
        assert "<title>Test Report Title</title>" in html
        assert "<h1>Test Report Title</h1>" in html

    def test_bootstrap_css_linked(self, owr_path, all_disabled_config):
        b = _make_builder(owr_path, all_disabled_config)
        html = b.build()
        assert "bootstrap" in html.lower()
        assert "cdn.jsdelivr.net" in html

    def test_bootstrap_js_linked(self, owr_path, all_disabled_config):
        b = _make_builder(owr_path, all_disabled_config)
        html = b.build()
        assert "bootstrap.bundle.min.js" in html


# ---------------------------------------------------------------------------
# generate_orcawave_report edge cases
# ---------------------------------------------------------------------------


class TestGenerateReportEdgeCases:

    def test_config_yml_loading(self, tmp_path):
        from digitalmodel.orcawave.reporting import generate_orcawave_report

        yml = tmp_path / "cfg.yml"
        yml.write_text("title: 'YML Report'\n")
        owr = tmp_path / "test.owr"
        owr.touch()
        out = tmp_path / "out.html"

        with patch(
            "digitalmodel.orcawave.reporting.OrcaWaveReportBuilder"
        ) as MockBuilder:
            mock_instance = MockBuilder.return_value
            mock_instance.build.return_value = "<html>yml</html>"
            result = generate_orcawave_report(owr, out, config_yml=yml)

        assert result == out
        # Verify the builder was created with a config that has our title
        call_args = MockBuilder.call_args
        actual_cfg = call_args[0][1]
        assert actual_cfg.title == "YML Report"

    def test_output_file_created(self, tmp_path):
        from digitalmodel.orcawave.reporting import generate_orcawave_report

        owr = tmp_path / "test.owr"
        owr.touch()
        out = tmp_path / "subdir" / "out.html"

        with patch(
            "digitalmodel.orcawave.reporting.OrcaWaveReportBuilder"
        ) as MockBuilder:
            mock_instance = MockBuilder.return_value
            mock_instance.build.return_value = "<html>test</html>"
            # Need to create parent dir since write_text doesn't auto-create
            out.parent.mkdir(parents=True, exist_ok=True)
            result = generate_orcawave_report(owr, out)

        assert out.exists()
        assert out.read_text() == "<html>test</html>"
