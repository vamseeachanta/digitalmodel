# ABOUTME: TDD tests for the interactive HTML report builder with Tom Select
# ABOUTME: Validates HTML structure, Tom Select presence, chart divs, and code toggles

import os
import tempfile

import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
)
from digitalmodel.structural.analysis.wall_thickness_phases import (
    PhaseAnalysisRunner,
    PipeDefinition,
    create_standard_phases,
)
from digitalmodel.structural.analysis.wall_thickness_interactive_report import (
    InteractiveReportBuilder,
)


def make_builder_with_util_chart():
    builder = InteractiveReportBuilder()
    geom = PipeGeometry(outer_diameter=0.27305, wall_thickness=0.0214, corrosion_allowance=0.001)
    mat = PipeMaterial(grade="X65", smys=448e6, smts=531e6)
    builder.add_utilisation_vs_wt_chart(
        geometry=geom,
        material=mat,
        internal_pressure=20e6,
        external_pressure=5e6,
        codes=[DesignCode.DNV_ST_F101, DesignCode.API_RP_1111, DesignCode.PD_8010_2],
        wt_steps=5,
    )
    return builder


def make_builder_with_phase_chart():
    builder = InteractiveReportBuilder()
    pipe = PipeDefinition(
        outer_diameter=0.27305, wall_thickness=0.0214,
        grade="X65", smys=448e6, smts=531e6,
        corrosion_allowance=0.001,
    )
    phases = create_standard_phases(water_depth=500, design_pressure=20e6)
    codes = [DesignCode.DNV_ST_F101, DesignCode.API_RP_1111]
    runner = PhaseAnalysisRunner(pipe, phases, codes)
    comparison = runner.run()
    builder.add_phase_comparison_chart(comparison)
    return builder


class TestInteractiveReportStructure:
    def test_build_returns_html_string(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            assert isinstance(html, str)
            assert len(html) > 0

    def test_build_creates_file(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            builder.build(path)
            assert os.path.isfile(path)

    def test_html_contains_doctype(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            assert "<!DOCTYPE html>" in html

    def test_html_contains_title(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path, title="Test Report")
            assert "Test Report" in html


class TestTomSelectPresence:
    def test_html_contains_tom_select_css(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            assert "tom-select.css" in html

    def test_html_contains_tom_select_js(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            assert "tom-select.complete.min.js" in html

    def test_html_contains_code_select_element(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            assert 'id="code-select"' in html

    def test_html_contains_code_options(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            assert "DNV-ST-F101" in html
            assert "API-RP-1111" in html
            assert "PD-8010-2" in html

    def test_html_options_are_pre_selected(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            # All options should have 'selected' attribute
            assert html.count("selected") >= 3


class TestChartPresence:
    def test_html_contains_utilisation_chart_div(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            assert "chart-util-vs-wt" in html

    def test_html_contains_plotly_cdn(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            assert "plotly" in html.lower()

    def test_html_contains_phase_chart_div(self):
        builder = make_builder_with_phase_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            assert "chart-phase-comparison" in html


class TestToggleJavaScript:
    def test_html_contains_plotly_restyle(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            assert "Plotly.restyle" in html

    def test_html_contains_change_listener(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            assert "on('change'" in html

    def test_html_contains_meta_code_reference(self):
        builder = make_builder_with_util_chart()
        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            assert "meta" in html
            assert "code" in html


class TestMultipleCharts:
    def test_both_charts_present(self):
        builder = InteractiveReportBuilder()
        geom = PipeGeometry(outer_diameter=0.27305, wall_thickness=0.0214, corrosion_allowance=0.001)
        mat = PipeMaterial(grade="X65", smys=448e6, smts=531e6)
        builder.add_utilisation_vs_wt_chart(
            geometry=geom, material=mat,
            internal_pressure=20e6, external_pressure=5e6,
            codes=[DesignCode.DNV_ST_F101, DesignCode.API_RP_1111],
            wt_steps=3,
        )
        pipe = PipeDefinition(
            outer_diameter=0.27305, wall_thickness=0.0214,
            grade="X65", smys=448e6, smts=531e6, corrosion_allowance=0.001,
        )
        phases = create_standard_phases(water_depth=500, design_pressure=20e6)
        runner = PhaseAnalysisRunner(pipe, phases, [DesignCode.DNV_ST_F101, DesignCode.API_RP_1111])
        comparison = runner.run()
        builder.add_phase_comparison_chart(comparison)

        with tempfile.TemporaryDirectory() as tmpdir:
            path = os.path.join(tmpdir, "report.html")
            html = builder.build(path)
            assert "chart-util-vs-wt" in html
            assert "chart-phase-comparison" in html
