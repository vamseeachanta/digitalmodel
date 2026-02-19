# ABOUTME: Tests for design-code fatigue report templates (WRK-157 Phase 5)
# ABOUTME: Validates per-standard HTML generation, references, status colors, and file output

"""Tests for design_code_report module."""

import tempfile

import pytest

from digitalmodel.structural.fatigue.design_code_report import (
    DesignCodeReport,
    generate_design_code_report,
)
from digitalmodel.structural.fatigue.worked_examples import pipeline_girth_weld


STANDARDS = ["DNV-RP-C203", "BS 7608", "API RP 2A", "IIW"]


@pytest.fixture()
def example_result():
    return pipeline_girth_weld()


@pytest.fixture()
def raw_damage_payload():
    return {
        "total_damage": 0.24,
        "dff": 3.0,
        "design_life_years": 20.0,
        "curve_class": "D",
        "A": 5.73e11,
        "m": 3.0,
        "fatigue_limit": 52.63,
        "histogram": [
            {"range": 45.0, "count": 2.0e6},
            {"range": 70.0, "count": 6.0e5},
            {"range": 95.0, "count": 1.5e5},
        ],
    }


class TestDesignCodeReportSmoke:
    @pytest.mark.parametrize("standard", STANDARDS)
    def test_generate_returns_html_string(self, standard, example_result):
        html = DesignCodeReport(standard=standard, result=example_result).generate()
        assert isinstance(html, str)
        assert "<html" in html.lower()

    @pytest.mark.parametrize("standard", STANDARDS)
    def test_contains_standard_name(self, standard, example_result):
        html = DesignCodeReport(standard=standard, result=example_result).generate()
        assert standard in html

    @pytest.mark.parametrize("standard", STANDARDS)
    def test_contains_plotly_script(self, standard, example_result):
        html = DesignCodeReport(standard=standard, result=example_result).generate()
        assert "cdn.plot.ly" in html
        assert "Plotly.newPlot" in html

    @pytest.mark.parametrize("standard", STANDARDS)
    def test_contains_damage_value(self, standard, raw_damage_payload):
        html = DesignCodeReport(standard=standard, result=raw_damage_payload).generate()
        assert "Total Damage" in html
        assert "0.240000" in html


class TestSectionsAndContent:
    @pytest.mark.parametrize("standard", STANDARDS)
    def test_required_sections_exist(self, standard, example_result):
        html = DesignCodeReport(standard=standard, result=example_result).generate()
        assert "Methodology Summary" in html
        assert "Applicable Scope" in html
        assert "S-N Curve Parameters" in html
        assert "Damage Breakdown" in html
        assert "Recommendations" in html
        assert "References" in html

    @pytest.mark.parametrize("standard", STANDARDS)
    def test_palmgren_miner_methodology_block_present(self, standard, example_result):
        html = DesignCodeReport(standard=standard, result=example_result).generate()
        assert "Palmgren-Miner" in html
        assert "D = Σ(ni/Ni)" in html

    @pytest.mark.parametrize("standard", STANDARDS)
    def test_sn_parameter_table_headers_present(self, standard, example_result):
        html = DesignCodeReport(standard=standard, result=example_result).generate()
        assert "m-Slope" in html
        assert "log(A)" in html
        assert "CAFL" in html

    @pytest.mark.parametrize("standard", STANDARDS)
    def test_damage_breakdown_headers_present(self, standard, example_result):
        html = DesignCodeReport(standard=standard, result=example_result).generate()
        assert "Stress Range (MPa)" in html
        assert ">ni<" in html
        assert ">Ni<" in html
        assert "ni/Ni" in html

    @pytest.mark.parametrize(
        ("standard", "expected_reference"),
        [
            ("DNV-RP-C203", "DNV-RP-C203 (2016) including 2019 amendments"),
            ("BS 7608", "BS 7608:2014+A1:2015"),
            ("API RP 2A", "API RP 2A-WSD 22nd Edition"),
            ("IIW", "XIII-2460-13"),
        ],
    )
    def test_references_include_standard_specific_bibliography(
        self,
        standard,
        expected_reference,
        example_result,
    ):
        html = DesignCodeReport(standard=standard, result=example_result).generate()
        assert expected_reference in html


class TestSummaryStatusAndRecommendations:
    def test_pass_status_has_green_class(self, raw_damage_payload):
        payload = dict(raw_damage_payload)
        payload["total_damage"] = 0.20
        payload["dff"] = 3.0
        html = DesignCodeReport(standard="DNV-RP-C203", result=payload).generate()
        assert "PASS" in html
        assert "status-pass" in html
        assert "#1f7a3a" in html

    def test_fail_status_has_red_class(self, raw_damage_payload):
        payload = dict(raw_damage_payload)
        payload["total_damage"] = 0.60
        payload["dff"] = 3.0
        html = DesignCodeReport(standard="DNV-RP-C203", result=payload).generate()
        assert "FAIL" in html
        assert "status-fail" in html
        assert "#c62828" in html

    @pytest.mark.parametrize(
        ("damage", "expected_text"),
        [
            (0.05, "Structure is in good condition."),
            (0.30, "Increase inspection frequency."),
            (0.75, "Detailed inspection is required."),
            (1.20, "Immediate inspection is required."),
        ],
    )
    def test_recommendations_follow_damage_bands(self, damage, expected_text, raw_damage_payload):
        payload = dict(raw_damage_payload)
        payload["total_damage"] = damage
        html = DesignCodeReport(standard="BS 7608", result=payload).generate()
        assert expected_text in html


class TestConvenienceFunctionAndFileOutput:
    @pytest.mark.parametrize("standard", STANDARDS)
    def test_wrapper_function_returns_html(self, standard, example_result):
        html = generate_design_code_report(example_result, standard=standard)
        assert isinstance(html, str)
        assert "<html" in html.lower()

    @pytest.mark.parametrize("standard", STANDARDS)
    def test_wrapper_supports_metadata_kwargs(self, standard, raw_damage_payload):
        html = generate_design_code_report(
            raw_damage_payload,
            standard=standard,
            title="Code Report",
            company="Digital Model",
            project_ref="WRK-157",
            revision="B",
        )
        assert "Code Report" in html
        assert "Digital Model" in html
        assert "WRK-157" in html
        assert "Revision:</strong> B" in html

    def test_file_write_output(self, example_result):
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as handle:
            path = handle.name

        html = generate_design_code_report(
            example_result,
            standard="API RP 2A",
            output_path=path,
        )

        with open(path, encoding="utf-8") as infile:
            content = infile.read()

        assert "API RP 2A" in content
        assert "Plotly.newPlot" in content
        assert html == content

    def test_invalid_standard_raises_value_error(self, example_result):
        with pytest.raises(ValueError):
            DesignCodeReport(standard="NOT-A-STANDARD", result=example_result)
