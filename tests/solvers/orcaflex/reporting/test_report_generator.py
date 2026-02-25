import pytest
from pathlib import Path
from html.parser import HTMLParser
from digitalmodel.solvers.orcaflex.reporting import generate_orcaflex_report, OrcaFlexAnalysisReport
from digitalmodel.solvers.orcaflex.reporting.models import (
    GeometryData, LineProfileData, KeyPointData, DesignCheckData, UtilizationData
)

class ReportStructureParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.sections = {} # anchor_id -> {has_card, has_empty, title}
        self.current_section = None

    def handle_starttag(self, tag, attrs):
        attrs_dict = dict(attrs)
        if tag == "section" and "id" in attrs_dict:
            self.current_section = attrs_dict["id"]
            self.sections[self.current_section] = {"has_card": False, "has_empty": False}
        elif tag == "div" and self.current_section:
            if "section-card" in attrs_dict.get("class", ""):
                self.sections[self.current_section]["has_card"] = True
                if "section-empty" in attrs_dict["class"]:
                    self.sections[self.current_section]["has_empty"] = True

    def handle_endtag(self, tag):
        if tag == "section":
            self.current_section = None

def test_generate_basic_report(tmp_path):
    # Setup minimal data
    report_data = OrcaFlexAnalysisReport(
        project_name="Test Project",
        structure_id="TEST-001",
        structure_type="riser",
        recommendations=["Follow design codes", "Monitor TDP"]
    )
    
    # Add geometry
    arc_lengths = [0.0, 10.0, 20.0]
    report_data.geometry = GeometryData(
        water_depth_m=100.0,
        line_profile=LineProfileData(
            arc_length=arc_lengths,
            x=[0.0, 5.0, 10.0],
            y=[0.0, 0.0, 0.0],
            z=[0.0, -50.0, -100.0]
        ),
        key_points=[
            KeyPointData(label="Hang-off", arc_length_m=0.0, x=0.0, z=0.0),
            KeyPointData(label="TDP", arc_length_m=20.0, x=10.0, z=-100.0)
        ]
    )
    
    # Add design checks
    report_data.design_checks = DesignCheckData(
        code="API RP 2RD",
        checks=[
            UtilizationData(name="Max Tension", uc=0.85, load_case="100yr"),
            UtilizationData(name="Bending", uc=0.92, load_case="100yr")
        ]
    )
    
    output_file = tmp_path / "test_report.html"
    
    # Generate report
    result_path = generate_orcaflex_report(
        report_data,
        output_path=output_file
    )
    
    assert result_path.exists()
    
    with open(result_path, 'r', encoding='utf-8') as f:
        content = f.read()
        assert "<title>Test Project - TEST-001</title>" in content
        assert "2. Executive Summary" in content
        assert "4. Geometry" in content
        assert "Max Utilization" in content
        assert "0.920" in content  # UC value
        assert "badge-pass" in content  # Overall pass
        assert "100.0 m" in content # Water depth

        # Structural verification
        parser = ReportStructureParser()
        parser.feed(content)
        # All mandatory sections must be present and have a section-card
        for anchor in ["executive-summary", "geometry", "materials", "boundary-conditions", "mesh", "loads", "design-checks"]:
            assert anchor in parser.sections, f"Missing anchor: {anchor}"
            assert parser.sections[anchor]["has_card"], f"Anchor {anchor} missing section-card"

def test_generate_report_offline_mode(tmp_path):
    """include_plotlyjs=True embeds bundle inline — no CDN reference in output."""
    report_data = OrcaFlexAnalysisReport(
        project_name="Offline Test",
        structure_id="OFF-001",
        structure_type="riser",
    )
    output_file = tmp_path / "offline_report.html"
    result_path = generate_orcaflex_report(
        report_data, output_path=output_file, include_plotlyjs=True
    )

    assert result_path.exists()
    with open(result_path, "r", encoding="utf-8") as f:
        content = f.read()
    # CDN script tag must be absent; inline bundle tag must be present
    assert 'src="https://cdn.plot.ly/' not in content
    assert '<script type="text/javascript">' in content


def test_generate_report_invalid_structure_type(tmp_path):
    """Unknown structure_type falls back to BaseRenderer without raising."""
    report_data = OrcaFlexAnalysisReport(
        project_name="Fallback Test",
        structure_id="FALL-001",
        structure_type="unknown_type",
    )
    output_file = tmp_path / "fallback_report.html"
    result_path = generate_orcaflex_report(report_data, output_path=output_file)

    assert result_path.exists()
    with open(result_path, "r", encoding="utf-8") as f:
        content = f.read()
    assert "FALL-001" in content
    assert "Fallback Test" in content


def test_generate_report_creates_parent_directory(tmp_path):
    """Parent directory is created automatically if it does not exist."""
    report_data = OrcaFlexAnalysisReport(
        project_name="Dir Test",
        structure_id="DIR-001",
        structure_type="pipeline",
    )
    output_file = tmp_path / "nested" / "deep" / "report.html"
    assert not output_file.parent.exists()

    result_path = generate_orcaflex_report(report_data, output_path=output_file)

    assert result_path.exists()
    assert result_path.parent.exists()


def test_generate_report_no_data(tmp_path):
    report_data = OrcaFlexAnalysisReport(
        project_name="Empty Project",
        structure_id="EMPTY-001",
        structure_type="pipeline"
    )
    
    output_file = tmp_path / "empty_report.html"
    result_path = generate_orcaflex_report(report_data, output_path=output_file)
    
    assert result_path.exists()
    with open(result_path, 'r', encoding='utf-8') as f:
        content = f.read()
        assert "No data available" in content
        assert "4. Geometry" in content # Anchor should still be present
        assert "3. Model Overview" in content

        # Structural verification of empty states
        parser = ReportStructureParser()
        parser.feed(content)
        assert parser.sections["geometry"]["has_empty"]
        assert parser.sections["materials"]["has_empty"]
