import pytest
from digitalmodel.solvers.orcaflex.reporting.renderers.base import BaseRenderer
from digitalmodel.solvers.orcaflex.reporting.renderers.riser import RiserRenderer
from digitalmodel.solvers.orcaflex.reporting.renderers.pipeline import PipelineRenderer
from digitalmodel.solvers.orcaflex.reporting.models.report import OrcaFlexAnalysisReport

@pytest.fixture
def empty_report():
    return OrcaFlexAnalysisReport(
        project_name="Test Project",
        structure_id="TEST-001",
        structure_type="riser"
    )

def test_base_renderer_sections():
    renderer = BaseRenderer()
    config = renderer.get_section_config()
    ids = [item["id"] for item in config]
    assert "executive-summary" in ids
    assert "geometry" in ids
    assert "design-checks" in ids

def test_riser_renderer_inheritance():
    renderer = RiserRenderer()
    assert isinstance(renderer, BaseRenderer)
    config = renderer.get_section_config()
    # Riser renderer should have same base sections but some might be wrapped
    ids = [item["id"] for item in config]
    assert "dynamic-results" in ids

def test_pipeline_renderer_sections():
    renderer = PipelineRenderer()
    config = renderer.get_section_config()
    ids = [item["id"] for item in config]
    assert "geometry" in ids
    # Specific pipeline checks would be inside the builders or added as new sections

def test_riser_renderer_with_tdp_excursion(empty_report):
    """RiserRenderer injects TDP excursion section when tdp_excursion_history is set."""
    from digitalmodel.solvers.orcaflex.reporting.models.results import (
        DynamicResultsData, TimeSeriesData
    )

    empty_report.dynamic_results = DynamicResultsData(
        ramp_end_time_s=100.0,
        time_series=[
            TimeSeriesData(
                id="tension-ts",
                label="Tension",
                t=[0.0, 50.0, 100.0, 150.0, 200.0],
                values=[1200.0, 1300.0, 1100.0, 1250.0, 1180.0],
                units="kN",
            )
        ],
        tdp_excursion_history=TimeSeriesData(
            id="tdp-excursion",
            label="TDP Excursion",
            t=[0.0, 50.0, 100.0, 150.0, 200.0],
            values=[0.0, 2.5, 5.1, 3.8, 1.2],
            units="m",
        ),
    )
    renderer = RiserRenderer()
    html_out = renderer.render(empty_report)
    assert "dynamic-tdp-excursion" in html_out
    assert "TDP X-Excursion Time History" in html_out


def test_pipeline_renderer_kp_chainage(empty_report):
    """PipelineRenderer injects KP chainage table when geometry has kp_chainage_table."""
    from digitalmodel.solvers.orcaflex.reporting.models.geometry import (
        GeometryData, LineProfileData
    )

    empty_report.structure_type = "pipeline"
    empty_report.geometry = GeometryData(
        water_depth_m=80.0,
        line_profile=LineProfileData(
            arc_length=[0.0, 500.0, 1000.0],
            x=[0.0, 500.0, 1000.0],
            y=[0.0, 0.0, 0.0],
            z=[-5.0, -40.0, -80.0],
        ),
        kp_chainage_table=[
            {"KP": 0.0, "E": 400000.0, "N": 6500000.0, "WD": 5.0},
            {"KP": 500.0, "E": 400500.0, "N": 6500000.0, "WD": 40.0},
            {"KP": 1000.0, "E": 401000.0, "N": 6500000.0, "WD": 80.0},
        ],
    )
    renderer = PipelineRenderer()
    html_out = renderer.render(empty_report)
    assert "geometry-kp-chainage" in html_out
    assert "KP Chainage Table" in html_out


def test_renderer_rendered_anchors(empty_report):
    renderer = BaseRenderer()
    html_out = renderer.render(empty_report)
    
    # Check for mandatory anchors from GEMINI.md contract
    mandatory_anchors = [
        "header", "executive-summary", "model-overview", "geometry", 
        "materials", "boundary-conditions", "mesh", "loads", 
        "analysis-setup", "static-results", "design-checks", 
        "summary", "appendices"
    ]
    for anchor in mandatory_anchors:
        assert f'id="{anchor}"' in html_out
