import pytest
from digitalmodel.solvers.orcaflex.reporting.models.report import OrcaFlexAnalysisReport
from digitalmodel.solvers.orcaflex.reporting.section_builders.header import _build_header_html
from digitalmodel.solvers.orcaflex.reporting.section_builders.executive_summary import _build_executive_summary_html
from digitalmodel.solvers.orcaflex.reporting.section_builders.geometry import _build_geometry_html
from digitalmodel.solvers.orcaflex.reporting.section_builders.mesh import _build_mesh_html
from digitalmodel.solvers.orcaflex.reporting.models.geometry import GeometryData, LineProfileData
from digitalmodel.solvers.orcaflex.reporting.models.mesh import MeshData, MeshQualityData

@pytest.fixture
def empty_report():
    return OrcaFlexAnalysisReport(
        project_name="Test Project",
        structure_id="TEST-001",
        structure_type="riser"
    )

def test_header_builder(empty_report):
    html_out = _build_header_html(empty_report)
    assert '<header id="header">' in html_out
    assert 'TEST-001' in html_out
    assert 'RISER' in html_out

def test_executive_summary_builder_empty(empty_report):
    html_out = _build_executive_summary_html(empty_report)
    assert html_out == ""

def test_geometry_builder_empty(empty_report):
    html_out = _build_geometry_html(empty_report)
    assert html_out == ""

def test_geometry_builder_with_data(empty_report):
    empty_report.geometry = GeometryData(
        line_profile=LineProfileData(
            arc_length=[0, 1], x=[0, 1], y=[0, 0], z=[0, 0]
        )
    )
    html_out = _build_geometry_html(empty_report)
    assert '<section id="geometry">' in html_out
    assert 'plotly-graph-div' in html_out
    assert 'geometry-3d' in html_out

def test_mesh_builder_with_data(empty_report):
    empty_report.mesh = MeshData(
        total_segment_count=10,
        quality=MeshQualityData(
            max_adjacent_ratio=1.1,
            worst_ratio_arc_length_m=5.0,
            verdict="PASS",
            adjacent_ratios=[1.1]*9
        )
    )
    html_out = _build_mesh_html(empty_report)
    assert '<section id="mesh">' in html_out
    assert 'mesh-segment-lengths' in html_out
    assert 'mesh-quality-table' in html_out
    assert 'PASS' in html_out

def test_design_checks_heatmap(empty_report):
    from digitalmodel.solvers.orcaflex.reporting.models.design_checks import DesignCheckData, UtilizationData
    from digitalmodel.solvers.orcaflex.reporting.section_builders.design_checks import _build_design_checks_html
    
    empty_report.design_checks = DesignCheckData(
        code="DNV-OS-F101",
        checks=[
            UtilizationData(name="Burst", uc=0.5, location_arc_m=0.0),
            UtilizationData(name="Burst", uc=0.6, location_arc_m=10.0),
            UtilizationData(name="Combined", uc=0.7, location_arc_m=0.0),
            UtilizationData(name="Combined", uc=0.8, location_arc_m=10.0),
        ]
    )
    
    html_out = _build_design_checks_html(empty_report)
    assert '<section id="design-checks">' in html_out
    assert 'plotly-graph-div' in html_out
    assert 'heatmap' in html_out.lower()

def test_xss_escaping_field_specific(empty_report):
    """Verify that field-specific strings are escaped (6 fields required by spec)."""
    from digitalmodel.solvers.orcaflex.reporting.models.design_checks import DesignCheckData, UtilizationData
    from digitalmodel.solvers.orcaflex.reporting.section_builders.design_checks import _build_design_checks_html
    from digitalmodel.solvers.orcaflex.reporting.section_builders.summary import _build_summary_html
    
    payload = "<script>alert(1)</script>"
    escaped = "&lt;script&gt;alert(1)&lt;/script&gt;"
    
    # 1. project_name
    empty_report.project_name = payload
    assert escaped in _build_header_html(empty_report)
    
    # 2. structure_id
    empty_report.structure_id = payload
    assert escaped in _build_header_html(empty_report)
    
    # 3. analyst
    empty_report.analyst = payload
    assert escaped in _build_header_html(empty_report)
    
    # 4. load_case label (UtilizationData.load_case)
    empty_report.design_checks = DesignCheckData(
        code="DNV",
        checks=[UtilizationData(name="Check", uc=0.5, load_case=payload)]
    )
    assert escaped in _build_design_checks_html(empty_report)
    
    # 5. UtilizationData.name
    empty_report.design_checks.checks[0].name = payload
    assert escaped in _build_design_checks_html(empty_report)
    
    # 6. Recommendation text
    empty_report.recommendations = [payload]
    assert escaped in _build_summary_html(empty_report)

def test_static_results_builder_with_data(empty_report):
    from digitalmodel.solvers.orcaflex.reporting.models.results import StaticResultsData
    from digitalmodel.solvers.orcaflex.reporting.section_builders.results_static import _build_static_results_html

    empty_report.static_results = StaticResultsData(
        end_tensions_kn={"End A": 1500.0, "End B": 1200.0},
        tdp_position_m=450.0,
        tension_profile={"arc_length": [0, 100, 200], "tension": [1500, 1200, 800]},
        bm_profile={"arc_length": [0, 100, 200], "bm": [50, 30, 10]},
    )
    html_out = _build_static_results_html(empty_report)
    assert '<section id="static-results">' in html_out
    assert "static-tension-profile" in html_out
    assert "static-bm-profile" in html_out
    assert "1500.0 kN" in html_out
    assert "450.0 m" in html_out


def test_dynamic_results_builder_with_data(empty_report):
    from digitalmodel.solvers.orcaflex.reporting.models.results import (
        DynamicResultsData, TimeSeriesData, EnvelopeData
    )
    from digitalmodel.solvers.orcaflex.reporting.section_builders.results_dynamic import _build_dynamic_results_html

    empty_report.dynamic_results = DynamicResultsData(
        ramp_end_time_s=100.0,
        time_series=[
            TimeSeriesData(
                id="tension-ts",
                label="Effective Tension",
                t=[0.0, 50.0, 100.0, 150.0, 200.0],
                values=[1200.0, 1300.0, 1100.0, 1250.0, 1180.0],
                units="kN",
            )
        ],
        envelopes=[
            EnvelopeData(
                id="tension-env",
                label="Effective Tension",
                arc_length=[0.0, 100.0, 200.0],
                max_values=[1400.0, 1200.0, 800.0],
                min_values=[900.0, 700.0, 400.0],
                units="kN",
            )
        ],
        statistical_summary=[{"Variable": "Max Tension", "MPM [kN]": "1380"}],
    )
    html_out = _build_dynamic_results_html(empty_report)
    assert '<section id="dynamic-results">' in html_out
    assert "plotly-graph-div" in html_out
    assert "dynamic-tension-ts" in html_out
    assert "dynamic-tension-env" in html_out
    assert "Max Tension" in html_out


def test_extreme_results_builder_with_data(empty_report):
    from digitalmodel.solvers.orcaflex.reporting.models.results import ExtremeResultsData
    from digitalmodel.solvers.orcaflex.reporting.section_builders.results_extreme import _build_extreme_results_html

    empty_report.extreme_results = ExtremeResultsData(
        mpm_values=[
            {"label": "Max Tension", "value": 1580.0},
            {"label": "Max Bending Moment", "value": 95.0},
        ],
        governing_load_case="100yr Storm",
        governing_location_arc_m=250.0,
    )
    html_out = _build_extreme_results_html(empty_report)
    assert '<section id="extreme-results">' in html_out
    assert "extreme-mpm-chart" in html_out
    assert "100yr Storm" in html_out
    assert "250.0 m" in html_out


def test_loads_builder_with_data(empty_report):
    from digitalmodel.solvers.orcaflex.reporting.models.loads import EnvironmentData, LoadCaseData
    from digitalmodel.solvers.orcaflex.reporting.section_builders.loads import _build_loads_html

    empty_report.loads = EnvironmentData(
        load_cases=[
            LoadCaseData(
                case_id="LC001",
                hs_m=4.5,
                tp_s=12.0,
                current_velocity_m_s=0.8,
                current_direction_deg=180.0,
            ),
            LoadCaseData(
                case_id="LC002",
                hs_m=8.0,
                tp_s=15.0,
                current_velocity_m_s=1.2,
                current_direction_deg=270.0,
            ),
        ]
    )
    html_out = _build_loads_html(empty_report)
    assert '<section id="loads">' in html_out
    assert "loads-case-table" in html_out
    assert "LC001" in html_out
    assert "4.50" in html_out


def test_fatigue_builder_with_data(empty_report):
    from digitalmodel.solvers.orcaflex.reporting.models.fatigue import FatigueResultsData
    from digitalmodel.solvers.orcaflex.reporting.section_builders.fatigue import _build_fatigue_html

    empty_report.fatigue = FatigueResultsData(
        method="Time-domain rainflow",
        sn_curve="DNV F3",
        scf=1.15,
        design_life_yrs=25.0,
        max_damage=0.042,
        max_damage_location_arc_m=180.0,
        damage_per_node={
            "arc_length": [0, 50, 100, 150, 200],
            "damage_yr": [0.001, 0.010, 0.042, 0.020, 0.005],
        },
    )
    html_out = _build_fatigue_html(empty_report)
    assert '<section id="fatigue">' in html_out
    assert "fatigue-metadata" in html_out
    assert "Time-domain rainflow" in html_out
    assert "DNV F3" in html_out
    assert "fatigue-damage-profile" in html_out
    assert "plotly-graph-div" in html_out


def test_materials_builder_with_data(empty_report):
    from digitalmodel.solvers.orcaflex.reporting.models.materials import MaterialData, LineTypeData
    from digitalmodel.solvers.orcaflex.reporting.section_builders.materials import _build_materials_html

    empty_report.materials = MaterialData(
        line_types=[
            LineTypeData(
                name="8in Pipe",
                od=0.2191,
                id=0.2032,
                wt=0.0318,
                grade="X65",
                smys_mpa=448.0,
                youngs_modulus_mpa=207000.0,
                density_kg_m3=7850.0,
                ea_kn=14000.0,
                ei_knm2=850.0,
            )
        ]
    )
    html_out = _build_materials_html(empty_report)
    assert '<section id="materials">' in html_out
    assert "materials-linetype-table" in html_out
    assert "8in Pipe" in html_out
    assert "materials-section-props" in html_out
    assert "plotly-graph-div" in html_out


def test_boundary_conditions_builder_with_data(empty_report):
    from digitalmodel.solvers.orcaflex.reporting.models.boundary_conditions import (
        BCData, BCEndData, SeabedModelData
    )
    from digitalmodel.solvers.orcaflex.reporting.section_builders.boundary_conditions import (
        _build_boundary_conditions_html
    )

    empty_report.boundary_conditions = BCData(
        end_a=BCEndData(
            name="Hang-off",
            type="Vessel",
            x=0.0,
            y=0.0,
            z=75.0,
            connected_to="Production Vessel",
        ),
        end_b=BCEndData(name="Touchdown", type="Pinned", x=450.0, y=0.0, z=-100.0),
        seabed=SeabedModelData(
            type="Linear",
            stiffness_kn_m2=100.0,
            friction_axial=0.3,
            friction_lateral=0.5,
        ),
    )
    html_out = _build_boundary_conditions_html(empty_report)
    assert '<section id="boundary-conditions">' in html_out
    assert "bc-end-table" in html_out
    assert "Hang-off" in html_out
    assert "bc-seabed-table" in html_out
    assert "Linear" in html_out


def test_other_structures_builder_with_data(empty_report):
    from digitalmodel.solvers.orcaflex.reporting.models.other_structures import (
        OtherStructuresData, AttachedStructureData
    )
    from digitalmodel.solvers.orcaflex.reporting.section_builders.other_structures import (
        _build_other_structures_html
    )

    empty_report.other_structures = OtherStructuresData(
        attached_structures=[
            AttachedStructureData(
                name="Buoy Module 1",
                type="Buoyancy Module",
                mass_kg=450.0,
                arc_length_m=100.0,
                x=5.0,
                y=0.0,
                z=-20.0,
            )
        ],
        minimum_clearance_m=2.5,
    )
    html_out = _build_other_structures_html(empty_report)
    assert '<section id="other-structures">' in html_out
    assert "Buoy Module 1" in html_out
    assert "2.50 m" in html_out


def test_xss_global_literal_search(empty_report):
    """
    Global XSS test: inject payload into multiple fields and verify raw literal 
    is ABSENT from full HTML output. Legitimate <script> tags may exist.
    """
    from digitalmodel.solvers.orcaflex.reporting import generate_orcaflex_report
    from digitalmodel.solvers.orcaflex.reporting.models.design_checks import DesignCheckData, UtilizationData
    from digitalmodel.solvers.orcaflex.reporting.models.loads import EnvironmentData, LoadCaseData
    from digitalmodel.solvers.orcaflex.reporting.models.analysis import AnalysisSetupData, SolverSettingsData
    from digitalmodel.solvers.orcaflex.reporting.models.fatigue import FatigueResultsData
    from digitalmodel.solvers.orcaflex.reporting.models.results import StaticResultsData
    
    payload = "<script>alert(1)</script>"
    
    # Inject into enumerated fields from spec + more for coverage
    empty_report.project_name = payload
    empty_report.structure_id = payload
    empty_report.analyst = payload
    empty_report.analysis_ref = payload
    empty_report.orcaflex_version = payload
    empty_report.design_codes = [payload]
    empty_report.summary_notes = payload
    
    empty_report.loads = EnvironmentData(
        load_cases=[LoadCaseData(case_id=payload, hs_m=1.0, tp_s=10.0)]
    )
    empty_report.design_checks = DesignCheckData(
        code=payload,
        checks=[UtilizationData(name=payload, uc=0.5, load_case=payload)]
    )
    empty_report.recommendations = [payload]
    
    empty_report.analysis_setup = AnalysisSetupData(
        analysis_types=payload,
        safety_class=payload,
        location_class=payload,
        input_files=payload,
        solver_settings=SolverSettingsData(
            static_convergence_criterion=1e-7,
            static_max_iterations=100,
            dynamic_time_step_s=0.1,
            ramp_duration_s=10.0,
            simulation_duration_s=100.0,
            orcaflex_version=payload,
            python_api_version=payload,
            wave_theory=payload,
            damping_model=payload
        )
    )
    
    empty_report.fatigue = FatigueResultsData(
        method=payload,
        sn_curve=payload,
        design_life_yrs=25.0
    )
    
    empty_report.static_results = StaticResultsData(
        end_tensions_kn={payload: 1000.0}
    )
    
    # Generate report to a string (using a dummy path)
    from digitalmodel.solvers.orcaflex.reporting.renderers.base import BaseRenderer
    renderer = BaseRenderer(include_plotlyjs=False) # No plotly to minimize noise
    html_out = renderer.render(empty_report)
    
    # Assert raw literal is ABSENT (Plotly <script> tags are okay, but our payload is not)
    assert payload not in html_out
    # Assert escaped form is PRESENT
    assert "&lt;script&gt;alert(1)&lt;/script&gt;" in html_out
