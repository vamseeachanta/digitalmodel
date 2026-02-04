# ABOUTME: TDD tests for pipeline operational phases with multi-code utilisation analysis
# ABOUTME: Covers PipeDefinition, PipelinePhase, PhaseAnalysisRunner, and visualisation

"""Tests for wall_thickness_phases module — operational phases, multi-code comparison."""

import math

import numpy as np
import pandas as pd
import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    FabricationType,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
)
from digitalmodel.structural.analysis.wall_thickness_phases import (
    PipeDefinition,
    PipelinePhase,
    PhaseResult,
    PhaseComparisonResult,
    PhaseAnalysisRunner,
    compute_hydrostatic_pressure,
    create_standard_phases,
    plot_phase_utilisation_comparison,
    plot_phase_tm_interaction,
    generate_phase_summary_table,
    generate_phase_report,
)


# ---------------------------------------------------------------------------
# Fixtures / helpers
# ---------------------------------------------------------------------------

def make_pipe():
    """10.75" OD, 21.4mm WT, X65 steel — standard test pipe."""
    return PipeDefinition(
        outer_diameter=0.27305,
        wall_thickness=0.0214,
        grade="X65",
        smys=448e6,
        smts=531e6,
    )


def make_thick_pipe():
    """12.75" OD, 25.4mm WT, X65 — thicker pipe for deep water."""
    return PipeDefinition(
        outer_diameter=0.32385,
        wall_thickness=0.0254,
        grade="X65",
        smys=448e6,
        smts=531e6,
    )


# ===================================================================
# Step 1 — Data models + hydrostatic pressure
# ===================================================================


class TestComputeHydrostaticPressure:
    def test_zero_depth_gives_zero_pressure(self):
        assert compute_hydrostatic_pressure(0.0) == 0.0

    def test_500m_depth_default_seawater(self):
        # Pe = rho * g * d = 1025 * 9.80665 * 500
        expected = 1025.0 * 9.80665 * 500.0
        result = compute_hydrostatic_pressure(500.0)
        assert result == pytest.approx(expected, rel=1e-6)

    def test_custom_density_and_gravity(self):
        result = compute_hydrostatic_pressure(100.0, rho=1000.0, g=10.0)
        assert result == pytest.approx(1_000_000.0, rel=1e-9)

    def test_negative_depth_raises(self):
        with pytest.raises(ValueError, match="depth"):
            compute_hydrostatic_pressure(-10.0)


class TestPipeDefinition:
    def test_valid_construction(self):
        p = make_pipe()
        assert p.outer_diameter == 0.27305
        assert p.wall_thickness == 0.0214
        assert p.grade == "X65"
        assert p.smys == 448e6
        assert p.smts == 531e6

    def test_default_optional_fields(self):
        p = make_pipe()
        assert p.corrosion_allowance == 0.0
        assert p.fabrication_tolerance == 0.125
        assert p.fabrication_type == FabricationType.SEAMLESS

    def test_to_geometry_returns_pipe_geometry(self):
        p = make_pipe()
        g = p.to_geometry()
        assert isinstance(g, PipeGeometry)
        assert g.outer_diameter == p.outer_diameter
        assert g.wall_thickness == p.wall_thickness
        assert g.corrosion_allowance == p.corrosion_allowance
        assert g.fabrication_tolerance == p.fabrication_tolerance

    def test_to_material_returns_pipe_material(self):
        p = make_pipe()
        m = p.to_material()
        assert isinstance(m, PipeMaterial)
        assert m.grade == "X65"
        assert m.smys == 448e6
        assert m.smts == 531e6
        assert m.fabrication_type == FabricationType.SEAMLESS

    def test_custom_corrosion_allowance(self):
        p = PipeDefinition(
            outer_diameter=0.27305,
            wall_thickness=0.0214,
            grade="X65",
            smys=448e6,
            smts=531e6,
            corrosion_allowance=0.003,
        )
        g = p.to_geometry()
        assert g.corrosion_allowance == 0.003

    def test_invalid_od_raises(self):
        with pytest.raises(ValueError):
            PipeDefinition(
                outer_diameter=-0.1,
                wall_thickness=0.02,
                grade="X65",
                smys=448e6,
                smts=531e6,
            )

    def test_wt_exceeds_half_od_raises(self):
        with pytest.raises(ValueError):
            PipeDefinition(
                outer_diameter=0.1,
                wall_thickness=0.06,
                grade="X65",
                smys=448e6,
                smts=531e6,
            )


class TestPipelinePhase:
    def test_frozen_dataclass(self):
        phase = PipelinePhase(
            name="Installation (empty)",
            internal_pressure=0.0,
            external_pressure=5e6,
            bending_moment=100e3,
            effective_tension=50e3,
            description="Empty pipe during installation",
        )
        with pytest.raises(AttributeError):
            phase.name = "changed"

    def test_to_design_loads(self):
        phase = PipelinePhase(
            name="Test",
            internal_pressure=10e6,
            external_pressure=5e6,
            bending_moment=200e3,
            effective_tension=80e3,
        )
        loads = phase.to_design_loads()
        assert loads.internal_pressure == 10e6
        assert loads.external_pressure == 5e6
        assert loads.bending_moment == 200e3
        assert loads.effective_tension == 80e3

    def test_default_description(self):
        phase = PipelinePhase(
            name="Op",
            internal_pressure=0.0,
            external_pressure=0.0,
            bending_moment=0.0,
            effective_tension=0.0,
        )
        assert phase.description == ""


# ===================================================================
# Step 2 — Phase factory
# ===================================================================


class TestCreateStandardPhases:
    def test_returns_five_phases(self):
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        assert len(phases) == 5

    def test_phase_names(self):
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        names = [p.name for p in phases]
        assert "Installation (empty)" in names
        assert "Installation (water-filled)" in names
        assert "Hydrotest" in names
        assert "Operation" in names
        assert "Shutdown" in names

    def test_external_pressure_matches_hydrostatic(self):
        depth = 500.0
        phases = create_standard_phases(water_depth=depth, design_pressure=10e6)
        pe_expected = compute_hydrostatic_pressure(depth)
        for phase in phases:
            assert phase.external_pressure == pytest.approx(pe_expected, rel=1e-6)

    def test_installation_empty_has_zero_internal_pressure(self):
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        install_empty = [p for p in phases if p.name == "Installation (empty)"][0]
        assert install_empty.internal_pressure == 0.0

    def test_installation_water_filled_pi_approx_pe(self):
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        install_wf = [p for p in phases if p.name == "Installation (water-filled)"][0]
        pe = compute_hydrostatic_pressure(500.0)
        # Water-filled: Pi ≈ Pe (flooded pipe)
        assert install_wf.internal_pressure == pytest.approx(pe, rel=1e-6)

    def test_hydrotest_pressure_is_125_percent_maop(self):
        dp = 10e6
        phases = create_standard_phases(water_depth=500.0, design_pressure=dp)
        hydrotest = [p for p in phases if p.name == "Hydrotest"][0]
        assert hydrotest.internal_pressure == pytest.approx(1.25 * dp, rel=1e-6)

    def test_operation_has_design_pressure(self):
        dp = 10e6
        phases = create_standard_phases(water_depth=500.0, design_pressure=dp)
        operation = [p for p in phases if p.name == "Operation"][0]
        assert operation.internal_pressure == pytest.approx(dp, rel=1e-6)

    def test_shutdown_has_zero_internal_pressure(self):
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        shutdown = [p for p in phases if p.name == "Shutdown"][0]
        assert shutdown.internal_pressure == 0.0
        assert shutdown.bending_moment == 0.0
        assert shutdown.effective_tension == 0.0

    def test_custom_installation_loads(self):
        phases = create_standard_phases(
            water_depth=500.0,
            design_pressure=10e6,
            install_bending_moment=150e3,
            install_effective_tension=60e3,
        )
        install_empty = [p for p in phases if p.name == "Installation (empty)"][0]
        assert install_empty.bending_moment == 150e3
        assert install_empty.effective_tension == 60e3

    def test_custom_operational_loads(self):
        phases = create_standard_phases(
            water_depth=500.0,
            design_pressure=10e6,
            oper_bending_moment=80e3,
            oper_effective_tension=40e3,
        )
        operation = [p for p in phases if p.name == "Operation"][0]
        assert operation.bending_moment == 80e3
        assert operation.effective_tension == 40e3


# ===================================================================
# Step 3 — PhaseAnalysisRunner
# ===================================================================


class TestPhaseAnalysisRunner:
    def test_single_code_returns_correct_result_count(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        runner = PhaseAnalysisRunner(
            pipe=pipe,
            phases=phases,
            codes=[DesignCode.DNV_ST_F101],
        )
        comparison = runner.run()
        # 5 phases × 1 code = 5 results
        assert len(comparison.results) == 5

    def test_multi_code_returns_correct_result_count(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        runner = PhaseAnalysisRunner(
            pipe=pipe,
            phases=phases,
            codes=[DesignCode.DNV_ST_F101, DesignCode.API_RP_1111],
        )
        comparison = runner.run()
        # 5 phases × 2 codes = 10 results
        assert len(comparison.results) == 10

    def test_result_contains_phase_names(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        runner = PhaseAnalysisRunner(
            pipe=pipe,
            phases=phases,
            codes=[DesignCode.DNV_ST_F101],
        )
        comparison = runner.run()
        result_phases = {r.phase_name for r in comparison.results}
        assert "Installation (empty)" in result_phases
        assert "Operation" in result_phases

    def test_result_contains_correct_codes(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        runner = PhaseAnalysisRunner(
            pipe=pipe,
            phases=phases,
            codes=[DesignCode.DNV_ST_F101, DesignCode.API_RP_1111],
        )
        comparison = runner.run()
        result_codes = {r.code for r in comparison.results}
        assert DesignCode.DNV_ST_F101 in result_codes
        assert DesignCode.API_RP_1111 in result_codes

    def test_each_result_has_wall_thickness_result(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        runner = PhaseAnalysisRunner(
            pipe=pipe,
            phases=phases,
            codes=[DesignCode.DNV_ST_F101],
        )
        comparison = runner.run()
        for r in comparison.results:
            assert r.wt_result is not None
            assert r.wt_result.max_utilisation >= 0.0

    def test_custom_safety_class(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        runner = PhaseAnalysisRunner(
            pipe=pipe,
            phases=phases,
            codes=[DesignCode.DNV_ST_F101],
            safety_class=SafetyClass.HIGH,
        )
        comparison = runner.run()
        # Higher safety class → higher utilisation
        assert len(comparison.results) == 5


class TestPhaseComparisonResult:
    def test_to_dataframe_shape(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        runner = PhaseAnalysisRunner(
            pipe=pipe,
            phases=phases,
            codes=[DesignCode.DNV_ST_F101, DesignCode.API_RP_1111],
        )
        comparison = runner.run()
        df = comparison.to_dataframe()
        assert isinstance(df, pd.DataFrame)
        assert len(df) > 0
        # Must have phase, code, check, utilisation columns
        assert "phase" in df.columns
        assert "code" in df.columns
        assert "check" in df.columns
        assert "utilisation" in df.columns

    def test_to_dataframe_has_all_phases_and_codes(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        runner = PhaseAnalysisRunner(
            pipe=pipe,
            phases=phases,
            codes=[DesignCode.DNV_ST_F101, DesignCode.API_RP_1111],
        )
        comparison = runner.run()
        df = comparison.to_dataframe()
        assert set(df["phase"].unique()) == {p.name for p in phases}
        assert len(df["code"].unique()) == 2

    def test_summary_dataframe_one_row_per_phase_code(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        codes = [DesignCode.DNV_ST_F101, DesignCode.API_RP_1111]
        runner = PhaseAnalysisRunner(pipe=pipe, phases=phases, codes=codes)
        comparison = runner.run()
        summary = comparison.summary_dataframe()
        assert isinstance(summary, pd.DataFrame)
        # 5 phases × 2 codes = 10 rows
        assert len(summary) == 10
        assert "max_utilisation" in summary.columns
        assert "governing_check" in summary.columns
        assert "is_safe" in summary.columns


# ===================================================================
# Step 4 — Visualisation + report
# ===================================================================


class TestPlotPhaseUtilisationComparison:
    def test_returns_plotly_figure(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        codes = [DesignCode.DNV_ST_F101, DesignCode.API_RP_1111]
        runner = PhaseAnalysisRunner(pipe=pipe, phases=phases, codes=codes)
        comparison = runner.run()
        import plotly.graph_objects as go

        fig = plot_phase_utilisation_comparison(comparison)
        assert isinstance(fig, go.Figure)

    def test_has_subplot_per_code(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        codes = [DesignCode.DNV_ST_F101, DesignCode.API_RP_1111]
        runner = PhaseAnalysisRunner(pipe=pipe, phases=phases, codes=codes)
        comparison = runner.run()
        fig = plot_phase_utilisation_comparison(comparison)
        # Figure should have traces — at least one per code
        assert len(fig.data) > 0

    def test_single_code_works(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        codes = [DesignCode.DNV_ST_F101]
        runner = PhaseAnalysisRunner(pipe=pipe, phases=phases, codes=codes)
        comparison = runner.run()
        import plotly.graph_objects as go

        fig = plot_phase_utilisation_comparison(comparison)
        assert isinstance(fig, go.Figure)


class TestPlotPhaseTMInteraction:
    def test_returns_plotly_figure(self):
        pipe = make_pipe()
        phases = create_standard_phases(
            water_depth=500.0,
            design_pressure=10e6,
            install_bending_moment=100e3,
            oper_bending_moment=50e3,
        )
        codes = [DesignCode.DNV_ST_F101, DesignCode.API_RP_1111]
        import plotly.graph_objects as go

        fig = plot_phase_tm_interaction(pipe, phases, codes)
        assert isinstance(fig, go.Figure)

    def test_single_code_works(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        codes = [DesignCode.API_RP_1111]
        import plotly.graph_objects as go

        fig = plot_phase_tm_interaction(pipe, phases, codes)
        assert isinstance(fig, go.Figure)

    def test_custom_n_points(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        codes = [DesignCode.DNV_ST_F101]
        import plotly.graph_objects as go

        fig = plot_phase_tm_interaction(pipe, phases, codes, n_points=50)
        assert isinstance(fig, go.Figure)


class TestGeneratePhaseSummaryTable:
    def test_returns_dataframe(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        codes = [DesignCode.DNV_ST_F101, DesignCode.API_RP_1111]
        runner = PhaseAnalysisRunner(pipe=pipe, phases=phases, codes=codes)
        comparison = runner.run()
        df = generate_phase_summary_table(comparison)
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 10  # 5 phases × 2 codes

    def test_columns_present(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        codes = [DesignCode.DNV_ST_F101]
        runner = PhaseAnalysisRunner(pipe=pipe, phases=phases, codes=codes)
        comparison = runner.run()
        df = generate_phase_summary_table(comparison)
        for col in ["phase", "code", "max_utilisation", "governing_check", "is_safe"]:
            assert col in df.columns


class TestGeneratePhaseReport:
    def test_returns_html_string(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        codes = [DesignCode.DNV_ST_F101, DesignCode.API_RP_1111]
        runner = PhaseAnalysisRunner(pipe=pipe, phases=phases, codes=codes)
        comparison = runner.run()
        html = generate_phase_report(comparison)
        assert isinstance(html, str)
        assert "<html" in html.lower()

    def test_contains_summary_table(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        codes = [DesignCode.DNV_ST_F101]
        runner = PhaseAnalysisRunner(pipe=pipe, phases=phases, codes=codes)
        comparison = runner.run()
        html = generate_phase_report(comparison)
        assert "Installation (empty)" in html
        assert "Operation" in html

    def test_writes_to_file(self, tmp_path):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        codes = [DesignCode.DNV_ST_F101]
        runner = PhaseAnalysisRunner(pipe=pipe, phases=phases, codes=codes)
        comparison = runner.run()
        out = tmp_path / "report.html"
        html = generate_phase_report(comparison, output_path=str(out))
        assert out.exists()
        content = out.read_text()
        assert "<html" in content.lower()

    def test_contains_chart_divs(self):
        pipe = make_pipe()
        phases = create_standard_phases(water_depth=500.0, design_pressure=10e6)
        codes = [DesignCode.DNV_ST_F101, DesignCode.API_RP_1111]
        runner = PhaseAnalysisRunner(pipe=pipe, phases=phases, codes=codes)
        comparison = runner.run()
        html = generate_phase_report(comparison)
        # Plotly charts produce div elements
        assert "plotly" in html.lower()
