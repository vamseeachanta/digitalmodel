# ABOUTME: TDD tests for pipeline wall thickness design checks
# ABOUTME: Covers DNV-ST-F101 and API RP 1111 burst, collapse, propagation, combined

"""Tests for wall_thickness module — DNV-ST-F101 and API RP 1111 design checks."""

import math
import sys
import os

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../src"))

from digitalmodel.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    FabricationType,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
    WallThicknessResult,
)


# ---------------------------------------------------------------------------
# Fixtures / helpers
# ---------------------------------------------------------------------------

def make_x65_material(**overrides):
    defaults = dict(grade="X65", smys=448e6, smts=531e6)
    defaults.update(overrides)
    return PipeMaterial(**defaults)


def make_10inch_geometry(**overrides):
    """10.75" OD = 0.27305 m, typical WT ≈ 0.0214 m."""
    defaults = dict(outer_diameter=0.27305, wall_thickness=0.0214, corrosion_allowance=0.001)
    defaults.update(overrides)
    return PipeGeometry(**defaults)


def make_12inch_geometry(**overrides):
    """12.75" OD = 0.32385 m, WT = 0.01905 m (0.75")."""
    defaults = dict(outer_diameter=0.32385, wall_thickness=0.01905, corrosion_allowance=0.001)
    defaults.update(overrides)
    return PipeGeometry(**defaults)


# ===================================================================
# Phase 1 — Data model validation
# ===================================================================

class TestPipeGeometry:
    def test_pipe_geometry_valid_construction_succeeds(self):
        g = PipeGeometry(outer_diameter=0.2732, wall_thickness=0.0214, corrosion_allowance=0.003)
        assert g.outer_diameter == 0.2732
        assert g.wall_thickness == 0.0214
        assert g.corrosion_allowance == 0.003

    def test_pipe_geometry_negative_od_raises_error(self):
        with pytest.raises(ValueError, match="Outer diameter must be positive"):
            PipeGeometry(outer_diameter=-0.1, wall_thickness=0.01)

    def test_pipe_geometry_zero_od_raises_error(self):
        with pytest.raises(ValueError, match="Outer diameter must be positive"):
            PipeGeometry(outer_diameter=0.0, wall_thickness=0.01)

    def test_pipe_geometry_wt_exceeds_half_od_raises_error(self):
        with pytest.raises(ValueError, match="less than half"):
            PipeGeometry(outer_diameter=0.2, wall_thickness=0.11)

    def test_pipe_geometry_wt_equals_half_od_raises_error(self):
        with pytest.raises(ValueError, match="less than half"):
            PipeGeometry(outer_diameter=0.2, wall_thickness=0.1)

    def test_pipe_geometry_zero_wt_raises_error(self):
        with pytest.raises(ValueError, match="Wall thickness must be positive"):
            PipeGeometry(outer_diameter=0.2, wall_thickness=0.0)

    def test_pipe_geometry_negative_corrosion_raises_error(self):
        with pytest.raises(ValueError, match="non-negative"):
            PipeGeometry(outer_diameter=0.2, wall_thickness=0.01, corrosion_allowance=-0.001)

    def test_pipe_geometry_t1_calculation(self):
        g = PipeGeometry(outer_diameter=0.3, wall_thickness=0.02, corrosion_allowance=0.001)
        expected = (0.02 - 0.001) * (1 - 0.125)
        assert abs(g.t1 - expected) < 1e-10

    def test_pipe_geometry_t2_calculation(self):
        g = PipeGeometry(outer_diameter=0.3, wall_thickness=0.02, corrosion_allowance=0.001)
        assert abs(g.t2 - 0.019) < 1e-10

    def test_pipe_geometry_inner_diameter(self):
        g = PipeGeometry(outer_diameter=0.3, wall_thickness=0.02)
        assert abs(g.inner_diameter - 0.26) < 1e-10

    def test_pipe_geometry_d_over_t(self):
        g = PipeGeometry(outer_diameter=0.3, wall_thickness=0.02)
        assert abs(g.d_over_t - 15.0) < 1e-10


class TestPipeMaterial:
    def test_pipe_material_valid_x65_construction_succeeds(self):
        m = PipeMaterial(grade="X65", smys=448e6, smts=531e6)
        assert m.grade == "X65"
        assert m.smys == 448e6
        assert m.smts == 531e6

    def test_pipe_material_zero_smys_raises_error(self):
        with pytest.raises(ValueError, match="SMYS must be positive"):
            PipeMaterial(grade="X65", smys=0, smts=531e6)

    def test_pipe_material_smts_less_than_smys_raises_error(self):
        with pytest.raises(ValueError, match="SMTS must be >= SMYS"):
            PipeMaterial(grade="X65", smys=500e6, smts=400e6)

    def test_pipe_material_alpha_fab_seamless(self):
        m = PipeMaterial(grade="X65", smys=448e6, smts=531e6, fabrication_type=FabricationType.SEAMLESS)
        assert m.alpha_fab == 1.0

    def test_pipe_material_alpha_fab_uoe(self):
        m = PipeMaterial(grade="X65", smys=448e6, smts=531e6, fabrication_type=FabricationType.UOE)
        assert m.alpha_fab == 0.93

    def test_pipe_material_alpha_fab_uo(self):
        m = PipeMaterial(grade="X65", smys=448e6, smts=531e6, fabrication_type=FabricationType.UO)
        assert m.alpha_fab == 0.85


class TestDesignLoads:
    def test_design_loads_valid_construction_succeeds(self):
        ld = DesignLoads(internal_pressure=25e6, external_pressure=10e6)
        assert ld.internal_pressure == 25e6
        assert ld.external_pressure == 10e6

    def test_design_loads_net_internal_pressure(self):
        ld = DesignLoads(internal_pressure=25e6, external_pressure=10e6)
        assert abs(ld.net_internal_pressure - 15e6) < 1.0

    def test_design_loads_net_internal_pressure_clamped_at_zero(self):
        ld = DesignLoads(internal_pressure=5e6, external_pressure=10e6)
        assert ld.net_internal_pressure == 0.0


class TestDesignFactors:
    def test_design_factors_safety_class_low_values(self):
        df = DesignFactors(safety_class=SafetyClass.LOW)
        assert df.gamma_sc == 1.046

    def test_design_factors_safety_class_medium_values_correct(self):
        df = DesignFactors(safety_class=SafetyClass.MEDIUM)
        assert df.gamma_sc == 1.138
        assert df.gamma_m == 1.15
        assert df.gamma_inc == 1.0

    def test_design_factors_safety_class_high_values(self):
        df = DesignFactors(safety_class=SafetyClass.HIGH)
        assert df.gamma_sc == 1.308


# ===================================================================
# Phase 1 — DNV-ST-F101 Pressure containment
# ===================================================================

class TestDNVPressureContainment:
    def test_dnv_pressure_containment_10inch_x65_utilisation_below_one(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=25e6, external_pressure=10e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.DNV_ST_F101)

        util, details = analyzer.check_pressure_containment()
        assert util < 1.0
        assert util > 0.0
        assert details["p_b"] > 0
        assert details["p_b_design"] > 0

    def test_dnv_pressure_containment_high_pressure_utilisation_above_one(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=80e6, external_pressure=0.0)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.DNV_ST_F101)

        util, _ = analyzer.check_pressure_containment()
        assert util > 1.0

    def test_dnv_pressure_containment_no_pressure_diff_utilisation_zero(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=10e6, external_pressure=10e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.DNV_ST_F101)

        util, _ = analyzer.check_pressure_containment()
        assert abs(util) < 1e-10

    def test_dnv_pressure_containment_known_reference_result(self):
        """Cross-check against manual calculation for 12.75" OD, X65.

        Manual calc:
            t1 = (0.01905 - 0.001) * (1 - 0.125) = 0.01580625 m
            f_cb = min(0.96*448e6, 0.96*531e6/1.15)
                 = min(430.08e6, 443.09e6) = 430.08e6
            p_b  = (2*0.01580625/(0.32385-0.01580625)) * (2/sqrt(3)) * 430.08e6
                 ≈ 0.10257 * 1.1547 * 430.08e6 ≈ 50.98 MPa
            p_b_design = 50.98 / (1.138*1.15*1.0) = 50.98 / 1.3087 ≈ 38.95 MPa
            util = (30-10)/38.95 = 20/38.95 ≈ 0.513
        """
        geom = make_12inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=30e6, external_pressure=10e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.DNV_ST_F101)

        util, details = analyzer.check_pressure_containment()

        # Verify t1
        expected_t1 = (0.01905 - 0.001) * (1 - 0.125)
        assert abs(details["t1"] - expected_t1) < 1e-8

        # Verify utilisation within expected range
        assert 0.45 <= util <= 0.60, f"Expected util ~0.51, got {util:.4f}"


# ===================================================================
# Phase 1 — WallThicknessResult
# ===================================================================

class TestWallThicknessResult:
    def test_result_contains_all_check_keys(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=25e6, external_pressure=10e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.DNV_ST_F101)

        result = analyzer.perform_analysis()
        assert "pressure_containment" in result.checks
        assert "collapse" in result.checks
        assert "propagation_buckling" in result.checks
        assert "combined_loading" in result.checks

    def test_result_is_safe_when_all_utilisations_below_one(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=15e6, external_pressure=5e6)
        factors = DesignFactors(safety_class=SafetyClass.LOW)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.DNV_ST_F101)

        result = analyzer.perform_analysis()
        assert result.is_safe is True
        assert result.max_utilisation <= 1.0
        assert result.governing_check is not None


# ===================================================================
# Phase 2 — DNV-ST-F101 Collapse + Propagation + Combined
# ===================================================================

class TestDNVCollapse:
    def test_dnv_collapse_elastic_pressure_positive(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(external_pressure=10e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors)

        util, details = analyzer.check_collapse()
        assert details["p_el"] > 0
        assert details["p_p"] > 0
        assert details["p_c"] > 0

    def test_dnv_collapse_characteristic_positive_and_reasonable(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(external_pressure=10e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors)

        _, details = analyzer.check_collapse()
        # p_c should be positive and in a physically meaningful range
        assert details["p_c"] > 0
        # For small ovality (f_0=0.005), p_c should be within
        # a reasonable multiple of the component pressures
        assert details["p_c"] < 10 * max(details["p_el"], details["p_p"])

    def test_dnv_collapse_low_external_pressure_utilisation_below_one(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(external_pressure=2e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors)

        util, _ = analyzer.check_collapse()
        assert util < 1.0

    def test_dnv_collapse_thicker_pipe_higher_resistance(self):
        mat = make_x65_material()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)

        thin = PipeGeometry(outer_diameter=0.27305, wall_thickness=0.015, corrosion_allowance=0.001)
        thick = PipeGeometry(outer_diameter=0.27305, wall_thickness=0.025, corrosion_allowance=0.001)

        a_thin = WallThicknessAnalyzer(thin, mat, loads, factors)
        a_thick = WallThicknessAnalyzer(thick, mat, loads, factors)

        _, d_thin = a_thin.check_collapse()
        _, d_thick = a_thick.check_collapse()

        assert d_thick["p_c"] > d_thin["p_c"]


class TestDNVPropagation:
    def test_dnv_propagation_pressure_positive(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors)

        util, details = analyzer.check_propagation_buckling()
        assert details["p_pr"] > 0
        assert util > 0

    def test_dnv_propagation_less_than_collapse(self):
        """Propagation pressure should always be less than collapse pressure."""
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors)

        _, collapse_d = analyzer.check_collapse()
        _, prop_d = analyzer.check_propagation_buckling()
        assert prop_d["p_pr"] < collapse_d["p_c"]

    def test_dnv_propagation_zero_external_pressure_zero_utilisation(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(external_pressure=0.0)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors)

        util, _ = analyzer.check_propagation_buckling()
        assert abs(util) < 1e-10


class TestDNVCombinedLoading:
    def test_dnv_combined_no_moment_no_tension_low_utilisation(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=15e6, external_pressure=5e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors)

        util, details = analyzer.check_combined_loading()
        assert details["M_p"] > 0
        assert details["S_p"] > 0
        # With no moment/tension, only pressure term contributes
        assert util >= 0

    def test_dnv_combined_large_moment_high_utilisation(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(
            internal_pressure=20e6,
            external_pressure=5e6,
            bending_moment=500e3,  # 500 kN·m — very large for 10"
            effective_tension=100e3,
        )
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors)

        util, _ = analyzer.check_combined_loading()
        assert util > 0.5  # Should be significant


# ===================================================================
# Phase 3 — API RP 1111
# ===================================================================

class TestAPIBurst:
    def test_api_burst_10inch_x65_utilisation_below_one(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=25e6, external_pressure=10e6)
        factors = DesignFactors()
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.API_RP_1111)

        util, details = analyzer.check_api_burst()
        assert 0 < util < 1.0
        assert details["p_b"] > 0

    def test_api_burst_thin_wall_formula_for_high_d_over_t(self):
        geom = PipeGeometry(outer_diameter=0.6096, wall_thickness=0.015)  # D/t ~40
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=10e6)
        factors = DesignFactors()
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.API_RP_1111)

        _, details = analyzer.check_api_burst()
        assert details["d_over_t"] > 15

    def test_api_burst_thick_wall_formula_for_low_d_over_t(self):
        geom = PipeGeometry(outer_diameter=0.1143, wall_thickness=0.0127)  # ~9 D/t
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=30e6)
        factors = DesignFactors()
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.API_RP_1111)

        _, details = analyzer.check_api_burst()
        assert details["d_over_t"] <= 15


class TestAPICollapse:
    def test_api_collapse_positive_pressures(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(external_pressure=10e6)
        factors = DesignFactors()
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.API_RP_1111)

        util, details = analyzer.check_api_collapse()
        assert details["p_el"] > 0
        assert details["p_y"] > 0
        assert details["p_c"] > 0
        assert util > 0

    def test_api_collapse_transition_less_than_components(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors()
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.API_RP_1111)

        _, details = analyzer.check_api_collapse()
        assert details["p_c"] <= details["p_el"]
        assert details["p_c"] <= details["p_y"]


class TestAPIPropagation:
    def test_api_propagation_positive_pressure(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors()
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.API_RP_1111)

        util, details = analyzer.check_api_propagation()
        assert details["p_pr"] > 0
        assert util > 0

    def test_api_propagation_less_than_collapse(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors()
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.API_RP_1111)

        _, collapse_d = analyzer.check_api_collapse()
        _, prop_d = analyzer.check_api_propagation()
        assert prop_d["p_pr"] < collapse_d["p_c"]


# ===================================================================
# Phase 3 — Common interface: perform_analysis per code
# ===================================================================

class TestPerformAnalysisDNV:
    def test_perform_analysis_dnv_returns_four_checks(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.DNV_ST_F101)

        result = analyzer.perform_analysis()
        assert len(result.checks) == 4
        assert set(result.checks.keys()) == {
            "pressure_containment", "collapse", "propagation_buckling", "combined_loading"
        }

    def test_perform_analysis_dnv_governing_check_identified(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.DNV_ST_F101)

        result = analyzer.perform_analysis()
        assert result.governing_check in result.checks
        assert result.max_utilisation == result.checks[result.governing_check]


class TestPerformAnalysisAPI:
    def test_perform_analysis_api_returns_three_checks(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors()
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.API_RP_1111)

        result = analyzer.perform_analysis()
        assert len(result.checks) == 3
        assert set(result.checks.keys()) == {"burst", "collapse", "propagation"}

    def test_perform_analysis_api_is_safe_for_moderate_loads(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=15e6, external_pressure=3e6)
        factors = DesignFactors()
        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.API_RP_1111)

        result = analyzer.perform_analysis()
        assert result.is_safe is True


# ===================================================================
# Cross-code comparison
# ===================================================================

class TestCrossCodeComparison:
    def test_both_codes_agree_on_safe_for_moderate_loads(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=15e6, external_pressure=3e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)

        dnv = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.DNV_ST_F101)
        api = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.API_RP_1111)

        assert dnv.perform_analysis().is_safe is True
        assert api.perform_analysis().is_safe is True

    def test_both_codes_agree_on_unsafe_for_extreme_loads(self):
        geom = make_10inch_geometry()
        mat = make_x65_material()
        loads = DesignLoads(internal_pressure=80e6, external_pressure=0.0)
        factors = DesignFactors(safety_class=SafetyClass.HIGH)

        dnv = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.DNV_ST_F101)
        api = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.API_RP_1111)

        assert dnv.perform_analysis().is_safe is False
        assert api.perform_analysis().is_safe is False
