"""Tests for pipeline pressure containment module (WRK-355).

TDD: tests written before implementation per mandatory TDD policy.

Covers:
- DNV-ST-F101: wall thickness sizing, system pressure test, MAOP, burst resistance
- API RP 1111: external collapse, propagating buckle
- Material library: X52, X60, X65, X70
- All pressures in MPa; dimensions in metres (SI throughout)
"""
from __future__ import annotations

import math

import pytest

from digitalmodel.subsea.pipeline.pipeline_pressure import (
    # Constants
    GAMMA_SC,
    GAMMA_M,
    GAMMA_INC,
    ALPHA_U,
    DF_COLLAPSE,
    DF_PROPAGATION,
    MATERIAL_LIBRARY,
    # Data classes
    PipeGeometry,
    PipeMaterial,
    # DNV-ST-F101 functions
    dnv_wall_thickness,
    dnv_burst_resistance,
    dnv_pressure_containment_check,
    dnv_system_pressure_test,
    dnv_maop,
    # API RP 1111 functions
    api_water_pressure,
    api_elastic_collapse_pressure,
    api_collapse_pressure,
    api_external_collapse_check,
    api_propagating_buckle_pressure,
    api_propagating_buckle_check,
)


# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def x65() -> PipeMaterial:
    """X65 material — SMYS 448 MPa, SMTS 531 MPa."""
    return PipeMaterial(name="X65", SMYS=448.0, SMTS=531.0)


@pytest.fixture
def x52() -> PipeMaterial:
    """X52 material — SMYS 358 MPa, SMTS 455 MPa."""
    return PipeMaterial(name="X52", SMYS=358.0, SMTS=455.0)


@pytest.fixture
def std_geom() -> PipeGeometry:
    """12-inch schedule 80 equivalent: OD=0.3239 m, WT=0.01905 m."""
    return PipeGeometry(OD=0.3239, WT=0.01905)


# ---------------------------------------------------------------------------
# Module-level constants
# ---------------------------------------------------------------------------

class TestModuleConstants:
    """Published design constants are fixed."""

    def test_gamma_sc_has_three_safety_classes(self):
        assert set(GAMMA_SC.keys()) == {"low", "normal", "high"}

    def test_gamma_sc_low(self):
        assert GAMMA_SC["low"] == pytest.approx(1.046, abs=1e-6)

    def test_gamma_sc_normal(self):
        assert GAMMA_SC["normal"] == pytest.approx(1.138, abs=1e-6)

    def test_gamma_sc_high(self):
        assert GAMMA_SC["high"] == pytest.approx(1.308, abs=1e-6)

    def test_gamma_m(self):
        """Material resistance factor per DNV-ST-F101."""
        assert GAMMA_M == pytest.approx(1.15, abs=1e-9)

    def test_gamma_inc(self):
        """Incidental-to-design pressure ratio."""
        assert GAMMA_INC == pytest.approx(1.1, abs=1e-9)

    def test_alpha_u(self):
        """Material strength factor (usage factor)."""
        assert ALPHA_U == pytest.approx(0.96, abs=1e-9)

    def test_df_collapse(self):
        """API RP 1111 collapse design factor."""
        assert DF_COLLAPSE == pytest.approx(0.7, abs=1e-9)

    def test_df_propagation(self):
        """API RP 1111 propagating buckle safety factor."""
        assert DF_PROPAGATION == pytest.approx(1.3, abs=1e-9)


# ---------------------------------------------------------------------------
# Material library
# ---------------------------------------------------------------------------

class TestMaterialLibrary:
    """MATERIAL_LIBRARY contains standard grades."""

    def test_x52_present(self):
        assert "X52" in MATERIAL_LIBRARY

    def test_x60_present(self):
        assert "X60" in MATERIAL_LIBRARY

    def test_x65_present(self):
        assert "X65" in MATERIAL_LIBRARY

    def test_x70_present(self):
        assert "X70" in MATERIAL_LIBRARY

    def test_x65_smys(self):
        m = MATERIAL_LIBRARY["X65"]
        assert m.SMYS == pytest.approx(448.0, abs=1.0)

    def test_x65_smts_gte_smys(self):
        m = MATERIAL_LIBRARY["X65"]
        assert m.SMTS >= m.SMYS

    def test_x70_smys_gt_x65_smys(self):
        assert MATERIAL_LIBRARY["X70"].SMYS > MATERIAL_LIBRARY["X65"].SMYS


# ---------------------------------------------------------------------------
# PipeGeometry dataclass
# ---------------------------------------------------------------------------

class TestPipeGeometry:
    """Geometry dataclass construction and validation."""

    def test_valid_geometry_created(self):
        geom = PipeGeometry(OD=0.3239, WT=0.01905)
        assert geom.OD == pytest.approx(0.3239)
        assert geom.WT == pytest.approx(0.01905)

    def test_od_must_be_positive(self):
        with pytest.raises(ValueError, match="OD"):
            PipeGeometry(OD=0.0, WT=0.01905)

    def test_wt_must_be_positive(self):
        with pytest.raises(ValueError, match="WT"):
            PipeGeometry(OD=0.3239, WT=0.0)

    def test_wt_less_than_od_over_2(self):
        """WT cannot equal or exceed the pipe radius."""
        with pytest.raises(ValueError):
            PipeGeometry(OD=0.3239, WT=0.3239)

    def test_d_over_t_property(self):
        geom = PipeGeometry(OD=0.3239, WT=0.01905)
        assert geom.D_over_t == pytest.approx(0.3239 / 0.01905, rel=1e-6)


# ---------------------------------------------------------------------------
# PipeMaterial dataclass
# ---------------------------------------------------------------------------

class TestPipeMaterial:
    """Material dataclass construction and validation."""

    def test_valid_material_created(self):
        mat = PipeMaterial(name="X65", SMYS=448.0, SMTS=531.0)
        assert mat.SMYS == pytest.approx(448.0)
        assert mat.SMTS == pytest.approx(531.0)

    def test_smys_must_be_positive(self):
        with pytest.raises(ValueError, match="SMYS"):
            PipeMaterial(name="bad", SMYS=0.0, SMTS=500.0)

    def test_smts_must_be_gte_smys(self):
        with pytest.raises(ValueError, match="SMTS"):
            PipeMaterial(name="bad", SMYS=500.0, SMTS=400.0)


# ---------------------------------------------------------------------------
# DNV-ST-F101: wall thickness sizing
# ---------------------------------------------------------------------------

class TestDnvWallThickness:
    """Minimum required wall thickness per DNV-ST-F101 pressure containment limit state.

    t_min = (p_d - p_e) * D / (2 * SMYS * gamma_SC * gamma_m * alpha_U)
    plus corrosion allowance and fabrication tolerance.
    """

    def test_returns_positive_thickness(self, x65):
        t_min = dnv_wall_thickness(
            p_d=20.0, p_e=0.0, D=0.3239, mat=x65, safety_class="normal"
        )
        assert t_min > 0.0

    def test_formula_no_allowances(self, x65):
        """Bare formula without corrosion or fabrication tolerance.

        t_bare = (p_d - p_e) * D * gamma_inc * gamma_m * gamma_SC / (2 * SMYS * alpha_U)
        """
        p_d = 20.0
        p_e = 0.0
        D = 0.3239
        SMYS = x65.SMYS
        gsc = GAMMA_SC["normal"]

        expected = (p_d - p_e) * D * GAMMA_INC * GAMMA_M * gsc / (2.0 * SMYS * ALPHA_U)
        result = dnv_wall_thickness(
            p_d=p_d, p_e=p_e, D=D, mat=x65, safety_class="normal",
            t_corr=0.0, fab_tol_pct=0.0,
        )
        assert result == pytest.approx(expected, rel=1e-6)

    def test_corrosion_allowance_increases_t_min(self, x65):
        base = dnv_wall_thickness(
            p_d=20.0, p_e=0.0, D=0.3239, mat=x65,
            safety_class="normal", t_corr=0.0, fab_tol_pct=0.0,
        )
        with_corr = dnv_wall_thickness(
            p_d=20.0, p_e=0.0, D=0.3239, mat=x65,
            safety_class="normal", t_corr=0.003, fab_tol_pct=0.0,
        )
        assert with_corr > base

    def test_fabrication_tolerance_increases_t_min(self, x65):
        base = dnv_wall_thickness(
            p_d=20.0, p_e=0.0, D=0.3239, mat=x65,
            safety_class="normal", t_corr=0.0, fab_tol_pct=0.0,
        )
        with_tol = dnv_wall_thickness(
            p_d=20.0, p_e=0.0, D=0.3239, mat=x65,
            safety_class="normal", t_corr=0.0, fab_tol_pct=12.5,
        )
        assert with_tol > base

    def test_high_safety_class_gives_thicker_wall(self, x65):
        t_normal = dnv_wall_thickness(
            p_d=20.0, p_e=0.0, D=0.3239, mat=x65, safety_class="normal"
        )
        t_high = dnv_wall_thickness(
            p_d=20.0, p_e=0.0, D=0.3239, mat=x65, safety_class="high"
        )
        assert t_high > t_normal

    def test_external_pressure_reduces_t_min(self, x65):
        t_no_ext = dnv_wall_thickness(
            p_d=20.0, p_e=0.0, D=0.3239, mat=x65, safety_class="normal"
        )
        t_with_ext = dnv_wall_thickness(
            p_d=20.0, p_e=2.0, D=0.3239, mat=x65, safety_class="normal"
        )
        assert t_with_ext < t_no_ext

    def test_invalid_safety_class_raises(self, x65):
        with pytest.raises(ValueError):
            dnv_wall_thickness(
                p_d=20.0, p_e=0.0, D=0.3239, mat=x65, safety_class="ultra"
            )

    def test_combined_allowances_formula(self, x65):
        """With corrosion and fabrication: t_req = t_bare + t_corr + fab_fraction * t_bare."""
        p_d, p_e, D = 15.0, 1.0, 0.508
        t_corr = 0.003
        fab_tol_pct = 12.5

        t_bare = dnv_wall_thickness(
            p_d=p_d, p_e=p_e, D=D, mat=x65,
            safety_class="normal", t_corr=0.0, fab_tol_pct=0.0,
        )
        result = dnv_wall_thickness(
            p_d=p_d, p_e=p_e, D=D, mat=x65,
            safety_class="normal", t_corr=t_corr, fab_tol_pct=fab_tol_pct,
        )
        # t_min_nominal = t_bare / (1 - fab_tol_pct/100) + t_corr
        expected = t_bare / (1.0 - fab_tol_pct / 100.0) + t_corr
        assert result == pytest.approx(expected, rel=1e-5)


# ---------------------------------------------------------------------------
# DNV-ST-F101: burst resistance
# ---------------------------------------------------------------------------

class TestDnvBurstResistance:
    """Burst resistance p_b per DNV-ST-F101 Eq. (5.4).

    p_b(t1) = 2 * (t1/D) * min(SMYS, 0.9*SMTS) * f_cb
    """

    def test_returns_positive_value(self, std_geom, x65):
        p_b = dnv_burst_resistance(
            D=std_geom.OD, t_nom=std_geom.WT, mat=x65
        )
        assert p_b > 0.0

    def test_formula_no_deductions(self, x65):
        OD = 0.3239
        WT = 0.01905
        smys_t = min(x65.SMYS, 0.9 * x65.SMTS)
        expected = 2.0 * (WT / OD) * smys_t

        result = dnv_burst_resistance(
            D=OD, t_nom=WT, mat=x65,
            t_fab_neg=0.0, t_corr=0.0, temp_derating=0.0,
        )
        assert result == pytest.approx(expected, rel=1e-6)

    def test_fabrication_tolerance_reduces_p_b(self, x65):
        p_no_tol = dnv_burst_resistance(
            D=0.3239, t_nom=0.01905, mat=x65, t_fab_neg=0.0
        )
        p_with_tol = dnv_burst_resistance(
            D=0.3239, t_nom=0.01905, mat=x65, t_fab_neg=12.5
        )
        assert p_with_tol < p_no_tol

    def test_corrosion_allowance_reduces_p_b(self, x65):
        p_no_corr = dnv_burst_resistance(
            D=0.3239, t_nom=0.01905, mat=x65, t_corr=0.0
        )
        p_with_corr = dnv_burst_resistance(
            D=0.3239, t_nom=0.01905, mat=x65, t_corr=0.003
        )
        assert p_with_corr < p_no_corr

    def test_temp_derating_reduces_p_b(self, x65):
        p_no_derating = dnv_burst_resistance(
            D=0.3239, t_nom=0.01905, mat=x65, temp_derating=0.0
        )
        p_with_derating = dnv_burst_resistance(
            D=0.3239, t_nom=0.01905, mat=x65, temp_derating=30.0
        )
        assert p_with_derating < p_no_derating


# ---------------------------------------------------------------------------
# DNV-ST-F101: pressure containment check
# ---------------------------------------------------------------------------

class TestDnvPressureContainmentCheck:
    """Local incidental pressure vs. burst resistance."""

    def test_pass_below_limit(self, x65):
        result = dnv_pressure_containment_check(
            p_li=10.0, D=0.3239, t_nom=0.01905, mat=x65, safety_class="normal"
        )
        assert result["pass"] is True

    def test_fail_above_limit(self, x65):
        result = dnv_pressure_containment_check(
            p_li=500.0, D=0.3239, t_nom=0.01905, mat=x65, safety_class="normal"
        )
        assert result["pass"] is False

    def test_utilization_at_unity_is_limit(self, x65):
        """When p_li equals the allowable pressure, utilization == 1.0."""
        D = 0.3239
        t_nom = 0.01905
        p_b = dnv_burst_resistance(D=D, t_nom=t_nom, mat=x65)
        p_b_allowable = p_b / GAMMA_INC
        result = dnv_pressure_containment_check(
            p_li=p_b_allowable, D=D, t_nom=t_nom, mat=x65, safety_class="normal"
        )
        assert result["utilization"] == pytest.approx(1.0, rel=1e-5)

    def test_result_contains_p_b_and_allowable(self, x65):
        result = dnv_pressure_containment_check(
            p_li=15.0, D=0.3239, t_nom=0.01905, mat=x65, safety_class="normal"
        )
        assert "p_b" in result
        assert "p_b_allowable" in result
        assert result["p_b"] > 0.0
        assert result["p_b_allowable"] == pytest.approx(result["p_b"] / GAMMA_INC, rel=1e-6)


# ---------------------------------------------------------------------------
# DNV-ST-F101: system pressure test
# ---------------------------------------------------------------------------

class TestDnvSystemPressureTest:
    """System pressure test: p_test = factor * p_d (min factor = 1.25)."""

    def test_default_factor_is_1_25(self):
        p_test = dnv_system_pressure_test(p_d=20.0)
        assert p_test == pytest.approx(1.25 * 20.0, rel=1e-9)

    def test_custom_factor(self):
        p_test = dnv_system_pressure_test(p_d=20.0, factor=1.5)
        assert p_test == pytest.approx(1.5 * 20.0, rel=1e-9)

    def test_positive_p_d_required(self):
        with pytest.raises(ValueError):
            dnv_system_pressure_test(p_d=0.0)

    def test_p_test_always_greater_than_p_d(self):
        p_d = 15.0
        assert dnv_system_pressure_test(p_d=p_d) > p_d

    def test_hoop_stress_at_test_pressure(self, x65):
        """Hoop stress at test pressure must be <= 0.96 * SMYS."""
        OD = 0.3239
        WT = 0.01905
        p_d = 15.0
        p_test = dnv_system_pressure_test(p_d=p_d)
        hoop_stress = p_test * OD / (2.0 * WT)
        assert hoop_stress <= ALPHA_U * x65.SMYS


# ---------------------------------------------------------------------------
# DNV-ST-F101: MAOP
# ---------------------------------------------------------------------------

class TestDnvMaop:
    """Maximum Allowable Operating Pressure."""

    def test_returns_positive_value(self, x65):
        maop_val = dnv_maop(
            D=0.3239, t_nom=0.01905, mat=x65, safety_class="normal"
        )
        assert maop_val > 0.0

    def test_formula_maop(self, x65):
        """MAOP = p_b / (gamma_inc * gamma_m * gamma_sc)."""
        D = 0.3239
        t_nom = 0.01905
        p_b = dnv_burst_resistance(D=D, t_nom=t_nom, mat=x65)
        gsc = GAMMA_SC["normal"]
        expected = p_b / (GAMMA_INC * GAMMA_M * gsc)
        result = dnv_maop(D=D, t_nom=t_nom, mat=x65, safety_class="normal")
        assert result == pytest.approx(expected, rel=1e-6)

    def test_high_safety_class_reduces_maop(self, x65):
        maop_normal = dnv_maop(
            D=0.3239, t_nom=0.01905, mat=x65, safety_class="normal"
        )
        maop_high = dnv_maop(
            D=0.3239, t_nom=0.01905, mat=x65, safety_class="high"
        )
        assert maop_high < maop_normal

    def test_thicker_wall_increases_maop(self, x65):
        maop_thin = dnv_maop(D=0.3239, t_nom=0.010, mat=x65, safety_class="normal")
        maop_thick = dnv_maop(D=0.3239, t_nom=0.025, mat=x65, safety_class="normal")
        assert maop_thick > maop_thin

    def test_invalid_safety_class_raises(self, x65):
        with pytest.raises(ValueError):
            dnv_maop(D=0.3239, t_nom=0.01905, mat=x65, safety_class="extreme")


# ---------------------------------------------------------------------------
# API RP 1111: water pressure at depth
# ---------------------------------------------------------------------------

class TestApiWaterPressure:
    """Hydrostatic pressure calculation."""

    def test_zero_depth_returns_zero(self):
        assert api_water_pressure(depth=0.0) == pytest.approx(0.0)

    def test_positive_depth_gives_positive_pressure(self):
        assert api_water_pressure(depth=500.0) > 0.0

    def test_formula_1000m(self):
        """p = rho * g * d / 1e6  at 1000 m depth, rho=1025 kg/m3, g=9.81 m/s2."""
        expected = 1025.0 * 9.81 * 1000.0 / 1e6
        assert api_water_pressure(depth=1000.0) == pytest.approx(expected, rel=1e-6)

    def test_negative_depth_returns_zero(self):
        assert api_water_pressure(depth=-10.0) == pytest.approx(0.0)

    def test_pressure_proportional_to_depth(self):
        p_500 = api_water_pressure(depth=500.0)
        p_1000 = api_water_pressure(depth=1000.0)
        assert p_1000 == pytest.approx(2.0 * p_500, rel=1e-6)


# ---------------------------------------------------------------------------
# API RP 1111: elastic collapse pressure
# ---------------------------------------------------------------------------

class TestApiElasticCollapseP:
    """Timoshenko ring formula: p_e = 2*E*(t/D)^3 / (1 - nu^2)."""

    def test_positive_value_returned(self):
        assert api_elastic_collapse_pressure(D=0.3239, t=0.01905) > 0.0

    def test_formula_exact(self):
        D, t = 0.3239, 0.01905
        E, nu = 207000.0, 0.3
        expected = 2.0 * E * (t / D) ** 3 / (1.0 - nu ** 2)
        assert api_elastic_collapse_pressure(D=D, t=t) == pytest.approx(expected, rel=1e-6)

    def test_increases_with_wall_thickness(self):
        p_thin = api_elastic_collapse_pressure(D=0.3239, t=0.010)
        p_thick = api_elastic_collapse_pressure(D=0.3239, t=0.025)
        assert p_thick > p_thin


# ---------------------------------------------------------------------------
# API RP 1111: collapse pressure (interaction equation)
# ---------------------------------------------------------------------------

class TestApiCollapsePressure:
    """Two-term collapse interaction: p_plastic * p_e / (p_plastic + p_e)."""

    def test_positive_value_returned(self, x65):
        assert api_collapse_pressure(D=0.3239, t=0.01905, SMYS=x65.SMYS) > 0.0

    def test_increases_with_wt(self, x65):
        p_thin = api_collapse_pressure(D=0.3239, t=0.010, SMYS=x65.SMYS)
        p_thick = api_collapse_pressure(D=0.3239, t=0.025, SMYS=x65.SMYS)
        assert p_thick > p_thin

    def test_p_c_less_than_both_components(self, x65):
        """Harmonic interaction: p_c < p_plastic and p_c < p_e."""
        D, t = 0.3239, 0.01905
        f_o = 1.0
        p_plastic = f_o * 0.45 * x65.SMYS * (2.0 * t / D)
        p_e = api_elastic_collapse_pressure(D=D, t=t)
        p_c = api_collapse_pressure(D=D, t=t, SMYS=x65.SMYS)
        assert p_c < p_plastic
        assert p_c < p_e

    def test_ovality_reduces_collapse_pressure(self, x65):
        D, t = 0.3239, 0.01905
        p_round = api_collapse_pressure(D=D, t=t, SMYS=x65.SMYS, f_o=1.0)
        p_oval = api_collapse_pressure(D=D, t=t, SMYS=x65.SMYS, f_o=0.85)
        assert p_oval < p_round


# ---------------------------------------------------------------------------
# API RP 1111: external collapse check
# ---------------------------------------------------------------------------

class TestApiExternalCollapseCheck:
    """p_water <= DF_COLLAPSE * p_c."""

    def test_pass_at_shallow_depth(self, x65):
        result = api_external_collapse_check(
            depth=100.0, D=0.3239, t=0.01905, SMYS=x65.SMYS
        )
        assert result["pass"] is True

    def test_fail_at_extreme_depth(self, x65):
        result = api_external_collapse_check(
            depth=10000.0, D=0.3239, t=0.005, SMYS=x65.SMYS
        )
        assert result["pass"] is False

    def test_result_keys_present(self, x65):
        result = api_external_collapse_check(
            depth=500.0, D=0.3239, t=0.01905, SMYS=x65.SMYS
        )
        for key in ("pass", "utilization", "p_water", "p_c", "p_allowable"):
            assert key in result

    def test_utilization_formula(self, x65):
        depth = 500.0
        D, t = 0.3239, 0.01905
        result = api_external_collapse_check(depth=depth, D=D, t=t, SMYS=x65.SMYS)
        expected_util = result["p_water"] / result["p_allowable"]
        assert result["utilization"] == pytest.approx(expected_util, rel=1e-6)

    def test_p_allowable_equals_df_times_pc(self, x65):
        result = api_external_collapse_check(
            depth=500.0, D=0.3239, t=0.01905, SMYS=x65.SMYS
        )
        assert result["p_allowable"] == pytest.approx(DF_COLLAPSE * result["p_c"], rel=1e-6)


# ---------------------------------------------------------------------------
# API RP 1111: propagating buckle pressure
# ---------------------------------------------------------------------------

class TestApiPropagatingBucklePressure:
    """p_p = 24 * SMYS * (t/D)^2.4."""

    def test_positive_value_returned(self, x65):
        assert api_propagating_buckle_pressure(D=0.3239, t=0.01905, SMYS=x65.SMYS) > 0.0

    def test_formula_exact(self, x65):
        D, t = 0.3239, 0.01905
        expected = 24.0 * x65.SMYS * (t / D) ** 2.4
        result = api_propagating_buckle_pressure(D=D, t=t, SMYS=x65.SMYS)
        assert result == pytest.approx(expected, rel=1e-6)

    def test_increases_with_wt(self, x65):
        p_thin = api_propagating_buckle_pressure(D=0.3239, t=0.010, SMYS=x65.SMYS)
        p_thick = api_propagating_buckle_pressure(D=0.3239, t=0.025, SMYS=x65.SMYS)
        assert p_thick > p_thin


# ---------------------------------------------------------------------------
# API RP 1111: propagating buckle check
# ---------------------------------------------------------------------------

class TestApiPropagatingBuckleCheck:
    """p_water <= p_p / DF_PROPAGATION; if fails, arrestors required."""

    def test_pass_and_no_arrestors_at_shallow_depth(self, x65):
        result = api_propagating_buckle_check(
            depth=100.0, D=0.3239, t=0.01905, SMYS=x65.SMYS
        )
        assert result["pass"] is True
        assert result["arrestors_required"] is False

    def test_fail_and_arrestors_required_at_deep(self, x65):
        result = api_propagating_buckle_check(
            depth=8000.0, D=0.3239, t=0.008, SMYS=x65.SMYS
        )
        assert result["pass"] is False
        assert result["arrestors_required"] is True

    def test_result_keys_present(self, x65):
        result = api_propagating_buckle_check(
            depth=500.0, D=0.3239, t=0.01905, SMYS=x65.SMYS
        )
        for key in ("pass", "utilization", "p_water", "p_p", "p_p_allowable",
                    "arrestors_required"):
            assert key in result

    def test_p_p_allowable_equals_pp_over_df(self, x65):
        result = api_propagating_buckle_check(
            depth=500.0, D=0.3239, t=0.01905, SMYS=x65.SMYS
        )
        assert result["p_p_allowable"] == pytest.approx(
            result["p_p"] / DF_PROPAGATION, rel=1e-6
        )

    def test_pass_flag_consistent_with_utilization(self, x65):
        result = api_propagating_buckle_check(
            depth=500.0, D=0.3239, t=0.01905, SMYS=x65.SMYS
        )
        if result["utilization"] <= 1.0:
            assert result["pass"] is True
            assert result["arrestors_required"] is False
        else:
            assert result["pass"] is False
            assert result["arrestors_required"] is True


# ---------------------------------------------------------------------------
# Cross-code consistency
# ---------------------------------------------------------------------------

class TestCrossCodeConsistency:
    """Sanity-check relationships between DNV and API results."""

    def test_thicker_wall_satisfies_both_codes(self, x65):
        """A sufficiently thick-walled pipe passes both DNV pressure check
        and API collapse check at moderate depth."""
        D = 0.4064  # 16-inch
        t = 0.025
        SMYS = x65.SMYS
        depth = 300.0
        p_d = 15.0

        dnv_result = dnv_pressure_containment_check(
            p_li=p_d * GAMMA_INC, D=D, t_nom=t, mat=x65, safety_class="normal"
        )
        api_result = api_external_collapse_check(
            depth=depth, D=D, t=t, SMYS=SMYS
        )
        assert dnv_result["pass"] is True
        assert api_result["pass"] is True

    def test_wall_thickness_sizing_produces_passing_check(self, x65):
        """A pipe sized by dnv_wall_thickness should pass dnv_pressure_containment_check."""
        D = 0.3239
        p_d = 18.0
        p_e = 1.0

        t_min = dnv_wall_thickness(
            p_d=p_d, p_e=p_e, D=D, mat=x65, safety_class="normal",
            t_corr=0.003, fab_tol_pct=12.5,
        )
        # Use the nominal (designed) wall thickness to verify it passes
        result = dnv_pressure_containment_check(
            p_li=p_d * GAMMA_INC, D=D, t_nom=t_min, mat=x65, safety_class="normal"
        )
        assert result["pass"] is True
