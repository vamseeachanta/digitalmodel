# ABOUTME: TDD/validation tests for the DNV-ST-F201 dynamic-riser LRFD strategy
# ABOUTME: Worked-example pressure-containment check, LRFD factor application, determinism

import math

import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY
from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f201 import (
    DnvStF201Strategy,
)


def make_x65():
    return PipeMaterial(grade="X65", smys=448e6, smts=531e6)


def make_10inch(wall_thickness=0.0214, corrosion=0.001):
    # OD = 10.75 in = 0.27305 m, default WT = 0.0214 m, CA = 1 mm, fab tol = 12.5%
    return PipeGeometry(
        outer_diameter=0.27305,
        wall_thickness=wall_thickness,
        corrosion_allowance=corrosion,
    )


class TestF201Registration:
    def test_registered_in_code_registry(self):
        assert DesignCode.DNV_ST_F201 in CODE_REGISTRY
        assert CODE_REGISTRY[DesignCode.DNV_ST_F201] is DnvStF201Strategy

    def test_code_name(self):
        assert DnvStF201Strategy.code_name == "DNV-ST-F201"

    def test_check_names(self):
        assert DnvStF201Strategy.check_names == ["pressure_containment"]

    def test_protocol_attributes(self):
        inst = DnvStF201Strategy()
        for attr in ("code_name", "check_names", "run_checks",
                     "compute_plastic_moment", "compute_plastic_tension"):
            assert hasattr(inst, attr)

    def test_default_factors(self):
        f = DnvStF201Strategy().get_factors()
        assert f["alpha_U"] == pytest.approx(0.96)
        assert f["gamma_m"] == pytest.approx(1.15)
        assert f["gamma_dyn"] == pytest.approx(1.0)
        assert f["gamma_SC"]["medium"] == pytest.approx(1.138)


class TestF201WorkedExample:
    """Worked example, hand-verified.

    Geometry: OD = 0.27305 m, WT = 0.0214 m, CA = 0.001 m, fab tol = 0.125
        t1 = (0.0214 - 0.001) * (1 - 0.125) = 0.017850 m
    Material X65: SMYS = 448 MPa, SMTS = 531 MPa, alpha_U = 0.96
        f_y = 448 * 0.96 = 430.08 MPa
        f_u = 531 * 0.96 = 509.76 MPa ; f_u/1.15 = 443.27 MPa
        f_cb = min(f_y, f_u/1.15) = 430.08 MPa
    Burst resistance:
        p_b = (2*t1/(D-t1)) * (2/sqrt(3)) * f_cb = 69.471 MPa
    LRFD factors: gamma_m = 1.15, gamma_SC(normal) = 1.138, gamma_dyn = 1.0
        p_b_design = 69.471 / (1.15 * 1.138) = 53.084 MPa
    Demand at p_li = 20 MPa, p_e = 2 MPa: p_net = 18 MPa
        utilisation = 1.0 * 18 / 53.084 = 0.3391
    """

    def test_burst_resistance_hand_calc(self):
        strat = DnvStF201Strategy()
        util, d = strat._check_pressure_containment(
            make_10inch(), make_x65(),
            DesignLoads(internal_pressure=20e6, external_pressure=2e6),
            DesignFactors(safety_class=SafetyClass.MEDIUM),
        )

        t1 = (0.0214 - 0.001) * (1 - 0.125)
        D = 0.27305
        f_cb = min(448e6 * 0.96, 531e6 * 0.96 / 1.15)
        exp_pb = (2 * t1 / (D - t1)) * (2 / math.sqrt(3)) * f_cb
        exp_pbd = exp_pb / (1.15 * 1.138)

        assert d["t1"] == pytest.approx(0.017850, rel=1e-6)
        assert d["f_cb"] == pytest.approx(f_cb, rel=1e-9)
        assert d["p_b"] == pytest.approx(exp_pb, rel=1e-9)
        assert d["p_b_design"] == pytest.approx(exp_pbd, rel=1e-9)
        # hand-verified utilisation
        assert util == pytest.approx(0.3391, abs=1e-4)

    def test_within_limit_pipe_passes(self):
        analyzer = WallThicknessAnalyzer(
            make_10inch(), make_x65(),
            DesignLoads(internal_pressure=20e6, external_pressure=2e6),
            DesignFactors(safety_class=SafetyClass.MEDIUM),
            DesignCode.DNV_ST_F201,
        )
        result = analyzer.perform_analysis()
        assert set(result.checks) == {"pressure_containment"}
        assert result.checks["pressure_containment"] < 1.0
        assert result.is_safe is True

    def test_under_thickness_pipe_fails(self):
        """Same loads, thin 8 mm wall -> utilisation > 1 (limit state violated)."""
        analyzer = WallThicknessAnalyzer(
            make_10inch(wall_thickness=0.008), make_x65(),
            DesignLoads(internal_pressure=20e6, external_pressure=2e6),
            DesignFactors(safety_class=SafetyClass.MEDIUM),
            DesignCode.DNV_ST_F201,
        )
        result = analyzer.perform_analysis()
        assert result.checks["pressure_containment"] > 1.0
        assert result.is_safe is False


class TestF201LrfdFactors:
    def test_gamma_dyn_scales_utilisation_linearly(self):
        base = DnvStF201Strategy()
        dyn = DnvStF201Strategy(gamma_dyn=1.3)
        loads = DesignLoads(internal_pressure=20e6, external_pressure=2e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        u0, _ = base._check_pressure_containment(make_10inch(), make_x65(), loads, factors)
        u1, _ = dyn._check_pressure_containment(make_10inch(), make_x65(), loads, factors)
        assert u1 == pytest.approx(1.3 * u0, rel=1e-9)

    def test_safety_class_increases_utilisation(self):
        strat = DnvStF201Strategy()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=2e6)
        u_low, _ = strat._check_pressure_containment(
            make_10inch(), make_x65(), loads, DesignFactors(safety_class=SafetyClass.LOW))
        u_med, _ = strat._check_pressure_containment(
            make_10inch(), make_x65(), loads, DesignFactors(safety_class=SafetyClass.MEDIUM))
        u_high, _ = strat._check_pressure_containment(
            make_10inch(), make_x65(), loads, DesignFactors(safety_class=SafetyClass.HIGH))
        assert u_low < u_med < u_high

    def test_gamma_sc_factor_value_applied(self):
        strat = DnvStF201Strategy()
        _, d = strat._check_pressure_containment(
            make_10inch(), make_x65(),
            DesignLoads(internal_pressure=20e6, external_pressure=2e6),
            DesignFactors(safety_class=SafetyClass.HIGH),
        )
        assert d["gamma_sc"] == pytest.approx(1.308)
        assert d["gamma_m"] == pytest.approx(1.15)

    def test_overridable_material_strength_factor(self):
        # alpha_U = 1.0 (supplementary requirement U) raises resistance, lowers util
        default = DnvStF201Strategy()
        sru = DnvStF201Strategy(alpha_U=1.0)
        loads = DesignLoads(internal_pressure=20e6, external_pressure=2e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        u_def, _ = default._check_pressure_containment(make_10inch(), make_x65(), loads, factors)
        u_sru, _ = sru._check_pressure_containment(make_10inch(), make_x65(), loads, factors)
        assert u_sru < u_def

    def test_zero_net_pressure_zero_utilisation(self):
        strat = DnvStF201Strategy()
        u, _ = strat._check_pressure_containment(
            make_10inch(), make_x65(),
            DesignLoads(internal_pressure=5e6, external_pressure=5e6),
            DesignFactors(),
        )
        assert abs(u) < 1e-12


class TestF201Determinism:
    def test_repeated_runs_identical(self):
        loads = DesignLoads(internal_pressure=20e6, external_pressure=2e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)
        results = [
            DnvStF201Strategy()._check_pressure_containment(
                make_10inch(), make_x65(), loads, factors)[0]
            for _ in range(5)
        ]
        assert all(r == results[0] for r in results)
