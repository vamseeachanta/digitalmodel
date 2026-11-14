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
from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f101 import (
    DnvStF101Strategy,
    EDITION_FACTORS,
    LATEST_EDITION,
)
from digitalmodel.structural.pipe_capacity.custom.DNV_OS_F201 import (
    dnv_os_f201_lrfd_wsd as legacy,
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
        assert DnvStF201Strategy.check_names == ["pressure_containment", "combined"]

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
        assert set(result.checks) == {"pressure_containment", "combined"}
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


def test_combined_spreadsheet_oracle_in_si():
    """Rounded sheet values use in, ksi, kip.ft; the legacy oracle uses rtol=1e-3.

    One inch is 0.0254 m; one kip is 4448.2216152605 N; one foot is 0.3048 m.
    The resistance thickness is t2, without a fabrication-tolerance deduction.
    """
    inch = 0.0254
    kip = 4448.2216152605
    ksi = kip / inch**2
    geometry = PipeGeometry(20 * inch, 0.82678 * inch)
    material = PipeMaterial("X65", 65 * ksi, 77 * ksi)
    loads = DesignLoads(internal_pressure=3 * ksi,
                        bending_moment=-1252.8 * kip * 0.3048)
    utilisation, details = DnvStF201Strategy(gamma_m=1.0).run_checks(
        geometry, material, loads, DesignFactors(safety_class=SafetyClass.HIGH)
    )["combined"]
    assert utilisation == pytest.approx(0.999883, rel=1e-3)
    assert utilisation <= details["limit"] == 1.0
    assert details["gamma_sc"] == 1.26


@pytest.mark.parametrize("pi,pe", [(20e6, 2e6), (2e6, 20e6), (2e6, 2e6)])
@pytest.mark.parametrize("safety_class", list(SafetyClass))
def test_combined_delegation_machine_precision(pi, pe, safety_class):
    geometry, material = make_10inch(), make_x65()
    diameter, t2 = geometry.outer_diameter, geometry.t2
    fy = legacy.yield_strength(material.smys, 0.0, 0.96)
    fu = legacy.tensile_strength(material.smts, 0.0, 0.96)
    pb = legacy.bursting_resistance(diameter, t2, fy, fu)
    alpha_c = legacy.flow_stress_coefficient(diameter, t2, fy, fu, pi, pe, pb)
    mk = legacy.plastic_moment_resistance(diameter, t2, fy, alpha_c)
    tk = legacy.plastic_axial_resistance(diameter, t2, fy, alpha_c)
    sc = "normal" if safety_class == SafetyClass.MEDIUM else safety_class.value
    gamma_sc = legacy.SAFETY_CLASS_FACTOR[sc]
    args = (125e3, mk, 900e3, tk)
    if pi > pe:
        expected = legacy.combined_loading_internal_overpressure(
            *args, pi, pe, pb, gamma_sc=gamma_sc, gamma_m=1.15)
    else:
        f0 = EDITION_FACTORS[LATEST_EDITION]["f_0"]
        p_el = 2 * material.youngs_modulus * (t2 / diameter)**3 / (1 - material.poissons_ratio**2)
        p_p = material.smys * material.alpha_fab * 2 * t2 / diameter
        pc = DnvStF101Strategy._solve_collapse_pressure(p_el, p_p, f0, diameter, t2)
        expected = legacy.combined_loading_external_overpressure(
            *args, pe, pi, pc, gamma_sc=gamma_sc, gamma_m=1.15)
    checks = DnvStF201Strategy().run_checks(
        geometry, material, DesignLoads(pi, pe, 125e3, 900e3),
        DesignFactors(safety_class=safety_class))
    actual, details = checks["combined"]
    assert actual == expected.utilisation
    assert (details["Mk"], details["Tk"], details["alpha_c"], details["p_b"]) == (mk, tk, alpha_c, pb)
    assert details["gamma_sc"] == gamma_sc
    assert details["gamma_m"] == 1.15
    assert details["regime"] == ("internal" if pi > pe else "external")
    assert details["utilisation"] == actual
    assert details["limit"] == expected.limit == 1.0
    assert checks["pressure_containment"][1]["gamma_sc"] == DnvStF201Strategy().get_factors()["gamma_SC"][safety_class.value]
    if pi <= pe:
        assert details["p_c"] == pc
        assert details["p_min_internal"] == pi
        assert details["f_0"] == f0
        assert details["alpha_fab"] == material.alpha_fab
        assert details["collapse_reference"] == "DNV-ST-F101 Eq. 5.9"


def test_combined_zero_loads_and_monotonic_moment():
    strategy = DnvStF201Strategy()
    geometry, material, factors = make_10inch(), make_x65(), DesignFactors()
    zero, _ = strategy.run_checks(geometry, material, DesignLoads(), factors)["combined"]
    assert zero == 0.0
    loads = [DesignLoads(20e6, 2e6, moment, 900e3) for moment in (0, 100e3, 200e3)]
    values = [strategy.run_checks(geometry, material, load, factors)["combined"][0]
              for load in loads]
    assert values[0] < values[1] < values[2]


def test_combined_pressure_changes_flow_stress_and_runs_are_deterministic():
    strategy = DnvStF201Strategy()
    args = (make_10inch(), make_x65())
    low = strategy.run_checks(*args, DesignLoads(10e6, 2e6, 100e3, 900e3), DesignFactors())
    loads = DesignLoads(30e6, 2e6, 100e3, 900e3)
    high = strategy.run_checks(*args, loads, DesignFactors())
    assert high == strategy.run_checks(*args, loads, DesignFactors())
    assert high["combined"][1]["alpha_c"] > low["combined"][1]["alpha_c"]
    assert strategy.get_factors()["combined_gamma_SC"] == legacy.SAFETY_CLASS_FACTOR


def test_combined_above_burst_domain_fails_without_square_root_error():
    result = DnvStF201Strategy().run_checks(
        make_10inch(wall_thickness=0.008), make_x65(),
        DesignLoads(internal_pressure=100e6), DesignFactors())
    assert result["pressure_containment"][0] > 1.0
    utilisation, details = result["combined"]
    assert utilisation > details["limit"] == 1.0
    assert details["criterion_domain_exceeded"] is True


def test_combined_consistent_with_simplified_code_check_example():
    """Full pressure-corrected and simplified pressure-free criteria differ.

    The example's three points must share verdicts and agree within a factor
    of two (not numerical equality: simplified moment terms are quadratic).
    """
    import numpy as np
    import yaml
    from pathlib import Path
    from digitalmodel.orcaflex.code_check_engine import (
        DNVOSF201Input, check_dnv_os_f201,
    )

    source = Path(__file__).resolve().parents[2] / "examples/workflows/dnv-os-f201-riser/input.yml"
    inputs = yaml.safe_load(source.read_text())["code_check"]["inputs"]
    simplified = check_dnv_os_f201(
        DNVOSF201Input(**inputs), np.array(inputs["arc_lengths"]),
        np.array(inputs["effective_tension_kN"]),
        np.array(inputs["bending_moment_kNm"]),
        np.array(inputs["external_pressure_MPa"]),
        np.array(inputs["internal_pressure_MPa"]),
    )
    geometry = PipeGeometry(inputs["outer_diameter"], inputs["wall_thickness"])
    material = PipeMaterial("X65", inputs["smys"], inputs["smts"])
    for i, reference in enumerate(simplified):
        loads = DesignLoads(
            inputs["internal_pressure_MPa"][i] * 1e6,
            inputs["external_pressure_MPa"][i] * 1e6,
            inputs["bending_moment_kNm"][i] * 1e3,
            inputs["effective_tension_kN"][i] * 1e3)
        utilisation, _ = DnvStF201Strategy().run_checks(
            geometry, material, loads, DesignFactors())["combined"]
        assert (utilisation <= 1.0) == reference.pass_
        assert 0.5 <= utilisation / reference.utilisation <= 2.0
