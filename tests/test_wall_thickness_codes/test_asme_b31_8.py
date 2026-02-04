# ABOUTME: TDD tests for ASME B31.8 gas transmission pipeline design code
# ABOUTME: Hand-calculation validation for Barlow burst, collapse, propagation

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
from digitalmodel.structural.analysis.wall_thickness_codes.asme_b31_8 import AsmeB318Strategy


def make_x65():
    return PipeMaterial(grade="X65", smys=448e6, smts=531e6)


def make_10inch(corrosion=0.001):
    return PipeGeometry(outer_diameter=0.27305, wall_thickness=0.0214, corrosion_allowance=corrosion)


class TestAsmeB318Registration:
    def test_asme_b31_8_registered_in_code_registry(self):
        assert DesignCode.ASME_B31_8 in CODE_REGISTRY

    def test_asme_b31_8_strategy_check_names(self):
        assert set(AsmeB318Strategy.check_names) == {"burst", "collapse", "propagation"}

    def test_asme_b31_8_code_name(self):
        assert AsmeB318Strategy.code_name == "ASME-B31.8"


class TestAsmeB318Burst:
    def test_burst_moderate_pressure_safe(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=25e6, external_pressure=10e6)
        factors = DesignFactors()

        strategy = AsmeB318Strategy()
        util, details = strategy._check_burst(geom, mat, loads)
        assert 0 < util < 1.0
        assert details["p_d"] > 0

    def test_burst_hand_calc_validation(self):
        """Validate Barlow burst against hand calculation.

        p_d = 2 * S * t * F * E * T / D
            = 2 * 448e6 * 0.0214 * 0.72 * 1.0 * 1.0 / 0.27305
            = 2 * 448e6 * 0.0214 * 0.72 / 0.27305
            = 50.52 MPa

        p_net = 25e6 - 10e6 = 15e6 Pa
        util = 15 / 50.52 = 0.297
        """
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=25e6, external_pressure=10e6)
        factors = DesignFactors()

        strategy = AsmeB318Strategy()
        util, details = strategy._check_burst(geom, mat, loads)

        D = 0.27305
        t = 0.0214
        expected_p_d = 2 * 448e6 * t * 0.72 * 1.0 * 1.0 / D
        expected_util = 15e6 / expected_p_d

        assert abs(details["p_d"] - expected_p_d) / expected_p_d < 0.001
        assert abs(util - expected_util) / expected_util < 0.001

    def test_burst_zero_pressure_diff_zero_utilisation(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=10e6, external_pressure=10e6)
        factors = DesignFactors()

        strategy = AsmeB318Strategy()
        util, _ = strategy._check_burst(geom, mat, loads)
        assert abs(util) < 1e-10

    def test_burst_high_pressure_above_one(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=80e6, external_pressure=0.0)
        factors = DesignFactors()

        strategy = AsmeB318Strategy()
        util, _ = strategy._check_burst(geom, mat, loads)
        assert util > 1.0


class TestAsmeB318Collapse:
    def test_collapse_pressures_positive(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=10e6)
        factors = DesignFactors()

        strategy = AsmeB318Strategy()
        util, details = strategy._check_collapse(geom, mat, loads)
        assert details["p_el"] > 0
        assert details["p_y"] > 0
        assert details["p_c"] > 0

    def test_collapse_hand_calc_transition(self):
        """Verify p_c = p_el*p_y / sqrt(p_el^2 + p_y^2).

        Using nominal t = 0.0214 m, D = 0.27305 m.
        p_el = 2*207e9*(0.0214/0.27305)^3 / (1-0.09) = ...
        p_y = 2*448e6*0.0214/0.27305 = ...
        """
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors()

        strategy = AsmeB318Strategy()
        _, details = strategy._check_collapse(geom, mat, loads)

        D = 0.27305
        t = 0.0214
        E = 207e9
        nu = 0.3

        expected_p_el = 2 * E * (t / D) ** 3 / (1 - nu**2)
        expected_p_y = 2 * 448e6 * t / D
        expected_p_c = expected_p_el * expected_p_y / math.sqrt(expected_p_el**2 + expected_p_y**2)

        assert abs(details["p_el"] - expected_p_el) / expected_p_el < 0.001
        assert abs(details["p_y"] - expected_p_y) / expected_p_y < 0.001
        assert abs(details["p_c"] - expected_p_c) / expected_p_c < 0.001

    def test_collapse_transition_less_than_components(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors()

        strategy = AsmeB318Strategy()
        _, details = strategy._check_collapse(geom, mat, loads)
        assert details["p_c"] <= details["p_el"]
        assert details["p_c"] <= details["p_y"]

    def test_collapse_low_pressure_safe(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=2e6)
        factors = DesignFactors()

        strategy = AsmeB318Strategy()
        util, _ = strategy._check_collapse(geom, mat, loads)
        assert util < 1.0


class TestAsmeB318Propagation:
    def test_propagation_hand_calc(self):
        """Verify p_pr = 24 * SMYS * (t/D)^2.4.

        t = 0.0214, D = 0.27305 (nominal values)
        """
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors()

        strategy = AsmeB318Strategy()
        _, details = strategy._check_propagation(geom, mat, loads)

        t = 0.0214
        D = 0.27305
        expected_p_pr = 24 * 448e6 * (t / D) ** 2.4
        assert abs(details["p_pr"] - expected_p_pr) / expected_p_pr < 0.001

    def test_propagation_zero_external_zero_utilisation(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=0.0)
        factors = DesignFactors()

        strategy = AsmeB318Strategy()
        util, _ = strategy._check_propagation(geom, mat, loads)
        assert abs(util) < 1e-10


class TestAsmeB318Integration:
    def test_perform_analysis_returns_three_checks(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)

        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.ASME_B31_8)
        result = analyzer.perform_analysis()
        assert len(result.checks) == 3
        assert set(result.checks.keys()) == {"burst", "collapse", "propagation"}

    def test_perform_analysis_safe_for_moderate_loads(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=15e6, external_pressure=3e6)
        factors = DesignFactors()

        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.ASME_B31_8)
        result = analyzer.perform_analysis()
        assert result.is_safe is True

    def test_perform_analysis_unsafe_for_extreme_pressure(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=80e6, external_pressure=0.0)
        factors = DesignFactors()

        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.ASME_B31_8)
        result = analyzer.perform_analysis()
        assert result.is_safe is False
