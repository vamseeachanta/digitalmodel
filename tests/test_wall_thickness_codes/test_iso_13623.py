# ABOUTME: TDD tests for ISO 13623 international pipeline design code
# ABOUTME: Hand-calculation validation for pressure containment, collapse, propagation

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
from digitalmodel.structural.analysis.wall_thickness_codes.iso_13623 import Iso13623Strategy


def make_x65():
    return PipeMaterial(grade="X65", smys=448e6, smts=531e6)


def make_10inch(corrosion=0.001):
    return PipeGeometry(outer_diameter=0.27305, wall_thickness=0.0214, corrosion_allowance=corrosion)


class TestIso13623Registration:
    def test_iso_13623_registered_in_code_registry(self):
        assert DesignCode.ISO_13623 in CODE_REGISTRY

    def test_iso_13623_strategy_check_names(self):
        assert set(Iso13623Strategy.check_names) == {"pressure_containment", "collapse", "propagation"}

    def test_iso_13623_code_name(self):
        assert Iso13623Strategy.code_name == "ISO-13623"


class TestIso13623PressureContainment:
    def test_pressure_containment_moderate_pressure_safe(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=25e6, external_pressure=10e6)
        factors = DesignFactors()

        strategy = Iso13623Strategy()
        util, details = strategy._check_pressure_containment(geom, mat, loads)
        assert 0 < util < 1.0
        assert details["p_d"] > 0

    def test_pressure_containment_hand_calc_validation(self):
        """Validate against hand calculation.

        p_d = 2 * t * f_y * e_j / (D * gamma_s)
        t = 0.0214 - 0.001 = 0.0204 m (corroded)
        f_y = 448e6 Pa
        e_j = 1.0
        gamma_s = 1.39
        D = 0.27305 m

        p_d = 2 * 0.0204 * 448e6 * 1.0 / (0.27305 * 1.39)
            = 18278400 / 0.37954
            = 48.16 MPa

        p_net = 25e6 - 10e6 = 15e6
        util = 15 / 48.16 = 0.3115
        """
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=25e6, external_pressure=10e6)
        factors = DesignFactors()

        strategy = Iso13623Strategy()
        util, details = strategy._check_pressure_containment(geom, mat, loads)

        t = 0.0214 - 0.001
        D = 0.27305
        expected_p_d = 2 * t * 448e6 * 1.0 / (D * 1.39)
        expected_util = 15e6 / expected_p_d

        assert abs(details["p_d"] - expected_p_d) / expected_p_d < 0.001
        assert abs(util - expected_util) / expected_util < 0.001

    def test_pressure_containment_zero_diff_zero_utilisation(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=10e6, external_pressure=10e6)
        factors = DesignFactors()

        strategy = Iso13623Strategy()
        util, _ = strategy._check_pressure_containment(geom, mat, loads)
        assert abs(util) < 1e-10

    def test_pressure_containment_high_pressure_above_one(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=80e6, external_pressure=0.0)
        factors = DesignFactors()

        strategy = Iso13623Strategy()
        util, _ = strategy._check_pressure_containment(geom, mat, loads)
        assert util > 1.0


class TestIso13623Collapse:
    def test_collapse_pressures_positive(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=10e6)
        factors = DesignFactors()

        strategy = Iso13623Strategy()
        util, details = strategy._check_collapse(geom, mat, loads)
        assert details["p_el"] > 0
        assert details["p_p"] > 0
        assert details["p_c"] > 0

    def test_collapse_hand_calc_elastic_pressure(self):
        """Verify p_el = 2*E*(t/D)^3 / (1-nu^2).

        t = 0.0204 m (corroded), D = 0.27305 m
        """
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors()

        strategy = Iso13623Strategy()
        _, details = strategy._check_collapse(geom, mat, loads)

        t = 0.0214 - 0.001
        D = 0.27305
        expected_p_el = 2 * 207e9 * (t / D) ** 3 / (1 - 0.3**2)
        assert abs(details["p_el"] - expected_p_el) / expected_p_el < 0.001

    def test_collapse_design_factor_applied(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors()

        strategy = Iso13623Strategy()
        _, details = strategy._check_collapse(geom, mat, loads)
        assert abs(details["p_c_design"] - 0.80 * details["p_c"]) < 1.0

    def test_collapse_low_pressure_safe(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=2e6)
        factors = DesignFactors()

        strategy = Iso13623Strategy()
        util, _ = strategy._check_collapse(geom, mat, loads)
        assert util < 1.0


class TestIso13623Propagation:
    def test_propagation_hand_calc(self):
        """Verify p_pr = 24 * sigma_y * (t/D)^2.4.

        t = 0.0204 m (corroded), D = 0.27305 m
        """
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors()

        strategy = Iso13623Strategy()
        _, details = strategy._check_propagation(geom, mat, loads)

        t = 0.0214 - 0.001
        D = 0.27305
        expected_p_pr = 24 * 448e6 * (t / D) ** 2.4
        assert abs(details["p_pr"] - expected_p_pr) / expected_p_pr < 0.001

    def test_propagation_zero_external_zero_utilisation(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=0.0)
        factors = DesignFactors()

        strategy = Iso13623Strategy()
        util, _ = strategy._check_propagation(geom, mat, loads)
        assert abs(util) < 1e-10


class TestIso13623Integration:
    def test_perform_analysis_returns_three_checks(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)

        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.ISO_13623)
        result = analyzer.perform_analysis()
        assert len(result.checks) == 3
        assert set(result.checks.keys()) == {"pressure_containment", "collapse", "propagation"}

    def test_perform_analysis_safe_for_moderate_loads(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=15e6, external_pressure=3e6)
        factors = DesignFactors()

        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.ISO_13623)
        result = analyzer.perform_analysis()
        assert result.is_safe is True

    def test_perform_analysis_unsafe_for_extreme_pressure(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=80e6, external_pressure=0.0)
        factors = DesignFactors()

        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.ISO_13623)
        result = analyzer.perform_analysis()
        assert result.is_safe is False
