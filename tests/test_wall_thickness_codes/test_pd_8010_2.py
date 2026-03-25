# ABOUTME: TDD tests for PD 8010-2 UK offshore pipeline design code
# ABOUTME: Hand-calculation validation for hoop stress, collapse, propagation

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
from digitalmodel.structural.analysis.wall_thickness_codes.pd_8010_2 import Pd80102Strategy


def make_x65():
    return PipeMaterial(grade="X65", smys=448e6, smts=531e6)


def make_10inch(corrosion=0.001):
    return PipeGeometry(outer_diameter=0.27305, wall_thickness=0.0214, corrosion_allowance=corrosion)


class TestPd80102Registration:
    def test_pd_8010_2_registered_in_code_registry(self):
        assert DesignCode.PD_8010_2 in CODE_REGISTRY

    def test_pd_8010_2_strategy_check_names(self):
        assert set(Pd80102Strategy.check_names) == {"hoop_stress", "collapse", "propagation"}

    def test_pd_8010_2_code_name(self):
        assert Pd80102Strategy.code_name == "PD-8010-2"


class TestPd80102HoopStress:
    def test_hoop_stress_moderate_pressure_safe(self):
        """10" X65 pipe at 25 MPa internal, 10 MPa external should be safe."""
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=25e6, external_pressure=10e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)

        strategy = Pd80102Strategy()
        util, details = strategy._check_hoop_stress(geom, mat, loads)
        assert 0 < util < 1.0
        assert details["sigma_h"] > 0
        assert details["sigma_allow"] > 0

    def test_hoop_stress_hand_calc_validation(self):
        """Validate hoop stress against hand calculation.

        sigma_h = p_d * D / (2 * t_min)
        t_min = 0.0214 - 0.001 = 0.0204 m
        p_d = 25e6 - 10e6 = 15e6 Pa
        sigma_h = 15e6 * 0.27305 / (2 * 0.0204) = 100.48 MPa
        sigma_allow = 0.72 * 448 = 322.56 MPa
        util = 100.48 / 322.56 = 0.3114
        """
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=25e6, external_pressure=10e6)
        factors = DesignFactors()

        strategy = Pd80102Strategy()
        util, details = strategy._check_hoop_stress(geom, mat, loads)

        t_min = 0.0214 - 0.001
        p_d = 15e6
        expected_sigma_h = p_d * 0.27305 / (2 * t_min)
        expected_allow = 0.72 * 448e6
        expected_util = expected_sigma_h / expected_allow

        assert abs(details["sigma_h"] - expected_sigma_h) / expected_sigma_h < 0.001
        assert abs(details["sigma_allow"] - expected_allow) / expected_allow < 0.001
        assert abs(util - expected_util) / expected_util < 0.001

    def test_hoop_stress_zero_pressure_diff_zero_utilisation(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=10e6, external_pressure=10e6)
        factors = DesignFactors()

        strategy = Pd80102Strategy()
        util, _ = strategy._check_hoop_stress(geom, mat, loads)
        assert abs(util) < 1e-10

    def test_hoop_stress_high_pressure_above_one(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=80e6, external_pressure=0.0)
        factors = DesignFactors()

        strategy = Pd80102Strategy()
        util, _ = strategy._check_hoop_stress(geom, mat, loads)
        assert util > 1.0


class TestPd80102Collapse:
    def test_collapse_pressures_positive(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=10e6)
        factors = DesignFactors()

        strategy = Pd80102Strategy()
        util, details = strategy._check_collapse(geom, mat, loads)
        assert details["p_el"] > 0
        assert details["p_p"] > 0
        assert details["p_c"] > 0

    def test_collapse_hand_calc_elastic_pressure(self):
        """Verify p_el = 2*E*(t/D)^3 / (1-nu^2).

        t = 0.0214 - 0.001 = 0.0204 m
        p_el = 2*207e9*(0.0204/0.27305)^3 / (1-0.3^2)
        """
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors()

        strategy = Pd80102Strategy()
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

        strategy = Pd80102Strategy()
        _, details = strategy._check_collapse(geom, mat, loads)
        assert abs(details["p_c_design"] - 0.80 * details["p_c"]) < 1.0

    def test_collapse_low_pressure_safe(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=2e6)
        factors = DesignFactors()

        strategy = Pd80102Strategy()
        util, _ = strategy._check_collapse(geom, mat, loads)
        assert util < 1.0


class TestPd80102Propagation:
    def test_propagation_hand_calc(self):
        """Verify p_pr = 24 * sigma_y * (t/D)^2.4.

        t = 0.0214 - 0.001 = 0.0204 m
        p_pr = 24 * 448e6 * (0.0204/0.27305)^2.4
        """
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors()

        strategy = Pd80102Strategy()
        _, details = strategy._check_propagation(geom, mat, loads)

        t = 0.0214 - 0.001
        D = 0.27305
        expected_p_pr = 24 * 448e6 * (t / D) ** 2.4
        assert abs(details["p_pr"] - expected_p_pr) / expected_p_pr < 0.001

    def test_propagation_design_factor(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors()

        strategy = Pd80102Strategy()
        _, details = strategy._check_propagation(geom, mat, loads)
        assert abs(details["p_pr_design"] - 0.80 * details["p_pr"]) < 1.0

    def test_propagation_zero_external_zero_utilisation(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=0.0)
        factors = DesignFactors()

        strategy = Pd80102Strategy()
        util, _ = strategy._check_propagation(geom, mat, loads)
        assert abs(util) < 1e-10


class TestPd80102Integration:
    def test_perform_analysis_returns_three_checks(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors(safety_class=SafetyClass.MEDIUM)

        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.PD_8010_2)
        result = analyzer.perform_analysis()
        assert len(result.checks) == 3
        assert set(result.checks.keys()) == {"hoop_stress", "collapse", "propagation"}

    def test_perform_analysis_safe_for_moderate_loads(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=15e6, external_pressure=3e6)
        factors = DesignFactors()

        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.PD_8010_2)
        result = analyzer.perform_analysis()
        assert result.is_safe is True

    def test_perform_analysis_unsafe_for_extreme_pressure(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=80e6, external_pressure=0.0)
        factors = DesignFactors()

        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.PD_8010_2)
        result = analyzer.perform_analysis()
        assert result.is_safe is False
