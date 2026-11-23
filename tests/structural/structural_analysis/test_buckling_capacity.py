# ABOUTME: Skeleton tests for structural_analysis buckling and capacity modules
# ABOUTME: Tests PlateBucklingAnalyzer, ColumnBucklingAnalyzer, MemberCapacityChecker

import math
import pytest

from digitalmodel.structural.structural_analysis.models import (
    BucklingResult,
    CapacityResult,
    MaterialProperties,
    PlateGeometry,
    STEEL_S275,
    STEEL_S355,
    STEEL_S420,
    StressState,
)


# ---------------------------------------------------------------------------
# Model dataclass tests
# ---------------------------------------------------------------------------
class TestStressState:
    def test_von_mises_uniaxial(self):
        state = StressState(sigma_x=100.0)
        vm = state.von_mises()
        assert vm == pytest.approx(100.0, rel=1e-4)

    def test_von_mises_biaxial_equal(self):
        # equal biaxial: VM = sqrt(0.5*((s-s)^2 + (s-0)^2 + (0-s)^2)) = s
        state = StressState(sigma_x=100.0, sigma_y=100.0)
        vm = state.von_mises()
        assert vm == pytest.approx(100.0, rel=1e-4)

    def test_von_mises_pure_shear(self):
        state = StressState(tau_xy=100.0)
        vm = state.von_mises()
        assert vm == pytest.approx(100.0 * math.sqrt(3), rel=1e-4)

    def test_principal_stresses_uniaxial(self):
        state = StressState(sigma_x=200.0)
        p1, p2, p3 = state.principal_stresses()
        assert p1 == pytest.approx(200.0, rel=1e-4)
        assert p2 == pytest.approx(0.0, abs=1e-4)
        assert p3 == pytest.approx(0.0, abs=1e-4)

    def test_max_shear_uniaxial(self):
        state = StressState(sigma_x=200.0)
        tau_max = state.max_shear()
        assert tau_max == pytest.approx(100.0, rel=1e-4)


class TestMaterialProperties:
    def test_predefined_s275(self):
        assert STEEL_S275.yield_strength == 275
        assert STEEL_S275.ultimate_strength == 430
        assert STEEL_S275.name == "S275"

    def test_predefined_s355(self):
        assert STEEL_S355.yield_strength == 355
        assert STEEL_S355.name == "S355"

    def test_predefined_s420(self):
        assert STEEL_S420.yield_strength == 420


class TestPlateGeometry:
    def test_creation(self):
        pg = PlateGeometry(length=1000.0, width=300.0, thickness=12.0)
        assert pg.length == 1000.0
        assert pg.width == 300.0
        assert pg.thickness == 12.0


class TestBucklingResult:
    def test_passes_when_util_le_1(self):
        r = BucklingResult(
            critical_stress=200.0,
            applied_stress=150.0,
            utilization=0.75,
            safety_factor=1.33,
            mode="plate_buckling",
            passes=True,
        )
        assert r.passes is True

    def test_fails_when_util_gt_1(self):
        r = BucklingResult(
            critical_stress=200.0,
            applied_stress=250.0,
            utilization=1.25,
            safety_factor=0.8,
            mode="column_buckling",
            passes=False,
        )
        assert r.passes is False


# ---------------------------------------------------------------------------
# PlateBucklingAnalyzer tests
# ---------------------------------------------------------------------------
class TestPlateBucklingAnalyzer:
    def _make(self, material=None):
        from digitalmodel.structural.structural_analysis.buckling import PlateBucklingAnalyzer
        return PlateBucklingAnalyzer(material or STEEL_S355)

    def test_elastic_buckling_stress_square_plate(self):
        analyzer = self._make()
        plate = PlateGeometry(length=600.0, width=600.0, thickness=10.0)
        sigma_e = analyzer.elastic_buckling_stress(plate)
        # For square plate, k=4, sigma_e = 4 * pi^2 * E / (12*(1-nu^2)) * (t/b)^2
        E = STEEL_S355.youngs_modulus
        nu = STEEL_S355.poissons_ratio
        expected = 4 * math.pi**2 * E / (12 * (1 - nu**2)) * (10 / 600) ** 2
        assert sigma_e == pytest.approx(expected, rel=1e-4)

    def test_elastic_buckling_stress_thin_plate_lower(self):
        analyzer = self._make()
        plate_thick = PlateGeometry(length=600, width=300, thickness=15.0)
        plate_thin = PlateGeometry(length=600, width=300, thickness=8.0)
        sigma_thick = analyzer.elastic_buckling_stress(plate_thick)
        sigma_thin = analyzer.elastic_buckling_stress(plate_thin)
        assert sigma_thick > sigma_thin

    def test_johnson_ostenfeld_elastic_regime(self):
        analyzer = self._make()
        fy = STEEL_S355.yield_strength  # 355 MPa
        sigma_e = 0.3 * fy  # < 0.5 * fy => elastic, no correction
        sigma_cr = analyzer.johnson_ostenfeld(sigma_e)
        assert sigma_cr == pytest.approx(sigma_e, rel=1e-6)

    def test_johnson_ostenfeld_inelastic_regime(self):
        analyzer = self._make()
        fy = STEEL_S355.yield_strength
        sigma_e = 0.8 * fy  # > 0.5 * fy => inelastic correction applied
        sigma_cr = analyzer.johnson_ostenfeld(sigma_e)
        expected = fy * (1 - fy / (4 * sigma_e))
        assert sigma_cr == pytest.approx(expected, rel=1e-6)

    def test_check_plate_buckling_returns_result(self):
        analyzer = self._make()
        plate = PlateGeometry(length=800, width=400, thickness=12.0)
        result = analyzer.check_plate_buckling(plate, sigma_x=100.0)
        assert isinstance(result, BucklingResult)
        assert result.utilization >= 0
        assert result.mode == "plate_buckling"

    def test_check_plate_buckling_zero_stress_passes(self):
        analyzer = self._make()
        plate = PlateGeometry(length=800, width=400, thickness=12.0)
        result = analyzer.check_plate_buckling(plate, sigma_x=0.0)
        assert result.utilization == pytest.approx(0.0, abs=1e-6)
        assert result.passes is True

    def test_reduced_slenderness_positive(self):
        analyzer = self._make()
        plate = PlateGeometry(length=600, width=300, thickness=10.0)
        lp = analyzer.reduced_slenderness(plate)
        assert lp > 0


# ---------------------------------------------------------------------------
# ColumnBucklingAnalyzer tests
# ---------------------------------------------------------------------------
class TestColumnBucklingAnalyzer:
    def _make(self, material=None):
        from digitalmodel.structural.structural_analysis.buckling import ColumnBucklingAnalyzer
        return ColumnBucklingAnalyzer(material or STEEL_S355)

    def test_euler_buckling_load(self):
        analyzer = self._make()
        # I = pi * d^4 / 64 for solid rod d=100mm
        d = 100.0
        I = math.pi * d**4 / 64
        L_eff = 3000.0
        E = STEEL_S355.youngs_modulus
        N_cr = analyzer.euler_buckling_load(I, L_eff)
        expected = math.pi**2 * E * I / L_eff**2
        assert N_cr == pytest.approx(expected, rel=1e-4)

    def test_slenderness_ratio(self):
        analyzer = self._make()
        L_eff = 4000.0
        r = 50.0
        assert analyzer.slenderness_ratio(L_eff, r) == pytest.approx(80.0, rel=1e-6)

    def test_reduction_factor_zero_slenderness(self):
        analyzer = self._make()
        chi = analyzer.reduction_factor(0.0)
        assert chi == pytest.approx(1.0, rel=1e-4)

    def test_reduction_factor_high_slenderness_low_chi(self):
        analyzer = self._make()
        chi = analyzer.reduction_factor(2.0)
        assert chi < 0.5

    def test_reduction_factor_curve_b_default(self):
        analyzer = self._make()
        chi_b = analyzer.reduction_factor(0.5, buckling_curve="b")
        chi_c = analyzer.reduction_factor(0.5, buckling_curve="c")
        assert chi_b > chi_c  # curve b is less conservative

    def test_check_column_buckling_returns_result(self):
        analyzer = self._make()
        # HEB 240 approximate section: A=10600 mm², I_min=3923e4 mm⁴
        A = 10600.0
        I_min = 3923e4
        L_eff = 4000.0
        N_applied = 500e3  # 500 kN
        result = analyzer.check_column_buckling(N_applied, A, I_min, L_eff)
        assert isinstance(result, BucklingResult)
        assert result.mode == "column_buckling"
        assert result.utilization >= 0

    def test_check_column_buckling_zero_force_passes(self):
        analyzer = self._make()
        A = 10600.0
        I_min = 3923e4
        result = analyzer.check_column_buckling(0.0, A, I_min, 4000.0)
        assert result.utilization == pytest.approx(0.0, abs=1e-6)
        assert result.passes


# ---------------------------------------------------------------------------
# MemberCapacityChecker tests
# ---------------------------------------------------------------------------
class TestMemberCapacityChecker:
    def _make(self, material=None):
        from digitalmodel.structural.structural_analysis.capacity import MemberCapacityChecker
        return MemberCapacityChecker(material or STEEL_S355)

    def test_tension_member_passes_low_force(self):
        checker = self._make()
        # Area = 6000 mm²  -> N_pl = 6000 * 355 = 2130 kN
        result = checker.check_tension_member(
            axial_force=500e3,
            area_gross=6000.0,
            area_net=5400.0,
        )
        assert isinstance(result, CapacityResult)
        assert result.utilization < 1.0
        assert result.passes is True

    def test_tension_member_fails_high_force(self):
        checker = self._make()
        result = checker.check_tension_member(
            axial_force=5000e3,
            area_gross=2000.0,
            area_net=1800.0,
        )
        assert result.utilization > 1.0
        assert result.passes is False

    def test_combined_loading_no_moments(self):
        checker = self._make()
        A = 10600.0
        W_pl = 500e3  # mm^3
        N_cr = 1e9
        result = checker.check_combined_loading(
            N=100e3, M_y=0.0, M_z=0.0,
            area=A, W_pl_y=W_pl, W_pl_z=W_pl,
            N_cr_y=N_cr, N_cr_z=N_cr,
        )
        assert isinstance(result, CapacityResult)
        assert result.utilization >= 0
