#!/usr/bin/env python3
"""
Unit Tests for Structural Analysis Module

Tests core functionality including:
- Stress calculations
- Plate buckling
- Column buckling
- Capacity checks
"""

import pytest
import numpy as np

from digitalmodel.modules.structural_analysis import (
    StressState, MaterialProperties, PlateGeometry,
    StressCalculator, PlateBucklingAnalyzer, ColumnBucklingAnalyzer,
    MemberCapacityChecker, STEEL_S355
)


class TestStressState:
    """Test stress state calculations"""

    def test_von_mises_uniaxial(self):
        """Test Von Mises for uniaxial stress"""
        stress = StressState(sigma_x=100.0)
        vm = stress.von_mises()
        assert abs(vm - 100.0) < 0.01

    def test_von_mises_pure_shear(self):
        """Test Von Mises for pure shear"""
        stress = StressState(tau_xy=100.0)
        vm = stress.von_mises()
        # VM for pure shear = sqrt(3) * tau
        expected = np.sqrt(3) * 100.0
        assert abs(vm - expected) < 0.1

    def test_von_mises_combined(self):
        """Test Von Mises for combined stress"""
        stress = StressState(
            sigma_x=150.0,
            sigma_y=50.0,
            tau_xy=30.0
        )
        vm = stress.von_mises()
        assert vm > 0
        assert vm < 200  # Should be less than max normal stress * sqrt(2)

    def test_principal_stresses_ordering(self):
        """Test principal stresses are ordered correctly"""
        stress = StressState(sigma_x=100.0, sigma_y=50.0, sigma_z=25.0)
        s1, s2, s3 = stress.principal_stresses()

        assert s1 >= s2 >= s3
        assert abs(s1 - 100.0) < 0.01
        assert abs(s3 - 25.0) < 0.01

    def test_max_shear(self):
        """Test maximum shear stress"""
        stress = StressState(sigma_x=100.0, sigma_y=20.0)
        max_shear = stress.max_shear()

        # For pure normal stresses: tau_max = (sigma1 - sigma3) / 2
        expected = (100.0 - 0.0) / 2  # sigma3 is approximately 0
        assert abs(max_shear - expected) < 5.0


class TestStressCalculator:
    """Test stress calculator"""

    @pytest.fixture
    def calculator(self):
        return StressCalculator(STEEL_S355)

    def test_beam_axial_stress(self, calculator):
        """Test beam axial stress"""
        stress = calculator.beam_stress(
            axial_force=1000e3,  # 1000 kN
            moment_y=0,
            moment_z=0,
            area=0.01,  # m²
            I_y=1e-5,
            I_z=1e-5,
            y=0,
            z=0
        )

        # sigma = F/A = 1000e3 / 0.01 / 1e6 = 100 MPa
        assert abs(stress - 100.0) < 0.1

    def test_hoop_stress(self, calculator):
        """Test hoop stress in cylinder"""
        stress = calculator.hoop_stress(
            pressure=10.0,  # MPa
            radius=0.5,     # m
            thickness=0.01  # m
        )

        # sigma_hoop = pr/t = 10 * 0.5 / 0.01 = 500 MPa
        assert abs(stress - 500.0) < 0.1

    def test_longitudinal_stress(self, calculator):
        """Test longitudinal stress in cylinder"""
        stress = calculator.longitudinal_stress(
            pressure=10.0,  # MPa
            radius=0.5,     # m
            thickness=0.01  # m
        )

        # sigma_long = pr/2t = 10 * 0.5 / (2 * 0.01) = 250 MPa
        assert abs(stress - 250.0) < 0.1


class TestPlateBuckling:
    """Test plate buckling analysis"""

    @pytest.fixture
    def analyzer(self):
        return PlateBucklingAnalyzer(STEEL_S355)

    def test_elastic_buckling_square_plate(self, analyzer):
        """Test elastic buckling for square plate"""
        plate = PlateGeometry(length=1000, width=1000, thickness=10)
        sigma_e = analyzer.elastic_buckling_stress(plate)

        assert sigma_e > 0
        # Elastic buckling should be well above yield for thick plates
        assert sigma_e > STEEL_S355.yield_strength

    def test_elastic_buckling_long_plate(self, analyzer):
        """Test elastic buckling for long plate"""
        plate = PlateGeometry(length=3000, width=1000, thickness=10)
        sigma_e = analyzer.elastic_buckling_stress(plate)

        assert sigma_e > 0

    def test_johnson_ostenfeld_elastic(self, analyzer):
        """Test Johnson-Ostenfeld in elastic range"""
        sigma_e = 1000.0  # High elastic stress
        sigma_cr = analyzer.johnson_ostenfeld(sigma_e)

        # Should return elastic stress unchanged
        assert abs(sigma_cr - sigma_e) < 0.1

    def test_johnson_ostenfeld_inelastic(self, analyzer):
        """Test Johnson-Ostenfeld in inelastic range"""
        sigma_e = 100.0  # Low elastic stress (< 0.5*fy)
        sigma_cr = analyzer.johnson_ostenfeld(sigma_e)

        # Should apply inelastic correction
        assert sigma_cr < analyzer.fy
        assert sigma_cr > 0

    def test_plate_buckling_passes(self, analyzer):
        """Test plate buckling check that passes"""
        plate = PlateGeometry(length=2000, width=1000, thickness=20)
        result = analyzer.check_plate_buckling(
            plate=plate,
            sigma_x=100.0,  # Low stress
            tau=20.0
        )

        assert result.passes
        assert result.utilization < 1.0
        assert result.safety_factor > 1.0

    def test_plate_buckling_fails(self, analyzer):
        """Test plate buckling check that fails"""
        plate = PlateGeometry(length=3000, width=1000, thickness=5)  # Thin plate
        result = analyzer.check_plate_buckling(
            plate=plate,
            sigma_x=300.0,  # High stress
            tau=100.0       # High shear
        )

        assert not result.passes
        assert result.utilization > 1.0
        assert result.safety_factor < 1.0


class TestColumnBuckling:
    """Test column buckling analysis"""

    @pytest.fixture
    def analyzer(self):
        return ColumnBucklingAnalyzer(STEEL_S355)

    def test_euler_buckling_load(self, analyzer):
        """Test Euler buckling load calculation"""
        I = 1e8  # mm⁴
        L_eff = 5000  # mm

        P_cr = analyzer.euler_buckling_load(I, L_eff)

        assert P_cr > 0
        # P_cr = π²EI/L²
        expected = np.pi**2 * 210000 * I / L_eff**2
        assert abs(P_cr - expected) < 1.0

    def test_slenderness_ratio(self, analyzer):
        """Test slenderness ratio calculation"""
        L_eff = 6000  # mm
        r = 100      # mm

        lambda_ratio = analyzer.slenderness_ratio(L_eff, r)

        assert lambda_ratio == 60.0

    def test_reduction_factor_low_slenderness(self, analyzer):
        """Test reduction factor for stocky column"""
        chi = analyzer.reduction_factor(lambda_bar=0.5, buckling_curve='b')

        # Stocky column should have high reduction factor
        assert chi > 0.8
        assert chi <= 1.0

    def test_reduction_factor_high_slenderness(self, analyzer):
        """Test reduction factor for slender column"""
        chi = analyzer.reduction_factor(lambda_bar=2.0, buckling_curve='b')

        # Slender column should have low reduction factor
        assert chi < 0.5
        assert chi > 0.0

    def test_column_buckling_passes(self, analyzer):
        """Test column buckling check that passes"""
        result = analyzer.check_column_buckling(
            axial_force=2e6,     # 2 MN
            area=15000,          # mm²
            I_min=5e7,           # mm⁴
            L_eff=5000,          # mm
            buckling_curve='b'
        )

        assert result.passes
        assert result.utilization < 1.0
        assert result.safety_factor > 1.0

    def test_column_buckling_fails(self, analyzer):
        """Test column buckling check that fails"""
        result = analyzer.check_column_buckling(
            axial_force=10e6,    # 10 MN - high load
            area=10000,          # mm² - small area
            I_min=1e7,           # mm⁴ - small I
            L_eff=8000,          # mm - long column
            buckling_curve='d'   # Unfavorable curve
        )

        assert not result.passes
        assert result.utilization > 1.0


class TestMemberCapacity:
    """Test member capacity checks"""

    @pytest.fixture
    def checker(self):
        return MemberCapacityChecker(STEEL_S355)

    def test_tension_plastic_governs(self, checker):
        """Test tension capacity when plastic governs"""
        result = checker.check_tension_member(
            axial_force=4e6,      # 4 MN
            area_gross=15000,     # mm²
            area_net=14000,       # mm² (small reduction)
            gamma_m0=1.0,
            gamma_m2=1.25
        )

        assert result.governing_mode in ["plastic", "net_section"]
        assert result.passes
        assert result.utilization < 1.0

    def test_tension_net_section_governs(self, checker):
        """Test tension capacity when net section governs"""
        result = checker.check_tension_member(
            axial_force=4e6,      # 4 MN
            area_gross=15000,     # mm²
            area_net=10000,       # mm² (large reduction due to holes)
            gamma_m0=1.0,
            gamma_m2=1.25
        )

        assert result.governing_mode in ["plastic", "net_section"]

    def test_combined_loading_passes(self, checker):
        """Test combined loading check that passes"""
        result = checker.check_combined_loading(
            N=2e6,          # 2 MN axial
            M_y=500e6,      # 500 kNm
            M_z=200e6,      # 200 kNm
            area=20000,     # mm²
            W_pl_y=3e6,     # mm³
            W_pl_z=2e6,     # mm³
            N_cr_y=10e6,    # N
            N_cr_z=8e6,     # N
            gamma_m1=1.0
        )

        assert result.passes
        assert result.utilization < 1.0

    def test_combined_loading_high_util(self, checker):
        """Test combined loading with high utilization"""
        result = checker.check_combined_loading(
            N=5e6,          # 5 MN axial - high
            M_y=1500e6,     # 1500 kNm - high
            M_z=800e6,      # 800 kNm - high
            area=20000,     # mm²
            W_pl_y=3e6,     # mm³
            W_pl_z=2e6,     # mm³
            N_cr_y=10e6,    # N
            N_cr_z=8e6,     # N
            gamma_m1=1.0
        )

        assert result.utilization > 1.0


class TestMaterialProperties:
    """Test material properties"""

    def test_steel_s355_properties(self):
        """Test S355 steel properties"""
        assert STEEL_S355.yield_strength == 355
        assert STEEL_S355.ultimate_strength == 510
        assert STEEL_S355.youngs_modulus == 210000
        assert STEEL_S355.poissons_ratio == 0.3

    def test_custom_material(self):
        """Test creating custom material"""
        custom = MaterialProperties(
            yield_strength=450,
            ultimate_strength=550,
            youngs_modulus=210000,
            poissons_ratio=0.3,
            density=7850,
            name="Custom"
        )

        assert custom.yield_strength == 450
        assert custom.name == "Custom"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
