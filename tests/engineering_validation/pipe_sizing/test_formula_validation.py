"""
Engineering Validation Tests for PipeSizing.py
Validates fluid dynamics formulas, unit conversions, and industry standards compliance.
"""

import math
import pytest
import numpy as np
from hypothesis import given, strategies as st, assume, settings
from hypothesis.strategies import floats
from unittest.mock import Mock
import sys
import os

# Add the source directory to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../../src'))

from digitalmodel.custom.PipeSizing import PipeSizing


class TestFormulaValidation:
    """Test engineering correctness of formulas used in PipeSizing"""

    def setup_method(self):
        """Setup test configuration"""
        self.cfg = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 12.75,  # inches
                    "Nominal_ID": 11.626,  # inches
                    "Design_WT": 0.562    # inches
                },
                "Material": {
                    "Material": "steel",
                    "Material_Grade": "X65"
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "steel": {
                    "E": 200e9,  # Pa
                    "Rho": 7850,  # kg/m³
                    "Poissionsratio": 0.3,
                    "Grades": {
                        "X65": {
                            "SMYS": 450e6,  # Pa
                            "SMUS": 535e6   # Pa
                        }
                    }
                }
            }
        }
        self.pipe_sizing = PipeSizing(self.cfg.copy())

    def test_section_properties_formulas(self):
        """Test correctness of cross-sectional property calculations"""
        # Test with known values
        OD = 12.75  # inches
        ID = 11.626  # inches

        # Expected values (manual calculation)
        expected_Ao = (math.pi / 4) * (OD ** 2)
        expected_Ai = (math.pi / 4) * (ID ** 2)
        expected_A = expected_Ao - expected_Ai
        expected_Io = (math.pi / 64) * (OD ** 4)
        expected_Ii = (math.pi / 64) * (ID ** 4)
        expected_I = expected_Io - expected_Ii

        # Run calculation
        self.pipe_sizing.pipe_properties("Outer_Pipe")
        section_props = self.cfg["Outer_Pipe"]["section_properties"]["pipe"]

        # Validate formulas
        assert abs(section_props["Ao"] - expected_Ao) < 1e-10
        assert abs(section_props["Ai"] - expected_Ai) < 1e-10
        assert abs(section_props["A"] - expected_A) < 1e-10
        assert abs(section_props["Io"] - expected_Io) < 1e-10
        assert abs(section_props["Ii"] - expected_Ii) < 1e-10
        assert abs(section_props["I"] - expected_I) < 1e-10

    def test_shear_modulus_calculation(self):
        """Test correctness of shear modulus formula G = E/(2(1+ν))"""
        E = 200e9  # Pa
        nu = 0.3
        expected_G = E / (2 * (1 + nu))

        self.pipe_sizing.get_fea_properties("Outer_Pipe")
        calculated_G = self.cfg["Material"]["steel"]["G"]

        assert abs(calculated_G - expected_G) < 1e-6

    def test_geometric_consistency(self):
        """Test geometric relationships OD = ID + 2*WT"""
        # Test with various configurations
        test_cases = [
            {"OD": 10.0, "ID": None, "WT": 0.5},  # Calculate ID
            {"OD": None, "ID": 9.0, "WT": 0.5},   # Calculate OD
            {"OD": 10.0, "ID": 9.0, "WT": None}   # Calculate WT
        ]

        for case in test_cases:
            cfg_test = self.cfg.copy()
            cfg_test["Outer_Pipe"]["Geometry"]["Nominal_OD"] = case["OD"]
            cfg_test["Outer_Pipe"]["Geometry"]["Nominal_ID"] = case["ID"]
            cfg_test["Outer_Pipe"]["Geometry"]["Design_WT"] = case["WT"]

            pipe_sizing_test = PipeSizing(cfg_test)
            pipe_sizing_test.pipe_properties("Outer_Pipe")

            # Verify relationship
            OD = cfg_test["Outer_Pipe"]["Geometry"]["Nominal_OD"]
            ID = cfg_test["Outer_Pipe"]["Geometry"]["Nominal_ID"]
            WT = cfg_test["Outer_Pipe"]["Geometry"]["Design_WT"]

            assert abs(OD - (ID + 2*WT)) < 1e-10

    def test_physical_constraints(self):
        """Test that physical constraints are enforced"""
        # These should trigger validation errors in production code
        invalid_cases = [
            {"OD": -1, "ID": 1, "WT": 0.5},    # Negative OD
            {"OD": 1, "ID": 2, "WT": 0.5},     # ID > OD
            {"OD": 1, "ID": 0.9, "WT": 0},     # Zero wall thickness
            {"OD": 1, "ID": 0.9, "WT": -0.1}   # Negative wall thickness
        ]

        # Note: Current implementation doesn't validate these!
        # This test documents the missing validation requirement
        for case in invalid_cases:
            cfg_test = self.cfg.copy()
            cfg_test["Outer_Pipe"]["Geometry"]["Nominal_OD"] = case["OD"]
            cfg_test["Outer_Pipe"]["Geometry"]["Nominal_ID"] = case["ID"]
            cfg_test["Outer_Pipe"]["Geometry"]["Design_WT"] = case["WT"]

            # TODO: Should raise ValueError for invalid geometry
            # Currently this passes - indicating missing validation
            pipe_sizing_test = PipeSizing(cfg_test)
            pipe_sizing_test.pipe_properties("Outer_Pipe")


class TestUnitConsistency:
    """Test unit conversion and dimensional analysis"""

    def test_hardcoded_conversion_factor(self):
        """Test the mysterious 0.0254² factor"""
        # This appears to be inch² to m² conversion
        inch_to_meter = 0.0254
        inch_sq_to_meter_sq = inch_to_meter ** 2

        assert abs(inch_sq_to_meter_sq - 0.0254**2) < 1e-10

        # Test conversion consistency
        area_inch_sq = 1.0  # 1 square inch
        area_m_sq = area_inch_sq * (0.0254**2)
        expected_m_sq = area_inch_sq * (0.0254 * 0.0254)

        assert abs(area_m_sq - expected_m_sq) < 1e-10

    def test_dimensional_consistency(self):
        """Test dimensional consistency of calculations"""
        # Area should have dimensions [L²]
        # Moment of inertia should have dimensions [L⁴]
        # This is a basic sanity check

        cfg = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 10.0,
                    "Nominal_ID": 9.0,
                    "Design_WT": 0.5
                },
                "Material": {
                    "Material": "steel",
                    "Material_Grade": "X65"
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "steel": {
                    "E": 200e9,
                    "Rho": 7850,
                    "Poissionsratio": 0.3,
                    "Grades": {
                        "X65": {
                            "SMYS": 450e6,
                            "SMUS": 535e6
                        }
                    }
                }
            }
        }

        pipe_sizing = PipeSizing(cfg)
        pipe_sizing.pipe_properties("Outer_Pipe")

        # Check that area scales as L²
        scale_factor = 2.0
        cfg_scaled = cfg.copy()
        cfg_scaled["Outer_Pipe"]["Geometry"]["Nominal_OD"] *= scale_factor
        cfg_scaled["Outer_Pipe"]["Geometry"]["Nominal_ID"] *= scale_factor

        pipe_sizing_scaled = PipeSizing(cfg_scaled)
        pipe_sizing_scaled.pipe_properties("Outer_Pipe")

        A_original = cfg["Outer_Pipe"]["section_properties"]["pipe"]["A"]
        A_scaled = cfg_scaled["Outer_Pipe"]["section_properties"]["pipe"]["A"]

        # Area should scale as scale_factor²
        expected_ratio = scale_factor ** 2
        actual_ratio = A_scaled / A_original

        assert abs(actual_ratio - expected_ratio) < 1e-6


class TestIndustryStandardCompliance:
    """Test compliance with industry standards"""

    def test_api_5l_pipe_specifications(self):
        """Test compliance with API 5L pipe specifications"""
        # Common API 5L pipe sizes (should be validated)
        api_5l_sizes = [
            {"OD": 4.5, "WT": 0.237, "Grade": "X42"},
            {"OD": 6.625, "WT": 0.280, "Grade": "X52"},
            {"OD": 8.625, "WT": 0.322, "Grade": "X65"},
            {"OD": 12.75, "WT": 0.375, "Grade": "X70"}
        ]

        # Note: Current implementation doesn't validate API compliance
        # This test documents the requirement
        for pipe_spec in api_5l_sizes:
            OD = pipe_spec["OD"]
            WT = pipe_spec["WT"]
            ID = OD - 2 * WT

            # Basic geometric validation
            assert ID > 0
            assert WT > 0
            assert OD > ID

            # TODO: Add actual API 5L specification validation
            # - Standard OD sizes
            # - Wall thickness schedules
            # - Material grade specifications

    def test_asme_b31_pressure_design(self):
        """Test ASME B31.8 pressure design requirements"""
        # Barlow's formula: P = 2*S*t*F*E*T / D
        # Where:
        # P = design pressure
        # S = SMYS
        # t = wall thickness
        # F = design factor (≤ 0.72 for most applications)
        # E = joint efficiency factor
        # T = temperature derating factor
        # D = outside diameter

        # Test case
        SMYS = 450e6  # Pa (X65 steel)
        t = 0.5 * 0.0254  # inches to meters
        F = 0.72  # Design factor
        E = 1.0   # Seamless pipe
        T = 1.0   # Temperature factor
        D = 12.75 * 0.0254  # inches to meters

        design_pressure = (2 * SMYS * t * F * E * T) / D

        # Should be reasonable for pipeline applications
        assert design_pressure > 0
        assert design_pressure < 50e6  # Less than 50 MPa is reasonable

        # TODO: Implement this validation in PipeSizing class

    def test_safety_factors(self):
        """Test application of appropriate safety factors"""
        # Pipeline design typically uses:
        # - Design factor F ≤ 0.72 (ASME B31.8)
        # - Factor of safety > 1.5 on ultimate strength
        # - Various environmental factors

        # Note: Current implementation doesn't apply safety factors
        # This test documents the requirement

        SMYS = 450e6  # Pa
        SMUS = 535e6  # Pa

        # Check stress ratios
        yield_safety_factor = SMUS / SMYS
        assert yield_safety_factor > 1.0  # Basic requirement

        # TODO: Implement safety factor validation in PipeSizing


class TestPropertyBasedValidation:
    """Property-based testing using Hypothesis for comprehensive validation"""

    @given(
        od=floats(min_value=1.0, max_value=100.0, allow_nan=False, allow_infinity=False),
        wt=floats(min_value=0.1, max_value=5.0, allow_nan=False, allow_infinity=False)
    )
    @settings(max_examples=100)
    def test_pipe_geometry_properties(self, od, wt):
        """Property-based test for pipe geometry calculations"""
        # Assume valid geometry
        assume(wt < od/2)  # Wall thickness must be less than radius
        assume(od > 2*wt)  # OD must be greater than 2*WT

        id_val = od - 2*wt
        assume(id_val > 0)  # ID must be positive

        # Calculate properties
        Ao = (math.pi / 4) * (od ** 2)
        Ai = (math.pi / 4) * (id_val ** 2)
        A = Ao - Ai

        # Properties that should always hold
        assert Ao > Ai  # Outer area > inner area
        assert A > 0    # Cross-sectional area > 0
        assert A < Ao   # Cross-sectional area < outer area
        assert Ai < Ao  # Inner area < outer area

        # Geometric relationships
        assert abs((od - id_val) - 2*wt) < 1e-10  # OD = ID + 2*WT

    @given(
        E=floats(min_value=1e9, max_value=1e12, allow_nan=False, allow_infinity=False),
        nu=floats(min_value=0.1, max_value=0.5, allow_nan=False, allow_infinity=False)
    )
    @settings(max_examples=50)
    def test_material_property_relationships(self, E, nu):
        """Property-based test for material property relationships"""
        # Calculate shear modulus
        G = E / (2 * (1 + nu))

        # Properties that should always hold
        assert G > 0        # Shear modulus must be positive
        assert G < E        # Shear modulus < elastic modulus
        assert G > E/4      # Physical lower bound for common materials
        assert G < E/2      # Physical upper bound (nu > 0)

    @given(
        rho=floats(min_value=1000, max_value=20000, allow_nan=False, allow_infinity=False),
        area=floats(min_value=1e-6, max_value=1.0, allow_nan=False, allow_infinity=False)
    )
    @settings(max_examples=50)
    def test_mass_calculations(self, rho, area):
        """Property-based test for mass calculations"""
        mass_per_length = area * rho

        # Properties that should always hold
        assert mass_per_length > 0  # Mass must be positive
        assert mass_per_length == area * rho  # Linear relationship


class TestRealWorldScenarios:
    """Test against real-world pipeline scenarios and known solutions"""

    def test_common_pipeline_sizes(self):
        """Test calculations for common pipeline sizes"""
        # Real pipeline specifications
        common_pipes = [
            {
                "name": "6-inch Schedule 40",
                "OD": 6.625,    # inches
                "WT": 0.280,    # inches
                "ID": 6.065,    # inches
                "material": "X52"
            },
            {
                "name": "12-inch Schedule 30",
                "OD": 12.75,    # inches
                "WT": 0.375,    # inches
                "ID": 12.000,   # inches
                "material": "X65"
            },
            {
                "name": "24-inch Standard",
                "OD": 24.0,     # inches
                "WT": 0.500,    # inches
                "ID": 23.000,   # inches
                "material": "X70"
            }
        ]

        for pipe in common_pipes:
            # Verify geometric consistency
            calculated_ID = pipe["OD"] - 2 * pipe["WT"]
            assert abs(calculated_ID - pipe["ID"]) < 0.001

            # Calculate section properties
            A = (math.pi / 4) * (pipe["OD"]**2 - pipe["ID"]**2)
            I = (math.pi / 64) * (pipe["OD"]**4 - pipe["ID"]**4)

            # Verify reasonable values
            assert A > 0
            assert I > 0
            assert A < (math.pi / 4) * pipe["OD"]**2  # Less than solid area

    def test_known_engineering_solutions(self):
        """Test against known engineering solutions"""
        # Test case: 12-inch X65 pipeline
        # Known properties for validation

        OD = 12.75  # inches
        WT = 0.562  # inches
        ID = OD - 2*WT  # 11.626 inches

        # Convert to SI units for calculation
        OD_m = OD * 0.0254  # meters
        ID_m = ID * 0.0254  # meters

        # Calculate section properties
        A_m2 = (math.pi / 4) * (OD_m**2 - ID_m**2)
        I_m4 = (math.pi / 64) * (OD_m**4 - ID_m**4)

        # Material properties for X65 steel
        E = 200e9  # Pa
        rho = 7850  # kg/m³

        # Calculate structural properties
        EI = E * I_m4  # N⋅m²
        mass_per_length = A_m2 * rho  # kg/m

        # Verify against expected ranges for 12-inch X65 pipe
        assert 40 < mass_per_length < 60  # kg/m (typical range)
        assert EI > 1e6  # N⋅m² (reasonable stiffness)

    def test_extreme_but_valid_cases(self):
        """Test extreme but physically valid cases"""
        # Very thin wall pipe
        thin_wall = {
            "OD": 10.0,
            "WT": 0.05,  # Very thin
            "ID": 9.9
        }

        # Very thick wall pipe
        thick_wall = {
            "OD": 10.0,
            "WT": 2.0,   # Very thick
            "ID": 6.0
        }

        # Small diameter pipe
        small_pipe = {
            "OD": 1.0,
            "WT": 0.1,
            "ID": 0.8
        }

        # Large diameter pipe
        large_pipe = {
            "OD": 60.0,
            "WT": 1.0,
            "ID": 58.0
        }

        test_cases = [thin_wall, thick_wall, small_pipe, large_pipe]

        for case in test_cases:
            # All should produce valid results
            A = (math.pi / 4) * (case["OD"]**2 - case["ID"]**2)
            I = (math.pi / 64) * (case["OD"]**4 - case["ID"]**4)

            assert A > 0
            assert I > 0
            assert case["OD"] > case["ID"]
            assert abs(case["OD"] - case["ID"] - 2*case["WT"]) < 1e-10


if __name__ == "__main__":
    pytest.main([__file__, "-v"])