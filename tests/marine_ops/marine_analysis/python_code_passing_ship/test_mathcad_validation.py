"""
Validation tests comparing Python implementation against original MathCAD calculations.

This test suite validates the passing ship force and moment calculations against
the reference values from Wang's MathCAD implementation documented in:
'docs/modules/ship_design/passing_ship/Calculation of forces and moments from Wang.pdf'

Reference Test Case (from MathCAD):
-----------------------------------
Moored Vessel:
  - Length (L): 950 ft
  - Midship cross-sectional area (A1): 3192 ft²

Passing Vessel:
  - Length (L2): 0.5 × L = 475 ft
  - Midship cross-sectional area (A2): 6413 ft²

Environment:
  - Water density (ρ): 1.9905 slug/ft³
  - Passing ship velocity (U): 11.2 ft/s
  - Separation distance: 0.2 × L = 190 ft
  - Stagger distance: 0 × L = 0 ft
  - Water depth (D): 0.1 × L = 95 ft

Expected Results (from MathCAD):
  - Surge Force: 1.016 × 10⁻¹¹ lbf (effectively zero)
  - Sway Force: 7.644 × 10⁴ lbf = 76,440 lbf
  - Yaw Moment: 7.851 × 10⁻⁹ ft-lbf (effectively zero)
"""

import pytest
import numpy as np
import sys
from pathlib import Path

# Import the package - assumes digitalmodel is installed or PYTHONPATH includes src/
pytest.importorskip(
    "digitalmodel.marine_ops.marine_analysis.python_mathcad",
    reason="module digitalmodel.marine_ops.marine_analysis.python_mathcad not available"
)
from digitalmodel.marine_ops.marine_analysis.python_mathcad.calculator import PassingShipCalculator


class TestMathCADValidation:
    """Validation tests against MathCAD reference implementation."""

    @pytest.fixture
    def mathcad_reference_params(self):
        """
        Returns the exact parameters used in the MathCAD reference calculation.

        All values are in Imperial units (ft, slug, lbf) to match MathCAD.
        """
        return {
            # Moored vessel
            'L': 950.0,              # Length in feet
            'A1': 3192.0,            # Midship cross-sectional area in ft²

            # Passing vessel
            'L2': 475.0,             # 0.5 × L = 475 ft
            'A2': 6413.0,            # Midship cross-sectional area in ft²

            # Environment
            'rho': 1.9905,           # Water density in slug/ft³
            'U': 11.2,               # Passing ship velocity in ft/s
            'SEP_DIST': 190.0,       # 0.2 × L = 190 ft
            'STA_DIST': 0.0,         # 0 × L = 0 ft
            'D': 95.0,               # 0.1 × L = 95 ft (water depth)
        }

    @pytest.fixture
    def mathcad_expected_results(self):
        """
        Expected results from MathCAD calculation.

        Returns:
            dict: Expected surge force, sway force, and yaw moment
        """
        return {
            'surge_force': 1.016e-11,      # lbf (effectively zero)
            'sway_force': 7.644e4,         # lbf = 76,440 lbf
            'yaw_moment': 7.851e-9,        # ft-lbf (effectively zero)
        }

    @pytest.fixture
    def calculator(self, mathcad_reference_params):
        """Create calculator instance with MathCAD reference parameters."""
        p = mathcad_reference_params
        return PassingShipCalculator(
            moored_vessel_length=p['L'],
            moored_vessel_area=p['A1'],
            passing_vessel_length=p['L2'],
            passing_vessel_area=p['A2'],
            water_density=p['rho'],
            passing_vessel_velocity=p['U'],
            separation_distance=p['SEP_DIST'],
            stagger_distance=p['STA_DIST'],
            water_depth=p['D']
        )

    def test_sway_force_validation(self, calculator, mathcad_expected_results):
        """
        Test sway force against MathCAD reference value.

        The sway force is the most significant force component and should match
        the MathCAD result of 76,440 lbf within reasonable tolerance.
        """
        result = calculator.calculate_forces()

        expected_sway = mathcad_expected_results['sway_force']
        calculated_sway = result['sway_force']

        # Allow 1% relative tolerance for numerical integration differences
        relative_error = abs((calculated_sway - expected_sway) / expected_sway)

        assert relative_error < 0.01, (
            f"Sway force validation failed:\n"
            f"  Expected (MathCAD): {expected_sway:,.2f} lbf\n"
            f"  Calculated (Python): {calculated_sway:,.2f} lbf\n"
            f"  Relative error: {relative_error*100:.2f}%\n"
            f"  Tolerance: 1.0%"
        )

    def test_surge_force_near_zero(self, calculator, mathcad_expected_results):
        """
        Test that surge force is near zero for aligned vessels.

        When stagger distance is zero (vessels aligned), the surge force
        should be effectively zero due to symmetry.
        """
        result = calculator.calculate_forces()

        expected_surge = mathcad_expected_results['surge_force']
        calculated_surge = result['surge_force']

        # For near-zero values, use absolute tolerance instead of relative
        # MathCAD shows 1.016e-11, which is effectively zero
        absolute_tolerance = 1e-6  # 0.000001 lbf

        assert abs(calculated_surge) < absolute_tolerance, (
            f"Surge force validation failed:\n"
            f"  Expected (MathCAD): ~0 lbf ({expected_surge:.2e})\n"
            f"  Calculated (Python): {calculated_surge:.2e} lbf\n"
            f"  Should be < {absolute_tolerance:.2e} lbf (effectively zero)"
        )

    def test_yaw_moment_near_zero(self, calculator, mathcad_expected_results):
        """
        Test that yaw moment is near zero for aligned vessels.

        When stagger distance is zero (vessels aligned), the yaw moment
        should be effectively zero due to symmetry.
        """
        result = calculator.calculate_forces()

        expected_yaw = mathcad_expected_results['yaw_moment']
        calculated_yaw = result['yaw_moment']

        # For near-zero values, use absolute tolerance
        # MathCAD shows 7.851e-9, which is effectively zero
        absolute_tolerance = 1e-3  # 0.001 ft-lbf

        assert abs(calculated_yaw) < absolute_tolerance, (
            f"Yaw moment validation failed:\n"
            f"  Expected (MathCAD): ~0 ft-lbf ({expected_yaw:.2e})\n"
            f"  Calculated (Python): {calculated_yaw:.2e} ft-lbf\n"
            f"  Should be < {absolute_tolerance:.2e} ft-lbf (effectively zero)"
        )

    def test_complete_mathcad_scenario(self, calculator, mathcad_expected_results):
        """
        Comprehensive validation test for all forces and moments.

        This test validates the complete calculation against all MathCAD
        reference values simultaneously.
        """
        result = calculator.calculate_forces()

        # Validate sway force (main force component)
        sway_error = abs((result['sway_force'] - mathcad_expected_results['sway_force'])
                        / mathcad_expected_results['sway_force'])

        # Validate surge and yaw are near zero
        surge_near_zero = abs(result['surge_force']) < 1e-6
        yaw_near_zero = abs(result['yaw_moment']) < 1e-3

        validation_summary = f"""
        MathCAD Validation Summary:
        ===========================

        Surge Force:
          MathCAD:  {mathcad_expected_results['surge_force']:.2e} lbf
          Python:   {result['surge_force']:.2e} lbf
          Status:   {'✓ PASS' if surge_near_zero else '✗ FAIL'} (near zero)

        Sway Force:
          MathCAD:  {mathcad_expected_results['sway_force']:,.2f} lbf
          Python:   {result['sway_force']:,.2f} lbf
          Error:    {sway_error*100:.3f}%
          Status:   {'✓ PASS' if sway_error < 0.01 else '✗ FAIL'} (< 1% tolerance)

        Yaw Moment:
          MathCAD:  {mathcad_expected_results['yaw_moment']:.2e} ft-lbf
          Python:   {result['yaw_moment']:.2e} ft-lbf
          Status:   {'✓ PASS' if yaw_near_zero else '✗ FAIL'} (near zero)
        """

        all_passed = surge_near_zero and (sway_error < 0.01) and yaw_near_zero

        assert all_passed, validation_summary

    def test_dimensional_consistency(self, calculator):
        """
        Test that results have correct dimensions and reasonable magnitudes.

        Validates that:
        - Forces are in lbf (positive values)
        - Moments are in ft-lbf
        - All values are finite and non-NaN
        """
        result = calculator.calculate_forces()

        # Check all values are finite
        assert np.isfinite(result['surge_force']), "Surge force is not finite"
        assert np.isfinite(result['sway_force']), "Sway force is not finite"
        assert np.isfinite(result['yaw_moment']), "Yaw moment is not finite"

        # Sway force should be positive (lateral force)
        assert result['sway_force'] > 0, "Sway force should be positive"

        # Check reasonable magnitude (sway force should be in thousands of lbf range)
        assert 1e3 < result['sway_force'] < 1e6, (
            f"Sway force magnitude unreasonable: {result['sway_force']:,.2f} lbf"
        )


class TestMathCADParameterSensitivity:
    """
    Test parameter sensitivity to validate calculation behavior.

    These tests verify that the Python implementation responds correctly
    to parameter changes, matching expected physical behavior.
    """

    @pytest.fixture
    def base_params(self):
        """Base parameters for sensitivity analysis."""
        return {
            'L': 950.0,
            'A1': 3192.0,
            'L2': 475.0,
            'A2': 6413.0,
            'rho': 1.9905,
            'U': 11.2,
            'SEP_DIST': 190.0,
            'STA_DIST': 0.0,
            'D': 95.0,
        }

    def test_velocity_squared_relationship(self, base_params):
        """
        Test that forces scale with velocity squared.

        According to Wang's formulation, forces should be proportional to U².
        """
        # Base case
        calc_base = PassingShipCalculator(**base_params)
        result_base = calc_base.calculate_forces()

        # Double velocity case
        params_2x = base_params.copy()
        params_2x['U'] = base_params['U'] * 2
        calc_2x = PassingShipCalculator(**params_2x)
        result_2x = calc_2x.calculate_forces()

        # Forces should quadruple (2² = 4)
        expected_ratio = 4.0
        actual_ratio = result_2x['sway_force'] / result_base['sway_force']

        relative_error = abs((actual_ratio - expected_ratio) / expected_ratio)

        assert relative_error < 0.01, (
            f"Velocity scaling test failed:\n"
            f"  Expected force ratio: {expected_ratio:.2f}x\n"
            f"  Actual force ratio: {actual_ratio:.2f}x\n"
            f"  Error: {relative_error*100:.2f}%"
        )

    def test_separation_distance_effect(self, base_params):
        """
        Test that forces decrease with increased separation distance.

        Forces should decrease as ships are further apart.
        """
        # Close separation
        calc_close = PassingShipCalculator(**base_params)
        result_close = calc_close.calculate_forces()

        # Larger separation
        params_far = base_params.copy()
        params_far['SEP_DIST'] = base_params['SEP_DIST'] * 2  # Double the distance
        calc_far = PassingShipCalculator(**params_far)
        result_far = calc_far.calculate_forces()

        # Force should decrease with distance
        assert result_far['sway_force'] < result_close['sway_force'], (
            f"Separation distance effect failed:\n"
            f"  Closer separation: {result_close['sway_force']:,.2f} lbf\n"
            f"  Farther separation: {result_far['sway_force']:,.2f} lbf\n"
            f"  Force should decrease with distance"
        )

    def test_water_depth_effect(self, base_params):
        """
        Test shallow water effect on forces.

        Shallow water should amplify forces compared to deep water.
        """
        # Shallow water (base case)
        calc_shallow = PassingShipCalculator(**base_params)
        result_shallow = calc_shallow.calculate_forces()

        # Deeper water
        params_deep = base_params.copy()
        params_deep['D'] = base_params['D'] * 5  # Much deeper
        calc_deep = PassingShipCalculator(**params_deep)
        result_deep = calc_deep.calculate_forces()

        # Shallow water should produce larger forces
        assert result_shallow['sway_force'] > result_deep['sway_force'], (
            f"Water depth effect failed:\n"
            f"  Shallow water ({base_params['D']} ft): {result_shallow['sway_force']:,.2f} lbf\n"
            f"  Deep water ({params_deep['D']} ft): {result_deep['sway_force']:,.2f} lbf\n"
            f"  Shallow water should produce larger forces"
        )


if __name__ == '__main__':
    # Run tests with verbose output
    pytest.main([__file__, '-v', '--tb=short'])
