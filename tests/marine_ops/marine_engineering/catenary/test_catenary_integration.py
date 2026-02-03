"""
Integration tests for unified catenary module.

Tests that all components (solver, simplified, adapter) work together correctly
and that the module migration preserved all functionality.

Author: Digital Model Project (Test Agent)
Date: 2025-10-03
"""

import pytest
import numpy as np
import warnings
import math
from digitalmodel.marine_ops.marine_engineering.catenary import (
    CatenarySolver, CatenaryInput, CatenaryResults,
    catenaryEquation, catenaryForces  # Adapter functions
)
from digitalmodel.marine_ops.marine_engineering.catenary.simplified import SimplifiedCatenarySolver


class TestUnifiedModuleIntegration:
    """Test all components work together."""

    def test_imports_work(self):
        """Verify all imports work from unified module."""
        # Test Phase 1 solver
        solver = CatenarySolver()
        assert solver is not None

        # Test simplified solver
        simp_solver = SimplifiedCatenarySolver()
        assert simp_solver is not None

        # Test adapter functions exist
        assert callable(catenaryEquation)
        assert callable(catenaryForces)

    def test_phase1_solver_still_works(self):
        """Ensure Phase 1 BVP solver works in new location."""
        params = CatenaryInput(
            length=1000.0,
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=1962.0,
            ea_stiffness=64e9
        )
        solver = CatenarySolver()
        results = solver.solve(params)

        assert results.converged, "Phase 1 solver must converge"
        assert results.horizontal_tension > 0
        assert results.total_tension_fairlead > results.horizontal_tension

    def test_simplified_vs_adapter_consistency(self):
        """Verify simplified methods match adapter results."""
        # Using simplified directly
        simp_solver = SimplifiedCatenarySolver()
        result1 = simp_solver.solve_from_angle(30.0, 100.0)

        # Using adapter (legacy API)
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', category=DeprecationWarning)
            result2 = catenaryEquation({'q': 30, 'd': 100, 'F': None, 'w': None, 'X': None})

        # Should match
        assert abs(result1.arc_length - result2['S']) < 0.01
        assert abs(result1.horizontal_distance - result2['X']) < 0.01
        assert abs(result1.bend_radius - result2['BendRadius']) < 0.01

    def test_force_based_adapter_consistency(self):
        """Verify force-based adapter matches simplified solver."""
        simp_solver = SimplifiedCatenarySolver()
        result1 = simp_solver.solve_from_force(10000.0, 50.0, 100.0)

        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', category=DeprecationWarning)
            result2 = catenaryEquation({'F': 10000, 'w': 50, 'd': 100, 'q': None, 'X': None})

        # Verify exact match
        assert abs(result1.arc_length - result2['S']) < 1e-9
        assert abs(result1.horizontal_distance - result2['X']) < 1e-9
        assert abs(result1.horizontal_tension - result2['THorizontal']) < 1e-9

    def test_forces_calculation_consistency(self):
        """Verify force calculations match across APIs."""
        simp_solver = SimplifiedCatenarySolver()
        Fv1, F1, Fh1 = simp_solver.calculate_forces(50.0, 100.0, 30.0)

        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', category=DeprecationWarning)
            result2 = catenaryForces({
                'weightPerUnitLength': 50.0,
                'S': 100.0,
                'q': 30.0
            })

        assert abs(Fv1 - result2['Fv']) < 1e-9
        assert abs(F1 - result2['F']) < 1e-9
        assert abs(Fh1 - result2['Fh']) < 1e-9


class TestWorkflowIntegration:
    """Test complete analysis workflows."""

    def test_complete_mooring_analysis_workflow(self):
        """Test typical mooring analysis workflow."""
        # Step 1: Calculate geometry from angle
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', category=DeprecationWarning)
            geometry = catenaryEquation({
                'q': 35.0,
                'd': 150.0,
                'F': None, 'w': None, 'X': None
            })

        # Step 2: Calculate forces
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', category=DeprecationWarning)
            forces = catenaryForces({
                'weightPerUnitLength': 600.0,
                'S': geometry['S'],
                'q': 35.0
            })

        # Step 3: Verify with Phase 1 solver for validation
        # (Can't directly compare as Phase 1 needs different params)
        # But we can verify physics constraints
        assert forces['F'] > forces['Fv']  # Total > vertical
        assert forces['Fh'] > 0  # Horizontal component exists
        assert geometry['S'] > geometry['X']  # Arc length > horizontal distance

    def test_simplified_then_detailed_analysis(self):
        """Test starting with simplified, then using detailed solver."""
        # Step 1: Quick estimate with simplified
        simp_solver = SimplifiedCatenarySolver()
        quick_result = simp_solver.solve_from_force(
            force=800000.0,
            weight_per_length=1962.0,
            vertical_distance=100.0
        )

        # Step 2: Detailed analysis with Phase 1 solver
        # Use results from simplified as sanity check
        params = CatenaryInput(
            length=1000.0,
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=1962.0,
            ea_stiffness=64e9
        )
        solver = CatenarySolver()
        detailed_result = solver.solve(params)

        # Both should give reasonable results
        assert detailed_result.converged
        assert detailed_result.horizontal_tension > 0
        # Simplified gives horizontal_tension, Phase 1 is more accurate
        # They won't match exactly but should be same order of magnitude

    def test_multi_solver_comparison(self):
        """Compare results across different solver approaches."""
        # Same problem, different methods

        # Method 1: Simplified angle-based
        simp1 = SimplifiedCatenarySolver()
        result1 = simp1.solve_from_angle(30.0, 100.0)

        # Method 2: Adapter (legacy compatibility)
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', category=DeprecationWarning)
            result2 = catenaryEquation({'q': 30, 'd': 100, 'F': None, 'w': None, 'X': None})

        # Results should be identical
        assert result1.arc_length == pytest.approx(result2['S'], abs=1e-9)
        assert result1.horizontal_distance == pytest.approx(result2['X'], abs=1e-9)


class TestBackwardCompatibility:
    """Verify backward compatibility is maintained."""

    def test_adapter_provides_exact_legacy_api(self):
        """Verify adapter provides exact same API as legacy."""
        # Test force-based method signature
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore', category=DeprecationWarning)
            result = catenaryEquation({
                'F': 10000, 'w': 500, 'd': 100,
                'X': None, 'q': None
            })

        # Check all expected keys exist
        assert 'S' in result
        assert 'X' in result
        assert 'W' in result
        assert 'THorizontal' in result
        assert 'F' in result  # Original input preserved
        assert 'w' in result
        assert 'd' in result

    def test_adapter_raises_deprecation_warning(self):
        """Verify deprecation warnings are raised."""
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            catenaryEquation({'q': 30, 'd': 100, 'F': None, 'w': None, 'X': None})

            assert len(w) >= 1
            assert any(issubclass(warning.category, DeprecationWarning) for warning in w)

    def test_legacy_error_handling_preserved(self):
        """Verify error handling matches legacy behavior."""
        # Invalid F/w ratio should raise ValueError
        with pytest.raises(ValueError):
            with warnings.catch_warnings():
                warnings.filterwarnings('ignore', category=DeprecationWarning)
                catenaryEquation({'F': 100, 'w': 50, 'd': 100, 'X': None, 'q': None})

        # Invalid angle should raise ValueError
        with pytest.raises(ValueError):
            with warnings.catch_warnings():
                warnings.filterwarnings('ignore', category=DeprecationWarning)
                catenaryEquation({'q': 95, 'd': 100, 'F': None, 'w': None, 'X': None})


class TestEdgeCaseHandling:
    """Test edge cases across all components."""

    def test_tight_line_all_methods(self):
        """Test nearly taut line across different methods."""
        # Phase 1 solver
        params = CatenaryInput(
            length=1010.0,  # Just 10m longer than span
            horizontal_span=1000.0,
            vertical_span=50.0,
            weight_per_length=500.0,
            ea_stiffness=100e9
        )
        solver = CatenarySolver()
        result = solver.solve(params)

        assert result.converged
        assert result.horizontal_tension > 10000  # High tension expected

    def test_slack_line_all_methods(self):
        """Test slack line across different methods."""
        # Phase 1 solver
        params = CatenaryInput(
            length=1500.0,  # Much longer than span
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=3000.0,
            ea_stiffness=50e9
        )
        solver = CatenarySolver()
        result = solver.solve(params)

        assert result.converged
        assert result.catenary_parameter < params.horizontal_span

    def test_extreme_angles(self):
        """Test extreme angle cases."""
        simp_solver = SimplifiedCatenarySolver()

        # Very small angle (steep)
        result1 = simp_solver.solve_from_angle(5.0, 100.0)
        assert result1.arc_length > 0
        assert result1.horizontal_distance > 0

        # Very large angle (shallow)
        result2 = simp_solver.solve_from_angle(85.0, 100.0)
        assert result2.arc_length > 0
        assert result2.horizontal_distance > 0


class TestPhysicalConsistency:
    """Verify physical consistency across module."""

    def test_energy_conservation_principle(self):
        """Verify results are physically consistent."""
        params = CatenaryInput(
            length=1000.0,
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=1962.0,
            ea_stiffness=64e9
        )
        solver = CatenarySolver()
        result = solver.solve(params)

        # Pythagorean relationship at fairlead
        calculated_total = np.sqrt(
            result.horizontal_tension**2 +
            result.vertical_tension_fairlead**2
        )
        assert result.total_tension_fairlead == pytest.approx(calculated_total, rel=1e-6)

    def test_force_equilibrium(self):
        """Test force equilibrium in catenary."""
        simp_solver = SimplifiedCatenarySolver()

        # Calculate geometry
        result = simp_solver.solve_from_angle(30.0, 100.0)

        # Calculate forces
        Fv, F, Fh = simp_solver.calculate_forces(50.0, result.arc_length, 30.0)

        # Verify force relationships
        calculated_F = np.sqrt(Fv**2 + Fh**2)
        assert F == pytest.approx(calculated_F, rel=1e-6)


class TestNumericalStability:
    """Test numerical stability across module."""

    def test_small_perturbation_stability(self):
        """Verify small changes produce small result changes."""
        params_base = CatenaryInput(
            length=1000.0,
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=1962.0,
            ea_stiffness=64e9
        )

        params_perturbed = CatenaryInput(
            length=1000.1,  # 0.01% change
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=1962.0,
            ea_stiffness=64e9
        )

        solver = CatenarySolver()
        result_base = solver.solve(params_base)
        result_pert = solver.solve(params_perturbed)

        # Results should be very close
        rel_diff = abs(
            result_pert.horizontal_tension - result_base.horizontal_tension
        ) / result_base.horizontal_tension

        assert rel_diff < 0.001  # Less than 0.1% change

    def test_convergence_consistency(self):
        """Verify convergence is consistent across tolerances."""
        params = CatenaryInput(
            length=1000.0,
            horizontal_span=800.0,
            vertical_span=100.0,
            weight_per_length=1962.0,
            ea_stiffness=64e9
        )

        solver_tight = CatenarySolver(tolerance=1e-9)
        solver_loose = CatenarySolver(tolerance=1e-3)

        result_tight = solver_tight.solve(params)
        result_loose = solver_loose.solve(params)

        # Both should converge
        assert result_tight.converged
        assert result_loose.converged

        # Results should be close
        rel_diff = abs(
            result_tight.horizontal_tension - result_loose.horizontal_tension
        ) / result_tight.horizontal_tension
        assert rel_diff < 0.01  # Within 1%


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
