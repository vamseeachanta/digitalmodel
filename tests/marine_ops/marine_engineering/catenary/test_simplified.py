"""
Comprehensive tests for simplified catenary module.

Tests verify:
1. Exact numerical match to closed-form catenary equations (±0.0001 tolerance)
2. Edge case handling
3. Input validation
4. API consistency
"""

import math
import pytest
from digitalmodel.marine_ops.marine_analysis.catenary.simplified import (
    SimplifiedCatenarySolver,
    SimplifiedCatenaryInput,
    SimplifiedCatenaryResults,
    solve_catenary_dict,
    calculate_forces_dict
)


def expected_from_angle(angle_deg, vertical_distance):
    """Closed-form catenary geometry from fairlead angle and vertical span.

    Complementary-angle convention (q measured from vertical), consistent with
    the canonical `catenary` workflow (S=173.2050808 for q=30, d=100) and the
    legacy catenaryEquation adapter.
    """
    complementary_rad = math.radians(90.0 - angle_deg)
    tanqc = math.tan(complementary_rad)
    cos_comp = math.cos(complementary_rad)
    catenary_parameter = vertical_distance * cos_comp / (1.0 - cos_comp)
    return {
        "arc_length": catenary_parameter * tanqc,
        "horizontal_distance": catenary_parameter * math.asinh(tanqc),
        "bend_radius": catenary_parameter,
    }


def expected_from_force(force, weight_per_length, vertical_distance):
    """Closed-form catenary geometry from total fairlead force."""
    horizontal_tension = force - weight_per_length * vertical_distance
    catenary_parameter = horizontal_tension / weight_per_length
    x_over_a = math.acosh(1.0 + vertical_distance / catenary_parameter)
    arc_length = catenary_parameter * math.sinh(x_over_a)
    return {
        "arc_length": arc_length,
        "horizontal_distance": catenary_parameter * x_over_a,
        "weight_suspended": weight_per_length * arc_length,
        "horizontal_tension": horizontal_tension,
        "shape_parameter": weight_per_length / horizontal_tension,
    }


class TestSimplifiedCatenarySolverAngleBased:
    """Tests for angle-based catenary calculations."""

    def test_angle_based_basic(self):
        """Test basic angle-based calculation matches closed form."""
        solver = SimplifiedCatenarySolver()

        # Test case: 30 degrees, 100m vertical distance
        result = solver.solve_from_angle(angle_deg=30.0, vertical_distance=100.0)
        expected = expected_from_angle(30.0, 100.0)

        assert result.arc_length == pytest.approx(expected["arc_length"], abs=0.0001)
        assert result.horizontal_distance == pytest.approx(expected["horizontal_distance"], abs=0.0001)
        assert result.bend_radius == pytest.approx(expected["bend_radius"], abs=0.0001)
        assert result.horizontal_tension is None
        assert result.shape_parameter is None

    def test_angle_based_45_degrees(self):
        """Test 45-degree angle case."""
        solver = SimplifiedCatenarySolver()

        result = solver.solve_from_angle(angle_deg=45.0, vertical_distance=100.0)
        expected = expected_from_angle(45.0, 100.0)

        assert result.arc_length == pytest.approx(expected["arc_length"], abs=0.0001)
        assert result.horizontal_distance == pytest.approx(expected["horizontal_distance"], abs=0.001)
        assert result.bend_radius == pytest.approx(expected["bend_radius"], abs=0.0001)

    def test_angle_based_small_angle(self):
        """Test small angle case (shallow catenary)."""
        solver = SimplifiedCatenarySolver()

        result = solver.solve_from_angle(angle_deg=10.0, vertical_distance=100.0)
        expected = expected_from_angle(10.0, 100.0)

        assert result.arc_length == pytest.approx(expected["arc_length"], abs=0.001)
        assert result.horizontal_distance == pytest.approx(expected["horizontal_distance"], abs=0.001)
        assert result.bend_radius == pytest.approx(expected["bend_radius"], abs=0.001)

    def test_angle_based_large_angle(self):
        """Test large angle case (steep catenary)."""
        solver = SimplifiedCatenarySolver()

        result = solver.solve_from_angle(angle_deg=80.0, vertical_distance=100.0)
        expected = expected_from_angle(80.0, 100.0)

        assert result.arc_length == pytest.approx(expected["arc_length"], abs=0.001)
        assert result.horizontal_distance == pytest.approx(expected["horizontal_distance"], abs=0.001)
        assert result.bend_radius == pytest.approx(expected["bend_radius"], abs=0.001)

    def test_angle_based_invalid_angle_zero(self):
        """Test that angle=0 raises ValueError."""
        solver = SimplifiedCatenarySolver()

        with pytest.raises(ValueError, match="must be between 0 and 90"):
            solver.solve_from_angle(angle_deg=0.0, vertical_distance=100.0)

    def test_angle_based_invalid_angle_90(self):
        """Test that angle=90 raises ValueError."""
        solver = SimplifiedCatenarySolver()

        with pytest.raises(ValueError, match="must be between 0 and 90"):
            solver.solve_from_angle(angle_deg=90.0, vertical_distance=100.0)

    def test_angle_based_invalid_angle_negative(self):
        """Test that negative angle raises ValueError."""
        solver = SimplifiedCatenarySolver()

        with pytest.raises(ValueError, match="must be between 0 and 90"):
            solver.solve_from_angle(angle_deg=-10.0, vertical_distance=100.0)

    def test_angle_based_invalid_distance_negative(self):
        """Test that negative distance raises ValueError."""
        solver = SimplifiedCatenarySolver()

        with pytest.raises(ValueError, match="must be positive"):
            solver.solve_from_angle(angle_deg=30.0, vertical_distance=-100.0)

    def test_angle_based_invalid_distance_zero(self):
        """Test that zero distance raises ValueError."""
        solver = SimplifiedCatenarySolver()

        with pytest.raises(ValueError, match="must be positive"):
            solver.solve_from_angle(angle_deg=30.0, vertical_distance=0.0)


class TestSimplifiedCatenarySolverForceBased:
    """Tests for force-based catenary calculations."""

    def test_force_based_basic(self):
        """Test basic force-based calculation matches closed form."""
        solver = SimplifiedCatenarySolver()

        result = solver.solve_from_force(
            force=10000.0,
            weight_per_length=50.0,
            vertical_distance=100.0
        )
        expected = expected_from_force(10000.0, 50.0, 100.0)

        assert result.arc_length == pytest.approx(expected["arc_length"], abs=0.0001)
        assert result.horizontal_distance == pytest.approx(expected["horizontal_distance"], abs=0.01)
        assert result.weight_suspended == pytest.approx(expected["weight_suspended"], abs=0.0001)
        assert result.horizontal_tension == pytest.approx(expected["horizontal_tension"], abs=0.01)
        assert result.shape_parameter == pytest.approx(expected["shape_parameter"], abs=0.001)

    def test_force_based_realistic_marine_cable(self):
        """Test with realistic marine cable parameters."""
        solver = SimplifiedCatenarySolver()

        # Realistic: 50-ton cable, 100kg/m weight, 500m depth
        # Need F > w*d = 981*500 = 490,500 N
        result = solver.solve_from_force(
            force=500000.0,  # 50 tons = 500 kN
            weight_per_length=981.0,  # 100 kg/m * 9.81 m/s^2
            vertical_distance=500.0
        )
        expected = expected_from_force(500000.0, 981.0, 500.0)

        assert result.arc_length == pytest.approx(expected["arc_length"], abs=1.0)
        assert result.horizontal_distance == pytest.approx(expected["horizontal_distance"], abs=0.1)
        assert result.weight_suspended == pytest.approx(expected["weight_suspended"], abs=1000.0)

    def test_force_based_invalid_force_too_small(self):
        """Test that force too small raises ValueError."""
        solver = SimplifiedCatenarySolver()

        # F <= w*d should fail
        # w*d = 50*100 = 5000
        with pytest.raises(ValueError, match="force too small"):
            solver.solve_from_force(
                force=2000.0,  # Less than 2500
                weight_per_length=50.0,
                vertical_distance=100.0
            )

    def test_force_based_invalid_negative_force(self):
        """Test that negative force raises ValueError."""
        solver = SimplifiedCatenarySolver()

        with pytest.raises(ValueError, match="must be positive"):
            solver.solve_from_force(
                force=-1000.0,
                weight_per_length=50.0,
                vertical_distance=100.0
            )

    def test_force_based_invalid_zero_weight(self):
        """Test that zero weight raises ValueError."""
        solver = SimplifiedCatenarySolver()

        with pytest.raises(ValueError, match="must be positive"):
            solver.solve_from_force(
                force=10000.0,
                weight_per_length=0.0,
                vertical_distance=100.0
            )

    def test_force_based_invalid_negative_distance(self):
        """Test that negative distance raises ValueError."""
        solver = SimplifiedCatenarySolver()

        with pytest.raises(ValueError, match="must be positive"):
            solver.solve_from_force(
                force=10000.0,
                weight_per_length=50.0,
                vertical_distance=-100.0
            )


class TestCalculateForces:
    """Tests for force calculation from geometry."""

    def test_calculate_forces_basic(self):
        """Test basic force calculation."""
        solver = SimplifiedCatenarySolver()

        # Test: w=50 N/m, S=100m, q=30°
        Fv, F, Fh = solver.calculate_forces(
            weight_per_length=50.0,
            arc_length=100.0,
            angle_deg=30.0
        )

        # Fv = 50 * 100 = 5000
        # sin(30°) = 0.5
        # cos(30°) = 0.866025404
        # F = 5000 / 0.5 = 10000
        # Fh = 10000 * 0.866025404 = 8660.254038

        assert Fv == pytest.approx(5000.0, abs=0.0001)
        assert F == pytest.approx(10000.0, abs=0.0001)
        assert Fh == pytest.approx(8660.254038, abs=0.0001)

    def test_calculate_forces_45_degrees(self):
        """Test force calculation at 45 degrees."""
        solver = SimplifiedCatenarySolver()

        Fv, F, Fh = solver.calculate_forces(
            weight_per_length=100.0,
            arc_length=200.0,
            angle_deg=45.0
        )

        # Fv = 100 * 200 = 20000
        # sin(45°) = cos(45°) = 0.707106781
        # F = 20000 / 0.707106781 = 28284.27125
        # Fh = 28284.27125 * 0.707106781 = 20000

        assert Fv == pytest.approx(20000.0, abs=0.0001)
        assert F == pytest.approx(28284.27125, abs=0.0001)
        assert Fh == pytest.approx(20000.0, abs=0.0001)

    def test_calculate_forces_invalid_angle_zero(self):
        """Test that angle=0 raises ValueError."""
        solver = SimplifiedCatenarySolver()

        with pytest.raises(ValueError, match="must be between 0 and 90"):
            solver.calculate_forces(50.0, 100.0, 0.0)

    def test_calculate_forces_invalid_weight_negative(self):
        """Test that negative weight raises ValueError."""
        solver = SimplifiedCatenarySolver()

        with pytest.raises(ValueError, match="must be positive"):
            solver.calculate_forces(-50.0, 100.0, 30.0)


class TestDictionaryApi:
    """Tests for dictionary-based API."""

    def test_solve_catenary_dict_force_based(self):
        """Test dictionary API for force-based calculation."""
        data = {
            'F': 10000.0,
            'w': 50.0,
            'd': 100.0,
            'q': None,
            'X': None
        }

        result = solve_catenary_dict(data)

        assert 'S' in result
        assert 'X' in result
        assert 'W' in result
        assert 'THorizontal' in result
        assert 'b' in result
        expected = expected_from_force(10000.0, 50.0, 100.0)
        assert result['S'] == pytest.approx(expected["arc_length"], abs=0.0001)

    def test_solve_catenary_dict_angle_based(self):
        """Test dictionary API for angle-based calculation."""
        data = {
            'F': None,
            'w': None,
            'd': 100.0,
            'q': 30.0,
            'X': None
        }

        result = solve_catenary_dict(data)

        assert 'S' in result
        assert 'X' in result
        assert 'BendRadius' in result
        expected = expected_from_angle(30.0, 100.0)
        assert result['S'] == pytest.approx(expected["arc_length"], abs=0.0001)
        assert result['X'] == pytest.approx(expected["horizontal_distance"], abs=0.0001)
        assert result['BendRadius'] == pytest.approx(expected["bend_radius"], abs=0.0001)

    def test_solve_catenary_dict_x_based_not_implemented(self):
        """Test that X-based calculation raises NotImplementedError."""
        data = {
            'F': None,
            'w': None,
            'd': 100.0,
            'q': None,
            'X': 150.0
        }

        with pytest.raises(NotImplementedError):
            solve_catenary_dict(data)

    def test_calculate_forces_dict(self):
        """Test dictionary API for force calculation."""
        data = {
            'weightPerUnitLength': 50.0,
            'S': 100.0,
            'q': 30.0
        }

        result = calculate_forces_dict(data)

        assert 'Fv' in result
        assert 'F' in result
        assert 'Fh' in result
        assert result['Fv'] == pytest.approx(5000.0, abs=0.0001)
        assert result['F'] == pytest.approx(10000.0, abs=0.0001)
        assert result['Fh'] == pytest.approx(8660.254038, abs=0.0001)


class TestComparisonWithClosedForm:
    """Direct comparison tests with closed-form catenary equations."""

    def test_angle_based_matches_closed_form_exactly(self):
        """Verify implementation matches closed form for multiple angle cases."""
        solver = SimplifiedCatenarySolver()

        test_cases = [
            (10.0, 100.0),
            (20.0, 100.0),
            (30.0, 100.0),
            (45.0, 100.0),
            (60.0, 100.0),
            (70.0, 100.0),
            (80.0, 100.0),
        ]

        for angle, distance in test_cases:
            # Calculate using new implementation
            result = solver.solve_from_angle(angle, distance)

            expected = expected_from_angle(angle, distance)

            # Verify exact match
            assert result.arc_length == pytest.approx(expected["arc_length"], abs=1e-9)
            assert result.horizontal_distance == pytest.approx(expected["horizontal_distance"], abs=1e-9)
            assert result.bend_radius == pytest.approx(expected["bend_radius"], abs=1e-9)

    def test_force_based_matches_closed_form_exactly(self):
        """Verify implementation matches closed form for multiple force cases."""
        solver = SimplifiedCatenarySolver()

        test_cases = [
            (10000.0, 50.0, 100.0),   # F=10000 > w*d=5000
            (25000.0, 100.0, 200.0),  # F=25000 > w*d=20000
            (50000.0, 200.0, 150.0),  # F=50000 > w*d=30000
        ]

        for force, weight, distance in test_cases:
            # Calculate using new implementation
            result = solver.solve_from_force(force, weight, distance)

            expected = expected_from_force(force, weight, distance)

            # Verify exact match
            assert result.arc_length == pytest.approx(expected["arc_length"], abs=1e-9)
            assert result.horizontal_distance == pytest.approx(expected["horizontal_distance"], abs=1e-9)
            assert result.weight_suspended == pytest.approx(expected["weight_suspended"], abs=1e-9)
            assert result.horizontal_tension == pytest.approx(expected["horizontal_tension"], abs=1e-9)
            assert result.shape_parameter == pytest.approx(expected["shape_parameter"], abs=1e-9)


if __name__ == '__main__':
    pytest.main([__file__, '-v', '--tb=short'])
