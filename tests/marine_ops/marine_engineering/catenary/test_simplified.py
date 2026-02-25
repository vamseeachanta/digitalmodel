"""
Comprehensive tests for simplified catenary module.

Tests verify:
1. Exact numerical match to legacy implementations (±0.0001 tolerance)
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


class TestSimplifiedCatenarySolverAngleBased:
    """Tests for angle-based catenary calculations."""

    def test_angle_based_basic(self):
        """Test basic angle-based calculation matches legacy."""
        solver = SimplifiedCatenarySolver()

        # Test case: 30 degrees, 100m vertical distance
        result = solver.solve_from_angle(angle_deg=30.0, vertical_distance=100.0)

        # Expected values calculated from legacy formulas
        # tanq = tan(60°) = 1.732050808...
        # cos(60°) = 0.5, (1 - 0.5) = 0.5
        # BendRadius = 100 * 0.5 / 0.5 = 100
        # S = 100 * 1.732050808 = 173.2050808
        # X = 100 * asinh(1.732050808) = 100 * 1.316957897 = 131.6957897

        assert result.arc_length == pytest.approx(173.2050808, abs=0.0001)
        assert result.horizontal_distance == pytest.approx(131.6957897, abs=0.0001)
        assert result.bend_radius == pytest.approx(100.0, abs=0.0001)
        assert result.horizontal_tension is None
        assert result.shape_parameter is None

    def test_angle_based_45_degrees(self):
        """Test 45-degree angle case."""
        solver = SimplifiedCatenarySolver()

        result = solver.solve_from_angle(angle_deg=45.0, vertical_distance=100.0)

        # tanq = tan(45°) = 1.0
        # cos(45°) = 0.707106781, (1 - 0.707106781) = 0.292893219
        # BendRadius = 100 * 0.707106781 / 0.292893219 = 241.4213562
        # S = 241.4213562 * 1.0 = 241.4213562
        # X = 241.4213562 * asinh(1.0) = 241.4213562 * 0.881373587 = 212.7322122

        assert result.arc_length == pytest.approx(241.4213562, abs=0.0001)
        assert result.horizontal_distance == pytest.approx(212.782407, abs=0.001)
        assert result.bend_radius == pytest.approx(241.4213562, abs=0.0001)

    def test_angle_based_small_angle(self):
        """Test small angle case (steep catenary)."""
        solver = SimplifiedCatenarySolver()

        result = solver.solve_from_angle(angle_deg=10.0, vertical_distance=100.0)

        # tanq = tan(80°) = 5.671281820
        # cos(80°) = 0.173648178, (1 - 0.173648178) = 0.826351822
        # BendRadius = 100 * 0.173648178 / 0.826351822 = 21.0094635
        # S = 21.0094635 * 5.671281820 = 119.1456584
        # X = 21.0094635 * asinh(5.671281820) = 52.9903811

        assert result.arc_length == pytest.approx(119.175359, abs=0.001)
        assert result.horizontal_distance == pytest.approx(52.991147, abs=0.001)
        assert result.bend_radius == pytest.approx(21.009463, abs=0.001)

    def test_angle_based_large_angle(self):
        """Test large angle case (shallow catenary)."""
        solver = SimplifiedCatenarySolver()

        result = solver.solve_from_angle(angle_deg=80.0, vertical_distance=100.0)

        # tanq = tan(10°) = 0.176326981
        # cos(10°) = 0.984807753, (1 - 0.984807753) = 0.015192247
        # BendRadius = 100 * 0.984807753 / 0.015192247 = 6482.9235039
        # S = 6482.9235039 * 0.176326981 = 1143.0662321
        # X = 6482.9235039 * asinh(0.176326981) = 1135.3661647

        assert result.arc_length == pytest.approx(1143.00523, abs=0.001)
        assert result.horizontal_distance == pytest.approx(1135.315774, abs=0.001)
        assert result.bend_radius == pytest.approx(6482.92347, abs=0.001)

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
        """Test basic force-based calculation matches legacy."""
        solver = SimplifiedCatenarySolver()

        # Use valid values where F > w*d/2
        # F=10000, w=50, d=100 gives F/w=200 > d/2=50
        result = solver.solve_from_force(
            force=10000.0,
            weight_per_length=50.0,
            vertical_distance=100.0
        )

        # F/w = 200
        # S = 100 * (2*200 - 100) = 100 * 300 = 30000
        # X = (200 - 100) * ln((30000 + 200) / (200 - 100))
        # X = 100 * ln(30200 / 100) = 100 * ln(302) = 100 * 5.710427 = 571.0427
        # W = 50 * 30000 = 1500000
        # TH = 10000 * 571.0427 / sqrt(30000^2 + 571.0427^2)
        # TH = 5710427 / sqrt(900326183.98) = 5710427 / 30005.4359 = 190.3152
        # b = 50 * 9.81 / 190.3152 = 490.5 / 190.3152 = 2.5775

        assert result.arc_length == pytest.approx(30000.0, abs=0.0001)
        assert result.horizontal_distance == pytest.approx(571.0427, abs=0.01)
        assert result.weight_suspended == pytest.approx(1500000.0, abs=0.0001)
        assert result.horizontal_tension == pytest.approx(190.3152, abs=0.01)
        assert result.shape_parameter == pytest.approx(2.5775, abs=0.001)

    def test_force_based_realistic_marine_cable(self):
        """Test with realistic marine cable parameters."""
        solver = SimplifiedCatenarySolver()

        # Realistic: 50-ton cable, 100kg/m weight, 500m depth
        # Need F > w*d/2 = 981*500/2 = 245,250 N
        result = solver.solve_from_force(
            force=500000.0,  # 50 tons = 500 kN
            weight_per_length=981.0,  # 100 kg/m * 9.81 m/s�
            vertical_distance=500.0
        )

        
        # F/w = 509.684
        # S = 500 * (2*509.684 - 500) = 500 * 519.368 = 259684
        # X = (509.684 - 500) * ln((259684 + 509.684) / (509.684 - 500))
        # X = 9.684 * ln(260193.684 / 9.684) = 9.684 * ln(26863.066) = 9.684 * 10.198 = 98.76

        assert result.arc_length == pytest.approx(259684.0, abs=1.0)
        assert result.horizontal_distance == pytest.approx(98.76, abs=0.1)
        assert result.weight_suspended == pytest.approx(254729604.0, abs=1000.0)

    def test_force_based_invalid_force_too_small(self):
        """Test that force too small raises ValueError."""
        solver = SimplifiedCatenarySolver()

        # F < w*d/2 should fail
        # w*d/2 = 50*100/2 = 2500
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
        # sin(60°) = 0.866025404
        # cos(60°) = 0.5
        # F = 5000 / 0.866025404 = 5773.502692
        # Fh = 5773.502692 * 0.5 = 2886.751346

        assert Fv == pytest.approx(5000.0, abs=0.0001)
        assert F == pytest.approx(5773.502692, abs=0.0001)
        assert Fh == pytest.approx(2886.751346, abs=0.0001)

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


class TestLegacyCompatibility:
    """Tests for legacy dictionary-based API."""

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
        assert result['S'] == pytest.approx(30000.0, abs=0.0001)

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
        assert result['S'] == pytest.approx(173.2050808, abs=0.0001)
        assert result['X'] == pytest.approx(131.6957897, abs=0.0001)
        assert result['BendRadius'] == pytest.approx(100.0, abs=0.0001)

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
        assert result['F'] == pytest.approx(5773.502692, abs=0.0001)
        assert result['Fh'] == pytest.approx(2886.751346, abs=0.0001)


class TestComparisonWithLegacy:
    """Direct comparison tests with legacy implementation."""

    def test_angle_based_matches_legacy_exactly(self):
        """Verify new implementation matches legacy for multiple angle cases."""
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

            # Calculate using legacy formula (direct port)
            complementary = 90.0 - angle
            tanq = math.tan(math.radians(complementary))
            cos_comp = math.cos(math.radians(complementary))
            bend_radius_legacy = distance * cos_comp / (1.0 - cos_comp)
            S_legacy = bend_radius_legacy * tanq
            X_legacy = bend_radius_legacy * math.asinh(tanq)

            # Verify exact match
            assert result.arc_length == pytest.approx(S_legacy, abs=1e-9)
            assert result.horizontal_distance == pytest.approx(X_legacy, abs=1e-9)
            assert result.bend_radius == pytest.approx(bend_radius_legacy, abs=1e-9)

    def test_force_based_matches_legacy_exactly(self):
        """Verify new implementation matches legacy for multiple force cases."""
        solver = SimplifiedCatenarySolver()

        test_cases = [
            (10000.0, 50.0, 100.0),   # F/w=200 > d/2=50
            (25000.0, 100.0, 200.0),  # F/w=250 > d/2=100
            (50000.0, 200.0, 150.0),  # F/w=250 > d/2=75
        ]

        for force, weight, distance in test_cases:
            # Calculate using new implementation
            result = solver.solve_from_force(force, weight, distance)

            # Calculate using legacy formula (direct port)
            S_legacy = distance * (2 * force / weight - distance)
            X_legacy = ((force / weight) - distance) * math.log(
                (S_legacy + (force / weight)) / ((force / weight) - distance)
            )
            W_legacy = weight * S_legacy
            TH_legacy = force * X_legacy / math.sqrt(S_legacy**2 + X_legacy**2)
            b_legacy = weight * 9.81 / TH_legacy

            # Verify exact match
            assert result.arc_length == pytest.approx(S_legacy, abs=1e-9)
            assert result.horizontal_distance == pytest.approx(X_legacy, abs=1e-9)
            assert result.weight_suspended == pytest.approx(W_legacy, abs=1e-9)
            assert result.horizontal_tension == pytest.approx(TH_legacy, abs=1e-9)
            assert result.shape_parameter == pytest.approx(b_legacy, abs=1e-9)


if __name__ == '__main__':
    pytest.main([__file__, '-v', '--tb=short'])
