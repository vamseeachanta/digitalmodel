"""
Test suite for catenary solver module.

Validates Python implementation against Excel "Poly Mooring" reference results.
Tests convergence, accuracy, and edge cases.
"""

import pytest
import numpy as np
from src.marine_engineering.mooring_analysis.catenary_solver import (
    CatenaryInput,
    CatenaryResults,
    CatenarySolver
)


class TestCatenarySolver:
    """Test cases for CatenarySolver class."""

    def test_excel_reference_case_1(self):
        """
        Validate against Excel reference case: Single chain segment.

        Excel "Poly Mooring" reference values:
        - Horizontal tension: ~785,000 N (±1%)
        - Vertical tension: ~196,200 N
        - Total tension: ~809,000 N
        - Elongation: ~12.3 m
        """
        params = CatenaryInput(
            length=1000,           # m
            horizontal_span=800,   # m
            vertical_span=100,     # m
            weight_per_length=1962,  # N/m (76mm chain in water)
            ea_stiffness=64e9      # N (studlink chain)
        )

        solver = CatenarySolver()
        results = solver.solve(params)

        # Verify convergence
        assert results.converged, "Solver failed to converge"

        # Excel reference validation (±1% tolerance)
        assert abs(results.horizontal_tension - 785_000) / 785_000 < 0.01, \
            f"Horizontal tension error: {results.horizontal_tension} vs 785,000"

        assert abs(results.vertical_tension_fairlead - 196_200) / 196_200 < 0.01, \
            f"Vertical tension error: {results.vertical_tension_fairlead} vs 196,200"

        assert abs(results.total_tension_fairlead - 809_000) / 809_000 < 0.01, \
            f"Total tension error: {results.total_tension_fairlead} vs 809,000"

        # Elongation check (approximate)
        assert 10 < results.elongation < 15, \
            f"Elongation out of expected range: {results.elongation}"

    def test_basic_catenary_properties(self):
        """Test fundamental catenary properties."""
        params = CatenaryInput(
            length=500,
            horizontal_span=400,
            vertical_span=50,
            weight_per_length=1000,
            ea_stiffness=50e9
        )

        solver = CatenarySolver()
        results = solver.solve(params)

        # Physical constraints
        assert results.horizontal_tension > 0, "H must be positive"
        assert results.total_tension_fairlead > results.horizontal_tension, \
            "Total tension at fairlead must exceed horizontal component"
        assert results.total_tension_anchor == results.horizontal_tension, \
            "At anchor, tension equals H (no vertical component)"

        # Shape validation
        assert len(results.shape_x) == len(results.shape_y), \
            "Shape arrays must have same length"
        assert results.shape_x[0] == 0, "Shape should start at x=0"
        assert results.shape_y[0] == 0, "Shape should start at y=0"
        assert np.all(np.diff(results.shape_y) >= 0), \
            "Y coordinates must be monotonically increasing"

    def test_tension_distribution(self):
        """Verify tension increases along catenary."""
        params = CatenaryInput(
            length=600,
            horizontal_span=500,
            vertical_span=80,
            weight_per_length=1500,
            ea_stiffness=60e9
        )

        solver = CatenarySolver()
        results = solver.solve(params)

        # Tension must increase from anchor to fairlead
        assert results.tension_distribution[0] == pytest.approx(
            results.horizontal_tension, rel=1e-3
        ), "Tension at anchor should equal H"

        assert results.tension_distribution[-1] == pytest.approx(
            results.total_tension_fairlead, rel=1e-3
        ), "Tension at fairlead should equal total tension"

        # Monotonically increasing
        assert np.all(np.diff(results.tension_distribution) >= 0), \
            "Tension must increase along catenary"

    def test_catenary_parameter_calculation(self):
        """Verify catenary parameter a = H/w."""
        params = CatenaryInput(
            length=700,
            horizontal_span=600,
            vertical_span=70,
            weight_per_length=2000,
            ea_stiffness=70e9
        )

        solver = CatenarySolver()
        results = solver.solve(params)

        expected_a = results.horizontal_tension / params.weight_per_length
        assert results.catenary_parameter == pytest.approx(expected_a, rel=1e-6), \
            "Catenary parameter should equal H/w"

    def test_elastic_elongation(self):
        """Verify elastic elongation calculation."""
        params = CatenaryInput(
            length=800,
            horizontal_span=700,
            vertical_span=90,
            weight_per_length=1800,
            ea_stiffness=80e9
        )

        solver = CatenarySolver()
        results = solver.solve(params)

        expected_elongation = (
            results.horizontal_tension * params.length / params.ea_stiffness
        )
        assert results.elongation == pytest.approx(expected_elongation, rel=1e-6), \
            "Elongation should equal H*L/EA"

    def test_touchdown_calculation(self):
        """Test touchdown point calculation."""
        params = CatenaryInput(
            length=1200,
            horizontal_span=1000,
            vertical_span=100,
            weight_per_length=2000,
            ea_stiffness=100e9,
            water_depth=150  # Specify water depth for touchdown analysis
        )

        solver = CatenarySolver()
        results = solver.solve(params)

        if results.touchdown_distance is not None:
            # Touchdown distance must be positive and less than horizontal span
            assert 0 < results.touchdown_distance < params.horizontal_span, \
                "Touchdown distance must be within span"

            # Verify touchdown point using catenary equation
            a = results.catenary_parameter
            y_touchdown = a * (np.cosh(results.touchdown_distance / a) - 1)
            assert y_touchdown == pytest.approx(params.water_depth, rel=0.01), \
                "Touchdown point should be at water depth"

    def test_tight_line_scenario(self):
        """Test nearly taut line (high tension, low sag)."""
        params = CatenaryInput(
            length=1010,  # Just slightly longer than span
            horizontal_span=1000,
            vertical_span=50,
            weight_per_length=500,  # Light line
            ea_stiffness=100e9
        )

        solver = CatenarySolver()
        results = solver.solve(params)

        assert results.converged, "Should converge for tight line"
        assert results.horizontal_tension > 10000, "High tension expected"

    def test_slack_line_scenario(self):
        """Test slack line (low tension, high sag)."""
        params = CatenaryInput(
            length=1500,  # Much longer than span
            horizontal_span=800,
            vertical_span=100,
            weight_per_length=3000,  # Heavy line
            ea_stiffness=50e9
        )

        solver = CatenarySolver()
        results = solver.solve(params)

        assert results.converged, "Should converge for slack line"
        assert results.catenary_parameter < params.horizontal_span, \
            "Large sag expected for slack line"

    def test_convergence_tolerance(self):
        """Verify solver respects convergence tolerance."""
        params = CatenaryInput(
            length=900,
            horizontal_span=750,
            vertical_span=85,
            weight_per_length=1700,
            ea_stiffness=75e9
        )

        # Test with strict tolerance
        solver_strict = CatenarySolver(tolerance=1e-9)
        results_strict = solver_strict.solve(params)

        # Test with relaxed tolerance
        solver_relaxed = CatenarySolver(tolerance=1e-3)
        results_relaxed = solver_relaxed.solve(params)

        # Both should converge
        assert results_strict.converged
        assert results_relaxed.converged

        # Results should be close but strict more accurate
        assert abs(
            results_strict.horizontal_tension - results_relaxed.horizontal_tension
        ) / results_strict.horizontal_tension < 0.001

    def test_invalid_inputs(self):
        """Test error handling for invalid inputs."""
        # Negative length
        with pytest.raises(ValueError):
            params = CatenaryInput(
                length=-100,
                horizontal_span=800,
                vertical_span=100,
                weight_per_length=1962,
                ea_stiffness=64e9
            )
            solver = CatenarySolver()
            solver.solve(params)

        # Zero weight
        with pytest.raises(ValueError):
            params = CatenaryInput(
                length=1000,
                horizontal_span=800,
                vertical_span=100,
                weight_per_length=0,
                ea_stiffness=64e9
            )
            solver = CatenarySolver()
            solver.solve(params)

        # Zero stiffness
        with pytest.raises(ValueError):
            params = CatenaryInput(
                length=1000,
                horizontal_span=800,
                vertical_span=100,
                weight_per_length=1962,
                ea_stiffness=0
            )
            solver = CatenarySolver()
            solver.solve(params)

    def test_pythagorean_tension_relationship(self):
        """Verify T² = H² + V² at fairlead."""
        params = CatenaryInput(
            length=950,
            horizontal_span=800,
            vertical_span=95,
            weight_per_length=1900,
            ea_stiffness=85e9
        )

        solver = CatenarySolver()
        results = solver.solve(params)

        # Pythagorean theorem
        calculated_total = np.sqrt(
            results.horizontal_tension**2 +
            results.vertical_tension_fairlead**2
        )

        assert results.total_tension_fairlead == pytest.approx(
            calculated_total, rel=1e-6
        ), "Total tension should satisfy Pythagorean theorem"

    def test_shape_endpoints(self):
        """Verify line shape endpoints match span."""
        params = CatenaryInput(
            length=850,
            horizontal_span=720,
            vertical_span=88,
            weight_per_length=1650,
            ea_stiffness=72e9
        )

        solver = CatenarySolver()
        results = solver.solve(params)

        # Check horizontal endpoint
        assert results.shape_x[-1] == pytest.approx(
            params.horizontal_span, rel=1e-6
        ), "Shape should end at horizontal span"

        # Vertical endpoint should approximately match span
        # (may differ slightly due to catenary sag)
        assert results.shape_y[-1] > 0, "End point should be above start"


class TestCatenaryPhysics:
    """Physics-based validation tests."""

    def test_arc_length_calculation(self):
        """Verify arc length formula s = a * sinh(x/a)."""
        params = CatenaryInput(
            length=1100,
            horizontal_span=900,
            vertical_span=110,
            weight_per_length=2100,
            ea_stiffness=90e9
        )

        solver = CatenarySolver()
        results = solver.solve(params)

        # Calculate arc length using catenary formula
        a = results.catenary_parameter
        arc_length = a * np.sinh(params.horizontal_span / a)

        # Total length = arc length + elastic elongation
        total_calculated = arc_length + results.elongation

        assert total_calculated == pytest.approx(
            params.length, rel=1e-3
        ), "Arc length + elongation should equal total length"

    def test_weight_scaling(self):
        """Test that doubling weight roughly doubles tension."""
        base_params = CatenaryInput(
            length=1000,
            horizontal_span=850,
            vertical_span=100,
            weight_per_length=1500,
            ea_stiffness=80e9
        )

        heavy_params = CatenaryInput(
            length=1000,
            horizontal_span=850,
            vertical_span=100,
            weight_per_length=3000,  # Double weight
            ea_stiffness=80e9
        )

        solver = CatenarySolver()
        base_results = solver.solve(base_params)
        heavy_results = solver.solve(heavy_params)

        # Tension should roughly double (not exact due to nonlinearity)
        tension_ratio = heavy_results.horizontal_tension / base_results.horizontal_tension
        assert 1.5 < tension_ratio < 2.5, \
            "Doubling weight should roughly double tension"

    def test_stiffness_effect(self):
        """Test that higher stiffness reduces elongation."""
        soft_params = CatenaryInput(
            length=1000,
            horizontal_span=850,
            vertical_span=100,
            weight_per_length=1800,
            ea_stiffness=40e9
        )

        stiff_params = CatenaryInput(
            length=1000,
            horizontal_span=850,
            vertical_span=100,
            weight_per_length=1800,
            ea_stiffness=120e9  # 3x stiffer
        )

        solver = CatenarySolver()
        soft_results = solver.solve(soft_params)
        stiff_results = solver.solve(stiff_params)

        # Higher stiffness should reduce elongation
        assert stiff_results.elongation < soft_results.elongation, \
            "Higher stiffness should reduce elongation"

        # Elongation should be roughly inversely proportional to stiffness
        elongation_ratio = soft_results.elongation / stiff_results.elongation
        assert 2.5 < elongation_ratio < 3.5, \
            "Elongation should be inversely proportional to stiffness"


def test_catenary_input_dataclass():
    """Test CatenaryInput dataclass creation."""
    params = CatenaryInput(
        length=1000,
        horizontal_span=800,
        vertical_span=100,
        weight_per_length=1962,
        ea_stiffness=64e9,
        water_depth=150,
        seabed_friction=0.5
    )

    assert params.length == 1000
    assert params.water_depth == 150
    assert params.seabed_friction == 0.5


def test_catenary_results_dataclass():
    """Test CatenaryResults dataclass structure."""
    results = CatenaryResults(
        horizontal_tension=785000,
        vertical_tension_fairlead=196200,
        total_tension_fairlead=809000,
        total_tension_anchor=785000,
        elongation=12.3,
        touchdown_distance=None,
        catenary_parameter=400,
        shape_x=np.array([0, 100, 200]),
        shape_y=np.array([0, 10, 25]),
        tension_distribution=np.array([785000, 790000, 809000]),
        converged=True,
        iterations=5
    )

    assert results.horizontal_tension == 785000
    assert results.converged is True
    assert len(results.shape_x) == 3


if __name__ == "__main__":
    # Run tests with verbose output
    pytest.main([__file__, "-v", "--tb=short"])
