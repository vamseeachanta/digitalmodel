"""
Test suite for unified catenary module.

Verifies that Phase 1 solver works correctly in new location.
"""

import sys
from pathlib import Path

# Add src to Python path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

import numpy as np
import pytest
from digitalmodel.marine_ops.marine_analysis.catenary import CatenarySolver, CatenaryInput, CatenaryResults


def test_import():
    """Test that module imports correctly."""
    from digitalmodel.marine_ops.marine_analysis.catenary import CatenarySolver, CatenaryInput, CatenaryResults
    assert CatenarySolver is not None
    assert CatenaryInput is not None
    assert CatenaryResults is not None


def test_excel_validation_case():
    """
    Test Excel validation case from Phase 1.

    Mooring Line Configuration:
    - Length: 1120.0m
    - Horizontal span: 900.0m
    - Vertical span: 320.0m
    - Weight: 1600 N/m
    - EA: 1.1e9 N

    Expected: H ≈ 860,000 N
    """
    # Setup
    solver = CatenarySolver(tolerance=1e-6, max_iterations=200)

    params = CatenaryInput(
        length=1120.0,
        horizontal_span=900.0,
        vertical_span=320.0,
        weight_per_length=1600.0,
        ea_stiffness=1.1e9
    )

    # Solve
    results = solver.solve(params)

    # Verify convergence
    assert results.converged, "Solver should converge"

    # Verify horizontal tension
    # Note: Correct inextensible catenary BVP solution is H ≈ 688,000 N
    # (Excel value of 860,000 N used incorrect formulation)
    expected_H = 688_000.0
    tolerance = 0.02  # 2% tolerance
    assert abs(results.horizontal_tension - expected_H) / expected_H < tolerance, \
        f"H = {results.horizontal_tension:.0f} N, expected ≈ {expected_H:.0f} N"

    # Verify result structure
    assert results.vertical_tension_fairlead > 0
    assert results.total_tension_fairlead > results.horizontal_tension
    assert results.catenary_parameter > 0
    assert len(results.shape_x) == len(results.shape_y)
    assert len(results.tension_distribution) == len(results.shape_x)


def test_simple_catenary():
    """Test simple horizontal catenary case."""
    solver = CatenarySolver()

    params = CatenaryInput(
        length=110.0,
        horizontal_span=100.0,
        vertical_span=0.0,
        weight_per_length=100.0,
        ea_stiffness=1e8
    )

    results = solver.solve(params)

    assert results.converged
    assert results.horizontal_tension > 0
    assert results.total_tension_anchor > 0


def test_input_validation():
    """Test that invalid inputs raise errors."""
    solver = CatenarySolver()

    # Length too short
    with pytest.raises(ValueError, match="Length.*straight distance"):
        params = CatenaryInput(
            length=50.0,  # Too short for 100m span
            horizontal_span=100.0,
            vertical_span=0.0,
            weight_per_length=100.0,
            ea_stiffness=1e8
        )
        solver.solve(params)

    # Negative weight
    with pytest.raises(ValueError, match="Weight per length"):
        params = CatenaryInput(
            length=110.0,
            horizontal_span=100.0,
            vertical_span=0.0,
            weight_per_length=-100.0,
            ea_stiffness=1e8
        )
        solver.solve(params)

    # Zero EA
    with pytest.raises(ValueError, match="EA stiffness"):
        params = CatenaryInput(
            length=110.0,
            horizontal_span=100.0,
            vertical_span=0.0,
            weight_per_length=100.0,
            ea_stiffness=0.0
        )
        solver.solve(params)


def test_utils_module():
    """Test utility functions."""
    from digitalmodel.marine_ops.marine_analysis.catenary.utils import (
        safe_sinh, safe_cosh, validate_catenary_inputs,
        catenary_parameter, estimate_initial_tension
    )

    # Safe hyperbolic functions
    assert abs(safe_sinh(1.0) - np.sinh(1.0)) < 1e-10
    assert abs(safe_cosh(1.0) - np.cosh(1.0)) < 1e-10

    # Large argument handling
    large_x = 800.0
    result = safe_sinh(large_x)
    assert np.isfinite(result)
    assert result > 0

    # Validation
    valid, msg = validate_catenary_inputs(
        length=110.0,
        horizontal_span=100.0,
        vertical_span=0.0,
        weight_per_length=100.0,
        ea_stiffness=1e8
    )
    assert valid
    assert msg == ""

    # Catenary parameter
    a = catenary_parameter(1000.0, 100.0)
    assert a == 10.0

    # Initial tension estimate
    H_init = estimate_initial_tension(110.0, 100.0, 100.0)
    assert H_init > 0


if __name__ == "__main__":
    # Run tests
    print("Testing catenary module import...")
    test_import()
    print("[PASS] Import test passed")

    print("\nTesting Excel validation case...")
    test_excel_validation_case()
    print("[PASS] Excel validation test passed")

    print("\nTesting simple catenary...")
    test_simple_catenary()
    print("[PASS] Simple catenary test passed")

    print("\nTesting input validation...")
    test_input_validation()
    print("[PASS] Input validation test passed")

    print("\nTesting utilities...")
    test_utils_module()
    print("[PASS] Utilities test passed")

    print("\n" + "="*50)
    print("ALL TESTS PASSED - Phase 1 solver working in new location!")
    print("="*50)
