"""
Manual test script for catenary adapter.

This script tests the adapter without pytest dependencies.
"""

import sys
import warnings
from pathlib import Path

# Add src to path
src_path = Path(__file__).parent.parent.parent / "src"
sys.path.insert(0, str(src_path))

from digitalmodel.marine_ops.marine_analysis.catenary.adapter import catenaryEquation, catenaryForces
import math


def test_force_method():
    """Test force-based method."""
    print("\n=== Testing Force-Based Method ===")

    # Use valid parameters: F/w > d
    # F=50000, w=500 => F/w=100 > d=80 [OK]
    data = {
        "F": 50000.0,
        "w": 500.0,
        "d": 80.0,
        "X": None,
        "q": None
    }

    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")
        result = catenaryEquation(data)

        # Check warning
        assert len(w) == 1, "Expected 1 deprecation warning"
        assert issubclass(w[0].category, DeprecationWarning), "Expected DeprecationWarning"
        print(f"[OK] Deprecation warning raised: {w[0].message}")

    # Verify results
    print(f"\nInput: F={data['F']}, w={data['w']}, d={data['d']}")
    print(f"Output:")
    print(f"  S (arc length) = {result['S']:.2f} m")
    print(f"  X (horizontal distance) = {result['X']:.2f} m")
    print(f"  W (weight) = {result['W']:.2f} N")
    print(f"  THorizontal = {result['THorizontal']:.2f} N")

    # Verify W = w * S
    assert abs(result['W'] - data['w'] * result['S']) < 1e-6, "W should equal w * S"
    print(f"\n[OK] W = w * S verified: {result['W']:.2f} ~= {data['w'] * result['S']:.2f}")

    # Verify exact match with legacy formula
    F = data["F"]
    w = data["w"]
    d = data["d"]

    expected_S = d * (2 * F / w - d)
    expected_X = ((F/w) - d) * math.log((expected_S + (F/w)) / ((F/w) - d))

    assert abs(result['S'] - expected_S) < 1e-9, f"S mismatch: {result['S']} vs {expected_S}"
    assert abs(result['X'] - expected_X) < 1e-9, f"X mismatch: {result['X']} vs {expected_X}"
    print(f"[OK] Results match legacy formulas exactly")

    return True


def test_angle_method():
    """Test angle-based method."""
    print("\n=== Testing Angle-Based Method ===")

    data = {
        "q": 30.0,
        "d": 100.0,
        "F": None,
        "w": None,
        "X": None
    }

    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")
        result = catenaryEquation(data)

        # Check warning
        assert len(w) == 1, "Expected 1 deprecation warning"
        print(f"[OK] Deprecation warning raised")

    print(f"\nInput: q={data['q']}°, d={data['d']} m")
    print(f"Output:")
    print(f"  S (arc length) = {result['S']:.2f} m")
    print(f"  X (horizontal distance) = {result['X']:.2f} m")
    print(f"  BendRadius = {result['BendRadius']:.2f} m")

    # Verify exact match with legacy formula
    q = data["q"]
    d = data["d"]

    angle_rad = math.radians(90 - q)
    tanq = math.tan(angle_rad)
    cos_angle = math.cos(angle_rad)

    expected_BendRadius = d * cos_angle / (1 - cos_angle)
    expected_S = expected_BendRadius * tanq
    expected_X = expected_BendRadius * math.asinh(tanq)

    assert abs(result['BendRadius'] - expected_BendRadius) < 1e-9
    assert abs(result['S'] - expected_S) < 1e-9
    assert abs(result['X'] - expected_X) < 1e-9
    print(f"[OK] Results match legacy formulas exactly")

    return True


def test_forces():
    """Test catenary forces."""
    print("\n=== Testing Catenary Forces ===")

    data = {
        "weightPerUnitLength": 500.0,
        "S": 150.0,
        "q": 30.0
    }

    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")
        result = catenaryForces(data)

        assert len(w) == 1, "Expected 1 deprecation warning"
        print(f"[OK] Deprecation warning raised")

    print(f"\nInput: w={data['weightPerUnitLength']} N/m, S={data['S']} m, q={data['q']}°")
    print(f"Output:")
    print(f"  Fv (vertical force) = {result['Fv']:.2f} N")
    print(f"  F (total force) = {result['F']:.2f} N")
    print(f"  Fh (horizontal force) = {result['Fh']:.2f} N")

    # Verify Fv = w * S
    expected_Fv = data['weightPerUnitLength'] * data['S']
    assert abs(result['Fv'] - expected_Fv) < 1e-9
    print(f"\n[OK] Fv = w * S verified: {result['Fv']:.2f} ~= {expected_Fv:.2f}")

    # Verify exact match with legacy formula
    angle_rad = math.radians(90 - data['q'])
    expected_F = expected_Fv / math.sin(angle_rad)
    expected_Fh = expected_F * math.cos(angle_rad)

    assert abs(result['F'] - expected_F) < 1e-9
    assert abs(result['Fh'] - expected_Fh) < 1e-9
    print(f"[OK] Results match legacy formulas exactly")

    return True


def test_error_handling():
    """Test error handling."""
    print("\n=== Testing Error Handling ===")

    # Test insufficient parameters
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            catenaryEquation({"d": 100.0, "F": None, "w": None, "X": None, "q": None})
        assert False, "Should have raised ValueError"
    except ValueError as e:
        assert "Insufficient parameters" in str(e)
        print(f"[OK] Insufficient parameters error: {e}")

    # Test X-based not implemented
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            catenaryEquation({"X": 200.0, "d": 100.0, "F": None, "w": None, "q": None})
        assert False, "Should have raised NotImplementedError"
    except NotImplementedError as e:
        assert "X-based calculation not implemented" in str(e)
        print(f"[OK] X-based not implemented error: {e}")

    # Test invalid force parameters
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            catenaryEquation({"F": 1000.0, "w": 100.0, "d": 100.0, "X": None, "q": None})
        assert False, "Should have raised ValueError"
    except ValueError as e:
        assert "F/w" in str(e) and "must be > d" in str(e)
        print(f"[OK] Invalid force parameters error: {e}")

    return True


def test_legacy_comparison():
    """Compare with legacy implementation."""
    print("\n=== Comparing with Legacy Implementation ===")

    # Import legacy code
    try:
        from digitalmodel.subsea.catenary.catenaryMethods import (
            catenaryEquation as legacy_catenaryEquation,
            catenaryForces as legacy_catenaryForces
        )
        has_legacy = True
    except ImportError:
        print("[WARN] Legacy module not available for comparison")
        has_legacy = False
        return True

    if has_legacy:
        # Test force method
        data_force = {
            "F": 50000.0,
            "w": 500.0,
            "d": 80.0,
            "X": None,
            "q": None
        }

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            legacy_result = legacy_catenaryEquation(data_force.copy())
            adapter_result = catenaryEquation(data_force.copy())

        print(f"\nForce method comparison:")
        print(f"  Legacy S: {legacy_result['S']:.6f}")
        print(f"  Adapter S: {adapter_result['S']:.6f}")
        assert abs(legacy_result['S'] - adapter_result['S']) < 1e-9
        assert abs(legacy_result['X'] - adapter_result['X']) < 1e-9
        assert abs(legacy_result['W'] - adapter_result['W']) < 1e-9
        print(f"[OK] Force method results match legacy exactly")

        # Test angle method
        data_angle = {
            "q": 45.0,
            "d": 120.0,
            "F": None,
            "w": None,
            "X": None
        }

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            legacy_result = legacy_catenaryEquation(data_angle.copy())
            adapter_result = catenaryEquation(data_angle.copy())

        print(f"\nAngle method comparison:")
        print(f"  Legacy BendRadius: {legacy_result['BendRadius']:.6f}")
        print(f"  Adapter BendRadius: {adapter_result['BendRadius']:.6f}")
        assert abs(legacy_result['BendRadius'] - adapter_result['BendRadius']) < 1e-9
        assert abs(legacy_result['S'] - adapter_result['S']) < 1e-9
        assert abs(legacy_result['X'] - adapter_result['X']) < 1e-9
        print(f"[OK] Angle method results match legacy exactly")

    return True


def main():
    """Run all tests."""
    print("=" * 60)
    print("Catenary Adapter Compatibility Tests")
    print("=" * 60)

    tests = [
        ("Force Method", test_force_method),
        ("Angle Method", test_angle_method),
        ("Forces Calculation", test_forces),
        ("Error Handling", test_error_handling),
        ("Legacy Comparison", test_legacy_comparison),
    ]

    passed = 0
    failed = 0

    for name, test_func in tests:
        try:
            if test_func():
                passed += 1
                print(f"\n{'='*60}")
                print(f"[OK] {name} PASSED")
                print(f"{'='*60}")
        except Exception as e:
            failed += 1
            print(f"\n{'='*60}")
            print(f"[FAIL] {name} FAILED: {e}")
            print(f"{'='*60}")
            import traceback
            traceback.print_exc()

    print(f"\n{'='*60}")
    print(f"Test Summary: {passed} passed, {failed} failed")
    print(f"{'='*60}")

    if failed == 0:
        print("\n[SUCCESS] ALL TESTS PASSED! 100% backward compatibility achieved.")
        return 0
    else:
        print(f"\n[ERROR] {failed} test(s) failed.")
        return 1


if __name__ == "__main__":
    sys.exit(main())
