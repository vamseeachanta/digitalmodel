"""
Quick validation script to test catenary module without pytest conftest issues.
"""

import warnings

# Test imports
print("Testing imports...")
from digitalmodel.marine_ops.marine_engineering.catenary import (
    CatenarySolver, CatenaryInput,
    catenaryEquation, catenaryForces
)
from digitalmodel.marine_ops.marine_engineering.catenary.simplified import SimplifiedCatenarySolver

print("[OK] Imports successful")

# Test Phase 1 solver
print("\nTesting Phase 1 BVP solver...")
params = CatenaryInput(
    length=1000.0,
    horizontal_span=800.0,
    vertical_span=100.0,
    weight_per_length=1962.0,
    ea_stiffness=64e9
)
solver = CatenarySolver()
result = solver.solve(params)

print(f"[OK] Phase 1 solver converged: {result.converged}")
print(f"  H = {result.horizontal_tension:.1f} N")
print(f"  V = {result.vertical_tension_fairlead:.1f} N")
print(f"  T = {result.total_tension_fairlead:.1f} N")

# Test simplified solver
print("\nTesting simplified solver...")
simp_solver = SimplifiedCatenarySolver()
simp_result = simp_solver.solve_from_angle(30.0, 100.0)

print(f"[OK] Simplified angle-based:")
print(f"  S = {simp_result.arc_length:.2f} m")
print(f"  X = {simp_result.horizontal_distance:.2f} m")

# Test adapter (legacy compatibility)
print("\nTesting adapter (legacy API)...")
with warnings.catch_warnings():
    warnings.filterwarnings('ignore', category=DeprecationWarning)
    adapter_result = catenaryEquation({
        'q': 30, 'd': 100,
        'F': None, 'w': None, 'X': None
    })

print(f"[OK] Adapter result:")
print(f"  S = {adapter_result['S']:.2f} m")
print(f"  X = {adapter_result['X']:.2f} m")

# Verify consistency
print("\nVerifying consistency...")
assert abs(simp_result.arc_length - adapter_result['S']) < 0.01, "Simplified vs adapter mismatch!"
assert abs(simp_result.horizontal_distance - adapter_result['X']) < 0.01, "Simplified vs adapter mismatch!"

print("[OK] All components consistent!")

print("\n" + "="*60)
print("SUCCESS: Unified catenary module working correctly!")
print("="*60)
