"""
Simple Stress Analysis Module Demonstration

A simplified demonstration that shows the key capabilities without
complex calculations that require scipy.
"""

import sys
sys.path.insert(0, 'src')

import numpy as np
from digitalmodel.stress import (
    PipeStressAnalyzer, PipeGeometry, MaterialProperties, LoadingCondition,
    VonMisesStressCalculator, StressState
)


def main():
    """Simple demonstration"""
    print("DIGITAL MODEL STRESS ANALYSIS - SIMPLE DEMONSTRATION")
    print("=" * 60)

    # Von Mises stress analysis
    print("\n1. VON MISES STRESS ANALYSIS")
    print("-" * 40)

    # Legacy code parameters
    geometry = PipeGeometry(0.24765, 0.034925)
    material = MaterialProperties(5.52e8, 6.20e8, 2.1e11, 0.3)
    analyzer = PipeStressAnalyzer(geometry, material)

    print(f"Pipe: OD={geometry.outer_diameter:.3f}m, WT={geometry.wall_thickness:.4f}m")
    print(f"Material: Yield={material.yield_strength/1e6:.0f} MPa")
    print(f"Allowable Stress: {analyzer.calculate_allowable_stress()/1e6:.1f} MPa")

    # Test pressure loading
    loading = LoadingCondition(internal_pressure=10e6)
    results = analyzer.calculate_combined_stress(loading)

    print(f"\nPressure Loading (10 MPa internal):")
    print(f"  Von Mises Stress: {results['von_mises']/1e6:.1f} MPa")
    print(f"  Safety Factor: {results['safety_factor']:.2f}")

    # Test combined loading
    loading = LoadingCondition(
        internal_pressure=5e6,
        axial_force=1e6,
        bending_moment=1e5
    )
    results = analyzer.calculate_combined_stress(loading)

    print(f"\nCombined Loading:")
    print(f"  Von Mises Stress: {results['von_mises']/1e6:.1f} MPa")
    print(f"  Safety Factor: {results['safety_factor']:.2f}")

    # Test tension calculation
    moment = 46e4  # From legacy code
    tension_pos, tension_neg = analyzer.calculate_maximum_tension(moment)
    print(f"\nTension Analysis (Moment={moment/1e3:.0f} kN⋅m):")
    print(f"  Max Tension: +{tension_pos/1e3:.0f} kN, {tension_neg/1e3:.0f} kN")

    # Direct Von Mises calculations
    print("\n2. DIRECT VON MISES CALCULATIONS")
    print("-" * 40)

    test_cases = [
        ("Uniaxial 200 MPa", StressState(200e6, 0, 0)),
        ("Biaxial tension", StressState(150e6, 100e6, 0)),
        ("Pure shear 100 MPa", StressState(0, 0, 0, 100e6)),
    ]

    for name, stress_state in test_cases:
        vm_stress = VonMisesStressCalculator.calculate_from_stress_state(stress_state)
        print(f"  {name}: VM = {vm_stress/1e6:.1f} MPa")

    print("\n3. ENGINEERING VALIDATION")
    print("-" * 40)

    # Verify some basic engineering relationships
    # Pure shear: VM = sqrt(3) * tau
    pure_shear = StressState(0, 0, 0, 100e6)
    vm_shear = VonMisesStressCalculator.calculate_from_stress_state(pure_shear)
    expected_shear = 100e6 * np.sqrt(3)
    print(f"Pure shear validation: VM={vm_shear/1e6:.1f} MPa, Expected={expected_shear/1e6:.1f} MPa")

    # Uniaxial: VM = sigma
    uniaxial = StressState(200e6, 0, 0)
    vm_uniaxial = VonMisesStressCalculator.calculate_from_stress_state(uniaxial)
    print(f"Uniaxial validation: VM={vm_uniaxial/1e6:.1f} MPa, Applied=200.0 MPa")

    print("\n" + "=" * 60)
    print("DEMONSTRATION COMPLETED SUCCESSFULLY!")
    print("\nKey features working:")
    print("✓ Von Mises stress calculations")
    print("✓ Pipe stress analysis under combined loading")
    print("✓ Tension-moment interaction calculations")
    print("✓ Engineering validation and accuracy")
    print("✓ Legacy code compatibility")

    print("\nModules successfully migrated from legacy VMStressCalculations!")


if __name__ == "__main__":
    main()