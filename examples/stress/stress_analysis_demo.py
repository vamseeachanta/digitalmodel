"""
Stress Analysis Module Demonstration

This script demonstrates the key capabilities of the migrated stress analysis
modules, showing how they reproduce and extend the legacy VMStressCalculations
functionality with modern Python best practices.
"""

import numpy as np
from digitalmodel.stress import (
    # Von Mises stress analysis
    PipeStressAnalyzer, PipeGeometry, MaterialProperties, LoadingCondition,
    VonMisesStressCalculator, StressState,

    # Stress-strain analysis
    StressStrainAnalyzer, MaterialModel,

    # Nonlinear analysis
    NonlinearStressAnalyzer, VonMisesYield, LinearHardening
)


def demonstrate_vm_stress_analysis():
    """Demonstrate Von Mises stress analysis capabilities"""
    print("=" * 60)
    print("VON MISES STRESS ANALYSIS DEMONSTRATION")
    print("=" * 60)

    # Legacy code parameters
    geometry = PipeGeometry(
        outer_diameter=0.24765,   # m (from legacy code)
        wall_thickness=0.034925   # m (from legacy code)
    )

    material = MaterialProperties(
        yield_strength=5.52e8,    # Pa (from legacy code)
        ultimate_strength=6.20e8, # Pa
        elastic_modulus=2.1e11,   # Pa
        poisson_ratio=0.3
    )

    analyzer = PipeStressAnalyzer(geometry, material)

    print(f"Pipe Properties:")
    print(f"  Outer Diameter: {geometry.outer_diameter:.5f} m")
    print(f"  Inner Diameter: {geometry.inner_diameter:.5f} m")
    print(f"  Wall Thickness: {geometry.wall_thickness:.6f} m")
    print(f"  Cross-sectional Area: {geometry.cross_sectional_area:.6e} m²")

    print(f"\nMaterial Properties:")
    print(f"  Yield Strength: {material.yield_strength/1e6:.0f} MPa")
    print(f"  Allowable Stress: {analyzer.calculate_allowable_stress()/1e6:.1f} MPa")

    # Test cases from legacy code
    test_cases = [
        ("Pure bending", LoadingCondition(bending_moment=46e4)),
        ("Internal pressure", LoadingCondition(internal_pressure=10e6)),
        ("Combined loading", LoadingCondition(
            internal_pressure=5e6, axial_force=1e6, bending_moment=2e5
        ))
    ]

    print(f"\nStress Analysis Results:")
    print("-" * 80)
    print(f"{'Case':<15} {'VM Stress':<12} {'Safety Factor':<15} {'Status':<10}")
    print("-" * 80)

    for case_name, loading in test_cases:
        results = analyzer.calculate_combined_stress(loading)
        vm_stress = results['von_mises'] / 1e6  # Convert to MPa
        safety_factor = results['safety_factor']

        status = "SAFE" if safety_factor > 1.0 else "UNSAFE"
        print(f"{case_name:<15} {vm_stress:>9.1f} MPa {safety_factor:>12.2f} {status:>12}")

    # Demonstrate tension calculation (legacy feature)
    print(f"\nTension-Moment Analysis (Legacy Reproduction):")
    moment = 46e4  # N⋅m (from legacy code)
    tension_pos, tension_neg = analyzer.calculate_maximum_tension(moment)
    print(f"  Moment: {moment/1e3:.0f} kN⋅m")
    print(f"  Max Positive Tension: {tension_pos/1e3:.0f} kN")
    print(f"  Max Negative Tension: {tension_neg/1e3:.0f} kN")


def demonstrate_stress_strain_analysis():
    """Demonstrate stress-strain analysis capabilities"""
    print("\n" + "=" * 60)
    print("STRESS-STRAIN ANALYSIS DEMONSTRATION")
    print("=" * 60)

    analyzer = StressStrainAnalyzer()

    # Legacy Ramberg-Osgood parameters
    legacy_params = {
        'elastic_modulus': 2.12e8,   # Pa (from legacy code)
        'yield_strength': 8.27e5,    # Pa (from legacy code)
        'k': 0.002,                  # From legacy code
        'n': 18.85                   # From legacy code
    }

    print(f"Ramberg-Osgood Parameters (from legacy code):")
    print(f"  Elastic Modulus: {legacy_params['elastic_modulus']/1e6:.0f} MPa")
    print(f"  Yield Strength: {legacy_params['yield_strength']/1e3:.0f} kPa")
    print(f"  K parameter: {legacy_params['k']:.3f}")
    print(f"  n parameter: {legacy_params['n']:.2f}")

    # Generate stress-strain curve
    strain_range = np.linspace(0, 0.02, 100)  # Up to 2% strain

    curve = analyzer.generate_curve(
        MaterialModel.RAMBERG_OSGOOD,
        strain_range,
        **legacy_params
    )

    # Calculate specific points (reproducing legacy calculations)
    from digitalmodel.stress.stress_strain import RambergOsgoodModel
    ro_model = RambergOsgoodModel()

    print(f"\nStress-Strain Points (reproducing legacy calculations):")
    print(f"{'Stress (kPa)':<12} {'Strain (%)':<12} {'Type':<15}")
    print("-" * 40)

    test_stresses = [100e3, 200e3, 400e3, 600e3, 800e3]  # From legacy code

    for stress in test_stresses:
        strain = ro_model.calculate_strain(stress, **legacy_params)

        # Determine if elastic or plastic
        elastic_strain = stress / legacy_params['elastic_modulus']
        stress_type = "Elastic" if strain <= elastic_strain * 1.01 else "Plastic"

        print(f"{stress/1e3:>8.0f}    {strain*100:>9.4f}    {stress_type:<15}")

    # Calculate engineering properties
    properties = analyzer.calculate_engineering_properties(curve)
    print(f"\nEngineering Properties:")
    print(f"  Calculated E: {properties['elastic_modulus']/1e6:.0f} MPa")
    print(f"  Ultimate Strength: {properties['ultimate_strength']/1e3:.0f} kPa")


def demonstrate_nonlinear_analysis():
    """Demonstrate nonlinear stress analysis capabilities"""
    print("\n" + "=" * 60)
    print("NONLINEAR STRESS ANALYSIS DEMONSTRATION")
    print("=" * 60)

    # Material properties
    elastic_modulus = 200e9  # 200 GPa
    yield_stress = 250e6     # 250 MPa
    hardening_modulus = 2e9  # 2 GPa

    print(f"Material Properties:")
    print(f"  Elastic Modulus: {elastic_modulus/1e9:.0f} GPa")
    print(f"  Yield Stress: {yield_stress/1e6:.0f} MPa")
    print(f"  Hardening Modulus: {hardening_modulus/1e9:.1f} GPa")

    # Create analysis components
    yield_criterion = VonMisesYield(yield_stress)
    hardening_model = LinearHardening(hardening_modulus)

    analyzer = NonlinearStressAnalyzer(
        elastic_modulus, 0.3, yield_criterion, hardening_model
    )

    # Test different strain levels
    yield_strain = yield_stress / elastic_modulus
    test_strains = [
        ("Elastic", yield_strain * 0.8),
        ("At yield", yield_strain),
        ("Plastic", yield_strain * 2.0),
        ("Large plastic", yield_strain * 3.0)
    ]

    print(f"\nNonlinear Response Analysis:")
    print(f"{'Case':<15} {'Strain (%)':<12} {'Stress (MPa)':<15} {'Plastic (%)':<15}")
    print("-" * 65)

    for case_name, strain in test_strains:
        total_strain = np.array([strain, 0, 0, 0, 0, 0])
        solution = analyzer.solve_incremental_plasticity(total_strain, 5)

        final_stress = solution.final_stress[0] / 1e6  # MPa
        plastic_strain = solution.plastic_strain_history[-1][0] * 100  # %

        print(f"{case_name:<15} {strain*100:>9.3f}    {final_stress:>11.1f}      {plastic_strain:>11.3f}")

    # Demonstrate yield criterion
    print(f"\nYield Criterion Evaluation:")
    stress_states = [
        ("Uniaxial at yield", np.array([yield_stress, 0, 0, 0, 0, 0])),
        ("Pure shear at yield", np.array([0, 0, 0, yield_stress/np.sqrt(3), 0, 0])),
        ("Above yield", np.array([yield_stress*1.2, 0, 0, 0, 0, 0]))
    ]

    print(f"{'Stress State':<20} {'Yield Function':<15} {'Status':<10}")
    print("-" * 50)

    for description, stress in stress_states:
        yield_value = yield_criterion.evaluate(stress)
        status = "Yielding" if yield_value > 0 else "Elastic"
        print(f"{description:<20} {yield_value/1e6:>11.2f} MPa  {status:<10}")


def main():
    """Main demonstration function"""
    print("DIGITAL MODEL STRESS ANALYSIS MODULE DEMONSTRATION")
    print("Migrated and modernized from legacy VMStressCalculations")
    print("=" * 80)

    try:
        # Demonstrate each major component
        demonstrate_vm_stress_analysis()
        demonstrate_stress_strain_analysis()
        demonstrate_nonlinear_analysis()

        print("\n" + "=" * 80)
        print("DEMONSTRATION COMPLETED SUCCESSFULLY!")
        print("=" * 80)

        print("\nKey Features Demonstrated:")
        print("✓ Von Mises stress analysis for pipes under combined loading")
        print("✓ Reproduction of legacy VMStressCalculations results")
        print("✓ Ramberg-Osgood stress-strain curve generation")
        print("✓ Nonlinear plasticity analysis with hardening")
        print("✓ Modern Python implementation with type hints and validation")
        print("✓ Comprehensive error handling and engineering standards")

        print("\nNext Steps:")
        print("- Run complete examples in examples/stress/ directory")
        print("- Explore test cases in tests/stress/ directory")
        print("- See README.md for detailed API documentation")
        print("- Integrate with your specific engineering applications")

    except Exception as e:
        print(f"\nError during demonstration: {e}")
        print("Please check that all modules are properly installed.")
        return False

    return True


if __name__ == "__main__":
    success = main()
    exit(0 if success else 1)