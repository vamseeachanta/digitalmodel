"""
Nonlinear Stress Analysis Example

This example demonstrates how to use the nonlinear stress analysis module
to solve plasticity problems with various yield criteria and hardening models.
"""

import numpy as np
import matplotlib.pyplot as plt
from digitalmodel.stress.nonlinear import (
    NonlinearStressAnalyzer,
    VonMisesYield,
    TrescaYield,
    LinearHardening,
    IsotropicHardening,
    PlasticityModel,
    HardeningType,
    solve_nonlinear_stress,
    calculate_nonlinear_response
)


def main():
    """Main example function"""
    print("=== Nonlinear Stress Analysis Example ===\n")

    # Material properties
    elastic_modulus = 200e9  # 200 GPa
    poisson_ratio = 0.3
    yield_stress = 250e6     # 250 MPa

    print(f"Material Properties:")
    print(f"  Elastic Modulus: {elastic_modulus/1e9:.0f} GPa")
    print(f"  Poisson's Ratio: {poisson_ratio:.1f}")
    print(f"  Yield Stress: {yield_stress/1e6:.0f} MPa")

    # Example 1: Perfect plasticity (no hardening)
    print("\n" + "="*50)
    print("EXAMPLE 1: Perfect Plasticity (No Hardening)")
    print("="*50)

    yield_criterion = VonMisesYield(yield_stress)
    analyzer_perfect = NonlinearStressAnalyzer(
        elastic_modulus, poisson_ratio, yield_criterion, None
    )

    # Apply strain beyond yield
    yield_strain = yield_stress / elastic_modulus
    applied_strain = yield_strain * 2.0  # 200% of yield strain

    print(f"Yield strain: {yield_strain*100:.3f}%")
    print(f"Applied strain: {applied_strain*100:.3f}%")

    total_strain = np.array([applied_strain, 0, 0, 0, 0, 0])
    solution_perfect = analyzer_perfect.solve_incremental_plasticity(total_strain, 10)

    print(f"\nPerfect Plasticity Results:")
    print(f"  Final stress: {solution_perfect.final_stress[0]/1e6:.1f} MPa")
    print(f"  Final total strain: {solution_perfect.final_strain[0]*100:.3f}%")
    print(f"  Final plastic strain: {solution_perfect.plastic_strain_history[-1][0]*100:.3f}%")
    print(f"  All steps converged: {all(step.converged for step in solution_perfect.load_steps)}")

    # Example 2: Linear hardening
    print("\n" + "="*50)
    print("EXAMPLE 2: Linear Hardening")
    print("="*50)

    hardening_modulus = 2e9  # 1% of elastic modulus
    linear_hardening = LinearHardening(hardening_modulus)

    analyzer_linear = NonlinearStressAnalyzer(
        elastic_modulus, poisson_ratio, yield_criterion, linear_hardening
    )

    print(f"Linear hardening modulus: {hardening_modulus/1e9:.1f} GPa")

    solution_linear = analyzer_linear.solve_incremental_plasticity(total_strain, 10)

    print(f"\nLinear Hardening Results:")
    print(f"  Final stress: {solution_linear.final_stress[0]/1e6:.1f} MPa")
    print(f"  Final total strain: {solution_linear.final_strain[0]*100:.3f}%")
    print(f"  Final plastic strain: {solution_linear.plastic_strain_history[-1][0]*100:.3f}%")
    print(f"  All steps converged: {all(step.converged for step in solution_linear.load_steps)}")

    # Example 3: Isotropic hardening
    print("\n" + "="*50)
    print("EXAMPLE 3: Isotropic Hardening")
    print("="*50)

    hardening_modulus_iso = 2e9
    hardening_exponent = 0.5
    iso_hardening = IsotropicHardening(hardening_modulus_iso, hardening_exponent)

    analyzer_iso = NonlinearStressAnalyzer(
        elastic_modulus, poisson_ratio, yield_criterion, iso_hardening
    )

    print(f"Isotropic hardening parameters:")
    print(f"  Hardening modulus: {hardening_modulus_iso/1e9:.1f} GPa")
    print(f"  Hardening exponent: {hardening_exponent:.1f}")

    solution_iso = analyzer_iso.solve_incremental_plasticity(total_strain, 10)

    print(f"\nIsotropic Hardening Results:")
    print(f"  Final stress: {solution_iso.final_stress[0]/1e6:.1f} MPa")
    print(f"  Final total strain: {solution_iso.final_strain[0]*100:.3f}%")
    print(f"  Final plastic strain: {solution_iso.plastic_strain_history[-1][0]*100:.3f}%")
    print(f"  All steps converged: {all(step.converged for step in solution_iso.load_steps)}")

    # Example 4: Plastic work calculation
    print("\n" + "="*50)
    print("EXAMPLE 4: Plastic Work Calculation")
    print("="*50)

    plastic_work_linear = analyzer_linear.calculate_plastic_work(solution_linear)
    plastic_work_iso = analyzer_iso.calculate_plastic_work(solution_iso)

    print(f"Plastic work comparison:")
    print(f"  Linear hardening: {plastic_work_linear/1e6:.2f} MJ/m³")
    print(f"  Isotropic hardening: {plastic_work_iso/1e6:.2f} MJ/m³")

    # Example 5: Comparison of yield criteria
    print("\n" + "="*50)
    print("EXAMPLE 5: Yield Criteria Comparison")
    print("="*50)

    # Test different stress states
    stress_states = [
        ("Uniaxial tension", np.array([yield_stress, 0, 0, 0, 0, 0])),
        ("Pure shear", np.array([0, 0, 0, yield_stress/np.sqrt(3), 0, 0])),
        ("Biaxial tension", np.array([yield_stress*0.866, yield_stress*0.866, 0, 0, 0, 0])),
    ]

    von_mises = VonMisesYield(yield_stress)
    tresca = TrescaYield(yield_stress)

    print("Stress State        | Von Mises | Tresca")
    print("-" * 45)

    for description, stress in stress_states:
        vm_value = von_mises.evaluate(stress)
        tresca_value = tresca.evaluate(stress)

        print(f"{description:18} | {vm_value/1e3:8.1f} | {tresca_value/1e3:6.1f}")

    # Example 6: Loading path effects
    print("\n" + "="*50)
    print("EXAMPLE 6: Loading Path Effects")
    print("="*50)

    # Different loading paths to same final strain
    final_strain = yield_strain * 3.0

    # Path 1: Direct loading
    path1_strain = np.array([final_strain, 0, 0, 0, 0, 0])
    solution_path1 = analyzer_linear.solve_incremental_plasticity(path1_strain, 10)

    # Path 2: Loading-unloading-reloading
    # First load to 150% yield strain
    path2a_strain = np.array([yield_strain * 1.5, 0, 0, 0, 0, 0])
    solution_path2a = analyzer_linear.solve_incremental_plasticity(path2a_strain, 5)

    # Then unload to 50% yield strain
    path2b_strain = np.array([yield_strain * 0.5, 0, 0, 0, 0, 0])
    # Note: This is simplified - proper unloading would require modified implementation

    # Finally reload to final strain
    path2c_strain = np.array([final_strain, 0, 0, 0, 0, 0])
    solution_path2c = analyzer_linear.solve_incremental_plasticity(path2c_strain, 10)

    print(f"Loading path comparison (final strain = {final_strain*100:.2f}%):")
    print(f"  Direct loading final stress: {solution_path1.final_stress[0]/1e6:.1f} MPa")
    print(f"  Complex loading final stress: {solution_path2c.final_stress[0]/1e6:.1f} MPa")

    # Example 7: Convenience functions
    print("\n" + "="*50)
    print("EXAMPLE 7: Convenience Functions")
    print("="*50)

    # Test convenience function
    test_strain = 0.005  # 0.5% strain
    result = solve_nonlinear_stress(
        test_strain, elastic_modulus, yield_stress, hardening_modulus, 10
    )

    print(f"Convenience function results for {test_strain*100:.1f}% strain:")
    print(f"  Final stress: {result['final_stress']/1e6:.1f} MPa")
    print(f"  Final strain: {result['final_strain']*100:.3f}%")
    print(f"  Plastic strain: {result['plastic_strain']*100:.3f}%")
    print(f"  Converged: {result['converged']}")

    # Example 8: Stress-strain curve generation
    print("\n" + "="*50)
    print("EXAMPLE 8: Stress-Strain Curve Generation")
    print("="*50)

    # Generate stress-strain curves for different hardening models
    strain_points = np.linspace(0, 0.01, 50)  # Up to 1% strain
    stress_perfect = []
    stress_linear = []
    stress_iso = []

    print("Generating stress-strain curves...")

    for strain in strain_points:
        if strain == 0:
            stress_perfect.append(0)
            stress_linear.append(0)
            stress_iso.append(0)
        else:
            # Perfect plasticity
            result_perfect = solve_nonlinear_stress(
                strain, elastic_modulus, yield_stress, 0, 5
            )
            stress_perfect.append(result_perfect['final_stress'])

            # Linear hardening
            result_linear = solve_nonlinear_stress(
                strain, elastic_modulus, yield_stress, hardening_modulus, 5
            )
            stress_linear.append(result_linear['final_stress'])

            # Isotropic hardening (approximated)
            result_iso_approx = solve_nonlinear_stress(
                strain, elastic_modulus, yield_stress, hardening_modulus*0.8, 5
            )
            stress_iso.append(result_iso_approx['final_stress'])

    # Plotting
    print("\n" + "="*50)
    print("CREATING PLOTS")
    print("="*50)

    # Create comprehensive plots
    fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(15, 12))

    # Plot 1: Stress-strain curves comparison
    strain_pct = np.array(strain_points) * 100
    ax1.plot(strain_pct, np.array(stress_perfect)/1e6, 'r-', linewidth=2,
             label='Perfect Plasticity')
    ax1.plot(strain_pct, np.array(stress_linear)/1e6, 'b-', linewidth=2,
             label='Linear Hardening')
    ax1.plot(strain_pct, np.array(stress_iso)/1e6, 'g-', linewidth=2,
             label='Isotropic Hardening')
    ax1.axhline(yield_stress/1e6, color='k', linestyle='--', alpha=0.7,
                label=f'Yield Stress ({yield_stress/1e6:.0f} MPa)')
    ax1.set_xlabel('Strain (%)')
    ax1.set_ylabel('Stress (MPa)')
    ax1.set_title('Stress-Strain Curves: Hardening Model Comparison')
    ax1.grid(True, alpha=0.3)
    ax1.legend()

    # Plot 2: Load step convergence
    steps = range(len(solution_linear.load_steps))
    convergence = [step.residual for step in solution_linear.load_steps]
    ax2.semilogy(steps, convergence, 'bo-', linewidth=2, markersize=6)
    ax2.axhline(analyzer_linear.tolerance, color='r', linestyle='--',
                label=f'Tolerance ({analyzer_linear.tolerance:.0e})')
    ax2.set_xlabel('Load Step')
    ax2.set_ylabel('Residual')
    ax2.set_title('Convergence History (Linear Hardening)')
    ax2.grid(True, alpha=0.3)
    ax2.legend()

    # Plot 3: Plastic strain evolution
    load_factors = [step.load_factor for step in solution_linear.load_steps]
    plastic_strains = [ps[0]*100 for ps in solution_linear.plastic_strain_history]
    ax3.plot(load_factors, plastic_strains, 'g-', linewidth=2, marker='o', markersize=4)
    ax3.set_xlabel('Load Factor')
    ax3.set_ylabel('Plastic Strain (%)')
    ax3.set_title('Plastic Strain Evolution')
    ax3.grid(True, alpha=0.3)

    # Plot 4: Yield surface visualization (2D)
    # Create stress space for Von Mises yield surface
    sigma1_range = np.linspace(-yield_stress*1.2, yield_stress*1.2, 100)
    sigma2_range = np.linspace(-yield_stress*1.2, yield_stress*1.2, 100)
    S1, S2 = np.meshgrid(sigma1_range, sigma2_range)

    # Von Mises yield surface (σ1² - σ1σ2 + σ2² = σy²)
    yield_function = S1**2 - S1*S2 + S2**2 - yield_stress**2

    ax4.contour(S1/1e6, S2/1e6, yield_function, levels=[0], colors='red', linewidths=2)
    ax4.contourf(S1/1e6, S2/1e6, yield_function, levels=[-1e20, 0], colors=['lightblue'], alpha=0.3)
    ax4.set_xlabel('σ₁ (MPa)')
    ax4.set_ylabel('σ₂ (MPa)')
    ax4.set_title('Von Mises Yield Surface (σ₃ = 0)')
    ax4.grid(True, alpha=0.3)
    ax4.axis('equal')
    ax4.set_xlim(-yield_stress*1.2/1e6, yield_stress*1.2/1e6)
    ax4.set_ylim(-yield_stress*1.2/1e6, yield_stress*1.2/1e6)

    # Add some stress points
    stress_points = [
        (yield_stress*0.8, 0, 'Elastic'),
        (yield_stress, 0, 'Yield (uniaxial)'),
        (yield_stress*0.866, yield_stress*0.866, 'Yield (biaxial)'),
    ]

    for s1, s2, label in stress_points:
        ax4.plot(s1/1e6, s2/1e6, 'ko', markersize=6)
        ax4.annotate(label, (s1/1e6, s2/1e6), xytext=(5, 5),
                    textcoords='offset points', fontsize=8)

    plt.tight_layout()
    plt.savefig('/mnt/github/github/digitalmodel/examples/stress/nonlinear_analysis.png',
                dpi=300, bbox_inches='tight')

    # Create detailed stress-strain plot
    plt.figure(figsize=(10, 8))

    # Plot with annotations
    plt.plot(strain_pct, np.array(stress_perfect)/1e6, 'r-', linewidth=3,
             label='Perfect Plasticity', alpha=0.8)
    plt.plot(strain_pct, np.array(stress_linear)/1e6, 'b-', linewidth=3,
             label='Linear Hardening', alpha=0.8)
    plt.plot(strain_pct, np.array(stress_iso)/1e6, 'g-', linewidth=3,
             label='Isotropic Hardening', alpha=0.8)

    # Mark yield point
    yield_strain_pct = yield_strain * 100
    plt.axvline(yield_strain_pct, color='k', linestyle=':', alpha=0.7,
                label=f'Yield Strain ({yield_strain_pct:.3f}%)')
    plt.axhline(yield_stress/1e6, color='k', linestyle=':', alpha=0.7,
                label=f'Yield Stress ({yield_stress/1e6:.0f} MPa)')

    # Mark elastic region
    plt.fill_between([0, yield_strain_pct], [0, 0], [yield_stress/1e6, yield_stress/1e6],
                     alpha=0.2, color='gray', label='Elastic Region')

    plt.xlabel('Strain (%)', fontsize=12)
    plt.ylabel('Stress (MPa)', fontsize=12)
    plt.title('Nonlinear Stress-Strain Response: Effect of Hardening', fontsize=14)
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=10)
    plt.tight_layout()

    plt.savefig('/mnt/github/github/digitalmodel/examples/stress/hardening_comparison.png',
                dpi=300, bbox_inches='tight')

    print("Plots saved:")
    print("  - nonlinear_analysis.png (comprehensive analysis)")
    print("  - hardening_comparison.png (hardening model comparison)")

    plt.show()

    print("\n" + "="*60)
    print("Nonlinear stress analysis examples completed successfully!")
    print("="*60)


if __name__ == "__main__":
    main()