"""
Stress-Strain Analysis Example

This example demonstrates how to use the stress-strain analysis module
to create and analyze stress-strain curves using various material models,
reproducing and extending the legacy stress-strain calculation functionality.
"""

import numpy as np
import matplotlib.pyplot as plt
from digitalmodel.structural.stress import (
    StressStrainAnalyzer,
    MaterialModel,
    StressStrainCurve,
    MaterialProperties,
    calculate_stress_strain_curve,
    fit_stress_strain_data
)


def main():
    """Main example function"""
    print("=== Stress-Strain Analysis Example ===\n")

    # Create analyzer
    analyzer = StressStrainAnalyzer()

    # Example 1: Linear elastic model
    print("="*50)
    print("EXAMPLE 1: Linear Elastic Model")
    print("="*50)

    elastic_modulus = 200e9  # 200 GPa
    strain_range = np.linspace(0, 0.002, 100)  # Up to 0.2% strain

    linear_curve = analyzer.generate_curve(
        MaterialModel.LINEAR_ELASTIC,
        strain_range,
        elastic_modulus=elastic_modulus
    )

    print(f"Linear Elastic Parameters:")
    print(f"  Elastic Modulus: {elastic_modulus/1e9:.0f} GPa")
    print(f"  Strain Range: 0 to {max(strain_range)*100:.2f}%")
    print(f"  Max Stress: {max(linear_curve.stresses)/1e6:.0f} MPa")

    # Example 2: Ramberg-Osgood model (reproducing legacy calculations)
    print("\n" + "="*50)
    print("EXAMPLE 2: Ramberg-Osgood Model (Legacy Parameters)")
    print("="*50)

    # Parameters from legacy code
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

    # Generate curve with extended strain range
    strain_range_ro = np.linspace(0, 0.1, 200)  # Up to 10% strain

    ramberg_osgood_curve = analyzer.generate_curve(
        MaterialModel.RAMBERG_OSGOOD,
        strain_range_ro,
        **legacy_params
    )

    print(f"\nRamberg-Osgood Curve Results:")
    print(f"  Number of points: {len(ramberg_osgood_curve.strains)}")
    print(f"  Max stress: {max(ramberg_osgood_curve.stresses)/1e3:.0f} kPa")
    print(f"  Max strain: {max(ramberg_osgood_curve.strains)*100:.1f}%")

    # Calculate some specific points as in legacy code
    test_stresses = [100e3, 200e3, 400e3, 600e3, 800e3]  # kPa (from legacy)

    print(f"\nSpecific Stress-Strain Points (reproducing legacy calculations):")
    print("Stress (kPa) | Strain (%)")
    print("-" * 25)

    from digitalmodel.structural.stress.stress_strain import RambergOsgoodModel
    ro_model = RambergOsgoodModel()

    for stress in test_stresses:
        strain = ro_model.calculate_strain(stress, **legacy_params)
        print(f"{stress/1e3:8.0f}     | {strain*100:.4f}")

    # Example 3: Bilinear model
    print("\n" + "="*50)
    print("EXAMPLE 3: Bilinear Elastic-Plastic Model")
    print("="*50)

    bilinear_params = {
        'elastic_modulus': 200e9,
        'yield_strength': 250e6,
        'hardening_modulus': 2e9  # 1% of elastic modulus
    }

    print(f"Bilinear Parameters:")
    print(f"  Elastic Modulus: {bilinear_params['elastic_modulus']/1e9:.0f} GPa")
    print(f"  Yield Strength: {bilinear_params['yield_strength']/1e6:.0f} MPa")
    print(f"  Hardening Modulus: {bilinear_params['hardening_modulus']/1e9:.1f} GPa")

    strain_range_bilinear = np.linspace(0, 0.01, 200)  # Up to 1% strain

    bilinear_curve = analyzer.generate_curve(
        MaterialModel.BILINEAR,
        strain_range_bilinear,
        **bilinear_params
    )

    yield_strain = bilinear_params['yield_strength'] / bilinear_params['elastic_modulus']
    print(f"\nYield strain: {yield_strain*100:.3f}%")

    # Example 4: Engineering properties calculation
    print("\n" + "="*50)
    print("EXAMPLE 4: Engineering Properties Calculation")
    print("="*50)

    properties = analyzer.calculate_engineering_properties(ramberg_osgood_curve)

    print("Engineering Properties from Ramberg-Osgood Curve:")
    for prop_name, value in properties.items():
        if value is not None:
            if 'modulus' in prop_name or 'strength' in prop_name or 'limit' in prop_name:
                if value > 1e6:
                    print(f"  {prop_name.replace('_', ' ').title()}: {value/1e6:.1f} MPa")
                else:
                    print(f"  {prop_name.replace('_', ' ').title()}: {value/1e3:.1f} kPa")
            else:
                print(f"  {prop_name.replace('_', ' ').title()}: {value:.4f}")
        else:
            print(f"  {prop_name.replace('_', ' ').title()}: Not available")

    # Example 5: Model comparison
    print("\n" + "="*50)
    print("EXAMPLE 5: Material Model Comparison")
    print("="*50)

    # Create synthetic experimental data
    strain_exp = np.linspace(0, 0.005, 50)
    stress_exp = []

    # Generate "experimental" data using Ramberg-Osgood
    for strain in strain_exp:
        if strain == 0:
            stress_exp.append(0)
        else:
            stress = ro_model.calculate_stress(
                strain, **legacy_params
            )
            stress_exp.append(stress)

    experimental_curve = StressStrainCurve(
        strains=strain_exp.tolist(),
        stresses=stress_exp
    )

    # Compare models
    comparison_results = analyzer.compare_models(experimental_curve)

    print("Model Comparison Results:")
    print("Model                | R-squared | RMSE (MPa)")
    print("-" * 50)

    for model_name, results in comparison_results.items():
        if 'error' not in results:
            r_squared = results['r_squared']
            rmse = results['rmse'] / 1e6  # Convert to MPa
            print(f"{model_name:20} | {r_squared:8.4f} | {rmse:8.2f}")
        else:
            print(f"{model_name:20} | Error: {results['error']}")

    # Plotting
    print("\n" + "="*50)
    print("CREATING PLOTS")
    print("="*50)

    # Create comprehensive plot
    fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(15, 12))

    # Plot 1: Linear elastic
    ax1.plot(np.array(linear_curve.strains)*100,
             np.array(linear_curve.stresses)/1e6, 'b-', linewidth=2,
             label='Linear Elastic')
    ax1.set_xlabel('Strain (%)')
    ax1.set_ylabel('Stress (MPa)')
    ax1.set_title('Linear Elastic Model')
    ax1.grid(True, alpha=0.3)
    ax1.legend()

    # Plot 2: Ramberg-Osgood (reproducing legacy plot)
    ax2.plot(np.array(ramberg_osgood_curve.strains)*100,
             np.array(ramberg_osgood_curve.stresses)/1e3, 'r-', linewidth=2,
             label='Ramberg-Osgood (Legacy)')
    ax2.set_xlabel('Strain (%)')
    ax2.set_ylabel('Stress (kPa)')
    ax2.set_title('Non-Linear Stress Strain Curve (Legacy Reproduction)')
    ax2.grid(True, alpha=0.3)
    ax2.legend()

    # Plot 3: Bilinear model
    ax3.plot(np.array(bilinear_curve.strains)*100,
             np.array(bilinear_curve.stresses)/1e6, 'g-', linewidth=2,
             label='Bilinear')
    # Mark yield point
    yield_strain_pct = yield_strain * 100
    yield_stress_mpa = bilinear_params['yield_strength'] / 1e6
    ax3.plot(yield_strain_pct, yield_stress_mpa, 'ro', markersize=8,
             label=f'Yield Point ({yield_strain_pct:.2f}%, {yield_stress_mpa:.0f} MPa)')
    ax3.set_xlabel('Strain (%)')
    ax3.set_ylabel('Stress (MPa)')
    ax3.set_title('Bilinear Elastic-Plastic Model')
    ax3.grid(True, alpha=0.3)
    ax3.legend()

    # Plot 4: Model comparison
    strain_pct = np.array(strain_exp) * 100
    stress_mpa = np.array(stress_exp) / 1e6

    ax4.plot(strain_pct, stress_mpa, 'ko', markersize=4,
             label='Experimental Data', alpha=0.7)

    for model_name, results in comparison_results.items():
        if 'error' not in results:
            predicted_curve = results['predicted_curve']
            strain_pred = np.array(predicted_curve.strains) * 100
            stress_pred = np.array(predicted_curve.stresses) / 1e6
            ax4.plot(strain_pred, stress_pred, linewidth=2,
                     label=f'{model_name} (R²={results["r_squared"]:.3f})')

    ax4.set_xlabel('Strain (%)')
    ax4.set_ylabel('Stress (MPa)')
    ax4.set_title('Model Comparison')
    ax4.grid(True, alpha=0.3)
    ax4.legend()

    plt.tight_layout()
    plt.savefig('/mnt/github/github/digitalmodel/examples/stress/stress_strain_analysis.png',
                dpi=300, bbox_inches='tight')

    # Create legacy-style plot (similar to original output)
    plt.figure(figsize=(10, 8))
    plt.plot(np.array(ramberg_osgood_curve.strains)*100,
             np.array(ramberg_osgood_curve.stresses)/1e3, 'b-', linewidth=2)
    plt.xlabel('Strain (%)', fontsize=15)
    plt.ylabel('Stress (kPa)', fontsize=15)
    plt.title('Non-Linear Stress Strain Curve')
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    plt.savefig('/mnt/github/github/digitalmodel/examples/stress/legacy_style_curve.png',
                dpi=300, bbox_inches='tight')

    print("Plots saved:")
    print("  - stress_strain_analysis.png (comprehensive analysis)")
    print("  - legacy_style_curve.png (legacy-style reproduction)")

    # Example 6: Advanced curve fitting
    print("\n" + "="*50)
    print("EXAMPLE 6: Advanced Curve Fitting")
    print("="*50)

    # Add some noise to experimental data to make it more realistic
    np.random.seed(42)  # For reproducible results
    noise_level = 0.05  # 5% noise
    noisy_stress = np.array(stress_exp) * (1 + noise_level * np.random.randn(len(stress_exp)))

    noisy_curve = StressStrainCurve(
        strains=strain_exp.tolist(),
        stresses=noisy_stress.tolist()
    )

    print("Fitting Ramberg-Osgood parameters to noisy experimental data...")
    fitted_params = analyzer.fit_ramberg_osgood(noisy_curve)

    print(f"\nOriginal vs Fitted Parameters:")
    print("Parameter        | Original    | Fitted      | Error (%)")
    print("-" * 60)
    for param_name in ['elastic_modulus', 'yield_strength', 'k', 'n']:
        original = legacy_params[param_name]
        fitted = fitted_params[param_name]
        error = abs(fitted - original) / original * 100
        print(f"{param_name:15} | {original:10.2e} | {fitted:10.2e} | {error:7.1f}")

    plt.show()

    print("\n" + "="*60)
    print("Stress-strain analysis examples completed successfully!")
    print("="*60)


if __name__ == "__main__":
    main()