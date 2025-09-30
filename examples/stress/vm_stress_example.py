"""
Von Mises Stress Analysis Example

This example demonstrates how to use the Von Mises stress analysis module
to analyze pipe stress under various loading conditions, reproducing and
extending the functionality of the legacy VMStressCalculations code.
"""

import numpy as np
import matplotlib.pyplot as plt
from digitalmodel.stress import (
    PipeStressAnalyzer,
    PipeGeometry,
    MaterialProperties,
    LoadingCondition,
    VonMisesStressCalculator,
    StressState
)


def main():
    """Main example function"""
    print("=== Von Mises Stress Analysis Example ===\n")

    # Define pipe geometry (based on legacy code values)
    geometry = PipeGeometry(
        outer_diameter=0.24765,  # m
        wall_thickness=0.034925  # m
    )

    print(f"Pipe Geometry:")
    print(f"  Outer Diameter: {geometry.outer_diameter:.5f} m")
    print(f"  Wall Thickness: {geometry.wall_thickness:.6f} m")
    print(f"  Inner Diameter: {geometry.inner_diameter:.6f} m")
    print(f"  Cross-sectional Area: {geometry.cross_sectional_area:.6e} m²")
    print(f"  Moment of Inertia: {geometry.moment_of_inertia:.6e} m⁴")

    # Define material properties (based on legacy code)
    material = MaterialProperties(
        yield_strength=5.52e8,   # Pa (552 MPa)
        ultimate_strength=6.20e8,  # Pa (620 MPa)
        elastic_modulus=2.1e11,  # Pa (210 GPa)
        poisson_ratio=0.3
    )

    print(f"\nMaterial Properties:")
    print(f"  Yield Strength: {material.yield_strength/1e6:.0f} MPa")
    print(f"  Ultimate Strength: {material.ultimate_strength/1e6:.0f} MPa")
    print(f"  Elastic Modulus: {material.elastic_modulus/1e9:.0f} GPa")

    # Create stress analyzer
    analyzer = PipeStressAnalyzer(geometry, material)

    print(f"\nDesign Parameters:")
    print(f"  Allowable Stress: {analyzer.calculate_allowable_stress()/1e6:.1f} MPa")

    # Example 1: Pressure vessel analysis
    print("\n" + "="*50)
    print("EXAMPLE 1: Pressure Vessel Analysis")
    print("="*50)

    internal_pressure = 10e6  # 10 MPa
    pressure_stresses = analyzer.calculate_pressure_stresses(internal_pressure)

    print(f"\nPressure Loading (Internal Pressure = {internal_pressure/1e6:.1f} MPa):")
    for stress_type, value in pressure_stresses.items():
        print(f"  {stress_type.replace('_', ' ').title()}: {value/1e6:.2f} MPa")

    # Example 2: Combined loading analysis
    print("\n" + "="*50)
    print("EXAMPLE 2: Combined Loading Analysis")
    print("="*50)

    loading_conditions = [
        LoadingCondition(internal_pressure=5e6, axial_force=1e6, bending_moment=1e5),
        LoadingCondition(internal_pressure=0, axial_force=0, bending_moment=46e4),
        LoadingCondition(internal_pressure=2e6, axial_force=5e5, bending_moment=2e5),
    ]

    for i, loading in enumerate(loading_conditions, 1):
        print(f"\nLoading Case {i}:")
        print(f"  Internal Pressure: {loading.internal_pressure/1e6:.1f} MPa")
        print(f"  Axial Force: {loading.axial_force/1e3:.1f} kN")
        print(f"  Bending Moment: {loading.bending_moment/1e3:.1f} kN⋅m")

        results = analyzer.calculate_combined_stress(loading)

        print(f"  Results:")
        print(f"    Radial Stress: {results['radial']/1e6:.2f} MPa")
        print(f"    Circumferential Stress: {results['circumferential']/1e6:.2f} MPa")
        print(f"    Axial Stress: {results['axial']/1e6:.2f} MPa")
        print(f"    Von Mises Stress: {results['von_mises']/1e6:.2f} MPa")
        print(f"    Safety Factor: {results['safety_factor']:.2f}")

    # Example 3: Interaction envelope (reproducing legacy plotting)
    print("\n" + "="*50)
    print("EXAMPLE 3: Tension-Moment Interaction Envelope")
    print("="*50)

    # Moment range (based on legacy code values)
    moment_range = [0, 1e3, 5e3, 10e3, 30e4, 36e4, 42e4, 46e4]  # N⋅m

    print("\nCalculating maximum allowable tensions:")
    for moment in moment_range:
        tension_pos, tension_neg = analyzer.calculate_maximum_tension(moment)
        print(f"  Moment: {moment/1e3:.0f} kN⋅m → "
              f"Tension: +{tension_pos/1e3:.0f} kN, {tension_neg/1e3:.0f} kN")

    # Generate full interaction envelope
    envelope = analyzer.generate_interaction_envelope(moment_range)

    # Plot the envelope
    plt.figure(figsize=(10, 8))
    plt.plot(envelope['moments'], envelope['tensions'], 'b-', linewidth=2,
             label='Interaction Envelope')

    # Plot positive and negative envelopes separately for clarity
    pos_env = envelope['positive_envelope']
    plt.plot(pos_env['moments'], pos_env['tensions'], 'r--', linewidth=1,
             label='Positive Envelope', alpha=0.7)
    plt.plot([-m for m in pos_env['moments']], [-t for t in pos_env['tensions']],
             'r--', linewidth=1, alpha=0.7)

    plt.xlabel('Bending Moment (kN⋅m)')
    plt.ylabel('Effective Tension (kN)')
    plt.title('Tension vs Bending Moment Interaction Envelope')
    plt.grid(True, alpha=0.3)
    plt.legend()
    plt.axis('equal')

    # Save plot
    plt.tight_layout()
    plt.savefig('/mnt/github/github/digitalmodel/examples/stress/tension_moment_envelope.png',
                dpi=300, bbox_inches='tight')
    print(f"\nInteraction envelope plot saved to: tension_moment_envelope.png")

    # Example 4: Direct Von Mises stress calculation
    print("\n" + "="*50)
    print("EXAMPLE 4: Direct Von Mises Stress Calculations")
    print("="*50)

    # Various stress states
    stress_states = [
        ("Uniaxial tension", StressState(300e6, 0, 0)),
        ("Biaxial tension", StressState(200e6, 100e6, 0)),
        ("Pure shear", StressState(0, 0, 0, 150e6)),
        ("Complex state", StressState(100e6, 50e6, 25e6, 30e6, 20e6, 10e6)),
    ]

    for description, stress_state in stress_states:
        vm_stress = VonMisesStressCalculator.calculate_from_stress_state(stress_state)
        print(f"\n{description}:")
        print(f"  σx = {stress_state.sigma_x/1e6:.1f} MPa")
        print(f"  σy = {stress_state.sigma_y/1e6:.1f} MPa")
        print(f"  σz = {stress_state.sigma_z/1e6:.1f} MPa")
        if any([stress_state.tau_xy, stress_state.tau_yz, stress_state.tau_zx]):
            print(f"  τxy = {stress_state.tau_xy/1e6:.1f} MPa")
            print(f"  τyz = {stress_state.tau_yz/1e6:.1f} MPa")
            print(f"  τzx = {stress_state.tau_zx/1e6:.1f} MPa")
        print(f"  Von Mises Stress: {vm_stress/1e6:.1f} MPa")

    # Example 5: Parametric study
    print("\n" + "="*50)
    print("EXAMPLE 5: Parametric Study - Effect of Pressure")
    print("="*50)

    pressures = np.linspace(0, 20e6, 11)  # 0 to 20 MPa
    vm_stresses = []
    safety_factors = []

    print("\nPressure vs Von Mises Stress:")
    print("Pressure (MPa) | VM Stress (MPa) | Safety Factor")
    print("-" * 50)

    for pressure in pressures:
        loading = LoadingCondition(internal_pressure=pressure)
        results = analyzer.calculate_combined_stress(loading)

        vm_stress = results['von_mises']
        safety_factor = results['safety_factor']

        vm_stresses.append(vm_stress)
        safety_factors.append(safety_factor)

        print(f"{pressure/1e6:8.1f}      | {vm_stress/1e6:10.1f}     | {safety_factor:8.2f}")

    # Plot parametric study
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8))

    # Von Mises stress vs pressure
    ax1.plot(pressures/1e6, np.array(vm_stresses)/1e6, 'b-', linewidth=2)
    ax1.axhline(material.yield_strength/1e6, color='r', linestyle='--',
                label=f'Yield Strength ({material.yield_strength/1e6:.0f} MPa)')
    ax1.axhline(analyzer.calculate_allowable_stress()/1e6, color='orange',
                linestyle='--', label=f'Allowable Stress ({analyzer.calculate_allowable_stress()/1e6:.0f} MPa)')
    ax1.set_xlabel('Internal Pressure (MPa)')
    ax1.set_ylabel('Von Mises Stress (MPa)')
    ax1.set_title('Von Mises Stress vs Internal Pressure')
    ax1.grid(True, alpha=0.3)
    ax1.legend()

    # Safety factor vs pressure
    ax2.plot(pressures/1e6, safety_factors, 'g-', linewidth=2)
    ax2.axhline(1.0, color='r', linestyle='--', label='Safety Factor = 1.0')
    ax2.set_xlabel('Internal Pressure (MPa)')
    ax2.set_ylabel('Safety Factor')
    ax2.set_title('Safety Factor vs Internal Pressure')
    ax2.grid(True, alpha=0.3)
    ax2.legend()
    ax2.set_ylim(0, max(safety_factors) * 1.1)

    plt.tight_layout()
    plt.savefig('/mnt/github/github/digitalmodel/examples/stress/pressure_study.png',
                dpi=300, bbox_inches='tight')
    print(f"\nParametric study plot saved to: pressure_study.png")

    plt.show()

    print("\n" + "="*60)
    print("Von Mises stress analysis examples completed successfully!")
    print("="*60)


if __name__ == "__main__":
    main()