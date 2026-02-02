"""
OCIMF Environmental Loading - Demo Script

This script demonstrates all features of the OCIMF module and generates example charts.
"""

import sys
from pathlib import Path
import numpy as np

# Add src to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root / 'src'))

from digitalmodel.marine_engineering.environmental_loading import (
    OCIMFDatabase,
    EnvironmentalForces,
    EnvironmentalConditions,
    VesselGeometry,
    create_sample_database,
)


def main():
    """Run OCIMF demo with chart generation."""
    print("=" * 80)
    print("OCIMF ENVIRONMENTAL LOADING - DEMONSTRATION")
    print("=" * 80)

    # Create output directory
    output_dir = project_root / 'examples' / 'outputs' / 'ocimf_demo'
    output_dir.mkdir(parents=True, exist_ok=True)

    # 1. Create/load database
    print("\n1. Loading OCIMF Database...")
    db_path = project_root / 'data' / 'ocimf' / 'ocimf_coefficients_sample.csv'

    if not db_path.exists():
        print(f"   Creating sample database at {db_path}")
        create_sample_database(str(db_path), num_vessels=5, num_headings=13,
                              num_displacements=3)

    db = OCIMFDatabase(str(db_path))
    print(f"   [OK] Database loaded: {len(db.data)} entries")
    print(f"   [OK] Vessel types: {list(db.data['vessel_type'].unique())}")

    # 2. Demonstrate coefficient interpolation
    print("\n2. Coefficient Interpolation...")
    test_cases = [
        (0, 250000, "Head-on, VLCC"),
        (45, 275000, "Quartering, VLCC (interpolated)"),
        (90, 300000, "Beam, VLCC"),
    ]

    for heading, displacement, description in test_cases:
        coeffs = db.get_coefficients(heading, displacement)
        print(f"   {description}:")
        print(f"      CXw={coeffs.CXw:.3f}, CYw={coeffs.CYw:.3f}, CMw={coeffs.CMw:.3f}")

    # 3. Generate 3D surface plots
    print("\n3. Generating 3D Surface Plots...")
    for coef in ['CYw', 'CXw', 'CMw']:
        print(f"   - {coef} coefficient surface")
        db.plot_coefficient_surface(
            coef,
            save_path=str(output_dir / f'surface_{coef}.png'),
            interactive=False
        )
    print(f"   [OK] Saved to {output_dir}")

    # 4. Generate polar diagrams
    print("\n4. Generating Polar Diagrams...")
    for disp in [250000, 300000]:
        print(f"   - Polar diagram for {disp:,} tonnes")
        db.plot_polar_diagram(
            displacement=disp,
            save_path=str(output_dir / f'polar_{disp}.png')
        )
    print(f"   [OK] Saved to {output_dir}")

    # 4b. Generate multi-displacement comparison polar plot
    print("\n4b. Generating Multi-Displacement Comparison...")
    import matplotlib.pyplot as plt

    # Create comparison plot with multiple displacements
    displacements = [150000, 250000, 300000, 350000]  # Changed from 200000 to 150000 (Suezmax)
    colors = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728']

    fig, axes = plt.subplots(2, 2, figsize=(18, 16), subplot_kw=dict(projection='polar'))
    axes = axes.flatten()

    # Wind coefficients
    for idx, coef in enumerate(['CXw', 'CYw']):
        ax = axes[idx]

        for disp, color in zip(displacements, colors):
            # Get coefficient values at all headings
            headings = np.linspace(0, 360, 37)
            coefficients = []

            for heading in headings:
                coeffs = db.get_coefficients(heading, disp)
                coefficients.append(getattr(coeffs, coef))

            # Convert to radians for polar plot
            theta = np.radians(headings)

            # Plot with label
            ax.plot(theta, coefficients, linewidth=2.5, marker='o', markersize=5,
                   label=f'{disp/1000:.0f}t', color=color, alpha=0.8)

        ax.set_theta_zero_location('N')
        ax.set_theta_direction(-1)
        ax.set_title(f'{coef} - Wind Force Coefficients\nComparison by Displacement',
                    fontsize=13, fontweight='bold', pad=20)
        ax.legend(loc='upper right', bbox_to_anchor=(1.35, 1.1), fontsize=10)
        ax.grid(True, alpha=0.3, linestyle='--')
        ax.set_ylim(bottom=min(0, ax.get_ylim()[0]))

    # Current coefficients
    for idx, coef in enumerate(['CXc', 'CYc']):
        ax = axes[idx + 2]

        for disp, color in zip(displacements, colors):
            # Get coefficient values at all headings
            headings = np.linspace(0, 360, 37)
            coefficients = []

            for heading in headings:
                coeffs = db.get_coefficients(heading, disp)
                coefficients.append(getattr(coeffs, coef))

            # Convert to radians for polar plot
            theta = np.radians(headings)

            # Plot with label
            ax.plot(theta, coefficients, linewidth=2.5, marker='o', markersize=5,
                   label=f'{disp/1000:.0f}t', color=color, alpha=0.8)

        ax.set_theta_zero_location('N')
        ax.set_theta_direction(-1)
        ax.set_title(f'{coef} - Current Force Coefficients\nComparison by Displacement',
                    fontsize=13, fontweight='bold', pad=20)
        ax.legend(loc='upper right', bbox_to_anchor=(1.35, 1.1), fontsize=10)
        ax.grid(True, alpha=0.3, linestyle='--')
        ax.set_ylim(bottom=min(0, ax.get_ylim()[0]))

    plt.suptitle('OCIMF Force Coefficients - Multi-Displacement Comparison\n' +
                 f'Vessel Type: {db.data["vessel_type"].iloc[0]} | Wind Speed: 50 kt, Current: 3 kt',
                 fontsize=15, fontweight='bold', y=0.985)
    plt.tight_layout(rect=[0, 0, 1, 0.98])
    plt.savefig(output_dir / 'polar_comparison_multi_displacement.png', dpi=300, bbox_inches='tight')
    plt.close()
    print(f"   [OK] Multi-displacement comparison saved")

    # 5. Interpolation quality
    print("\n5. Generating Interpolation Quality Heatmap...")
    db.plot_interpolation_quality(
        coefficient_name='CYw',
        save_path=str(output_dir / 'interp_quality_CYw.png')
    )
    print(f"   [OK] Saved to {output_dir}")

    # 6. Force calculations
    print("\n6. Calculating Environmental Forces...")
    calc = EnvironmentalForces(db)

    conditions = EnvironmentalConditions(
        wind_speed=25.0,  # m/s (severe storm)
        wind_direction=60.0,  # degrees
        current_speed=2.0,  # m/s (strong current)
        current_direction=45.0  # degrees
    )

    geometry = VesselGeometry(loa=330.0, beam=60.0, draft=22.0)
    displacement = 300000

    results = calc.calculate_total_forces(conditions, geometry, displacement)

    print(f"\n   Environmental Conditions:")
    print(f"   - Wind: {conditions.wind_speed} m/s @ {conditions.wind_direction}°")
    print(f"   - Current: {conditions.current_speed} m/s @ {conditions.current_direction}°")
    print(f"\n   Calculated Forces (VLCC, {displacement:,}t):")
    print(f"   - Wind Fx:    {results.wind_fx/1e3:>8.1f} kN")
    print(f"   - Wind Fy:    {results.wind_fy/1e3:>8.1f} kN")
    print(f"   - Wind Mz:    {results.wind_mz/1e6:>8.1f} MN·m")
    print(f"   - Current Fx: {results.current_fx/1e3:>8.1f} kN")
    print(f"   - Current Fy: {results.current_fy/1e3:>8.1f} kN")
    print(f"   - Current Mz: {results.current_mz/1e6:>8.1f} MN·m")
    print(f"   - Total Fx:   {results.total_fx/1e3:>8.1f} kN")
    print(f"   - Total Fy:   {results.total_fy/1e3:>8.1f} kN")
    print(f"   - Total Mz:   {results.total_mz/1e6:>8.1f} MN·m")
    print(f"   - Resultant:  {np.sqrt(results.total_fx**2 + results.total_fy**2)/1e3:>8.1f} kN")

    # 7. Force diagram
    print("\n7. Generating Force Vector Diagram...")
    calc.plot_force_diagram(
        results,
        save_path=str(output_dir / 'force_diagram.png')
    )
    print(f"   [OK] Saved to {output_dir}")

    # 7b. Multi-displacement force diagram comparison
    print("\n7b. Generating Multi-Displacement Force Comparison...")
    import matplotlib.pyplot as plt
    from matplotlib.patches import FancyArrowPatch

    fig, axes = plt.subplots(2, 2, figsize=(18, 16))
    axes = axes.flatten()

    # Test multiple displacements
    test_displacements = [150000, 250000, 300000, 350000]  # Changed from 200000 to 150000 (Suezmax)
    colors = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728']

    # Calculate forces for all displacements
    all_results = []
    for disp in test_displacements:
        res = calc.calculate_total_forces(conditions, geometry, disp)
        all_results.append(res)

    # Plot 1: Wind Forces
    ax = axes[0]
    for disp, res, color in zip(test_displacements, all_results, colors):
        # Plot wind force vector
        ax.arrow(0, 0, res.wind_fx/1e3, res.wind_fy/1e3,
                head_width=50, head_length=100, fc=color, ec=color,
                label=f'{disp/1000:.0f}t', linewidth=2, alpha=0.7)

    ax.set_xlabel('Fx - Longitudinal Force (kN)', fontsize=11, fontweight='bold')
    ax.set_ylabel('Fy - Lateral Force (kN)', fontsize=11, fontweight='bold')
    ax.set_title('Wind Forces vs Displacement\n' +
                 f'Wind: {conditions.wind_speed} m/s @ {conditions.wind_direction}°',
                 fontsize=12, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=10)
    ax.axhline(y=0, color='k', linewidth=0.5)
    ax.axvline(x=0, color='k', linewidth=0.5)
    ax.set_aspect('equal')

    # Plot 2: Current Forces
    ax = axes[1]
    for disp, res, color in zip(test_displacements, all_results, colors):
        # Plot current force vector
        ax.arrow(0, 0, res.current_fx/1e3, res.current_fy/1e3,
                head_width=200, head_length=400, fc=color, ec=color,
                label=f'{disp/1000:.0f}t', linewidth=2, alpha=0.7)

    ax.set_xlabel('Fx - Longitudinal Force (kN)', fontsize=11, fontweight='bold')
    ax.set_ylabel('Fy - Lateral Force (kN)', fontsize=11, fontweight='bold')
    ax.set_title('Current Forces vs Displacement\n' +
                 f'Current: {conditions.current_speed} m/s @ {conditions.current_direction}°',
                 fontsize=12, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=10)
    ax.axhline(y=0, color='k', linewidth=0.5)
    ax.axvline(x=0, color='k', linewidth=0.5)
    ax.set_aspect('equal')

    # Plot 3: Total Forces
    ax = axes[2]
    for disp, res, color in zip(test_displacements, all_results, colors):
        # Plot total force vector
        ax.arrow(0, 0, res.total_fx/1e3, res.total_fy/1e3,
                head_width=200, head_length=400, fc=color, ec=color,
                label=f'{disp/1000:.0f}t', linewidth=2, alpha=0.7)

    ax.set_xlabel('Fx - Longitudinal Force (kN)', fontsize=11, fontweight='bold')
    ax.set_ylabel('Fy - Lateral Force (kN)', fontsize=11, fontweight='bold')
    ax.set_title('Total Environmental Forces vs Displacement',
                 fontsize=12, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=10)
    ax.axhline(y=0, color='k', linewidth=0.5)
    ax.axvline(x=0, color='k', linewidth=0.5)
    ax.set_aspect('equal')

    # Plot 4: Force Magnitude Comparison
    ax = axes[3]
    wind_mags = [np.sqrt(r.wind_fx**2 + r.wind_fy**2)/1e3 for r in all_results]
    current_mags = [np.sqrt(r.current_fx**2 + r.current_fy**2)/1e3 for r in all_results]
    total_mags = [np.sqrt(r.total_fx**2 + r.total_fy**2)/1e3 for r in all_results]

    x = np.arange(len(test_displacements))
    width = 0.25

    ax.bar(x - width, wind_mags, width, label='Wind', color='#1f77b4', alpha=0.8)
    ax.bar(x, current_mags, width, label='Current', color='#ff7f0e', alpha=0.8)
    ax.bar(x + width, total_mags, width, label='Total', color='#2ca02c', alpha=0.8)

    ax.set_xlabel('Vessel Displacement', fontsize=11, fontweight='bold')
    ax.set_ylabel('Resultant Force (kN)', fontsize=11, fontweight='bold')
    ax.set_title('Force Magnitudes vs Displacement', fontsize=12, fontweight='bold')
    ax.set_xticks(x)
    ax.set_xticklabels([f'{d/1000:.0f}t' for d in test_displacements])
    ax.legend(fontsize=10)
    ax.grid(True, alpha=0.3, axis='y')

    plt.suptitle('Environmental Forces - Multi-Displacement Comparison\n' +
                 f'Conditions: Wind {conditions.wind_speed} m/s @ {conditions.wind_direction}°, ' +
                 f'Current {conditions.current_speed} m/s @ {conditions.current_direction}°',
                 fontsize=15, fontweight='bold', y=0.995)
    plt.tight_layout(rect=[0, 0, 1, 0.99])
    plt.savefig(output_dir / 'force_diagram_multi_displacement.png', dpi=300, bbox_inches='tight')
    plt.close()
    print(f"   [OK] Multi-displacement force comparison saved")

    # 8. Validation chart
    print("\n8. Generating Validation Chart...")
    # Simulate Excel results with small differences
    excel_results = {
        'wind_fx': results.wind_fx * 1.02,
        'wind_fy': results.wind_fy * 0.98,
        'wind_mz': results.wind_mz * 1.01,
        'current_fx': results.current_fx * 1.03,
        'current_fy': results.current_fy * 0.97,
        'current_mz': results.current_mz * 1.02
    }

    calc.plot_validation_chart(
        excel_results,
        results,
        save_path=str(output_dir / 'validation_chart.png')
    )
    print(f"   [OK] Saved to {output_dir}")

    # Summary
    print("\n" + "=" * 80)
    print("DEMO COMPLETED SUCCESSFULLY")
    print("=" * 80)
    print(f"\nAll charts saved to: {output_dir}")
    print("\nGenerated files:")
    for file in sorted(output_dir.glob('*.png')):
        size_kb = file.stat().st_size / 1024
        print(f"  - {file.name:<35} ({size_kb:>6.1f} KB)")

    print("\n" + "=" * 80)
    print("Next steps:")
    print("  1. Run the Jupyter notebook: examples/ocimf_visualization_example.ipynb")
    print("  2. Run tests: pytest tests/marine_engineering/environmental_loading/")
    print("  3. Review generated charts in: examples/outputs/ocimf_demo/")
    print("=" * 80)


if __name__ == '__main__':
    main()
