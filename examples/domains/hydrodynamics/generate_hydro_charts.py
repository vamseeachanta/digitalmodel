"""
Generate Example Charts for Hydrodynamic Coefficients Module

This script creates 10+ comprehensive example charts demonstrating all
visualization capabilities of the hydrodynamic coefficients module.
"""

import numpy as np
from pathlib import Path
import sys

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.marine_ops.marine_engineering.hydrodynamic_coefficients import (
    CoefficientDatabase,
    HydrodynamicPlotter,
    DOF_NAMES
)


def main():
    """Generate all example charts."""

    # Setup paths
    data_dir = Path(__file__).parent.parent / 'data' / 'marine_engineering' / 'hydrodynamic'
    output_dir = Path(__file__).parent.parent / 'outputs' / 'hydrodynamic_charts'
    output_dir.mkdir(parents=True, exist_ok=True)

    print("=" * 70)
    print("HYDRODYNAMIC COEFFICIENTS - CHART GENERATION")
    print("=" * 70)

    # Load database
    print(f"\n1. Loading coefficient database from: {data_dir}")
    db = CoefficientDatabase.from_csv(data_dir)
    print(f"   + Loaded {len(db.frequencies)} frequencies")
    print(f"   + Frequency range: {db.get_frequency_range()} rad/s")

    # Create plotter
    plotter = HydrodynamicPlotter(db)

    # Chart 1: Heave Frequency Response
    print("\n2. Generating Chart 1: Heave Frequency Response (Both)")
    plotter.plot_frequency_response(
        dof='Heave',
        coefficient_type='both',
        coupled=False,
        save_path=output_dir / '01_heave_frequency_response.png',
        figsize=(14, 6)
    )
    print("   + Saved: 01_heave_frequency_response.png")

    # Chart 2: Pitch Frequency Response with Coupling
    print("\n3. Generating Chart 2: Pitch Frequency Response (Coupled)")
    plotter.plot_frequency_response(
        dof='Pitch',
        coefficient_type='both',
        coupled=True,
        save_path=output_dir / '02_pitch_coupled_response.png',
        figsize=(16, 7)
    )
    print("   + Saved: 02_pitch_coupled_response.png")

    # Chart 3: Added Mass Matrix Heatmap
    print("\n4. Generating Chart 3: Added Mass Matrix Heatmap")
    plotter.plot_coefficient_matrix(
        frequency=0.5,
        coefficient_type='added_mass',
        save_path=output_dir / '03_added_mass_matrix_heatmap.png'
    )
    print("   + Saved: 03_added_mass_matrix_heatmap.png")

    # Chart 4: Damping Matrix Heatmap
    print("\n5. Generating Chart 4: Damping Matrix Heatmap")
    plotter.plot_coefficient_matrix(
        frequency=0.5,
        coefficient_type='damping',
        save_path=output_dir / '04_damping_matrix_heatmap.png'
    )
    print("   + Saved: 04_damping_matrix_heatmap.png")

    # Chart 5: Heave Added Mass 3D Surface
    print("\n6. Generating Chart 5: Heave Added Mass 3D Surface")
    plotter.plot_added_mass_surface(
        dof_i='Heave',
        dof_j='Heave',
        save_path=output_dir / '05_heave_added_mass_surface.png'
    )
    print("   + Saved: 05_heave_added_mass_surface.png")

    # Chart 6: Heave Damping 3D Surface
    print("\n7. Generating Chart 6: Heave Damping 3D Surface")
    plotter.plot_damping_surface(
        dof_i='Heave',
        dof_j='Heave',
        save_path=output_dir / '06_heave_damping_surface.png'
    )
    print("   + Saved: 06_heave_damping_surface.png")

    # Chart 7: Added Mass Cross-Coupling
    print("\n8. Generating Chart 7: Added Mass Cross-Coupling")
    plotter.plot_cross_coupling(
        coefficient_type='added_mass',
        threshold=50.0,
        save_path=output_dir / '07_added_mass_cross_coupling.png'
    )
    print("   + Saved: 07_added_mass_cross_coupling.png")

    # Chart 8: Damping Cross-Coupling
    print("\n9. Generating Chart 8: Damping Cross-Coupling")
    plotter.plot_cross_coupling(
        coefficient_type='damping',
        threshold=0.1,
        save_path=output_dir / '08_damping_cross_coupling.png'
    )
    print("   + Saved: 08_damping_cross_coupling.png")

    # Chart 9: Critical Damping Ratios
    print("\n10. Generating Chart 9: Critical Damping Ratios")
    mass_matrix = np.diag([10000, 10000, 10000, 1000, 1000, 1000])
    stiffness_matrix = np.diag([50000, 50000, 50000, 5000, 5000, 5000])
    plotter.plot_critical_damping(
        mass_matrix=mass_matrix,
        stiffness_matrix=stiffness_matrix,
        save_path=output_dir / '09_critical_damping_ratios.png'
    )
    print("   + Saved: 09_critical_damping_ratios.png")

    # Chart 10: Added Mass Correlation Matrix
    print("\n11. Generating Chart 10: Added Mass Correlation Matrix")
    plotter.plot_matrix_correlation(
        coefficient_type='added_mass',
        save_path=output_dir / '10_added_mass_correlation.png'
    )
    print("   + Saved: 10_added_mass_correlation.png")

    # Chart 11: All DOFs Added Mass Comparison
    print("\n12. Generating Chart 11: All DOFs Added Mass Comparison")
    plotter.plot_all_dofs_comparison(
        coefficient_type='added_mass',
        save_path=output_dir / '11_all_dofs_added_mass.png'
    )
    print("   + Saved: 11_all_dofs_added_mass.png")

    # Chart 12: All DOFs Damping Comparison
    print("\n13. Generating Chart 12: All DOFs Damping Comparison")
    plotter.plot_all_dofs_comparison(
        coefficient_type='damping',
        save_path=output_dir / '12_all_dofs_damping.png'
    )
    print("   + Saved: 12_all_dofs_damping.png")

    # Chart 13: Surge-Pitch Coupling Surface
    print("\n14. Generating Chart 13: Surge-Pitch Added Mass Surface")
    plotter.plot_added_mass_surface(
        dof_i='Surge',
        dof_j='Pitch',
        save_path=output_dir / '13_surge_pitch_coupling_surface.png'
    )
    print("   + Saved: 13_surge_pitch_coupling_surface.png")

    # Chart 14: Animated Frequency Sweep
    print("\n15. Generating Chart 14: Animated Frequency Sweep (GIF)")
    plotter.animate_frequency_sweep(
        coefficient_type='added_mass',
        save_path=output_dir / '14_added_mass_animation.gif',
        fps=5
    )
    print("   + Saved: 14_added_mass_animation.gif")

    # Generate summary report
    print("\n" + "=" * 70)
    print("CHART GENERATION COMPLETE")
    print("=" * 70)
    print(f"\nTotal charts generated: 14")
    print(f"Output directory: {output_dir}")
    print("\nGenerated charts:")

    chart_list = [
        "01. Heave Frequency Response (Both)",
        "02. Pitch Frequency Response (Coupled)",
        "03. Added Mass Matrix Heatmap",
        "04. Damping Matrix Heatmap",
        "05. Heave Added Mass 3D Surface",
        "06. Heave Damping 3D Surface",
        "07. Added Mass Cross-Coupling",
        "08. Damping Cross-Coupling",
        "09. Critical Damping Ratios",
        "10. Added Mass Correlation Matrix",
        "11. All DOFs Added Mass Comparison",
        "12. All DOFs Damping Comparison",
        "13. Surge-Pitch Coupling Surface",
        "14. Animated Frequency Sweep (GIF)"
    ]

    for chart in chart_list:
        print(f"  + {chart}")

    print("\n" + "=" * 70)

    # Generate statistics report
    print("\nGenerating statistics report...")
    stats_am = db.get_coefficient_statistics('added_mass')
    stats_damp = db.get_coefficient_statistics('damping')

    import pandas as pd
    with pd.ExcelWriter(output_dir / 'coefficient_statistics.xlsx') as writer:
        stats_am.to_excel(writer, sheet_name='Added Mass', index=False)
        stats_damp.to_excel(writer, sheet_name='Damping', index=False)

    print(f"   + Saved: coefficient_statistics.xlsx")

    # Generate README
    readme_content = f"""# Hydrodynamic Coefficients - Example Charts

This directory contains example charts generated by the hydrodynamic coefficients module.

## Database Information
- Number of frequencies: {len(db.frequencies)}
- Frequency range: {db.get_frequency_range()} rad/s
- DOFs: {', '.join(DOF_NAMES)}

## Generated Charts

1. **01_heave_frequency_response.png** - Frequency response for Heave DOF (added mass & damping)
2. **02_pitch_coupled_response.png** - Pitch frequency response with all coupling terms
3. **03_added_mass_matrix_heatmap.png** - 6×6 added mass matrix heatmap at ω=0.5 rad/s
4. **04_damping_matrix_heatmap.png** - 6×6 damping matrix heatmap at ω=0.5 rad/s
5. **05_heave_added_mass_surface.png** - 3D surface plot for Heave added mass vs frequency
6. **06_heave_damping_surface.png** - 3D surface plot for Heave damping vs frequency
7. **07_added_mass_cross_coupling.png** - Off-diagonal coupling terms for added mass
8. **08_damping_cross_coupling.png** - Off-diagonal coupling terms for damping
9. **09_critical_damping_ratios.png** - Critical damping ratios for all DOFs
10. **10_added_mass_correlation.png** - Coefficient correlation matrix for added mass
11. **11_all_dofs_added_mass.png** - Comparison of all DOFs for added mass
12. **12_all_dofs_damping.png** - Comparison of all DOFs for damping
13. **13_surge_pitch_coupling_surface.png** - Surge-Pitch coupling added mass surface
14. **14_added_mass_animation.gif** - Animated frequency sweep of added mass matrix

## Statistics
Statistical summaries are available in: `coefficient_statistics.xlsx`

Generated using marine_engineering.hydrodynamic_coefficients module.
"""

    (output_dir / 'README.md').write_text(readme_content)
    print(f"   + Saved: README.md")

    print("\n+ All tasks completed successfully!")
    print("=" * 70 + "\n")


if __name__ == '__main__':
    main()
