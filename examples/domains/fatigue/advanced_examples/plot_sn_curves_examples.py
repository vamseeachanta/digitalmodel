"""
S-N Curve Plotting Examples

This script demonstrates various ways to plot and compare S-N curves
from the fatigue database, matching the reference Excel file capabilities.

Examples include:
1. Basic log-log plot
2. Linear-log plot
3. With/without stress concentration factors
4. With/without fatigue limits
5. Multi-curve comparison
"""

import sys
from pathlib import Path

# Add src to path for imports
src_path = Path(__file__).parent.parent.parent / 'src'
sys.path.insert(0, str(src_path))

from digitalmodel.structural.fatigue.sn_curve_plotter import SNCurvePlotter


def example_1_basic_plot():
    """Example 1: Basic log-log plot of a single curve."""
    print("\n" + "="*60)
    print("EXAMPLE 1: Basic Log-Log Plot")
    print("="*60)

    plotter = SNCurvePlotter()

    # Plot API-X' curve (lookup index 64)
    plotter.plot_curves(
        lookup_indices=64,
        scf=1.0,
        include_fatigue_limit=True,
        plot_type='log-log',
        title="API-X' Seawater CP - Basic Plot",
        save_path='outputs/example1_basic_plot.png',
        show_plot=False
    )
    print("✓ Example 1 complete: outputs/example1_basic_plot.png")


def example_2_linear_log():
    """Example 2: Linear-log plot for better stress range visualization."""
    print("\n" + "="*60)
    print("EXAMPLE 2: Linear-Log Plot")
    print("="*60)

    plotter = SNCurvePlotter()

    # Plot with linear stress axis
    plotter.plot_curves(
        lookup_indices=[63, 64],
        scf=1.0,
        include_fatigue_limit=True,
        plot_type='linear-log',
        title="API Curves Comparison - Linear-Log Scale",
        save_path='outputs/example2_linear_log.png',
        show_plot=False
    )
    print("✓ Example 2 complete: outputs/example2_linear_log.png")


def example_3_with_scf():
    """Example 3: Plot with stress concentration factor."""
    print("\n" + "="*60)
    print("EXAMPLE 3: Plot with SCF=2.0")
    print("="*60)

    plotter = SNCurvePlotter()

    # Plot with SCF = 2.0
    plotter.plot_curves(
        lookup_indices=[64, 175],
        scf=2.0,
        include_fatigue_limit=True,
        plot_type='log-log',
        title="S-N Curves with SCF=2.0",
        save_path='outputs/example3_with_scf.png',
        show_plot=False
    )
    print("✓ Example 3 complete: outputs/example3_with_scf.png")


def example_4_without_fatigue_limit():
    """Example 4: Plot without fatigue limit (continuous degradation)."""
    print("\n" + "="*60)
    print("EXAMPLE 4: Without Fatigue Limit")
    print("="*60)

    plotter = SNCurvePlotter()

    # Plot without fatigue limit
    plotter.plot_curves(
        lookup_indices=[64, 175],
        scf=1.0,
        include_fatigue_limit=False,
        plot_type='log-log',
        title="S-N Curves without Fatigue Limit",
        save_path='outputs/example4_no_limit.png',
        show_plot=False
    )
    print("✓ Example 4 complete: outputs/example4_no_limit.png")


def example_5_dnv_comparison():
    """Example 5: Compare multiple DNV curves."""
    print("\n" + "="*60)
    print("EXAMPLE 5: DNV Curves Comparison")
    print("="*60)

    plotter = SNCurvePlotter()

    # List DNV curves in air
    print("\nAvailable DNV curves in air:")
    dnv_curves = plotter.list_curves(curve_type_filter='DNV', environment_filter='Air')
    print(dnv_curves[['Lookup Index', 'Curve Type']].head(10))

    # Plot first 5 DNV curves
    dnv_indices = dnv_curves['Lookup Index'].head(5).tolist()
    plotter.plot_curves(
        lookup_indices=dnv_indices,
        scf=1.0,
        include_fatigue_limit=True,
        plot_type='log-log',
        title="DNV S-N Curves - In Air Environment",
        save_path='outputs/example5_dnv_comparison.png',
        show_plot=False
    )
    print("✓ Example 5 complete: outputs/example5_dnv_comparison.png")


def example_6_abs_comparison():
    """Example 6: Compare ABS curves across environments."""
    print("\n" + "="*60)
    print("EXAMPLE 6: ABS Curves Across Environments")
    print("="*60)

    plotter = SNCurvePlotter()

    # List all ABS curves
    print("\nAvailable ABS curves:")
    abs_curves = plotter.list_curves(curve_type_filter='ABS')
    print(abs_curves[['Lookup Index', 'Curve Type', 'In air (or) Corrosion Protected in seawater (or) Free Corrosion in seawater']])

    # Plot ABS C curves in different environments
    abs_c_air = abs_curves[
        (abs_curves['Curve Type'] == "ABS'20 C") &
        (abs_curves['In air (or) Corrosion Protected in seawater (or) Free Corrosion in seawater'] == 'In Air')
    ]['Lookup Index'].iloc[0]

    abs_c_seawater = abs_curves[
        (abs_curves['Curve Type'] == "ABS'20 C") &
        (abs_curves['In air (or) Corrosion Protected in seawater (or) Free Corrosion in seawater'] == 'Seawater CP')
    ]['Lookup Index'].iloc[0]

    abs_c_corrosion = abs_curves[
        (abs_curves['Curve Type'] == "ABS'20 C") &
        (abs_curves['In air (or) Corrosion Protected in seawater (or) Free Corrosion in seawater'] == 'Free Corrosion')
    ]['Lookup Index'].iloc[0]

    plotter.plot_curves(
        lookup_indices=[abs_c_air, abs_c_seawater, abs_c_corrosion],
        scf=1.0,
        include_fatigue_limit=True,
        plot_type='log-log',
        title="ABS'20 C Curve - Environmental Effects",
        save_path='outputs/example6_abs_environments.png',
        show_plot=False
    )
    print("✓ Example 6 complete: outputs/example6_abs_environments.png")


def example_7_reference_comparison():
    """Example 7: Comparison plot with reference curve highlighted."""
    print("\n" + "="*60)
    print("EXAMPLE 7: Reference Comparison Plot")
    print("="*60)

    plotter = SNCurvePlotter()

    # Use API-X' as reference, compare with others
    reference_idx = 64  # API-X' Seawater CP
    comparison_indices = [63, 175, 183, 190]  # API-X, BS'14 D, BV'20 C, ABS'20 C

    plotter.create_comparison_plot(
        reference_index=reference_idx,
        comparison_indices=comparison_indices,
        scf=1.0,
        include_fatigue_limit=True,
        plot_type='log-log',
        save_path='outputs/example7_reference_comparison.png'
    )
    print("✓ Example 7 complete: outputs/example7_reference_comparison.png")


def example_8_joint_type_comparison():
    """Example 8: Compare curves by joint type."""
    print("\n" + "="*60)
    print("EXAMPLE 8: Joint Type Comparison")
    print("="*60)

    plotter = SNCurvePlotter()

    # Get curves for different joint types
    print("\nJoint type distribution:")
    plated = plotter.list_curves(joint_type_filter='Plated')
    tubular = plotter.list_curves(joint_type_filter='Tubular')
    print(f"Plated welded joints: {len(plated)}")
    print(f"Tubular joints: {len(tubular)}")

    # Plot one curve from each joint type
    plated_idx = plated['Lookup Index'].iloc[0]
    tubular_idx = tubular[tubular['Joint Type'] == 'Tubular joint']['Lookup Index'].iloc[0]

    plotter.plot_curves(
        lookup_indices=[plated_idx, tubular_idx],
        scf=1.0,
        include_fatigue_limit=True,
        plot_type='log-log',
        title="Joint Type Comparison: Plated vs Tubular",
        save_path='outputs/example8_joint_types.png',
        show_plot=False
    )
    print("✓ Example 8 complete: outputs/example8_joint_types.png")


def main():
    """Run all examples."""
    print("\n" + "="*60)
    print("S-N CURVE PLOTTING EXAMPLES")
    print("="*60)
    print("\nThis script demonstrates various S-N curve plotting capabilities")
    print("matching the reference Excel file functionality.\n")

    # Create output directory
    import os
    os.makedirs('outputs', exist_ok=True)

    # Run examples
    try:
        example_1_basic_plot()
        example_2_linear_log()
        example_3_with_scf()
        example_4_without_fatigue_limit()
        example_5_dnv_comparison()
        example_6_abs_comparison()
        example_7_reference_comparison()
        example_8_joint_type_comparison()

        print("\n" + "="*60)
        print("ALL EXAMPLES COMPLETED SUCCESSFULLY")
        print("="*60)
        print("\nOutput files are in the 'outputs/' directory:")
        print("  - example1_basic_plot.png")
        print("  - example2_linear_log.png")
        print("  - example3_with_scf.png")
        print("  - example4_no_limit.png")
        print("  - example5_dnv_comparison.png")
        print("  - example6_abs_environments.png")
        print("  - example7_reference_comparison.png")
        print("  - example8_joint_types.png")

    except Exception as e:
        print(f"\n❌ Error running examples: {e}")
        import traceback
        traceback.print_exc()


if __name__ == '__main__':
    main()
