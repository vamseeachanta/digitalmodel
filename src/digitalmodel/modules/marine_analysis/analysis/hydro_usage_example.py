"""
Example usage of hydrodynamic coefficient extractor.

This script demonstrates various ways to use the HydrodynamicCoefficientExtractor
class for analyzing marine engineering data.

Author: Digital Model Team
Date: 2025-10-03
"""

import sys
import os

# Set UTF-8 encoding for Windows
if sys.platform == 'win32':
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

# Add scripts to path
sys.path.insert(0, os.path.dirname(__file__))

from digitalmodel.modules.marine_analysis.extraction.extract_hydro import HydrodynamicCoefficientExtractor
import numpy as np


def example_1_basic_extraction():
    """Example 1: Basic extraction with default settings."""
    print("\n" + "=" * 80)
    print("EXAMPLE 1: Basic Extraction")
    print("=" * 80)

    extractor = HydrodynamicCoefficientExtractor(
        excel_path='your_file.xlsx',
        output_dir='data/marine_engineering/hydrodynamic'
    )

    # Extract from Excel (will use sample data if file not found)
    extractor.extract_from_excel(sheet_name='Damping')

    # Save to CSV
    extractor.save_data_to_csv()

    print(f"\nExtracted {len(extractor.frequencies)} frequency points")
    print(f"Frequency range: {extractor.frequencies[0]:.3f} - {extractor.frequencies[-1]:.3f} rad/s")


def example_2_custom_visualizations():
    """Example 2: Generate specific visualizations."""
    print("\n" + "=" * 80)
    print("EXAMPLE 2: Custom Visualizations")
    print("=" * 80)

    extractor = HydrodynamicCoefficientExtractor(
        excel_path='sample_data.xlsx',
        output_dir='data/marine_engineering/hydrodynamic'
    )

    extractor.create_sample_data()

    # Generate specific charts
    print("\nGenerating visualizations...")

    # 1. Frequency response curves
    extractor.plot_frequency_response_curves()

    # 2. Critical damping ratios
    extractor.plot_critical_damping_ratios()

    # 3. Coupling network
    extractor.plot_coupling_network()

    # 4. Natural periods
    extractor.plot_natural_periods()

    print("\nAll visualizations saved to docs/charts/phase2/")


def example_3_analyze_specific_frequency():
    """Example 3: Analyze coefficients at specific frequency."""
    print("\n" + "=" * 80)
    print("EXAMPLE 3: Analyze Specific Frequency")
    print("=" * 80)

    extractor = HydrodynamicCoefficientExtractor(
        excel_path='sample_data.xlsx',
        output_dir='data/marine_engineering/hydrodynamic'
    )

    extractor.create_sample_data()

    # Find frequency closest to 1.0 rad/s
    target_omega = 1.0
    idx = np.argmin(np.abs(extractor.frequencies - target_omega))
    omega = extractor.frequencies[idx]

    print(f"\nAnalyzing frequency: {omega:.4f} rad/s (Period: {2*np.pi/omega:.2f} s)")

    # Get matrices
    A = extractor.added_mass_matrices[omega]
    B = extractor.damping_matrices[omega]

    print("\nAdded Mass Matrix (diagonal):")
    for i, label in enumerate(extractor.DOF_LABELS):
        print(f"  {label:6s}: {A[i, i]:10.2f}")

    print("\nDamping Matrix (diagonal):")
    for i, label in enumerate(extractor.DOF_LABELS):
        print(f"  {label:6s}: {B[i, i]:10.2f}")

    # Generate heatmaps
    extractor.plot_heatmap_at_frequency(omega, 'added_mass')
    extractor.plot_heatmap_at_frequency(omega, 'damping')

    print(f"\nHeatmaps saved to docs/charts/phase2/")


def example_4_compare_frequencies():
    """Example 4: Compare coefficients across multiple frequencies."""
    print("\n" + "=" * 80)
    print("EXAMPLE 4: Compare Multiple Frequencies")
    print("=" * 80)

    extractor = HydrodynamicCoefficientExtractor(
        excel_path='sample_data.xlsx',
        output_dir='data/marine_engineering/hydrodynamic'
    )

    extractor.create_sample_data()

    # Compare low, medium, and high frequencies
    target_omegas = [0.5, 1.0, 2.0]

    print("\nComparing frequencies:")
    print(f"{'Frequency':<12} {'Period':<10} {'Heave A33':<12} {'Heave B33':<12}")
    print("-" * 50)

    for target in target_omegas:
        idx = np.argmin(np.abs(extractor.frequencies - target))
        omega = extractor.frequencies[idx]
        period = 2 * np.pi / omega

        A = extractor.added_mass_matrices[omega]
        B = extractor.damping_matrices[omega]

        print(f"{omega:8.4f} rad/s  {period:6.2f} s  {A[2,2]:10.1f}    {B[2,2]:10.1f}")


def example_5_export_for_analysis():
    """Example 5: Export data for external analysis tools."""
    print("\n" + "=" * 80)
    print("EXAMPLE 5: Export for External Analysis")
    print("=" * 80)

    extractor = HydrodynamicCoefficientExtractor(
        excel_path='sample_data.xlsx',
        output_dir='data/marine_engineering/hydrodynamic'
    )

    extractor.create_sample_data()

    # Export frequency-dependent diagonal coefficients
    import pandas as pd

    # Create DataFrame with all diagonal terms
    data = {
        'Frequency_rad_s': extractor.frequencies,
        'Period_s': extractor.periods,
    }

    # Add added mass diagonal terms
    for i, label in enumerate(extractor.DOF_LABELS):
        data[f'A_{i+1}{i+1}_{label}'] = [
            extractor.added_mass_matrices[omega][i, i]
            for omega in extractor.frequencies
        ]

    # Add damping diagonal terms
    for i, label in enumerate(extractor.DOF_LABELS):
        data[f'B_{i+1}{i+1}_{label}'] = [
            extractor.damping_matrices[omega][i, i]
            for omega in extractor.frequencies
        ]

    df = pd.DataFrame(data)

    # Save to CSV
    output_file = 'data/marine_engineering/hydrodynamic/diagonal_coefficients_vs_frequency.csv'
    df.to_csv(output_file, index=False)

    print(f"\nExported diagonal coefficients to: {output_file}")
    print(f"Rows: {len(df)}")
    print(f"Columns: {len(df.columns)}")

    # Show sample
    print("\nSample data (first 5 rows):")
    print(df.head().to_string())


def example_6_create_animations():
    """Example 6: Create animated visualizations."""
    print("\n" + "=" * 80)
    print("EXAMPLE 6: Create Animations")
    print("=" * 80)

    extractor = HydrodynamicCoefficientExtractor(
        excel_path='sample_data.xlsx',
        output_dir='data/marine_engineering/hydrodynamic'
    )

    extractor.create_sample_data()

    print("\nCreating animations (this may take a moment)...")

    # Create added mass animation
    anim1 = extractor.create_heatmap_animation('added_mass')
    print(f"Added mass animation: {anim1}")

    # Create damping animation
    anim2 = extractor.create_heatmap_animation('damping')
    print(f"Damping animation: {anim2}")

    print("\nAnimations can be viewed in a web browser or image viewer.")


def example_7_complete_pipeline():
    """Example 7: Complete extraction and reporting pipeline."""
    print("\n" + "=" * 80)
    print("EXAMPLE 7: Complete Pipeline")
    print("=" * 80)

    extractor = HydrodynamicCoefficientExtractor(
        excel_path='sample_data.xlsx',
        output_dir='data/marine_engineering/hydrodynamic'
    )

    # Run complete extraction with all outputs
    extractor.run_complete_extraction()

    print("\nComplete pipeline executed!")
    print("Check the following outputs:")
    print("  - CSV data files: data/marine_engineering/hydrodynamic/")
    print("  - Visualization charts: docs/charts/phase2/")
    print("  - HTML report: docs/hydro_coefficients_extraction_report.html")


def main():
    """Run all examples."""
    print("=" * 80)
    print("HYDRODYNAMIC COEFFICIENT EXTRACTOR - USAGE EXAMPLES")
    print("=" * 80)

    examples = [
        ("Basic Extraction", example_1_basic_extraction),
        ("Custom Visualizations", example_2_custom_visualizations),
        ("Analyze Specific Frequency", example_3_analyze_specific_frequency),
        ("Compare Frequencies", example_4_compare_frequencies),
        ("Export for Analysis", example_5_export_for_analysis),
        ("Create Animations", example_6_create_animations),
        ("Complete Pipeline", example_7_complete_pipeline),
    ]

    print("\nAvailable examples:")
    for i, (name, _) in enumerate(examples, 1):
        print(f"  {i}. {name}")

    print("\nRunning all examples...")

    for name, example_func in examples:
        try:
            example_func()
        except Exception as e:
            print(f"\nError in {name}: {e}")
            import traceback
            traceback.print_exc()

    print("\n" + "=" * 80)
    print("ALL EXAMPLES COMPLETED!")
    print("=" * 80)


if __name__ == '__main__':
    main()
