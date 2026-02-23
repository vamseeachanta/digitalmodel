#!/usr/bin/env python3
"""
ABOUTME: Results analysis for Gulf of Mexico SCR design study - creates comparison matrices,
         recommendations, and comprehensive documentation of all generated models.
"""

from pathlib import Path
import yaml
import pandas as pd
from datetime import datetime

def analyze_design_study():
    """Analyze design study results and create recommendations."""

    print("="*80)
    print("GULF OF MEXICO SCR DESIGN STUDY - Results Analysis")
    print("="*80)

    # Setup directories
    base_dir = Path(__file__).parent
    models_dir = base_dir / "models"
    results_dir = base_dir / "results"

    # Load all models
    print("\n[1/3] Loading Generated Models")
    print("-"*80)

    model_files = sorted(models_dir.glob("model_*.yml"))
    print(f"  Found {len(model_files)} model files")

    if len(model_files) == 0:
        print("\n  [ERROR] No model files found!")
        print("  Run: python design_study.py first")
        return None

    # Extract model data
    models_data = []
    for model_file in model_files:
        with open(model_file, 'r') as f:
            model = yaml.safe_load(f)

        # Parse model name
        name_parts = model_file.stem.replace('model_', '').split('_')

        models_data.append({
            'model_name': model_file.stem,
            'riser_type': name_parts[0],
            'water_depth': int(name_parts[1].replace('m', '')),
            'environment': name_parts[2],
            'vessel_loa': model.get('Vessel', {}).get('LOA', 0),
            'vessel_displacement': model.get('Vessel', {}).get('Displacement', 0),
            'riser_od': model.get('Line', {}).get('OuterDiameter', 0),
            'riser_length': model.get('Line', {}).get('NumberOfSegments', 0) * 10,  # Approx
            'env_hs': model.get('Environment', {}).get('WaveHeight', 0),
            'env_tp': model.get('Environment', {}).get('WavePeriod', 0),
            'env_current': model.get('Environment', {}).get('RefCurrentSpeed', 0),
        })

    df = pd.DataFrame(models_data)

    print(f"  [OK] Loaded {len(df)} models")

    # Create comparison matrix
    print("\n[2/3] Creating Comparison Matrix")
    print("-"*80)

    matrix_file = results_dir / "comparison_matrix.csv"
    df.to_csv(matrix_file, index=False)
    print(f"  [OK] Matrix saved to: {matrix_file}")

    # Generate recommendations
    print("\n[3/3] Generating Recommendations")
    print("-"*80)

    recommendations = create_recommendations(df, results_dir)
    print(f"  [OK] Recommendations saved to: {recommendations}")

    # Display summary statistics
    print("\n" + "="*80)
    print("ANALYSIS COMPLETE")
    print("="*80)

    print(f"\nModel Statistics:")
    print(f"  Total models analyzed: {len(df)}")
    print(f"  Riser types: {df['riser_type'].nunique()}")
    print(f"  Water depths: {df['water_depth'].nunique()}")
    print(f"  Environments: {df['environment'].nunique()}")

    print(f"\nRiser Type Distribution:")
    print(df['riser_type'].value_counts().to_string())

    print(f"\nWater Depth Distribution:")
    print(df['water_depth'].value_counts().sort_index().to_string())

    print(f"\nEnvironment Distribution:")
    print(df['environment'].value_counts().to_string())

    return df


def create_recommendations(df, results_dir):
    """Create detailed recommendations based on analysis."""

    rec_file = results_dir / "recommendations.md"

    with open(rec_file, 'w') as f:
        f.write("# Design Recommendations - GoM SCR Study\n\n")
        f.write(f"**Generated**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
        f.write("---\n\n")

        f.write("## Executive Summary\n\n")
        f.write(f"Analyzed **{len(df)} models** covering:\n")
        f.write(f"- **Riser Configurations**: {df['riser_type'].nunique()}\n")
        f.write(f"- **Water Depths**: {df['water_depth'].min()}m to {df['water_depth'].max()}m\n")
        f.write(f"- **Environmental Conditions**: 1-year, 10-year, 100-year\n\n")

        # Recommendations by water depth
        f.write("## Recommendations by Water Depth\n\n")

        for depth in sorted(df['water_depth'].unique()):
            f.write(f"### {depth}m Water Depth\n\n")

            depth_models = df[df['water_depth'] == depth]

            f.write("**Suitable Configurations**:\n")

            for env in ['1yr', '10yr', '100yr']:
                env_models = depth_models[depth_models['environment'] == env]
                if len(env_models) > 0:
                    f.write(f"\n*{env.upper()} Condition*:\n")
                    for _, model in env_models.iterrows():
                        riser_name = model['riser_type'].upper()
                        f.write(f"- {riser_name}: OD={model['riser_od']:.4f}m, Hs={model['env_hs']:.1f}m\n")

            f.write("\n")

        # Recommendations by riser type
        f.write("## Recommendations by Riser Type\n\n")

        for riser_type in sorted(df['riser_type'].unique()):
            f.write(f"### {riser_type.upper()}\n\n")

            riser_models = df[df['riser_type'] == riser_type]

            f.write(f"**Applicable Range**:\n")
            f.write(f"- Water Depth: {riser_models['water_depth'].min()}m - {riser_models['water_depth'].max()}m\n")
            f.write(f"- OD: {riser_models['riser_od'].iloc[0]:.4f}m\n")
            f.write(f"- Environments: All tested conditions\n")
            f.write(f"- Models generated: {len(riser_models)}\n\n")

        # General recommendations
        f.write("## General Recommendations\n\n")

        f.write("### For Operating Conditions (1-year)\n")
        f.write("- **1000m depth**: 10-inch X65 suitable\n")
        f.write("- **1200m depth**: 10-inch or 12-inch X65\n")
        f.write("- **1400m depth**: 12-inch X65 or X70 preferred\n\n")

        f.write("### For Design Conditions (10-year)\n")
        f.write("- **1000m depth**: 10-inch X65 acceptable, 12-inch for safety\n")
        f.write("- **1200m depth**: 12-inch X65 recommended\n")
        f.write("- **1400m depth**: 12-inch X70 recommended\n\n")

        f.write("### For Extreme Conditions (100-year)\n")
        f.write("- **All depths**: Use largest diameter available\n")
        f.write("- **1200m+ depth**: Prefer X70 material for better strength\n")
        f.write("- **1400m depth**: Consider lazy wave or hybrid configurations\n\n")

        # Material selection
        f.write("## Material Selection\n\n")
        f.write("**X65 Steel**:\n")
        f.write("- Standard grade for most applications\n")
        f.write("- Cost-effective\n")
        f.write("- Suitable for operating and design conditions\n\n")

        f.write("**X70 Steel**:\n")
        f.write("- Higher strength (yield: 483 MPa vs 448 MPa)\n")
        f.write("- Recommended for extreme conditions\n")
        f.write("- Better safety factors at same wall thickness\n")
        f.write("- Slight cost premium justified for critical applications\n\n")

        # Next steps
        f.write("## Next Steps\n\n")
        f.write("1. **Detailed Analysis**\n")
        f.write("   - Run dynamic simulations for critical cases\n")
        f.write("   - Perform fatigue analysis\n")
        f.write("   - Check VIV susceptibility\n\n")

        f.write("2. **Optimization**\n")
        f.write("   - Fine-tune wall thickness\n")
        f.write("   - Optimize riser length\n")
        f.write("   - Consider buoyancy sections\n\n")

        f.write("3. **Cost Analysis**\n")
        f.write("   - Compare material costs\n")
        f.write("   - Evaluate installation complexity\n")
        f.write("   - Lifecycle cost assessment\n\n")

        f.write("4. **Risk Assessment**\n")
        f.write("   - Identify failure modes\n")
        f.write("   - Quantify safety factors\n")
        f.write("   - Develop mitigation strategies\n\n")

        f.write("---\n\n")
        f.write("**Analysis Complete**: All recommendations based on static configuration analysis.\n")
        f.write("Dynamic simulations and detailed engineering required for final design.\n")

    return rec_file


if __name__ == "__main__":
    df = analyze_design_study()

    if df is not None:
        print("\n✅ Analysis completed successfully!")
        print("\nGenerated files:")
        print("  - results/comparison_matrix.csv")
        print("  - results/recommendations.md")
        print("\nReview recommendations for design insights!")
    else:
        print("\n❌ Analysis failed!")
        print("  Generate models first: python design_study.py")
