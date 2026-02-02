#!/usr/bin/env python3
"""
ABOUTME: Command-line script for operability analysis of CALM Buoy simulation results
ABOUTME: Generates comprehensive HTML reports with interactive visualizations
"""

import sys
import argparse
from pathlib import Path
import pandas as pd
import yaml
import io

# Configure stdout for UTF-8 on Windows
if sys.platform == 'win32':
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from digitalmodel.orcaflex.operability_analysis import OperabilityAnalyzer


def load_project_config(config_path: Path) -> dict:
    """Load project configuration YAML."""
    with open(config_path, 'r', encoding='utf-8') as f:
        return yaml.safe_load(f)


def extract_design_parameters(config: dict) -> dict:
    """Extract key design parameters from project configuration."""
    human_input = config.get('human_input', {})
    mooring = human_input.get('mooring', {})

    # Get mooring line MBL
    line_segments = mooring.get('line_segments', [])
    mbl = line_segments[0].get('mbl', 7300) if line_segments else 7300

    # Get safety factors
    sf_intact = mooring.get('safety_factor_intact', 1.8)
    sf_damaged = mooring.get('safety_factor_damaged', 1.3)

    # Get project info
    project = human_input.get('project', {})
    site = human_input.get('site', {})

    return {
        'mbl': mbl,
        'sf_intact': sf_intact,
        'sf_damaged': sf_damaged,
        'project_info': {
            'name': project.get('name', 'Unknown'),
            'code': project.get('code', 'UNKNOWN'),
            'location': project.get('location', 'Unknown'),
            'water_depth': site.get('water_depth', 0)
        }
    }


def create_sample_results(num_directions: int = 12) -> pd.DataFrame:
    """
    Create sample results for testing (when OrcaFlex results not available).

    Args:
        num_directions: Number of directions analyzed

    Returns:
        Sample results DataFrame
    """
    import numpy as np

    angle_step = 360 / num_directions
    headings = [int(i * angle_step) for i in range(num_directions)]

    # Generate realistic-looking tensions (highest around 0°-90°, lowest around 180°-270°)
    max_tensions = []
    mean_tensions = []

    for heading in headings:
        # Create variation with heading (highest at beam seas ~90°)
        angle_rad = np.deg2rad(heading)
        base_max = 3500 + 1000 * np.sin(angle_rad)**2  # Peak around 90°/270°
        base_mean = base_max * 0.75

        # Add some randomness
        max_tensions.append(base_max + np.random.normal(0, 100))
        mean_tensions.append(base_mean + np.random.normal(0, 80))

    return pd.DataFrame({
        'heading': headings,
        'max_tension': max_tensions,
        'mean_tension': mean_tensions,
        'std_tension': [200 + np.random.normal(0, 30) for _ in range(num_directions)]
    })


def create_sample_wave_scatter() -> pd.DataFrame:
    """Create sample wave scatter diagram for North Sea."""
    return pd.DataFrame({
        'Hs': [0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 6.0, 7.0, 8.0],
        'Tp': [5.0, 6.0, 7.0, 7.5, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0],
        'occurrence_pct': [8.0, 18.0, 22.0, 20.0, 12.0, 8.0, 5.0, 3.0, 2.0, 1.0, 0.5, 0.3, 0.2]
    })


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Analyze CALM Buoy operability simulation results"
    )
    parser.add_argument(
        '--project',
        type=Path,
        required=True,
        help='Path to project directory'
    )
    parser.add_argument(
        '--results',
        type=Path,
        help='Path to results CSV file (optional, uses project/results/operability_results.csv by default)'
    )
    parser.add_argument(
        '--wave-scatter',
        type=Path,
        help='Path to wave scatter diagram CSV (optional)'
    )
    parser.add_argument(
        '--operability-limit',
        type=float,
        default=2.5,
        help='Operability limit Hs (m) for downtime calculation (default: 2.5)'
    )
    parser.add_argument(
        '--use-sample-data',
        action='store_true',
        help='Use sample data for testing (when OrcaFlex results not available)'
    )
    parser.add_argument(
        '--sample-directions',
        type=int,
        default=12,
        choices=[12, 24, 36],
        help='Number of directions for sample data (default: 12)'
    )

    args = parser.parse_args()

    print("=" * 80)
    print("CALM BUOY OPERABILITY ANALYSIS")
    print("=" * 80)

    # Initialize analyzer
    analyzer = OperabilityAnalyzer(args.project)
    print(f"\n📁 Project directory: {args.project}")

    # Load project configuration
    config_path = args.project / "project_config.yml"
    if config_path.exists():
        print(f"📖 Loading configuration: {config_path}")
        config = load_project_config(config_path)
        params = extract_design_parameters(config)
        print(f"✅ Configuration loaded")
        print(f"   MBL: {params['mbl']:.0f} kN")
        print(f"   SF (Intact): {params['sf_intact']:.2f}")
        print(f"   SF (Damaged): {params['sf_damaged']:.2f}")
    else:
        print(f"⚠️  Configuration not found, using defaults")
        params = {
            'mbl': 7300,
            'sf_intact': 1.8,
            'sf_damaged': 1.3,
            'project_info': {'name': 'Unknown', 'code': 'UNKNOWN', 'location': 'Unknown', 'water_depth': 0}
        }

    # Load or create simulation results
    if args.use_sample_data:
        print(f"\n📊 Creating sample results data ({args.sample_directions} directions)...")
        results = create_sample_results(args.sample_directions)
        print(f"✅ Sample data created: {len(results)} data points")
    else:
        print(f"\n📊 Loading simulation results...")
        try:
            results = analyzer.load_simulation_results(args.results)
            print(f"✅ Results loaded: {len(results)} data points")
        except FileNotFoundError as e:
            print(f"❌ {e}")
            print(f"\n💡 Tip: Use --use-sample-data flag to generate example report")
            return 1

    # Load wave scatter if provided
    wave_scatter = None
    if args.wave_scatter:
        print(f"\n🌊 Loading wave scatter diagram: {args.wave_scatter}")
        wave_scatter = pd.read_csv(args.wave_scatter)
        print(f"✅ Wave scatter loaded: {len(wave_scatter)} sea states")
    elif args.use_sample_data:
        print(f"\n🌊 Creating sample wave scatter diagram...")
        wave_scatter = create_sample_wave_scatter()
        print(f"✅ Sample wave scatter created")

    # Generate comprehensive report
    print(f"\n📈 Generating operability analysis report...")
    print(f"   Operability limit: Hs = {args.operability_limit} m")

    report_path = analyzer.generate_comprehensive_report(
        results=results,
        mbl=params['mbl'],
        sf_intact=params['sf_intact'],
        sf_damaged=params['sf_damaged'],
        wave_scatter=wave_scatter,
        operability_limit_hs=args.operability_limit if wave_scatter is not None else None,
        project_info=params['project_info']
    )

    # Summary
    print("\n" + "=" * 80)
    print("✅ OPERABILITY ANALYSIS COMPLETE")
    print("=" * 80)
    print(f"\nReport location: {report_path}")
    print(f"\nNext steps:")
    print(f"  1. Open report in browser: {report_path}")
    print(f"  2. Review critical headings and tension envelope")
    print(f"  3. Check weather downtime statistics (if wave scatter provided)")

    # Print summary statistics
    print(f"\n📊 Quick Summary:")
    print(f"   Maximum tension: {results['max_tension'].max():.1f} kN @ {results.loc[results['max_tension'].idxmax(), 'heading']:.0f}°")
    print(f"   Minimum tension: {results['max_tension'].min():.1f} kN @ {results.loc[results['max_tension'].idxmin(), 'heading']:.0f}°")
    print(f"   Mean tension: {results['max_tension'].mean():.1f} kN")
    print(f"   Intact limit: {params['mbl']/params['sf_intact']:.1f} kN")

    max_utilization = results['max_tension'].max() / (params['mbl']/params['sf_intact'])
    status_emoji = "✅" if max_utilization < 1.0 else "❌"
    print(f"   Max utilization: {max_utilization:.3f} {status_emoji}")

    if max_utilization > 1.0:
        print(f"\n⚠️  WARNING: Maximum utilization exceeds 1.0 - design may not meet safety criteria!")
    elif max_utilization > 0.9:
        print(f"\n⚠️  CAUTION: Maximum utilization > 0.9 - approaching design limit")
    else:
        print(f"\n✅ PASS: All headings meet design criteria")

    return 0


if __name__ == "__main__":
    sys.exit(main())
