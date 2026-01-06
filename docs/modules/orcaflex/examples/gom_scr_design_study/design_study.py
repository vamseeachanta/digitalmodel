#!/usr/bin/env python3
"""
ABOUTME: Gulf of Mexico SCR design study execution script - generates 27 OrcaFlex models
         from parametric configurations and creates comprehensive analysis summary.
"""

from pathlib import Path
import yaml
import time
from datetime import datetime
from digitalmodel.modules.orcaflex.model_generator import OrcaFlexModelGenerator

def run_design_study():
    """Execute complete design study workflow."""

    print("="*80)
    print("GULF OF MEXICO SCR DESIGN STUDY - Execution")
    print("="*80)

    # Setup directories
    base_dir = Path(__file__).parent
    configs_dir = base_dir / "configs"
    models_dir = base_dir / "models"
    results_dir = base_dir / "results"

    models_dir.mkdir(exist_ok=True)
    results_dir.mkdir(exist_ok=True)

    # Initialize generator
    print("\n[1/4] Initializing Model Generator")
    print("-"*80)
    generator = OrcaFlexModelGenerator()
    print("  [OK] Generator ready")

    # Find configuration files
    print("\n[2/4] Loading Configurations")
    print("-"*80)
    config_files = sorted(configs_dir.glob("model_*.yml"))
    print(f"  Found {len(config_files)} configuration files")

    if len(config_files) == 0:
        print("\n  [ERROR] No configuration files found!")
        print("  Run: python generate_configs.py first")
        return None

    # Generate models
    print("\n[3/4] Generating OrcaFlex Models")
    print("-"*80)

    results = {
        'total': len(config_files),
        'successful': 0,
        'failed': 0,
        'models': [],
        'errors': [],
        'start_time': time.time()
    }

    for idx, config_file in enumerate(config_files, 1):
        try:
            # Load configuration
            with open(config_file, 'r') as f:
                config = yaml.safe_load(f)

            model_name = config_file.stem
            output_file = models_dir / f"{model_name}.yml"

            # Generate model
            start = time.time()
            model = generator.generate_from_template(
                template="risers/scr_catenary",
                config=config,
                output=output_file
            )
            elapsed = time.time() - start

            # Validate
            validation = generator.validate(model)

            results['successful'] += 1
            results['models'].append({
                'name': model_name,
                'config': config_file.name,
                'output': output_file.name,
                'time': elapsed,
                'valid': validation['is_valid'],
                'warnings': len(validation['warnings'])
            })

            status = "[OK]" if validation['is_valid'] else "[WARN]"
            print(f"  [{idx:02d}/{len(config_files)}] {status} {model_name} ({elapsed:.3f}s)")

        except Exception as e:
            results['failed'] += 1
            results['errors'].append({
                'config': config_file.name,
                'error': str(e)
            })
            print(f"  [{idx:02d}/{len(config_files)}] [FAIL] {model_name}: {e}")

    results['end_time'] = time.time()
    results['total_time'] = results['end_time'] - results['start_time']

    # Generate summary
    print("\n[4/4] Creating Summary Report")
    print("-"*80)

    summary = create_summary_report(results, results_dir)
    print(f"  [OK] Summary saved to: {summary}")

    # Display results
    print("\n" + "="*80)
    print("DESIGN STUDY COMPLETE")
    print("="*80)

    print(f"\nResults:")
    print(f"  Total models: {results['total']}")
    print(f"  Successful: {results['successful']}")
    print(f"  Failed: {results['failed']}")
    print(f"  Success rate: {results['successful']/results['total']*100:.1f}%")

    print(f"\nPerformance:")
    print(f"  Total time: {results['total_time']:.3f}s")
    print(f"  Avg per model: {results['total_time']/results['total']:.3f}s")
    print(f"  Throughput: {results['total']/results['total_time']:.1f} models/sec")

    print(f"\nOutput:")
    print(f"  Models: {models_dir}/")
    print(f"  Results: {results_dir}/")

    if results['errors']:
        print(f"\nErrors: {len(results['errors'])}")
        for error in results['errors']:
            print(f"  - {error['config']}: {error['error']}")

    return results


def create_summary_report(results, results_dir):
    """Create comprehensive summary report."""

    report_file = results_dir / "design_study_summary.md"

    with open(report_file, 'w') as f:
        f.write("# Gulf of Mexico SCR Design Study - Summary Report\n\n")
        f.write(f"**Generated**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
        f.write("---\n\n")

        # Executive Summary
        f.write("## Executive Summary\n\n")
        f.write(f"- **Total Models**: {results['total']}\n")
        f.write(f"- **Successful**: {results['successful']}\n")
        f.write(f"- **Failed**: {results['failed']}\n")
        f.write(f"- **Success Rate**: {results['successful']/results['total']*100:.1f}%\n")
        f.write(f"- **Total Time**: {results['total_time']:.3f}s\n")
        f.write(f"- **Avg Per Model**: {results['total_time']/results['total']:.3f}s\n")
        f.write(f"- **Throughput**: {results['total']/results['total_time']:.1f} models/sec\n\n")

        # Model List
        f.write("## Generated Models\n\n")
        f.write("| # | Model Name | Time (s) | Valid | Warnings |\n")
        f.write("|---|------------|----------|-------|----------|\n")

        for idx, model in enumerate(results['models'], 1):
            valid_icon = "✅" if model['valid'] else "⚠️"
            f.write(f"| {idx:02d} | {model['name']} | {model['time']:.3f} | {valid_icon} | {model['warnings']} |\n")

        # Study Parameters
        f.write("\n## Study Parameters\n\n")
        f.write("**Riser Configurations**: 3\n")
        f.write("- SCR 10-inch X65\n")
        f.write("- SCR 12-inch X65\n")
        f.write("- SCR 10-inch X70\n\n")

        f.write("**Water Depths**: 3\n")
        f.write("- 1000m (shallow deepwater)\n")
        f.write("- 1200m (medium deepwater)\n")
        f.write("- 1400m (deep water)\n\n")

        f.write("**Environmental Conditions**: 3\n")
        f.write("- GoM 1-year (operating)\n")
        f.write("- GoM 10-year (design)\n")
        f.write("- GoM 100-year (extreme)\n\n")

        # Errors
        if results['errors']:
            f.write("## Errors\n\n")
            for error in results['errors']:
                f.write(f"- **{error['config']}**: {error['error']}\n")
        else:
            f.write("## Errors\n\nNone\n")

        f.write("\n---\n\n")
        f.write("**Status**: Complete\n")

    return report_file


if __name__ == "__main__":
    results = run_design_study()

    if results:
        print("\n✅ Design study completed successfully!")
        print("\nNext steps:")
        print("  1. Review models in models/ directory")
        print("  2. Convert to .dat if needed for OrcaFlex")
        print("  3. Run simulations")
        print("  4. Analyze results")
    else:
        print("\n❌ Design study failed!")
        print("  Check error messages above")
