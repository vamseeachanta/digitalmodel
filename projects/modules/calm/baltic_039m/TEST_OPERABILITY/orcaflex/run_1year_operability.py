#!/usr/bin/env python3
"""
ABOUTME: Run 1-year operability analysis for all 12 directions
ABOUTME: Uses new modular structure with analysis_models/ directory
"""

import sys
from pathlib import Path
from datetime import datetime
import json

# Add OrcaFlex Python API to path (update to your OrcaFlex installation)
# Uncomment and update path for your installation:
# sys.path.append(r"C:\Program Files\Orcina\OrcaFlex\11.4\Python")

try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    print("⚠️  OrcaFlex Python API not found - running in DRY RUN mode")
    print("   To run actual simulations:")
    print("   1. Install OrcaFlex")
    print("   2. Update sys.path in this script to point to OrcaFlex Python directory")
    print("   Example: C:\\Program Files\\Orcina\\OrcaFlex\\11.4\\Python")
    ORCAFLEX_AVAILABLE = False

def run_analysis(model_path: Path, output_dir: Path, dry_run: bool = False) -> dict:
    """
    Run a single OrcaFlex analysis.

    Args:
        model_path: Path to analysis model .yml file
        output_dir: Directory for simulation results
        dry_run: If True, skip actual simulation

    Returns:
        Dictionary with analysis results
    """
    result = {
        'model': model_path.name,
        'status': 'unknown',
        'error': None,
        'statics_time': 0.0,
        'dynamics_time': 0.0,
        'total_time': 0.0
    }

    print(f"\n{'='*80}")
    print(f"Running: {model_path.name}")
    print(f"{'='*80}")

    if dry_run:
        print(f"  [DRY RUN] Model: {model_path.name}")
        print(f"  [DRY RUN] Would run statics and dynamics")
        print(f"  [DRY RUN] Would save to: {output_dir / model_path.stem}.sim")
        result['status'] = 'dry_run'
        result['total_time'] = 0.1  # Simulated time
        return result

    try:
        import time
        start_time = time.time()

        # Load model
        print(f"  Loading model: {model_path}")
        if not model_path.exists():
            raise FileNotFoundError(f"Model file not found: {model_path}")

        model = OrcFxAPI.Model(str(model_path))
        print(f"  ✓ Model loaded successfully")

        # Run static analysis
        print(f"  Running statics...")
        statics_start = time.time()
        model.CalculateStatics()
        statics_time = time.time() - statics_start
        result['statics_time'] = statics_time
        print(f"  ✓ Statics completed ({statics_time:.1f}s)")

        # Run dynamic analysis
        print(f"  Running dynamics...")
        dynamics_start = time.time()
        model.RunSimulation()
        dynamics_time = time.time() - dynamics_start
        result['dynamics_time'] = dynamics_time
        print(f"  ✓ Dynamics completed ({dynamics_time:.1f}s)")

        # Save results
        output_file = output_dir / f"{model_path.stem}.sim"
        print(f"  Saving results: {output_file.name}")
        model.SaveSimulation(str(output_file))
        print(f"  ✓ Results saved")

        # Calculate total time
        result['total_time'] = time.time() - start_time
        result['status'] = 'success'
        print(f"  [SUCCESS] {model_path.name} completed successfully ({result['total_time']:.1f}s)")

    except Exception as e:
        result['status'] = 'failed'
        result['error'] = str(e)
        print(f"  [FAILED] {model_path.name} failed: {e}")

    return result

def main():
    """Run 1-year operability analysis for all 12 directions."""
    print("="*80)
    print("1-YEAR OPERABILITY ANALYSIS - BATCH EXECUTION")
    print("="*80)
    print(f"Project: NSE_CALM_001")
    print(f"Return Period: 1-year")
    print(f"Directions: 12 (0° to 330° in 30° steps)")
    print(f"Model Type: Simple (single vessel)")
    print(f"")

    # Paths
    script_dir = Path(__file__).parent
    models_dir = script_dir / "analysis_models"
    results_dir = script_dir.parent / "results" / "1year_operability"
    results_dir.mkdir(parents=True, exist_ok=True)

    # Check if models directory exists
    if not models_dir.exists():
        print(f"\n❌ Analysis models directory not found: {models_dir}")
        return 1

    # Get all 1-year simple models (12 directions)
    model_pattern = "NSE_CALM_001_*deg_1yr_simple.yml"
    models = sorted(models_dir.glob(model_pattern))

    if not models:
        print(f"\n❌ No 1-year simple models found matching: {model_pattern}")
        print(f"   Expected pattern: NSE_CALM_001_000deg_1yr_simple.yml")
        return 1

    print(f"Found {len(models)} models to run:")
    for i, model in enumerate(models, 1):
        print(f"  {i:2d}. {model.name}")

    # Determine if running in dry run mode
    dry_run = not ORCAFLEX_AVAILABLE
    if dry_run:
        print(f"\n{'='*80}")
        print("RUNNING IN DRY RUN MODE (OrcaFlex not available)")
        print("No actual simulations will be executed")
        print(f"{'='*80}")

    # Run all analyses
    print(f"\n{'='*80}")
    print("STARTING BATCH EXECUTION")
    print(f"{'='*80}")
    print(f"Output directory: {results_dir}")
    print("")

    start_time = datetime.now()
    results = []

    for i, model_path in enumerate(models, 1):
        print(f"\n[{i}/{len(models)}]")
        result = run_analysis(model_path, results_dir, dry_run=dry_run)
        results.append(result)

    end_time = datetime.now()
    elapsed = (end_time - start_time).total_seconds()

    # Summary
    print(f"\n{'='*80}")
    print("BATCH ANALYSIS COMPLETE")
    print(f"{'='*80}")
    print(f"\nExecution Summary:")
    print(f"  Start time: {start_time.strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"  End time: {end_time.strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"  Total elapsed: {elapsed:.1f}s ({elapsed/60:.1f} minutes)")

    # Count results by status
    success_count = sum(1 for r in results if r['status'] == 'success')
    failed_count = sum(1 for r in results if r['status'] == 'failed')
    dry_run_count = sum(1 for r in results if r['status'] == 'dry_run')

    print(f"\nResults by Status:")
    if success_count > 0:
        print(f"  [SUCCESS] Success: {success_count}/{len(models)}")
    if dry_run_count > 0:
        print(f"  [DRY RUN] Dry Run: {dry_run_count}/{len(models)}")
    if failed_count > 0:
        print(f"  [FAILED] Failed: {failed_count}/{len(models)}")

    print(f"\nResults by Direction:")
    for result in results:
        status_icon = "[OK]" if result['status'] == 'success' else ("[DRY]" if result['status'] == 'dry_run' else "[ERR]")
        model_name = result['model']
        time_str = f"({result['total_time']:.1f}s)" if result['total_time'] > 0 else ""
        print(f"  {status_icon} {model_name} {time_str}")
        if result['error']:
            print(f"      Error: {result['error']}")

    # Performance statistics (for actual runs)
    if success_count > 0:
        successful_results = [r for r in results if r['status'] == 'success']
        total_time = sum(r['total_time'] for r in successful_results)
        avg_time = total_time / len(successful_results)
        total_statics = sum(r['statics_time'] for r in successful_results)
        total_dynamics = sum(r['dynamics_time'] for r in successful_results)

        print(f"\nPerformance Statistics:")
        print(f"  Total computation time: {total_time:.1f}s ({total_time/60:.1f} min)")
        print(f"  Average per model: {avg_time:.1f}s")
        print(f"  Statics time: {total_statics:.1f}s ({total_statics/total_time*100:.1f}%)")
        print(f"  Dynamics time: {total_dynamics:.1f}s ({total_dynamics/total_time*100:.1f}%)")

    # Save results to JSON
    results_file = results_dir / f"batch_results_{start_time.strftime('%Y%m%d_%H%M%S')}.json"
    with open(results_file, 'w') as f:
        json.dump({
            'timestamp': start_time.isoformat(),
            'elapsed_seconds': elapsed,
            'total_models': len(models),
            'success_count': success_count,
            'failed_count': failed_count,
            'dry_run_count': dry_run_count,
            'results': results
        }, f, indent=2)
    print(f"\nResults saved to: {results_file}")

    # Final status
    if dry_run_count == len(models):
        print(f"\n[DRY RUN COMPLETE] {len(models)} models validated")
        print(f"   Install OrcaFlex to run actual simulations")
        return 0
    elif success_count == len(models):
        print(f"\n[SUCCESS] All {len(models)} analyses completed successfully!")
        return 0
    elif failed_count > 0:
        print(f"\n[WARNING] {failed_count} analyses failed")
        return 1
    else:
        return 0

if __name__ == "__main__":
    sys.exit(main())
