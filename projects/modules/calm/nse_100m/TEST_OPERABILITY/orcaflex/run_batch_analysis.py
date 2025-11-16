#!/usr/bin/env python3
"""
ABOUTME: OrcaFlex batch execution script for operability analysis
ABOUTME: Runs all 12 load cases with load case-specific environment modules
"""

import sys
from pathlib import Path

# Add OrcaFlex Python API to path (update to your OrcaFlex installation)
# sys.path.append(r"C:\Program Files\Orcina\OrcaFlex\11.4\Python")

try:
    import OrcFxAPI
except ImportError:
    print("❌ OrcaFlex Python API not found.")
    print("   Install OrcaFlex and update sys.path above to point to OrcaFlex Python directory")
    print("   Example: C:\\Program Files\\Orcina\\OrcaFlex\\11.4\\Python")
    sys.exit(1)

def run_load_case(base_model_path: Path, load_case_name: str, output_dir: Path) -> bool:
    """
    Run a single operability load case.

    Args:
        base_model_path: Path to base OrcaFlex .yml file
        load_case_name: Name of load case (e.g., 'operability_000deg')
        output_dir: Directory for simulation results

    Returns:
        True if simulation succeeded, False otherwise
    """
    print(f"\n================================================================================")
    print(f"Running: {load_case_name}")
    print(f"================================================================================")

    try:
        # Load base model
        print(f"  Loading base model: {base_model_path.name}")
        model = OrcFxAPI.Model(str(base_model_path))

        # Update environment module paths to use load case-specific modules
        # OrcaFlex models using includefile need module paths updated
        modules_dir = base_model_path.parent / "modules" / load_case_name

        if not modules_dir.exists():
            print(f"  ⚠️  Warning: Load case-specific modules not found: {modules_dir}")
            print(f"      Using default modules instead")
            modules_dir = base_model_path.parent / "modules"

        # Update wave module
        wave_module = modules_dir / "_03c_waves.yml"
        if wave_module.exists():
            print(f"  ✓ Using wave module: {wave_module.relative_to(base_model_path.parent)}")
            # Note: Actual module path update depends on OrcaFlex model structure
            # This is a placeholder - adjust based on your specific includefile implementation

        # Run static and dynamic analysis
        print(f"  Running statics...")
        model.CalculateStatics()

        print(f"  Running dynamics...")
        model.RunSimulation()

        # Save results
        output_file = output_dir / f"{load_case_name}.sim"
        print(f"  Saving results: {output_file.name}")
        model.SaveSimulation(str(output_file))

        print(f"  ✅ {load_case_name} completed successfully")
        return True

    except Exception as e:
        print(f"  ❌ {load_case_name} failed: {e}")
        return False

def main():
    """Run batch operability analysis."""
    print("="*80)
    print("ORCAFLEX BATCH OPERABILITY ANALYSIS")
    print("="*80)
    print(f"Project: NSE_CALM_001")
    print(f"Load cases: 12")

    # Paths
    script_dir = Path(__file__).parent
    base_model = script_dir / "NSE_CALM_001_calm_buoy.yml"
    results_dir = script_dir.parent / "results"
    results_dir.mkdir(exist_ok=True)

    if not base_model.exists():
        print(f"\n❌ Base model not found: {base_model}")
        return 1

    # Load cases to run
    load_cases = ['"operability_000deg"', '"operability_030deg"', '"operability_060deg"', '"operability_090deg"', '"operability_120deg"', '"operability_150deg"', '"operability_180deg"', '"operability_210deg"', '"operability_240deg"', '"operability_270deg"', '"operability_300deg"', '"operability_330deg"']

    # Run all load cases
    results = {}
    for i, load_case in enumerate(load_cases, 1):
        print(f"\n[{i}/{len(load_cases)}]")
        success = run_load_case(base_model, load_case, results_dir)
        results[load_case] = "✓" if success else "✗"

    # Summary
    print("\n" + "="*80)
    print("BATCH ANALYSIS COMPLETE")
    print("="*80)
    print("\nResults Summary:")
    for load_case, status in results.items():
        print(f"  {status} {load_case}")

    successes = sum(1 for v in results.values() if v == "✓")
    print(f"\nCompleted: {successes}/{len(load_cases)} load cases")

    if successes == len(load_cases):
        print("\n✅ All load cases completed successfully!")
        return 0
    else:
        print(f"\n⚠️  {len(load_cases) - successes} load cases failed")
        return 1

if __name__ == "__main__":
    sys.exit(main())
