#!/usr/bin/env python
"""Extract validation reference results from OrcaFlex library models.

Runs statics on monolithic models and extracts engineering results
to store as reference values in validation/results.json.

Usage:
    uv run python scripts/extract_library_validation.py [model_dir]

Examples:
    uv run python scripts/extract_library_validation.py a01_lazy_wave_riser
    uv run python scripts/extract_library_validation.py  # Process all tier2_fast
"""

from __future__ import annotations

import json
import sys
import time
from pathlib import Path

try:
    import OrcFxAPI
except ImportError:
    print("OrcFxAPI not available - cannot extract validation results")
    sys.exit(1)


LIBRARY_ROOT = Path("docs/domains/orcaflex/library")


def extract_line_results(model: OrcFxAPI.Model, line_name: str) -> dict:
    """Extract static results from a line object."""
    line = model[line_name]

    # Get line properties
    num_sections = line.NumberOfSections
    total_length = sum(line.Length[i] for i in range(num_sections))

    # Extract end results
    end_a_tension = line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
    end_b_tension = line.StaticResult("Effective Tension", OrcFxAPI.oeEndB)
    end_a_bending = line.StaticResult("Bend Moment", OrcFxAPI.oeEndA)
    end_b_bending = line.StaticResult("Bend Moment", OrcFxAPI.oeEndB)

    # Get max/min along length
    tension_rg = line.RangeGraph("Effective Tension")
    bending_rg = line.RangeGraph("Bend Moment")

    return {
        "line_name": line_name,
        "total_length_m": total_length,
        "num_sections": num_sections,
        "end_a_tension_kN": end_a_tension,
        "end_b_tension_kN": end_b_tension,
        "end_a_bending_kNm": end_a_bending,
        "end_b_bending_kNm": end_b_bending,
        "max_tension_kN": max(tension_rg.Mean),
        "min_tension_kN": min(tension_rg.Mean),
        "max_bending_kNm": max(abs(v) for v in bending_rg.Mean),
    }


def process_model_dir(model_dir: Path) -> dict | None:
    """Process a single model directory and extract validation results."""
    monolithic_dir = model_dir / "monolithic"
    validation_dir = model_dir / "validation"

    if not monolithic_dir.exists():
        print(f"  Skipping {model_dir.name}: no monolithic/ directory")
        return None

    # Find .dat or .yml file
    model_files = list(monolithic_dir.glob("*.dat")) + list(monolithic_dir.glob("*.yml"))
    dat_files = [f for f in model_files if f.suffix == ".dat"]

    if dat_files:
        model_path = dat_files[0]  # Prefer .dat
    elif model_files:
        model_path = model_files[0]
    else:
        print(f"  Skipping {model_dir.name}: no model file found")
        return None

    print(f"\nProcessing: {model_dir.name}")
    print(f"  Loading: {model_path.name}")

    try:
        model = OrcFxAPI.Model(str(model_path))
    except OrcFxAPI.DLLError as exc:
        print(f"  ERROR loading model: {exc}")
        return None

    print(f"  Model loaded with {len(model.objects)} objects")

    # Run statics
    print("  Running CalculateStatics()...")
    start_time = time.time()
    try:
        model.CalculateStatics()
    except OrcFxAPI.DLLError as exc:
        print(f"  ERROR in statics: {exc}")
        return None
    statics_time = time.time() - start_time
    print(f"  Statics converged in {statics_time:.2f}s")

    # Find line objects
    lines = [obj for obj in model.objects if obj.type == OrcFxAPI.ObjectType.Line]
    print(f"  Found {len(lines)} line(s): {[l.Name for l in lines]}")

    # Extract results
    results = {
        "model": model_dir.name,
        "source": str(model_path),
        "statics_time_seconds": round(statics_time, 2),
        "object_count": len(model.objects),
        "lines": {}
    }

    for line in lines:
        line_results = extract_line_results(model, line.Name)
        results["lines"][line.Name] = line_results
        print(f"    {line.Name}: {line_results['total_length_m']:.1f}m, "
              f"EndA={line_results['end_a_tension_kN']:.1f}kN, "
              f"EndB={line_results['end_b_tension_kN']:.1f}kN")

    # Save results
    validation_dir.mkdir(parents=True, exist_ok=True)
    output_path = validation_dir / "results.json"
    with open(output_path, "w") as f:
        json.dump(results, f, indent=2)
    print(f"  Saved: {output_path}")

    return results


def main() -> int:
    """Extract validation results from library models."""
    if len(sys.argv) > 1:
        # Process specific model
        model_name = sys.argv[1]
        # Search in all tiers
        for tier_dir in LIBRARY_ROOT.glob("tier*"):
            model_dir = tier_dir / model_name
            if model_dir.exists():
                result = process_model_dir(model_dir)
                return 0 if result else 1
        print(f"Model not found: {model_name}")
        return 1
    else:
        # Process all tier2_fast models
        tier2_dir = LIBRARY_ROOT / "tier2_fast"
        if not tier2_dir.exists():
            print(f"Library not found: {tier2_dir}")
            return 1

        print(f"Processing all models in: {tier2_dir}")
        success_count = 0
        fail_count = 0

        for model_dir in sorted(tier2_dir.iterdir()):
            if model_dir.is_dir():
                result = process_model_dir(model_dir)
                if result:
                    success_count += 1
                else:
                    fail_count += 1

        print(f"\n=== Summary ===")
        print(f"Processed: {success_count} successful, {fail_count} failed")
        return 0 if fail_count == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
