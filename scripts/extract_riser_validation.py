#!/usr/bin/env python
"""Extract validation reference results from A01 Catenary Riser model.

Runs statics on the monolithic model and extracts engineering results
to store as reference values in validation/results.json.

Usage:
    uv run python scripts/extract_riser_validation.py
"""

from __future__ import annotations

import json
import time
from pathlib import Path

try:
    import OrcFxAPI
except ImportError:
    print("OrcFxAPI not available - cannot extract validation results")
    exit(1)


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


def main() -> int:
    """Extract validation results from A01 Catenary Riser."""
    library_dir = Path("docs/modules/orcaflex/library/tier2_fast/a01_catenary_riser")
    monolithic_path = library_dir / "monolithic" / "A01 Catenary riser.dat"
    output_path = library_dir / "validation" / "results.json"

    if not monolithic_path.exists():
        print(f"Model not found: {monolithic_path}")
        return 1

    print(f"Loading model: {monolithic_path}")
    model = OrcFxAPI.Model(str(monolithic_path))

    print(f"Model loaded with {len(model.objects)} objects")

    # Run statics
    print("Running CalculateStatics()...")
    start_time = time.time()
    model.CalculateStatics()
    statics_time = time.time() - start_time
    print(f"Statics converged in {statics_time:.2f}s")

    # Find line objects
    lines = [obj for obj in model.objects if obj.type == OrcFxAPI.ObjectType.Line]
    print(f"Found {len(lines)} line objects: {[l.Name for l in lines]}")

    # Extract results
    results = {
        "model": "A01 Catenary Riser",
        "source": str(monolithic_path),
        "statics_time_seconds": round(statics_time, 2),
        "object_count": len(model.objects),
        "lines": {}
    }

    for line in lines:
        print(f"\nExtracting results for: {line.Name}")
        line_results = extract_line_results(model, line.Name)
        results["lines"][line.Name] = line_results

        print(f"  Total length: {line_results['total_length_m']:.2f} m")
        print(f"  End A tension: {line_results['end_a_tension_kN']:.2f} kN")
        print(f"  End B tension: {line_results['end_b_tension_kN']:.2f} kN")
        print(f"  Max tension: {line_results['max_tension_kN']:.2f} kN")

    # Save results
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, "w") as f:
        json.dump(results, f, indent=2)

    print(f"\nResults saved to: {output_path}")
    return 0


if __name__ == "__main__":
    exit(main())
