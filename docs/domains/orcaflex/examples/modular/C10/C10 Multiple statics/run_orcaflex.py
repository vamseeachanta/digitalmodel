#!/usr/bin/env python3
"""
Run OrcaFlex multiple static analyses for C10.

ABOUTME: Demonstrates YAML-driven multi-static sweep — running the same model across
a range of parameter combinations using multiple static calls without a dynamic stage.
Equivalent to a quasi-static sensitivity study or load-case matrix.

Example features:
- Multiple static analyses from a single loaded model
- Parameter sweep: wave direction (0°–180° in 45° steps) at fixed Hs/Tp
- Each static result is extracted and accumulated before saving
- Base model: 100m WD, Hs=7m, Tp=8s

Expected outputs:
- C10_static_results.csv      (summary: case, wave_dir, top_tension, max_curvature)
- Console: per-case static results table

Run with: uv run python run_orcaflex.py

Note: Multiple statics workflow — load once, modify parameters, re-run statics.
This avoids the overhead of reloading the model for each case.
"""
import sys
import os
import csv
import time
from pathlib import Path


def _load_orcfxapi() -> None:
    api_path = os.environ.get(
        "ORCAFLEX_API_PATH",
        r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcFxAPI\Python",
    )
    if api_path not in sys.path:
        sys.path.insert(0, api_path)
    try:
        import OrcFxAPI  # noqa: F401
        print(f"[OK] OrcFxAPI loaded from {api_path}")
    except ImportError as exc:
        print(f"[ERROR] Cannot import OrcFxAPI: {exc}")
        print("  Set ORCAFLEX_API_PATH env var or install OrcaFlex 11.x")
        sys.exit(1)


def main() -> None:
    _load_orcfxapi()
    import OrcFxAPI

    example_dir = Path(__file__).parent
    model_file = example_dir / "master.yml"

    if not model_file.exists():
        print(f"[ERROR] Model file not found: {model_file}")
        sys.exit(1)

    print(f"[INFO] Loading base model: {model_file}")
    model = OrcFxAPI.Model(str(model_file))
    print(f"[OK]  Model loaded — {len(list(model.objects))} objects")

    # Parameter sweep definition
    # Each tuple: (case_id, wave_direction_deg)
    sweep_cases = [
        ("case_000", 0.0),
        ("case_045", 45.0),
        ("case_090", 90.0),
        ("case_135", 135.0),
        ("case_180", 180.0),
    ]

    results = []
    print(f"\n{'Case':12s}  {'WaveDir':>8s}  {'TopTension(kN)':>15s}  {'MaxCurv(1/m)':>13s}")
    print("-" * 56)

    env = model.environment
    lines = [o for o in model.objects if isinstance(o, OrcFxAPI.Line)]

    for case_id, wave_dir in sweep_cases:
        # Modify wave direction in-place — no model reload needed
        env.WaveDirection = wave_dir

        t0 = time.time()
        model.CalculateStatics()
        elapsed = time.time() - t0

        row = {"case": case_id, "wave_direction_deg": wave_dir}
        top_tension = None
        max_curv = None

        if lines:
            line = lines[0]
            top_tension = line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
            curv_graph = line.RangeGraph("Curvature")
            max_curv = max(curv_graph.Max) if curv_graph.Max else 0.0
            row["top_tension_kN"] = round(top_tension, 2)
            row["max_curvature_per_m"] = round(max_curv, 5)

        results.append(row)
        tt_str = f"{top_tension:.1f}" if top_tension is not None else "N/A"
        mc_str = f"{max_curv:.5f}" if max_curv is not None else "N/A"
        print(f"{case_id:12s}  {wave_dir:>8.1f}  {tt_str:>15s}  {mc_str:>13s}  ({elapsed:.1f}s)")

    # Write CSV summary
    csv_path = example_dir / "C10_static_results.csv"
    fieldnames = ["case", "wave_direction_deg", "top_tension_kN", "max_curvature_per_m"]
    with open(csv_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for row in results:
            writer.writerow({k: row.get(k, "") for k in fieldnames})

    print(f"\n[OK]  Results saved: {csv_path.name} ({len(results)} cases)")


if __name__ == "__main__":
    main()
