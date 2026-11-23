#!/usr/bin/env python3
"""
Run OrcaFlex pipelay analysis for E08 PipelayConfig (lay table automation).

ABOUTME: YAML-driven lay table automation example. Demonstrates running a series of
pipelay static analyses at different vessel positions (lay table) without reloading
the model — the same approach used for operability windows and fatigue screening.

Example features:
- Pipelay S-lay configuration (stinger + vessel)
- Lay table sweep: vessel advances in 10m increments along lay route
- Static analysis at each position, extract overbend and sagbend curvatures
- Documents the YAML batch config pattern: params injected from lay table config

Expected outputs:
- E08_lay_table_results.csv   (position, overbend_curv, sagbend_curv, top_tension)
- Console: lay table progress, key curvature checks

Run with: uv run python run_orcaflex.py

Note: E08 is the reference example for YAML-driven batch pipelay analysis.
In production, the lay table positions are injected from a YAML config rather
than hardcoded — see orcaflex-batch-manager and orcaflex-installation-analysis skills.
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

    print(f"[INFO] Loading pipelay model: {model_file}")
    model = OrcFxAPI.Model(str(model_file))
    print(f"[OK]  Model loaded — {len(list(model.objects))} objects")

    # Lay table: vessel X-position increments (metres along lay route)
    # In production these come from a YAML lay table config
    lay_positions = list(range(0, 110, 10))  # 0m to 100m in 10m steps

    lines = [o for o in model.objects if isinstance(o, OrcFxAPI.Line)]
    vessels = [o for o in model.objects if isinstance(o, OrcFxAPI.Vessel)]

    results = []
    print(f"\n{'Position(m)':>12s}  {'OverbendCurv':>13s}  {'SagbendCurv':>12s}  {'TopTens(kN)':>12s}")
    print("-" * 55)

    for pos in lay_positions:
        # Advance vessel position along X (simplified lay table step)
        if vessels:
            vessel = vessels[0]
            vessel.InitialX = float(pos)

        t0 = time.time()
        model.CalculateStatics()
        elapsed = time.time() - t0

        row = {"lay_position_m": pos}
        ob_curv = sb_curv = top_t = None

        if lines:
            pipe = lines[0]
            curv_graph = pipe.RangeGraph("Curvature")
            if curv_graph.Max:
                # Overbend = max curvature in upper 20% of arc length
                n = len(curv_graph.Max)
                ob_curv = max(curv_graph.Max[: max(1, n // 5)])
                sb_curv = max(curv_graph.Max[n // 2 :])
            top_t = pipe.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
            row.update({
                "overbend_curvature_1_m": round(ob_curv, 5) if ob_curv else "",
                "sagbend_curvature_1_m": round(sb_curv, 5) if sb_curv else "",
                "top_tension_kN": round(top_t, 2) if top_t else "",
            })

        results.append(row)
        ob_s = f"{ob_curv:.5f}" if ob_curv is not None else "N/A"
        sb_s = f"{sb_curv:.5f}" if sb_curv is not None else "N/A"
        tt_s = f"{top_t:.1f}" if top_t is not None else "N/A"
        print(f"{pos:>12d}  {ob_s:>13s}  {sb_s:>12s}  {tt_s:>12s}  ({elapsed:.1f}s)")

    csv_path = example_dir / "E08_lay_table_results.csv"
    fieldnames = ["lay_position_m", "overbend_curvature_1_m", "sagbend_curvature_1_m", "top_tension_kN"]
    with open(csv_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for row in results:
            writer.writerow({k: row.get(k, "") for k in fieldnames})

    print(f"\n[OK]  Lay table results saved: {csv_path.name} ({len(results)} positions)")


if __name__ == "__main__":
    main()
