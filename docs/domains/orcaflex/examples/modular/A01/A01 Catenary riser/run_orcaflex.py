#!/usr/bin/env python3
"""
Run OrcaFlex time-domain analysis for A01 Catenary riser.

ABOUTME: Single flexible catenary riser from seabed touchdown to a floating vessel.
Demonstrates baseline riser analysis: static equilibrium + dynamic storm loading.

Example features:
- Catenary riser geometry in 100m water depth
- Environment: current 0.7 m/s, Hs=6m, Tp=7s, wave_direction=180°
- 2-stage simulation: build-up (7s) + storm (35s)
- Modular YAML structure with 7 include files
- Key results: top tension, TDP tension, maximum curvature

Expected outputs:
- A01_catenary_riser.sim      (~50-70 MB, dynamic)
- Console: top tension range, TDP position, max curvature

Run with: uv run python run_orcaflex.py
Run static only: uv run python run_orcaflex.py --static
"""
import sys
import os
import time
import argparse
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


def main(run_dynamic: bool = True) -> None:
    _load_orcfxapi()
    import OrcFxAPI

    example_dir = Path(__file__).parent
    model_file = example_dir / "master.yml"

    if not model_file.exists():
        print(f"[ERROR] Model file not found: {model_file}")
        sys.exit(1)

    print(f"[INFO] Loading model: {model_file}")
    model = OrcFxAPI.Model(str(model_file))
    print(f"[OK]  Model loaded — {len(list(model.objects))} objects")

    # --- Static analysis ---
    print("[INFO] Running static analysis...")
    t0 = time.time()
    model.CalculateStatics()
    print(f"[OK]  Static complete in {time.time() - t0:.1f}s")

    riser = model["Line1"]
    static_top_tension = riser.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
    static_tdp = riser.StaticResult("Z", OrcFxAPI.oeEndB)
    print(f"      Top tension (static): {static_top_tension:.1f} kN")
    print(f"      TDP depth (static):   {static_tdp:.2f} m")

    if not run_dynamic:
        print("[INFO] Static-only run complete.")
        return

    # --- Dynamic analysis ---
    print("[INFO] Running dynamic simulation...")
    t0 = time.time()
    model.RunSimulation()
    elapsed = time.time() - t0
    print(f"[OK]  Dynamic complete in {elapsed:.1f}s")

    # Key dynamic results
    tension_th = riser.TimeHistory("Effective Tension", OrcFxAPI.oeEndA)
    curvature = riser.RangeGraph("Curvature")

    print(f"      Top tension — min: {min(tension_th):.1f} kN  max: {max(tension_th):.1f} kN")
    print(f"      Max curvature along riser: {max(curvature.Max):.4f} 1/m")

    # Save simulation
    sim_path = example_dir / "A01_catenary_riser.sim"
    model.SaveSimulation(str(sim_path))
    size_mb = sim_path.stat().st_size / 1e6
    print(f"[OK]  Saved: {sim_path.name} ({size_mb:.1f} MB)")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run A01 Catenary riser example")
    parser.add_argument("--static", action="store_true", help="Static analysis only")
    args = parser.parse_args()
    main(run_dynamic=not args.static)
