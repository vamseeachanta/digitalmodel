#!/usr/bin/env python3
"""
Run OrcaFlex analysis for L01 Default vessel.

ABOUTME: Default OrcaFlex vessel example demonstrating vessel motion response
in regular sea states. Used for vessel setup validation and RAO extraction.

Example features:
- Single vessel in 100m water depth
- Environment: Hs=7m, Tp=8s, wave_direction=180° (head seas)
- 2-stage simulation: build-up (8s) + storm (16s)
- Key results: vessel surge/heave/pitch RAOs, static draft

Expected outputs:
- L01_default_vessel.sim       (~50-70 MB, dynamic)
- Console: vessel draft, surge/heave/pitch range, motion summary

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

    # Find first vessel object
    vessels = [o for o in model.objects if isinstance(o, OrcFxAPI.Vessel)]
    if not vessels:
        print("[WARN] No vessel objects found in model")
        return
    vessel = vessels[0]
    static_draft = vessel.StaticResult("Z")
    print(f"      Vessel '{vessel.name}' static Z: {static_draft:.3f} m")

    if not run_dynamic:
        print("[INFO] Static-only run complete.")
        return

    # --- Dynamic analysis ---
    print("[INFO] Running dynamic simulation...")
    t0 = time.time()
    model.RunSimulation()
    print(f"[OK]  Dynamic complete in {time.time() - t0:.1f}s")

    # Vessel motion summary
    for dof, label in [("X", "Surge"), ("Z", "Heave"), ("Ry", "Pitch")]:
        try:
            th = vessel.TimeHistory(dof)
            print(f"      {label:6s} — min: {min(th):7.3f}  max: {max(th):7.3f}")
        except Exception:
            pass

    sim_path = example_dir / "L01_default_vessel.sim"
    model.SaveSimulation(str(sim_path))
    size_mb = sim_path.stat().st_size / 1e6
    print(f"[OK]  Saved: {sim_path.name} ({size_mb:.1f} MB)")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run L01 Default vessel example")
    parser.add_argument("--static", action="store_true", help="Static analysis only")
    args = parser.parse_args()
    main(run_dynamic=not args.static)
