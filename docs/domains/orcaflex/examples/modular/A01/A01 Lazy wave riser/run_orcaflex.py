#!/usr/bin/env python3
"""
Run OrcaFlex time-domain analysis for A01 Lazy wave riser.

ABOUTME: Lazy wave flexible riser with mid-water buoyancy arch in 100m water depth.
The buoyancy section reduces dynamic top tension and decouples vessel motion from TDP.

Example features:
- Lazy wave geometry: distributed buoyancy modules creating mid-water arch
- Environment: current 0.7 m/s, Hs=6m, Tp=7s, wave_direction=90° (beam seas)
- 2-stage simulation: build-up (7s) + storm (35s)
- Key comparison with catenary: top tension reduction, arch stability

Expected outputs:
- A01_lazy_wave_riser.sim     (~50-70 MB, dynamic)
- Console: top tension, arch apex depth, TDP position

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
    # Buoyancy arch apex: minimum Z along riser (highest point of arch)
    z_profile = riser.RangeGraph("Z")
    arch_apex_depth = max(z_profile.Mean)  # highest Z = shallowest depth
    print(f"      Top tension (static): {static_top_tension:.1f} kN")
    print(f"      Arch apex depth:      {arch_apex_depth:.1f} m (above seabed reference)")

    if not run_dynamic:
        print("[INFO] Static-only run complete.")
        return

    # --- Dynamic analysis ---
    print("[INFO] Running dynamic simulation...")
    t0 = time.time()
    model.RunSimulation()
    print(f"[OK]  Dynamic complete in {time.time() - t0:.1f}s")

    tension_th = riser.TimeHistory("Effective Tension", OrcFxAPI.oeEndA)
    curvature = riser.RangeGraph("Curvature")

    print(f"      Top tension — min: {min(tension_th):.1f} kN  max: {max(tension_th):.1f} kN")
    print(f"      Max curvature: {max(curvature.Max):.4f} 1/m")

    sim_path = example_dir / "A01_lazy_wave_riser.sim"
    model.SaveSimulation(str(sim_path))
    size_mb = sim_path.stat().st_size / 1e6
    print(f"[OK]  Saved: {sim_path.name} ({size_mb:.1f} MB)")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run A01 Lazy wave riser example")
    parser.add_argument("--static", action="store_true", help="Static analysis only")
    args = parser.parse_args()
    main(run_dynamic=not args.static)
