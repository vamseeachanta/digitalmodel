#!/usr/bin/env python3
"""
Run OrcaFlex analysis for K01 5MW spar FOWT.

ABOUTME: 5MW spar floating offshore wind turbine (FOWT) with catenary mooring system.
Demonstrates coupled wind-wave-mooring analysis for offshore wind applications.

Example features:
- Spar hull with tower and RNA (Rotor-Nacelle Assembly)
- Wind loading: 15 m/s uniform wind
- Wave loading: Hs=6m, Tp=10s, wave_direction=180°
- 2-stage simulation: build-up (250s) + storm (250s) — longer stages for slow dynamics
- Mooring system: 3 catenary lines (typical spar configuration)
- Key results: spar surge/pitch, fairlead tensions, tower top displacement

Expected outputs:
- K01_spar_fowt.sim           (~100-150 MB, long dynamic)
- Console: spar offset, fairlead tensions, tower top displacement

Run with: uv run python run_orcaflex.py
Run static only: uv run python run_orcaflex.py --static

Note: Long stage durations (2×250s) make this one of the most expensive examples.
K02 (external .bts wind file) requires the binary turbulence file — not run here.
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

    vessels = [o for o in model.objects if isinstance(o, OrcFxAPI.Vessel)]
    lines = [o for o in model.objects if isinstance(o, OrcFxAPI.Line)]

    if vessels:
        spar = vessels[0]
        surge = spar.StaticResult("X")
        print(f"      Spar '{spar.name}' static surge offset: {surge:.3f} m")

    if lines:
        for line in lines:
            try:
                top_t = line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
                print(f"      Mooring '{line.name}' fairlead tension: {top_t:.1f} kN")
            except Exception:
                pass

    if not run_dynamic:
        print("[INFO] Static-only run complete.")
        return

    # --- Dynamic analysis ---
    print(f"[INFO] Running dynamic simulation (2×250s — this takes time)...")
    t0 = time.time()
    model.RunSimulation()
    print(f"[OK]  Dynamic complete in {time.time() - t0:.1f}s")

    if vessels:
        spar = vessels[0]
        surge_th = spar.TimeHistory("X")
        pitch_th = spar.TimeHistory("Ry")
        print(f"      Surge — min: {min(surge_th):.2f} m  max: {max(surge_th):.2f} m")
        print(f"      Pitch — min: {min(pitch_th):.3f}°  max: {max(pitch_th):.3f}°")

    if lines:
        for line in lines:
            try:
                th = line.TimeHistory("Effective Tension", OrcFxAPI.oeEndA)
                print(f"      '{line.name}' fairlead — min: {min(th):.1f}  max: {max(th):.1f} kN")
            except Exception:
                pass

    sim_path = example_dir / "K01_spar_fowt.sim"
    model.SaveSimulation(str(sim_path))
    size_mb = sim_path.stat().st_size / 1e6
    print(f"[OK]  Saved: {sim_path.name} ({size_mb:.1f} MB)")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run K01 5MW spar FOWT example")
    parser.add_argument("--static", action="store_true", help="Static analysis only")
    args = parser.parse_args()
    main(run_dynamic=not args.static)
