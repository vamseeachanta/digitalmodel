#!/usr/bin/env python3
"""Generate the padeye screening APDL (padeye.inp) for the run_ansys lane case.

    uv run python examples/ansys/padeye/build.py

Edit the PadeyeGeometry below (or import and parameterise) and re-run to refresh
padeye.inp. Dispatch the solve on a licensed host via input.yml (#940/#948).
"""

from pathlib import Path

from digitalmodel.ansys.padeye import PadeyeGeometry, write_padeye_inp

HERE = Path(__file__).resolve().parent

GEOM = PadeyeGeometry(
    plate_width_mm=400.0,
    plate_height_mm=300.0,
    thickness_mm=40.0,
    hole_diameter_mm=80.0,
    sling_load_kn=500.0,
    sling_angle_deg=0.0,
    yield_strength_mpa=355.0,
    design_factor=1.67,
    element_size_mm=10.0,
)

if __name__ == "__main__":
    out = write_padeye_inp(GEOM, HERE / "padeye.inp")
    print(f"wrote {out}")
