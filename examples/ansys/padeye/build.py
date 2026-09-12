#!/usr/bin/env python3
"""Generate the padeye screening APDL (padeye.inp) for the run_ansys lane case.

    uv run python examples/ansys/padeye/build.py
    uv run python examples/ansys/padeye/build.py --mesh-study-dir results/mesh-study

Edit the PadeyeGeometry below (or import and parameterise) and re-run to refresh
padeye.inp. Dispatch the solve on a licensed host via input.yml (#940/#948).
"""

from dataclasses import replace
from pathlib import Path

from digitalmodel.ansys.padeye import PadeyeGeometry, write_padeye_inp

HERE = Path(__file__).resolve().parent

# Candidate only: retained load, thicker plate; native qualification is pending.
GEOM = PadeyeGeometry(
    plate_width_mm=400.0,
    plate_height_mm=300.0,
    thickness_mm=80.0,
    hole_diameter_mm=80.0,
    sling_load_kn=500.0,
    sling_angle_deg=0.0,
    yield_strength_mpa=355.0,
    design_factor=1.67,
    element_size_mm=10.0,
)

def prepare_mesh_study(output_dir: Path | str) -> tuple[Path, Path]:
    """Write 10 mm and 5 mm decks in separate run folders; do not solve."""
    root = Path(output_dir)
    coarse = write_padeye_inp(GEOM, root / "coarse" / "padeye.inp")
    fine = write_padeye_inp(
        replace(GEOM, element_size_mm=GEOM.element_size_mm / 2.0),
        root / "fine" / "padeye.inp",
    )
    return coarse, fine


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--mesh-study-dir", type=Path)
    args = parser.parse_args()
    outputs = (prepare_mesh_study(args.mesh_study_dir) if args.mesh_study_dir
               else (write_padeye_inp(GEOM, HERE / "padeye.inp"),))
    for out in outputs:
        print(f"wrote {out}; native qualification pending")
