"""
ABOUTME: CLI script to generate FreeCAD model and STEP export
ABOUTME: Usage: PYTHONPATH=src freecadcmd src/.../generate_frame_cad.py

Run from the digitalmodel repo root with PYTHONPATH=src set.
freecadcmd does not pass CLI args to scripts, so configuration
is via environment variables or defaults.

Environment variables:
    OUTPUT_DIR  — output directory (default: current dir)
    TUBE_OD     — tube outer diameter, inches (default: 1.75)
    TUBE_WALL   — tube wall thickness, inches (default: 0.120)
    BEND_CLR    — bend centerline radius, inches (default: 5.25)
"""
import os
import sys
from pathlib import Path


def main():
    output_dir = os.environ.get("OUTPUT_DIR", ".")
    tube_od = float(os.environ.get("TUBE_OD", "1.75"))
    tube_wall = float(os.environ.get("TUBE_WALL", "0.120"))
    bend_clr = float(os.environ.get("BEND_CLR", "5.25"))

    from digitalmodel.structural.parachute.frame_geometry_3d import (
        build_gt1r_frame_3d,
    )
    from digitalmodel.structural.parachute.freecad_frame_builder import (
        build_freecad_model,
        export_step,
    )
    import FreeCAD

    out = Path(output_dir)
    out.mkdir(parents=True, exist_ok=True)

    geo = build_gt1r_frame_3d(
        tube_od=tube_od, tube_wall=tube_wall, bend_clr=bend_clr
    )
    doc = build_freecad_model(geo)

    step_path = str(out / "gt1r_parachute_frame.step")
    export_step(doc, step_path)
    print(f"STEP exported: {step_path}")

    fcstd_path = str(out / "gt1r_parachute_frame.FCStd")
    doc.saveAs(fcstd_path)
    print(f"FreeCAD file: {fcstd_path}")

    FreeCAD.closeDocument(doc.Name)
    return 0


if __name__ == "__main__":
    sys.exit(main())
