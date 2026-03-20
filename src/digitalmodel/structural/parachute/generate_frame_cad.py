"""
ABOUTME: CLI script to generate FreeCAD model and STEP export
ABOUTME: Usage: freecadcmd generate_frame_cad.py [--output-dir DIR]
"""
import argparse
import sys
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(
        description="Generate GT1R parachute frame FreeCAD model"
    )
    parser.add_argument(
        "--output-dir", default=".", help="Output directory for STEP/FCStd"
    )
    parser.add_argument(
        "--bar-od", type=float, default=1.5, help="Bar tube OD (inches)"
    )
    parser.add_argument(
        "--bar-wall", type=float, default=0.120, help="Bar wall thickness"
    )
    args = parser.parse_args()

    from digitalmodel.structural.parachute.frame_geometry_3d import (
        build_gt1r_frame_3d,
    )
    from digitalmodel.structural.parachute.freecad_frame_builder import (
        build_freecad_model,
        export_step,
    )
    import FreeCAD

    out = Path(args.output_dir)
    out.mkdir(parents=True, exist_ok=True)

    geo = build_gt1r_frame_3d(bar_od=args.bar_od, bar_wall=args.bar_wall)
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
