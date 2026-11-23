#!/usr/bin/env python3
# ABOUTME: CLI wrapper for field development schematic generator (WRK-192)
"""
generate_schematic.py — Command-line interface for field development schematics.

Usage examples
--------------
# Basic subsea tieback (FPSO host, 2 templates, 3 wells each, 300 m water depth)
python scripts/generate_schematic.py \
    --name "Example Field" \
    --water-depth 300 \
    --templates 2 \
    --wells 3 \
    --host FPSO \
    --flowline 8.0 \
    --format png \
    --output outputs/example_field.png

# Fixed platform
python scripts/generate_schematic.py \
    --name "Platform Alpha" \
    --water-depth 80 \
    --templates 0 \
    --wells 6 \
    --host fixed \
    --format svg \
    --output outputs/platform_alpha.svg

# FPSO spread mooring
python scripts/generate_schematic.py \
    --name "Spread FPSO" \
    --water-depth 600 \
    --templates 2 \
    --wells 4 \
    --host FPSO \
    --dev-type fpso_spread \
    --mooring-lines 8 \
    --format png \
    --output outputs/spread_fpso.png
"""

import argparse
import sys
from pathlib import Path

# Make sure the package is importable when running from digitalmodel root
sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from digitalmodel.field_development.schematic_generator import generate_field_schematic


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Generate a field development schematic (SVG/PNG).",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    p.add_argument("--name", required=True, help="Field name")
    p.add_argument(
        "--water-depth", required=True, type=float,
        dest="water_depth_m", metavar="METRES",
        help="Water depth in metres",
    )
    p.add_argument(
        "--templates", type=int, default=2,
        dest="n_templates", metavar="N",
        help="Number of subsea templates (0 = platform stand-alone)",
    )
    p.add_argument(
        "--wells", type=int, default=3,
        dest="n_wells", metavar="N",
        help="Wells per template (tieback) or total conductor wells (platform)",
    )
    p.add_argument(
        "--host", default="FPSO",
        dest="host_type",
        choices=["FPSO", "TLP", "SPAR", "fixed"],
        help="Host facility type",
    )
    p.add_argument(
        "--flowline", type=float, default=5.0,
        dest="flowline_length_km", metavar="KM",
        help="Flowline length in km",
    )
    p.add_argument(
        "--format", default="png",
        dest="output_format",
        choices=["svg", "png"],
        help="Output format",
    )
    p.add_argument(
        "--output", required=True,
        dest="output_path",
        help="Output file path",
    )
    p.add_argument(
        "--dev-type",
        dest="development_type",
        choices=["subsea_tieback", "platform", "fpso_spread"],
        default=None,
        help="Override development type (inferred if omitted)",
    )
    p.add_argument(
        "--mooring-lines", type=int, default=8,
        dest="n_mooring_lines", metavar="N",
        help="Number of mooring lines (fpso_spread only)",
    )
    p.add_argument(
        "--satellite-wells", type=int, default=0,
        dest="n_satellite_wells", metavar="N",
        help="Number of satellite subsea wells (platform type only)",
    )
    p.add_argument(
        "--satellite-tieback", type=float, default=3.0,
        dest="satellite_tieback_km", metavar="KM",
        help="Satellite tieback distance in km",
    )
    return p


def main(argv=None):
    parser = build_parser()
    args = parser.parse_args(argv)

    config = {
        "name": args.name,
        "water_depth_m": args.water_depth_m,
        "n_templates": args.n_templates,
        "n_wells": args.n_wells,
        "host_type": args.host_type,
        "flowline_length_km": args.flowline_length_km,
        "output_format": args.output_format,
        "output_path": args.output_path,
        "n_mooring_lines": args.n_mooring_lines,
        "n_satellite_wells": args.n_satellite_wells,
        "satellite_tieback_km": args.satellite_tieback_km,
    }
    if args.development_type is not None:
        config["development_type"] = args.development_type

    try:
        result = generate_field_schematic(config)
        print(f"Schematic written to: {result}")
        return 0
    except (KeyError, ValueError) as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
