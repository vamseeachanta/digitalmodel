# ABOUTME: Command-line interface for pipe cross-section analysis.
# ABOUTME: Provides CLI entry point for generating reports and calculations.

"""
Pipe Cross-Section CLI
======================

Command-line interface for pipe cross-section analysis.

Usage:
    pipe-cross-section --steel-od 24 --steel-wt 0.5625 --lpp 3.5 --concrete 3.15

Examples:
    # Basic analysis with imperial units
    pipe-cross-section --steel-od-inch 24 --steel-wt-inch 0.5625

    # Full analysis with all parameters
    pipe-cross-section \\
        --steel-od 609.6 \\
        --steel-wt 14.29 \\
        --lpp 3.5 \\
        --concrete 80.0 \\
        --output-dir ./results

    # From YAML configuration
    pipe-cross-section --config pipe_config.yaml
"""

import argparse
import json
import sys
from pathlib import Path
from typing import Optional

from .calculator import PipeCrossSection
from .models import PipeCrossSectionConfig, inch_to_mm
from .visualization import PipeCrossSectionVisualizer


def parse_args(args=None):
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        prog="pipe-cross-section",
        description="Pipe cross-section analysis for offshore pipelines and risers",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )

    # Input options
    input_group = parser.add_argument_group("Input Options")
    input_group.add_argument(
        "--config", "-c",
        type=Path,
        help="YAML configuration file",
    )

    # Metric inputs
    metric_group = parser.add_argument_group("Metric Dimensions (mm)")
    metric_group.add_argument(
        "--steel-od",
        type=float,
        help="Steel pipe outer diameter (mm)",
    )
    metric_group.add_argument(
        "--steel-wt",
        type=float,
        help="Steel pipe wall thickness (mm)",
    )
    metric_group.add_argument(
        "--lpp",
        type=float,
        default=0,
        help="3LPP coating thickness (mm)",
    )
    metric_group.add_argument(
        "--concrete",
        type=float,
        default=0,
        help="Concrete coating thickness (mm)",
    )

    # Imperial inputs
    imperial_group = parser.add_argument_group("Imperial Dimensions (inches)")
    imperial_group.add_argument(
        "--steel-od-inch",
        type=float,
        help="Steel pipe outer diameter (inches)",
    )
    imperial_group.add_argument(
        "--steel-wt-inch",
        type=float,
        help="Steel pipe wall thickness (inches)",
    )
    imperial_group.add_argument(
        "--concrete-inch",
        type=float,
        default=0,
        help="Concrete coating thickness (inches)",
    )

    # Material properties
    material_group = parser.add_argument_group("Material Properties")
    material_group.add_argument(
        "--steel-density",
        type=float,
        default=7850,
        help="Steel density (kg/m³, default: 7850)",
    )
    material_group.add_argument(
        "--lpp-density",
        type=float,
        default=1100,
        help="3LPP coating density (kg/m³, default: 1100)",
    )
    material_group.add_argument(
        "--concrete-density",
        type=float,
        default=3000,
        help="Concrete density (kg/m³, default: 3000)",
    )
    material_group.add_argument(
        "--seawater-density",
        type=float,
        default=1025,
        help="Seawater density (kg/m³, default: 1025)",
    )

    # Output options
    output_group = parser.add_argument_group("Output Options")
    output_group.add_argument(
        "--output-dir", "-o",
        type=Path,
        default=Path("./output"),
        help="Output directory (default: ./output)",
    )
    output_group.add_argument(
        "--name",
        type=str,
        default="pipe_cross_section",
        help="Base name for output files",
    )
    output_group.add_argument(
        "--no-html",
        action="store_true",
        help="Skip HTML report generation",
    )
    output_group.add_argument(
        "--no-png",
        action="store_true",
        help="Skip PNG image generation",
    )
    output_group.add_argument(
        "--no-csv",
        action="store_true",
        help="Skip CSV data export",
    )
    output_group.add_argument(
        "--json",
        action="store_true",
        help="Export JSON configuration",
    )
    output_group.add_argument(
        "--quiet", "-q",
        action="store_true",
        help="Suppress console output",
    )

    return parser.parse_args(args)


def load_config_from_yaml(config_path: Path) -> dict:
    """Load configuration from YAML file."""
    try:
        import yaml
    except ImportError:
        try:
            from assetutilities.common.yml_utilities import WorkingWithYAML
            wwy = WorkingWithYAML()
            return wwy.yaml_load(str(config_path))
        except ImportError:
            raise ImportError("PyYAML is required for YAML configuration. Install with: pip install pyyaml")

    with open(config_path) as f:
        return yaml.safe_load(f)


def main(args=None):
    """Main CLI entry point."""
    args = parse_args(args)

    # Determine configuration source
    if args.config:
        # Load from YAML
        cfg = load_config_from_yaml(args.config)
        pipe = PipeCrossSection.from_dict(cfg)
    elif args.steel_od_inch and args.steel_wt_inch:
        # Imperial units
        pipe = PipeCrossSection.from_inches(
            steel_od_inch=args.steel_od_inch,
            steel_wt_inch=args.steel_wt_inch,
            lpp_thickness_mm=args.lpp,
            concrete_thickness_inch=args.concrete_inch or (args.concrete / 25.4 if args.concrete else 0),
            steel_density=args.steel_density,
            lpp_density=args.lpp_density,
            concrete_density=args.concrete_density,
            seawater_density=args.seawater_density,
        )
    elif args.steel_od and args.steel_wt:
        # Metric units
        pipe = PipeCrossSection(
            steel_od_mm=args.steel_od,
            steel_wt_mm=args.steel_wt,
            lpp_thickness_mm=args.lpp,
            concrete_thickness_mm=args.concrete,
            steel_density=args.steel_density,
            lpp_density=args.lpp_density,
            concrete_density=args.concrete_density,
            seawater_density=args.seawater_density,
        )
    else:
        print("Error: Must provide either --config, --steel-od/--steel-wt, or --steel-od-inch/--steel-wt-inch")
        sys.exit(1)

    # Print summary
    if not args.quiet:
        pipe.print_summary()

    # Create output directory
    output_dir = args.output_dir
    output_dir.mkdir(parents=True, exist_ok=True)

    # Generate outputs
    viz = PipeCrossSectionVisualizer(pipe)

    if not args.quiet:
        print("\nGenerating outputs...")

    if not args.no_html:
        html_path = output_dir / f"{args.name}_report.html"
        viz.generate_html_report(html_path)
        if not args.quiet:
            print(f"  HTML Report: {html_path}")

    if not args.no_png:
        png_path = output_dir / f"{args.name}.png"
        try:
            viz.export_static_image(png_path)
            if not args.quiet:
                print(f"  PNG Diagram: {png_path}")
        except Exception as e:
            if not args.quiet:
                print(f"  PNG export failed: {e}")

    if not args.no_csv:
        csv_path = output_dir / f"{args.name}.csv"
        viz.export_csv(csv_path)
        if not args.quiet:
            print(f"  CSV Data: {csv_path}")

    if args.json:
        json_path = output_dir / f"{args.name}.json"
        with open(json_path, "w") as f:
            json.dump(pipe.to_dict(), f, indent=2, default=str)
        if not args.quiet:
            print(f"  JSON Config: {json_path}")

    if not args.quiet:
        print("\nAnalysis complete.")

    return pipe


if __name__ == "__main__":
    main()
