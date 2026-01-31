"""Unified CLI for OrcaFlex format conversion.

Subcommands:
    single2modular  Convert single YAML to modular format
    modular2single  Convert modular format to single YAML
    spec2modular    Convert spec.yml to modular format
    spec2single     Convert spec.yml to single YAML
    modular2spec    Extract spec from modular format (best-effort)
    single2spec     Extract spec from single YAML (best-effort)
    detect          Detect format of a file or directory
    auto            Auto-detect source and convert to target format

Usage:
    python -m digitalmodel.modules.orcaflex.format_converter single2modular input.yml -o output_dir/
    python -m digitalmodel.modules.orcaflex.format_converter detect path/
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from .format_detector import FormatType, detect_format
from .modular_to_single import ModularToSingleConverter
from .modular_to_spec import ModularToSpecConverter
from .protocols import ConversionReport
from .single_to_modular import SingleToModularConverter
from .single_to_spec import SingleToSpecConverter
from .spec_to_modular import SpecToModularConverter
from .spec_to_single import SpecToSingleConverter


def _print_report(report: ConversionReport) -> None:
    """Print conversion report to stdout."""
    print(report.summary())
    if report.warnings:
        for w in report.warnings:
            print(f"  WARNING: {w}")
    if report.unmapped_sections:
        print(f"  Unmapped sections: {', '.join(report.unmapped_sections)}")
    if report.success:
        print(f"  Output: {report.target_path}")


def cmd_single2modular(args: argparse.Namespace) -> int:
    """Convert single YAML to modular format."""
    converter = SingleToModularConverter(
        source_path=args.source,
        output_dir=args.output,
    )
    report = converter.convert()
    _print_report(report)
    return 0 if report.success else 1


def cmd_modular2single(args: argparse.Namespace) -> int:
    """Convert modular format to single YAML."""
    converter = ModularToSingleConverter()
    report = converter.convert(source=args.source, target=args.output)
    _print_report(report)
    return 0 if report.success else 1


def cmd_spec2modular(args: argparse.Namespace) -> int:
    """Convert spec.yml to modular format."""
    converter = SpecToModularConverter()
    report = converter.convert(source=args.source, target=args.output)
    _print_report(report)
    return 0 if report.success else 1


def cmd_spec2single(args: argparse.Namespace) -> int:
    """Convert spec.yml to single YAML."""
    converter = SpecToSingleConverter()
    report = converter.convert(source=args.source, target=args.output)
    _print_report(report)
    return 0 if report.success else 1


def cmd_modular2spec(args: argparse.Namespace) -> int:
    """Extract spec from modular format."""
    converter = ModularToSpecConverter()
    report = converter.convert(source=args.source, target=args.output)
    _print_report(report)
    return 0 if report.success else 1


def cmd_single2spec(args: argparse.Namespace) -> int:
    """Extract spec from single YAML."""
    converter = SingleToSpecConverter()
    report = converter.convert(source=args.source, target=args.output)
    _print_report(report)
    return 0 if report.success else 1


def cmd_detect(args: argparse.Namespace) -> int:
    """Detect format of a file or directory."""
    try:
        fmt = detect_format(args.source)
        print(f"{fmt.value}")
        return 0
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1


def cmd_auto(args: argparse.Namespace) -> int:
    """Auto-detect source format and convert to target."""
    target_format = args.to

    try:
        source_format = detect_format(args.source)
    except ValueError as e:
        print(f"Error detecting format: {e}", file=sys.stderr)
        return 1

    conversion_key = f"{source_format.value}2{target_format}"

    converters = {
        "single2modular": lambda: SingleToModularConverter(
            args.source, args.output
        ).convert(),
        "modular2single": lambda: ModularToSingleConverter().convert(
            source=args.source, target=args.output
        ),
        "spec2modular": lambda: SpecToModularConverter().convert(
            source=args.source, target=args.output
        ),
        "spec2single": lambda: SpecToSingleConverter().convert(
            source=args.source, target=args.output
        ),
        "modular2spec": lambda: ModularToSpecConverter().convert(
            source=args.source, target=args.output
        ),
        "single2spec": lambda: SingleToSpecConverter().convert(
            source=args.source, target=args.output
        ),
    }

    if conversion_key not in converters:
        print(
            f"Error: Cannot convert {source_format.value} -> {target_format}",
            file=sys.stderr,
        )
        return 1

    print(f"Detected: {source_format.value} -> {target_format}")
    report = converters[conversion_key]()
    _print_report(report)
    return 0 if report.success else 1


def create_parser() -> argparse.ArgumentParser:
    """Create the argument parser."""
    parser = argparse.ArgumentParser(
        prog="orcaflex-convert",
        description="OrcaFlex Three-Way Format Converter",
    )
    subparsers = parser.add_subparsers(dest="command", help="Conversion command")

    def add_conversion_args(sub: argparse.ArgumentParser) -> None:
        """Add common arguments for conversion subcommands."""
        sub.add_argument("source", type=Path, help="Source file or directory")
        sub.add_argument(
            "-o", "--output", type=Path, default=None, help="Output path"
        )
        sub.add_argument(
            "--dry-run", action="store_true", help="Show what would be done"
        )
        sub.add_argument(
            "--batch",
            action="store_true",
            help="Process all matching files in directory",
        )

    # single2modular
    sub = subparsers.add_parser(
        "single2modular", help="Single YAML -> modular format"
    )
    add_conversion_args(sub)
    sub.set_defaults(func=cmd_single2modular)

    # modular2single
    sub = subparsers.add_parser(
        "modular2single", help="Modular format -> single YAML"
    )
    add_conversion_args(sub)
    sub.set_defaults(func=cmd_modular2single)

    # spec2modular
    sub = subparsers.add_parser(
        "spec2modular", help="Spec YAML -> modular format"
    )
    add_conversion_args(sub)
    sub.set_defaults(func=cmd_spec2modular)

    # spec2single
    sub = subparsers.add_parser(
        "spec2single", help="Spec YAML -> single YAML"
    )
    add_conversion_args(sub)
    sub.set_defaults(func=cmd_spec2single)

    # modular2spec
    sub = subparsers.add_parser(
        "modular2spec", help="Modular format -> spec YAML (best-effort)"
    )
    add_conversion_args(sub)
    sub.set_defaults(func=cmd_modular2spec)

    # single2spec
    sub = subparsers.add_parser(
        "single2spec", help="Single YAML -> spec YAML (best-effort)"
    )
    add_conversion_args(sub)
    sub.set_defaults(func=cmd_single2spec)

    # detect
    sub = subparsers.add_parser("detect", help="Detect file format")
    sub.add_argument("source", type=Path, help="File or directory to detect")
    sub.set_defaults(func=cmd_detect)

    # auto
    sub = subparsers.add_parser("auto", help="Auto-detect source and convert")
    sub.add_argument("source", type=Path, help="Source file or directory")
    sub.add_argument(
        "--to",
        required=True,
        choices=["single", "modular", "spec"],
        help="Target format",
    )
    sub.add_argument(
        "-o", "--output", type=Path, default=None, help="Output path"
    )
    sub.set_defaults(func=cmd_auto)

    return parser


def main(argv: list[str] | None = None) -> int:
    """CLI entry point."""
    parser = create_parser()
    args = parser.parse_args(argv)

    if not args.command:
        parser.print_help()
        return 1

    return args.func(args)
