#!/usr/bin/env python3
"""
ABOUTME: Standalone CLI for OrcaFlex file format conversion with integration hooks for universal runner.

OrcaFlex Conversion CLI
=======================

Simple command-line interface for converting OrcaFlex files between formats.
Can be used standalone or integrated with the universal runner.

Usage:
    # Convert single file
    python -m digitalmodel.solvers.orcaflex.convert_cli model.dat

    # Batch conversion
    python -m digitalmodel.solvers.orcaflex.convert_cli --batch models/ models_yml/

    # Specify output format
    python -m digitalmodel.solvers.orcaflex.convert_cli model.yml --format dat
"""

import argparse
import sys
from pathlib import Path
from typing import Optional

from digitalmodel.solvers.orcaflex.orcaflex_converter_enhanced import OrcaFlexConverterEnhanced


def convert_single(input_file: Path,
                   output_file: Optional[Path] = None,
                   output_format: str = 'yml',
                   mock: bool = False) -> int:
    """
    Convert a single OrcaFlex file.

    Args:
        input_file: Source file
        output_file: Destination file (optional)
        output_format: Target format ('yml' or 'dat')
        mock: Use mock mode

    Returns:
        Exit code (0 = success, 1 = failure)
    """
    converter = OrcaFlexConverterEnhanced(
        output_format=output_format,
        use_mock=mock
    )

    print(f"Converting: {input_file.name}")

    success, output, error = converter.convert_file(input_file, output_file)

    if success:
        print(f"✓ Success: {output}")
        return 0
    else:
        print(f"✗ Failed: {error}")
        return 1


def convert_batch(input_dir: Path,
                  output_dir: Path,
                  pattern: str = '*',
                  output_format: str = 'yml',
                  mock: bool = False,
                  parallel: bool = False,
                  workers: int = 4) -> int:
    """
    Convert multiple OrcaFlex files in batch.

    Args:
        input_dir: Source directory
        output_dir: Destination directory
        pattern: File pattern (e.g., '*.dat')
        output_format: Target format ('yml' or 'dat')
        mock: Use mock mode
        parallel: Enable parallel processing
        workers: Number of parallel workers

    Returns:
        Exit code (0 = success, 1 = failure)
    """
    converter = OrcaFlexConverterEnhanced(
        input_dir=input_dir,
        output_dir=output_dir,
        output_format=output_format,
        use_mock=mock,
        parallel=parallel,
        max_workers=workers
    )

    print(f"Batch conversion: {input_dir} → {output_dir}")
    print(f"Pattern: {pattern}")
    print(f"Output format: .{output_format}")

    results = converter.convert_batch(pattern=pattern)
    stats = results['statistics']

    # Display summary
    print("\n" + "="*60)
    print("BATCH CONVERSION SUMMARY")
    print("="*60)
    print(f"Total files:    {stats['total_files']}")
    print(f"Successful:     {stats['successful']}")
    print(f"Failed:         {stats['failed']}")
    print(f"Skipped:        {stats['skipped']}")
    print(f"Time:           {stats['processing_time']:.2f}s")
    print("="*60)

    if stats['failed'] > 0:
        print("\n⚠ Some files failed to convert. Check conversion_report.md for details.")
        return 1
    else:
        print("\n✓ All files converted successfully!")
        return 0


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description='OrcaFlex File Format Converter',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Convert single .dat to .yml
  %(prog)s model.dat

  # Convert single .yml to .dat
  %(prog)s model.yml --format dat

  # Batch convert directory
  %(prog)s --batch models/ models_yml/

  # Batch with pattern
  %(prog)s --batch models/ output/ --pattern "*.dat"

  # Parallel batch conversion
  %(prog)s --batch models/ output/ --parallel --workers 8

  # Mock mode (no OrcaFlex license)
  %(prog)s --batch models/ output/ --mock
        """
    )

    # Single file mode arguments
    parser.add_argument('input', nargs='?', type=Path,
                        help='Input file (single mode) or input directory (with --batch)')
    parser.add_argument('output', nargs='?', type=Path,
                        help='Output file (single mode) or output directory (with --batch)')

    # Batch mode
    parser.add_argument('--batch', '-b', action='store_true',
                        help='Batch mode: input and output are directories')
    parser.add_argument('--pattern', '-p', default='*',
                        help='File pattern for batch mode (default: *)')

    # Conversion options
    parser.add_argument('--format', '-f', choices=['yml', 'dat'], default='yml',
                        help='Output format (default: yml)')
    parser.add_argument('--mock', '-m', action='store_true',
                        help='Mock mode (no OrcaFlex license required)')

    # Parallel processing
    parser.add_argument('--parallel', action='store_true',
                        help='Enable parallel processing (batch mode only)')
    parser.add_argument('--workers', '-w', type=int, default=4,
                        help='Number of parallel workers (default: 4)')

    # Help
    parser.add_argument('--version', action='version', version='1.0.0')

    args = parser.parse_args()

    # Validate input
    if not args.input:
        parser.print_help()
        return 1

    # Batch mode
    if args.batch:
        if not args.output:
            print("Error: Output directory required for batch mode")
            parser.print_help()
            return 1

        return convert_batch(
            input_dir=args.input,
            output_dir=args.output,
            pattern=args.pattern,
            output_format=args.format,
            mock=args.mock,
            parallel=args.parallel,
            workers=args.workers
        )

    # Single file mode
    else:
        return convert_single(
            input_file=args.input,
            output_file=args.output,
            output_format=args.format,
            mock=args.mock
        )


if __name__ == "__main__":
    sys.exit(main())
