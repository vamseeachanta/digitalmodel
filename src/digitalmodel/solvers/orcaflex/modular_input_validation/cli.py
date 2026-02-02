"""
ABOUTME: Command-line interface for modular input validation
ABOUTME: Provides CLI for validating OrcaFlex YAML files with multiple levels
"""

import argparse
import sys
from pathlib import Path
from typing import List, Optional

from .validator import ModularInputValidator
from .config import ValidationConfig


VERSION = "1.0.0"


def create_parser() -> argparse.ArgumentParser:
    """
    Create argument parser for CLI.

    Returns:
        Configured ArgumentParser
    """
    parser = argparse.ArgumentParser(
        prog="orcaflex-validate",
        description="Validate OrcaFlex modular YAML input files",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Validate a single file
  python -m digitalmodel.orcaflex.modular_input_validation.cli file.yml

  # Validate all YAML files in a directory
  python -m digitalmodel.orcaflex.modular_input_validation.cli path/to/files/

  # Run only Level 1 and Level 2 validation
  python -m digitalmodel.orcaflex.modular_input_validation.cli file.yml --skip-level 3

  # Customize tolerance and output formats
  python -m digitalmodel.orcaflex.modular_input_validation.cli file.yml --tolerance 15 --formats csv markdown html

  # Validate with custom data directory
  python -m digitalmodel.orcaflex.modular_input_validation.cli file.yml --data-dir /path/to/data
        """
    )

    # Version
    parser.add_argument(
        '--version',
        action='version',
        version=f'%(prog)s {VERSION}'
    )

    # Input paths (positional)
    parser.add_argument(
        'paths',
        nargs='+',
        help='YAML file(s) or directory to validate'
    )

    # Validation levels
    level_group = parser.add_argument_group('Validation Levels')
    level_group.add_argument(
        '--skip-level',
        type=int,
        action='append',
        choices=[1, 2, 3],
        help='Skip specific validation level (can be used multiple times)'
    )
    level_group.add_argument(
        '--level-1-only',
        action='store_true',
        help='Run only Level 1 (YAML syntax) validation'
    )
    level_group.add_argument(
        '--level-2-only',
        action='store_true',
        help='Run only Level 2 (OrcaFlex API) validation'
    )
    level_group.add_argument(
        '--level-3-only',
        action='store_true',
        help='Run only Level 3 (Physical consistency) validation'
    )

    # Configuration
    config_group = parser.add_argument_group('Configuration')
    config_group.add_argument(
        '--tolerance',
        type=float,
        default=10.0,
        help='Tolerance percentage for physical parameter validation (default: 10.0)'
    )
    config_group.add_argument(
        '--data-dir',
        type=Path,
        default=Path('data'),
        help='Root directory for CALM buoy reference data (default: data/)'
    )
    config_group.add_argument(
        '--no-orcaflex',
        action='store_true',
        help='Skip OrcaFlex API validation (useful when OrcaFlex not installed)'
    )

    # Reporting
    report_group = parser.add_argument_group('Reporting')
    report_group.add_argument(
        '--no-reports',
        action='store_true',
        help='Disable report generation'
    )
    report_group.add_argument(
        '--formats',
        nargs='+',
        choices=['console', 'csv', 'markdown', 'html'],
        default=['console', 'csv', 'markdown', 'html'],
        help='Report formats to generate (default: all)'
    )
    report_group.add_argument(
        '--reports-dir',
        type=Path,
        default=Path('reports/validation/calm_buoy'),
        help='Directory for validation reports'
    )
    report_group.add_argument(
        '--no-color',
        action='store_true',
        help='Disable colored console output'
    )

    # File matching
    file_group = parser.add_argument_group('File Matching')
    file_group.add_argument(
        '--pattern',
        type=str,
        default='*.yml',
        help='Glob pattern for file matching when validating directories (default: *.yml)'
    )

    return parser


def resolve_paths(paths: List[str]) -> List[Path]:
    """
    Resolve input paths to list of files.

    Args:
        paths: List of file or directory paths

    Returns:
        List of resolved file paths
    """
    resolved_files = []

    for path_str in paths:
        path = Path(path_str)

        if not path.exists():
            print(f"Warning: Path not found: {path}", file=sys.stderr)
            continue

        if path.is_file():
            resolved_files.append(path)
        elif path.is_dir():
            # Will be handled by validator's validate_directory
            resolved_files.append(path)
        else:
            print(f"Warning: Invalid path: {path}", file=sys.stderr)

    return resolved_files


def create_config_from_args(args: argparse.Namespace) -> ValidationConfig:
    """
    Create ValidationConfig from command-line arguments.

    Args:
        args: Parsed arguments

    Returns:
        ValidationConfig instance
    """
    # Determine skip levels
    skip_levels = []

    if args.skip_level:
        skip_levels.extend(args.skip_level)

    if args.level_1_only:
        skip_levels.extend([2, 3])
    elif args.level_2_only:
        skip_levels.extend([1, 3])
    elif args.level_3_only:
        skip_levels.extend([1, 2])

    # Remove duplicates
    skip_levels = list(set(skip_levels))

    # Create config
    config = ValidationConfig(
        tolerance_percent=args.tolerance,
        enable_orcaflex=not args.no_orcaflex,
        calm_buoy_data_dir=args.data_dir,
        reports_dir=args.reports_dir,
        skip_levels=skip_levels,
        generate_reports=not args.no_reports,
        report_formats=args.formats,
        enable_color=not args.no_color
    )

    return config


def main(argv: Optional[List[str]] = None) -> int:
    """
    Main CLI entry point.

    Args:
        argv: Command-line arguments (uses sys.argv if None)

    Returns:
        Exit code (0 for success, 1 for failure)
    """
    parser = create_parser()
    args = parser.parse_args(argv)

    # Resolve input paths
    paths = resolve_paths(args.paths)

    if not paths:
        print("Error: No valid paths provided", file=sys.stderr)
        return 1

    # Create configuration
    config = create_config_from_args(args)

    # Create validator
    validator = ModularInputValidator(config)

    # Print configuration summary
    print(f"OrcaFlex Modular Input Validation v{VERSION}")
    print("=" * 80)
    print(f"Validation levels: {[i for i in [1, 2, 3] if i not in config.skip_levels]}")
    print(f"Tolerance: ±{config.tolerance_percent}%")
    print(f"OrcaFlex enabled: {config.enable_orcaflex}")
    print(f"Report formats: {', '.join(config.report_formats)}")
    print(f"Reports directory: {config.reports_dir}")
    print("=" * 80)
    print()

    # Run validation
    try:
        all_results = []

        for path in paths:
            if path.is_file():
                results = [validator.validate_file(path)]
            elif path.is_dir():
                results = validator.validate_directory(path, pattern=args.pattern)
            else:
                continue

            all_results.extend(results)

        if not all_results:
            print("No files were validated", file=sys.stderr)
            return 1

        # Generate reports
        if config.generate_reports:
            validator.generate_reports(all_results)

        # Get summary
        summary = validator.get_validation_summary(all_results)

        # Print summary
        print()
        print("=" * 80)
        print("VALIDATION SUMMARY")
        print("=" * 80)
        print(f"Total files: {summary['total_files']}")
        print(f"Passed: {summary['passed']}")
        print(f"Warnings: {summary['warnings']}")
        print(f"Failed: {summary['failed']}")
        print(f"Pass rate: {summary['pass_rate']:.1f}%")
        print()

        # Determine exit code
        if summary['failed'] > 0:
            print("❌ VALIDATION FAILED")
            return 1
        elif summary['warnings'] > 0:
            print("⚠️ VALIDATION PASSED WITH WARNINGS")
            return 0
        else:
            print("✅ VALIDATION PASSED")
            return 0

    except KeyboardInterrupt:
        print("\nValidation interrupted by user", file=sys.stderr)
        return 1
    except Exception as e:
        print(f"Error during validation: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())
