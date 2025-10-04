"""CLI Entry Point for Marine Analysis Module.

This module provides command-line interface for running marine analysis tools.

Usage:
    python -m digitalmodel.modules.marine_analysis <command> [options]

Commands:
    profile         Run performance profiling
    extract         Extract data from various sources
    validate        Run validation tests
    visualize       Generate visualizations
    analyze         Run general analysis

Examples:
    python -m digitalmodel.modules.marine_analysis profile --module wave_spectra
    python -m digitalmodel.modules.marine_analysis extract --type ocimf
    python -m digitalmodel.modules.marine_analysis validate --phase 2
    python -m digitalmodel.modules.marine_analysis visualize --type integration
"""

import sys
import argparse
from pathlib import Path


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description='Marine Analysis Module CLI',
        epilog='For detailed help on each command, use: <command> --help'
    )

    subparsers = parser.add_subparsers(dest='command', help='Available commands')

    # Profile command
    profile_parser = subparsers.add_parser('profile', help='Run performance profiling')
    profile_parser.add_argument('--module', type=str, default='all',
                               help='Module to profile (default: all)')
    profile_parser.add_argument('--output', type=str, default='tests/outputs/profiling',
                               help='Output directory for profiling results')

    # Extract command
    extract_parser = subparsers.add_parser('extract', help='Extract data from sources')
    extract_parser.add_argument('--type', type=str, required=True,
                               choices=['ocimf', 'hydro', 'mooring'],
                               help='Type of data to extract')
    extract_parser.add_argument('--input', type=str, required=True,
                               help='Input file or database path')
    extract_parser.add_argument('--output', type=str, default='tests/outputs/extraction',
                               help='Output directory for extracted data')

    # Validate command
    validate_parser = subparsers.add_parser('validate', help='Run validation tests')
    validate_parser.add_argument('--phase', type=int, choices=[1, 2, 3],
                                default=2, help='Phase to validate (default: 2)')
    validate_parser.add_argument('--output', type=str, default='tests/outputs/validation',
                                help='Output directory for validation results')

    # Visualize command
    viz_parser = subparsers.add_parser('visualize', help='Generate visualizations')
    viz_parser.add_argument('--type', type=str, required=True,
                           choices=['integration', 'ocimf', 'performance'],
                           help='Type of visualization to generate')
    viz_parser.add_argument('--input', type=str, required=True,
                           help='Input data file or directory')
    viz_parser.add_argument('--output', type=str, default='tests/outputs/visualization',
                           help='Output directory for charts')

    # Analyze command
    analyze_parser = subparsers.add_parser('analyze', help='Run general analysis')
    analyze_parser.add_argument('--input', type=str, required=True,
                               help='Input file to analyze')
    analyze_parser.add_argument('--output', type=str, default='tests/outputs/analysis',
                               help='Output directory for analysis results')

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        return 1

    # Execute command
    try:
        if args.command == 'profile':
            from .profiling import profile_modules
            print(f"Running profiling for module: {args.module}")
            print(f"Output directory: {args.output}")
            # Execute profiling
            result = profile_modules.main(['--module', args.module, '--output', args.output])
            return result

        elif args.command == 'extract':
            print(f"Extracting {args.type} data from: {args.input}")
            print(f"Output directory: {args.output}")

            if args.type == 'ocimf':
                from .extraction import extract_ocimf
                result = extract_ocimf.main(['--input', args.input, '--output', args.output])
            elif args.type == 'hydro':
                from .extraction import extract_hydro
                result = extract_hydro.main(['--input', args.input, '--output', args.output])
            elif args.type == 'mooring':
                from .extraction import extract_mooring
                result = extract_mooring.main(['--input', args.input, '--output', args.output])
            return result

        elif args.command == 'validate':
            from .validation import validate_phase2
            print(f"Running Phase {args.phase} validation")
            print(f"Output directory: {args.output}")
            result = validate_phase2.main(['--output', args.output])
            return result

        elif args.command == 'visualize':
            print(f"Generating {args.type} visualization")
            print(f"Input: {args.input}")
            print(f"Output directory: {args.output}")

            if args.type == 'integration':
                from .visualization import integration_charts
                result = integration_charts.main(['--input', args.input, '--output', args.output])
            elif args.type == 'ocimf':
                from .visualization import ocimf_charts
                result = ocimf_charts.main(['--input', args.input, '--output', args.output])
            elif args.type == 'performance':
                from .profiling import performance_charts
                result = performance_charts.main(['--input', args.input, '--output', args.output])
            return result

        elif args.command == 'analyze':
            from .analysis import excel_analyzer
            print(f"Analyzing file: {args.input}")
            print(f"Output directory: {args.output}")
            result = excel_analyzer.main(['--input', args.input, '--output', args.output])
            return result

    except Exception as e:
        print(f"Error executing command '{args.command}': {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        return 1

    return 0


if __name__ == '__main__':
    sys.exit(main())
