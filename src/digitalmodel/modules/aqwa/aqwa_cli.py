#!/usr/bin/env python3
"""
AQWA Analysis CLI - Command Line Interface

Simple command-line interface for running AQWA diffraction analysis workflows.
Supports RAO extraction, damping analysis, and external force server.
"""

import argparse
import sys
import yaml
from pathlib import Path
from digitalmodel.modules.aqwa.aqwa_analysis import AqwaAnalysis


def create_parser():
    """Create command line argument parser"""

    parser = argparse.ArgumentParser(
        description='AQWA Diffraction Analysis CLI',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Extract RAOs from AQWA analysis
  python aqwa_cli.py --method raos --folder C:/AQWA/MyProject --name vessel

  # Run damping analysis
  python aqwa_cli.py --method damping --folder C:/AQWA/MyProject

  # Use configuration file
  python aqwa_cli.py --config my_analysis.yml

  # List available methods
  python aqwa_cli.py --list-methods
        """
    )

    # Analysis method
    parser.add_argument(
        '--method',
        choices=['raos', 'rao', 'damping', 'viscous_damping', 'ef_server', 'external_forces'],
        help='Analysis method to run'
    )

    # Folder and file settings
    parser.add_argument(
        '--folder',
        help='Analysis root folder path'
    )

    parser.add_argument(
        '--name',
        help='Model/file name (without extension)'
    )

    # Configuration file
    parser.add_argument(
        '--config',
        help='Path to YAML configuration file'
    )

    # Information commands
    parser.add_argument(
        '--list-methods',
        action='store_true',
        help='List all available analysis methods'
    )

    # Output options
    parser.add_argument(
        '--output',
        help='Output directory for results (optional)'
    )

    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Enable verbose output'
    )

    return parser


def list_methods():
    """Display available analysis methods"""

    methods = {
        'RAO Analysis': {
            'methods': ['raos', 'rao'],
            'description': 'Extract Response Amplitude Operators from AQWA results',
            'requirements': 'AQWA .LIS file with hydrodynamic analysis'
        },
        'Damping Analysis': {
            'methods': ['damping', 'viscous_damping'],
            'description': 'Compute hydrodynamic damping matrices',
            'requirements': 'AQWA analysis results with velocity data'
        },
        'External Force Server': {
            'methods': ['ef_server', 'external_forces', 'external_forces_server'],
            'description': 'Launch AQWA EF server for coupled analysis',
            'requirements': 'AQWA model and communication port configuration'
        }
    }

    print("\nAvailable AQWA Analysis Methods:")
    print("=" * 70)

    for category, info in methods.items():
        print(f"\n{category}")
        print("-" * 70)
        print(f"Methods: {', '.join(info['methods'])}")
        print(f"Description: {info['description']}")
        print(f"Requirements: {info['requirements']}")

    print("\n" + "=" * 70)
    print("\nUsage Examples:")
    print("  python aqwa_cli.py --method raos --folder C:/AQWA/Project --name vessel")
    print("  python aqwa_cli.py --method damping --folder C:/AQWA/Project")
    print("  python aqwa_cli.py --config analysis_config.yml\n")


def load_config_file(config_path: str) -> dict:
    """Load configuration from YAML file"""

    config_file = Path(config_path)

    if not config_file.exists():
        print(f"Error: Configuration file not found: {config_path}")
        sys.exit(1)

    try:
        with open(config_file, 'r') as f:
            config = yaml.safe_load(f)
        return config
    except Exception as e:
        print(f"Error loading configuration file: {e}")
        sys.exit(1)


def build_config_from_args(args) -> dict:
    """Build configuration dictionary from command line arguments"""

    if not args.method:
        print("Error: --method is required when not using --config")
        sys.exit(1)

    if not args.folder:
        print("Error: --folder is required when not using --config")
        sys.exit(1)

    config = {
        'Analysis': {
            'analysis_root_folder': args.folder
        },
        'analysis_settings': {
            'method': args.method
        }
    }

    # Add optional file name
    if args.name:
        config['Analysis']['file_name'] = args.name

    # Add output directory if specified
    if args.output:
        config['Analysis']['output_folder'] = args.output

    return config


def run_analysis(config: dict, verbose: bool = False):
    """Execute AQWA analysis with given configuration"""

    if verbose:
        print("\n" + "=" * 70)
        print("AQWA Analysis Configuration:")
        print("=" * 70)
        print(yaml.dump(config, default_flow_style=False))
        print("=" * 70 + "\n")

    # Validate required fields
    if 'Analysis' not in config or 'analysis_root_folder' not in config['Analysis']:
        print("Error: Configuration missing 'Analysis.analysis_root_folder'")
        sys.exit(1)

    if 'analysis_settings' not in config or 'method' not in config['analysis_settings']:
        print("Error: Configuration missing 'analysis_settings.method'")
        sys.exit(1)

    # Verify analysis folder exists
    analysis_folder = Path(config['Analysis']['analysis_root_folder'])
    if not analysis_folder.exists():
        print(f"Error: Analysis folder not found: {analysis_folder}")
        sys.exit(1)

    try:
        # Run analysis
        analyzer = AqwaAnalysis()

        if verbose:
            print(f"Starting {config['analysis_settings']['method']} analysis...")
            print(f"Analysis folder: {analysis_folder}\n")

        results = analyzer.analysis_router(config)

        # Display results
        print("\n" + "=" * 70)
        print("Analysis Complete!")
        print("=" * 70)
        print(f"Status: {results['results']['status']}")
        print(f"Method: {results['results']['method']}")

        if verbose and 'results' in results:
            print("\nDetailed Results:")
            for key, value in results['results'].items():
                if key not in ['status', 'method']:
                    print(f"  {key}: {value}")

        print("=" * 70 + "\n")

        return results

    except ValueError as e:
        print(f"\nError: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"\nUnexpected error: {e}")
        if verbose:
            import traceback
            traceback.print_exc()
        sys.exit(1)


def main():
    """Main CLI entry point"""

    parser = create_parser()
    args = parser.parse_args()

    # Handle information commands
    if args.list_methods:
        list_methods()
        return

    # Check if any analysis command provided
    if not args.config and not args.method:
        parser.print_help()
        print("\nError: Either --config or --method is required")
        sys.exit(1)

    # Build configuration
    if args.config:
        config = load_config_file(args.config)
    else:
        config = build_config_from_args(args)

    # Run analysis
    run_analysis(config, verbose=args.verbose)


if __name__ == '__main__':
    main()
