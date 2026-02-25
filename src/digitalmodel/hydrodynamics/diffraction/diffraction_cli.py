#!/usr/bin/env python3
"""
Unified Diffraction Analysis CLI

Single entry point for both AQWA and OrcaWave diffraction analysis workflows.
Simplifies execution and provides consistent interface across tools.
"""

import argparse
import sys
import yaml
from pathlib import Path


def create_parser():
    """Create unified CLI argument parser"""

    parser = argparse.ArgumentParser(
        description='Unified Diffraction Analysis CLI (AQWA + OrcaWave)',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # List available tools
  python diffraction_cli.py --list-tools

  # AQWA RAO extraction
  python diffraction_cli.py aqwa --method raos --folder C:/AQWA/Vessel --name vessel

  # OrcaWave analysis
  python diffraction_cli.py orcawave --vessel sea_cypress

  # Use configuration file
  python diffraction_cli.py aqwa --config analysis.yml
  python diffraction_cli.py orcawave --config diffraction.yml

  # Dry run (validation only)
  python diffraction_cli.py orcawave --vessel sea_cypress --dry-run
        """
    )

    # Tool selection
    parser.add_argument(
        'tool',
        nargs='?',
        choices=['aqwa', 'orcawave'],
        help='Analysis tool to use'
    )

    # Information commands
    parser.add_argument(
        '--list-tools',
        action='store_true',
        help='List available analysis tools and capabilities'
    )

    # Common options
    parser.add_argument(
        '--config',
        help='Path to YAML configuration file'
    )

    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Enable verbose output'
    )

    # AQWA-specific options
    aqwa_group = parser.add_argument_group('AQWA Options')
    aqwa_group.add_argument(
        '--method',
        choices=['raos', 'rao', 'damping', 'viscous_damping', 'ef_server', 'external_forces'],
        help='AQWA analysis method'
    )

    aqwa_group.add_argument(
        '--folder',
        help='AQWA analysis root folder'
    )

    aqwa_group.add_argument(
        '--name',
        help='AQWA model/file name'
    )

    # OrcaWave-specific options
    orcawave_group = parser.add_argument_group('OrcaWave Options')
    orcawave_group.add_argument(
        '--vessel',
        help='Vessel name for OrcaWave analysis'
    )

    orcawave_group.add_argument(
        '--phase',
        choices=['setup', 'execute', 'process', 'qa', 'package'],
        help='Specific workflow phase to run'
    )

    orcawave_group.add_argument(
        '--dry-run',
        action='store_true',
        help='Validate configuration without running analysis'
    )

    orcawave_group.add_argument(
        '--list-vessels',
        action='store_true',
        help='List available vessel configurations'
    )

    return parser


def list_tools():
    """Display available diffraction analysis tools"""

    tools = {
        'AQWA': {
            'description': 'ANSYS AQWA hydrodynamic diffraction analysis',
            'capabilities': [
                'RAO extraction (Response Amplitude Operators)',
                'Hydrodynamic damping computation',
                'External force server for coupled analysis',
                'Multi-body interaction analysis',
                'Both Workbench and DAT file workflows'
            ],
            'methods': ['raos', 'damping', 'ef_server'],
            'quick_start': 'docs/domains/aqwa/QUICK_START.md'
        },
        'OrcaWave': {
            'description': 'Orcina OrcaWave panel method diffraction analysis',
            'capabilities': [
                'Geometry validation and quality checks',
                'Automated diffraction coefficient generation',
                'Direct export to OrcaFlex vessel types',
                'Multi-vessel batch processing',
                'Comprehensive QA and validation reports'
            ],
            'phases': ['setup', 'execute', 'process', 'qa', 'package'],
            'quick_start': 'src/digitalmodel/modules/orcawave/diffraction/QUICK_START.md'
        }
    }

    print("\n" + "=" * 80)
    print("AVAILABLE DIFFRACTION ANALYSIS TOOLS")
    print("=" * 80)

    for tool_name, info in tools.items():
        print(f"\n{tool_name}")
        print("-" * 80)
        print(f"Description: {info['description']}")
        print(f"\nCapabilities:")
        for cap in info['capabilities']:
            print(f"  - {cap}")

        if 'methods' in info:
            print(f"\nMethods: {', '.join(info['methods'])}")
        if 'phases' in info:
            print(f"\nPhases: {', '.join(info['phases'])}")

        print(f"\nQuick Start: {info['quick_start']}")

    print("\n" + "=" * 80)
    print("\nUsage:")
    print("  python diffraction_cli.py aqwa --method raos --folder <path> --name <model>")
    print("  python diffraction_cli.py orcawave --vessel <vessel_name>")
    print("\n" + "=" * 80 + "\n")


def run_aqwa(args):
    """Execute AQWA analysis"""

    from digitalmodel.aqwa.aqwa_analysis import AqwaAnalysis

    # Build configuration
    if args.config:
        config_file = Path(args.config)
        if not config_file.exists():
            print(f"Error: Configuration file not found: {args.config}")
            sys.exit(1)

        with open(config_file, 'r') as f:
            config = yaml.safe_load(f)
    else:
        if not args.method or not args.folder:
            print("Error: --method and --folder are required for AQWA analysis")
            print("       (or use --config to specify configuration file)")
            sys.exit(1)

        config = {
            'Analysis': {
                'analysis_root_folder': args.folder
            },
            'analysis_settings': {
                'method': args.method
            }
        }

        if args.name:
            config['Analysis']['file_name'] = args.name

    if args.verbose:
        print("\n" + "=" * 80)
        print("AQWA Analysis Configuration:")
        print("=" * 80)
        print(yaml.dump(config, default_flow_style=False))
        print("=" * 80 + "\n")

    # Run analysis
    try:
        analyzer = AqwaAnalysis()

        if args.verbose:
            print(f"Starting {config['analysis_settings']['method']} analysis...\n")

        results = analyzer.analysis_router(config)

        print("\n" + "=" * 80)
        print("AQWA Analysis Complete!")
        print("=" * 80)
        print(f"Status: {results['results']['status']}")
        print(f"Method: {results['results']['method']}")
        print("=" * 80 + "\n")

    except Exception as e:
        print(f"\nError running AQWA analysis: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        sys.exit(1)


def run_orcawave(args):
    """Execute OrcaWave analysis"""

    # OrcaWave orchestrator imports
    try:
        from digitalmodel.orcawave.diffraction.orchestrator import OrcaWaveOrchestrator
    except ImportError:
        print("Error: OrcaWave module not found")
        print("Ensure repository is properly set up and dependencies installed")
        sys.exit(1)

    # Handle list vessels command
    if args.list_vessels:
        vessel_configs = Path("src/digitalmodel/modules/orcawave/diffraction/configs/vessels")
        if vessel_configs.exists():
            vessels = [f.stem for f in vessel_configs.glob("*.yml")]
            print("\nAvailable Vessels:")
            for v in vessels:
                print(f"  - {v}")
            print()
        else:
            print("No vessel configurations found")
        return

    # Validate vessel or config
    if not args.vessel and not args.config:
        print("Error: --vessel or --config is required for OrcaWave analysis")
        sys.exit(1)

    # Load configuration
    if args.config:
        config_file = Path(args.config)
        if not config_file.exists():
            print(f"Error: Configuration file not found: {args.config}")
            sys.exit(1)

        with open(config_file, 'r') as f:
            config = yaml.safe_load(f)

        vessel_name = config.get('vessel', {}).get('name', 'unknown')
    else:
        vessel_name = args.vessel
        config = None

    if args.verbose:
        print("\n" + "=" * 80)
        print(f"OrcaWave Analysis: {vessel_name}")
        print("=" * 80)
        if args.dry_run:
            print("MODE: Dry run (validation only)")
        if args.phase:
            print(f"PHASE: {args.phase}")
        print("=" * 80 + "\n")

    # Run analysis
    try:
        orchestrator = OrcaWaveOrchestrator(
            vessel_name=vessel_name,
            config_path=args.config
        )

        if args.dry_run:
            print("Validating configuration...")
            # Run phase 1 (setup and validation) only
            orchestrator.run_workflow(max_phase=1)
            print("\nConfiguration valid!")
        elif args.phase:
            print(f"Running phase: {args.phase}")
            phase_map = {
                'setup': 1,
                'execute': 2,
                'process': 3,
                'qa': 4,
                'package': 5
            }
            max_phase = phase_map.get(args.phase, 5)
            orchestrator.run_workflow(max_phase=max_phase)
        else:
            print("Running complete OrcaWave workflow...")
            orchestrator.run_workflow()

        print("\n" + "=" * 80)
        print("OrcaWave Analysis Complete!")
        print("=" * 80 + "\n")

    except Exception as e:
        print(f"\nError running OrcaWave analysis: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        sys.exit(1)


def main():
    """Main CLI entry point"""

    parser = create_parser()
    args = parser.parse_args()

    # Handle information commands
    if args.list_tools:
        list_tools()
        return

    # Require tool selection
    if not args.tool:
        parser.print_help()
        print("\nError: Tool selection required (aqwa or orcawave)")
        print("       Use --list-tools to see available tools")
        sys.exit(1)

    # Route to appropriate tool
    if args.tool == 'aqwa':
        run_aqwa(args)
    elif args.tool == 'orcawave':
        run_orcawave(args)


if __name__ == '__main__':
    main()
