#!/usr/bin/env python3
# ABOUTME: CLI wrapper for agent health monitoring dashboard
# Provides generate and view commands for dashboard operations
"""
Agent Dashboard CLI

Simple command-line interface for generating and viewing agent health dashboards.

Usage:
    python agent_dashboard_cli.py generate [--start YYYY-MM-DD] [--end YYYY-MM-DD]
    python agent_dashboard_cli.py view
"""

import sys
import argparse
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from digitalmodel.modules.visualization.agent_dashboard import (
    generate_dashboard,
    view_dashboard
)


def main():
    parser = argparse.ArgumentParser(
        description="Agent Health Monitoring Dashboard CLI",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Generate dashboard with default 30-day range
  python agent_dashboard_cli.py generate

  # Generate dashboard with custom date range
  python agent_dashboard_cli.py generate --start 2026-01-01 --end 2026-01-06

  # View dashboard in browser
  python agent_dashboard_cli.py view

  # Generate and view
  python agent_dashboard_cli.py generate && python agent_dashboard_cli.py view
        """
    )

    subparsers = parser.add_subparsers(dest='command', help='Command to execute')

    # Generate command
    gen_parser = subparsers.add_parser('generate', help='Generate dashboard from CSV data')
    gen_parser.add_argument(
        '--start',
        help='Start date (YYYY-MM-DD)',
        type=str,
        default=None
    )
    gen_parser.add_argument(
        '--end',
        help='End date (YYYY-MM-DD)',
        type=str,
        default=None
    )
    gen_parser.add_argument(
        '--data-dir',
        help='Data directory containing CSV files',
        type=str,
        default='data/agent_metrics'
    )
    gen_parser.add_argument(
        '--output-dir',
        help='Output directory for HTML',
        type=str,
        default='reports'
    )

    # View command
    view_parser = subparsers.add_parser('view', help='View dashboard in browser')
    view_parser.add_argument(
        '--file',
        help='Path to HTML dashboard file',
        type=str,
        default='reports/agent_health_dashboard.html'
    )

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        return 1

    try:
        if args.command == 'generate':
            print("Generating agent health dashboard...")
            output_path = generate_dashboard(
                data_dir=args.data_dir,
                output_dir=args.output_dir,
                start_date=args.start,
                end_date=args.end
            )
            print(f"Dashboard generated successfully: {output_path}")
            print(f"Run 'python {sys.argv[0]} view' to open in browser")
            return 0

        elif args.command == 'view':
            view_dashboard(args.file)
            return 0

    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        print(f"Hint: Run 'python {sys.argv[0]} generate' first", file=sys.stderr)
        return 1
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
