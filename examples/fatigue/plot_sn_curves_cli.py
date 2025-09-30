#!/usr/bin/env python3
"""
S-N Curve Plotting CLI Tool

Command-line interface for plotting S-N curves from the fatigue database.

Usage:
    python plot_sn_curves_cli.py --indices 64,175 --scf 1.0 --output my_plot.png
    python plot_sn_curves_cli.py --list --filter DNV
    python plot_sn_curves_cli.py --compare-ref 64 --compare 63,175,183
"""

import sys
import argparse
from pathlib import Path

# Add src to path for imports
src_path = Path(__file__).parent.parent.parent / 'src'
sys.path.insert(0, str(src_path))

from digitalmodel.fatigue.sn_curve_plotter import SNCurvePlotter


def list_curves(args):
    """List available curves with optional filtering."""
    plotter = SNCurvePlotter()

    df = plotter.list_curves(
        curve_type_filter=args.curve_type,
        environment_filter=args.environment,
        joint_type_filter=args.joint_type
    )

    print("\nAvailable S-N Curves:")
    print("=" * 80)
    print(df.to_string(index=False))
    print(f"\nTotal: {len(df)} curves")


def plot_curves(args):
    """Plot S-N curves."""
    plotter = SNCurvePlotter()

    # Parse indices
    indices = [int(idx.strip()) for idx in args.indices.split(',')]

    print(f"\nPlotting curves: {indices}")
    print(f"SCF: {args.scf}")
    print(f"Fatigue limit: {'Included' if args.with_limit else 'Excluded'}")
    print(f"Plot type: {args.plot_type}")

    plotter.plot_curves(
        lookup_indices=indices,
        scf=args.scf,
        include_fatigue_limit=args.with_limit,
        plot_type=args.plot_type,
        title=args.title,
        save_path=args.output,
        show_plot=not args.no_show
    )

    if args.output:
        print(f"\n✓ Plot saved to: {args.output}")


def compare_curves(args):
    """Create comparison plot with reference curve."""
    plotter = SNCurvePlotter()

    # Parse indices
    comparison_indices = [int(idx.strip()) for idx in args.compare.split(',')]

    print(f"\nReference curve: {args.reference}")
    print(f"Comparison curves: {comparison_indices}")
    print(f"SCF: {args.scf}")
    print(f"Fatigue limit: {'Included' if args.with_limit else 'Excluded'}")

    plotter.create_comparison_plot(
        reference_index=args.reference,
        comparison_indices=comparison_indices,
        scf=args.scf,
        include_fatigue_limit=args.with_limit,
        plot_type=args.plot_type,
        save_path=args.output
    )

    if args.output:
        print(f"\n✓ Plot saved to: {args.output}")


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        description='S-N Curve Plotting Tool',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # List all curves
  %(prog)s --list

  # List DNV curves in air
  %(prog)s --list --curve-type DNV --environment Air

  # Plot specific curves
  %(prog)s --plot --indices 64,175 --output plot.png

  # Plot with SCF=2.0 and no fatigue limit
  %(prog)s --plot --indices 64,175 --scf 2.0 --no-limit

  # Comparison plot with reference
  %(prog)s --compare-ref 64 --compare 63,175,183 --output comparison.png

  # Linear-log plot
  %(prog)s --plot --indices 64 --plot-type linear-log
        """
    )

    # Mode selection
    mode_group = parser.add_mutually_exclusive_group(required=True)
    mode_group.add_argument('--list', action='store_true',
                           help='List available curves')
    mode_group.add_argument('--plot', action='store_true',
                           help='Plot S-N curves')
    mode_group.add_argument('--compare-ref', type=int, metavar='INDEX',
                           help='Create comparison plot with reference curve')

    # Listing options
    list_group = parser.add_argument_group('listing options')
    list_group.add_argument('--curve-type', type=str,
                           help='Filter by curve type (e.g., DNV, API, ABS)')
    list_group.add_argument('--environment', type=str,
                           help='Filter by environment')
    list_group.add_argument('--joint-type', type=str,
                           help='Filter by joint type')

    # Plotting options
    plot_group = parser.add_argument_group('plotting options')
    plot_group.add_argument('--indices', type=str,
                           help='Comma-separated lookup indices to plot')
    plot_group.add_argument('--compare', type=str,
                           help='Comma-separated comparison indices (for --compare-ref)')
    plot_group.add_argument('--scf', type=float, default=1.0,
                           help='Stress concentration factor (default: 1.0)')
    plot_group.add_argument('--no-limit', action='store_false', dest='with_limit',
                           help='Exclude fatigue limit')
    plot_group.add_argument('--plot-type', choices=['log-log', 'linear-log'],
                           default='log-log',
                           help='Plot type (default: log-log)')
    plot_group.add_argument('--title', type=str,
                           help='Custom plot title')
    plot_group.add_argument('--output', '-o', type=str,
                           help='Output file path')
    plot_group.add_argument('--no-show', action='store_true',
                           help='Don\'t display plot')

    args = parser.parse_args()

    # Validate and execute
    try:
        if args.list:
            list_curves(args)

        elif args.plot:
            if not args.indices:
                parser.error('--plot requires --indices')
            plot_curves(args)

        elif args.compare_ref is not None:
            if not args.compare:
                parser.error('--compare-ref requires --compare')
            compare_curves(args)

    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == '__main__':
    main()
