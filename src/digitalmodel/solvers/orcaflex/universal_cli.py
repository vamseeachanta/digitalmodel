#!/usr/bin/env python
"""
Command-Line Interface for Universal OrcaFlex Runner
====================================================

Provides CLI access to the Universal OrcaFlex simulation runner.
"""

import sys
import os
import click
import logging
from pathlib import Path
from typing import Optional
import json
import yaml

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from digitalmodel.solvers.orcaflex.universal import (
    UniversalOrcaFlexRunner,
    StatusReporter
)

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%H:%M:%S'
)
logger = logging.getLogger(__name__)


@click.command()
@click.option('--pattern', '-p', 
              default='*.yml',
              help='File pattern to match (e.g., "*.yml", "fsts_*.dat")')
@click.option('--input-dir', '-i',
              type=click.Path(exists=False),
              help='Input directory containing models')
@click.option('--output-dir', '-o',
              type=click.Path(),
              help='Output directory for .sim files')
@click.option('--models', '-m',
              multiple=True,
              help='Specific model files to process (can be used multiple times)')
@click.option('--config', '-c',
              type=click.Path(exists=True),
              help='Configuration file (YAML)')
@click.option('--recursive', '-r',
              is_flag=True,
              help='Search directories recursively')
@click.option('--parallel/--sequential',
              default=True,
              help='Enable/disable parallel processing')
@click.option('--workers', '-w',
              type=int,
              default=30,
              help='Maximum number of parallel workers')
@click.option('--mock',
              is_flag=True,
              help='Run in mock mode (no OrcaFlex license required)')
@click.option('--exclude', '-e',
              multiple=True,
              help='Patterns to exclude from processing')
@click.option('--verbose', '-v',
              is_flag=True,
              help='Enable verbose output')
@click.option('--report',
              type=click.Path(),
              help='Save JSON report to specified file')
@click.option('--html-report',
              type=click.Path(),
              default=None,
              help='Generate structured HTML analysis report to this path')
@click.option('--all',
              is_flag=True,
              help='Process all matching files in directory')
@click.option('--test',
              is_flag=True,
              help='Run in test mode (process first 3 files only)')
@click.version_option(version='1.0.0')
def main(pattern: str,
         input_dir: Optional[str],
         output_dir: Optional[str],
         models: tuple,
         config: Optional[str],
         recursive: bool,
         parallel: bool,
         workers: int,
         mock: bool,
         exclude: tuple,
         verbose: bool,
         report: Optional[str],
         html_report: Optional[str],
         all: bool,
         test: bool):
    """
    Universal OrcaFlex Simulation Runner
    
    Run OrcaFlex simulations from any directory with flexible options.
    
    Examples:
    
        # Process all .yml files in current directory
        orcaflex-universal --all
        
        # Process specific pattern with output directory
        orcaflex-universal -p "fsts_*.yml" -i ./models -o ./sims
        
        # Process specific files
        orcaflex-universal -m model1.yml -m model2.yml
        
        # Use configuration file
        orcaflex-universal -c batch_config.yml
        
        # Run in mock mode for testing
        orcaflex-universal --mock --test
        
        # Recursive search with exclusions
        orcaflex-universal -r -e "*backup*" -e "*test*" --all
    """
    
    # Header
    click.echo("=" * 80)
    click.echo(click.style("UNIVERSAL ORCAFLEX RUNNER", fg='blue', bold=True))
    click.echo("=" * 80)
    
    try:
        # Initialize runner
        runner = UniversalOrcaFlexRunner(
            mock_mode=mock,
            max_workers=workers,
            verbose=verbose
        )
        
        # Initialize status reporter
        status_reporter = StatusReporter(enable_colors=True)
        
        # Prepare model list
        model_list = None
        if models:
            model_list = list(models)
            click.echo(f"Processing {len(model_list)} specified models")
        elif test:
            click.echo("Test mode: Will process first 3 matching files")
        elif not all and not config:
            click.echo(click.style(
                "Warning: No files specified. Use --all to process all matching files, "
                "--models to specify files, or --config for batch configuration.",
                fg='yellow'
            ))
            if not click.confirm("Process all matching files?"):
                click.echo("Aborted.")
                return 1
        
        # Run simulations
        results = runner.run(
            pattern=pattern,
            input_directory=input_dir,
            output_directory=output_dir,
            models=model_list,
            config_file=config,
            recursive=recursive,
            parallel=parallel,
            exclude_patterns=list(exclude) if exclude else None,
            status_reporter=status_reporter,
            test_mode=test
        )
        
        # Display summary
        status_reporter.display_summary()
        
        # Save JSON report if requested
        if report:
            report_path = Path(report)
            status_reporter.save_report(report_path)
            click.echo(f"\nReport saved to: {report_path}")

        # Generate HTML analysis report if requested
        if html_report:
            _generate_html_report(results, html_report, mock, verbose)

        # Set final terminal status
        status_reporter.set_final_status()
        
        # Exit code based on results
        if results.failed > 0:
            click.echo(click.style(
                f"\n⚠ {results.failed} models failed to process",
                fg='red'
            ))
            return 1
        else:
            click.echo(click.style(
                f"\n✓ All {results.successful} models processed successfully",
                fg='green'
            ))
            return 0
            
    except KeyboardInterrupt:
        click.echo("\n\nProcess interrupted by user")
        return 130
    except Exception as e:
        click.echo(click.style(f"\nError: {e}", fg='red'), err=True)
        if verbose:
            import traceback
            traceback.print_exc()
        return 1


def _generate_html_report(results, html_report_path: str, mock: bool, verbose: bool) -> None:
    """Generate a structured HTML analysis report from run results."""
    from pathlib import Path as _Path

    html_path = _Path(html_report_path)
    html_path.parent.mkdir(parents=True, exist_ok=True)

    try:
        from digitalmodel.solvers.orcaflex.reporting import generate_orcaflex_report
        from digitalmodel.solvers.orcaflex.reporting.extractors.aggregator import (
            build_report_from_model,
        )
    except ImportError as exc:
        click.echo(click.style(f"\n[html-report] Import error: {exc}", fg='yellow'))
        return

    if mock:
        # In mock mode produce a minimal synthetic report
        _write_mock_html_report(html_path)
        click.echo(f"\n[html-report] Mock HTML report: {html_path}")
        return

    # Locate first .sim file from run results
    sim_path = _find_sim_file(results)
    if sim_path is None:
        click.echo(click.style(
            "\n[html-report] No .sim files found in results; skipping HTML report.",
            fg='yellow'
        ))
        return

    try:
        import OrcFxAPI as ofx
        model = ofx.Model(str(sim_path))
        report_data = build_report_from_model(
            model,
            project_name="OrcaFlex Analysis",
            structure_type="riser",
        )
        generate_orcaflex_report(report_data, output_path=html_path)
        click.echo(f"\n[html-report] HTML report written: {html_path}")
    except Exception as exc:
        click.echo(click.style(f"\n[html-report] Failed: {exc}", fg='red'))
        if verbose:
            import traceback
            traceback.print_exc()


def _find_sim_file(results):
    """Return the first .sim file path from runner results, or None."""
    try:
        sim_files = getattr(results, "sim_files_created", None) or []
        for f in sim_files:
            p = Path(f)
            if p.suffix.lower() == ".sim" and p.exists():
                return p
    except Exception:
        pass
    return None


def _write_mock_html_report(html_path) -> None:
    """Write a minimal synthetic HTML report for mock/test mode."""
    import numpy as np
    from digitalmodel.solvers.orcaflex.reporting import generate_orcaflex_report
    from digitalmodel.solvers.orcaflex.reporting.models.report import OrcaFlexAnalysisReport
    from digitalmodel.solvers.orcaflex.reporting.models.geometry import (
        GeometryData, LineProfileData, KeyPointData,
    )
    from digitalmodel.solvers.orcaflex.reporting.models.results import (
        DynamicResultsData, TimeSeriesData, EnvelopeData,
    )

    arc_lengths = list(np.linspace(0, 1000, 20))
    geometry = GeometryData(
        water_depth_m=500.0,
        line_profile=LineProfileData(
            arc_length=arc_lengths,
            x=arc_lengths,
            y=[0.0] * len(arc_lengths),
            z=[-500.0 * s / 1000.0 for s in arc_lengths],
        ),
        key_points=[
            KeyPointData(label="End A", arc_length_m=0.0, x=0.0, z=0.0),
            KeyPointData(label="End B", arc_length_m=1000.0, x=1000.0, z=-500.0),
        ],
    )
    times = list(np.linspace(0, 100, 10))
    dynamic = DynamicResultsData(
        ramp_end_time_s=20.0,
        time_series=[
            TimeSeriesData(
                id="te_mock", label="Te @ mock",
                t=times,
                values=[1000 + 50 * np.sin(t / 10) for t in times],
                units="kN",
            )
        ],
        envelopes=[
            EnvelopeData(
                id="te_env", label="Effective Tension Envelope",
                arc_length=arc_lengths,
                max_values=[1100.0] * len(arc_lengths),
                min_values=[900.0] * len(arc_lengths),
                units="kN",
            )
        ],
    )
    report_data = OrcaFlexAnalysisReport(
        project_name="Mock OrcaFlex Run",
        structure_id="MOCK-001",
        structure_type="riser",
        geometry=geometry,
        dynamic_results=dynamic,
    )
    generate_orcaflex_report(report_data, output_path=html_path)


@click.command()
@click.option('--output', '-o',
              default='batch_config.yml',
              help='Output file name for configuration template')
def create_config(output: str):
    """Create a template batch configuration file."""
    
    template = {
        'processing': {
            'pattern': '*.yml',
            'input_directory': './models',
            'output_directory': './simulations',
            'recursive': False,
            'parallel': True,
            'max_workers': 30,
            'exclude_patterns': [
                '*backup*',
                '*test*',
                '*includefile*'
            ]
        },
        'models': {
            'specific_files': [
                '# List specific files here if not using pattern',
                '# - model1.yml',
                '# - model2.dat'
            ]
        },
        'options': {
            'mock_mode': False,
            'verbose': True,
            'save_report': True,
            'report_path': './simulation_report.json'
        }
    }
    
    with open(output, 'w') as f:
        yaml.dump(template, f, default_flow_style=False, sort_keys=False)
    
    click.echo(f"Created configuration template: {output}")
    click.echo("\nEdit this file and run:")
    click.echo(f"  orcaflex-universal -c {output}")


@click.group()
def cli():
    """Universal OrcaFlex Runner CLI."""
    pass


# Add commands to group
cli.add_command(main, name='run')
cli.add_command(create_config, name='create-config')


if __name__ == '__main__':
    # If called directly, run the main command
    sys.exit(main())