# ABOUTME: CLI entry point for dynacard analysis providing sucker rod pump diagnostics.
# ABOUTME: Loads well data from JSON, runs the DynacardWorkflow, and prints key results.

import json
import sys
from pathlib import Path

import click

from .data_loader import load_from_json_file
from .solver import DynacardWorkflow


@click.command()
@click.argument("json_file", type=click.Path(exists=True))
@click.option(
    "--solver",
    type=click.Choice(["gibbs", "finite_difference"], case_sensitive=False),
    default="gibbs",
    help="Physics solver method (default: gibbs)",
)
@click.option(
    "--output",
    "-o",
    type=click.Path(),
    default=None,
    help="Output file for results (JSON)",
)
def cli(json_file: str, solver: str, output: str):
    """Run dynacard analysis on a well data JSON file.

    JSON_FILE is the path to a legacy well data JSON file.
    """
    try:
        # Load well data
        filepath = Path(json_file)
        click.echo(f"Loading well data from: {filepath}")
        context = load_from_json_file(filepath)

        # Run analysis
        click.echo(f"Running dynacard analysis (solver: {solver})...")
        workflow = DynacardWorkflow(context, solver_method=solver)
        results = workflow.run_full_analysis()

        # Print summary
        click.echo("")
        click.echo("=" * 60)
        click.echo("Dynacard Analysis Results")
        click.echo("=" * 60)
        click.echo(f"  Well API14:              {context.api14}")
        click.echo(f"  Solver Method:           {results.solver_method}")
        click.echo(f"  SPM:                     {context.spm:.1f}")
        click.echo(f"  Pump Depth:              {context.pump.depth:.1f} ft")
        click.echo("-" * 60)

        click.echo("  Rod Loads:")
        click.echo(
            f"    Peak Polished Rod:     {results.peak_polished_rod_load:.1f} lbs"
        )
        click.echo(
            f"    Min Polished Rod:      {results.minimum_polished_rod_load:.1f} lbs"
        )

        if results.fluid_load:
            click.echo(f"  Fluid Load:              {results.fluid_load.fluid_load:.1f} lbs")

        if results.fillage:
            click.echo(f"  Pump Fillage:            {results.fillage.fillage:.1f}%")

        if results.production:
            click.echo(
                f"  Theoretical Production:  {results.production.theoretical_production:.1f} bbl/day"
            )

        if results.cpip:
            click.echo(
                f"  CPIP:                    {results.cpip.pump_intake_pressure:.1f} psi"
            )

        click.echo(f"  Buckling Detected:       {results.buckling_detected}")
        click.echo("-" * 60)
        click.echo(f"  Diagnostic:              {results.diagnostic_message}")
        click.echo("=" * 60)

        # Save to file
        if output:
            output_path = Path(output)
            output_path.parent.mkdir(parents=True, exist_ok=True)
            output_data = results.model_dump(
                exclude={"ctx", "downhole_card"},
                exclude_none=True,
            )
            with open(output_path, "w") as f:
                json.dump(output_data, f, indent=2, default=str)
            click.echo(f"\nResults saved to: {output}")

    except Exception as e:
        click.echo(f"Error: {e}", err=True)
        sys.exit(1)


def main():
    """Main entry point for CLI."""
    cli()


if __name__ == "__main__":
    main()
