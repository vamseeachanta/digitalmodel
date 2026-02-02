"""
ABOUTME: CLI interface for quality gate validation system.
Provides commands for checking gates and generating reports.
"""

import sys
from pathlib import Path

import click
from loguru import logger

from .quality_gates import QualityGateValidator


@click.group()
@click.option("--verbose", "-v", is_flag=True, help="Enable verbose logging")
def cli(verbose: bool):
    """Quality Gate Validation CLI."""
    if verbose:
        logger.remove()
        logger.add(sys.stderr, level="DEBUG")
    else:
        logger.remove()
        logger.add(sys.stderr, level="INFO")


@cli.command()
@click.option(
    "--config",
    "-c",
    type=click.Path(exists=True, path_type=Path),
    help="Path to quality-gates.yaml config file",
)
@click.option(
    "--strict",
    is_flag=True,
    help="Strict mode: warnings become failures",
)
@click.option(
    "--json",
    "json_output",
    is_flag=True,
    help="Output results as JSON",
)
def check(config: Path | None, strict: bool, json_output: bool):
    """
    Execute quality gates and report results.

    \b
    Examples:
        quality-gates check
        quality-gates check --strict
        quality-gates check --config custom-gates.yaml
    """
    try:
        validator = QualityGateValidator(config_path=config, strict_mode=strict)
        report = validator.execute_all_gates()

        if json_output:
            import json

            data = {
                "overall_status": report.overall_status.value,
                "gates_executed": report.gates_executed,
                "gates_passed": report.gates_passed,
                "gates_warned": report.gates_warned,
                "gates_failed": report.gates_failed,
                "results": [
                    {
                        "gate": r.gate_name,
                        "status": r.status.value,
                        "message": r.message,
                        "metrics": r.metrics,
                    }
                    for r in report.results
                ],
            }
            click.echo(json.dumps(data, indent=2))
        else:
            validator.print_report(report)

        # Exit with appropriate code
        if report.overall_status.value == "failure":
            sys.exit(1)
        elif report.overall_status.value == "warning" and strict:
            sys.exit(1)
        else:
            sys.exit(0)

    except Exception as e:
        logger.exception("Quality gate execution failed")
        click.echo(f"Error: {str(e)}", err=True)
        sys.exit(1)


@cli.command()
@click.option(
    "--config",
    "-c",
    type=click.Path(exists=True, path_type=Path),
    help="Path to quality-gates.yaml config file",
)
@click.option(
    "--output",
    "-o",
    type=click.Path(path_type=Path),
    help="Output file path (default: reports/quality_gates_report.html)",
)
def report(config: Path | None, output: Path | None):
    """
    Generate detailed quality gate report.

    \b
    Examples:
        quality-gates report
        quality-gates report --output custom_report.html
    """
    try:
        validator = QualityGateValidator(config_path=config)
        gate_report = validator.execute_all_gates()

        # For now, just print console report
        # TODO: Implement HTML report with Plotly in future
        validator.print_report(gate_report)

        click.echo("\n📊 HTML report generation coming soon (Plotly dashboard)")
        click.echo(f"📄 JSON report available at: {validator.reports_dir / 'quality_gates_results.json'}")

    except Exception as e:
        logger.exception("Report generation failed")
        click.echo(f"Error: {str(e)}", err=True)
        sys.exit(1)


if __name__ == "__main__":
    cli()
