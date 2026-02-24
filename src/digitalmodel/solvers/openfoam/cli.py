#!/usr/bin/env python3
"""
ABOUTME: Command-line interface for OpenFOAM CFD case setup, mesh generation,
and post-processing. Follows the same pattern as the GMSH meshing CLI.
"""

from __future__ import annotations

import sys
from pathlib import Path

import click

from .case_builder import OpenFOAMCaseBuilder
from .domain_builder import DomainBuilder
from .models import CaseType, OpenFOAMCase, SolverConfig
from .parametric import ParametricStudy, StudyParameter


@click.group()
def cli() -> None:
    """OpenFOAM CFD case generation tools for marine/offshore engineering."""


# ============================================================================
# openfoam setup
# ============================================================================


@cli.command("setup")
@click.option(
    "--type",
    "case_type",
    type=click.Choice(
        [ct.value for ct in CaseType], case_sensitive=False
    ),
    required=True,
    help="Marine application type.",
)
@click.option(
    "--name",
    required=True,
    help="Case directory name.",
)
@click.option(
    "--output-dir",
    "-o",
    type=click.Path(),
    default=".",
    show_default=True,
    help="Parent directory where the case is created.",
)
@click.option(
    "--hull-length",
    type=float,
    default=None,
    help="Hull length (m) for automatic domain sizing.",
)
@click.option(
    "--hull-beam",
    type=float,
    default=None,
    help="Hull beam (m) for automatic domain sizing.",
)
@click.option(
    "--hull-draft",
    type=float,
    default=None,
    help="Hull draught (m) for automatic domain sizing.",
)
def setup_cmd(
    case_type: str,
    name: str,
    output_dir: str,
    hull_length: float | None,
    hull_beam: float | None,
    hull_draft: float | None,
) -> None:
    """Create an OpenFOAM case directory from a marine application template."""
    ct = CaseType(case_type)
    case = OpenFOAMCase.for_case_type(ct, name)

    if hull_length and hull_beam and hull_draft:
        case.domain = DomainBuilder.from_hull_dims(
            length=hull_length,
            beam=hull_beam,
            draft=hull_draft,
        )
        click.echo(
            f"Auto-sized domain from hull: L={hull_length} B={hull_beam} T={hull_draft}"
        )

    builder = OpenFOAMCaseBuilder(case)
    out_path = Path(output_dir)
    case_dir = builder.build(out_path)
    click.echo(f"Case created: {case_dir}")


# ============================================================================
# openfoam parametric
# ============================================================================


@cli.command("parametric")
@click.option(
    "--type",
    "case_type",
    type=click.Choice(
        [ct.value for ct in CaseType], case_sensitive=False
    ),
    required=True,
    help="Marine application type.",
)
@click.option(
    "--study-name",
    default="study",
    show_default=True,
    help="Study name prefix for case directories.",
)
@click.option(
    "--output-dir",
    "-o",
    type=click.Path(),
    default=".",
    show_default=True,
    help="Parent directory for generated cases.",
)
@click.option(
    "--param",
    "param_specs",
    multiple=True,
    metavar="NAME:START:STOP:STEP",
    help=(
        "Parameter range spec NAME:START:STOP:STEP. "
        "Repeat option to add more parameters."
    ),
)
def parametric_cmd(
    case_type: str,
    study_name: str,
    output_dir: str,
    param_specs: tuple[str, ...],
) -> None:
    """Generate a parametric study with multiple OpenFOAM cases."""
    if not param_specs:
        click.echo("Error: at least one --param is required.", err=True)
        sys.exit(1)

    ct = CaseType(case_type)
    study = ParametricStudy(case_type=ct, study_name=study_name)

    for spec in param_specs:
        parts = spec.split(":")
        if len(parts) != 4:
            click.echo(
                f"Error: param spec must be NAME:START:STOP:STEP, got: {spec!r}",
                err=True,
            )
            sys.exit(1)
        p_name, p_start, p_stop, p_step = parts
        study.add_parameter(
            StudyParameter.from_range(
                name=p_name,
                start=float(p_start),
                stop=float(p_stop),
                step=float(p_step),
            )
        )

    dirs = study.generate_directories(base_dir=Path(output_dir))
    click.echo(f"Generated {len(dirs)} cases in {output_dir}:")
    for d in dirs:
        click.echo(f"  {d}")


def main() -> None:
    """Main CLI entry point."""
    cli()


if __name__ == "__main__":
    main()
