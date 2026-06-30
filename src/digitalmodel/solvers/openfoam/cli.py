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
from .runner import OpenFOAMRunConfig, OpenFOAMRunner, OpenFOAMRunStatus


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


# ============================================================================
# openfoam run
# ============================================================================


@cli.command("run")
@click.argument(
    "case_dir",
    type=click.Path(exists=True, file_okay=False),
)
@click.option(
    "--solver",
    default=None,
    help="Solver application (default: read 'application' from controlDict).",
)
@click.option(
    "--mesh-utility",
    default="blockMesh",
    show_default=True,
    help="Mesh generator to run first.",
)
@click.option(
    "--snappy/--no-snappy",
    default=False,
    show_default=True,
    help="Run snappyHexMesh -overwrite after the mesh utility (3D from STL).",
)
@click.option(
    "--vtk/--no-vtk",
    default=True,
    show_default=True,
    help="Run foamToVTK after the solver for PyVista/ParaView.",
)
@click.option(
    "--dry-run",
    is_flag=True,
    default=False,
    help="Validate the case and report stages without executing.",
)
def run_cmd(
    case_dir: str,
    solver: str | None,
    mesh_utility: str,
    snappy: bool,
    vtk: bool,
    dry_run: bool,
) -> None:
    """Execute a prepared OpenFOAM case (mesh -> solve -> VTK), fail-closed."""
    runner = OpenFOAMRunner(
        OpenFOAMRunConfig(
            solver=solver,
            mesh_utility=mesh_utility,
            run_snappy=snappy,
            to_vtk=vtk,
            dry_run=dry_run,
        )
    )
    result = runner.run(Path(case_dir))
    click.echo(f"Status : {result.status.value}")
    click.echo(f"Solver : {result.solver}")
    for stage in result.stages:
        mark = "ok" if stage.ok else "FAIL"
        click.echo(
            f"  [{mark}] {stage.name} "
            f"rc={stage.return_code} ({stage.duration_seconds:.1f}s)"
        )
    if result.error_message:
        click.echo(f"Note   : {result.error_message}", err=True)
    # Fail-closed: a DRY_RUN caused by a missing install, or any FAILED, is a
    # non-zero exit so the licensed-run lane never records a false finish.
    if result.status not in (OpenFOAMRunStatus.COMPLETED, OpenFOAMRunStatus.DRY_RUN):
        sys.exit(1)
    if result.status is OpenFOAMRunStatus.DRY_RUN and not dry_run:
        sys.exit(2)  # asked for a real solve but OpenFOAM was unavailable


# ============================================================================
# openfoam doctor
# ============================================================================


@cli.command("doctor")
@click.option(
    "--output-dir",
    "-o",
    type=click.Path(),
    default=None,
    help="Output root to test for writability (default: /mnt/ace/cfd-output).",
)
@click.option(
    "--require-solver",
    is_flag=True,
    default=False,
    help="Exit nonzero if the host cannot actually solve (dry-run only).",
)
def doctor_cmd(output_dir: str | None, require_solver: bool) -> None:
    """Diagnose OpenFOAM CFD readiness on this host.

    Reports PASS/WARN/FAIL for the OpenFOAM utilities, solvers, version, the
    Python post-processing/meshing stack, and output-root writability, then
    states whether the host is solver-capable or dry-run only.

    Exit policy: 0 = host usable (dry-run-only is a supported mode);
    nonzero = any FAIL check, or dry-run-only with --require-solver.
    """
    from .doctor import has_failure, run_doctor

    click.echo("=" * 80)
    click.echo("OpenFOAM CFD Doctor")
    click.echo("=" * 80)

    checks, capability = run_doctor(
        output_dir=Path(output_dir) if output_dir else None,
    )
    colors = {"PASS": "green", "WARN": "yellow", "FAIL": "red"}
    for check in checks:
        click.echo(
            click.style(f"[{check.status:4}] ", fg=colors[check.status])
            + f"{check.name}: {check.detail}"
        )

    if has_failure(checks):
        click.echo(click.style("\n[FAIL] Hard failures present.", fg="red"))
        sys.exit(1)
    if require_solver and capability == "dry-run-only":
        click.echo(
            click.style(
                "\n[FAIL] --require-solver: host is dry-run only.", fg="red"
            )
        )
        sys.exit(1)
    click.echo(click.style(f"\n[OK] Host usable ({capability}).", fg="green"))


def main() -> None:
    """Main CLI entry point."""
    cli()


if __name__ == "__main__":
    main()
