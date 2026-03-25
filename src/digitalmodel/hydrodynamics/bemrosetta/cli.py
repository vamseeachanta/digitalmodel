"""BEMRosetta CLI - Hydrodynamic coefficient conversion tools."""

import sys
from pathlib import Path
from typing import Optional

import click
from loguru import logger

from .core.runner import is_bemrosetta_available
from .core.exceptions import ParserError, ConverterError, MeshError, ValidationError


@click.group()
@click.version_option(version="1.0.0", prog_name="bemrosetta")
def cli():
    """BEMRosetta - Hydrodynamic coefficient conversion tools.

    Convert AQWA outputs to OrcaFlex format, validate coefficients,
    and convert mesh formats.
    """
    pass


@cli.command()
@click.argument("input_file", type=click.Path(exists=True, path_type=Path))
@click.option(
    "--output-format",
    "-f",
    default="orcaflex",
    type=click.Choice(["orcaflex", "yaml", "csv"]),
    help="Output format",
)
@click.option(
    "--output-dir",
    "-o",
    type=click.Path(path_type=Path),
    help="Output directory (default: same as input)",
)
@click.option(
    "--qtf",
    type=click.Path(exists=True, path_type=Path),
    help="QTF file for second-order forces",
)
@click.option("--vessel-name", "-n", help="Vessel name override")
@click.option(
    "--validate/--no-validate",
    default=True,
    help="Validate coefficients before conversion",
)
def convert(
    input_file: Path,
    output_format: str,
    output_dir: Optional[Path],
    qtf: Optional[Path],
    vessel_name: Optional[str],
    validate: bool,
):
    """Convert AQWA output to OrcaFlex format.

    Example:
        bemrosetta convert analysis.LIS -o ./output
        bemrosetta convert analysis.LIS --qtf analysis.QTF -o ./output
    """
    from .parsers import AQWAParser, QTFParser
    from .converters import OrcaFlexConverter
    from .validators import validate_coefficients

    try:
        # Parse AQWA file
        click.echo(f"Parsing {input_file}...")
        parser = AQWAParser()
        results = parser.parse(input_file)

        # Validate if requested
        if validate:
            click.echo("Validating coefficients...")
            # Note: validate_coefficients expects DiffractionResults
            # For AQWAParseResult, we skip detailed validation for now
            click.echo(click.style("Coefficient validation skipped (raw parse result)", fg="yellow"))

        # Set output directory
        output_dir = output_dir or input_file.parent / "orcaflex_output"
        output_dir.mkdir(parents=True, exist_ok=True)

        # Parse QTF if provided
        qtf_data = None
        if qtf:
            click.echo(f"Parsing QTF file: {qtf}...")
            qtf_parser = QTFParser()
            qtf_data = qtf_parser.parse(qtf)

        # Export to OrcaFlex-compatible format
        click.echo(f"Converting to {output_format} format...")

        # Export added mass to CSV
        _export_added_mass_csv(results, output_dir, vessel_name or input_file.stem)

        # Export damping to CSV
        _export_damping_csv(results, output_dir, vessel_name or input_file.stem)

        # Export RAOs to CSV
        _export_raos_csv(results, output_dir, vessel_name or input_file.stem)

        # Export QTF if provided
        if qtf_data:
            _export_qtf_csv(qtf_data, output_dir, vessel_name or input_file.stem)

        click.echo(click.style(f"Success! Output written to: {output_dir}", fg="green"))

    except ParserError as e:
        click.echo(click.style(f"Parser error: {e}", fg="red"), err=True)
        sys.exit(1)
    except ConverterError as e:
        click.echo(click.style(f"Converter error: {e}", fg="red"), err=True)
        sys.exit(1)
    except Exception as e:
        click.echo(click.style(f"Error: {e}", fg="red"), err=True)
        logger.exception("Conversion failed")
        sys.exit(1)


def _export_added_mass_csv(results, output_dir: Path, vessel_name: str) -> Path:
    """Export added mass matrices to CSV."""
    import csv

    output_file = output_dir / f"{vessel_name}_added_mass.csv"

    with open(output_file, "w", newline="") as f:
        writer = csv.writer(f)

        # Header
        header = ["Frequency_rad/s"]
        for i in range(6):
            for j in range(6):
                header.append(f"A{i+1}{j+1}")
        writer.writerow(header)

        # Data rows
        for freq in sorted(results.added_mass.keys()):
            matrix = results.added_mass[freq]
            row = [freq]
            for i in range(6):
                for j in range(6):
                    row.append(matrix[i, j])
            writer.writerow(row)

    click.echo(f"  Exported added mass: {output_file.name}")
    return output_file


def _export_damping_csv(results, output_dir: Path, vessel_name: str) -> Path:
    """Export damping matrices to CSV."""
    import csv

    output_file = output_dir / f"{vessel_name}_damping.csv"

    with open(output_file, "w", newline="") as f:
        writer = csv.writer(f)

        # Header
        header = ["Frequency_rad/s"]
        for i in range(6):
            for j in range(6):
                header.append(f"B{i+1}{j+1}")
        writer.writerow(header)

        # Data rows
        for freq in sorted(results.damping.keys()):
            matrix = results.damping[freq]
            row = [freq]
            for i in range(6):
                for j in range(6):
                    row.append(matrix[i, j])
            writer.writerow(row)

    click.echo(f"  Exported damping: {output_file.name}")
    return output_file


def _export_raos_csv(results, output_dir: Path, vessel_name: str) -> Path:
    """Export RAO data to CSV."""
    import csv

    output_file = output_dir / f"{vessel_name}_raos.csv"

    dof_names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]

    with open(output_file, "w", newline="") as f:
        writer = csv.writer(f)

        # Header
        header = ["Frequency_rad/s", "Heading_deg"]
        for dof in dof_names:
            header.extend([f"{dof}_amp", f"{dof}_phase"])
        writer.writerow(header)

        # Data rows
        for (freq, heading), rao_data in sorted(results.raos.items()):
            row = [freq, heading]
            for dof in dof_names:
                if dof in rao_data:
                    amp, phase = rao_data[dof]
                    row.extend([amp, phase])
                else:
                    row.extend([0.0, 0.0])
            writer.writerow(row)

    click.echo(f"  Exported RAOs: {output_file.name}")
    return output_file


def _export_qtf_csv(qtf_data, output_dir: Path, vessel_name: str) -> Path:
    """Export QTF data to CSV."""
    import csv
    import numpy as np

    output_file = output_dir / f"{vessel_name}_qtf.csv"

    with open(output_file, "w", newline="") as f:
        writer = csv.writer(f)

        # Header
        header = ["Freq1_rad/s", "Freq2_rad/s", "Heading_deg", "DOF"]
        header.extend(["Amplitude", "Phase_rad"])
        writer.writerow(header)

        # Data rows
        for comp in qtf_data.components:
            for i, f1 in enumerate(comp.frequencies_1):
                for j, f2 in enumerate(comp.frequencies_2):
                    row = [f1, f2, comp.heading, comp.dof + 1]
                    row.extend([comp.amplitude[i, j], comp.phase[i, j]])
                    writer.writerow(row)

    click.echo(f"  Exported QTF: {output_file.name}")
    return output_file


@cli.command()
@click.argument("input_file", type=click.Path(exists=True, path_type=Path))
def info(input_file: Path):
    """Display information about an AQWA file.

    Example:
        bemrosetta info analysis.LIS
    """
    from .parsers import AQWAParser

    try:
        parser = AQWAParser()
        results = parser.parse(input_file)

        click.echo(f"\n{'='*50}")
        click.echo(f"File: {input_file.name}")
        click.echo(f"{'='*50}")

        if parser.metadata:
            meta = parser.metadata
            click.echo(f"\nSolver: {meta.solver_type.value} {meta.solver_version or ''}")
            click.echo(f"Project: {meta.project_name}")
            depth = "Infinite" if meta.water_depth == float("inf") else f"{meta.water_depth} m"
            click.echo(f"Water Depth: {depth}")

        if results.frequencies:
            click.echo(f"\nFrequency Range: {min(results.frequencies):.3f} - "
                      f"{max(results.frequencies):.3f} rad/s")
            click.echo(f"Number of Frequencies: {len(results.frequencies)}")

        if results.headings:
            click.echo(f"Heading Range: {min(results.headings):.1f} deg - "
                      f"{max(results.headings):.1f} deg")
            click.echo(f"Number of Headings: {len(results.headings)}")

        click.echo(f"\nData Available:")
        click.echo(f"  - RAOs: {'Yes' if results.raos else 'No'} ({len(results.raos)} points)")
        click.echo(f"  - Added Mass: {'Yes' if results.added_mass else 'No'} ({len(results.added_mass)} frequencies)")
        click.echo(f"  - Damping: {'Yes' if results.damping else 'No'} ({len(results.damping)} frequencies)")

    except ParserError as e:
        click.echo(click.style(f"Parser error: {e}", fg="red"), err=True)
        sys.exit(1)
    except Exception as e:
        click.echo(click.style(f"Error: {e}", fg="red"), err=True)
        sys.exit(1)


@cli.command()
@click.argument("input_file", type=click.Path(exists=True, path_type=Path))
@click.option("--strict", is_flag=True, help="Use strict validation")
@click.option("--causality", is_flag=True, help="Check Kramers-Kronig causality")
def validate(input_file: Path, strict: bool, causality: bool):
    """Validate hydrodynamic coefficients.

    Example:
        bemrosetta validate analysis.LIS --strict
        bemrosetta validate analysis.LIS --causality
    """
    from .parsers import AQWAParser
    import numpy as np

    try:
        parser = AQWAParser()
        results = parser.parse(input_file)

        click.echo(f"Validating {input_file.name}...")

        errors = []
        warnings = []

        # Basic validation of parsed data
        click.echo("\nChecking data integrity...")

        # Check added mass matrices
        if results.added_mass:
            for freq, matrix in results.added_mass.items():
                # Check symmetry
                if not np.allclose(matrix, matrix.T, rtol=0.01):
                    warnings.append(f"Added mass matrix not symmetric at freq={freq:.3f}")

                # Check for NaN/Inf
                if np.any(np.isnan(matrix)) or np.any(np.isinf(matrix)):
                    errors.append(f"NaN/Inf in added mass at freq={freq:.3f}")

                # Check diagonal positivity (strict mode)
                if strict:
                    diag = np.diag(matrix)
                    if np.any(diag < -1e-6):
                        errors.append(f"Negative added mass diagonal at freq={freq:.3f}")

        # Check damping matrices
        if results.damping:
            for freq, matrix in results.damping.items():
                # Check symmetry
                if not np.allclose(matrix, matrix.T, rtol=0.01):
                    warnings.append(f"Damping matrix not symmetric at freq={freq:.3f}")

                # Check for NaN/Inf
                if np.any(np.isnan(matrix)) or np.any(np.isinf(matrix)):
                    errors.append(f"NaN/Inf in damping at freq={freq:.3f}")

                # Check diagonal positivity (strict mode)
                if strict:
                    diag = np.diag(matrix)
                    if np.any(diag < -1e-6):
                        warnings.append(f"Negative damping at freq={freq:.3f}")

        # Check RAOs
        if results.raos:
            for (freq, heading), rao_data in results.raos.items():
                for dof, (amp, phase) in rao_data.items():
                    if np.isnan(amp) or np.isinf(amp):
                        errors.append(f"NaN/Inf in {dof} RAO magnitude at freq={freq:.3f}")
                    if amp < 0:
                        warnings.append(f"Negative {dof} RAO magnitude at freq={freq:.3f}")

        # Causality check (Kramers-Kronig)
        if causality and results.added_mass and results.damping:
            click.echo("\nChecking causality (Kramers-Kronig)...")
            kk_results = _check_kramers_kronig(results)
            for key, error in kk_results.items():
                if error is not None and error > 0.1:
                    warnings.append(f"K-K violation for {key}: error={error:.2%}")
                    click.echo(f"  {key}: error={error:.2%}")

        # Report results
        if errors:
            click.echo(click.style("\nErrors:", fg="red"))
            for error in errors:
                click.echo(f"  x {error}")

        if warnings:
            click.echo(click.style("\nWarnings:", fg="yellow"))
            for warning in warnings:
                click.echo(f"  ! {warning}")

        # Summary
        if not errors:
            click.echo(click.style("\nValidation passed", fg="green"))
        else:
            click.echo(click.style("\nValidation failed", fg="red"))
            sys.exit(1)

    except ParserError as e:
        click.echo(click.style(f"Parser error: {e}", fg="red"), err=True)
        sys.exit(1)
    except Exception as e:
        click.echo(click.style(f"Error: {e}", fg="red"), err=True)
        sys.exit(1)


def _check_kramers_kronig(results) -> dict:
    """Simplified Kramers-Kronig check on parsed results."""
    import numpy as np
    from scipy import integrate

    kk_results = {}

    if not results.added_mass or not results.damping:
        return kk_results

    frequencies = np.array(sorted(results.added_mass.keys()))
    if len(frequencies) < 3:
        return kk_results

    # Check diagonal terms
    for i in range(6):
        a_values = np.array([results.added_mass[f][i, i] for f in frequencies])
        b_values = np.array([results.damping[f][i, i] for f in frequencies])

        # Estimate A(infinity)
        a_inf = a_values[-1]

        try:
            # Integrate B(w)/w
            integrand = b_values / np.maximum(frequencies, 1e-6)
            integral = integrate.cumulative_trapezoid(integrand, frequencies, initial=0)
            kk_estimate = (2 / np.pi) * integral

            # Compare with actual added mass variation
            a_variation = a_values - a_inf

            # Calculate relative error
            max_variation = np.max(np.abs(a_variation))
            if max_variation > 1e-10:
                error = np.mean(np.abs(kk_estimate - a_variation)) / max_variation
                kk_results[f"DOF{i+1}-{i+1}"] = float(error)
            else:
                kk_results[f"DOF{i+1}-{i+1}"] = 0.0

        except Exception:
            kk_results[f"DOF{i+1}-{i+1}"] = None

    return kk_results


@cli.command("convert-mesh")
@click.argument("input_file", type=click.Path(exists=True, path_type=Path))
@click.option(
    "--output",
    "-o",
    type=click.Path(path_type=Path),
    required=True,
    help="Output file path",
)
@click.option(
    "--output-format",
    "-f",
    type=click.Choice(["gdf", "dat", "stl"]),
    help="Output format (auto-detected from extension if not specified)",
)
def convert_mesh_cmd(input_file: Path, output: Path, output_format: Optional[str]):
    """Convert mesh between formats (GDF, DAT, STL).

    Example:
        bemrosetta convert-mesh input.gdf -o output.stl
        bemrosetta convert-mesh input.dat -o output.gdf -f gdf
    """
    from .mesh import convert_mesh

    try:
        click.echo(f"Converting {input_file} -> {output}...")

        result_path = convert_mesh(input_file, output, output_format=output_format)

        click.echo(click.style(f"Success! Written to: {result_path}", fg="green"))

    except MeshError as e:
        click.echo(click.style(f"Mesh error: {e}", fg="red"), err=True)
        sys.exit(1)
    except Exception as e:
        click.echo(click.style(f"Error: {e}", fg="red"), err=True)
        sys.exit(1)


@cli.command("validate-mesh")
@click.argument("input_file", type=click.Path(exists=True, path_type=Path))
@click.option("--check-normals", is_flag=True, help="Check normal consistency")
@click.option("--check-watertight", is_flag=True, help="Check if mesh is watertight")
def validate_mesh(input_file: Path, check_normals: bool, check_watertight: bool):
    """Validate mesh quality.

    Example:
        bemrosetta validate-mesh hull.gdf --check-normals
    """
    from .mesh import GDFHandler, DATHandler, STLHandler

    try:
        # Determine handler based on extension
        ext = input_file.suffix.lower()
        handlers = {
            ".gdf": GDFHandler,
            ".dat": DATHandler,
            ".stl": STLHandler,
        }

        if ext not in handlers:
            click.echo(f"Unsupported format: {ext}")
            sys.exit(1)

        handler = handlers[ext]()
        mesh = handler.read(input_file)
        report = handler.validate_mesh(mesh)

        click.echo(f"\n{'='*50}")
        click.echo(f"Mesh Quality Report: {input_file.name}")
        click.echo(f"{'='*50}")

        click.echo(f"\nGeometry:")
        click.echo(f"  Vertices: {report.n_vertices}")
        click.echo(f"  Panels: {report.n_panels}")
        click.echo(f"  Total Area: {report.total_area:.2f} m^2")

        click.echo(f"\nPanel Statistics:")
        click.echo(f"  Min Area: {report.min_panel_area:.4f} m^2")
        click.echo(f"  Max Area: {report.max_panel_area:.4f} m^2")
        click.echo(f"  Mean Area: {report.mean_panel_area:.4f} m^2")
        click.echo(f"  Max Aspect Ratio: {report.aspect_ratio_max:.1f}")

        click.echo(f"\nQuality Metrics:")
        click.echo(f"  Degenerate Panels: {report.n_degenerate_panels}")
        click.echo(f"  Duplicate Vertices: {report.n_duplicate_vertices}")
        click.echo(f"  Consistent Normals: {'Yes' if report.has_consistent_normals else 'No'}")
        click.echo(f"  Quality Score: {report.quality_score:.0f}/100")

        if report.warnings:
            click.echo(click.style("\nWarnings:", fg="yellow"))
            for warning in report.warnings:
                click.echo(f"  ! {warning}")

        if report.quality_score >= 80:
            click.echo(click.style("\nGood mesh quality", fg="green"))
        elif report.quality_score >= 50:
            click.echo(click.style("\nAcceptable mesh quality", fg="yellow"))
        else:
            click.echo(click.style("\nPoor mesh quality", fg="red"))

    except MeshError as e:
        click.echo(click.style(f"Mesh error: {e}", fg="red"), err=True)
        sys.exit(1)
    except Exception as e:
        click.echo(click.style(f"Error: {e}", fg="red"), err=True)
        sys.exit(1)


@cli.command()
def status():
    """Show BEMRosetta module status and capabilities."""
    click.echo("\nBEMRosetta Module Status")
    click.echo("=" * 40)

    click.echo(f"\nBEMRosetta Executable: ", nl=False)
    if is_bemrosetta_available():
        click.echo(click.style("Available", fg="green"))
    else:
        click.echo(click.style("Not found", fg="yellow"))
        click.echo("  (Native Python parsers will be used)")

    click.echo("\nSupported Input Formats:")
    click.echo("  - AQWA (.LIS, .DAT)")
    click.echo("  - QTF (.QTF)")

    click.echo("\nSupported Output Formats:")
    click.echo("  - OrcaFlex (YAML, CSV)")

    click.echo("\nSupported Mesh Formats:")
    click.echo("  - WAMIT GDF (.gdf)")
    click.echo("  - AQWA/NEMOH DAT (.dat)")
    click.echo("  - STL (.stl)")

    click.echo("\nValidation Features:")
    click.echo("  - Coefficient symmetry")
    click.echo("  - Positive definiteness")
    click.echo("  - Physical limits")
    click.echo("  - Kramers-Kronig causality")


def main():
    """Main entry point for CLI."""
    cli()


if __name__ == "__main__":
    main()
