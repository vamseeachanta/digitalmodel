#!/usr/bin/env python3
"""
Diffraction Analysis CLI Tools

ABOUTME: Command-line interface for diffraction analysis conversion, comparison, and batch processing.

Provides CLI commands for:
- Converting AQWA results to OrcaFlex format
- Converting OrcaWave results to OrcaFlex format
- Comparing AQWA vs OrcaWave results
- Batch processing multiple vessels
- Geometry quality validation

Version: 3.0.0 (Phase 3)
Status: CLI automation tools
"""

import click
from pathlib import Path
import json
import sys

from digitalmodel.hydrodynamics.diffraction import (
    AQWAConverter,
    OrcaFlexExporter,
    validate_results,
    ORCAWAVE_AVAILABLE
)

if ORCAWAVE_AVAILABLE:
    from digitalmodel.hydrodynamics.diffraction import OrcaWaveConverter

from digitalmodel.hydrodynamics.diffraction.comparison_framework import compare_diffraction_results
from digitalmodel.hydrodynamics.diffraction.batch_processor import process_batch_from_config_file


@click.group()
@click.version_option(version="3.0.0", prog_name="diffraction")
def cli():
    """
    Diffraction Analysis Tools - CLI for AQWA/OrcaWave conversion and analysis

    Convert diffraction results to OrcaFlex format, compare results,
    and process multiple vessels in batch mode.
    """
    pass


@cli.command('convert-aqwa')
@click.argument('analysis_folder', type=click.Path(exists=True))
@click.argument('vessel_name')
@click.option('--water-depth', '-d', type=float, required=True,
              help='Water depth in meters')
@click.option('--output', '-o', type=click.Path(), default='output',
              help='Output directory (default: output)')
@click.option('--validate/--no-validate', default=True,
              help='Run validation checks (default: True)')
@click.option('--formats', '-f', multiple=True,
              type=click.Choice(['all', 'vessel_type', 'rao_csv', 'added_mass_csv',
                                'damping_csv', 'excel', 'summary']),
              default=['all'],
              help='Export formats (default: all)')
def convert_aqwa(analysis_folder, vessel_name, water_depth, output, validate, formats):
    """
    Convert AQWA diffraction results to OrcaFlex format.

    ANALYSIS_FOLDER: Path to AQWA analysis folder containing .LIS files

    VESSEL_NAME: Name of vessel

    Example:
        diffraction convert-aqwa ./aqwa_analysis FPSO_A -d 1200 -o ./output
    """
    click.echo("=" * 80)
    click.echo("AQWA to OrcaFlex Conversion")
    click.echo("=" * 80)
    click.echo(f"Analysis folder: {analysis_folder}")
    click.echo(f"Vessel: {vessel_name}")
    click.echo(f"Water depth: {water_depth} m")
    click.echo(f"Output: {output}")
    click.echo()

    try:
        # Convert
        click.echo("Converting AQWA results...")
        converter = AQWAConverter(
            analysis_folder=Path(analysis_folder),
            vessel_name=vessel_name
        )

        results = converter.convert_to_unified_schema(water_depth=water_depth)
        click.echo(click.style("[OK] Conversion successful", fg='green'))

        # Validate
        if validate:
            click.echo("\nValidating results...")
            output_dir = Path(output)
            output_dir.mkdir(parents=True, exist_ok=True)

            validation_report = validate_results(
                results,
                output_file=output_dir / f"{vessel_name}_validation.json"
            )

            status = validation_report['overall_status']
            if status == 'PASS':
                click.echo(click.style(f"[OK] Validation: {status}", fg='green'))
            elif status == 'WARNING':
                click.echo(click.style(f"[WARNING] Validation: {status}", fg='yellow'))
            else:
                click.echo(click.style(f"[FAIL] Validation: {status}", fg='red'))

        # Export
        click.echo("\nExporting to OrcaFlex formats...")
        output_dir = Path(output)
        output_dir.mkdir(parents=True, exist_ok=True)

        exporter = OrcaFlexExporter(results, output_dir)

        if 'all' in formats:
            output_files = exporter.export_all()
        else:
            output_files = {}
            for fmt in formats:
                if fmt == 'vessel_type':
                    output_files['vessel_type'] = exporter.export_vessel_type()
                elif fmt == 'rao_csv':
                    output_files['rao_csv'] = exporter.export_raos_csv()
                elif fmt == 'added_mass_csv':
                    output_files['added_mass_csv'] = exporter.export_added_mass_csv()
                elif fmt == 'damping_csv':
                    output_files['damping_csv'] = exporter.export_damping_csv()
                elif fmt == 'excel':
                    output_files['excel'] = exporter.export_excel_workbook()
                elif fmt == 'summary':
                    output_files['summary'] = exporter.export_summary()

        click.echo(click.style(f"\n[OK] Exported {len(output_files)} files:", fg='green'))
        for fmt, path in output_files.items():
            click.echo(f"  - {fmt}: {path.name}")

        click.echo("\n" + "=" * 80)
        click.echo(click.style("Conversion Complete!", fg='green', bold=True))
        click.echo("=" * 80)
        click.echo(f"Output directory: {output_dir}")

    except Exception as e:
        click.echo(click.style(f"\n[ERROR] {str(e)}", fg='red', bold=True))
        if '--verbose' in sys.argv:
            import traceback
            traceback.print_exc()
        sys.exit(1)


@cli.command('convert-orcawave')
@click.argument('model_file', type=click.Path(exists=True))
@click.option('--vessel-name', '-v', type=str, default=None,
              help='Vessel name (auto-detect if not specified)')
@click.option('--water-depth', '-d', type=float, required=True,
              help='Water depth in meters')
@click.option('--output', '-o', type=click.Path(), default='output',
              help='Output directory (default: output)')
@click.option('--validate/--no-validate', default=True,
              help='Run validation checks (default: True)')
@click.option('--formats', '-f', multiple=True,
              type=click.Choice(['all', 'vessel_type', 'rao_csv', 'added_mass_csv',
                                'damping_csv', 'excel', 'summary']),
              default=['all'],
              help='Export formats (default: all)')
def convert_orcawave(model_file, vessel_name, water_depth, output, validate, formats):
    """
    Convert OrcaWave diffraction results to OrcaFlex format.

    MODEL_FILE: Path to OrcaFlex model file (.sim or .dat)

    Example:
        diffraction convert-orcawave model.sim -d 1200 -o ./output
        diffraction convert-orcawave model.sim -v FPSO_A -d 1200
    """
    if not ORCAWAVE_AVAILABLE:
        click.echo(click.style("[ERROR] OrcFxAPI not available. Install OrcaFlex with Python API.", fg='red', bold=True))
        sys.exit(1)

    click.echo("=" * 80)
    click.echo("OrcaWave to OrcaFlex Conversion")
    click.echo("=" * 80)
    click.echo(f"Model file: {model_file}")
    click.echo(f"Vessel: {vessel_name or 'auto-detect'}")
    click.echo(f"Water depth: {water_depth} m")
    click.echo(f"Output: {output}")
    click.echo()

    try:
        # Convert
        click.echo("Converting OrcaWave results...")
        converter = OrcaWaveConverter(
            model_file=Path(model_file),
            vessel_name=vessel_name
        )

        results = converter.convert_to_unified_schema(water_depth=water_depth)
        click.echo(click.style(f"[OK] Conversion successful (vessel: {results.vessel_name})", fg='green'))

        # Validate
        if validate:
            click.echo("\nValidating results...")
            output_dir = Path(output)
            output_dir.mkdir(parents=True, exist_ok=True)

            validation_report = validate_results(
                results,
                output_file=output_dir / f"{results.vessel_name}_validation.json"
            )

            status = validation_report['overall_status']
            if status == 'PASS':
                click.echo(click.style(f"[OK] Validation: {status}", fg='green'))
            elif status == 'WARNING':
                click.echo(click.style(f"[WARNING] Validation: {status}", fg='yellow'))
            else:
                click.echo(click.style(f"[FAIL] Validation: {status}", fg='red'))

        # Export
        click.echo("\nExporting to OrcaFlex formats...")
        output_dir = Path(output)
        output_dir.mkdir(parents=True, exist_ok=True)

        exporter = OrcaFlexExporter(results, output_dir)

        if 'all' in formats:
            output_files = exporter.export_all()
        else:
            output_files = {}
            for fmt in formats:
                if fmt == 'vessel_type':
                    output_files['vessel_type'] = exporter.export_vessel_type()
                elif fmt == 'rao_csv':
                    output_files['rao_csv'] = exporter.export_raos_csv()
                elif fmt == 'added_mass_csv':
                    output_files['added_mass_csv'] = exporter.export_added_mass_csv()
                elif fmt == 'damping_csv':
                    output_files['damping_csv'] = exporter.export_damping_csv()
                elif fmt == 'excel':
                    output_files['excel'] = exporter.export_excel_workbook()
                elif fmt == 'summary':
                    output_files['summary'] = exporter.export_summary()

        click.echo(click.style(f"\n[OK] Exported {len(output_files)} files:", fg='green'))
        for fmt, path in output_files.items():
            click.echo(f"  - {fmt}: {path.name}")

        click.echo("\n" + "=" * 80)
        click.echo(click.style("Conversion Complete!", fg='green', bold=True))
        click.echo("=" * 80)
        click.echo(f"Output directory: {output_dir}")

    except Exception as e:
        click.echo(click.style(f"\n[ERROR] {str(e)}", fg='red', bold=True))
        if '--verbose' in sys.argv:
            import traceback
            traceback.print_exc()
        sys.exit(1)


@cli.command('compare')
@click.argument('aqwa_folder', type=click.Path(exists=True))
@click.argument('orcawave_file', type=click.Path(exists=True))
@click.argument('vessel_name')
@click.option('--water-depth', '-d', type=float, required=True,
              help='Water depth in meters')
@click.option('--output', '-o', type=click.Path(), default='comparison',
              help='Output directory (default: comparison)')
@click.option('--tolerance', '-t', type=float, default=0.05,
              help='Comparison tolerance (default: 0.05 = 5%)')
def compare(aqwa_folder, orcawave_file, vessel_name, water_depth, output, tolerance):
    """
    Compare AQWA and OrcaWave diffraction results.

    AQWA_FOLDER: Path to AQWA analysis folder

    ORCAWAVE_FILE: Path to OrcaFlex model file

    VESSEL_NAME: Name of vessel

    Example:
        diffraction compare ./aqwa_analysis model.sim FPSO_A -d 1200
    """
    if not ORCAWAVE_AVAILABLE:
        click.echo(click.style("[ERROR] OrcFxAPI not available for OrcaWave comparison.", fg='red', bold=True))
        sys.exit(1)

    click.echo("=" * 80)
    click.echo("AQWA vs OrcaWave Comparison")
    click.echo("=" * 80)
    click.echo(f"AQWA folder: {aqwa_folder}")
    click.echo(f"OrcaWave file: {orcawave_file}")
    click.echo(f"Vessel: {vessel_name}")
    click.echo(f"Water depth: {water_depth} m")
    click.echo(f"Tolerance: {tolerance * 100}%")
    click.echo()

    try:
        # Convert AQWA
        click.echo("Converting AQWA results...")
        aqwa_converter = AQWAConverter(Path(aqwa_folder), vessel_name)
        aqwa_results = aqwa_converter.convert_to_unified_schema(water_depth)
        click.echo(click.style("[OK] AQWA conversion complete", fg='green'))

        # Convert OrcaWave
        click.echo("Converting OrcaWave results...")
        orcawave_converter = OrcaWaveConverter(Path(orcawave_file), vessel_name)
        orcawave_results = orcawave_converter.convert_to_unified_schema(water_depth)
        click.echo(click.style("[OK] OrcaWave conversion complete", fg='green'))

        # Compare
        click.echo("\nComparing results...")
        output_dir = Path(output)
        output_dir.mkdir(parents=True, exist_ok=True)

        report = compare_diffraction_results(
            aqwa_results,
            orcawave_results,
            output_dir,
            tolerance=tolerance
        )

        click.echo("\n" + "=" * 80)
        click.echo("Comparison Results")
        click.echo("=" * 80)
        click.echo(f"Overall Agreement: {report.overall_agreement}")
        click.echo(f"Comparison report: {output_dir / f'{vessel_name}_comparison.json'}")

        if report.notes:
            click.echo("\nNotes:")
            for note in report.notes:
                click.echo(f"  - {note}")

    except Exception as e:
        click.echo(click.style(f"\n[ERROR] {str(e)}", fg='red', bold=True))
        if '--verbose' in sys.argv:
            import traceback
            traceback.print_exc()
        sys.exit(1)


@cli.command('batch')
@click.argument('config_file', type=click.Path(exists=True))
def batch(config_file):
    """
    Batch process multiple vessels from configuration file.

    CONFIG_FILE: Path to JSON configuration file

    Example:
        diffraction batch config.json

    Config file format:
    {
      "configurations": [
        {
          "vessel_name": "FPSO_A",
          "source_type": "aqwa",
          "source_path": "./aqwa_analysis",
          "water_depth": 1200.0,
          "output_dir": "./output/FPSO_A"
        }
      ],
      "parallel": true,
      "max_workers": 4
    }
    """
    click.echo("=" * 80)
    click.echo("Batch Diffraction Processing")
    click.echo("=" * 80)
    click.echo(f"Configuration: {config_file}")
    click.echo()

    try:
        report = process_batch_from_config_file(Path(config_file))

        click.echo("\n" + "=" * 80)
        click.echo("Batch Processing Summary")
        click.echo("=" * 80)
        click.echo(f"Total configurations: {report.total_configurations}")
        click.echo(click.style(f"Successful: {report.successful}", fg='green'))
        if report.warnings > 0:
            click.echo(click.style(f"Warnings: {report.warnings}", fg='yellow'))
        if report.failed > 0:
            click.echo(click.style(f"Failed: {report.failed}", fg='red'))
        click.echo(f"Total duration: {report.total_duration:.2f} seconds")

    except Exception as e:
        click.echo(click.style(f"\n[ERROR] {str(e)}", fg='red', bold=True))
        if '--verbose' in sys.argv:
            import traceback
            traceback.print_exc()
        sys.exit(1)


# ---------------------------------------------------------------------------
# WRK-061: Spec converter commands
# ---------------------------------------------------------------------------

@cli.command("convert-spec")
@click.argument("spec_path", type=click.Path(exists=True))
@click.option(
    "--solver",
    "-s",
    type=click.Choice(["aqwa", "orcawave", "all"], case_sensitive=False),
    default="all",
    help="Target solver (default: all)",
)
@click.option(
    "--format",
    "-f",
    "fmt",
    type=click.Choice(["single", "modular"], case_sensitive=False),
    default="single",
    help="Output format (default: single)",
)
@click.option(
    "--output",
    "-o",
    type=click.Path(),
    default="output",
    help="Output directory (default: output)",
)
def convert_spec(spec_path, solver, fmt, output):
    """Convert a DiffractionSpec YAML to solver input files.

    SPEC_PATH: Path to a spec.yml file conforming to DiffractionSpec schema.

    Example:
        diffraction convert-spec analysis.yml --solver aqwa --format single
        diffraction convert-spec analysis.yml --solver all -o ./output
    """
    from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

    click.echo("=" * 80)
    click.echo("Spec Converter")
    click.echo("=" * 80)
    click.echo(f"Spec file : {spec_path}")
    click.echo(f"Solver    : {solver}")
    click.echo(f"Format    : {fmt}")
    click.echo(f"Output    : {output}")
    click.echo()

    try:
        converter = SpecConverter(Path(spec_path))

        if solver == "all":
            results = converter.convert_all(format=fmt, output_dir=Path(output))
            click.echo(click.style("[OK] Conversion complete:", fg="green"))
            for name, path in results.items():
                click.echo(f"  {name}: {path}")
        else:
            result_path = converter.convert(
                solver=solver, format=fmt, output_dir=Path(output)
            )
            click.echo(click.style("[OK] Conversion complete:", fg="green"))
            click.echo(f"  {solver}: {result_path}")

    except Exception as e:
        click.echo(click.style(f"\n[ERROR] {e}", fg="red", bold=True))
        sys.exit(1)


@cli.command("validate-spec")
@click.argument("spec_path", type=click.Path(exists=True))
def validate_spec(spec_path):
    """Validate a DiffractionSpec YAML file and report issues.

    SPEC_PATH: Path to a spec.yml file conforming to DiffractionSpec schema.

    Example:
        diffraction validate-spec analysis.yml
    """
    from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

    click.echo("=" * 80)
    click.echo("Spec Validator")
    click.echo("=" * 80)
    click.echo(f"Spec file: {spec_path}")
    click.echo()

    try:
        converter = SpecConverter(Path(spec_path))
        issues = converter.validate()

        if issues:
            click.echo(click.style(f"[WARN] {len(issues)} issue(s) found:", fg="yellow"))
            for issue in issues:
                click.echo(f"  - {issue}")
            sys.exit(1)
        else:
            click.echo(click.style("[OK] Spec is valid.", fg="green"))

    except Exception as e:
        click.echo(click.style(f"\n[ERROR] {e}", fg="red", bold=True))
        sys.exit(1)


# ---------------------------------------------------------------------------
# WRK-029: OrcaWave runner command
# ---------------------------------------------------------------------------


@cli.command("run-orcawave")
@click.argument("spec_path", type=click.Path(exists=True))
@click.option(
    "--output",
    "-o",
    type=click.Path(),
    default="output",
    help="Output directory (default: output)",
)
@click.option(
    "--dry-run",
    is_flag=True,
    default=False,
    help="Generate files only; do not invoke the solver.",
)
@click.option(
    "--timeout",
    "-t",
    type=int,
    default=7200,
    help="Solver timeout in seconds (default: 7200)",
)
@click.option(
    "--executable",
    type=click.Path(),
    default=None,
    help="Explicit path to OrcaWave executable.",
)
@click.option(
    "--modular/--no-modular",
    default=True,
    help="Generate modular section files alongside (default: --modular)",
)
def run_orcawave_cmd(spec_path, output, dry_run, timeout, executable, modular):
    """Run an OrcaWave diffraction analysis from a DiffractionSpec YAML.

    SPEC_PATH: Path to a spec.yml file conforming to DiffractionSpec schema.

    Example:\n
        diffraction run-orcawave analysis.yml --dry-run\n
        diffraction run-orcawave analysis.yml -o ./results --timeout 3600
    """
    from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
        OrcaWaveRunner,
        RunConfig,
    )

    click.echo("=" * 80)
    click.echo("OrcaWave Runner")
    click.echo("=" * 80)
    click.echo(f"Spec file  : {spec_path}")
    click.echo(f"Output     : {output}")
    click.echo(f"Dry run    : {dry_run}")
    click.echo(f"Timeout    : {timeout}s")
    click.echo(f"Executable : {executable or 'auto-detect'}")
    click.echo(f"Modular    : {modular}")
    click.echo()

    try:
        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            DiffractionSpec,
        )

        spec = DiffractionSpec.from_yaml(Path(spec_path))
        config = RunConfig(
            executable_path=Path(executable) if executable else None,
            output_dir=Path(output),
            dry_run=dry_run,
            timeout_seconds=timeout,
            generate_modular=modular,
        )
        runner = OrcaWaveRunner(config)
        result = runner.run(spec, spec_path=Path(spec_path))

        click.echo(click.style(f"[{result.status.value.upper()}]", fg="green" if result.status.value in ("completed", "dry_run") else "red"))
        click.echo(f"  Input file : {result.input_file}")
        click.echo(f"  Output dir : {result.output_dir}")
        if result.modular_files:
            click.echo(f"  Modular    : {len(result.modular_files)} section files")
        if result.mesh_files:
            click.echo(f"  Mesh files : {len(result.mesh_files)} copied")
        if result.duration_seconds is not None:
            click.echo(f"  Duration   : {result.duration_seconds:.2f}s")
        if result.error_message:
            click.echo(click.style(f"  Error      : {result.error_message}", fg="red"))

    except Exception as e:
        click.echo(click.style(f"\n[ERROR] {e}", fg="red", bold=True))
        sys.exit(1)


# ---------------------------------------------------------------------------
# WRK-030: Batch runner + RAO plotter commands
# ---------------------------------------------------------------------------


@cli.command("batch-orcawave")
@click.argument("config_path", type=click.Path(exists=True))
@click.option(
    "--output",
    "-o",
    type=click.Path(),
    default=None,
    help="Override base output directory.",
)
@click.option(
    "--mode",
    type=click.Choice(["sequential", "parallel"], case_sensitive=False),
    default=None,
    help="Execution mode (default: from config or sequential).",
)
@click.option(
    "--dry-run",
    is_flag=True,
    default=False,
    help="Run all jobs in dry-run mode.",
)
@click.option(
    "--max-workers",
    "-w",
    type=int,
    default=None,
    help="Max parallel workers (default: from config or 4).",
)
def batch_orcawave_cmd(config_path, output, mode, dry_run, max_workers):
    """Run OrcaWave batch analysis from a YAML configuration file.

    CONFIG_PATH: Path to a batch configuration YAML file.

    Example:\n
        diffraction batch-orcawave batch.yml --dry-run\n
        diffraction batch-orcawave batch.yml --mode parallel --max-workers 8
    """
    from digitalmodel.hydrodynamics.diffraction.orcawave_batch_runner import (
        OrcaWaveBatchConfig,
        OrcaWaveBatchRunner,
        ExecutionMode,
    )

    click.echo("=" * 80)
    click.echo("OrcaWave Batch Runner")
    click.echo("=" * 80)
    click.echo(f"Config     : {config_path}")
    click.echo(f"Output     : {output or 'from config'}")
    click.echo(f"Mode       : {mode or 'from config'}")
    click.echo(f"Dry run    : {dry_run}")
    click.echo(f"Workers    : {max_workers or 'from config'}")
    click.echo()

    try:
        cfg = OrcaWaveBatchConfig.from_yaml(Path(config_path))

        if output:
            cfg.base_output_dir = Path(output)
        if mode:
            cfg.execution_mode = ExecutionMode(mode)
        if max_workers:
            cfg.max_workers = max_workers
        if dry_run:
            for job in cfg.jobs:
                job.dry_run = True
            cfg.run_config.dry_run = True

        runner = OrcaWaveBatchRunner(cfg)
        report = runner.run()

        click.echo("\n" + "=" * 80)
        click.echo("Batch Results")
        click.echo("=" * 80)
        click.echo(f"Total jobs  : {report.total_jobs}")
        click.echo(click.style(f"Successful  : {report.successful}", fg="green"))
        if report.failed > 0:
            click.echo(click.style(f"Failed      : {report.failed}", fg="red"))
        if report.skipped > 0:
            click.echo(f"Skipped     : {report.skipped}")
        click.echo(f"Duration    : {report.total_duration:.2f}s")

        # Export report
        report_path = cfg.base_output_dir / "batch_report.json"
        runner.export_report(report, report_path)
        click.echo(f"Report      : {report_path}")

    except Exception as e:
        click.echo(click.style(f"\n[ERROR] {e}", fg="red", bold=True))
        sys.exit(1)


@cli.command("plot-raos")
@click.argument("model_file", type=click.Path(exists=True))
@click.option(
    "--water-depth",
    "-d",
    type=float,
    required=True,
    help="Water depth in metres.",
)
@click.option(
    "--vessel-name",
    "-v",
    type=str,
    default=None,
    help="Vessel name (auto-detect if not specified).",
)
@click.option(
    "--output",
    "-o",
    type=click.Path(),
    default="plots",
    help="Output directory for HTML plots (default: plots).",
)
@click.option(
    "--x-axis",
    type=click.Choice(["period", "frequency"], case_sensitive=False),
    default="period",
    help="X-axis type (default: period).",
)
@click.option(
    "--headings",
    type=str,
    default=None,
    help="Comma-separated heading angles to plot, e.g. '0,45,90'.",
)
def plot_raos_cmd(model_file, water_depth, vessel_name, output, x_axis, headings):
    """Generate interactive RAO plots from an OrcaFlex model file.

    MODEL_FILE: Path to an OrcaFlex model file (.sim or .dat).

    Example:\n
        diffraction plot-raos model.sim -d 100\n
        diffraction plot-raos model.sim -d 100 --x-axis frequency --headings 0,45,90
    """
    if not ORCAWAVE_AVAILABLE:
        click.echo(click.style(
            "[ERROR] OrcFxAPI not available. Install OrcaFlex with Python API.",
            fg="red", bold=True,
        ))
        sys.exit(1)

    click.echo("=" * 80)
    click.echo("RAO Plotter")
    click.echo("=" * 80)
    click.echo(f"Model file  : {model_file}")
    click.echo(f"Water depth : {water_depth} m")
    click.echo(f"Vessel      : {vessel_name or 'auto-detect'}")
    click.echo(f"Output      : {output}")
    click.echo(f"X-axis      : {x_axis}")
    click.echo(f"Headings    : {headings or 'all'}")
    click.echo()

    try:
        from digitalmodel.hydrodynamics.diffraction.rao_plotter import RAOPlotter

        converter = OrcaWaveConverter(
            model_file=Path(model_file),
            vessel_name=vessel_name,
        )
        results = converter.convert_to_unified_schema(water_depth=water_depth)
        click.echo(click.style(
            f"[OK] Loaded results for {results.vessel_name}", fg="green",
        ))

        heading_list = None
        if headings:
            heading_list = [float(h.strip()) for h in headings.split(",")]

        plotter = RAOPlotter(results, Path(output), x_axis=x_axis)
        paths = plotter.plot_all() if heading_list is None else [
            plotter.plot_amplitude(headings=heading_list),
            plotter.plot_phase(headings=heading_list),
            plotter.plot_combined(headings=heading_list),
        ]

        click.echo(click.style(f"\n[OK] Generated {len(paths)} plots:", fg="green"))
        for p in paths:
            click.echo(f"  - {p}")

    except Exception as e:
        click.echo(click.style(f"\n[ERROR] {e}", fg="red", bold=True))
        sys.exit(1)


if __name__ == '__main__':
    cli()
