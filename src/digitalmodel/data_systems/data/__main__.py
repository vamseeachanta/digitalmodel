"""
ABOUTME: CLI entry point for data migration utilities.
Provides command-line interface for Excel to Parquet conversion and rollback.
"""

import sys
from pathlib import Path

from typing import Optional
import click
from loguru import logger

from digitalmodel.data.migration import (
    convert_excel_to_parquet,
    rollback_migration,
    scan_excel_files,
)


@click.group()
@click.option("--verbose", "-v", is_flag=True, help="Enable verbose logging")
def cli(verbose: bool):
    """Excel to Parquet migration utilities."""
    if verbose:
        logger.remove()
        logger.add(sys.stderr, level="DEBUG")
    else:
        logger.remove()
        logger.add(sys.stderr, level="INFO")


@cli.command()
@click.option(
    "--source",
    "-s",
    type=click.Path(exists=True, file_okay=False, path_type=Path),
    required=True,
    help="Source directory containing Excel files",
)
@click.option(
    "--output",
    "-o",
    type=click.Path(file_okay=False, path_type=Path),
    default=None,
    help="Output directory for Parquet files (default: SOURCE/parquet)",
)
@click.option(
    "--verify",
    is_flag=True,
    default=False,
    help="Verify data integrity after conversion",
)
@click.option(
    "--update-catalog",
    is_flag=True,
    default=False,
    help="Update catalog.yml with Parquet entries",
)
def convert(
    source: Path,
    output: Optional[Path],
    verify: bool,
    update_catalog: bool,
):
    """Convert Excel files to Parquet format."""
    click.echo(f"🔄 Starting migration from {source}")

    # Show preview of files to convert
    excel_files = scan_excel_files(source)
    click.echo(f"Found {len(excel_files)} Excel file(s):")
    for f in excel_files[:10]:  # Show first 10
        click.echo(f"  • {f.relative_to(source)}")
    if len(excel_files) > 10:
        click.echo(f"  ... and {len(excel_files) - 10} more")

    # Confirm before proceeding
    if not click.confirm("\nProceed with conversion?", default=True):
        click.echo("❌ Aborted.")
        return

    # Perform conversion
    report = convert_excel_to_parquet(
        source_dir=source,
        output_dir=output,
        verify=verify,
        update_catalog=update_catalog,
    )

    # Display results
    click.echo("\n" + "=" * 60)
    click.echo(str(report))
    click.echo("=" * 60)

    if report.failed_conversions > 0:
        click.echo(f"\n⚠️  {report.failed_conversions} file(s) failed:")
        for error in report.errors:
            click.echo(f"  • {error['file']}: {error['error']}")

    manifest_path = (output or source / "parquet") / "migration_manifest.json"
    click.echo(f"\n📋 Manifest saved to: {manifest_path}")

    if report.successful_conversions > 0:
        click.echo(f"✅ Successfully converted {report.successful_conversions} file(s)")
    else:
        click.echo("❌ No files were converted successfully")
        sys.exit(1)


@cli.command()
@click.option(
    "--manifest",
    "-m",
    type=click.Path(exists=True, dir_okay=False, path_type=Path),
    required=True,
    help="Path to migration manifest JSON",
)
def rollback(manifest: Path):
    """Rollback migration using manifest file."""
    click.echo(f"⏪ Rolling back migration from {manifest}")

    # Confirm before proceeding
    if not click.confirm("This will delete all Parquet files. Continue?", default=False):
        click.echo("❌ Aborted.")
        return

    # Perform rollback
    rollback_migration(manifest)

    click.echo("✅ Rollback complete. Original Excel files preserved.")


@cli.command()
@click.option(
    "--source",
    "-s",
    type=click.Path(exists=True, file_okay=False, path_type=Path),
    required=True,
    help="Directory to scan for Excel files",
)
def scan(source: Path):
    """Scan directory for Excel files."""
    excel_files = scan_excel_files(source)

    click.echo(f"Found {len(excel_files)} Excel file(s) in {source}:\n")

    total_size = 0
    for f in excel_files:
        size_mb = f.stat().st_size / (1024 ** 2)
        total_size += f.stat().st_size
        rel_path = f.relative_to(source) if f.is_relative_to(source) else f
        click.echo(f"  {rel_path} ({size_mb:.2f} MB)")

    click.echo(f"\nTotal size: {total_size / (1024 ** 2):.2f} MB")


if __name__ == "__main__":
    cli()
