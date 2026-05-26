"""Command-line interface for the GIS imagery-timelapse pipeline.

Exposes the pipeline stages (``prepare-aoi``, ``probe``, ``render``) and an
end-to-end ``run`` command, all driven by an imagery manifest file.  Registered
as the ``imagery-timelapse`` console script.
"""

from __future__ import annotations

import json
import logging

import click

from digitalmodel.gis.imagery.aoi import prepare_all
from digitalmodel.gis.imagery.manifest import ImageryManifest
from digitalmodel.gis.imagery.renderer import render_all
from digitalmodel.gis.imagery.stac_client import probe_from_metadata

_MANIFEST_OPTION = click.option(
    "--manifest",
    "manifest_path",
    required=True,
    type=click.Path(exists=True, dir_okay=False),
    help="Path to the imagery manifest (.yaml/.yml/.json).",
)


@click.group()
@click.option("-v", "--verbose", is_flag=True, help="Enable info-level logging.")
def cli(verbose: bool) -> None:
    """Generate historical-imagery timelapse artifacts from an address manifest."""
    logging.basicConfig(level=logging.INFO if verbose else logging.WARNING)


@cli.command("prepare-aoi")
@_MANIFEST_OPTION
def prepare_aoi_cmd(manifest_path: str) -> None:
    """Build point/property/neighborhood AOI GeoJSON for every address."""
    manifest = ImageryManifest.from_file(manifest_path)
    click.echo(json.dumps(prepare_all(manifest), indent=2))


@cli.command("probe")
@_MANIFEST_OPTION
def probe_cmd(manifest_path: str) -> None:
    """Probe Earth Engine + Planetary Computer STAC coverage per address."""
    manifest = ImageryManifest.from_file(manifest_path)
    results = []
    for address in manifest.addresses:
        meta_path = manifest.output_root / address.slug / "manifests" / f"{address.slug}-aoi-metadata.json"
        results.append(probe_from_metadata(meta_path))
    click.echo(json.dumps(results, indent=2))


@cli.command("render")
@_MANIFEST_OPTION
@click.option("--max-years", default=8, show_default=True, help="Maximum number of yearly frames.")
def render_cmd(manifest_path: str, max_years: int) -> None:
    """Render GIF/MP4/contact-sheet preview artifacts for every address."""
    manifest = ImageryManifest.from_file(manifest_path)
    click.echo(json.dumps(render_all(manifest, max_years=max_years), indent=2))


@cli.command("run")
@_MANIFEST_OPTION
@click.option("--max-years", default=8, show_default=True, help="Maximum number of yearly frames.")
@click.option("--probe/--no-probe", default=True, show_default=True, help="Run the imagery-access probe stage.")
def run_cmd(manifest_path: str, max_years: int, probe: bool) -> None:
    """Run the full pipeline: prepare AOIs, (optionally) probe, then render."""
    manifest = ImageryManifest.from_file(manifest_path)
    prepare_all(manifest)
    if probe:
        for address in manifest.addresses:
            meta_path = manifest.output_root / address.slug / "manifests" / f"{address.slug}-aoi-metadata.json"
            probe_from_metadata(meta_path)
    click.echo(json.dumps(render_all(manifest, max_years=max_years), indent=2))


def main() -> None:
    """Console-script entry point."""
    cli()


if __name__ == "__main__":
    main()
