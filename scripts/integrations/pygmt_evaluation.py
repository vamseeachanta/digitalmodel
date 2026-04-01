"""PyGMT evaluation: Gulf of Mexico bathymetry with offshore platform markers.

Requires:
  - GMT system library: sudo apt-get install gmt gmt-gshhg-low gmt-gshhg-high
  - Python package: pygmt

Usage:
  uv run python scripts/integrations/pygmt_evaluation.py

Output:
  docs/integrations/pygmt-gulf-of-mexico.png

Note: PyGMT downloads earth relief data on first use.
  Cache location: ~/.gmt/server/
  Estimated cache size for this script: ~5-50 MB depending on resolution.
"""

import os
import shutil
import sys

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def check_gmt_installed():
    """Verify GMT system library is available."""
    if shutil.which("gmt") is None:
        print("ERROR: GMT system library not found.")
        print("Install with: sudo apt-get install gmt gmt-gshhg-low gmt-gshhg-high")
        print("Or via conda: conda install -c conda-forge gmt")
        sys.exit(1)


def plot_gulf_of_mexico_bathymetry():
    """Plot Gulf of Mexico bathymetry with coastlines and platform markers."""
    import pygmt

    print(f"PyGMT version: {pygmt.__version__}")

    # Gulf of Mexico bounding box
    region = [-98, -82, 18, 31]

    # Load earth relief (bathymetry + topography)
    # 02m = 2 arc-minute resolution (~3.7 km) — good balance of detail vs download
    print("Loading earth relief data (may download on first use)...")
    grid = pygmt.datasets.load_earth_relief(resolution="02m", region=region)
    print(f"Grid shape: {grid.shape}")
    print(f"Depth range: {float(grid.min()):.0f} m to {float(grid.max()):.0f} m")

    fig = pygmt.Figure()

    # Plot bathymetric relief
    fig.grdimage(
        grid=grid,
        projection="M20c",
        frame=["a2f1", "+tGulf of Mexico — Bathymetry & Offshore Platforms"],
        cmap="geo",
    )

    # Add coastlines
    fig.coast(
        shorelines="0.5p,black",
        borders=["1/0.5p,gray"],
        resolution="h",
    )

    # Offshore platform / field locations (approximate)
    platforms = {
        "Thunder Horse": (-89.0, 28.2),
        "Perdido": (-94.9, 26.1),
        "Mars-Ursa": (-89.7, 28.6),
        "Na Kika": (-89.6, 28.0),
        "Atlantis": (-90.0, 27.2),
        "Mad Dog": (-90.3, 27.0),
        "Stones": (-90.4, 27.8),
    }

    lons = [coord[0] for coord in platforms.values()]
    lats = [coord[1] for coord in platforms.values()]
    names = list(platforms.keys())

    # Plot platform markers
    fig.plot(
        x=lons,
        y=lats,
        style="t0.3c",  # triangle markers
        fill="red",
        pen="0.5p,black",
    )

    # Add labels
    for name, (lon, lat) in platforms.items():
        fig.text(
            x=lon,
            y=lat + 0.25,
            text=name,
            font="6p,Helvetica,black",
            justify="CB",
        )

    # Add colorbar
    fig.colorbar(
        frame=["a1000f500", "x+lElevation", "y+lm"],
        position="JBC+w15c/0.5c+o0/1c",
    )

    # Save output
    outdir = os.path.join(REPO_ROOT, "docs", "integrations")
    os.makedirs(outdir, exist_ok=True)
    outpath = os.path.join(outdir, "pygmt-gulf-of-mexico.png")
    fig.savefig(outpath, dpi=300)
    print(f"Saved: {outpath}")
    print(f"File size: {os.path.getsize(outpath) / 1024:.1f} KB")


if __name__ == "__main__":
    check_gmt_installed()
    plot_gulf_of_mexico_bathymetry()
