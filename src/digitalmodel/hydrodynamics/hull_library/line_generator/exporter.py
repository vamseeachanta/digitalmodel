"""
ABOUTME: Hull panel mesh exporter — Phase 4 of the hull panel generator.

Writes panelised hull geometry to:
  - GDF (WAMIT Geometry Definition Format) for BEMRosetta / WAMIT solvers
  - OrcaWave panel format (.dat) for Orcina OrcaWave solver
  - SVG cross-section visualisation for visual verification of line input

GDF format reference (WAMIT v7 manual, §4):
  Line 1: Title / comment
  Line 2: ULEN  GRAV
  Line 3: ISX   ISY  (symmetry flags)
  Line 4: NPAN
  Following lines: 4 × (X Y Z) per panel (12 lines per panel)

OrcaWave format reference (Orcina OrcaWave docs §B.2):
  Block header: NPAN
  Per panel: 4 lines of X Y Z vertex coordinates

AQWA mesh format is deferred until the solvers/aqwa/ module exists (see
WRK-039 and WRK-106 plan notes).
"""

from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
    from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
        HullLineDefinition,
    )


# ---------------------------------------------------------------------------
# GDF export
# ---------------------------------------------------------------------------


def export_gdf(mesh: "PanelMesh", path: str | Path) -> Path:
    """Write a PanelMesh to WAMIT GDF format.

    Args:
        mesh: PanelMesh with vertices and quad/tri panels.
        path: Output file path (any extension, ``.gdf`` recommended).

    Returns:
        Path to the written file.
    """
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)

    # Determine symmetry flags
    isx, isy = 0, 0
    if mesh.symmetry_plane:
        plane = mesh.symmetry_plane.lower()
        if "x" in plane:
            isx = 1
        if "y" in plane:
            isy = 1

    # Collect valid panels
    valid_panels = []
    for panel in mesh.panels:
        indices = [int(v) for v in panel if int(v) >= 0]
        if len(indices) >= 3:
            valid_panels.append(indices)

    with open(path, "w") as f:
        # Line 1: title
        f.write(f"Hull panel mesh — {mesh.name}\n")
        # Line 2: ULEN GRAV
        f.write("1.0  9.81\n")
        # Line 3: ISX ISY
        f.write(f"{isx}  {isy}\n")
        # Line 4: NPAN
        f.write(f"{len(valid_panels)}\n")
        # Panel vertex blocks
        for indices in valid_panels:
            for k in range(4):
                vi = indices[k] if k < len(indices) else indices[-1]
                v = mesh.vertices[vi]
                f.write(f"  {v[0]:.8f}  {v[1]:.8f}  {v[2]:.8f}\n")

    return path


# ---------------------------------------------------------------------------
# OrcaWave export
# ---------------------------------------------------------------------------


def export_orcawave(mesh: "PanelMesh", path: str | Path) -> Path:
    """Write a PanelMesh to OrcaWave panel format.

    The OrcaWave panel file format uses a simple block structure:
      - Header line: ``NPAN <n>``
      - For each panel: 4 lines of ``X Y Z`` vertex coordinates

    Args:
        mesh: PanelMesh with vertices and quad/tri panels.
        path: Output file path (any extension, ``.dat`` recommended).

    Returns:
        Path to the written file.
    """
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)

    valid_panels = []
    for panel in mesh.panels:
        indices = [int(v) for v in panel if int(v) >= 0]
        if len(indices) >= 3:
            valid_panels.append(indices)

    with open(path, "w") as f:
        f.write(f"NPAN {len(valid_panels)}\n")
        for indices in valid_panels:
            for k in range(4):
                vi = indices[k] if k < len(indices) else indices[-1]
                v = mesh.vertices[vi]
                f.write(f"  {v[0]:.8f}  {v[1]:.8f}  {v[2]:.8f}\n")

    return path


# ---------------------------------------------------------------------------
# SVG cross-section visualisation
# ---------------------------------------------------------------------------

_COLORS = {
    "background": "#ffffff",
    "waterline": "#00bcd4",
    "section": "#1a3a5c",
    "text": "#333333",
    "grid": "#e0e0e0",
}


def export_sections_svg(
    defn: "HullLineDefinition",
    path: str | Path,
    *,
    width: int = 800,
    height: int = 400,
    max_stations: int = 10,
) -> Path:
    """Export hull cross-sections as an SVG body-plan diagram.

    Draws each station's (y, z) profile overlaid in a single body-plan
    view for visual verification of the line input.

    Args:
        defn: Hull line definition with station offsets.
        path: Output SVG file path.
        width: SVG viewport width in pixels.
        height: SVG viewport height in pixels.
        max_stations: Maximum number of stations to draw (evenly sampled
            if *defn* has more).

    Returns:
        Path to the written SVG file.
    """
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)

    margin = 40
    plot_w = width - 2 * margin
    plot_h = height - 2 * margin

    # Determine extents
    half_beam = defn.beam / 2.0
    draft = defn.draft

    def to_px(y: float, z: float) -> tuple[float, float]:
        """Map (y, z) in metres to SVG pixel coordinates.

        y: 0 at centreline, half_beam at outer edge — maps to x pixels.
        z: 0 at keel, draft at WL (keel-up) — maps to y pixels (inverted).
        """
        px = margin + (y / max(half_beam, 1e-6)) * (plot_w / 2)
        py = margin + plot_h - (z / max(draft, 1e-6)) * plot_h
        return px, py

    lines: list[str] = [
        f'<svg xmlns="http://www.w3.org/2000/svg" '
        f'width="{width}" height="{height}" '
        f'viewBox="0 0 {width} {height}">',
        f'<rect width="{width}" height="{height}" '
        f'fill="{_COLORS["background"]}"/>',
    ]

    # Waterline
    wl_y0, wl_py = to_px(0.0, draft)
    wl_y1, _ = to_px(half_beam, draft)
    lines.append(
        f'<line x1="{margin}" y1="{wl_py:.1f}" '
        f'x2="{margin + plot_w:.1f}" y2="{wl_py:.1f}" '
        f'stroke="{_COLORS["waterline"]}" stroke-width="1.5" '
        f'stroke-dasharray="4,4"/>'
    )

    # Sample stations
    stations = defn.stations
    if len(stations) > max_stations:
        step = len(stations) // max_stations
        stations = stations[::step][:max_stations]

    n_stations = len(stations)
    colors = [
        f"hsl({int(360 * k / max(n_stations - 1, 1))},70%,40%)"
        for k in range(n_stations)
    ]

    for si, station in enumerate(stations):
        offsets_sorted = sorted(station.offsets, key=lambda p: p[0])
        pts_px = [to_px(y, z) for z, y in offsets_sorted]

        if len(pts_px) < 2:
            continue

        # Draw polyline for starboard section
        coords = " ".join(f"{px:.1f},{py:.1f}" for px, py in pts_px)
        lines.append(
            f'<polyline points="{coords}" '
            f'fill="none" stroke="{colors[si]}" '
            f'stroke-width="1.5"/>'
        )
        # Mirror to port (negative y maps to left of centreline)
        port_pts = [(to_px(-y_out, py_out) if False else (2 * margin + plot_w / 2 - (px_out - (margin + plot_w / 2)), py_out)) for px_out, py_out in pts_px]
        # Simpler: reflect about centreline px
        cx_px = margin + plot_w / 2
        port_coords = " ".join(
            f"{2 * cx_px - px:.1f},{py:.1f}" for px, py in pts_px
        )
        lines.append(
            f'<polyline points="{port_coords}" '
            f'fill="none" stroke="{colors[si]}" '
            f'stroke-width="1.5" stroke-dasharray="3,2"/>'
        )

    # Centreline
    lines.append(
        f'<line x1="{margin + plot_w / 2:.1f}" y1="{margin}" '
        f'x2="{margin + plot_w / 2:.1f}" y2="{margin + plot_h}" '
        f'stroke="{_COLORS["grid"]}" stroke-width="1"/>'
    )

    # Title
    lines.append(
        f'<text x="{width // 2}" y="18" '
        f'text-anchor="middle" font-size="12" '
        f'fill="{_COLORS["text"]}">'
        f'Body Plan — {defn.name}</text>'
    )

    lines.append("</svg>")

    path.write_text("\n".join(lines))
    return path


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    "export_gdf",
    "export_orcawave",
    "export_sections_svg",
]
