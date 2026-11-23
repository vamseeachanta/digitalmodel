"""
ABOUTME: Schematic generator producing SVG views (profile, plan, body plan)
of hull profiles for documentation and visual verification.

Generates three standard naval architecture views as SVG strings:
- Profile view (side elevation): x along length, z along height
- Plan view (waterplane from above): x along length, y along breadth
- Body plan (transverse cross-sections): y along breadth, z along height

Uses only the Python standard library for SVG generation.
"""

from __future__ import annotations

from pathlib import Path
from typing import Optional

from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile


# ---------------------------------------------------------------------------
# Color scheme
# ---------------------------------------------------------------------------

COLORS = {
    "hull": "#1a3a5c",
    "waterline": "#00bcd4",
    "grid": "#e0e0e0",
    "text": "#333333",
    "frame": "#666666",
    "station": "#8899aa",
    "background": "#ffffff",
}


# ---------------------------------------------------------------------------
# SVG helper functions
# ---------------------------------------------------------------------------


def _svg_header(width: int, height: int) -> str:
    return (
        f'<svg xmlns="http://www.w3.org/2000/svg" '
        f'width="{width}" height="{height}" '
        f'viewBox="0 0 {width} {height}">\n'
        f'<rect width="{width}" height="{height}" '
        f'fill="{COLORS["background"]}"/>\n'
    )


def _svg_footer() -> str:
    return "</svg>"


def _svg_line(
    x1: float,
    y1: float,
    x2: float,
    y2: float,
    color: str,
    width: float = 1.0,
    dash: Optional[str] = None,
) -> str:
    style = f'stroke="{color}" stroke-width="{width}" fill="none"'
    if dash:
        style += f' stroke-dasharray="{dash}"'
    return f'<line x1="{x1:.1f}" y1="{y1:.1f}" x2="{x2:.1f}" y2="{y2:.1f}" {style}/>\n'


def _svg_polyline(
    points: list[tuple[float, float]],
    color: str,
    width: float = 1.5,
    fill: str = "none",
    close: bool = False,
) -> str:
    pts = " ".join(f"{x:.1f},{y:.1f}" for x, y in points)
    tag = "polygon" if close else "polyline"
    return (
        f'<{tag} points="{pts}" '
        f'stroke="{color}" stroke-width="{width}" fill="{fill}"/>\n'
    )


def _svg_text(
    x: float,
    y: float,
    text: str,
    color: str = COLORS["text"],
    size: int = 12,
    anchor: str = "middle",
    rotate: Optional[float] = None,
) -> str:
    transform = ""
    if rotate is not None:
        transform = f' transform="rotate({rotate},{x:.1f},{y:.1f})"'
    return (
        f'<text x="{x:.1f}" y="{y:.1f}" '
        f'font-family="sans-serif" font-size="{size}" '
        f'fill="{color}" text-anchor="{anchor}"{transform}>'
        f"{text}</text>\n"
    )


def _svg_rect(
    x: float,
    y: float,
    w: float,
    h: float,
    color: str,
    width: float = 1.0,
    fill: str = "none",
) -> str:
    return (
        f'<rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" height="{h:.1f}" '
        f'stroke="{color}" stroke-width="{width}" fill="{fill}"/>\n'
    )


# ---------------------------------------------------------------------------
# Coordinate mapping
# ---------------------------------------------------------------------------


class _CoordMapper:
    """Maps from world coordinates to SVG pixel coordinates."""

    def __init__(
        self,
        world_x_min: float,
        world_x_max: float,
        world_y_min: float,
        world_y_max: float,
        svg_x_min: float,
        svg_x_max: float,
        svg_y_min: float,
        svg_y_max: float,
    ) -> None:
        self.wx_min = world_x_min
        self.wx_max = world_x_max
        self.wy_min = world_y_min
        self.wy_max = world_y_max
        self.sx_min = svg_x_min
        self.sx_max = svg_x_max
        self.sy_min = svg_y_min
        self.sy_max = svg_y_max

        wx_range = self.wx_max - self.wx_min
        wy_range = self.wy_max - self.wy_min
        self._sx_scale = (self.sx_max - self.sx_min) / wx_range if wx_range else 1.0
        self._sy_scale = (self.sy_max - self.sy_min) / wy_range if wy_range else 1.0

    def x(self, world_x: float) -> float:
        return self.sx_min + (world_x - self.wx_min) * self._sx_scale

    def y(self, world_y: float) -> float:
        """SVG y is inverted (0 at top), so higher world y maps to lower SVG y."""
        return self.sy_max - (world_y - self.wy_min) * self._sy_scale


# ---------------------------------------------------------------------------
# Grid drawing
# ---------------------------------------------------------------------------


def _draw_grid(
    mapper: _CoordMapper,
    n_x: int,
    n_y: int,
    x_label: str,
    y_label: str,
    x_unit: str = "m",
    y_unit: str = "m",
) -> str:
    """Draw grid lines and axis labels."""
    svg = ""
    wx_range = mapper.wx_max - mapper.wx_min
    wy_range = mapper.wy_max - mapper.wy_min

    # Vertical grid lines
    for i in range(n_x + 1):
        wx = mapper.wx_min + i * wx_range / n_x
        sx = mapper.x(wx)
        svg += _svg_line(sx, mapper.sy_min, sx, mapper.sy_max, COLORS["grid"], 0.5)
        label = f"{wx:.1f}"
        svg += _svg_text(sx, mapper.sy_max + 18, label, size=9)

    # Horizontal grid lines
    for i in range(n_y + 1):
        wy = mapper.wy_min + i * wy_range / n_y
        sy = mapper.y(wy)
        svg += _svg_line(mapper.sx_min, sy, mapper.sx_max, sy, COLORS["grid"], 0.5)
        label = f"{wy:.1f}"
        svg += _svg_text(mapper.sx_min - 8, sy + 3, label, size=9, anchor="end")

    # Axis labels
    mid_x = (mapper.sx_min + mapper.sx_max) / 2
    svg += _svg_text(mid_x, mapper.sy_max + 35, f"{x_label} ({x_unit})", size=11)
    mid_y = (mapper.sy_min + mapper.sy_max) / 2
    svg += _svg_text(
        mapper.sx_min - 40,
        mid_y,
        f"{y_label} ({y_unit})",
        size=11,
        rotate=-90,
    )

    return svg


# ---------------------------------------------------------------------------
# SchematicGenerator
# ---------------------------------------------------------------------------


class SchematicGenerator:
    """Generates standard naval architecture SVG views from hull profiles.

    Produces three views:
    - profile_view: side elevation (x along length, z along height)
    - plan_view: waterplane from above (x along length, y along breadth)
    - body_plan: transverse cross-sections (y along breadth, z along height)
    """

    def __init__(
        self,
        width: int = 800,
        height: int = 400,
        margin: int = 60,
    ) -> None:
        self.width = width
        self.height = height
        self.margin = margin

    def _plot_area(self) -> tuple[float, float, float, float]:
        """Return the SVG pixel bounds for the plot area (left, right, top, bottom)."""
        return (
            float(self.margin),
            float(self.width - self.margin),
            float(self.margin),
            float(self.height - self.margin),
        )

    # ---- Profile view ----

    def profile_view(self, profile: HullProfile) -> str:
        """Side elevation view: x along hull length, z along height.

        Shows hull outline, waterline, keel line, and station positions.
        """
        sx_min, sx_max, sy_min, sy_max = self._plot_area()

        stations = sorted(profile.stations, key=lambda s: s.x_position)
        x_min = stations[0].x_position
        x_max = stations[-1].x_position

        # World z range: 0 (keel) to depth
        z_min = 0.0
        z_max = profile.depth

        mapper = _CoordMapper(x_min, x_max, z_min, z_max, sx_min, sx_max, sy_min, sy_max)

        svg = _svg_header(self.width, self.height)

        # Title
        svg += _svg_text(
            self.width / 2, 20, f"Profile View - {profile.name}", size=14
        )

        # Grid
        svg += _draw_grid(mapper, 5, 4, "Length (x)", "Height (z)")

        # Frame
        svg += _svg_rect(
            sx_min, sy_min, sx_max - sx_min, sy_max - sy_min, COLORS["frame"]
        )

        # Build deck and keel outlines from stations
        deck_pts: list[tuple[float, float]] = []
        keel_pts: list[tuple[float, float]] = []

        for station in stations:
            offsets = sorted(station.waterline_offsets, key=lambda p: p[0])
            z_keel = offsets[0][0]
            z_deck = offsets[-1][0]
            deck_pts.append((mapper.x(station.x_position), mapper.y(z_deck)))
            keel_pts.append((mapper.x(station.x_position), mapper.y(z_keel)))

        # Hull outline: deck forward, then keel backward to close
        outline = deck_pts + list(reversed(keel_pts))
        svg += _svg_polyline(outline, COLORS["hull"], width=2.0, close=True)

        # Station lines (vertical dashed)
        for station in stations:
            sx = mapper.x(station.x_position)
            svg += _svg_line(sx, sy_min, sx, sy_max, COLORS["station"], 0.5, dash="4,3")

        # Waterline
        wl_y = mapper.y(profile.draft)
        svg += _svg_line(sx_min, wl_y, sx_max, wl_y, COLORS["waterline"], 1.5, dash="8,4")
        svg += _svg_text(
            sx_max + 5,
            wl_y + 4,
            "WL (waterline)",
            color=COLORS["waterline"],
            size=10,
            anchor="start",
        )

        svg += _svg_footer()
        return svg

    # ---- Plan view ----

    def plan_view(self, profile: HullProfile) -> str:
        """Waterplane view from above: x along hull length, y along breadth.

        For each station, plots +/- half-breadth at design draft (or nearest z).
        """
        sx_min, sx_max, sy_min, sy_max = self._plot_area()

        stations = sorted(profile.stations, key=lambda s: s.x_position)
        x_min = stations[0].x_position
        x_max = stations[-1].x_position
        half_beam = profile.beam / 2.0

        mapper = _CoordMapper(
            x_min, x_max, -half_beam, half_beam, sx_min, sx_max, sy_min, sy_max
        )

        svg = _svg_header(self.width, self.height)

        # Title
        svg += _svg_text(
            self.width / 2, 20, f"Plan View - {profile.name}", size=14
        )

        # Grid
        svg += _draw_grid(mapper, 5, 4, "Length (x)", "Half-breadth (y)")

        # Frame
        svg += _svg_rect(
            sx_min, sy_min, sx_max - sx_min, sy_max - sy_min, COLORS["frame"]
        )

        # For each station, find half-breadth at design draft (or nearest z)
        port_pts: list[tuple[float, float]] = []
        starboard_pts: list[tuple[float, float]] = []

        for station in stations:
            offsets = sorted(station.waterline_offsets, key=lambda p: p[0])
            # Find half-breadth at draft, or interpolate
            half_b = self._interpolate_half_breadth(offsets, profile.draft)
            sx = mapper.x(station.x_position)
            port_pts.append((sx, mapper.y(half_b)))
            starboard_pts.append((sx, mapper.y(-half_b)))

        # Draw waterplane outline (port side forward, starboard side backward)
        outline = port_pts + list(reversed(starboard_pts))
        svg += _svg_polyline(outline, COLORS["hull"], width=2.0, close=True)

        # Centerline
        cl_y = mapper.y(0.0)
        svg += _svg_line(sx_min, cl_y, sx_max, cl_y, COLORS["station"], 0.5, dash="4,3")
        svg += _svg_text(
            sx_max + 5,
            cl_y + 4,
            "CL",
            color=COLORS["station"],
            size=10,
            anchor="start",
        )

        svg += _svg_footer()
        return svg

    # ---- Body plan ----

    def body_plan(self, profile: HullProfile) -> str:
        """Transverse sections view: y along breadth, z along height.

        All stations overlaid. Convention: forward stations on the right half,
        aft stations on the left half (mirrored).
        """
        sx_min, sx_max, sy_min, sy_max = self._plot_area()

        stations = sorted(profile.stations, key=lambda s: s.x_position)
        half_beam = profile.beam / 2.0
        z_min = 0.0
        z_max = profile.depth
        mid_x = (stations[0].x_position + stations[-1].x_position) / 2.0

        mapper = _CoordMapper(
            -half_beam, half_beam, z_min, z_max, sx_min, sx_max, sy_min, sy_max
        )

        svg = _svg_header(self.width, self.height)

        # Title
        svg += _svg_text(
            self.width / 2, 20, f"Body Plan - {profile.name}", size=14
        )

        # Grid
        svg += _draw_grid(mapper, 4, 4, "Half-breadth (y)", "Height (z)")

        # Frame
        svg += _svg_rect(
            sx_min, sy_min, sx_max - sx_min, sy_max - sy_min, COLORS["frame"]
        )

        # Centerline
        cl_x = mapper.x(0.0)
        svg += _svg_line(cl_x, sy_min, cl_x, sy_max, COLORS["station"], 0.5, dash="4,3")

        # Waterline
        wl_y = mapper.y(profile.draft)
        svg += _svg_line(
            sx_min, wl_y, sx_max, wl_y, COLORS["waterline"], 1.0, dash="8,4"
        )
        svg += _svg_text(
            sx_max + 5,
            wl_y + 4,
            "WL",
            color=COLORS["waterline"],
            size=10,
            anchor="start",
        )

        # Draw each station as a polyline
        for station in stations:
            offsets = sorted(station.waterline_offsets, key=lambda p: p[0])
            is_forward = station.x_position >= mid_x

            # Build the section curve (z, y) -> SVG points
            section_pts: list[tuple[float, float]] = []
            for z_val, y_val in offsets:
                # Forward stations on right (+y), aft stations on left (-y)
                display_y = y_val if is_forward else -y_val
                section_pts.append((mapper.x(display_y), mapper.y(z_val)))

            if len(section_pts) >= 2:
                svg += _svg_polyline(section_pts, COLORS["hull"], width=1.5)

        svg += _svg_footer()
        return svg

    # ---- Batch generation ----

    def generate_all(self, profile: HullProfile) -> dict[str, str]:
        """Generate all three standard views.

        Returns a dict with keys: profile_view, plan_view, body_plan.
        """
        return {
            "profile_view": self.profile_view(profile),
            "plan_view": self.plan_view(profile),
            "body_plan": self.body_plan(profile),
        }

    def save_all(
        self, profile: HullProfile, output_dir: Path
    ) -> dict[str, Path]:
        """Save all three views as SVG files in the given directory.

        Returns a dict mapping view name to the saved file path.
        """
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        views = self.generate_all(profile)
        saved: dict[str, Path] = {}
        for name, svg_content in views.items():
            filepath = output_dir / f"{profile.name}_{name}.svg"
            filepath.write_text(svg_content)
            saved[name] = filepath

        return saved

    # ---- Helpers ----

    @staticmethod
    def _interpolate_half_breadth(
        offsets: list[tuple[float, float]], target_z: float
    ) -> float:
        """Find half-breadth at a target z by linear interpolation.

        offsets must be sorted by z (ascending).
        """
        if not offsets:
            return 0.0

        # Clamp to range
        if target_z <= offsets[0][0]:
            return offsets[0][1]
        if target_z >= offsets[-1][0]:
            return offsets[-1][1]

        # Linear interpolation between bounding entries
        for i in range(len(offsets) - 1):
            z0, y0 = offsets[i]
            z1, y1 = offsets[i + 1]
            if z0 <= target_z <= z1:
                if z1 == z0:
                    return y0
                t = (target_z - z0) / (z1 - z0)
                return y0 + t * (y1 - y0)

        return offsets[-1][1]


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    "SchematicGenerator",
    "COLORS",
]
