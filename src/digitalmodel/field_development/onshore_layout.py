# ABOUTME: Onshore field-development tracer — YAML layout + terrain routing + screening + plot.
# ABOUTME: Issue #1508 (tracer slice of epic #1507). Synthetic DEM stands in for USGS 3DEP.
"""
digitalmodel.field_development.onshore_layout
==============================================

Tracer slice (#1508) of the onshore field-development epic (#1507): ONE demo
onshore field taken end-to-end —

  1. **YAML config**  — terrain grid spec, wells, flowlines, host pad, fluid
     properties, plot/scene output paths. ALL constants live in the YAML
     (see ``data/onshore_demo_field.yml``); nothing is hardcoded here.
  2. **Terrain**      — a small deterministic elevation grid. Either a
     ``synthetic`` analytic surface sampled onto a grid, or a ``csv_grid``
     bundled elevation table. Real USGS 3DEP DEM ingestion is deliberately
     OUT of this slice (that wiring is worldenergydata#930's deliverable).
  3. **Layout model** — typed asset/flowline objects placed on the terrain
     surface; straight-line plan routing with the flowline length computed
     OVER the terrain (3D length, not plan length).
  4. **Screening**    — Darcy-Weisbach frictional pressure drop per flowline
     with the Swamee-Jain explicit friction factor (laminar: f = 64/Re),
     plus the hydrostatic elevation term. See
     :func:`darcy_weisbach_pressure_drop` for the citation.
  5. **2D plot**      — deterministic matplotlib figure (terrain contours +
     assets + flowlines) saved to the path given in the config.
  6. **Scene export** — a JSON scene file consumed by the Blender fly-through
     script (``scripts/field_development/onshore_flythrough_bpy.py``); JSON so
     Blender's bundled Python needs no third-party packages.
  7. **API**          — :func:`screen_layout` is the single entry point:
     ``screen_layout(config_path) -> dict`` (layout summary + screening
     results + artifact paths).

Scope honesty
-------------
Screening-grade only: straight-line routing (no obstacle avoidance, no
least-cost path), single-phase incompressible pressure drop (no multiphase
flow assurance), synthetic terrain. Each of those is a follow-on workstream
of epic #1507, not this tracer.

Usage
-----
>>> from digitalmodel.field_development import screen_layout
>>> result = screen_layout(
...     "src/digitalmodel/field_development/data/onshore_demo_field.yml",
...     output_dir="outputs/field_development",
... )  # doctest: +SKIP
>>> result["flowlines"][0]["screening"]["dp_total_kpa"]  # doctest: +SKIP
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Optional

import matplotlib
import numpy as np
import yaml

matplotlib.use("Agg")  # non-interactive backend for file output
import matplotlib.pyplot as plt  # noqa: E402

from .schematics.renderer import render_figure  # noqa: E402

# --- physical constants ------------------------------------------------------
GRAVITY_M_PER_S2 = 9.80665
SECONDS_PER_DAY = 86_400.0
# Below this Reynolds number the laminar friction factor f = 64/Re applies.
LAMINAR_REYNOLDS_LIMIT = 2_300.0

_REQUIRED_SECTIONS = ("field", "terrain", "host", "wells", "flowlines", "fluid", "plot")


# ---------------------------------------------------------------------------
# Terrain
# ---------------------------------------------------------------------------


@dataclass
class TerrainGrid:
    """A small DEM-like elevation grid over a rectangular domain.

    ``z_m`` has shape ``(ny, nx)``; ``z_m[j, i]`` is the elevation at
    ``(x_m[i], y_m[j])``. Elevation queries use bilinear interpolation, the
    standard treatment of a gridded DEM between posts.
    """

    x_m: np.ndarray
    y_m: np.ndarray
    z_m: np.ndarray
    source: str = "synthetic"

    def elevation_at(self, x: float, y: float) -> float:
        """Bilinear-interpolated elevation at plan position (x, y) [m]."""
        xg, yg = self.x_m, self.y_m
        if not (xg[0] <= x <= xg[-1] and yg[0] <= y <= yg[-1]):
            raise ValueError(
                f"point ({x}, {y}) lies outside the terrain domain "
                f"x=[{xg[0]}, {xg[-1]}], y=[{yg[0]}, {yg[-1]}]"
            )
        i = int(np.clip(np.searchsorted(xg, x) - 1, 0, len(xg) - 2))
        j = int(np.clip(np.searchsorted(yg, y) - 1, 0, len(yg) - 2))
        tx = (x - xg[i]) / (xg[i + 1] - xg[i])
        ty = (y - yg[j]) / (yg[j + 1] - yg[j])
        z = self.z_m
        return float(
            z[j, i] * (1 - tx) * (1 - ty)
            + z[j, i + 1] * tx * (1 - ty)
            + z[j + 1, i] * (1 - tx) * ty
            + z[j + 1, i + 1] * tx * ty
        )


def _build_synthetic_terrain(spec: dict[str, Any]) -> TerrainGrid:
    """Deterministic analytic surface sampled onto a grid (DEM stand-in).

    z(x, y) = base + amplitude * sin(2*pi*x / x_wavelength) * cos(2*pi*y / y_wavelength)
    """
    x = np.linspace(float(spec["x_min_m"]), float(spec["x_max_m"]), int(spec["nx"]))
    y = np.linspace(float(spec["y_min_m"]), float(spec["y_max_m"]), int(spec["ny"]))
    xx, yy = np.meshgrid(x, y)
    z = float(spec["base_elevation_m"]) + float(spec["amplitude_m"]) * np.sin(
        2.0 * math.pi * xx / float(spec["x_wavelength_m"])
    ) * np.cos(2.0 * math.pi * yy / float(spec["y_wavelength_m"]))
    return TerrainGrid(x_m=x, y_m=y, z_m=z, source="synthetic")


def _build_csv_terrain(spec: dict[str, Any], base_dir: Path) -> TerrainGrid:
    """Elevation grid from a bundled CSV table of z values (row = one y line)."""
    csv_path = Path(spec["csv_path"])
    if not csv_path.is_absolute():
        csv_path = base_dir / csv_path
    z = np.loadtxt(csv_path, delimiter=",", ndmin=2)
    ny, nx = z.shape
    x = np.linspace(float(spec["x_min_m"]), float(spec["x_max_m"]), nx)
    y = np.linspace(float(spec["y_min_m"]), float(spec["y_max_m"]), ny)
    return TerrainGrid(x_m=x, y_m=y, z_m=z, source=f"csv_grid:{csv_path.name}")


def build_terrain(spec: dict[str, Any], base_dir: Path) -> TerrainGrid:
    """Build the terrain grid described by the config ``terrain`` section."""
    kind = spec.get("kind")
    if kind == "synthetic":
        return _build_synthetic_terrain(spec)
    if kind == "csv_grid":
        return _build_csv_terrain(spec, base_dir)
    raise ValueError(f"terrain.kind must be 'synthetic' or 'csv_grid', got {kind!r}")


# ---------------------------------------------------------------------------
# Layout model
# ---------------------------------------------------------------------------


@dataclass
class Asset:
    """A surface asset placed on the terrain (well pad or host/processing pad)."""

    asset_id: str
    name: str
    kind: str  # "well" | "host"
    x_m: float
    y_m: float
    z_m: float
    rate_m3_per_day: float = 0.0


@dataclass
class FlowlineRoute:
    """A routed flowline: straight in plan, draped over the terrain surface."""

    flowline_id: str
    from_id: str
    to_id: str
    inner_diameter_m: float
    roughness_m: float
    rate_m3_per_day: float
    path_xyz_m: np.ndarray = field(repr=False)  # (n, 3) sampled polyline
    plan_length_m: float = 0.0
    terrain_length_m: float = 0.0
    elevation_change_m: float = 0.0  # z_to - z_from


@dataclass
class FieldLayout:
    """Typed layout of one onshore field on its terrain."""

    name: str
    terrain: TerrainGrid
    host: Asset
    wells: list[Asset]
    flowlines: list[FlowlineRoute]


def load_field_config(config_path: str | Path) -> dict[str, Any]:
    """Load and validate the onshore field YAML config."""
    path = Path(config_path)
    with open(path, "r", encoding="utf-8") as fh:
        config = yaml.safe_load(fh)
    if not isinstance(config, dict):
        raise ValueError(f"config {path} did not parse to a mapping")
    missing = [key for key in _REQUIRED_SECTIONS if key not in config]
    if missing:
        raise ValueError(f"config {path} missing required sections: {missing}")
    if not config["wells"]:
        raise ValueError("config must define at least one well")
    if not config["flowlines"]:
        raise ValueError("config must define at least one flowline")
    return config


def _route_flowline(
    spec: dict[str, Any],
    assets: dict[str, Asset],
    terrain: TerrainGrid,
    sample_spacing_m: float,
) -> FlowlineRoute:
    """Straight-line plan route, sampled and draped over the terrain."""
    from_id, to_id = str(spec["from_well"]), str(spec["to"])
    for asset_id in (from_id, to_id):
        if asset_id not in assets:
            raise ValueError(
                f"flowline {spec.get('id')!r} references unknown asset {asset_id!r}"
            )
    a, b = assets[from_id], assets[to_id]
    plan_length = math.hypot(b.x_m - a.x_m, b.y_m - a.y_m)
    n_segments = max(1, int(math.ceil(plan_length / sample_spacing_m)))
    t = np.linspace(0.0, 1.0, n_segments + 1)
    xs = a.x_m + t * (b.x_m - a.x_m)
    ys = a.y_m + t * (b.y_m - a.y_m)
    zs = np.array([terrain.elevation_at(x, y) for x, y in zip(xs, ys)])
    path = np.column_stack([xs, ys, zs])
    seg = np.diff(path, axis=0)
    terrain_length = float(np.sum(np.sqrt(np.sum(seg**2, axis=1))))
    return FlowlineRoute(
        flowline_id=str(spec["id"]),
        from_id=from_id,
        to_id=to_id,
        inner_diameter_m=float(spec["inner_diameter_m"]),
        roughness_m=float(spec["roughness_m"]),
        rate_m3_per_day=a.rate_m3_per_day,
        path_xyz_m=path,
        plan_length_m=plan_length,
        terrain_length_m=terrain_length,
        elevation_change_m=float(zs[-1] - zs[0]),
    )


def build_layout(config: dict[str, Any], base_dir: str | Path = ".") -> FieldLayout:
    """Build the typed field layout from a validated config mapping.

    ``base_dir`` resolves relative data paths inside the config (e.g. a
    ``csv_grid`` terrain file) — pass the config file's parent directory.
    """
    base = Path(base_dir)
    terrain = build_terrain(config["terrain"], base)

    host_spec = config["host"]
    host = Asset(
        asset_id=str(host_spec["id"]),
        name=str(host_spec.get("name", host_spec["id"])),
        kind="host",
        x_m=float(host_spec["x_m"]),
        y_m=float(host_spec["y_m"]),
        z_m=terrain.elevation_at(float(host_spec["x_m"]), float(host_spec["y_m"])),
    )
    wells = [
        Asset(
            asset_id=str(w["id"]),
            name=str(w.get("name", w["id"])),
            kind="well",
            x_m=float(w["x_m"]),
            y_m=float(w["y_m"]),
            z_m=terrain.elevation_at(float(w["x_m"]), float(w["y_m"])),
            rate_m3_per_day=float(w["rate_m3_per_day"]),
        )
        for w in config["wells"]
    ]
    assets = {host.asset_id: host, **{w.asset_id: w for w in wells}}
    if len(assets) != 1 + len(wells):
        raise ValueError("asset ids must be unique across host and wells")

    routing = config.get("routing", {})
    sample_spacing = float(routing.get("sample_spacing_m", 25.0))
    flowlines = [
        _route_flowline(fl, assets, terrain, sample_spacing)
        for fl in config["flowlines"]
    ]
    return FieldLayout(
        name=str(config["field"]["name"]),
        terrain=terrain,
        host=host,
        wells=wells,
        flowlines=flowlines,
    )


# ---------------------------------------------------------------------------
# Screening calc — Darcy-Weisbach + Swamee-Jain
# ---------------------------------------------------------------------------


def swamee_jain_friction_factor(reynolds: float, relative_roughness: float) -> float:
    """Darcy friction factor.

    Laminar (Re < 2300): f = 64/Re (exact, Hagen-Poiseuille).
    Turbulent: Swamee-Jain explicit approximation to the Colebrook-White
    equation —

        f = 0.25 / [log10(eps/(3.7 D) + 5.74 / Re^0.9)]^2

    Reference: Swamee, P.K. & Jain, A.K. (1976), "Explicit equations for
    pipe-flow problems", Journal of the Hydraulics Division, ASCE, 102(5),
    657-664. Stated validity 5e3 <= Re <= 3e8, 1e-6 <= eps/D <= 1e-2; within
    ~1% of Colebrook-White there. In the transition band 2300 < Re < 5000 it
    is used here as a screening-grade approximation.
    """
    if reynolds <= 0:
        raise ValueError(f"reynolds must be > 0, got {reynolds!r}")
    if reynolds < LAMINAR_REYNOLDS_LIMIT:
        return 64.0 / reynolds
    return 0.25 / math.log10(relative_roughness / 3.7 + 5.74 / reynolds**0.9) ** 2


def darcy_weisbach_pressure_drop(
    rate_m3_per_s: float,
    length_m: float,
    inner_diameter_m: float,
    roughness_m: float,
    density_kg_per_m3: float,
    viscosity_pa_s: float,
    elevation_change_m: float = 0.0,
) -> dict[str, float]:
    """Single-phase incompressible flowline pressure drop (screening grade).

    Frictional term: Darcy-Weisbach, dp = f (L/D) (rho v^2 / 2), with the
    friction factor from :func:`swamee_jain_friction_factor` (citation there).
    Elevation term: hydrostatic, dp = rho g dz (positive uphill).

    Reference for Darcy-Weisbach: e.g. White, F.M., "Fluid Mechanics",
    McGraw-Hill, ch. 6 (viscous flow in ducts).
    """
    if inner_diameter_m <= 0 or length_m < 0:
        raise ValueError("inner_diameter_m must be > 0 and length_m >= 0")
    if rate_m3_per_s <= 0:
        raise ValueError(f"rate_m3_per_s must be > 0, got {rate_m3_per_s!r}")
    area = math.pi * inner_diameter_m**2 / 4.0
    velocity = rate_m3_per_s / area
    reynolds = density_kg_per_m3 * velocity * inner_diameter_m / viscosity_pa_s
    friction = swamee_jain_friction_factor(reynolds, roughness_m / inner_diameter_m)
    dp_friction = (
        friction * (length_m / inner_diameter_m) * density_kg_per_m3 * velocity**2 / 2.0
    )
    dp_elevation = density_kg_per_m3 * GRAVITY_M_PER_S2 * elevation_change_m
    return {
        "velocity_m_per_s": velocity,
        "reynolds": reynolds,
        "friction_factor": friction,
        "flow_regime": "laminar" if reynolds < LAMINAR_REYNOLDS_LIMIT else "turbulent",
        "dp_friction_kpa": dp_friction / 1_000.0,
        "dp_elevation_kpa": dp_elevation / 1_000.0,
        "dp_total_kpa": (dp_friction + dp_elevation) / 1_000.0,
    }


def screen_flowline(
    flowline: FlowlineRoute,
    fluid: dict[str, Any],
    screening: Optional[dict[str, Any]] = None,
) -> dict[str, Any]:
    """Apply the pressure-drop screen to one routed flowline.

    Uses the TERRAIN length (3D) as the hydraulic length and the endpoint
    elevation difference for the hydrostatic term. Adds a pass/fail velocity
    flag against ``screening.max_velocity_m_per_s`` when configured.
    """
    result = darcy_weisbach_pressure_drop(
        rate_m3_per_s=flowline.rate_m3_per_day / SECONDS_PER_DAY,
        length_m=flowline.terrain_length_m,
        inner_diameter_m=flowline.inner_diameter_m,
        roughness_m=flowline.roughness_m,
        density_kg_per_m3=float(fluid["density_kg_per_m3"]),
        viscosity_pa_s=float(fluid["viscosity_pa_s"]),
        elevation_change_m=flowline.elevation_change_m,
    )
    max_velocity = (screening or {}).get("max_velocity_m_per_s")
    if max_velocity is not None:
        result["velocity_ok"] = result["velocity_m_per_s"] <= float(max_velocity)
    return result


# ---------------------------------------------------------------------------
# 2D layout plot
# ---------------------------------------------------------------------------


def plot_layout(
    layout: FieldLayout,
    output_path: str | Path,
    output_format: str = "png",
    dpi: int = 150,
) -> str:
    """Deterministic 2D layout figure: terrain contours + assets + flowlines."""
    fig, ax = plt.subplots(figsize=(9, 8))
    terrain = layout.terrain
    filled = ax.contourf(
        terrain.x_m, terrain.y_m, terrain.z_m, levels=12, cmap="terrain", alpha=0.75
    )
    contours = ax.contour(
        terrain.x_m,
        terrain.y_m,
        terrain.z_m,
        levels=12,
        colors="dimgray",
        linewidths=0.5,
    )
    ax.clabel(contours, inline=True, fontsize=7, fmt="%.0f")
    fig.colorbar(filled, ax=ax, label="Elevation [m]")

    for fl in layout.flowlines:
        ax.plot(
            fl.path_xyz_m[:, 0],
            fl.path_xyz_m[:, 1],
            color="firebrick",
            linewidth=2.0,
            zorder=3,
        )
        mid = fl.path_xyz_m[len(fl.path_xyz_m) // 2]
        ax.annotate(
            f"{fl.flowline_id}: {fl.terrain_length_m:,.0f} m",
            (mid[0], mid[1]),
            textcoords="offset points",
            xytext=(6, 6),
            fontsize=8,
        )
    for well in layout.wells:
        ax.plot(well.x_m, well.y_m, "o", color="black", markersize=8, zorder=4)
        ax.annotate(
            well.asset_id,
            (well.x_m, well.y_m),
            textcoords="offset points",
            xytext=(8, -12),
            fontsize=9,
        )
    host = layout.host
    ax.plot(host.x_m, host.y_m, "s", color="navy", markersize=12, zorder=4)
    ax.annotate(
        host.asset_id,
        (host.x_m, host.y_m),
        textcoords="offset points",
        xytext=(10, 8),
        fontsize=10,
        weight="bold",
    )

    ax.set_xlabel("Easting [m]")
    ax.set_ylabel("Northing [m]")
    ax.set_title(f"{layout.name} — onshore layout (screening)")
    ax.set_aspect("equal")
    saved = render_figure(fig, str(output_path), output_format, dpi=dpi)
    plt.close(fig)
    return saved


# ---------------------------------------------------------------------------
# Scene export (Blender fly-through input)
# ---------------------------------------------------------------------------


def export_scene_json(layout: FieldLayout, output_path: str | Path) -> str:
    """Write the layout as a JSON scene for the bpy fly-through script.

    JSON (not YAML/pickle) so Blender's bundled Python can read it with the
    standard library only. Consumed by
    ``scripts/field_development/onshore_flythrough_bpy.py``.
    """
    scene = {
        "field_name": layout.name,
        "terrain": {
            "x_m": layout.terrain.x_m.tolist(),
            "y_m": layout.terrain.y_m.tolist(),
            "z_m": layout.terrain.z_m.tolist(),
            "source": layout.terrain.source,
        },
        "assets": [
            {
                "id": a.asset_id,
                "name": a.name,
                "kind": a.kind,
                "x_m": a.x_m,
                "y_m": a.y_m,
                "z_m": a.z_m,
            }
            for a in [layout.host, *layout.wells]
        ],
        "flowlines": [
            {
                "id": fl.flowline_id,
                "from": fl.from_id,
                "to": fl.to_id,
                "path_xyz_m": fl.path_xyz_m.tolist(),
            }
            for fl in layout.flowlines
        ],
    }
    dest = Path(output_path)
    dest.parent.mkdir(parents=True, exist_ok=True)
    with open(dest, "w", encoding="utf-8") as fh:
        json.dump(scene, fh)
    return str(dest)


# ---------------------------------------------------------------------------
# API entry point
# ---------------------------------------------------------------------------


def _resolve_output(configured: str, output_dir: Optional[str | Path]) -> Path:
    """Configured output path, optionally redirected into ``output_dir``."""
    path = Path(configured)
    if output_dir is not None:
        return Path(output_dir) / path.name
    return path


def screen_layout(
    config_path: str | Path, output_dir: Optional[str | Path] = None
) -> dict[str, Any]:
    """Run the onshore layout tracer end-to-end from a YAML config.

    Loads the config, builds terrain + layout, screens every flowline
    (Darcy-Weisbach + Swamee-Jain, see :func:`darcy_weisbach_pressure_drop`),
    renders the 2D layout plot, and exports the Blender scene JSON.

    Parameters
    ----------
    config_path : str | Path
        Field YAML (schema: ``data/onshore_demo_field.yml``).
    output_dir : str | Path, optional
        Redirect all output artifacts (plot, scene JSON) into this directory,
        keeping the configured file names. Default: use the config paths as-is
        (relative paths resolve against the current working directory).

    Returns
    -------
    dict
        Layout summary + per-flowline screening results + artifact paths.
    """
    config_file = Path(config_path)
    config = load_field_config(config_file)
    layout = build_layout(config, base_dir=config_file.parent)

    fluid = config["fluid"]
    screening_cfg = config.get("screening", {})
    flowline_rows = []
    for fl in layout.flowlines:
        flowline_rows.append(
            {
                "id": fl.flowline_id,
                "from": fl.from_id,
                "to": fl.to_id,
                "rate_m3_per_day": fl.rate_m3_per_day,
                "inner_diameter_m": fl.inner_diameter_m,
                "plan_length_m": round(fl.plan_length_m, 2),
                "terrain_length_m": round(fl.terrain_length_m, 2),
                "elevation_change_m": round(fl.elevation_change_m, 2),
                "screening": screen_flowline(fl, fluid, screening_cfg),
            }
        )

    plot_cfg = config["plot"]
    plot_path = plot_layout(
        layout,
        _resolve_output(plot_cfg["output_path"], output_dir),
        output_format=str(plot_cfg.get("format", "png")),
        dpi=int(plot_cfg.get("dpi", 150)),
    )
    scene_cfg = config.get("scene_export", {})
    scene_path: Optional[str] = None
    if scene_cfg.get("output_path"):
        scene_path = export_scene_json(
            layout, _resolve_output(scene_cfg["output_path"], output_dir)
        )

    terrain = layout.terrain
    return {
        "field": layout.name,
        "terrain": {
            "source": terrain.source,
            "x_range_m": [float(terrain.x_m[0]), float(terrain.x_m[-1])],
            "y_range_m": [float(terrain.y_m[0]), float(terrain.y_m[-1])],
            "z_range_m": [float(terrain.z_m.min()), float(terrain.z_m.max())],
        },
        "host": {"id": layout.host.asset_id, "z_m": round(layout.host.z_m, 2)},
        "wells": [
            {
                "id": w.asset_id,
                "x_m": w.x_m,
                "y_m": w.y_m,
                "z_m": round(w.z_m, 2),
                "rate_m3_per_day": w.rate_m3_per_day,
            }
            for w in layout.wells
        ],
        "flowlines": flowline_rows,
        "plot_path": plot_path,
        "scene_path": scene_path,
    }
