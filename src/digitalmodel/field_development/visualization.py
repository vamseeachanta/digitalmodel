# ABOUTME: Visualization of the generalized field layout — 2D plot + 3D scene JSON.
# ABOUTME: Issue #1510 (workstream C of epic #1507). Feeds the Blender fly-through.
"""
digitalmodel.field_development.visualization
=============================================

Workstream C (#1510) of the field-development epic (#1507): visualization of
the GENERALIZED layout model
(:mod:`digitalmodel.field_development.layout_model`, #1509) —

1. **2D layout plot** — deterministic matplotlib figure over the surface
   (hypsometric tints onshore; a single-hue blue ramp for bathymetry, where
   elevation is negative), with a fixed marker/color per asset kind, a fixed
   color/linestyle per connection kind, waypoint markers, per-connection
   3D-length annotations, and a legend of the kinds present.
2. **3D scene export** — a typed JSON scene (schema
   ``digitalmodel.field_layout_scene/2``) carrying the surface grid, EVERY
   asset kind, and every routed connection (with waypoints and the sampled
   3D path), so the Blender script can style each kind differently. JSON so
   Blender's bundled Python needs no third-party packages. Consumed by
   ``scripts/field_development/field_flythrough_bpy.py``.
3. **Pipeline convenience** — :func:`render_layout_visualization` runs
   config -> model -> plot PNG -> scene JSON in one call.

The #1508 onshore tracer keeps its own plot/scene export
(:mod:`digitalmodel.field_development.onshore_layout`, v1 scene schema);
this module supersedes them for the generalized model — the tracer's YAML
loads here unchanged via the loader's back-compat normalization.

Determinism: styles are module-level constant maps covering every kind in
:data:`~digitalmodel.field_development.layout_model.ASSET_KINDS` /
:data:`~digitalmodel.field_development.layout_model.CONNECTION_KINDS`;
exports are plain data derived from the model — CI asserts file creation,
schema content, and layout numbers, never pixels.

Usage
-----
>>> from digitalmodel.field_development.layout_model import (
...     build_layout_model_from_file,
... )
>>> from digitalmodel.field_development.visualization import (
...     export_layout_scene_json, plot_field_layout,
... )
>>> model = build_layout_model_from_file(
...     "src/digitalmodel/field_development/data/offshore_demo_field.yml"
... )  # doctest: +SKIP
>>> plot_field_layout(model, "outputs/offshore_layout.png")  # doctest: +SKIP
>>> export_layout_scene_json(model, "outputs/offshore_scene.json")  # doctest: +SKIP
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any, Optional

import matplotlib

matplotlib.use("Agg")  # non-interactive backend for file output
import matplotlib.pyplot as plt  # noqa: E402
from matplotlib.lines import Line2D  # noqa: E402

from .layout_model import (  # noqa: E402
    FieldLayoutModel,
    build_layout_model_from_file,
    layout_summary,
)
from .schematics.renderer import render_figure  # noqa: E402

#: Version tag written into every exported scene JSON (bump on schema change).
SCENE_SCHEMA = "digitalmodel.field_layout_scene/2"

# --- fixed per-kind styles (identity = fixed assignment, never cycled) --------
#: Marker style per asset kind: (matplotlib marker, color, size_pt).
ASSET_STYLES: dict[str, tuple[str, str, float]] = {
    "host": ("s", "#1f3a5f", 12.0),  # square, dark navy
    "vessel": ("D", "#b3591c", 11.0),  # diamond, dark orange
    "well": ("o", "#222222", 8.0),  # circle, near-black
    "tree": ("^", "#2e6f40", 8.0),  # triangle, green
    "manifold": ("P", "#6b3fa0", 11.0),  # plus (filled), purple
    "wind_turbine": ("*", "#0b7285", 14.0),  # star, teal (#1513)
    "offshore_substation": ("h", "#8c6d1f", 12.0),  # hexagon, dark gold (#1513)
}
#: Line style per connection kind: (color, linestyle, linewidth_pt).
CONNECTION_STYLES: dict[str, tuple[str, str, float]] = {
    "jumper": ("#e07b39", "-", 1.6),  # orange, solid, thin
    "flowline": ("#b2182b", "-", 2.2),  # firebrick, solid
    "pipeline": ("#67001f", "--", 2.6),  # dark maroon, dashed, thick
    "umbilical": ("#2166ac", "-.", 1.6),  # steel blue, dash-dot
    "array_cable": ("#41ab5d", ":", 1.8),  # mid green, dotted (#1513)
    "export_cable": ("#00441b", "--", 2.4),  # darkest green, dashed (#1513)
}
_WAYPOINT_STYLE = {"marker": "x", "color": "#444444", "s": 36.0}

_SUBSEA_CMAP = "Blues_r"  # single-hue ramp: deeper water = darker blue
_ONSHORE_CMAP = "terrain"  # domain-standard hypsometric tints (matches #1508)


def _is_subsea(model: FieldLayoutModel) -> bool:
    """True when the whole surface is under water (bathymetry only)."""
    return bool(model.surface.z_m.max() <= 0.0)


# ---------------------------------------------------------------------------
# 2D layout plot
# ---------------------------------------------------------------------------


def plot_field_layout(
    model: FieldLayoutModel,
    output_path: str | Path,
    output_format: str = "png",
    dpi: int = 150,
) -> str:
    """Deterministic 2D layout figure for the generalized model.

    Surface as filled contours (blue depth ramp for bathymetry, hypsometric
    tints onshore) with labelled contour lines; every connection drawn in its
    kind's fixed color/linestyle with waypoint markers and a 3D route-length
    annotation; every asset in its kind's fixed marker; legend of the kinds
    present (fixed order). Returns the saved path.
    """
    fig, ax = plt.subplots(figsize=(9, 8))
    surface = model.surface
    subsea = _is_subsea(model)
    cmap = _SUBSEA_CMAP if subsea else _ONSHORE_CMAP
    filled = ax.contourf(
        surface.x_m, surface.y_m, surface.z_m, levels=12, cmap=cmap, alpha=0.75
    )
    lines = ax.contour(
        surface.x_m,
        surface.y_m,
        surface.z_m,
        levels=12,
        colors="dimgray",
        linewidths=0.5,
    )
    ax.clabel(lines, inline=True, fontsize=7, fmt="%.0f")
    colorbar_label = (
        "Elevation [m] (negative = water depth)" if subsea else "Elevation [m]"
    )
    fig.colorbar(filled, ax=ax, label=colorbar_label)

    for conn in model.connections:
        color, linestyle, width = CONNECTION_STYLES[conn.kind]
        ax.plot(
            conn.path_xyz_m[:, 0],
            conn.path_xyz_m[:, 1],
            color=color,
            linestyle=linestyle,
            linewidth=width,
            zorder=3,
        )
        if conn.waypoints_plan_m:
            ax.scatter(
                [w[0] for w in conn.waypoints_plan_m],
                [w[1] for w in conn.waypoints_plan_m],
                zorder=4,
                **_WAYPOINT_STYLE,
            )
        mid = conn.path_xyz_m[len(conn.path_xyz_m) // 2]
        ax.annotate(
            f"{conn.connection_id}: {conn.route_length_m:,.0f} m",
            (mid[0], mid[1]),
            textcoords="offset points",
            xytext=(6, 6),
            fontsize=8,
            zorder=5,
        )

    for asset in model.assets:
        marker, color, size = ASSET_STYLES[asset.kind]
        ax.plot(
            asset.x_m,
            asset.y_m,
            marker,
            color=color,
            markersize=size,
            linestyle="none",
            zorder=6,
        )
        if asset.kind != "tree":  # trees are co-located with wells; skip label
            ax.annotate(
                asset.asset_id,
                (asset.x_m, asset.y_m),
                textcoords="offset points",
                xytext=(8, -12),
                fontsize=8,
                zorder=7,
            )

    ax.legend(
        handles=_legend_handles(model),
        loc="upper left",
        fontsize=8,
        framealpha=0.9,
    )
    ax.set_xlabel("Easting [m]")
    ax.set_ylabel("Northing [m]")
    setting = "subsea" if subsea else "onshore"
    ax.set_title(f"{model.name} — {setting} layout (screening)")
    ax.set_aspect("equal")
    saved = render_figure(fig, str(output_path), output_format, dpi=dpi)
    plt.close(fig)
    return saved


def _legend_handles(model: FieldLayoutModel) -> list[Line2D]:
    """Legend proxies for the asset/connection kinds present (fixed order)."""
    handles: list[Line2D] = []
    present_assets = {a.kind for a in model.assets}
    for kind in ASSET_STYLES:  # dict order = fixed legend order
        if kind not in present_assets:
            continue
        marker, color, size = ASSET_STYLES[kind]
        handles.append(
            Line2D(
                [],
                [],
                marker=marker,
                color=color,
                markersize=size * 0.75,
                linestyle="none",
                label=kind,
            )
        )
    present_connections = {c.kind for c in model.connections}
    for kind in CONNECTION_STYLES:
        if kind not in present_connections:
            continue
        color, linestyle, width = CONNECTION_STYLES[kind]
        handles.append(
            Line2D(
                [], [], color=color, linestyle=linestyle, linewidth=width, label=kind
            )
        )
    return handles


# ---------------------------------------------------------------------------
# 3D scene export (Blender fly-through input)
# ---------------------------------------------------------------------------


def layout_scene_dict(model: FieldLayoutModel) -> dict[str, Any]:
    """The typed scene mapping (schema :data:`SCENE_SCHEMA`) for a model.

    Every asset carries its ``kind``/``subtype``/``on_surface`` and every
    connection its ``kind``, waypoints, and sampled 3D path, so the Blender
    script can style each kind differently. ``is_subsea`` tells the renderer
    to add a water surface at z = 0. Plain JSON-able data only.
    """
    return {
        "schema": SCENE_SCHEMA,
        "field_name": model.name,
        "is_subsea": _is_subsea(model),
        "surface": {
            "x_m": model.surface.x_m.tolist(),
            "y_m": model.surface.y_m.tolist(),
            "z_m": model.surface.z_m.tolist(),
            "source": model.surface.source,
        },
        "assets": [
            {
                "id": a.asset_id,
                "name": a.name,
                "kind": a.kind,
                "subtype": a.subtype,
                "x_m": a.x_m,
                "y_m": a.y_m,
                "z_m": a.z_m,
                "on_surface": a.on_surface,
            }
            for a in model.assets
        ],
        "connections": [
            {
                "id": c.connection_id,
                "kind": c.kind,
                "from": c.from_id,
                "to": c.to_id,
                "waypoints_plan_m": [[x, y] for x, y in c.waypoints_plan_m],
                "route_length_m": c.route_length_m,
                "path_xyz_m": c.path_xyz_m.tolist(),
            }
            for c in model.connections
        ],
    }


def export_layout_scene_json(model: FieldLayoutModel, output_path: str | Path) -> str:
    """Write :func:`layout_scene_dict` as JSON for the bpy fly-through script.

    JSON (not YAML/pickle) so Blender's bundled Python can read it with the
    standard library only. Consumed by
    ``scripts/field_development/field_flythrough_bpy.py``. Returns the path.
    """
    dest = Path(output_path)
    dest.parent.mkdir(parents=True, exist_ok=True)
    with open(dest, "w", encoding="utf-8") as fh:
        json.dump(layout_scene_dict(model), fh)
    return str(dest)


# ---------------------------------------------------------------------------
# Pipeline convenience: config -> model -> plot + scene
# ---------------------------------------------------------------------------


def render_layout_visualization(
    config_path: str | Path,
    output_dir: str | Path,
    output_format: str = "png",
    dpi: int = 150,
    stem: Optional[str] = None,
) -> dict[str, Any]:
    """Config -> model -> 2D plot + 3D scene JSON, in one call.

    Artifacts land in ``output_dir`` as ``<stem>_layout.<format>`` and
    ``<stem>_scene.json`` (default stem: the config file's stem). Returns the
    :func:`~digitalmodel.field_development.layout_model.layout_summary` plus
    the artifact paths — the numbers CI asserts against.
    """
    config_file = Path(config_path)
    model = build_layout_model_from_file(config_file)
    base = stem if stem is not None else config_file.stem
    out = Path(output_dir)
    plot_path = plot_field_layout(
        model,
        out / f"{base}_layout.{output_format}",
        output_format=output_format,
        dpi=dpi,
    )
    scene_path = export_layout_scene_json(model, out / f"{base}_scene.json")
    result = layout_summary(model)
    result["plot_path"] = plot_path
    result["scene_path"] = scene_path
    return result
