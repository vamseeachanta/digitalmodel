# ABOUTME: Full field-layout model — YAML asset schema + layout engine over terrain/bathymetry.
# ABOUTME: Issue #1509 (workstream B of epic #1507). Generalizes the #1508 onshore tracer.
"""
digitalmodel.field_development.layout_model
============================================

Workstream B (#1509) of the field-development epic (#1507): the FULL
field-layout model, generalizing the onshore tracer
(:mod:`digitalmodel.field_development.onshore_layout`, #1508) —

1. **Full asset schema (YAML-first)** — beyond the tracer's host/wells the
   schema covers the subsea/onshore taxonomy: ``host``, ``vessel``, ``well``,
   ``tree``, ``manifold`` assets connected by ``jumper``, ``flowline``,
   ``pipeline``, ``umbilical`` links. ALL configuration is externalized to
   YAML (see ``data/offshore_demo_field.yml``); nothing is hardcoded here.
2. **Offshore-ready surface** — the surface is the tracer's
   :class:`~digitalmodel.field_development.onshore_layout.TerrainGrid`
   (bilinear DEM queries). Elevation is signed relative to the field datum
   (sea level offshore), so bathymetry is simply NEGATIVE elevation — the
   later offshore data slice swaps the surface DATA, not this model.
3. **Layout engine** — assets are draped onto the surface (optional explicit
   ``z_m`` override for floating hosts/vessels at the waterline); connections
   route through optional intermediate plan waypoints as piecewise straight
   legs, each leg sampled and draped over the surface, with TRUE 3D lengths.
4. **Back-compat** — the tracer schema (``host``/``wells``/``flowlines`` with
   ``from_well``) still loads: :func:`load_layout_config` normalizes it into
   the generalized ``assets``/``connections`` form.

Scope honesty
-------------
Routing is piecewise straight in plan (no pathfinding, no obstacle avoidance,
no least-cost optimization — follow-on workstreams of #1507). A floating
host's riser is approximated by the straight final segment from the last
draped sample up to the host's explicit ``z_m`` (screening-grade geometry,
not a catenary). Hydraulic screening lives in workstream D and in the tracer's
:func:`~digitalmodel.field_development.onshore_layout.screen_flowline`.

Usage
-----
>>> from digitalmodel.field_development.layout_model import (
...     build_layout_model, load_layout_config,
... )
>>> config = load_layout_config(
...     "src/digitalmodel/field_development/data/offshore_demo_field.yml"
... )  # doctest: +SKIP
>>> model = build_layout_model(config, base_dir="...")  # doctest: +SKIP
>>> model.connections[0].route_length_m  # doctest: +SKIP
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Optional

import numpy as np
import yaml

from .onshore_layout import TerrainGrid, build_terrain

# --- schema vocabulary -------------------------------------------------------
#: Recognized asset kinds (surface-placed field infrastructure).
ASSET_KINDS = frozenset({"host", "vessel", "well", "tree", "manifold"})
#: Recognized connection kinds (routed links between assets).
CONNECTION_KINDS = frozenset({"jumper", "flowline", "pipeline", "umbilical"})

_REQUIRED_SECTIONS = ("field", "assets", "connections")
_TRACER_SECTIONS = ("host", "wells", "flowlines")

_ASSET_KNOWN_KEYS = frozenset(
    {"id", "kind", "subtype", "name", "x_m", "y_m", "z_m", "rate_m3_per_day"}
)
_CONNECTION_KNOWN_KEYS = frozenset(
    {"id", "kind", "from", "to", "waypoints", "inner_diameter_m", "roughness_m"}
)

_DEFAULT_SAMPLE_SPACING_M = 25.0


# ---------------------------------------------------------------------------
# Typed model
# ---------------------------------------------------------------------------


@dataclass
class LayoutAsset:
    """A field asset placed on (or above) the terrain/bathymetry surface.

    ``on_surface`` is True when ``z_m`` was draped from the surface; False
    when the config gave an explicit ``z_m`` (e.g. a floating host at the
    waterline, ``z_m: 0.0``, above negative-elevation bathymetry).
    """

    asset_id: str
    kind: str  # one of ASSET_KINDS
    x_m: float
    y_m: float
    z_m: float
    name: str = ""
    subtype: str = ""
    on_surface: bool = True
    rate_m3_per_day: Optional[float] = None
    properties: dict[str, Any] = field(default_factory=dict)


@dataclass
class RoutedConnection:
    """A routed connection: piecewise straight plan legs draped over the surface.

    ``route_length_m`` is the TRUE 3D polyline length (sum of straight-segment
    lengths of the sampled, draped path) — never shorter than
    ``plan_length_m``. ``elevation_change_m`` is ``z_to - z_from``.
    """

    connection_id: str
    kind: str  # one of CONNECTION_KINDS
    from_id: str
    to_id: str
    path_xyz_m: np.ndarray = field(repr=False)  # (n, 3) sampled polyline
    waypoints_plan_m: list[tuple[float, float]] = field(default_factory=list)
    plan_length_m: float = 0.0
    route_length_m: float = 0.0
    elevation_change_m: float = 0.0
    inner_diameter_m: Optional[float] = None
    roughness_m: Optional[float] = None
    properties: dict[str, Any] = field(default_factory=dict)


@dataclass
class FieldLayoutModel:
    """Typed layout of one field: surface + placed assets + routed connections."""

    name: str
    surface: TerrainGrid
    surface_spec: dict[str, Any]
    assets: list[LayoutAsset]
    connections: list[RoutedConnection]
    sample_spacing_m: float = _DEFAULT_SAMPLE_SPACING_M
    metadata: dict[str, Any] = field(default_factory=dict)

    def asset(self, asset_id: str) -> LayoutAsset:
        """Return the asset with ``asset_id`` (KeyError if absent)."""
        for a in self.assets:
            if a.asset_id == asset_id:
                return a
        raise KeyError(asset_id)

    def assets_of_kind(self, kind: str) -> list[LayoutAsset]:
        """All assets of one kind (e.g. ``\"well\"``)."""
        return [a for a in self.assets if a.kind == kind]

    def connections_of_kind(self, kind: str) -> list[RoutedConnection]:
        """All connections of one kind (e.g. ``\"flowline\"``)."""
        return [c for c in self.connections if c.kind == kind]


# ---------------------------------------------------------------------------
# Config loading + normalization (tracer schema back-compat)
# ---------------------------------------------------------------------------


def _normalize_tracer_config(config: dict[str, Any]) -> dict[str, Any]:
    """Map the #1508 tracer schema onto the generalized assets/connections form.

    Tracer: ``host`` (single mapping) + ``wells`` + ``flowlines`` (with
    ``from_well``/``to``). Generalized: ``assets`` + ``connections``.
    Unrelated sections (fluid, screening, plot, scene_export, ...) pass
    through untouched.
    """
    normalized = {k: v for k, v in config.items() if k not in _TRACER_SECTIONS}
    host = dict(config["host"])
    host["kind"] = "host"
    assets: list[dict[str, Any]] = [host]
    for well in config["wells"]:
        w = dict(well)
        w["kind"] = "well"
        assets.append(w)
    connections: list[dict[str, Any]] = []
    for fl in config["flowlines"]:
        c = dict(fl)
        c["kind"] = "flowline"
        c["from"] = c.pop("from_well")
        connections.append(c)
    normalized["assets"] = assets
    normalized["connections"] = connections
    return normalized


def load_layout_config(config_path: str | Path) -> dict[str, Any]:
    """Load + validate a field-layout YAML config, normalized to the full schema.

    Accepts either the generalized schema (``assets`` + ``connections`` +
    ``surface``) or the #1508 tracer schema (``host`` + ``wells`` +
    ``flowlines`` + ``terrain``), which is normalized into the generalized
    form. The returned mapping always has ``field``, ``surface``, ``assets``,
    ``connections``.
    """
    path = Path(config_path)
    with open(path, "r", encoding="utf-8") as fh:
        config = yaml.safe_load(fh)
    if not isinstance(config, dict):
        raise ValueError(f"config {path} did not parse to a mapping")
    return normalize_layout_config(config, source=str(path))


def normalize_layout_config(
    config: dict[str, Any], source: str = "<config>"
) -> dict[str, Any]:
    """Validate a parsed config mapping and normalize it to the full schema."""
    if "assets" not in config and all(k in config for k in _TRACER_SECTIONS):
        config = _normalize_tracer_config(config)
    # `surface` is the canonical section name; `terrain` (tracer) is an alias.
    if "surface" not in config and "terrain" in config:
        config = dict(config)
        config["surface"] = config.pop("terrain")
    missing = [k for k in (*_REQUIRED_SECTIONS, "surface") if k not in config]
    if missing:
        raise ValueError(f"config {source} missing required sections: {missing}")
    if not config["assets"]:
        raise ValueError(f"config {source} must define at least one asset")
    if config["connections"] is None:
        config = dict(config)
        config["connections"] = []
    _validate_schema(config, source)
    return config


def _validate_schema(config: dict[str, Any], source: str) -> None:
    """Schema-level checks: kinds, unique ids, connectivity (no dangling refs)."""
    asset_ids: set[str] = set()
    for spec in config["assets"]:
        asset_id = str(spec.get("id"))
        kind = spec.get("kind")
        if kind not in ASSET_KINDS:
            raise ValueError(
                f"{source}: asset {asset_id!r} has unknown kind {kind!r}; "
                f"expected one of {sorted(ASSET_KINDS)}"
            )
        if asset_id in asset_ids:
            raise ValueError(f"{source}: duplicate asset id {asset_id!r}")
        asset_ids.add(asset_id)
        for key in ("x_m", "y_m"):
            if key not in spec:
                raise ValueError(f"{source}: asset {asset_id!r} missing {key!r}")
    connection_ids: set[str] = set()
    for spec in config["connections"]:
        conn_id = str(spec.get("id"))
        kind = spec.get("kind")
        if kind not in CONNECTION_KINDS:
            raise ValueError(
                f"{source}: connection {conn_id!r} has unknown kind {kind!r}; "
                f"expected one of {sorted(CONNECTION_KINDS)}"
            )
        if conn_id in connection_ids:
            raise ValueError(f"{source}: duplicate connection id {conn_id!r}")
        connection_ids.add(conn_id)
        for end in ("from", "to"):
            ref = str(spec.get(end))
            if ref not in asset_ids:
                raise ValueError(
                    f"{source}: connection {conn_id!r} references unknown "
                    f"asset {ref!r} (dangling {end!r} reference)"
                )


# ---------------------------------------------------------------------------
# Layout engine: placement + waypoint routing
# ---------------------------------------------------------------------------


def _place_asset(spec: dict[str, Any], surface: TerrainGrid) -> LayoutAsset:
    """Place one asset: drape onto the surface unless an explicit z_m is given."""
    x, y = float(spec["x_m"]), float(spec["y_m"])
    if "z_m" in spec:
        z, on_surface = float(spec["z_m"]), False
    else:
        z, on_surface = surface.elevation_at(x, y), True
    rate = spec.get("rate_m3_per_day")
    extras = {k: v for k, v in spec.items() if k not in _ASSET_KNOWN_KEYS}
    return LayoutAsset(
        asset_id=str(spec["id"]),
        kind=str(spec["kind"]),
        x_m=x,
        y_m=y,
        z_m=z,
        name=str(spec.get("name", spec["id"])),
        subtype=str(spec.get("subtype", "")),
        on_surface=on_surface,
        rate_m3_per_day=None if rate is None else float(rate),
        properties=extras,
    )


def _drape_leg(
    surface: TerrainGrid,
    p0: tuple[float, float],
    p1: tuple[float, float],
    sample_spacing_m: float,
) -> np.ndarray:
    """One straight plan leg p0 -> p1, sampled and draped over the surface.

    Returns an (n+1, 3) array including both endpoints; z is the bilinear
    surface elevation at each sample.
    """
    plan_length = math.hypot(p1[0] - p0[0], p1[1] - p0[1])
    n_segments = max(1, int(math.ceil(plan_length / sample_spacing_m)))
    t = np.linspace(0.0, 1.0, n_segments + 1)
    xs = p0[0] + t * (p1[0] - p0[0])
    ys = p0[1] + t * (p1[1] - p0[1])
    zs = np.array([surface.elevation_at(x, y) for x, y in zip(xs, ys)])
    return np.column_stack([xs, ys, zs])


def route_connection(
    spec: dict[str, Any],
    assets: dict[str, LayoutAsset],
    surface: TerrainGrid,
    sample_spacing_m: float = _DEFAULT_SAMPLE_SPACING_M,
) -> RoutedConnection:
    """Route one connection: from-asset -> waypoints... -> to-asset.

    Piecewise straight in plan; every leg is sampled and draped over the
    surface. If an endpoint asset has an explicit (off-surface) ``z_m`` —
    e.g. a floating host at the waterline — the endpoint sample takes that
    elevation, so the final segment rises straight from the last draped
    sample to the asset (screening-grade riser stand-in). Lengths are true
    3D polyline lengths.
    """
    from_id, to_id = str(spec["from"]), str(spec["to"])
    for ref in (from_id, to_id):
        if ref not in assets:
            raise ValueError(
                f"connection {spec.get('id')!r} references unknown asset {ref!r}"
            )
    a, b = assets[from_id], assets[to_id]
    waypoints = [
        (float(w["x_m"]), float(w["y_m"])) for w in (spec.get("waypoints") or [])
    ]
    plan_points = [(a.x_m, a.y_m), *waypoints, (b.x_m, b.y_m)]

    legs = []
    for p0, p1 in zip(plan_points[:-1], plan_points[1:]):
        leg = _drape_leg(surface, p0, p1, sample_spacing_m)
        legs.append(leg if not legs else leg[1:])  # drop duplicate joint point
    path = np.vstack(legs)

    # Endpoint elevation overrides (off-surface assets, e.g. floating hosts).
    if not a.on_surface:
        path[0, 2] = a.z_m
    if not b.on_surface:
        path[-1, 2] = b.z_m

    plan_length = sum(
        math.hypot(p1[0] - p0[0], p1[1] - p0[1])
        for p0, p1 in zip(plan_points[:-1], plan_points[1:])
    )
    seg = np.diff(path, axis=0)
    route_length = float(np.sum(np.sqrt(np.sum(seg**2, axis=1))))

    diameter = spec.get("inner_diameter_m")
    roughness = spec.get("roughness_m")
    extras = {k: v for k, v in spec.items() if k not in _CONNECTION_KNOWN_KEYS}
    return RoutedConnection(
        connection_id=str(spec["id"]),
        kind=str(spec["kind"]),
        from_id=from_id,
        to_id=to_id,
        path_xyz_m=path,
        waypoints_plan_m=waypoints,
        plan_length_m=plan_length,
        route_length_m=route_length,
        elevation_change_m=float(path[-1, 2] - path[0, 2]),
        inner_diameter_m=None if diameter is None else float(diameter),
        roughness_m=None if roughness is None else float(roughness),
        properties=extras,
    )


def build_layout_model(
    config: dict[str, Any], base_dir: str | Path = "."
) -> FieldLayoutModel:
    """Build the typed field-layout model from a normalized config mapping.

    ``base_dir`` resolves relative data paths inside the config (e.g. a
    ``csv_grid`` surface file) — pass the config file's parent directory.
    """
    config = normalize_layout_config(config)
    surface_spec = config["surface"]
    surface = build_terrain(surface_spec, Path(base_dir))

    assets = [_place_asset(spec, surface) for spec in config["assets"]]
    asset_map = {a.asset_id: a for a in assets}

    routing = config.get("routing") or {}
    sample_spacing = float(routing.get("sample_spacing_m", _DEFAULT_SAMPLE_SPACING_M))
    connections = [
        route_connection(spec, asset_map, surface, sample_spacing)
        for spec in config["connections"]
    ]

    field_section = dict(config["field"])
    name = str(field_section.pop("name"))
    return FieldLayoutModel(
        name=name,
        surface=surface,
        surface_spec=dict(surface_spec),
        assets=assets,
        connections=connections,
        sample_spacing_m=sample_spacing,
        metadata=field_section,
    )


# ---------------------------------------------------------------------------
# Round-trip export + summary
# ---------------------------------------------------------------------------


def layout_model_to_config(model: FieldLayoutModel) -> dict[str, Any]:
    """Emit the model back as a normalized, YAML-serializable config mapping.

    Round-trip property: ``build_layout_model(layout_model_to_config(m))``
    rebuilds an equivalent model (same ids, kinds, coordinates, routes).
    """
    assets = []
    for a in model.assets:
        spec: dict[str, Any] = {
            "id": a.asset_id,
            "kind": a.kind,
            "x_m": a.x_m,
            "y_m": a.y_m,
        }
        if a.name and a.name != a.asset_id:
            spec["name"] = a.name
        if a.subtype:
            spec["subtype"] = a.subtype
        if not a.on_surface:
            spec["z_m"] = a.z_m
        if a.rate_m3_per_day is not None:
            spec["rate_m3_per_day"] = a.rate_m3_per_day
        spec.update(a.properties)
        assets.append(spec)
    connections = []
    for c in model.connections:
        cspec: dict[str, Any] = {
            "id": c.connection_id,
            "kind": c.kind,
            "from": c.from_id,
            "to": c.to_id,
        }
        if c.waypoints_plan_m:
            cspec["waypoints"] = [{"x_m": x, "y_m": y} for x, y in c.waypoints_plan_m]
        if c.inner_diameter_m is not None:
            cspec["inner_diameter_m"] = c.inner_diameter_m
        if c.roughness_m is not None:
            cspec["roughness_m"] = c.roughness_m
        cspec.update(c.properties)
        connections.append(cspec)
    return {
        "field": {"name": model.name, **model.metadata},
        "surface": dict(model.surface_spec),
        "assets": assets,
        "connections": connections,
        "routing": {"sample_spacing_m": model.sample_spacing_m},
    }


def layout_summary(model: FieldLayoutModel) -> dict[str, Any]:
    """JSON-able summary: surface ranges, assets by kind, per-connection lengths."""
    surface = model.surface
    by_kind: dict[str, int] = {}
    for a in model.assets:
        by_kind[a.kind] = by_kind.get(a.kind, 0) + 1
    return {
        "field": model.name,
        "surface": {
            "source": surface.source,
            "x_range_m": [float(surface.x_m[0]), float(surface.x_m[-1])],
            "y_range_m": [float(surface.y_m[0]), float(surface.y_m[-1])],
            "z_range_m": [float(surface.z_m.min()), float(surface.z_m.max())],
            "is_subsea": bool(surface.z_m.max() <= 0.0),
        },
        "assets_by_kind": by_kind,
        "assets": [
            {
                "id": a.asset_id,
                "kind": a.kind,
                "x_m": a.x_m,
                "y_m": a.y_m,
                "z_m": round(a.z_m, 3),
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
                "n_waypoints": len(c.waypoints_plan_m),
                "plan_length_m": round(c.plan_length_m, 2),
                "route_length_m": round(c.route_length_m, 2),
                "elevation_change_m": round(c.elevation_change_m, 2),
            }
            for c in model.connections
        ],
    }


def build_layout_model_from_file(config_path: str | Path) -> FieldLayoutModel:
    """Convenience: load + normalize + build in one call (paths resolve
    against the config file's directory)."""
    path = Path(config_path)
    config = load_layout_config(path)
    return build_layout_model(config, base_dir=path.parent)
