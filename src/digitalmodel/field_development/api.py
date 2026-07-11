# ABOUTME: Representative field-development API — layout / screen / visualize / analogs.
# ABOUTME: Issue #1512 (workstream E of epic #1507). Thin, iteration-cheap entry surface.
"""
digitalmodel.field_development.api
===================================

Workstream E (#1512) of the field-development epic (#1507): ONE coherent
entry surface over the domain modules landed in the earlier workstreams.
"API" here means the repo's convention — a clean set of plain-Python
functions (YAML config in, JSON-serializable dicts out), NOT an HTTP layer.

The four calls
--------------
- :func:`layout` — build the generalized layout model (#1509) and return its
  summary dict (assets, connections, true 3D lengths, ``is_subsea``).
- :func:`screen` — layout + flowline diameter sweeps (#1511) for every
  hydraulic connection, cable sizing screens when the config declares
  ``cables``, and an optional one-page markdown report.
- :func:`visualize` — 2D layout plot + 3D scene JSON (#1510), delegating to
  :func:`~digitalmodel.field_development.visualization.render_layout_visualization`.
- :func:`analogs` — thin adapter to the ``worldenergydata`` analog finder
  (cross-repo; see the function docstring for the contract).

Iteration cheapness (the design goal)
-------------------------------------
Real usage is several models per case with 10-20 iterations each, so every
call accepts ``**overrides``: dict-deep overrides of the YAML config applied
IN MEMORY, without editing the file. Mappings merge recursively; a mapping
override on a list of ``id``-keyed specs (``assets``, ``connections``,
``wells``, ``flowlines``, ``cables``) addresses entries BY ID; scalars
replace. Example — three rate variants of the demo field, no file edits:

>>> from digitalmodel.field_development import api  # doctest: +SKIP
>>> for rate in (280.0, 400.0, 520.0):  # doctest: +SKIP
...     result = api.screen(
...         "src/digitalmodel/field_development/data/onshore_demo_field.yml",
...         inlet_pressure_kpa=2000.0,
...         target_arrival_pressure_kpa=1500.0,
...         wells={"W-1": {"rate_m3_per_day": rate}},
...     )
...     print(rate, result["flowlines"][0]["selected"]["nps"])

Adapter honesty
---------------
:func:`screen` is where the generalized #1509 model meets the #1511
screening layer. ``screening.sweep_layout_flowlines`` consumes the #1508
tracer's ``FieldLayout`` objects, so THIS module adapts instead of editing
``screening.py``: it feeds each hydraulic connection of the generalized
model straight into
:func:`~digitalmodel.field_development.screening.flowline_diameter_sweep`,
resolving the line rate from (in order) an explicit ``rate_m3_per_day`` on
the connection spec, the from-asset's rate, a co-located rated asset (a tree
sits on its well), or the summed throughput of upstream hydraulic
connections. Connections whose rate cannot be resolved are reported under
``skipped_connections``, never silently dropped.
"""

from __future__ import annotations

import importlib
from pathlib import Path
from typing import Any, Optional

import yaml

from .layout_model import (
    FieldLayoutModel,
    RoutedConnection,
    build_layout_model,
    layout_summary,
)
from .screening import flowline_diameter_sweep, screening_report, size_cable

#: Connection kinds screened hydraulically by :func:`screen` (umbilicals
#: carry controls/chemicals, not the production bore, so they are excluded).
HYDRAULIC_CONNECTION_KINDS: tuple[str, ...] = ("jumper", "flowline", "pipeline")

#: Dotted path of the cross-repo analog finder (see :func:`analogs`).
ANALOGS_MODULE = "worldenergydata.field_development.analogs"

#: Plan-coordinate tolerance [m] for treating two assets as co-located
#: (e.g. a tree stacked on its well) when resolving line rates.
CO_LOCATION_TOLERANCE_M = 1.0e-6


# ---------------------------------------------------------------------------
# Config loading with dict-deep overrides
# ---------------------------------------------------------------------------


def deep_override(base: Any, override: Any) -> Any:
    """Merge ``override`` into ``base`` without mutating either.

    Rules (applied recursively):

    - mapping onto mapping — per-key recursive merge;
    - mapping onto a list of ``id``-keyed mappings — the override's keys
      address list entries BY ID (KeyError if an id is absent, so a typo
      never silently adds an asset);
    - anything else — the override value replaces the base value.
    """
    if isinstance(base, dict) and isinstance(override, dict):
        merged = dict(base)
        for key, value in override.items():
            merged[key] = deep_override(base[key], value) if key in base else value
        return merged
    if isinstance(base, list) and isinstance(override, dict):
        if not all(isinstance(entry, dict) and "id" in entry for entry in base):
            raise ValueError(
                "mapping override targets a list whose entries are not all "
                "'id'-keyed mappings; pass a full replacement list instead"
            )
        ids = [str(entry["id"]) for entry in base]
        merged_list: list[Any] = [dict(entry) for entry in base]
        for entry_id, patch in override.items():
            if str(entry_id) not in ids:
                raise KeyError(f"override id {entry_id!r} not found; available: {ids}")
            index = ids.index(str(entry_id))
            merged_list[index] = deep_override(merged_list[index], patch)
        return merged_list
    return override


def load_config_with_overrides(
    config_path: str | Path, overrides: dict[str, Any]
) -> dict[str, Any]:
    """Parse the YAML config and apply ``overrides`` (raw, pre-normalization).

    Overrides are applied to the config AS WRITTEN, so both the generalized
    schema (``assets``/``connections``) and the #1508 tracer schema
    (``host``/``wells``/``flowlines``) are addressable with the section names
    the author used. Schema normalization/validation happens downstream in
    :func:`~digitalmodel.field_development.layout_model.build_layout_model`.
    """
    path = Path(config_path)
    with open(path, "r", encoding="utf-8") as fh:
        config = yaml.safe_load(fh)
    if not isinstance(config, dict):
        raise ValueError(f"config {path} did not parse to a mapping")
    if overrides:
        config = deep_override(config, overrides)
    return config


def _build_model(
    config_path: str | Path, overrides: dict[str, Any]
) -> tuple[FieldLayoutModel, dict[str, Any]]:
    """Config file + overrides -> (built model, merged raw config)."""
    path = Path(config_path)
    config = load_config_with_overrides(path, overrides)
    model = build_layout_model(config, base_dir=path.parent)
    return model, config


# ---------------------------------------------------------------------------
# 1. layout
# ---------------------------------------------------------------------------


def layout(config_path: str | Path, **overrides: Any) -> dict[str, Any]:
    """Build the generalized layout model and return its summary dict.

    ``config_path`` is a YAML layout config (generalized #1509 schema or the
    #1508 tracer schema — both load). ``**overrides`` are dict-deep config
    overrides per :func:`deep_override`, keyed by top-level section name.

    Returns the
    :func:`~digitalmodel.field_development.layout_model.layout_summary`
    mapping: field name, surface ranges + ``is_subsea``, assets by kind, and
    per-connection plan/3D lengths. JSON-serializable.
    """
    model, _ = _build_model(config_path, overrides)
    return layout_summary(model)


# ---------------------------------------------------------------------------
# 2. screen — layout + flowline sweeps + cable screens (+ optional report)
# ---------------------------------------------------------------------------


def _co_located_rate(model: FieldLayoutModel, asset_id: str) -> Optional[float]:
    """Summed rate of OTHER rated assets at the same plan position, if any."""
    asset = model.asset(asset_id)
    total, found = 0.0, False
    for other in model.assets:
        if other.asset_id == asset.asset_id or other.rate_m3_per_day is None:
            continue
        if (
            abs(other.x_m - asset.x_m) <= CO_LOCATION_TOLERANCE_M
            and abs(other.y_m - asset.y_m) <= CO_LOCATION_TOLERANCE_M
        ):
            total += other.rate_m3_per_day
            found = True
    return total if found else None


def _asset_throughput(
    model: FieldLayoutModel,
    asset_id: str,
    kinds: tuple[str, ...],
    visiting: frozenset[str],
) -> Optional[float]:
    """Screening-grade throughput of an asset [m3/day], or None if unresolvable.

    own rate (or a co-located rated asset's — a tree sits on its well)
    + the resolved rates of all hydraulic connections INTO the asset.
    ``visiting`` guards against connection-graph cycles.
    """
    asset = model.asset(asset_id)
    own = asset.rate_m3_per_day
    if own is None:
        own = _co_located_rate(model, asset_id)
    total = own or 0.0
    for conn in model.connections:
        if conn.kind in kinds and conn.to_id == asset_id:
            inflow = _connection_rate(model, conn, kinds, visiting)
            if inflow is not None:
                total += inflow
    return total if total > 0.0 else None


def _connection_rate(
    model: FieldLayoutModel,
    conn: RoutedConnection,
    kinds: tuple[str, ...],
    visiting: frozenset[str] = frozenset(),
) -> Optional[float]:
    """Rate carried by one connection [m3/day], or None if unresolvable.

    Resolution order: explicit ``rate_m3_per_day`` on the connection spec
    (cheapest per-iteration override), then the from-asset's throughput
    (:func:`_asset_throughput`).
    """
    explicit = conn.properties.get("rate_m3_per_day")
    if explicit is not None:
        return float(explicit)
    if conn.from_id in visiting:  # cycle in the connection graph
        return None
    return _asset_throughput(model, conn.from_id, kinds, visiting | {conn.from_id})


def _screen_cables(
    config: dict[str, Any],
    connections: dict[str, RoutedConnection],
    defaults: Optional[dict[str, Any]],
) -> list[dict[str, Any]]:
    """Run :func:`~digitalmodel.field_development.screening.size_cable` for
    every spec in the config's ``cables`` section.

    Each spec needs ``load_kw``, ``line_voltage_v``, ``power_factor`` and a
    length: either ``length_m`` or ``connection: <id>`` (the routed
    connection's true 3D length — e.g. size a power core along an umbilical).
    """
    results: list[dict[str, Any]] = []
    for spec in config.get("cables") or []:
        if "length_m" in spec:
            length_m = float(spec["length_m"])
        elif "connection" in spec:
            ref = str(spec["connection"])
            if ref not in connections:
                raise ValueError(
                    f"cable {spec.get('id')!r} references unknown connection {ref!r}"
                )
            length_m = connections[ref].route_length_m
        else:
            raise ValueError(
                f"cable {spec.get('id')!r} needs 'length_m' or 'connection'"
            )
        result = size_cable(
            load_kw=float(spec["load_kw"]),
            line_voltage_v=float(spec["line_voltage_v"]),
            length_m=length_m,
            power_factor=float(spec["power_factor"]),
            defaults=defaults,
        )
        result["id"] = str(spec.get("id", spec.get("connection", "-")))
        results.append(result)
    return results


def screen(
    config_path: str | Path,
    *,
    inlet_pressure_kpa: Optional[float] = None,
    target_arrival_pressure_kpa: Optional[float] = None,
    fluid: Optional[dict[str, Any]] = None,
    connection_kinds: tuple[str, ...] = HYDRAULIC_CONNECTION_KINDS,
    screening_defaults: Optional[dict[str, Any]] = None,
    report_path: Optional[str | Path] = None,
    **overrides: Any,
) -> dict[str, Any]:
    """Layout + engineering screens for one config iteration.

    Builds the layout model, then for every connection whose kind is in
    ``connection_kinds`` runs the #1511 flowline diameter sweep (smallest
    standard pipe size meeting arrival pressure + the API RP 14E erosional
    limit) over the connection's TRUE 3D route length and endpoint elevation
    change. When the config declares a ``cables`` section, each entry gets
    the #1511 cable sizing screen (see :func:`_screen_cables` for the spec).

    Screening inputs resolve keyword-first, config-second: ``fluid`` falls
    back to the config's ``fluid`` section; the two pressures fall back to
    ``screening.inlet_pressure_kpa`` / ``screening.target_arrival_pressure_kpa``.
    A missing input raises ValueError (no invented numbers).
    ``screening_defaults`` overrides the bundled correlation constants
    (``data/screening_defaults.yml``); ``report_path`` additionally writes
    the deterministic one-page markdown report. ``**overrides`` are
    dict-deep config overrides per :func:`deep_override`.

    Returns ``{"field", "layout", "flowlines", "cables",
    "skipped_connections", "passes", "report_path"}`` — JSON-serializable;
    unresolvable lines land in ``skipped_connections`` with a reason.
    """
    model, config = _build_model(config_path, overrides)
    screening_cfg = config.get("screening") or {}
    if inlet_pressure_kpa is None:
        inlet_pressure_kpa = screening_cfg.get("inlet_pressure_kpa")
    if target_arrival_pressure_kpa is None:
        target_arrival_pressure_kpa = screening_cfg.get("target_arrival_pressure_kpa")
    if fluid is None:
        fluid = config.get("fluid")
    missing = [
        name
        for name, value in (
            ("inlet_pressure_kpa", inlet_pressure_kpa),
            ("target_arrival_pressure_kpa", target_arrival_pressure_kpa),
            ("fluid", fluid),
        )
        if value is None
    ]
    if missing:
        raise ValueError(
            f"screen() inputs missing (pass as keywords or config sections): {missing}"
        )

    flowlines: list[dict[str, Any]] = []
    skipped: list[dict[str, str]] = []
    for conn in model.connections:
        if conn.kind not in connection_kinds:
            continue
        rate = _connection_rate(model, conn, connection_kinds)
        if rate is None:
            skipped.append(
                {
                    "id": conn.connection_id,
                    "kind": conn.kind,
                    "reason": "no resolvable rate (no explicit, asset, "
                    "co-located, or upstream rate)",
                }
            )
            continue
        if conn.roughness_m is None:
            skipped.append(
                {
                    "id": conn.connection_id,
                    "kind": conn.kind,
                    "reason": "no roughness_m on the connection spec",
                }
            )
            continue
        sweep = flowline_diameter_sweep(
            rate_m3_per_day=rate,
            length_m=conn.route_length_m,
            elevation_change_m=conn.elevation_change_m,
            roughness_m=conn.roughness_m,
            density_kg_per_m3=float(fluid["density_kg_per_m3"]),
            viscosity_pa_s=float(fluid["viscosity_pa_s"]),
            inlet_pressure_kpa=float(inlet_pressure_kpa),
            target_arrival_pressure_kpa=float(target_arrival_pressure_kpa),
            defaults=screening_defaults,
        )
        sweep["id"] = conn.connection_id
        sweep["kind"] = conn.kind
        sweep["from"] = conn.from_id
        sweep["to"] = conn.to_id
        flowlines.append(sweep)

    connection_map = {c.connection_id: c for c in model.connections}
    cables = _screen_cables(config, connection_map, screening_defaults)

    screened = [*flowlines, *cables]
    result: dict[str, Any] = {
        "field": model.name,
        "layout": layout_summary(model),
        "flowlines": flowlines,
        "cables": cables,
        "skipped_connections": skipped,
        "passes": all(r["passes"] for r in screened) if screened else False,
        "report_path": None,
    }
    if report_path is not None:
        assumptions = [
            f"Fluid: density {float(fluid['density_kg_per_m3']):.1f} kg/m3, "
            f"viscosity {float(fluid['viscosity_pa_s']):.6f} Pa-s.",
            f"Inlet pressure {float(inlet_pressure_kpa):.1f} kPa; target "
            f"arrival {float(target_arrival_pressure_kpa):.1f} kPa.",
            "Line lengths/elevations from the routed layout model "
            "(true 3D route lengths).",
        ]
        result["report_path"] = screening_report(
            {
                "field": model.name,
                "assumptions": assumptions,
                "flowlines": flowlines,
                "cables": cables,
            },
            report_path,
        )
    return result


# ---------------------------------------------------------------------------
# 3. visualize
# ---------------------------------------------------------------------------


def visualize(
    config_path: str | Path,
    output_dir: str | Path,
    *,
    output_format: str = "png",
    dpi: int = 150,
    stem: Optional[str] = None,
    **overrides: Any,
) -> dict[str, Any]:
    """2D layout plot + 3D scene JSON for one config iteration.

    Delegates to
    :func:`~digitalmodel.field_development.visualization.render_layout_visualization`
    (#1510); with ``**overrides`` the merged config is built here first and
    rendered through the same plot/scene calls with the same artifact naming
    (``<stem>_layout.<format>``, ``<stem>_scene.json``; default stem = config
    file stem). Returns the layout summary plus ``plot_path``/``scene_path``.

    The visualization module (matplotlib) is imported lazily so
    :func:`layout`/:func:`screen` iterations never pay for it.
    """
    from . import visualization  # matplotlib import deferred to first use

    config_file = Path(config_path)
    if not overrides:
        return visualization.render_layout_visualization(
            config_file, output_dir, output_format=output_format, dpi=dpi, stem=stem
        )
    model, _ = _build_model(config_file, overrides)
    base = stem if stem is not None else config_file.stem
    out = Path(output_dir)
    plot_path = visualization.plot_field_layout(
        model,
        out / f"{base}_layout.{output_format}",
        output_format=output_format,
        dpi=dpi,
    )
    scene_path = visualization.export_layout_scene_json(
        model, out / f"{base}_scene.json"
    )
    result = layout_summary(model)
    result["plot_path"] = plot_path
    result["scene_path"] = scene_path
    return result


# ---------------------------------------------------------------------------
# 4. analogs — cross-repo adapter (worldenergydata)
# ---------------------------------------------------------------------------


def analogs(criteria: dict[str, Any]) -> dict[str, Any]:
    """Find analog fields/developments via the ``worldenergydata`` repo.

    Cross-repo contract (ADAPTER, documented here because ``digitalmodel``
    cannot depend on ``worldenergydata``): when the module named by
    :data:`ANALOGS_MODULE` (``worldenergydata.field_development.analogs``)
    is importable in the running environment, its ``find_analogs`` callable
    is invoked with ``criteria`` unpacked as keyword arguments and must
    return JSON-serializable data. When it is not importable (the normal
    case for a standalone ``digitalmodel`` install), this function does NOT
    raise — it returns a structured unavailability response so screening
    pipelines keep running::

        {"available": False, "reason": "...", "criteria": {...}}

    On success the response is
    ``{"available": True, "criteria": {...}, "result": <find_analogs(...)>}``.
    """
    if not isinstance(criteria, dict):
        raise TypeError(f"criteria must be a mapping, got {type(criteria).__name__}")
    try:
        module = importlib.import_module(ANALOGS_MODULE)
        find_analogs = module.find_analogs
    except (ImportError, AttributeError) as exc:
        return {
            "available": False,
            "reason": (
                f"{ANALOGS_MODULE}.find_analogs is not importable in this "
                f"environment ({exc}); install/point PYTHONPATH at the "
                "worldenergydata repo to enable analog lookups"
            ),
            "criteria": dict(criteria),
        }
    return {
        "available": True,
        "criteria": dict(criteria),
        "result": find_analogs(**criteria),
    }
