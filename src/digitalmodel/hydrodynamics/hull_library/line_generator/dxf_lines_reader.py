"""Read explicitly selected DXF body-plan layers into keel-up station offsets."""

from pathlib import Path
from typing import Literal

import numpy as np
from pydantic import BaseModel, ConfigDict, Field, field_validator

from ..profile_schema import HullProfile
from .dxf_entities import (
    UNIT_CODES,
    UNIT_FACTORS,
    DxfLinesError,
    DxfReadReport,
    assign_labels,
    collect,
    flatten,
    join_fragments,
    require_ezdxf,
)
from .line_parser import HullLineDefinition, StationOffset


class DxfLinesConfig(BaseModel):
    """Geometry/origins/tolerance in drawing units; station_x in full-scale metres."""

    model_config = ConfigDict(extra="forbid", allow_inf_nan=False)
    body_plan_layers: list[str] = Field(min_length=1)
    half_breadth_layers: list[str] | None = None
    sheer_layers: list[str] | None = None
    station_x: list[float] | None = None
    station_label_layer: str | None = "STATIONS"
    units: Literal["auto", "mm", "m", "ft", "in"] = "auto"
    scale: float = Field(default=1, gt=0)
    centreline_y: float = 0
    baseline_z: float = 0
    mirror_side: Literal["port", "starboard", "both"] = "both"
    n_waterlines: int = Field(default=201, ge=2, le=100001)
    tolerance: float = Field(default=1e-6, gt=0)

    @field_validator("body_plan_layers")
    @classmethod
    def validate_layers(cls, layers):
        if any(not layer.strip() for layer in layers):
            raise ValueError("body_plan_layers must contain nonempty names")
        if len({layer.casefold() for layer in layers}) != len(layers):
            raise ValueError("body_plan_layers must be unique")
        return layers


def _unit_factor(doc, config, report):
    unit = config.units
    if unit == "auto":
        code = doc.header.get("$INSUNITS", 0)
        unit = next((u for u, c in UNIT_CODES.items() if c == code), None)
        if unit is None:
            raise DxfLinesError(
                f"Unsupported $INSUNITS={code}; specify units mm/m/ft/in", report
            )
    report.units = unit
    return UNIT_FACTORS[unit] * config.scale


def _curves(entities, config, factor, report):
    rough = [flatten(e, config.tolerance, report, i) for i, e in enumerate(entities)]
    half_beam = max(
        float(np.max(abs(c.points[:, 0] - config.centreline_y))) for c in rough
    )
    if half_beam <= 0:
        raise DxfLinesError("Zero beam: " + "; ".join(c.context for c in rough), report)
    chord = half_beam * 1e-4
    report.flattening_tolerance_m = chord * factor
    # Resolve more finely than the reported bound near horizontal tangents.
    curves = [flatten(e, chord / 100, report, i) for i, e in enumerate(entities)]
    for entity in entities:
        report.count("used", entity)
    return join_fragments(curves, config.tolerance, report)


def _positions(curves, labels, config, factor, report):
    if config.station_x is not None:
        xs = config.station_x
        if len(xs) != len(curves) or np.any(np.diff(xs) <= 0):
            raise DxfLinesError(
                "station_x must match curve count and strictly increase: "
                + "; ".join(c.context for c in curves),
                report,
            )
    else:
        xs = assign_labels(curves, labels, config, factor, report)
    if len(xs) < 2 or len(set(xs)) != len(xs) or min(xs) < 0:
        raise DxfLinesError(
            "Stations need at least two distinct nonnegative x positions: "
            + "; ".join(c.context for c in curves),
            report,
        )
    return xs


def _offsets(curve, config, factor, grid, report):
    y = curve.points[:, 0] - config.centreline_y
    z = curve.points[:, 1] - config.baseline_z
    eps = config.tolerance
    if np.min(z) < -eps:
        raise DxfLinesError(f"Station below baseline: {curve.context}", report)
    has_port, has_starboard = np.any(y < -eps), np.any(y > eps)
    wrong_side = (config.mirror_side == "port" and has_starboard) or (
        config.mirror_side == "starboard" and has_port
    )
    if wrong_side or (has_port and has_starboard):
        raise DxfLinesError(
            f"Station crosses or violates configured side: {curve.context}", report
        )
    z, y = np.maximum(z, 0) * factor, abs(y) * factor
    if np.any(np.diff(z) <= 0):
        raise DxfLinesError(
            f"Non-monotone station at baseline: {curve.context}", report
        )
    local = np.unique(np.r_[z[0], grid[(grid > z[0]) & (grid < z[-1])], z[-1]])
    return list(zip(local.tolist(), np.interp(local, z, y).tolist()))


def _definition(curves, xs, config, factor, report):
    top = max(c.points[-1, 1] - config.baseline_z for c in curves) * factor
    if top <= 0:
        raise DxfLinesError(
            "No positive station height: " + "; ".join(c.context for c in curves),
            report,
        )
    grid = np.linspace(0, top, config.n_waterlines)
    stations = []
    for curve, x in zip(curves, xs):
        offsets = _offsets(curve, config, factor, grid, report)
        stations.append(StationOffset(x=x, offsets=offsets))
        report.station_assignments.append(
            {
                "x": x,
                "handles": curve.handles,
                "layers": curve.layers,
                "method": "station_x" if config.station_x is not None else "label",
            }
        )
        report.stations_found += 1
    beam = 2 * max(y for s in stations for _, y in s.offsets)
    return HullLineDefinition(
        name="dxf_body_plan",
        hull_type="custom",
        length_bp=max(xs),
        beam=beam,
        draft=top,
        depth=top,
        source="DXF body plan",
        stations=stations,
    )


def read_dxf_body_plan_with_report(
    path, config
) -> tuple[HullLineDefinition, DxfReadReport]:
    """Read modelspace only; diagnostics survive failure on DxfLinesError.report."""
    ezdxf = require_ezdxf()
    config = DxfLinesConfig.model_validate(config)
    report = DxfReadReport()
    if config.half_breadth_layers or config.sheer_layers:
        report.warnings.append(
            "Half-breadth and sheer reconciliation is not implemented in A1"
        )
    try:
        doc = ezdxf.readfile(path)
    except (OSError, ezdxf.DXFError) as exc:
        raise DxfLinesError(f"Cannot read DXF ({type(exc).__name__})", report) from exc
    entities, labels = collect(doc, config, report)
    factor = _unit_factor(doc, config, report)
    curves = _curves(entities, config, factor, report)
    xs = _positions(curves, labels, config, factor, report)
    return _definition(curves, xs, config, factor, report), report


def read_dxf_body_plan(path, config) -> HullLineDefinition:
    """Convenience reader without the successful read report."""
    return read_dxf_body_plan_with_report(path, config)[0]


def hull_line_definition_to_profile(
    defn, *, name, hull_type, length_bp, beam, draft, depth
) -> HullProfile:
    """Reuse the existing conversion after validating explicit full-scale dimensions."""
    for station in defn.stations:
        offsets = sorted(station.offsets)
        if offsets[-1][0] < draft or (offsets[0][0] > 0 and offsets[0][1] > 0):
            raise DxfLinesError(
                f"Incomplete submerged coverage at station x={station.x}"
            )
    data = defn.model_dump()
    data.update(
        name=name,
        hull_type=hull_type,
        length_bp=length_bp,
        beam=beam,
        draft=draft,
        depth=depth,
    )
    return HullLineDefinition.model_validate(data).to_hull_profile()


def write_body_plan_dxf(
    profile, path, *, layer="BODY_PLAN", label_layer="STATIONS", units="m"
):
    """Export starboard XY curves; coincident stations require station_x on import."""
    ezdxf = require_ezdxf()
    if units not in UNIT_FACTORS:
        raise DxfLinesError("Export units must be mm, m, ft or in")
    if (
        not layer.strip()
        or not label_layer.strip()
        or layer.casefold() == label_layer.casefold()
    ):
        raise DxfLinesError(
            "Body and station label layers must be nonempty and distinct"
        )
    doc = ezdxf.new("R2010")
    doc.units = UNIT_CODES[units]
    for name in (layer, label_layer):
        if name not in doc.layers:
            doc.layers.new(name)
    factor = UNIT_FACTORS[units]
    msp = doc.modelspace()
    for station in profile.stations:
        points = [(y / factor, z / factor) for z, y in station.waterline_offsets]
        msp.add_lwpolyline(points, dxfattribs={"layer": layer})
        msp.add_text(
            format(station.x_position / factor, ".17g"),
            dxfattribs={
                "layer": label_layer,
                "insert": points[-1],
                "height": profile.depth / factor * 0.02,
            },
        )
    path = Path(path)
    doc.saveas(path)
    return path
