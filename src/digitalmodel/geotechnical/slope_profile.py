# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Validate SI layered profiles before optional slope solver imports."""

from __future__ import annotations

from copy import deepcopy
import math
from typing import Any


def _number(value: Any, name: str, minimum: float | None = None) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value):
        raise ValueError(f"{name} must be a finite number")
    if minimum is not None and value < minimum:
        raise ValueError(f"{name} must be at least {minimum}")
    return float(value)


def _line(value: Any, name: str, *, increasing_x: bool = True) -> list[list[float]]:
    if not isinstance(value, (list, tuple)) or len(value) < 2:
        raise ValueError(f"{name} needs at least two points")
    points = []
    for point in value:
        if not isinstance(point, (list, tuple)) or len(point) != 2:
            raise ValueError(f"{name} points need x and elevation")
        points.append([_number(v, name) for v in point])
    if any(a == b for a, b in zip(points, points[1:])):
        raise ValueError(f"{name} cannot contain zero-length segments")
    if increasing_x and any(a[0] >= b[0] for a, b in zip(points, points[1:])):
        raise ValueError(f"{name} x coordinates must increase strictly")
    return points


def _elevation(line: list[list[float]], x: float) -> float:
    for a, b in zip(line, line[1:]):
        if a[0] <= x <= b[0]:
            return a[1] + (x - a[0]) * (b[1] - a[1]) / (b[0] - a[0])
    raise ValueError("boundary does not span the ground surface")


def _boundary(top: list[list[float]], lower: list[list[float]]) -> None:
    if [top[0][0], top[-1][0]] != [lower[0][0], lower[-1][0]]:
        raise ValueError("boundary must span the same x range")
    xs = sorted({p[0] for p in top + lower})
    gaps = [_elevation(top, x) - _elevation(lower, x) for x in xs]
    if min(gaps) < 0 or max(gaps) <= 0:
        raise ValueError("boundary crosses or duplicates the overlying boundary")


def _alias(layer: dict[str, Any], canonical: str, alias: str) -> None:
    if alias in layer:
        if canonical in layer and layer[canonical] != layer[alias]:
            raise ValueError(f"conflicting {canonical} and {alias}")
        layer[canonical] = layer.pop(alias)


def _strength(layer: dict[str, Any]) -> None:
    _alias(layer, "su_kpa", "Su_kpa")
    if "su_kpa" in layer:
        if "c_kpa" in layer or "phi_deg" in layer:
            raise ValueError("choose undrained Su or drained c and phi")
        layer["su_kpa"] = _number(layer["su_kpa"], "su_kpa", 0)
        layer["su_gradient_kpa_per_m"] = _number(
            layer.get("su_gradient_kpa_per_m", 0), "su_gradient_kpa_per_m", 0
        )
        # xslope 1.0.3 slice.py, cp rule:
        # Su(y) = Su_ref + gradient * max(0, reference_elevation - y).
        # A positive gradient therefore requires an explicit elevation datum.
        if layer["su_gradient_kpa_per_m"] > 0 and "reference_elevation_m" not in layer:
            raise ValueError("positive strength gradient requires reference_elevation_m")
        layer["reference_elevation_m"] = _number(
            layer.get("reference_elevation_m", 0), "reference_elevation_m"
        )
    else:
        if "su_gradient_kpa_per_m" in layer or "reference_elevation_m" in layer:
            raise ValueError("strength gradient requires Su")
        layer["c_kpa"] = _number(layer.get("c_kpa"), "c_kpa", 0)
        layer["phi_deg"] = _number(layer.get("phi_deg"), "phi_deg", 0)
        if layer["phi_deg"] >= 90:
            raise ValueError("phi_deg must be below 90")


def _layer(layer: dict[str, Any]) -> None:
    allowed = {"top_boundary_m", "unit_weight_kn_m3", "gamma_kn_m3",
               "saturated_unit_weight_kn_m3", "c_kpa", "phi_deg", "su_kpa",
               "Su_kpa", "su_gradient_kpa_per_m", "reference_elevation_m",
               "permeability_m_s"}
    if set(layer) - allowed:
        raise ValueError("unsupported layer fields")
    _alias(layer, "unit_weight_kn_m3", "gamma_kn_m3")
    weight = _number(layer.get("unit_weight_kn_m3"), "unit_weight_kn_m3", 0)
    if weight == 0:
        raise ValueError("unit_weight_kn_m3 must be positive")
    layer["unit_weight_kn_m3"] = weight
    if "saturated_unit_weight_kn_m3" in layer:
        _number(layer["saturated_unit_weight_kn_m3"], "saturated_unit_weight_kn_m3", weight)
    if "permeability_m_s" in layer:
        if _number(layer["permeability_m_s"], "permeability_m_s", 0) == 0:
            raise ValueError("permeability_m_s must be positive")
    _strength(layer)


def _strength_datums(layers: list[dict[str, Any]], bottom: float) -> None:
    for index, layer in enumerate(layers):
        if layer.get("su_gradient_kpa_per_m", 0) <= 0:
            continue
        lower_min = bottom
        if index + 1 < len(layers):
            lower_min = min(point[1] for point in layers[index + 1]["top_boundary_m"])
        # xslope 1.0.3 slice.py, cp rule (see the slope screening guide):
        # Su(y) = Su_ref + gradient * max(0, reference_elevation - y).
        # Below the entire layer the datum makes the gradient term zero.
        # Linear boundary segments attain their minima at vertices, including
        # interior vertices; the next layer top is this layer's lower boundary.
        if layer["reference_elevation_m"] < lower_min:
            raise ValueError(
                f"layers[{index}].reference_elevation_m is below the entire "
                "layer lower boundary; positive strength gradient is inactive"
            )


def _seepage(profile: dict[str, Any]) -> None:
    if "seepage" not in profile:
        return
    seep = profile["seepage"]
    if not isinstance(seep, dict):
        raise ValueError("seepage must be a mapping")
    if set(seep) - {"element_size_m", "heads_m"}:
        raise ValueError("unsupported seepage fields")
    if _number(seep.get("element_size_m"), "element_size_m", 0) == 0:
        raise ValueError("element_size_m must be positive")
    heads = seep.get("heads_m", [])
    if not isinstance(heads, (list, tuple)):
        raise ValueError("seepage heads_m must be a sequence")
    if not 2 <= len(heads) <= 5:
        raise ValueError("seepage needs two to five head boundaries")
    for head in heads:
        if not isinstance(head, dict) or set(head) != {"head_m", "boundary_m"}:
            raise ValueError("head boundary needs head_m and boundary_m")
        _number(head["head_m"], "head_m")
        # xslope v1.0.3 fileio.py _read_seep_bc_sheet: head coordinates are
        # polylines, including vertical model sides, rather than y(x) surfaces.
        head["boundary_m"] = _line(head["boundary_m"], "boundary_m", increasing_x=False)
    if any("permeability_m_s" not in layer for layer in profile["layers"]):
        raise ValueError("seepage needs permeability_m_s for every layer")


def validate_profile(profile: dict[str, Any]) -> dict[str, Any]:
    """Return a checked copy of an SI profile; reject unsupported fields.

    Coordinates are metres, weights kN/m³, strengths kPa, angles degrees,
    conductivity m/s and the pseudo-static horizontal coefficient dimensionless.
    Each layer starts at its top boundary; the first starts at ground level.
    """
    data = deepcopy(profile)
    allowed = {"units", "ground_surface_m", "bottom_elevation_m", "layers",
               "water_table_m", "water_unit_weight_kn_m3", "seismic_coefficient",
               "circles", "seepage"}
    if set(data) - allowed or data.get("units") != "SI":
        raise ValueError("profile requires SI units and supported fields")
    ground = _line(data.get("ground_surface_m"), "ground_surface_m")
    data["ground_surface_m"] = ground
    bottom = _number(data.get("bottom_elevation_m"), "bottom_elevation_m")
    data["seismic_coefficient"] = _number(
        data.get("seismic_coefficient", 0), "seismic_coefficient", 0
    )
    if _number(data.get("water_unit_weight_kn_m3"), "water_unit_weight_kn_m3", 0) == 0:
        raise ValueError("water_unit_weight_kn_m3 must be positive")
    layers = data.get("layers", [])
    if not 1 <= len(layers) <= 15:
        raise ValueError("one to fifteen layers are supported")
    previous = ground
    for index, layer in enumerate(layers):
        _layer(layer)
        if index == 0 and "top_boundary_m" in layer:
            raise ValueError("first layer starts at ground_surface_m")
        top = ground if index == 0 else _line(layer.get("top_boundary_m"), "boundary")
        if index:
            _boundary(previous, top)
        layer["top_boundary_m"] = top
        previous = top
    _boundary(previous, [[ground[0][0], bottom], [ground[-1][0], bottom]])
    _strength_datums(layers, bottom)
    if "water_table_m" in data:
        water = _line(data["water_table_m"], "water_table_m")
        if [water[0][0], water[-1][0]] != [ground[0][0], ground[-1][0]]:
            raise ValueError("water table must span the ground surface")
        data["water_table_m"] = water
    circles = data.get("circles", [])
    if not 1 <= len(circles) <= 10:
        raise ValueError("one to ten starting circles are required")
    for circle in circles:
        if set(circle) != {"x_m", "y_m", "radius_m"}:
            raise ValueError("circle needs x_m, y_m and radius_m")
        for key, value in circle.items():
            _number(value, key)
        if circle["radius_m"] <= 0:
            raise ValueError("radius_m must be positive")
    _seepage(data)
    return data
