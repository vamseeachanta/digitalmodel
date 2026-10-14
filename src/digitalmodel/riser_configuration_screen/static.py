# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Invert existing catenary kernels for fixed-length excursion cases.

Lengths are in metres and forces in newtons. The grounded bare tail is
frictionless. Buoyancy remains attached to fixed material arc coordinates.
The general SCR kernel is used in its inextensible limit; elastic response,
soil interaction and dynamic response are outside this screen.
"""

import math
from typing import Any

import numpy as np
from scipy.optimize import brentq, least_squares

from digitalmodel.marine_ops.marine_analysis.catenary.lazy_wave import (
    LazyWaveConfiguration,
    LazyWaveSolver,
)
from digitalmodel.marine_ops.marine_analysis.catenary.solver import (
    CatenaryInput,
    CatenarySolver,
)

CLOSURE_M = 1e-6
INEXTENSIBLE_EA_N = 1e20


def _positive(data: dict, name: str) -> float:
    value = float(data[name])
    if not math.isfinite(value) or value <= 0:
        raise ValueError(f"{name} must be finite and positive")
    return value


def _lazy(data: dict, parameters: list[float] | np.ndarray) -> Any:
    angle, sag, rise = parameters
    depth = _positive(data, "water_depth_m")
    weight = _positive(data, "submerged_weight_n_m")
    factor = _positive(data, "buoyancy_factor")
    return LazyWaveSolver().solve(
        LazyWaveConfiguration(
            hangoff_angle=float(angle),
            hangoff_below_msl=0.0,
            hog_bend_above_seabed=float(sag + rise),
            sag_bend_elevation=float(sag),
            weight_without_buoyancy=weight,
            weight_with_buoyancy=weight * (1 - factor),
            vertical_distance=depth,
        )
    )


def _buoyancy_lengths(result: Any) -> tuple[float, float]:
    start = result.hangoff_to_sag.arc_length + result.sag_to_buoyancy.arc_length
    length = result.buoyancy_to_hog.arc_length + result.hog_to_buoyancy_end.arc_length
    return start, length


def _lazy_shape(result: Any, depth: float) -> list[list[float]]:
    shape = [[0.0, 0.0, depth]]
    vertical = result.vertical_force
    horizontal = result.horizontal_force
    for segment in result.segments:
        start_s, start_x, start_z = shape[-1]
        weight = segment.weight_per_length
        end_vertical = vertical - weight * segment.arc_length
        for fraction in np.linspace(0.0, 1.0, 21)[1:]:
            distance = float(fraction) * segment.arc_length
            force = vertical - weight * distance
            x = horizontal / weight * (
                math.asinh(vertical / horizontal) - math.asinh(force / horizontal)
            )
            z = (math.hypot(horizontal, force) - math.hypot(horizontal, vertical)) / weight
            shape.append([start_s + distance, start_x + x, start_z + z])
        vertical = end_vertical
    return shape


def _lazy_row(data: dict, result: Any, offset: float, target: float, length: float) -> dict:
    depth = float(data["water_depth_m"])
    start, buoyed = _buoyancy_lengths(result)
    horizontal = result.horizontal_force
    weight = float(data["submerged_weight_n_m"])
    shape = _lazy_shape(result, depth)
    ground = length - result.total_arc_length
    closure_x = result.total_horizontal_distance + ground - target
    closure_z = shape[-1][2]
    if max(abs(closure_x), abs(closure_z)) > CLOSURE_M:
        raise ValueError("excursion shape does not close")
    if ground < 0:
        raise ValueError("grounded bare tail exhausted")
    return {
        "offset_m": offset,
        "shape": shape,
        "vertical_closure_error_m": closure_z,
        "horizontal_closure_error_m": closure_x,
        "horizontal_span_m": result.total_horizontal_distance,
        "suspended_length_m": result.total_arc_length,
        "grounded_length_m": ground,
        "horizontal_tension_n": horizontal,
        "touchdown_effective_tension_n": horizontal,
        "top_tension_n": result.total_tension,
        "top_angle_deg": math.degrees(math.atan2(horizontal, result.vertical_force)),
        "buoyed_start_m": start,
        "buoyed_length_m": buoyed,
        "sag_radius_m": horizontal / weight,
        "hog_radius_m": horizontal / abs(weight * (1 - float(data["buoyancy_factor"]))),
        "hog_clearance_m": depth - result.hangoff_to_sag.vertical_distance
        + result.sag_to_buoyancy.vertical_distance + result.buoyancy_to_hog.vertical_distance,
        "touchdown_curvature_per_m": weight / horizontal,
        "sag_curvature_per_m": weight / horizontal,
    }


def lazy_wave_offsets(data: dict, offsets: list[float]) -> list[dict]:
    """Preserve installed buoyancy and total length while moving the touchdown.

    Args:
        data: Synthetic or qualified static configuration in SI units.
        offsets: Radial horizontal span changes in metres.

    Returns:
        Closure-checked per-offset shapes and static forces.

    Raises:
        ValueError: If buoyancy is insufficient or an excursion cannot close.
    """
    depth = _positive(data, "water_depth_m")
    parameters = [float(data["hangoff_angle_deg"]), depth * float(data["sag_fraction"]),
                  depth * (float(data["hog_fraction"]) - float(data["sag_fraction"]))]
    baseline = _lazy(data, parameters)
    start, buoyed = _buoyancy_lengths(baseline)
    ground = _positive(data, "grounded_length_m")
    length = baseline.total_arc_length + ground
    span = baseline.total_horizontal_distance + ground
    rows = []
    for offset in offsets:
        target = span + float(offset)
        if not math.isfinite(target) or target <= 0:
            raise ValueError("excursion span must be finite and positive")
        def residual(values: np.ndarray) -> list[float]:
            result = _lazy(data, values)
            actual_start, actual_buoyed = _buoyancy_lengths(result)
            return [(actual_start - start) / depth, (actual_buoyed - buoyed) / depth,
                    (result.total_horizontal_distance + length - result.total_arc_length - target) / depth]
        fit = least_squares(residual, parameters,
                            bounds=([0.001, 0.001, 0.001], [89.0, depth - 0.001, depth]),
                            xtol=1e-12, ftol=1e-12, gtol=1e-12)
        if not fit.success or max(abs(v) * depth for v in residual(fit.x)) > CLOSURE_M:
            raise ValueError("fixed-buoyancy excursion did not converge")
        rows.append(_lazy_row(data, _lazy(data, fit.x), float(offset), target, length))
    return rows


def scr_offsets(data: dict, offsets: list[float]) -> list[dict]:
    """Use the general kernel for an inextensible SCR with mobile touchdown.

    Args:
        data: Static configuration in SI units.
        offsets: Radial horizontal span changes in metres.

    Returns:
        Closure-checked per-offset SCR shapes and forces.

    Raises:
        ValueError: If an excursion exhausts the grounded tail.
    """
    depth = _positive(data, "water_depth_m")
    weight = _positive(data, "submerged_weight_n_m")
    angle = math.radians(_positive(data, "hangoff_angle_deg"))
    if angle >= math.pi / 2:
        raise ValueError("hangoff angle must be below 90 degrees")
    a = depth * math.sin(angle) / (1 - math.sin(angle))
    suspended = math.sqrt(depth**2 + 2 * a * depth)
    span = a * math.asinh(suspended / a)
    ground = _positive(data, "grounded_length_m")
    length = suspended + ground
    maximum_a = (length**2 - depth**2) / (2 * depth)
    rows = []
    for offset in offsets:
        target = span + ground + float(offset)
        def residual(parameter: float) -> float:
            arc = math.sqrt(depth**2 + 2 * parameter * depth)
            return parameter * math.asinh(arc / parameter) + length - arc - target
        parameter = brentq(residual, 1e-6, maximum_a, xtol=1e-9)
        arc = math.sqrt(depth**2 + 2 * parameter * depth)
        horizontal = parameter * math.asinh(arc / parameter)
        result = CatenarySolver(tolerance=1e-9).solve(
            CatenaryInput(arc, horizontal, depth, weight, INEXTENSIBLE_EA_N)
        )
        shape = [[arc - float(s), horizontal - float(x), float(z)]
                 for s, x, z in zip(np.linspace(0, arc, len(result.shape_x)), result.shape_x, result.shape_y)]
        shape.reverse()
        rows.append({
            "offset_m": float(offset), "shape": shape,
            "vertical_closure_error_m": shape[-1][2],
            "horizontal_closure_error_m": horizontal + length - arc - target,
            "horizontal_span_m": horizontal, "suspended_length_m": arc,
            "grounded_length_m": length - arc,
            "horizontal_tension_n": result.horizontal_tension,
            "touchdown_effective_tension_n": result.total_tension_anchor,
            "top_tension_n": result.total_tension_fairlead,
            "top_angle_deg": math.degrees(math.atan2(result.horizontal_tension, result.vertical_tension_fairlead)),
            "buoyed_start_m": None, "buoyed_length_m": 0.0,
            "sag_radius_m": None, "hog_radius_m": None, "hog_clearance_m": None,
            "touchdown_curvature_per_m": 1 / parameter, "sag_curvature_per_m": None,
        })
    return rows
