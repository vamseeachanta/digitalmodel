"""Input validation for the pipeline defect screen (inches/psi)."""

import numpy as np


def _positive_value(inputs, key):
    value = inputs.get(key)
    if not isinstance(value, (int, float)) or isinstance(value, bool):
        raise ValueError(f"{key} must be a number")
    if not np.isfinite(value) or value <= 0:
        raise ValueError(f"{key} must be finite and positive")


def _measurement_array(inputs, key):
    if key not in inputs:
        raise ValueError(f"Missing measurement field: {key}")
    array = np.asarray(inputs[key])
    if not np.issubdtype(array.dtype, np.number):
        raise ValueError(f"{key} requires numerical measurements")
    return array.astype(float)


def validate(inputs):
    if not isinstance(inputs.get("length_confirmed", False), bool):
        raise ValueError("length_confirmed must be a boolean")
    keys = (
        "nominal_od_in",
        "nominal_wt_in",
        "smys_psi",
        "smts_psi",
        "design_pressure_psi",
        "axial_stress_psi",
        "circumferential_width_in",
        "safety_factor",
        "usage_factor",
        "axial_design_factor",
    )
    f2 = inputs.get("usage_factor")
    if (
        isinstance(f2, bool)
        or not isinstance(f2, (int, float))
        or not np.isfinite(f2)
        or not 0 < f2 <= 1
    ):
        raise ValueError(
            "usage_factor (operational usage factor F2) must be finite and in (0, 1]"
        )
    for key in keys:
        _positive_value(inputs, key)
    if not isinstance(inputs.get("component_id"), str) or not inputs["component_id"]:
        raise ValueError("component_id must be a nonempty string")
    if inputs["safety_factor"] < 1 or inputs["axial_design_factor"] > 1:
        raise ValueError("safety_factor >= 1 and design factors <= 1 required")
    t, diameter = inputs["nominal_wt_in"], inputs["nominal_od_in"]
    if t >= diameter / 2 or inputs["smts_psi"] < inputs["smys_psi"]:
        raise ValueError("Invalid pipe geometry or tensile strength below yield")
    grid = _measurement_array(inputs, "grid")
    positions = _measurement_array(inputs, "axial_positions_in")
    if (
        grid.ndim != 2
        or grid.shape[0] < 2
        or grid.shape[1] < 1
        or positions.shape != (grid.shape[0],)
    ):
        raise ValueError("A rectangular axial-by-circumferential grid is required")
    if (
        not np.isfinite(grid).all()
        or not np.isfinite(positions).all()
        or np.any(np.diff(positions) <= 0)
        or np.any(grid < 0)
        or np.any(grid > t)
    ):
        raise ValueError("Finite ordered positions and thickness in [0, t] required")
    return positions, grid, t - grid
