# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Current drift force magnitude using explicit SI inputs (issue #487).

The quadratic-load equation follows the existing repository implementation
in ``digitalmodel.tug.bollard_pull.escort_force``:
``F = 0.5 * rho * V**2 * A * C``. The one-half factor comes from dynamic
pressure; no fluid properties or drag coefficients are prescribed here.
"""

import math


def current_drift_force(
    density_kg_m3: float,
    current_speed_m_s: float,
    projected_area_m2: float,
    drag_coefficient: float,
) -> float:
    """Return the quasi-static current drag magnitude in newtons.

    Args:
        density_kg_m3: Finite positive fluid density in kg/m3.
        current_speed_m_s: Finite nonnegative current speed relative to the
            body in m/s.
        projected_area_m2: Finite nonnegative submerged area projected normal
            to the current, in m2.
        drag_coefficient: Finite nonnegative dimensionless drag coefficient
            corresponding to the supplied projected area.

    Returns:
        Force magnitude in newtons from dynamic pressure times coefficient
        and projected area, using ordinary floating-point arithmetic.

    Raises:
        ValueError: An input is nonfinite, density is not positive, or any
            other input is negative.

    Wind, force direction and towing-system dynamics are outside this model.
    """
    if not math.isfinite(density_kg_m3) or density_kg_m3 <= 0:
        raise ValueError("density_kg_m3 must be finite and positive")
    for name, value in (
        ("current_speed_m_s", current_speed_m_s),
        ("projected_area_m2", projected_area_m2),
        ("drag_coefficient", drag_coefficient),
    ):
        if not math.isfinite(value) or value < 0:
            raise ValueError(f"{name} must be finite and nonnegative")

    return (
        0.5
        * density_kg_m3
        * current_speed_m_s**2
        * projected_area_m2
        * drag_coefficient
    )
