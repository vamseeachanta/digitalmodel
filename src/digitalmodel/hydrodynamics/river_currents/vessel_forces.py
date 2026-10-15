# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Depth-integrated steady barge/tug loading using the existing load framework.

References
----------
NASA Glenn, Conservation of Mass:
https://www.grc.nasa.gov/www/k-12/airplane/mass.html (opened 2026-10-14).
For an assumed full-width under-keel slot, continuity gives u_gap/u = H/(H-T).
Quadratic drag then gives an effective Cd multiplier [H/(H-T)]**2. This is an
explicit screening hypothesis, not an empirical shallow-water barge fit; it
ignores side bypass and diverges at grounding. A measured coefficient and a
flow solution are required for operational use.

``OCIMFLoading.current_load`` is reused for its existing approximate force
and yaw coefficients; no licensed coefficient table is imported. Its cardinal
angle floating-point sign artefact is removed by setting the corresponding
surge/sway/yaw component to zero on the axis. ``passing_ship`` is not called:
its moving-vessel interaction history is not the steady ambient-current load
calculated here. ``tug.bollard_pull`` estimates capacity from installed power;
this module instead computes demand and accepts supplied capacity in newtons.

Midpoint quadrature integrates signed u*abs(u) over the occupied draft,
from height H-T to H above bed. Relative vessel motion is collinear with the
specified current direction. Tug self-resistance and speed-dependent available
thrust are not predicted. Yaw capacity must be supplied for a nonzero moment.
"""

import math
from collections.abc import Callable

from digitalmodel.hydrodynamics.models import VesselProperties
from digitalmodel.hydrodynamics.ocimf_loading import OCIMFLoading

from .bank_effects import _positive


def shallow_drag_factor(water_depth: float, draft: float) -> float:
    """Return the assumed full-width slot drag multiplier; require positive clearance."""
    _positive(water_depth=water_depth, draft=draft)
    if water_depth <= draft:
        raise ValueError("water depth must exceed draft")
    return (water_depth / (water_depth - draft)) ** 2


def current_load(vessel: VesselProperties, profile: Callable[[float], float],
                 water_depth: float, *, direction_deg: float = 0,
                 water_density: float = 1000, vessel_speed: float = 0,
                 n_slices: int = 1000) -> dict[str, float]:
    """Integrate a scalar bed-referenced signed profile over draft; return N and N·m.

    Density defaults to the synthetic freshwater assumption of 1000 kg/m³.
    Direction is relative to the bow, positive towards the positive sway axis.
    Vessel speed (m/s) is subtracted from the signed profile before integration.
    The slot correction is always applied; it is not a channel blockage model.
    """
    _positive(length=vessel.length_overall, beam=vessel.beam, draft=vessel.draft,
              water_density=water_density)
    factor = shallow_drag_factor(water_depth, vessel.draft)
    if not isinstance(n_slices, int) or isinstance(n_slices, bool) or n_slices < 1:
        raise ValueError("n_slices must be a positive integer")
    if not math.isfinite(direction_deg) or not math.isfinite(vessel_speed):
        raise ValueError("direction and vessel speed must be finite")
    direction = (direction_deg + 180) % 360 - 180
    unit = OCIMFLoading().current_load(vessel, 1, direction, water_density)
    theta = math.radians(direction)
    if abs(math.cos(theta)) < 1e-12:
        unit['Fx_surge_N'] = 0
    if abs(math.sin(theta)) < 1e-12:
        unit['Fy_sway_N'] = 0
        unit['Mz_yaw_Nm'] = 0
    values = []
    for i in range(n_slices):
        height = water_depth - vessel.draft + vessel.draft * (i + 0.5) / n_slices
        speed = float(profile(height)) - vessel_speed
        if not math.isfinite(speed):
            raise ValueError("profile must return finite velocities")
        values.append(speed * abs(speed))
    pressure_ratio = math.fsum(values) / n_slices * factor
    return {key: float(unit[key] * pressure_ratio)
            for key in ('Fx_surge_N', 'Fy_sway_N', 'Mz_yaw_Nm')}


def towing_assessment(loads: dict[str, float], available_pull_N: float, *,
                      efficiency: float, reserve_factor: float,
                      yaw_capacity_Nm: float | None = None) -> dict:
    """Compare resultant force and absolute yaw moment with supplied capacities.

    Available pull is nominal bollard pull (N); efficiency in (0,1] represents
    the stated thrust/towline derating. Reserve factor is at least one.
    A positive verdict addresses only these steady force and yaw criteria.
    """
    if not math.isfinite(available_pull_N) or available_pull_N < 0:
        raise ValueError("available pull must be finite and nonnegative")
    _positive(efficiency=efficiency, reserve_factor=reserve_factor)
    if efficiency > 1 or reserve_factor < 1:
        raise ValueError("efficiency must be <=1 and reserve factor >=1")
    if any(not math.isfinite(loads[key])
           for key in ('Fx_surge_N', 'Fy_sway_N', 'Mz_yaw_Nm')):
        raise ValueError("loads must be finite")
    if yaw_capacity_Nm is not None and (
            not math.isfinite(yaw_capacity_Nm) or yaw_capacity_Nm < 0):
        raise ValueError("yaw capacity must be finite and nonnegative")
    required = math.hypot(loads['Fx_surge_N'], loads['Fy_sway_N']) * reserve_factor / efficiency
    moment = abs(loads['Mz_yaw_Nm'])
    yaw_ok = moment == 0 if yaw_capacity_Nm is None else moment <= yaw_capacity_Nm
    return {'required_bollard_pull_N': required, 'available_bollard_pull_N': available_pull_N,
            'required_yaw_capacity_Nm': moment, 'yaw_ok': yaw_ok,
            'go': required <= available_pull_N and yaw_ok}
