"""Steady restriction loss from continuity and a measured loss coefficient.

DOE-HDBK-1012/3-92, Fluid Flow, 'Continuity Equation' and 'Head Loss',
gives Q=vA and minor loss K*v**2/(2g). Multiplication by rho*g yields
dp=K*rho*v**2/2. The moving-boundary continuity balance gives
vb=(Q-vt*Ac)/Ab. Effective driving area Ac is the task's piston screen
assumption, not Ap and not a vendor-calibrated plug-force law.
K=2.0 (representative bracket 1.5-2.5) is a task assumption. Calibrate K
against measured differential pressure at the same ring and fluid conditions.
"""

from dataclasses import dataclass

from digitalmodel.units import Q_

from ._validation import scalar
from .geometry import BypassGeometry


@dataclass(frozen=True)
class RestrictionResult:
    """Bypass velocity [m/s], differential pressure [Pa], driving force [N]."""

    bypass_velocity: Q_
    pressure_drop: Q_
    driving_force: Q_


def restriction(
    rate, tool_speed, geometry: BypassGeometry, density, k=2.0
) -> RestrictionResult:
    """Evaluate forward bypass loss; quantities or bare SI inputs.

    Equations and representative K basis are cited in the module docstring.
    Reverse flow (tool faster than Q/Ac) is outside this pump-down branch.
    """
    q = scalar(rate, "m**3/s", "rate")
    speed = scalar(tool_speed, "m/s", "tool speed")
    rho = scalar(density, "kg/m**3", "density", positive=True)
    loss = scalar(k, "dimensionless", "K", positive=True)
    ac = geometry.casing_area.to("m**2").magnitude
    ab = geometry.bypass_area.to("m**2").magnitude
    relative_flow = q - speed * ac
    if relative_flow < -1e-14 * max(q, 1e-12):
        raise ValueError("tool speed implies reverse bypass flow")
    vb = max(0.0, relative_flow) / ab
    dp = loss * rho * vb**2 / 2
    return RestrictionResult(Q_(vb, "m/s"), Q_(dp, "Pa"), Q_(dp * ac, "N"))


def calibrate_k(measured_dp, density, bypass_velocity) -> float:
    """Return K=2*dp/(rho*vb**2); positive measured loss and forward flow.

    Inversion of the DOE minor-loss equation cited above; no clipping to the
    representative 1.5-2.5 bracket is applied to measured calibration results.
    """
    dp = scalar(measured_dp, "Pa", "measured differential pressure", positive=True)
    rho = scalar(density, "kg/m**3", "density", positive=True)
    velocity = scalar(bypass_velocity, "m/s", "bypass velocity", positive=True)
    return 2 * dp / (rho * velocity**2)
