"""Chemical-injection delivery through an umbilical tube (API 17E / ISO 13628-5).

API 17E (ISO 13628-5) umbilicals carry chemical-injection fluids (methanol,
corrosion / scale / hydrate inhibitors) from a topside hydraulic-power /
chemical-injection unit down to the subsea trees through small-bore
thermoplastic or steel tubes.  The governing deterministic engineering
question is hydraulic: *given an available topside delivery pressure, can a
tube of a given bore and length deliver the required dosage flow rate at the
subsea injection point?*

This module encodes that bounded, pure-function hydraulics core (steady,
incompressible, single-phase Newtonian flow), which is the testable public
standard fluid mechanics used in umbilical line-sizing:

Equations
---------
Volumetric flow ``Q`` [m^3/s] from a dosage demand.  For a per-well injection
rate the dosage flow is simply the specified ``Q``.

Mean velocity in a circular bore of inner diameter ``d`` [m]:

    A = pi/4 * d^2                                                  (1)
    v = Q / A                                                       (2)

Reynolds number for fluid density ``rho`` [kg/m^3] and dynamic viscosity
``mu`` [Pa.s]:

    Re = rho * v * d / mu                                           (3)

Darcy friction factor ``f``.  Laminar (``Re <= 2300``):

    f = 64 / Re                                                     (4a)

Turbulent (``Re > 4000``) smooth-tube Blasius correlation:

    f = 0.3164 * Re^(-0.25)                                         (4b)

In the transitional band ``2300 < Re <= 4000`` the laminar value is held
(conservative for pressure drop; flagged in the result).

Frictional pressure drop over tube length ``L`` (Darcy-Weisbach):

    dP_friction = f * (L / d) * (rho * v^2 / 2)                     (5)

Static (hydrostatic) head over a vertical drop ``H`` (positive = injection
point below the topside outlet; this *assists* delivery, so it is added to the
available driving pressure):

    dP_static = rho * g * H                                         (6)

Required topside delivery pressure to push ``Q`` and meet the subsea injection
back-pressure ``P_inj`` at the tree:

    P_required = P_inj + dP_friction - dP_static                    (7)

Delivery is feasible when the available topside pressure ``P_available``
satisfies ``P_available >= P_required`` (utilisation ``U = P_required /
P_available <= 1``).

References
----------
API 17E / ISO 13628-5 — Subsea umbilicals.
White, *Fluid Mechanics* — Darcy-Weisbach, Blasius smooth-tube correlation.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

GRAVITY = 9.81  # m/s^2
LAMINAR_RE = 2300.0  # upper bound of laminar regime
TURBULENT_RE = 4000.0  # lower bound of fully turbulent regime


@dataclass(frozen=True)
class ChemicalFluid:
    """Newtonian chemical-injection fluid (SI units).

    Parameters
    ----------
    density : float
        Fluid density rho [kg/m^3].
    viscosity : float
        Dynamic viscosity mu [Pa.s].
    """

    density: float
    viscosity: float

    def __post_init__(self) -> None:
        if self.density <= 0:
            raise ValueError("density must be > 0")
        if self.viscosity <= 0:
            raise ValueError("viscosity must be > 0")


@dataclass(frozen=True)
class UmbilicalTube:
    """Chemical-injection / hydraulic tube within an umbilical (SI units).

    Parameters
    ----------
    inner_diameter : float
        Tube bore inner diameter d [m].
    length : float
        Developed tube length from topside outlet to subsea injection
        point L [m] (route length, not water depth).
    """

    inner_diameter: float
    length: float

    def __post_init__(self) -> None:
        if self.inner_diameter <= 0:
            raise ValueError("inner_diameter must be > 0")
        if self.length <= 0:
            raise ValueError("length must be > 0")

    @property
    def bore_area(self) -> float:
        """Bore cross-sectional area A = pi/4 d^2 [m^2], eq. (1)."""
        return math.pi / 4.0 * self.inner_diameter**2


def mean_velocity(flow_rate: float, tube: UmbilicalTube) -> float:
    """Mean bore velocity v = Q / A [m/s], eq. (2)."""
    if flow_rate < 0:
        raise ValueError("flow_rate must be >= 0")
    return flow_rate / tube.bore_area


def reynolds_number(flow_rate: float, tube: UmbilicalTube, fluid: ChemicalFluid) -> float:
    """Reynolds number Re = rho v d / mu, eq. (3)."""
    v = mean_velocity(flow_rate, tube)
    return fluid.density * v * tube.inner_diameter / fluid.viscosity


def friction_factor(reynolds: float) -> float:
    """Darcy friction factor f, eqs. (4a)/(4b).

    Laminar ``f = 64/Re``; turbulent (Re > 4000) Blasius smooth-tube
    ``f = 0.3164 Re^-0.25``.  In the transitional band the laminar value is
    held (conservative for pressure drop).
    """
    if reynolds <= 0:
        raise ValueError("reynolds must be > 0")
    if reynolds <= TURBULENT_RE:
        return 64.0 / reynolds
    return 0.3164 * reynolds**-0.25


def friction_pressure_drop(
    flow_rate: float, tube: UmbilicalTube, fluid: ChemicalFluid
) -> float:
    """Darcy-Weisbach frictional pressure drop dP_friction [Pa], eq. (5)."""
    re = reynolds_number(flow_rate, tube, fluid)
    f = friction_factor(re)
    v = mean_velocity(flow_rate, tube)
    return f * (tube.length / tube.inner_diameter) * (fluid.density * v**2 / 2.0)


def static_pressure(fluid: ChemicalFluid, vertical_drop: float) -> float:
    """Hydrostatic head dP_static = rho g H [Pa], eq. (6).

    ``vertical_drop`` H > 0 when the subsea injection point is below the
    topside outlet (assists delivery).
    """
    return fluid.density * GRAVITY * vertical_drop


@dataclass(frozen=True)
class DeliveryResult:
    """Result of an umbilical chemical-delivery feasibility check."""

    flow_rate: float  # Q [m^3/s]
    velocity: float  # v [m/s]
    reynolds: float  # Re [-]
    flow_regime: str  # "laminar" | "transitional" | "turbulent"
    friction_factor: float  # f [-]
    friction_drop: float  # dP_friction [Pa]
    static_pressure: float  # dP_static [Pa]
    required_pressure: float  # P_required [Pa], eq. (7)
    available_pressure: float  # P_available [Pa]
    utilisation: float  # U = P_required / P_available [-]

    @property
    def is_feasible(self) -> bool:
        """True when available pressure can deliver the dosage (U <= 1)."""
        return self.utilisation <= 1.0


def _flow_regime(reynolds: float) -> str:
    if reynolds <= LAMINAR_RE:
        return "laminar"
    if reynolds <= TURBULENT_RE:
        return "transitional"
    return "turbulent"


def check_delivery(
    flow_rate: float,
    tube: UmbilicalTube,
    fluid: ChemicalFluid,
    available_pressure: float,
    injection_pressure: float = 0.0,
    vertical_drop: float = 0.0,
) -> DeliveryResult:
    """Assess whether ``flow_rate`` can be delivered through ``tube``.

    Parameters
    ----------
    flow_rate : float
        Required dosage delivery rate Q [m^3/s].
    tube : UmbilicalTube
        Umbilical chemical/hydraulic tube.
    fluid : ChemicalFluid
        Injection fluid properties.
    available_pressure : float
        Topside delivery pressure P_available [Pa].
    injection_pressure : float, optional
        Subsea injection back-pressure at the tree P_inj [Pa].
    vertical_drop : float, optional
        Elevation drop H [m] from topside outlet to injection point
        (positive = injection point below outlet, assists flow).

    Returns
    -------
    DeliveryResult
        Velocity, regime, friction factor, pressure components, required
        pressure (eq. 7) and utilisation.
    """
    if available_pressure <= 0:
        raise ValueError("available_pressure must be > 0")

    v = mean_velocity(flow_rate, tube)
    re = reynolds_number(flow_rate, tube, fluid)
    f = friction_factor(re)
    dp_friction = friction_pressure_drop(flow_rate, tube, fluid)
    dp_static = static_pressure(fluid, vertical_drop)
    p_required = injection_pressure + dp_friction - dp_static
    utilisation = p_required / available_pressure

    return DeliveryResult(
        flow_rate=flow_rate,
        velocity=v,
        reynolds=re,
        flow_regime=_flow_regime(re),
        friction_factor=f,
        friction_drop=dp_friction,
        static_pressure=dp_static,
        required_pressure=p_required,
        available_pressure=available_pressure,
        utilisation=utilisation,
    )
