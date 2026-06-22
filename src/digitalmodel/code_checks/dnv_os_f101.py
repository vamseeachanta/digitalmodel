"""DNV-OS-F101 pipe local-buckling and external-pressure resistance checks.

Generic, standard-based screening calculations for a submarine pipeline per
DNV-OS-F101 (Submarine Pipeline Systems). Three methods are implemented:

1. **Local-buckling — load-controlled condition (LCC)** combined-loading
   utilisation for bending moment + effective tension + net internal/external
   pressure (the installation/operation feasibility check; UF < 1 passes).
2. **External-pressure collapse** characteristic resistance Pc — the real root
   of the DNV collapse cubic that blends elastic and plastic (yield) collapse
   with the initial ovality.
3. **Propagation-buckling** pressure Pprop and the propagation-arrest /
   initiation check.

All functions are unit-consistent: pass diameters/thicknesses, stresses and
pressures in the same unit system (e.g. mm / MPa / MPa, giving moments in
N.mm and forces in N when areas are mm^2). No internal unit conversion is done.

References
----------
- DNV-OS-F101 (2013), Sec.5 D — Local buckling:
    * Load-controlled (Sec.5 D404/D405) combined-loading criteria.
    * External-pressure collapse Pc (Sec.5 D401, the collapse cubic).
    * Propagation buckling Pprop (Sec.5 D501).
"""

from __future__ import annotations

import math
from dataclasses import dataclass

SQRT3 = math.sqrt(3.0)


# ---------------------------------------------------------------------------
# Plastic capacities (used by the load-controlled combined-loading check)
# ---------------------------------------------------------------------------


def plastic_axial_capacity(diameter: float, t: float, f_y: float) -> float:
    """Plastic axial-force capacity ``Sp = fy * pi * (D - t) * t`` [force]."""
    return f_y * math.pi * (diameter - t) * t


def plastic_moment_capacity(diameter: float, t: float, f_y: float) -> float:
    """Plastic moment capacity ``Mp = fy * (D - t)^2 * t`` [moment]."""
    return f_y * (diameter - t) ** 2 * t


def pressure_containment_resistance(diameter: float, t: float, f_y: float, f_u: float) -> float:
    """Burst (pressure-containment) resistance ``Pb = (2 t / (D - t)) * fcb * 2/sqrt(3)``.

    Uses ``fcb = min(fy, fu / 1.15)`` as the characteristic strength.
    """
    fcb = min(f_y, f_u / 1.15)
    return (2.0 * t / (diameter - t)) * fcb * (2.0 / SQRT3)


# ---------------------------------------------------------------------------
# 1. Local buckling — load-controlled condition (LCC) combined loading
# ---------------------------------------------------------------------------
#
# DNV-OS-F101 Sec.5 D combined-loading criterion. With characteristic plastic
# capacities Sp (axial) and Mp (moment) and pressure-containment Pb, the
# load-controlled utilisation for the two net-pressure regimes is:
#
# INTERNAL net overpressure (pi > pe):
#   UF = { gamma_m*gamma_SC*(|Md|/Mp) + [(gamma_m*gamma_SC*Sd)/Sp]^2 }^2
#        + [ gamma_m*gamma_SC*(pi - pe)/Pb ]^2
#
# EXTERNAL net overpressure (pe > pi):
#   UF = { gamma_m*gamma_SC*(|Md|/Mp) + [(gamma_m*gamma_SC*Sd)/Sp]^2 }^2
#        + [ gamma_m*gamma_SC*(pe - pmin)/Pc ]^2
#
# UF <= 1 passes (the bracketed "moment + tension" group is squared, the
# pressure group is added in quadrature). gamma_m and gamma_SC are the material
# and safety-class resistance factors; the load effects Md, Sd are factored
# design values supplied by the caller.


@dataclass(frozen=True)
class LocalBucklingResult:
    """Result of a DNV-OS-F101 local-buckling load-controlled check."""

    utilisation: float  # combined-loading utilisation factor (UF)
    passes: bool  # True if UF <= 1.0


def local_buckling_lcc(
    design_moment: float,
    design_effective_tension: float,
    pressure_internal: float,
    pressure_external: float,
    moment_capacity: float,
    axial_capacity: float,
    pressure_capacity: float,
    gamma_m: float = 1.15,
    gamma_sc: float = 1.14,
) -> LocalBucklingResult:
    """DNV-OS-F101 local-buckling, load-controlled combined-loading utilisation.

    Parameters
    ----------
    design_moment : float
        Design bending moment Md (factored) [moment].
    design_effective_tension : float
        Design effective (axial) tension Sd (factored) [force].
    pressure_internal : float
        Internal pressure pi [pressure].
    pressure_external : float
        External pressure pe [pressure]. For the external-pressure regime this
        is the (collapse) pressure acting; ``pressure_capacity`` should then be
        the collapse resistance Pc. For the internal regime it is the minimum
        internal pressure pmin / external pressure, and ``pressure_capacity``
        should be the burst resistance Pb.
    moment_capacity : float
        Plastic moment capacity Mp [moment].
    axial_capacity : float
        Plastic axial capacity Sp [force].
    pressure_capacity : float
        Pressure resistance — burst Pb for the internal regime, collapse Pc for
        the external regime [pressure].
    gamma_m : float, optional
        Material resistance factor. Default 1.15 (ULS).
    gamma_sc : float, optional
        Safety-class resistance factor. Default 1.14 (normal safety class).

    Returns
    -------
    LocalBucklingResult
        Combined-loading utilisation and pass/fail flag.
    """
    if moment_capacity <= 0.0 or axial_capacity <= 0.0 or pressure_capacity <= 0.0:
        raise ValueError("capacities must be positive")

    g = gamma_m * gamma_sc
    moment_term = g * abs(design_moment) / moment_capacity
    tension_term = (g * design_effective_tension / axial_capacity) ** 2
    group = (moment_term + tension_term) ** 2

    net = pressure_internal - pressure_external
    pressure_term = (g * abs(net) / pressure_capacity) ** 2

    uf = group + pressure_term
    return LocalBucklingResult(utilisation=uf, passes=uf <= 1.0)


# ---------------------------------------------------------------------------
# 2. External-pressure collapse (the DNV collapse cubic)
# ---------------------------------------------------------------------------
#
# Elastic collapse pressure (thin-shell):
#       Pel = 2 E (t/D)^3 / (1 - nu^2)
# Plastic (yield) collapse pressure:
#       Pp  = fy * alpha_fab * 2 (t/D)
# Characteristic collapse resistance Pc is the solution of the implicit cubic:
#       (Pc - Pel)(Pc^2 - Pp^2) = Pc * Pel * Pp * f0 * D / t
# with initial ovality f0 = (Dmax - Dmin)/D (>= 0.005 per code minimum).
# Pc is taken as the relevant (smallest positive) real root.


def elastic_collapse_pressure(
    diameter: float, t: float, youngs_modulus: float, poisson_ratio: float = 0.3
) -> float:
    """Elastic (thin-shell) collapse pressure ``Pel = 2 E (t/D)^3 / (1 - nu^2)``."""
    return 2.0 * youngs_modulus * (t / diameter) ** 3 / (1.0 - poisson_ratio**2)


def plastic_collapse_pressure(
    diameter: float, t: float, f_y: float, alpha_fab: float = 1.0
) -> float:
    """Plastic (yield) collapse pressure ``Pp = fy * alpha_fab * 2 (t/D)``.

    ``alpha_fab`` is the fabrication (manufacturing-process) factor (1.0 for
    seamless, < 1 for welded; supplied by the caller).
    """
    return f_y * alpha_fab * 2.0 * (t / diameter)


def collapse_pressure(
    diameter: float,
    t: float,
    f_y: float,
    youngs_modulus: float,
    ovality: float = 0.005,
    poisson_ratio: float = 0.3,
    alpha_fab: float = 1.0,
) -> float:
    """DNV-OS-F101 characteristic external-pressure collapse resistance Pc.

    Solves the collapse cubic

        (Pc - Pel)(Pc^2 - Pp^2) = Pc * Pel * Pp * f0 * (D / t)

    for the smallest positive real root (the governing collapse pressure).

    Parameters
    ----------
    diameter : float
        Outer diameter D [length].
    t : float
        Wall thickness (characteristic / t1) [length].
    f_y : float
        Yield strength [stress].
    youngs_modulus : float
        Young's modulus [stress].
    ovality : float, optional
        Initial ovality f0 = (Dmax - Dmin)/D. Default 0.005 (code minimum).
    poisson_ratio : float, optional
        Poisson's ratio. Default 0.3.
    alpha_fab : float, optional
        Fabrication factor. Default 1.0.

    Returns
    -------
    float
        Characteristic collapse resistance Pc [pressure].
    """
    if ovality < 0.005:
        ovality = 0.005  # DNV code minimum imperfection

    pel = elastic_collapse_pressure(diameter, t, youngs_modulus, poisson_ratio)
    pp = plastic_collapse_pressure(diameter, t, f_y, alpha_fab)

    # Expand to a monic cubic in Pc:
    #   Pc^3 - Pel*Pc^2 - (Pp^2 + Pp*Pel*f0*D/t)*Pc + Pel*Pp^2 = 0
    b = -pel
    c = -(pp**2 + pp * pel * ovality * diameter / t)
    d = pel * pp**2

    roots = _real_cubic_roots(1.0, b, c, d)
    positive = [r for r in roots if r > 0.0]
    if not positive:
        raise ValueError("no positive real root for the collapse cubic")
    return min(positive)


def _real_cubic_roots(a: float, b: float, c: float, d: float) -> list[float]:
    """Real roots of ``a x^3 + b x^2 + c x + d = 0`` (a != 0).

    Uses the depressed-cubic / trigonometric method; returns only real roots.
    """
    if a == 0.0:
        raise ValueError("not a cubic: a must be non-zero")
    # Normalise: x^3 + p2 x^2 + p1 x + p0
    p2 = b / a
    p1 = c / a
    p0 = d / a
    # Depress: x = y - p2/3 -> y^3 + p*y + q = 0
    shift = p2 / 3.0
    p = p1 - p2**2 / 3.0
    q = 2.0 * p2**3 / 27.0 - p2 * p1 / 3.0 + p0

    disc = (q / 2.0) ** 2 + (p / 3.0) ** 3
    roots: list[float] = []
    if disc > 0.0:
        # one real root
        sqrt_disc = math.sqrt(disc)
        u = math.cbrt(-q / 2.0 + sqrt_disc)
        v = math.cbrt(-q / 2.0 - sqrt_disc)
        roots.append(u + v - shift)
    else:
        # three real roots (trig method); handle p ~ 0 degenerate case
        if abs(p) < 1e-300:
            roots.append(math.cbrt(-q) - shift)
        else:
            r = math.sqrt(-(p**3) / 27.0)
            cos_arg = max(-1.0, min(1.0, -q / (2.0 * r)))
            phi = math.acos(cos_arg)
            m = 2.0 * math.sqrt(-p / 3.0)
            for k in range(3):
                roots.append(m * math.cos((phi + 2.0 * math.pi * k) / 3.0) - shift)
    return roots


# ---------------------------------------------------------------------------
# 3. Propagation buckling
# ---------------------------------------------------------------------------
#
# Once a local buckle forms, it can propagate along the line if the external
# overpressure exceeds the propagation pressure:
#       Ppr = 35 * fy * alpha_fab * (t/D)^2.5      (DNV-OS-F101 Sec.5 D501)
# Acceptance against propagation (no buckle arrestors): pe - pmin <= Ppr / gamma.


def propagation_pressure(
    diameter: float, t: float, f_y: float, alpha_fab: float = 1.0
) -> float:
    """Propagation-buckling pressure ``Ppr = 35 * fy * alpha_fab * (t/D)^2.5``.

    Parameters
    ----------
    diameter : float
        Outer diameter D [length].
    t : float
        Wall thickness [length].
    f_y : float
        Yield strength [stress].
    alpha_fab : float, optional
        Fabrication factor. Default 1.0.

    Returns
    -------
    float
        Propagation-buckling pressure [pressure].
    """
    return 35.0 * f_y * alpha_fab * (t / diameter) ** 2.5


@dataclass(frozen=True)
class PropagationCheck:
    """Result of a propagation-buckling acceptance check."""

    net_external_pressure: float  # pe - pmin [pressure]
    propagation_resistance: float  # Ppr / gamma [pressure]
    utilisation: float  # net / resistance [-]
    propagates: bool  # True if a buckle would propagate (utilisation > 1)


def propagation_check(
    net_external_pressure: float,
    diameter: float,
    t: float,
    f_y: float,
    alpha_fab: float = 1.0,
    gamma_m: float = 1.15,
    gamma_sc: float = 1.14,
) -> PropagationCheck:
    """Check net external overpressure against the propagation resistance.

    A buckle propagates if ``(pe - pmin) > Ppr / (gamma_m * gamma_SC)``. When it
    would propagate, buckle arrestors (or a thicker wall) are required.

    Parameters
    ----------
    net_external_pressure : float
        Net external overpressure pe - pmin [pressure].
    diameter, t, f_y : float
        Outer diameter, wall thickness and yield strength.
    alpha_fab : float, optional
        Fabrication factor. Default 1.0.
    gamma_m, gamma_sc : float, optional
        Material and safety-class resistance factors. Defaults 1.15 / 1.14.

    Returns
    -------
    PropagationCheck
        Net pressure, factored resistance, utilisation and propagate flag.
    """
    ppr = propagation_pressure(diameter, t, f_y, alpha_fab)
    resistance = ppr / (gamma_m * gamma_sc)
    if resistance <= 0.0:
        raise ValueError("propagation resistance must be positive")
    util = net_external_pressure / resistance
    return PropagationCheck(
        net_external_pressure=net_external_pressure,
        propagation_resistance=resistance,
        utilisation=util,
        propagates=util > 1.0,
    )
