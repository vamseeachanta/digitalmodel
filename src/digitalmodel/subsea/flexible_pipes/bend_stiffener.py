"""Bend-stiffener minimum-bend-radius screening (API 17B / API 17L).

A bend stiffener is a tapered polymer (typically polyurethane) moulded cone
fitted at the top connection of a dynamic flexible riser (and at other
connectors) to limit the local curvature of the pipe under combined tension
and angular deflection, keeping the bend radius above the pipe's minimum bend
radius (MBR).

This module gives the *deterministic* curvature-limiting core: for a pipe of
bending stiffness ``EI`` carried at effective top tension ``T`` and deflected
through an angle ``theta`` at the stiffener tip, a first-order
beam-on-tension (cable-with-stiffness) estimate of the minimum radius of
curvature developed by the bare pipe at the termination is

    R_min = (1 / theta) * sqrt(EI / T)                             (1)

This is the classic characteristic length ``lambda = sqrt(EI/T)`` of a
tensioned beam scaled by the imposed end rotation.  A bend stiffener works by
locally increasing the effective bending stiffness ``EI_eff`` (pipe + cone)
so that the developed radius ``R_min`` rises above the MBR.  The required
effective stiffness to just achieve a target radius ``R_target`` is, from (1),

    EI_required = T * (theta * R_target)^2                         (2)

The curvature utilisation against the qualified MBR is

    U_curv = MBR / R_min                                           (3)

A configuration PASSES the screening when ``R_min >= MBR`` (``U_curv <= 1``).

Assumptions / notes
-------------------
* Equation (1) is a screening estimate; detailed bend-stiffener design solves
  the nonlinear tapered beam-on-elastic-foundation problem (API 17L / 17B)
  with the moment-curvature law of the polymer cone.  This module sizes the
  *required effective EI* and checks the bare-pipe radius, flagging the full
  taper design as follow-on work.
* ``theta`` is the relative angle [rad] between the pipe at the stiffener tip
  and the termination axis (e.g. vessel offset + wave-induced rotation).
* Tension ``T`` is the effective top tension at the termination [N].

References
----------
* API 17B, Recommended Practice for Flexible Pipe.
* API 17L1 / 17L2, Flexible Pipe Ancillary Equipment (bend stiffeners).
"""

from __future__ import annotations

import math
from dataclasses import dataclass


@dataclass(frozen=True)
class BendStiffenerResult:
    """Result of a bend-stiffener MBR screening."""

    r_min: float               # developed minimum radius of bare pipe [m] (eq.1)
    ei_required: float         # effective EI to reach the MBR target [N.m^2] (eq.2)
    u_curvature: float         # eq. 3
    passes: bool               # R_min >= MBR


def developed_min_radius(
    bending_stiffness: float,
    effective_tension: float,
    deflection_angle: float,
) -> float:
    """Minimum radius developed by a tensioned beam at an end rotation [m].

    Equation (1).

    Parameters
    ----------
    bending_stiffness : float
        Effective bending stiffness EI [N.m^2].
    effective_tension : float
        Effective top tension T [N] (> 0).
    deflection_angle : float
        Imposed end rotation theta [rad] (> 0).
    """
    if bending_stiffness <= 0:
        raise ValueError("bending_stiffness must be positive")
    if effective_tension <= 0:
        raise ValueError("effective_tension must be positive")
    if deflection_angle <= 0:
        raise ValueError("deflection_angle must be positive")
    return math.sqrt(bending_stiffness / effective_tension) / deflection_angle


def required_effective_ei(
    target_radius: float,
    effective_tension: float,
    deflection_angle: float,
) -> float:
    """Effective EI needed for the bare-pipe radius to reach ``target_radius``.

    Equation (2).
    """
    if target_radius <= 0:
        raise ValueError("target_radius must be positive")
    if effective_tension <= 0:
        raise ValueError("effective_tension must be positive")
    if deflection_angle <= 0:
        raise ValueError("deflection_angle must be positive")
    return effective_tension * (deflection_angle * target_radius) ** 2


def screen_bend_stiffener(
    bending_stiffness: float,
    effective_tension: float,
    deflection_angle: float,
    minimum_bend_radius: float,
) -> BendStiffenerResult:
    """Screen whether the section satisfies the MBR at the termination.

    Computes the developed radius (eq. 1), the effective EI required to reach
    the MBR (eq. 2) and the curvature utilisation (eq. 3).  Passes when the
    developed radius is at least the MBR.
    """
    if minimum_bend_radius <= 0:
        raise ValueError("minimum_bend_radius must be positive")
    r_min = developed_min_radius(bending_stiffness, effective_tension, deflection_angle)
    ei_req = required_effective_ei(minimum_bend_radius, effective_tension, deflection_angle)
    u_curv = minimum_bend_radius / r_min
    return BendStiffenerResult(
        r_min=r_min,
        ei_required=ei_req,
        u_curvature=u_curv,
        passes=r_min >= minimum_bend_radius,
    )
