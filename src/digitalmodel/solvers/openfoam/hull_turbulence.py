#!/usr/bin/env python3
"""
ABOUTME: Inlet turbulence for an arbitrary hull (#2023), plus the near-wall
requirement the mesher cannot see.

``0.orig/{k,omega,nut}`` in the frozen template carry one benchmark hull's
values at that hull's condition. They are not wrong there and they are not
transferable: k follows the square of the tow speed, and omega follows the
viscosity, which is itself set by the Reynolds number the case is matched to.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum
from typing import Dict

__all__ = [
    "C_MU",
    "DEFAULT_LENGTH_SCALE_FACTOR",
    "DEFAULT_TURBULENCE_INTENSITY",
    "DEFAULT_VISCOSITY_RATIO",
    "InletTurbulence",
    "TurbulenceMethod",
    "derive_inlet_turbulence",
    "wall_normal_first_cell_height",
]

#: k-omega closure constant.
C_MU = 0.09


# --------------------------------------------------------------------------- #
#  Inlet turbulence
# --------------------------------------------------------------------------- #

class TurbulenceMethod(str, Enum):
    """How omega is closed once k is set from the intensity."""

    #: omega = k / (R_nu nu). Controls the freestream eddy viscosity directly.
    VISCOSITY_RATIO = "viscosity_ratio"
    #: omega = sqrt(k) / (C_mu^{1/4} l), l = factor * Lpp.
    LENGTH_SCALE = "length_scale"


#: Turbulence intensity at the inlet. A towing tank or an open-water inlet is
#: near-quiescent; 1% is the conventional stand-in when nothing was measured.
DEFAULT_TURBULENCE_INTENSITY = 0.01

#: nut_inf / nu. Small enough that the freestream carries no meaningful eddy
#: viscosity to the hull, large enough to seed the model.
DEFAULT_VISCOSITY_RATIO = 0.1

#: l / Lpp for the LENGTH_SCALE method.
DEFAULT_LENGTH_SCALE_FACTOR = 0.07


@dataclass(frozen=True)
class InletTurbulence:
    """Inlet k, omega and nut, and the assumptions that produced them."""

    k: float
    omega: float
    nut: float
    intensity: float
    method: TurbulenceMethod
    length_scale: float
    viscosity_ratio: float

    def to_provenance(self) -> Dict[str, object]:
        return {
            "k": self.k,
            "omega": self.omega,
            "nut": self.nut,
            "intensity": self.intensity,
            "method": self.method.value,
            "length_scale_m": self.length_scale,
            "eddy_viscosity_ratio": self.viscosity_ratio,
            "formulae": {
                "k": "1.5 * (U * I)^2",
                "omega_viscosity_ratio": "k / (R_nu * nu)",
                "omega_length_scale": "sqrt(k) / (C_mu^0.25 * l), l = f * Lpp",
                "nut": "k / omega",
            },
        }


def derive_inlet_turbulence(
    velocity: float,
    lpp: float,
    nu: float,
    *,
    intensity: float = DEFAULT_TURBULENCE_INTENSITY,
    method: TurbulenceMethod = TurbulenceMethod.VISCOSITY_RATIO,
    viscosity_ratio: float = DEFAULT_VISCOSITY_RATIO,
    length_scale_factor: float = DEFAULT_LENGTH_SCALE_FACTOR,
) -> InletTurbulence:
    """Inlet turbulence for kOmegaSST, from the tow condition.

    ``k = 3/2 (U I)^2`` is the isotropic definition and is not in question.
    Closing omega is, and the two routes differ by orders of magnitude at
    model-scale Reynolds numbers:

    * VISCOSITY_RATIO (default) sets ``nut_inf = R_nu nu`` and back-solves
      ``omega = k / nut_inf``. This is standard external-flow practice and it
      controls the quantity that damages the answer.
    * LENGTH_SCALE takes ``l = f Lpp``. At a model-scale Reynolds number any
      macroscopic length scale puts nut_inf two to four orders of magnitude
      above molecular. That turbulence convects 1.5 Lpp to the hull, thickens
      the boundary layer, and lands on the viscous coefficient -- which is
      exactly the quantity the KCS validation gates. Offered because it is the
      textbook form, not defaulted to.

    Whichever route set omega, ``nut = k / omega`` holds, and the implied
    length scale is reported so a reviewer can sanity-check either way.
    """
    if velocity <= 0:
        raise ValueError(f"velocity must be positive, got {velocity}")
    if not 0.0 < intensity < 1.0:
        raise ValueError(f"turbulence intensity must be in (0, 1), got {intensity}")
    if lpp <= 0:
        raise ValueError(f"lpp must be positive, got {lpp}")
    if nu <= 0:
        raise ValueError(f"nu must be positive, got {nu}")

    k = 1.5 * (velocity * intensity) ** 2

    if method is TurbulenceMethod.VISCOSITY_RATIO:
        if viscosity_ratio <= 0:
            raise ValueError(f"viscosity_ratio must be positive, got {viscosity_ratio}")
        nut = viscosity_ratio * nu
        omega = k / nut
        length_scale = math.sqrt(k) / (C_MU**0.25 * omega)
    else:
        if length_scale_factor <= 0:
            raise ValueError(
                f"length_scale_factor must be positive, got {length_scale_factor}"
            )
        length_scale = length_scale_factor * lpp
        omega = math.sqrt(k) / (C_MU**0.25 * length_scale)
        nut = k / omega
        viscosity_ratio = nut / nu

    return InletTurbulence(
        k=k,
        omega=omega,
        nut=nut,
        intensity=intensity,
        method=method,
        length_scale=length_scale,
        viscosity_ratio=viscosity_ratio,
    )


def wall_normal_first_cell_height(
    velocity: float, lpp: float, nu: float, y_plus: float = 50.0
) -> float:
    """First-cell height implied by a y+ target, via the ITTC-57 line.

    This is the requirement snappyHexMesh cannot see. Its settings are
    scale-invariant by construction -- ``relativeSizes true``, hull level
    (0 0), refinement driven by length-scaled topoSet boxes -- so a hull ten
    times larger gets a geometrically similar mesh. But it is then at ten times
    the Reynolds number, where the wall layer that mesh has to resolve is
    RELATIVELY thinner. Recorded in the case provenance so the gap is visible
    rather than assumed away.
    """
    if velocity <= 0 or lpp <= 0 or nu <= 0 or y_plus <= 0:
        raise ValueError("velocity, lpp, nu and y_plus must all be positive")
    reynolds = velocity * lpp / nu
    denom = math.log10(reynolds) - 2.0
    if denom <= 0:
        raise ValueError(f"ITTC-57 line is not usable at Re = {reynolds:.4g}")
    cf = 0.075 / denom**2
    u_tau = velocity * math.sqrt(cf / 2.0)
    return y_plus * nu / u_tau
