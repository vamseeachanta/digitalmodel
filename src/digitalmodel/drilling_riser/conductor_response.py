"""Wellhead/conductor bending moment — analytical screening tier (#1345, C2).

The drilling riser terminates at the lower flex-joint, which sits atop the
BOP/LMRP stack a stand-off height ``h_stack`` ABOVE the mudline wellhead datum.
The flex-joint transmits the riser's lateral shear ``H`` (and tension) but ~no
moment; that shear acting over the rigid stand-off arm applies an end moment
``M0 = H * h_stack`` at the top of the conductor. The conductor below the
mudline is modelled as a semi-infinite beam on an elastic (Winkler) foundation
loaded at its top by ``H`` and ``M0``:

    EI_c y'''' + k y = 0,     b = (k / (4 EI_c))^(1/4)
    M(x) = M0 e^{-bx}(cos bx + sin bx) - (H/b) e^{-bx} sin bx

with ``x`` the depth below mudline. The bending moment peaks at the mudline
(x=0, value M0) and decays over ~1/b. The stand-off moment M0 dominates the
pure soil-reaction peak (~0.32 H/b), so it is the governing static term for the
wellhead moment — omitting it understates the moment ~10x (non-conservative).

Provenance: standard beam-on-elastic-foundation theory (Hetényi, *Beams on
Elastic Foundation*, 1946; public), NOT any licensed project material. This is a
STATIC screening model: a single Winkler modulus ``k`` (not a layered p-y
curve) and NO conductor axial load (a real conductor carries axial load adding a
beam-column term); layered p-y + dynamics are the solver tier (#1281c). SI.
"""
from __future__ import annotations

import math
from dataclasses import dataclass

import numpy as np


@dataclass(frozen=True)
class ConductorResponse:
    """Result of :func:`solve_conductor_moment` (SI). ``x_m`` is depth below the
    mudline; ``moment_nm`` is M(x); ``stand_off_moment_nm`` is M0 = H*h_stack."""

    x_m: np.ndarray
    moment_nm: np.ndarray
    beta_per_m: float
    stand_off_moment_nm: float

    @property
    def max_moment_nm(self) -> float:
        return float(np.max(np.abs(self.moment_nm)))


def solve_conductor_moment(
    *,
    shear_n: float,
    stand_off_m: float,
    soil_modulus_n_per_m2: float,
    ei_nm2: float,
    n_char_lengths: float = 8.0,
    n_nodes: int = 201,
) -> ConductorResponse:
    """Wellhead/conductor bending moment via a Hetényi beam-on-elastic-foundation.

    Parameters
    ----------
    shear_n : float
        Lateral shear H transmitted from the riser lower flex-joint [N] (>= 0).
    stand_off_m : float
        BOP/LMRP stand-off height h_stack from mudline to the flex-joint [m].
    soil_modulus_n_per_m2 : float
        Winkler subgrade modulus k (force / length / deflection) [N/m^2], > 0.
    ei_nm2 : float
        Conductor bending stiffness EI_c [N.m^2], > 0.
    n_char_lengths : float
        Depth of the grid in characteristic lengths 1/b (>= a few for decay).
    n_nodes : int
        Number of depth nodes (>= 2).
    """
    if soil_modulus_n_per_m2 <= 0 or ei_nm2 <= 0:
        raise ValueError("soil_modulus_n_per_m2 and ei_nm2 must be positive")
    if shear_n < 0 or stand_off_m < 0:
        raise ValueError("shear_n and stand_off_m must be non-negative")

    beta = (soil_modulus_n_per_m2 / (4.0 * ei_nm2)) ** 0.25
    m0 = float(shear_n) * float(stand_off_m)

    x = np.linspace(0.0, n_char_lengths / beta, n_nodes)
    e = np.exp(-beta * x)
    c = np.cos(beta * x)
    s = np.sin(beta * x)
    moment = m0 * e * (c + s) - (float(shear_n) / beta) * e * s

    return ConductorResponse(
        x_m=x,
        moment_nm=moment,
        beta_per_m=float(beta),
        stand_off_moment_nm=m0,
    )
