"""Static tensioned beam-column riser response — analytical screening tier (#1281a).

Solves the tensioned beam-column boundary-value problem

    EI y'''' - T y'' = w        (SI: y[m], z[m], T[N], EI[N.m^2], w[N/m])

for a top-tensioned drilling riser under a static vessel offset and a uniform
current-drag load, yielding the deflected shape, the flex-joint angles (slopes
at the ends) and the bending-moment distribution ``M(z) = EI y''(z)``.

``z`` runs 0 (lower flex-joint, at the wellhead/BOP) to L (upper flex-joint, at
the vessel). Flex-joints transmit ~no moment, so the boundary conditions are
pinned (``M=0``) at both ends with ``y(0)=0`` and ``y(L)=offset`` (the vessel
watch-circle radius).

This is a STATIC SCREENING model: constant effective tension, uniform current
load, no dynamic amplification / VIV / vessel-motion dynamics — those require
the solver tier (#1281c). Provenance: standard tensioned-beam / beam-column
marine-riser mechanics (e.g. Sparks, *Fundamentals of Marine Riser Mechanics*),
NOT any licensed project material.

Closed-form solution (stable exponential form, avoiding the cosh/sinh
cancellation that blows up at the large ``kL`` of a real riser):

    k = sqrt(T/EI),  C = EI w / T^2,  eps = exp(-kL),  P = Q = C/(1+eps)
    y(z) = -C + B z - w z^2/(2T) + P e^{-kz} + Q e^{-k(L-z)},  B = X/L + wL/(2T)
    M(z) = -EI w/T + T P e^{-kz} + T Q e^{-k(L-z)}

Sanity: pure offset (w=0) -> rigid tilt y=Xz/L, both angles X/L, zero moment;
pure current -> lower-joint angle ~ wL/(2T) with a small -w/(kT) beam
correction, and a nonzero interior moment.
"""
from __future__ import annotations

import math
from dataclasses import dataclass

import numpy as np


@dataclass(frozen=True)
class RiserResponse:
    """Result of :func:`solve_static_response` (SI units).

    ``angle_*`` are the flex-joint rotations (slopes) at the ends, radians,
    signed; ``bending_moment_nm`` is ``M(z)`` along ``z_m``.
    """

    z_m: np.ndarray
    deflection_m: np.ndarray
    bending_moment_nm: np.ndarray
    angle_lower_rad: float
    angle_upper_rad: float
    #: Lateral shear (transverse reaction) at the lower flex-joint, V(0) magnitude
    #: [N]. A pinned end carries zero moment but non-zero shear; this is the
    #: lateral load handed to the conductor model (#1345). Equals T*X/L + w*L/2.
    shear_lower_n: float = 0.0

    @property
    def angle_lower_deg(self) -> float:
        return math.degrees(self.angle_lower_rad)

    @property
    def angle_upper_deg(self) -> float:
        return math.degrees(self.angle_upper_rad)

    @property
    def max_bending_moment_nm(self) -> float:
        return float(np.max(np.abs(self.bending_moment_nm)))


def solve_static_response(
    *,
    length_m: float,
    top_offset_m: float,
    tension_n: float,
    ei_nm2: float,
    current_load_n_per_m: float,
    n_nodes: int = 201,
) -> RiserResponse:
    """Solve the static tensioned beam-column BVP; see module docstring.

    Parameters
    ----------
    length_m : float
        Riser length between flex-joints L [m].
    top_offset_m : float
        Vessel offset at the upper flex-joint X [m] (e.g. watch-circle radius).
    tension_n : float
        Effective (constant, screening) tension T [N], must be > 0.
    ei_nm2 : float
        Bending stiffness EI [N.m^2], must be > 0.
    current_load_n_per_m : float
        Uniform lateral current-drag load w [N/m] (w = 1/2 rho Cd D U^2).
    n_nodes : int
        Number of z-grid nodes (>= 3).
    """
    if length_m <= 0 or tension_n <= 0 or ei_nm2 <= 0:
        raise ValueError("length_m, tension_n, ei_nm2 must all be positive")
    if n_nodes < 3:
        raise ValueError("n_nodes must be >= 3")

    L = float(length_m)
    X = float(top_offset_m)
    T = float(tension_n)
    EI = float(ei_nm2)
    w = float(current_load_n_per_m)

    k = math.sqrt(T / EI)
    C = EI * w / (T * T)
    eps = math.exp(-k * L)  # tiny for a real riser; kept for correctness
    P = C / (1.0 + eps)
    Q = P
    B = X / L + w * L / (2.0 * T)

    z = np.linspace(0.0, L, n_nodes)
    e_lo = np.exp(-k * z)          # boundary layer at z=0
    e_hi = np.exp(-k * (L - z))    # boundary layer at z=L

    y = -C + B * z - w * z * z / (2.0 * T) + P * e_lo + Q * e_hi
    moment = -EI * w / T + T * P * e_lo + T * Q * e_hi

    # slopes at the ends (analytic derivative of y)
    # y'(z) = B - w z/T - P k e^{-kz} + Q k e^{-k(L-z)}
    angle_lower = B - 0.0 - P * k * 1.0 + Q * k * eps
    angle_upper = B - w * L / T - P * k * eps + Q * k * 1.0

    # Lateral shear at the lower flex-joint: V(0) = EI y'''(0) - T y'(0); the
    # boundary-layer terms cancel (EI k^3 = T k), leaving V(0) = -T*B, so the
    # magnitude is T*X/L + w*L/2 (rises with offset X and with current via w).
    shear_lower = abs(T * B)

    return RiserResponse(
        z_m=z,
        deflection_m=y,
        bending_moment_nm=moment,
        angle_lower_rad=float(angle_lower),
        angle_upper_rad=float(angle_upper),
        shear_lower_n=float(shear_lower),
    )
