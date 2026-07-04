"""Lumped SDOF dynamic top-tension screen (motion_forecast, #1359 fast-follow).

Upgrades #1359's kinematic ``top_connection_vertical_velocity`` proxy to a
force-based screen. The deployed payload (ROV/TMS or a lifted module) is modelled
as a single mass on the lift-line axial stiffness, **base-excited** by the
deployment-point vertical motion (the standard DNV-RP-N103 splash-zone lift
screen)::

    m*x'' + c*(x' - z_top') + k*(x - z_top) = 0       (deviations from equilibrium)
    T(t) = static_tension + k*(z_top - x) + c*(z_top' - x')

with ``m = payload + added mass``, ``k = EA/L``, ``c = 2*zeta*sqrt(k*m)``.

HONESTY BOUNDARY: this is a lumped SDOF screen. It flags **slack onset**
(``T <= 0``) but does NOT resolve the nonlinear snatch impact load, and is NOT a
distributed riser/line dynamics model. Past first slack the linear model is
invalid, so ``daf`` is flagged not-trustworthy. ``zeta`` linearizes the (really
quadratic) hydrodynamic drag — acceptable for a screen.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from .reconstruct import time_derivative, vertical_motion_at


@dataclass(frozen=True)
class LumpedLine:
    """Lumped lift-line + payload for the SDOF tension screen.

    Parameters
    ----------
    payload_mass:
        Deployed dry/structural mass [kg], > 0.
    added_mass:
        Hydrodynamic added mass [kg], >= 0.
    ea:
        Line axial stiffness product E*A [N], > 0.
    length:
        Deployed line length [m], > 0 (``k = ea/length``).
    damping_ratio:
        Linearized damping ratio ``zeta`` in (0, 1).
    static_tension:
        Still-water tension the line carries (submerged weight) [N], > 0.
    """

    payload_mass: float
    added_mass: float
    ea: float
    length: float
    damping_ratio: float
    static_tension: float

    def __post_init__(self):
        if self.payload_mass <= 0 or self.ea <= 0 or self.length <= 0:
            raise ValueError("payload_mass, ea, length must be > 0")
        if self.added_mass < 0:
            raise ValueError("added_mass must be >= 0")
        if self.static_tension <= 0:
            raise ValueError("static_tension must be > 0")
        if not (0.0 < self.damping_ratio < 1.0):
            raise ValueError("damping_ratio must be in (0, 1) (over-damped excluded)")

    @property
    def m(self) -> float:
        return self.payload_mass + self.added_mass

    @property
    def k(self) -> float:
        return self.ea / self.length

    @property
    def c(self) -> float:
        return 2.0 * self.damping_ratio * np.sqrt(self.k * self.m)

    @property
    def wn(self) -> float:
        return float(np.sqrt(self.k / self.m))


def displacement_transmissibility(omega: float, line: LumpedLine) -> float:
    """|X/Z_top| — payload displacement per unit base displacement (base excitation)."""
    k, m, c = line.k, line.m, line.c
    num = complex(k, omega * c)
    den = complex(k - m * omega * omega, omega * c)
    return float(abs(num / den))


def force_transmissibility(omega: float, line: LumpedLine) -> float:
    """Tension-oscillation amplitude per unit base displacement [N/m].

    The dynamic tension is the payload inertia force ``m*x''``, so it carries an
    extra ``(omega/wn)^2`` vs the displacement transmissibility:
    ``m * omega^2 * |X/Z_top|``.
    """
    return line.m * omega * omega * displacement_transmissibility(omega, line)


@dataclass
class TensionResult:
    """Dynamic top-tension screen result."""

    t: np.ndarray
    tension: np.ndarray
    daf: float           # peak line load / static_tension (max(T), not max|T|)
    min_tension: float
    snatch: bool         # min_tension <= 0 -> line goes slack
    daf_valid: bool      # False when snatch (linear model invalid past first slack)


def _newmark(z: np.ndarray, zdot: np.ndarray, dt: float, line: LumpedLine):
    """Newmark-beta (beta=1/4, gamma=1/2) base-excited SDOF -> payload x, x'."""
    m, c, k = line.m, line.c, line.k
    beta, gamma = 0.25, 0.5
    f = c * zdot + k * z                       # absolute-coordinate forcing
    n = z.size
    x = np.zeros(n)
    v = np.zeros(n)
    a = np.zeros(n)
    x[0] = z[0]                                 # start at quasi-equilibrium
    v[0] = zdot[0]
    a[0] = (f[0] - c * v[0] - k * x[0]) / m
    a1 = 1.0 / (beta * dt * dt)
    a2 = 1.0 / (beta * dt)
    a3 = 1.0 / (2.0 * beta) - 1.0
    a4 = gamma / (beta * dt)
    a5 = gamma / beta - 1.0
    a6 = dt * (gamma / (2.0 * beta) - 1.0)
    keff = k + a4 * c + a1 * m
    for i in range(n - 1):
        feff = (f[i + 1]
                + m * (a1 * x[i] + a2 * v[i] + a3 * a[i])
                + c * (a4 * x[i] + a5 * v[i] + a6 * a[i]))
        x[i + 1] = feff / keff
        a[i + 1] = a1 * (x[i + 1] - x[i]) - a2 * v[i] - a3 * a[i]
        v[i + 1] = v[i] + dt * ((1.0 - gamma) * a[i] + gamma * a[i + 1])
    return x, v


def dynamic_tension(motion, line: LumpedLine, *, top_offset=(0.0, 0.0, 0.0)) -> TensionResult:
    """Dynamic line tension at the deployment point via the lumped SDOF screen.

    The deployment-point vertical motion base-excites the payload; the line
    tension is screened for dynamic amplification (``daf``) and slack onset
    (``snatch``). Refines the time grid to ``<= Tn/20`` for integrator accuracy
    near a short natural period.
    """
    if motion.t.size < 2 or not (float(motion.t[-1]) > float(motion.t[0])):
        raise ValueError("dynamic_tension needs a motion with >= 2 samples spanning t1 > t0")
    z_coarse = vertical_motion_at(motion, top_offset)
    zdot_coarse = time_derivative(motion.t, z_coarse)
    t0, t1 = float(motion.t[0]), float(motion.t[-1])

    tn = 2.0 * np.pi / line.wn
    dt_grid = (t1 - t0) / (motion.t.size - 1)
    dt = min(dt_grid, tn / 20.0)
    n = max(2, int(np.ceil((t1 - t0) / dt)) + 1)
    if n > 2_000_000:
        raise ValueError(
            f"refined grid too large ({n} steps): line too stiff (Tn={tn:.3g}s) "
            f"or record too long for this SDOF screen"
        )
    t = np.linspace(t0, t1, n)
    z = np.interp(t, motion.t, z_coarse)
    # Interpolate the coarse derivative (not re-differentiate the kinked interpolant);
    # when refinement triggers, the screen's fidelity ceiling is the input sampling.
    zdot = np.interp(t, motion.t, zdot_coarse)

    x, v = _newmark(z, zdot, t[1] - t[0], line)
    tension = line.static_tension + line.k * (z - x) + line.c * (zdot - v)

    tmax = float(np.max(tension))
    tmin = float(np.min(tension))
    snatch = tmin <= 0.0
    return TensionResult(
        t=t, tension=tension,
        daf=tmax / line.static_tension,
        min_tension=tmin, snatch=snatch, daf_valid=not snatch,
    )
