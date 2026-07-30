# ABOUTME: Everitt-Jennings space-marching finite-difference rod-string solver.
# ABOUTME: Converts a surface dynamometer card to a downhole pump card in SI units.
"""Surface-to-downhole card conversion by space-marching finite differences.

The rod string is a damped elastic bar. Given the measured position and load at
the polished rod, the damped wave equation is marched *down* the string one
spatial node at a time until the pump depth is reached. The downhole card is the
last row of that solution.

Two things distinguish this from a naive implementation, and both matter:

1. **Load is rebuilt from strain, never inherited.** The downhole load is
   ``F = EA * du/dx`` evaluated at the pump — Hooke's law on the computed
   displacement field. This is what removes the rod-string vibration harmonics
   from the card and makes a healthy pump card rectangular. Any implementation
   that scales the surface load instead will reproduce every ripple in the
   surface card and cannot diagnose anything.
2. **Damping is per-section and direction-dependent.** Upstroke and downstroke
   damping differ, which is what gives the card its enclosed area. See
   :mod:`.damping`.

The scheme marches in space (``i``) across all time steps (``j``), with the
surface card supplying the boundary rows ``i = 0`` and ``i = 1``. Time is
periodic: one stroke wraps onto itself.

All quantities are SI (m, kg, s, N, Pa). Use :mod:`.units` at the boundary.

Reference: Everitt, T.A. and Jennings, J.W., "An Improved Finite-Difference
Calculation of Downhole Dynamometer Cards for Sucker-Rod Pumps", SPE Production
Engineering, SPE 18189.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional, Tuple

import numpy as np
from scipy.signal import savgol_filter

from .damping import estimate_damping_profile

# ---------------------------------------------------------------------------
# Optional numba acceleration. The kernel is a doubly-nested Python loop, so
# numba gives a large speedup when present, but it is never required.
# ---------------------------------------------------------------------------
try:  # pragma: no cover - exercised implicitly by whichever path is installed
    from numba import njit as _njit

    HAS_NUMBA = True
except ImportError:  # pragma: no cover
    HAS_NUMBA = False

    def _njit(*args, **kwargs):
        """No-op stand-in for numba.njit when numba is not installed."""
        def decorator(func):
            return func

        if args and callable(args[0]):
            return args[0]
        return decorator


DEFAULT_NUM_NODES = 100
DEFAULT_SAVGOL_WINDOW = 21
DEFAULT_SAVGOL_ORDER = 5
STEEL_MODULUS_PA = 2.07e11
STEEL_DENSITY_KG_M3 = 7850.0
WATER_DENSITY_KG_M3 = 998.2
GRAVITY_M_S2 = 9.80665


# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------
@dataclass
class RodString:
    """Tapered rod string, ordered top-of-string first.

    Attributes:
        diameters: Rod body OD per section, m.
        lengths: Length per section, m.
        densities: Material density per section, kg/m^3.
        moduli: Young's modulus per section, Pa.
        coupling_diameters: Coupling OD per section, m. Used only by the
            damping estimator; defaults to 1.5x the rod body OD.
    """

    diameters: np.ndarray
    lengths: np.ndarray
    densities: Optional[np.ndarray] = None
    moduli: Optional[np.ndarray] = None
    coupling_diameters: Optional[np.ndarray] = None

    def __post_init__(self) -> None:
        self.diameters = np.asarray(self.diameters, dtype=np.float64)
        self.lengths = np.asarray(self.lengths, dtype=np.float64)
        n = len(self.diameters)
        if len(self.lengths) != n:
            raise ValueError(
                f"rod diameters ({n}) and lengths ({len(self.lengths)}) "
                "must have the same number of sections"
            )
        if n == 0:
            raise ValueError("rod string must have at least one section")
        if np.any(self.diameters <= 0) or np.any(self.lengths <= 0):
            raise ValueError("rod diameters and lengths must all be positive")

        if self.densities is None:
            self.densities = np.full(n, STEEL_DENSITY_KG_M3)
        if self.moduli is None:
            self.moduli = np.full(n, STEEL_MODULUS_PA)
        if self.coupling_diameters is None:
            self.coupling_diameters = self.diameters * 1.5
        self.densities = np.asarray(self.densities, dtype=np.float64)
        self.moduli = np.asarray(self.moduli, dtype=np.float64)
        self.coupling_diameters = np.asarray(
            self.coupling_diameters, dtype=np.float64
        )

    @property
    def total_length(self) -> float:
        """Total rod string length, m."""
        return float(np.sum(self.lengths))

    @property
    def areas(self) -> np.ndarray:
        """Cross-sectional area per section, m^2."""
        return np.pi * (self.diameters / 2.0) ** 2


@dataclass
class Survey:
    """Wellbore trajectory, used for deviated-well friction terms.

    A vertical well contributes no Coulomb friction and no curvature terms, so
    :meth:`vertical` is the correct default when no survey is available.
    """

    measured_depth: np.ndarray
    inclination: np.ndarray
    azimuth: np.ndarray
    inclination_gradient: Optional[np.ndarray] = None
    azimuth_gradient: Optional[np.ndarray] = None

    @classmethod
    def vertical(cls, total_length: float) -> "Survey":
        """Build a trivial vertical survey spanning the rod string."""
        md = np.array([0.0, total_length], dtype=np.float64)
        zeros = np.zeros(2, dtype=np.float64)
        return cls(
            measured_depth=md,
            inclination=zeros.copy(),
            azimuth=zeros.copy(),
            inclination_gradient=zeros.copy(),
            azimuth_gradient=zeros.copy(),
        )

    def __post_init__(self) -> None:
        self.measured_depth = np.asarray(self.measured_depth, dtype=np.float64)
        self.inclination = np.asarray(self.inclination, dtype=np.float64)
        self.azimuth = np.asarray(self.azimuth, dtype=np.float64)
        if self.inclination_gradient is None:
            self.inclination_gradient = np.gradient(
                self.inclination, self.measured_depth
            )
        if self.azimuth_gradient is None:
            self.azimuth_gradient = np.gradient(self.azimuth, self.measured_depth)
        self.inclination_gradient = np.asarray(
            self.inclination_gradient, dtype=np.float64
        )
        self.azimuth_gradient = np.asarray(self.azimuth_gradient, dtype=np.float64)

    def phi(self, s: np.ndarray) -> np.ndarray:
        """Inclination angle at measured depth ``s``, radians."""
        return np.interp(s, self.measured_depth, self.inclination)

    def d_phi(self, s: np.ndarray) -> np.ndarray:
        """Inclination gradient at measured depth ``s``."""
        return np.interp(s, self.measured_depth, self.inclination_gradient)

    def d_psi(self, s: np.ndarray) -> np.ndarray:
        """Azimuth gradient at measured depth ``s``."""
        return np.interp(s, self.measured_depth, self.azimuth_gradient)


# ---------------------------------------------------------------------------
# Derived simulation state
# ---------------------------------------------------------------------------
@dataclass
class Simulation:
    """Grid and physical arrays derived from the inputs. All SI."""

    u: np.ndarray = field(default_factory=lambda: np.empty(0))
    f: np.ndarray = field(default_factory=lambda: np.empty(0))
    taper_lengths: np.ndarray = field(default_factory=lambda: np.empty(0))
    n_tap: int = 0
    rod_length: float = 0.0
    areas: np.ndarray = field(default_factory=lambda: np.empty(0))
    rho_a: np.ndarray = field(default_factory=lambda: np.empty(0))
    moduli: np.ndarray = field(default_factory=lambda: np.empty(0))
    damping: np.ndarray = field(default_factory=lambda: np.empty(0))
    densities: np.ndarray = field(default_factory=lambda: np.empty(0))
    wave_speed: np.ndarray = field(default_factory=lambda: np.empty(0))
    volume: float = 0.0
    weight: float = 0.0
    buoyant_weight: float = 0.0
    gravity: float = GRAVITY_M_S2
    friction: float = 0.0
    n_x: int = DEFAULT_NUM_NODES
    n_t: int = 0
    period: float = 0.0
    dt: float = 0.0
    dx: float = 0.0
    x: np.ndarray = field(default_factory=lambda: np.empty(0))
    t: np.ndarray = field(default_factory=lambda: np.empty(0))
    up_dn: int = 0
    boundary: np.ndarray = field(default_factory=lambda: np.empty(0))
    cumulative_buoyant: np.ndarray = field(default_factory=lambda: np.empty(0))
    interfaces: np.ndarray = field(default_factory=lambda: np.empty(0, dtype=int))


@dataclass
class Coefficients:
    """Space-time coefficient matrices, each shaped ``(n_x, n_t)``."""

    wave_speed: np.ndarray = field(default_factory=lambda: np.empty(0))
    moduli: np.ndarray = field(default_factory=lambda: np.empty(0))
    areas: np.ndarray = field(default_factory=lambda: np.empty(0))
    rho_a: np.ndarray = field(default_factory=lambda: np.empty(0))
    damping: np.ndarray = field(default_factory=lambda: np.empty(0))
    phi: np.ndarray = field(default_factory=lambda: np.empty(0))
    d_phi: np.ndarray = field(default_factory=lambda: np.empty(0))
    d_psi: np.ndarray = field(default_factory=lambda: np.empty(0))


def _section_cuts(taper_lengths: np.ndarray) -> np.ndarray:
    """Cumulative section boundaries along the string, starting at zero."""
    cuts = np.zeros(len(taper_lengths) + 1, dtype=np.float64)
    cuts[1:] = np.cumsum(taper_lengths)
    return cuts


def build_simulation(
    position: np.ndarray,
    load: np.ndarray,
    rods: RodString,
    strokes_per_minute: float,
    fluid_density: float = WATER_DENSITY_KG_M3,
    damping: Optional[np.ndarray] = None,
    friction_coefficient: float = 0.0,
    n_nodes: int = DEFAULT_NUM_NODES,
    gravity: float = GRAVITY_M_S2,
    include_gravity: bool = True,
) -> Simulation:
    """Assemble the simulation grid from surface measurements and rod data.

    Args:
        position: Surface position, m. Sign convention is handled here: the
            solver marches with *up* as the positive direction, so the incoming
            conventional (positive-up-from-bottom) card is negated internally.
        load: Surface load, N.
        rods: The tapered rod string.
        strokes_per_minute: Pumping speed.
        fluid_density: Produced fluid density in the tubing, kg/m^3.
        damping: Damping coefficients, length ``2 * n_sections`` (upstroke
            first, then downstroke). If omitted, zeros are used and the caller
            is expected to supply an estimate.
        friction_coefficient: Coulomb friction coefficient for deviated wells.
        n_nodes: Number of spatial nodes down the string.
        gravity: Gravitational acceleration, m/s^2.
        include_gravity: When False, gravity and friction terms are muted —
            useful for isolating the elastic response.

    Returns:
        A populated :class:`Simulation`.

    Raises:
        ValueError: If the card arrays disagree in length or the speed is
            non-positive.
    """
    position = np.asarray(position, dtype=np.float64)
    load = np.asarray(load, dtype=np.float64)
    if len(position) != len(load):
        raise ValueError(
            f"position ({len(position)}) and load ({len(load)}) must have "
            "the same number of samples"
        )
    if len(position) < 4:
        raise ValueError(
            f"card needs at least 4 samples to march; got {len(position)}"
        )
    if strokes_per_minute <= 0:
        raise ValueError(f"strokes_per_minute must be positive; got {strokes_per_minute}")

    sim = Simulation()
    # Up is the positive marching direction, so negate the conventional card.
    sim.u = -position.copy()
    sim.f = load.copy()
    sim.gravity = gravity
    sim.friction = friction_coefficient

    sim.taper_lengths = rods.lengths
    sim.n_tap = len(rods.lengths)
    sim.rod_length = rods.total_length
    sim.areas = rods.areas
    sim.densities = rods.densities
    sim.moduli = rods.moduli
    sim.rho_a = rods.densities * rods.areas
    sim.damping = (
        np.zeros(2 * sim.n_tap, dtype=np.float64)
        if damping is None
        else np.asarray(damping, dtype=np.float64)
    )

    sim.volume = float(np.sum(sim.taper_lengths * sim.areas))
    sim.weight = float(np.sum(sim.rho_a * sim.taper_lengths))
    sim.buoyant_weight = (sim.weight - sim.volume * fluid_density) * gravity
    sim.wave_speed = np.sqrt(sim.moduli / sim.densities)

    sim.n_x = n_nodes
    sim.n_t = len(sim.f)
    sim.period = 60.0 / strokes_per_minute
    sim.t = np.linspace(0.0, sim.period, sim.n_t)
    sim.dt = sim.t[1] - sim.t[0]
    sim.x = np.linspace(0.0, sim.rod_length, sim.n_x)
    sim.dx = sim.x[1] - sim.x[0]

    # Distributed buoyant weight, accumulated from the pump upward.
    d_volume = sim.dx * sim.areas
    d_weight = sim.densities * d_volume
    d_buoyant = (d_weight - d_volume * fluid_density) * gravity

    cuts = _section_cuts(sim.taper_lengths)
    per_node = np.full(sim.n_x, np.nan, dtype=np.float64)
    for i in range(len(cuts) - 1):
        mask = (cuts[i] <= sim.x) & (sim.x <= cuts[i + 1])
        per_node[mask] = d_buoyant[i]
    sim.cumulative_buoyant = np.flip(np.cumsum(per_node), 0)

    # Index at which the stroke turns from down to up.
    sim.up_dn = int(np.mean(np.where(sim.u == np.min(sim.u))[0]))

    # Boundary forcing at the polished rod.
    #
    # The static rod weight is accounted for in exactly one place, never two.
    # When the gravity term is active in the marching kernel, the PDE carries
    # it and the boundary takes the measured load as-is. When gravity is muted,
    # the buoyant weight is instead pre-subtracted here. Getting this backwards
    # double-counts the string weight and shifts the whole downhole card.
    sim.boundary = sim.f if include_gravity else sim.f - sim.buoyant_weight
    return sim


def build_coefficients(sim: Simulation, survey: Survey) -> Coefficients:
    """Expand per-section and per-depth properties onto the (space, time) grid."""
    n_x, n_t = sim.n_x, sim.n_t
    coeff = Coefficients()
    cuts = _section_cuts(sim.taper_lengths)

    for name, values in (
        ("wave_speed", sim.wave_speed),
        ("moduli", sim.moduli),
        ("areas", sim.areas),
        ("rho_a", sim.rho_a),
    ):
        matrix = np.full((n_x, n_t), np.nan, dtype=np.float64)
        for i in range(len(cuts) - 1):
            mask = (cuts[i] <= sim.x) & (sim.x <= cuts[i + 1])
            matrix[mask, :] = values[i]
        setattr(coeff, name, matrix)

    # Damping varies with BOTH depth and stroke direction. The upstroke value
    # applies to time steps before the turnaround, the downstroke value after.
    #
    # NOTE: the reference implementation sliced the spatial axis twice here
    # (`c[lo:hi][:up_dn]`), which applied upstroke damping to the first `up_dn`
    # *nodes* of each section rather than the first `up_dn` *time steps*. The
    # sibling direction-coefficient routine indexes `[:, :up_dn]`, confirming
    # the intent. Fixed below; see the porting notes on the tracking issue.
    damping = np.reshape(sim.damping, (2, sim.n_tap)).T
    coeff.damping = np.empty((n_x, n_t), dtype=np.float64)
    for i in range(len(cuts) - 1):
        lo = np.searchsorted(sim.x, cuts[i])
        hi = np.searchsorted(sim.x, cuts[i + 1], side="right")
        coeff.damping[lo:hi, : sim.up_dn] = damping[i, 0]
        coeff.damping[lo:hi, sim.up_dn :] = damping[i, 1]

    phi = survey.phi(sim.x)
    d_phi = survey.d_phi(sim.x)
    d_psi = survey.d_psi(sim.x)
    coeff.phi = np.repeat(phi[:, None], n_t, axis=1)
    coeff.d_phi = np.repeat(d_phi[:, None], n_t, axis=1)
    coeff.d_psi = np.repeat(d_psi[:, None], n_t, axis=1)
    return coeff


def initial_matrix(sim: Simulation, coeff: Coefficients) -> np.ndarray:
    """Seed the displacement matrix with the two surface boundary rows.

    Row 0 is the measured surface position. Row 1 follows from the measured
    load through Hooke's law, which is what couples the load record into a
    solution otherwise written entirely in displacement.
    """
    solution = np.empty((sim.n_x, sim.n_t), dtype=np.float64)
    solution[0, :] = sim.u
    ea_dx = coeff.moduli[0, :] * coeff.areas[0, :] / sim.dx
    solution[1, :] = solution[0, :] + sim.boundary / ea_dx
    return solution


@_njit(cache=True)
def _normal_force_per_length(
    rho_a, gravity, phi, d_phi, d_psi, axial_force
):
    """Return borehole normal load per measured length, N/m."""
    gravity_normal = rho_a * gravity * np.sin(phi)
    curvature_normal = axial_force * d_phi
    azimuth_normal = axial_force * d_psi * np.sin(phi)
    return np.sqrt(
        (gravity_normal - curvature_normal) ** 2 + azimuth_normal ** 2
    )


@_njit(cache=True)
def _march(u, n_x, n_t, dt, dx, lam, rho_a, g, c, e_mod, area, phi, d_phi, d_psi, muteg):
    """March the damped wave equation down the rod string.

    Explicit in space: each new row ``i+1`` is built from rows ``i`` and
    ``i-1`` across all time steps. Time is treated as periodic so that the
    stroke closes on itself.
    """
    for i in range(1, n_x - 1):
        for j in range(1, n_t - 1):
            ea_plus = (e_mod[i + 1, j] * area[i + 1, j] + e_mod[i, j] * area[i, j]) / 2.0
            ea_minus = (e_mod[i - 1, j] * area[i - 1, j] + e_mod[i, j] * area[i, j]) / 2.0
            rho_a_ij = rho_a[i, j]
            rho_a_c = rho_a[i, j] * c[i, j]

            # Direction of travel, for Coulomb friction.
            delta = u[i, j + 1] - u[i, j]
            if delta > 0:
                sign = 1.0
            elif delta < 0:
                sign = -1.0
            else:
                sign = 0.0

            c1 = (rho_a_ij / ea_plus) * (dx / dt) ** 2
            c2 = (rho_a_c / ea_plus) * dx ** 2 / dt
            c3 = (lam * sign / ea_plus) * dx ** 2
            c4 = (rho_a_ij / ea_plus) * dx ** 2

            axial_force = ea_minus * (u[i, j] - u[i - 1, j]) / dx
            q_n = _normal_force_per_length(
                rho_a_ij,
                g,
                phi[i, j],
                d_phi[i, j],
                d_psi[i, j],
                axial_force,
            )

            u[i + 1, j] = (
                u[i, j]
                + (ea_minus / ea_plus) * u[i, j]
                - (ea_minus / ea_plus) * u[i - 1, j]
                + c1 * (u[i, j + 1] - 2 * u[i, j] + u[i, j - 1])
                + c2 * (u[i, j + 1] - u[i, j - 1]) / 2.0
                + c3 * q_n * muteg
                - c4 * g * np.cos(phi[i, j]) * muteg
            )

        # Wrap-around time step. Coefficients are re-evaluated at j = 0 rather
        # than reusing the last inner-loop values, which the reference
        # implementation did by leaking the loop variable.
        ea_plus = (e_mod[i + 1, 0] * area[i + 1, 0] + e_mod[i, 0] * area[i, 0]) / 2.0
        ea_minus = (e_mod[i - 1, 0] * area[i - 1, 0] + e_mod[i, 0] * area[i, 0]) / 2.0
        rho_a_ij = rho_a[i, 0]
        rho_a_c = rho_a[i, 0] * c[i, 0]

        c1 = (rho_a_ij / ea_plus) * (dx / dt) ** 2
        c2 = (rho_a_c / ea_plus) * dx ** 2 / dt
        c3 = (lam / ea_plus) * dx ** 2
        c4 = (rho_a_ij / ea_plus) * dx ** 2

        axial_force = ea_minus * (u[i, 0] - u[i - 1, 0]) / dx
        q_n = _normal_force_per_length(
            rho_a_ij,
            g,
            phi[i, 0],
            d_phi[i, 0],
            d_psi[i, 0],
            axial_force,
        )

        u[i + 1, 0] = (
            u[i, 0]
            + (ea_minus / ea_plus) * u[i, 0]
            - (ea_minus / ea_plus) * u[i - 1, 0]
            + c1 * (u[i, 1] - 2 * u[i, 0] + u[i, n_t - 2])
            + c2 * (u[i, 1] - u[i, n_t - 2]) / 2.0
            + c3 * q_n * muteg
            - c4 * g * np.cos(phi[i, 0]) * muteg
        )
        u[i + 1, n_t - 1] = u[i + 1, 0]

    # Downhole load from Hooke's law on the strain at the pump. This is the
    # step that rebuilds load rather than inheriting it from the surface.
    load = np.empty(n_t, dtype=np.float64)
    alpha = e_mod[n_x - 1, 0] * area[n_x - 1, 0] / (2 * dx)
    load[0] = alpha * (3 * u[n_x - 1, 0] - 4 * u[n_x - 2, 0] + u[n_x - 3, 0])
    load[n_t - 1] = load[0]
    for j in range(1, n_t - 1):
        load[j] = (e_mod[n_x - 1, j] * area[n_x - 1, j] / dx) * (
            u[n_x - 1, j] - u[n_x - 2, j]
        )

    # Back to the conventional positive-up sign.
    downhole_position = -u[n_x - 1, :]
    return u, downhole_position, load


def _full_load(u: np.ndarray, moduli: np.ndarray, areas: np.ndarray, dx: float) -> np.ndarray:
    """Load at every node from the displacement gradient, ``F = EA du/dx``."""
    full = np.empty_like(u)
    full[0] = 0.0
    for i in range(1, u.shape[0]):
        full[i] = (moduli[i] * areas[i] / dx) * (u[i] - u[i - 1])
    return full


@dataclass
class DownholeCard:
    """Result of a surface-to-downhole conversion. SI units."""

    position: np.ndarray
    load: np.ndarray
    full_position: Optional[np.ndarray] = None
    full_load: Optional[np.ndarray] = None
    simulation: Optional[Simulation] = None

    @property
    def stroke(self) -> float:
        """Net plunger travel, m."""
        return float(np.max(self.position) - np.min(self.position))

    @property
    def load_range(self) -> float:
        """Peak-to-minimum downhole load, N."""
        return float(np.max(self.load) - np.min(self.load))


class EverittJenningsSolver:
    """Space-marching finite-difference surface-to-downhole card solver.

    Args:
        n_nodes: Spatial nodes down the rod string. More nodes resolve taper
            interfaces better but amplify high-frequency noise.
        viscosity: Produced-fluid dynamic viscosity in Pa-s, used to estimate
            damping when no explicit coefficients are supplied.
        friction_coefficient: Coulomb friction coefficient (deviated wells).
        include_gravity: When True the marching kernel carries the gravity and
            Coulomb-friction terms directly. When False (the default, matching
            the reference implementation) those terms are muted and the static
            buoyant weight is pre-subtracted at the surface boundary instead.
            Either way the rod weight is counted exactly once.
        smooth_window: Savitzky-Golay window for the downhole load. Space
            marching amplifies measurement noise, so some smoothing is
            required; set to 0 to disable.
        smooth_order: Savitzky-Golay polynomial order.
        remove_interface_jumps: Average out the load discontinuities that
            appear at taper interfaces where area and modulus step.
    """

    def __init__(
        self,
        n_nodes: int = DEFAULT_NUM_NODES,
        viscosity: float = 0.01,
        friction_coefficient: float = 0.0,
        include_gravity: bool = False,
        smooth_window: int = DEFAULT_SAVGOL_WINDOW,
        smooth_order: int = DEFAULT_SAVGOL_ORDER,
        remove_interface_jumps: bool = True,
    ):
        self.n_nodes = n_nodes
        self.viscosity = viscosity
        self.friction_coefficient = friction_coefficient
        self.include_gravity = include_gravity
        self.smooth_window = smooth_window
        self.smooth_order = smooth_order
        self.remove_interface_jumps = remove_interface_jumps

    def solve(
        self,
        position: np.ndarray,
        load: np.ndarray,
        rods: RodString,
        strokes_per_minute: float,
        pump_diameter: float,
        tubing_id: float,
        fluid_density: float = WATER_DENSITY_KG_M3,
        survey: Optional[Survey] = None,
        damping: Optional[np.ndarray] = None,
    ) -> DownholeCard:
        """Convert a surface card to a downhole pump card.

        Args:
            position: Surface position, m (conventional positive-up sense).
            load: Surface load, N.
            rods: Tapered rod string, top section first.
            strokes_per_minute: Pumping speed.
            pump_diameter: Pump bore, m.
            tubing_id: Tubing inner diameter, m.
            fluid_density: Produced fluid density, kg/m^3.
            survey: Wellbore trajectory. Defaults to vertical.
            damping: Explicit damping coefficients, length ``2 * n_sections``.
                When omitted they are estimated from the annular geometry.

        Returns:
            The downhole card, plus the full displacement and load fields.
        """
        if survey is None:
            survey = Survey.vertical(rods.total_length)

        if damping is None:
            damping = estimate_damping_profile(
                rod_diameters=rods.diameters,
                coupling_diameters=rods.coupling_diameters,
                taper_lengths=rods.lengths,
                rod_densities=rods.densities,
                pump_diameter=pump_diameter,
                tubing_id=tubing_id,
                viscosity=self.viscosity,
            )

        sim = build_simulation(
            position=position,
            load=load,
            rods=rods,
            strokes_per_minute=strokes_per_minute,
            fluid_density=fluid_density,
            damping=damping,
            friction_coefficient=self.friction_coefficient,
            n_nodes=self.n_nodes,
            include_gravity=self.include_gravity,
        )
        coeff = build_coefficients(sim, survey)
        u = initial_matrix(sim, coeff)

        muteg = 1.0 if self.include_gravity else 0.0
        u, dh_position, dh_load = _march(
            u, sim.n_x, sim.n_t, sim.dt, sim.dx, sim.friction,
            coeff.rho_a, sim.gravity, coeff.damping, coeff.moduli,
            coeff.areas, coeff.phi, coeff.d_phi, coeff.d_psi, muteg,
        )

        if self.smooth_window and self.smooth_window > self.smooth_order:
            window = min(self.smooth_window, len(dh_load))
            if window % 2 == 0:
                window -= 1
            if window > self.smooth_order:
                dh_load = savgol_filter(dh_load, window, self.smooth_order)

        # Close the loop: one stroke must return to where it began.
        dh_load[sim.n_t - 1] = dh_load[0]
        dh_position[sim.n_t - 1] = dh_position[0]

        full_position = -u
        full_load = _full_load(u, coeff.moduli, coeff.areas, sim.dx)
        full_load = full_load + sim.cumulative_buoyant.reshape(-1, 1)
        full_load[0, :] = sim.f

        # Area and modulus step at taper interfaces, which shows up as a
        # discontinuity in the strain-derived load. Average it out.
        interfaces = np.where(np.abs(np.diff(coeff.areas[:, 0])) > 1e-14)[0] + 1
        sim.interfaces = interfaces
        if self.remove_interface_jumps and len(interfaces) > 0:
            if interfaces[-1] == len(full_load) - 1:
                interfaces[-1] -= 1
            full_load[interfaces, :] = (
                full_load[interfaces - 1, :] + full_load[interfaces + 1, :]
            ) / 2.0

        return DownholeCard(
            position=dh_position,
            load=dh_load,
            full_position=full_position,
            full_load=full_load,
            simulation=sim,
        )
