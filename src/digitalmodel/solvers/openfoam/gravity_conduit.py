#!/usr/bin/env python3
"""
ABOUTME: Gravity-driven conduit exchange model for coupled tank fill/drain
(#1528 slice 2). Computes signed hydrostatic head between two prismatic tanks,
a Cd/A/conduit-loss orifice flow law, and an RK4 integrator that moves liquid
through the conduit while conserving volume and mass, reversing with the head
sign, and rejecting (or flagging) dry-out and overflow states.

The reduced-order model here feeds the OpenFOAM/VOF coupling layers; it is
deliberately solver-free so the flow-law integral and conservation invariants
can be verified analytically without a CFD run.

Physics
-------
For two prismatic tanks joined by a conduit, the driving head is the signed
difference of free-surface elevations (plus any externally imposed head, e.g. a
roll-induced tilt)::

    H = z_surface_source - z_surface_dest (+ external_head(t))

The conduit obeys an orifice/Bernoulli law with an effective discharge
coefficient that folds minor losses (K) and Darcy friction (f L / D) into the
nominal discharge coefficient::

    Cd_eff = Cd / sqrt(1 + K + f L / D)
    Q      = sign(H) * Cd_eff * A * sqrt(2 g |H|)

Positive Q moves liquid from source to destination. Because the integrator
carries a single state variable (the source volume) and derives the destination
volume as ``V_total - V_source``, total volume -- and therefore mass for a single
incompressible fluid -- is conserved to machine precision by construction.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Callable, List, Optional

from loguru import logger

# Standard gravity (m/s^2), matching the CFD case conventions in this package.
G_STANDARD: float = 9.80665

# Absolute tolerance for fill-range validation (m^3) to absorb float round-off.
_VOLUME_TOL: float = 1e-9


# ============================================================================
# Tank state
# ============================================================================


@dataclass(frozen=True)
class TankState:
    """Instantaneous liquid state of one prismatic tank.

    Attributes:
        volume: Current liquid volume (m^3), must be in ``[0, capacity]``.
        plan_area: Horizontal cross-sectional (plan) area (m^2, > 0).
        height: Internal tank height (m, > 0).
        floor_elevation: Absolute elevation of the tank floor (m).

    Raises:
        ValueError: If the geometry is non-physical or the volume falls outside
            ``[0, capacity]`` (dry-out below empty / overflow above capacity).
    """

    volume: float
    plan_area: float
    height: float
    floor_elevation: float = 0.0

    def __post_init__(self) -> None:
        if self.plan_area <= 0.0:
            raise ValueError(f"plan_area must be positive, got {self.plan_area}")
        if self.height <= 0.0:
            raise ValueError(f"height must be positive, got {self.height}")
        capacity = self.plan_area * self.height
        if self.volume < -_VOLUME_TOL:
            raise ValueError(
                f"volume {self.volume} m^3 is below empty (dry-out); must be >= 0"
            )
        if self.volume > capacity + _VOLUME_TOL:
            raise ValueError(
                f"volume {self.volume} m^3 exceeds capacity {capacity} m^3 (overflow)"
            )

    @property
    def capacity(self) -> float:
        """Maximum liquid volume (m^3) = ``plan_area * height``."""
        return self.plan_area * self.height

    @property
    def fill_fraction(self) -> float:
        """Liquid volume as a fraction of capacity (0-1)."""
        return self.volume / self.capacity

    @property
    def liquid_depth(self) -> float:
        """Still-water liquid depth above the floor (m) = ``volume / plan_area``."""
        return self.volume / self.plan_area

    @property
    def surface_elevation(self) -> float:
        """Absolute free-surface elevation (m) = ``floor_elevation + liquid_depth``."""
        return self.floor_elevation + self.liquid_depth

    @property
    def free_volume(self) -> float:
        """Remaining volume before overflow (m^3) = ``capacity - volume``."""
        return self.capacity - self.volume

    def with_volume(self, volume: float) -> "TankState":
        """Return a copy of this tank with a new liquid ``volume`` (m^3)."""
        return TankState(
            volume=volume,
            plan_area=self.plan_area,
            height=self.height,
            floor_elevation=self.floor_elevation,
        )


# ============================================================================
# Conduit geometry + flow law
# ============================================================================


@dataclass(frozen=True)
class ConduitGeometry:
    """Geometry and loss parameters for the connecting conduit.

    The nominal discharge coefficient and the loss terms combine into an
    effective discharge coefficient ``Cd / sqrt(1 + K_total)`` where
    ``K_total = loss_coefficient + friction_factor * length / diameter``. The
    friction term is optional; if any of ``friction_factor``/``length``/
    ``diameter`` is supplied, all three are required.

    Attributes:
        area: Conduit flow cross-sectional area (m^2, > 0).
        discharge_coefficient: Nominal orifice discharge coefficient Cd (0-1].
        loss_coefficient: Sum of minor (K) loss coefficients (>= 0).
        friction_factor: Darcy friction factor f (optional, >= 0).
        length: Conduit length L (m, optional, > 0).
        diameter: Conduit hydraulic diameter D (m, optional, > 0).
        invert_elevation: Absolute elevation of the conduit connection (m). Flow
            requires the higher of the two liquid surfaces to sit above this.
    """

    area: float
    discharge_coefficient: float = 0.6
    loss_coefficient: float = 0.0
    friction_factor: Optional[float] = None
    length: Optional[float] = None
    diameter: Optional[float] = None
    invert_elevation: float = 0.0

    def __post_init__(self) -> None:
        if self.area <= 0.0:
            raise ValueError(f"area must be positive, got {self.area}")
        if not 0.0 < self.discharge_coefficient <= 1.0:
            raise ValueError(
                f"discharge_coefficient must be in (0, 1], got "
                f"{self.discharge_coefficient}"
            )
        if self.loss_coefficient < 0.0:
            raise ValueError(
                f"loss_coefficient must be >= 0, got {self.loss_coefficient}"
            )
        friction_terms = (self.friction_factor, self.length, self.diameter)
        if any(t is not None for t in friction_terms):
            if any(t is None for t in friction_terms):
                raise ValueError(
                    "friction_factor, length and diameter must all be given "
                    "together to include the Darcy friction term"
                )
            if self.friction_factor < 0.0:
                raise ValueError(
                    f"friction_factor must be >= 0, got {self.friction_factor}"
                )
            if self.length <= 0.0:
                raise ValueError(f"length must be positive, got {self.length}")
            if self.diameter <= 0.0:
                raise ValueError(f"diameter must be positive, got {self.diameter}")

    @property
    def total_loss_coefficient(self) -> float:
        """Combined minor + friction loss coefficient ``K + f L / D``."""
        k = self.loss_coefficient
        if self.friction_factor is not None:
            k += self.friction_factor * self.length / self.diameter
        return k

    @property
    def effective_discharge_coefficient(self) -> float:
        """Loss-adjusted discharge coefficient ``Cd / sqrt(1 + K_total)``."""
        return self.discharge_coefficient / math.sqrt(1.0 + self.total_loss_coefficient)


def signed_hydrostatic_head(source: TankState, dest: TankState) -> float:
    """Signed hydrostatic head (m) driving flow from ``source`` to ``dest``.

    Positive values mean the source free surface is higher and liquid tends to
    flow source -> dest; negative values drive the reverse.
    """
    return source.surface_elevation - dest.surface_elevation


def conduit_flow_rate(
    head: float, conduit: ConduitGeometry, *, g: float = G_STANDARD
) -> float:
    """Volumetric flow rate (m^3/s) through the conduit for a signed ``head`` (m).

    Applies the loss-adjusted orifice law
    ``Q = sign(H) * Cd_eff * A * sqrt(2 g |H|)``. The sign of the result follows
    the sign of ``head`` so the same primitive handles flow reversal; a zero head
    yields exactly zero flow.
    """
    if head == 0.0:
        return 0.0
    magnitude = (
        conduit.effective_discharge_coefficient
        * conduit.area
        * math.sqrt(2.0 * g * abs(head))
    )
    return math.copysign(magnitude, head)


def check_transfer_feasibility(
    source: TankState, dest: TankState, transfer_volume: float
) -> None:
    """Validate that ``transfer_volume`` (m^3) can move source -> dest.

    A positive ``transfer_volume`` moves liquid out of ``source`` into ``dest``;
    a negative value moves it the other way. The check rejects transfers that
    would drain a tank below empty (dry-out) or fill one past capacity
    (overflow) with an actionable diagnostic.

    Raises:
        ValueError: If the requested transfer exceeds the available liquid volume
            (dry-out) or the available free volume (overflow) on either side.
    """
    out_tank, in_tank = (source, dest) if transfer_volume >= 0.0 else (dest, source)
    magnitude = abs(transfer_volume)
    if magnitude > out_tank.volume + _VOLUME_TOL:
        raise ValueError(
            f"requested transfer {magnitude} m^3 exceeds available liquid "
            f"{out_tank.volume} m^3 (dry-out)"
        )
    if magnitude > in_tank.free_volume + _VOLUME_TOL:
        raise ValueError(
            f"requested transfer {magnitude} m^3 exceeds available free volume "
            f"{in_tank.free_volume} m^3 (overflow)"
        )


# ============================================================================
# Coupled gravity-exchange integrator
# ============================================================================


@dataclass(frozen=True)
class GravityExchangeResult:
    """Synchronized time histories and summary of a gravity-exchange run.

    Attributes:
        time: Sample times (s).
        source_volume: Source liquid volume at each time (m^3).
        dest_volume: Destination liquid volume at each time (m^3).
        head: Signed driving head at each time, including external head (m).
        flow_rate: Signed conduit flow rate at each time (m^3/s).
        transferred_volume: Net source -> dest volume moved over the run (m^3).
        mass_residual: Max absolute deviation of total mass from its initial
            value across the run (kg); ~0 for a conservative run.
        reversed_flow: True if the flow rate changed sign during the run.
        termination: Why the run ended -- one of ``"duration"``,
            ``"equilibrium"``, ``"source_dry"``, ``"dest_dry"``,
            ``"source_overflow"``, ``"dest_overflow"``, or ``"no_flow"``.
        density: Fluid density used for the mass residual (kg/m^3).
    """

    time: List[float]
    source_volume: List[float]
    dest_volume: List[float]
    head: List[float]
    flow_rate: List[float]
    transferred_volume: float
    mass_residual: float
    reversed_flow: bool
    termination: str
    density: float = 1025.0


def simulate_gravity_exchange(
    source: TankState,
    dest: TankState,
    conduit: ConduitGeometry,
    *,
    duration: float,
    dt: float,
    density: float = 1025.0,
    g: float = G_STANDARD,
    external_head: Optional[Callable[[float], float]] = None,
    equilibrium_tol: float = 1e-9,
    reject_dryout: bool = False,
    reject_overflow: bool = False,
) -> GravityExchangeResult:
    """Integrate gravity-driven exchange between two tanks through a conduit.

    The source volume is advanced with a classic RK4 step under
    ``dV_source/dt = -Q(H)``; the destination volume is derived as
    ``V_total - V_source`` so volume (and mass, for one incompressible fluid) is
    conserved to machine precision at every step -- including when the head
    changes sign and the flow reverses. Steps that would push a tank below empty
    or above capacity are clamped; with ``reject_dryout``/``reject_overflow`` the
    run raises instead of clamping.

    Args:
        source: Initial source tank state.
        dest: Initial destination tank state.
        conduit: Conduit geometry and loss parameters.
        duration: Total simulated time (s, > 0).
        dt: Fixed time step (s, > 0).
        density: Fluid density (kg/m^3) for the mass residual.
        g: Gravitational acceleration (m/s^2).
        external_head: Optional callable ``t -> head offset (m)`` added to the
            geometric head (e.g. a roll-induced surface tilt); enables and tests
            flow reversal within a single run.
        equilibrium_tol: Head magnitude (m) below which -- with no external head
            -- the run is considered equilibrated and stops early.
        reject_dryout: Raise ``ValueError`` if a tank would drain below empty.
        reject_overflow: Raise ``ValueError`` if a tank would fill past capacity.

    Returns:
        A :class:`GravityExchangeResult` with synchronized histories and summary.

    Raises:
        ValueError: For non-physical ``duration``/``dt``, or on a dry-out/overflow
            event when the corresponding reject flag is set.
    """
    if duration <= 0.0:
        raise ValueError(f"duration must be positive, got {duration}")
    if dt <= 0.0:
        raise ValueError(f"dt must be positive, got {dt}")
    if source.plan_area <= 0.0 or dest.plan_area <= 0.0:
        raise ValueError("both tanks must have positive plan area")

    ext = external_head if external_head is not None else (lambda _t: 0.0)

    area_s = source.plan_area
    area_d = dest.plan_area
    floor_s = source.floor_elevation
    floor_d = dest.floor_elevation
    v_total = source.volume + dest.volume

    # Source volume bounds that keep BOTH tanks physical (0 <= V <= capacity).
    vs_min = max(0.0, v_total - dest.capacity)   # dest would overflow below this
    vs_max = min(source.capacity, v_total)       # source would overflow above this

    def head_of(vs: float, t: float) -> float:
        z_source = floor_s + vs / area_s
        z_dest = floor_d + (v_total - vs) / area_d
        return (z_source - z_dest) + ext(t)

    def dvs_dt(vs: float, t: float) -> float:
        h = head_of(vs, t)
        # Submergence guard: the higher surface must reach the conduit invert,
        # otherwise the conduit is not submerged on the driving side -> no flow.
        z_source = floor_s + vs / area_s
        z_dest = floor_d + (v_total - vs) / area_d
        if max(z_source, z_dest) <= conduit.invert_elevation:
            return 0.0
        return -conduit_flow_rate(h, conduit, g=g)

    times: List[float] = [0.0]
    vs_hist: List[float] = [source.volume]
    vd_hist: List[float] = [dest.volume]
    head_hist: List[float] = [head_of(source.volume, 0.0)]
    q_hist: List[float] = [-dvs_dt(source.volume, 0.0)]

    vs = source.volume
    n_steps = int(math.ceil(duration / dt))
    termination = "duration"
    any_flow = q_hist[0] != 0.0

    for step in range(1, n_steps + 1):
        t0 = (step - 1) * dt
        h_dt = min(dt, duration - t0)
        if h_dt <= 0.0:
            break

        prev_head = head_hist[-1]

        # Classic RK4 on the source volume.
        k1 = dvs_dt(vs, t0)
        k2 = dvs_dt(vs + 0.5 * h_dt * k1, t0 + 0.5 * h_dt)
        k3 = dvs_dt(vs + 0.5 * h_dt * k2, t0 + 0.5 * h_dt)
        k4 = dvs_dt(vs + h_dt * k3, t0 + h_dt)
        vs_new = vs + (h_dt / 6.0) * (k1 + 2.0 * k2 + 2.0 * k3 + k4)

        clamp_event: Optional[str] = None
        if vs_new < vs_min:
            clamp_event = "dest_overflow" if vs_min > 0.0 else "source_dry"
            vs_new = vs_min
        elif vs_new > vs_max:
            clamp_event = "source_overflow" if vs_max < v_total else "dest_dry"
            vs_new = vs_max

        t1 = t0 + h_dt
        q = (vs - vs_new) / h_dt            # exact conduit flow implied by the step
        vs = vs_new
        new_head = head_of(vs, t1)

        times.append(t1)
        vs_hist.append(vs)
        vd_hist.append(v_total - vs)
        head_hist.append(new_head)
        q_hist.append(q)
        if q != 0.0:
            any_flow = True

        # Static equilibrium (only when no external head keeps re-energising the
        # system). The sqrt-head flow law is non-Lipschitz at H=0, so a fixed-step
        # integrator cannot land exactly on H=0 -- it either crosses zero or stalls
        # a hair short. Both mean the levels have equalised, as does the head
        # simply falling below tolerance. The head decreases monotonically through
        # the physical transient, so "stopped decreasing" is a dt-independent
        # equilibrium signal rather than a hand-tuned threshold.
        if clamp_event is None and external_head is None and any_flow:
            crossed_zero = prev_head * new_head < 0.0
            stalled = abs(new_head) >= abs(prev_head)
            if abs(new_head) <= equilibrium_tol or crossed_zero or stalled:
                termination = "equilibrium"
                break

        if clamp_event is not None:
            if clamp_event.endswith("overflow") and reject_overflow:
                raise ValueError(
                    f"gravity exchange would cause {clamp_event.replace('_', ' ')} "
                    f"at t={t1:.4g}s; requested transfer exceeds available free volume"
                )
            if clamp_event.endswith("dry") and reject_dryout:
                raise ValueError(
                    f"gravity exchange would cause {clamp_event.replace('_', ' ')} "
                    f"at t={t1:.4g}s; requested transfer exceeds available liquid"
                )
            termination = clamp_event
            logger.warning(
                "Gravity exchange clamped at t={:.4g}s ({}); flow stopped to keep "
                "tank volumes physical.",
                t1,
                clamp_event,
            )
            break

    if not any_flow:
        termination = "no_flow"

    reversed_flow = _has_sign_change(q_hist)
    transferred_volume = vs_hist[0] - vs_hist[-1]
    mass_residual = max(
        abs(density * (vs + vd) - density * v_total)
        for vs, vd in zip(vs_hist, vd_hist)
    )

    return GravityExchangeResult(
        time=times,
        source_volume=vs_hist,
        dest_volume=vd_hist,
        head=head_hist,
        flow_rate=q_hist,
        transferred_volume=transferred_volume,
        mass_residual=mass_residual,
        reversed_flow=reversed_flow,
        termination=termination,
        density=density,
    )


def _has_sign_change(values: List[float]) -> bool:
    """True if the non-zero entries of ``values`` include both signs."""
    seen_positive = False
    seen_negative = False
    for v in values:
        if v > 0.0:
            seen_positive = True
        elif v < 0.0:
            seen_negative = True
        if seen_positive and seen_negative:
            return True
    return False
