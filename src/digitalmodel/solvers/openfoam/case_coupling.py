#!/usr/bin/env python3
"""
ABOUTME: CFD case coupling and pre-run verification for the reduced-order
sloshing fill/drain workflow (#1528 slice 4). Maps a deterministic SweepCase
(+ a tank/conduit/roll spec) into a buildable multiphase OpenFOAMCase, then runs
a solver-free verification GATE that must pass BEFORE any CFD is launched: it
checks the half-sine flow-pulse volume-integral identity, mass/volume
conservation of the planned exchange (reusing gravity_conduit), and rejects
dry-out / overflow and missing/inconsistent boundary conditions. It also emits a
de-identified synthetic gravity-exchange case with a small, hashed manifest (not
a full raw case tree).

Half-sine transfer law (issue #1528)
------------------------------------
The roll-driven conduit pulse over one half-cycle is a half sine::

    Q(t) = Q_peak * sin(omega * t),   omega = 2*pi/T,   f = 1/T

so that the transferred volume over a half-cycle equals the integral of the
positive lobe::

    V_transfer = integral_0^{T/2} Q_peak sin(omega t) dt = Q_peak * T / pi
    Q_peak     = pi * f * V_transfer          (equivalently Q_peak = pi V_transfer / T)
    half-cycle mean flow = V_transfer / (T/2) = 2 f V_transfer

The volume-integral identity ``V_transfer == Q_peak * T / pi`` is the
conservation invariant the gate verifies independently of any CFD run.
"""

from __future__ import annotations

import hashlib
import json
import math
from dataclasses import dataclass, field
from typing import Any, Dict, Tuple

from loguru import logger

from .gravity_conduit import (
    ConduitGeometry,
    G_STANDARD,
    TankState,
    check_transfer_feasibility,
    simulate_gravity_exchange,
)
from .models import (
    BoundaryCondition,
    BoundaryType,
    CaseType,
    DomainConfig,
    OpenFOAMCase,
)
from .motion import MotionType, PrescribedMotion
from .sloshing_sweep import SweepCase

Vec3 = Tuple[float, float, float]

# Manifest schema version (bump on breaking field changes).
COUPLING_SCHEMA_VERSION = "1.0"

# Absolute tolerance for the flow-pulse volume-integral identity (m^3).
_PULSE_TOL: float = 1e-9

# Absolute tolerance for the planned-exchange mass residual (kg).
_MASS_TOL: float = 1e-6

# Rounding applied to inputs before hashing (kills float noise for reproducible
# manifest hashes across platforms).
_ROUND = 6

# Explicit, recorded motion/flow phase convention for the coupled exchange.
PHASE_CONVENTION = (
    "roll x(t)=A*sin(omega*t) [deg]; conduit pulse Q(t)=Q_peak*sin(omega*t) is "
    "taken in phase with roll velocity (positive Q = source->dest inflow at the "
    "inlet patch). Over one half-cycle V_transfer=integral_0^{T/2} Q dt="
    "Q_peak*T/pi, i.e. Q_peak=pi*f*V_transfer. The inlet is specified as a "
    "VOLUMETRIC flow rate (flowRateInletVelocity), area-independent, at the mean "
    "half-cycle rate 2*f*V_transfer so the integral over T/2 equals V_transfer; "
    "outlet is pressure-driven."
)


# ============================================================================
# Coupling spec
# ============================================================================


@dataclass(frozen=True)
class CouplingSpec:
    """Physical tank/conduit/roll spec paired with a SweepCase for coupling.

    The SweepCase carries the dimensionless sweep axes (fill fraction, conduit
    capacity, period ratio) and the roll period; this spec carries the dimensional
    geometry needed to build the OpenFOAM domain and to run the reduced-order
    conservation check for the coupled pair of tanks.

    Attributes:
        tank_length: Tank length in the sloshing (x) direction (m, > 0).
        tank_width: Tank transverse width (y) (m, > 0).
        tank_height: Tank internal height (z) (m, > 0); must be consistent with
            the SweepCase ``fill_depth = fill_fraction * tank_height``.
        roll_amplitude_deg: Prescribed roll amplitude (degrees, > 0).
        roll_origin: Roll rotation centre (m), passed to PrescribedMotion.
        conduit: Connecting-conduit geometry/losses (gravity_conduit.ConduitGeometry).
        dest_fill_fraction: Initial fill fraction of the paired destination tank
            (0-1); the SweepCase fill_fraction is the source/primary tank.
        density: Liquid density used for the mass residual (kg/m^3, > 0).
        n_cells: Background block-mesh cell counts (nx, ny, nz); the z count sets
            the free-surface snap resolution for the partial-fill setFieldsDict.
        gravity: Gravitational acceleration for the exchange check (m/s^2).
    """

    tank_length: float
    tank_width: float
    tank_height: float
    roll_amplitude_deg: float
    conduit: ConduitGeometry
    dest_fill_fraction: float
    roll_origin: Vec3 = (0.0, 0.0, 0.0)
    density: float = 1025.0
    n_cells: Tuple[int, int, int] = (40, 8, 40)
    gravity: float = G_STANDARD

    def __post_init__(self) -> None:
        for name in ("tank_length", "tank_width", "tank_height", "roll_amplitude_deg", "density"):
            if getattr(self, name) <= 0.0:
                raise ValueError(f"{name} must be positive, got {getattr(self, name)}")
        if not (0.0 <= self.dest_fill_fraction <= 1.0):
            raise ValueError(
                f"dest_fill_fraction must be in [0, 1], got {self.dest_fill_fraction}"
            )
        if len(self.n_cells) != 3 or any(n < 1 for n in self.n_cells):
            raise ValueError(f"n_cells must be three counts >= 1, got {self.n_cells}")
        if len(self.roll_origin) != 3:
            raise ValueError("roll_origin must be a 3-tuple (x y z)")

    @property
    def plan_area(self) -> float:
        """Horizontal (plan) cross-sectional area (m^2) = length * width."""
        return self.tank_length * self.tank_width

    @property
    def tank_volume(self) -> float:
        """Tank capacity (m^3) = plan_area * tank_height."""
        return self.plan_area * self.tank_height


# ============================================================================
# Verification-gate result + manifest types
# ============================================================================


@dataclass(frozen=True)
class VerificationResult:
    """Verdict of the pre-run coupling verification gate.

    Attributes:
        case_id: Coupled case identifier.
        passed: True only when every check passed.
        transfer_volume_m3: Planned half-cycle transfer volume V_transfer.
        q_peak_m3_s: Peak conduit flow rate Q_peak = pi * f * V_transfer.
        pulse_integral_m3: Half-sine volume integral Q_peak * T / pi (== V_transfer).
        pulse_integral_residual_m3: |pulse_integral - V_transfer| (~0 by identity).
        mass_residual_kg: Max mass drift of the planned exchange (from
            gravity_conduit.simulate_gravity_exchange).
        volume_residual_m3: Total-volume drift of the planned exchange (~0).
        feasible: True if the transfer avoids dry-out and overflow.
        boundary_conditions_ok: True if inlet+outlet BCs and the phase
            convention are present and consistent.
        phase_convention: The recorded motion/flow phase convention string.
        checks: Ordered mapping of check name -> pass/fail.
    """

    case_id: str
    passed: bool
    transfer_volume_m3: float
    q_peak_m3_s: float
    pulse_integral_m3: float
    pulse_integral_residual_m3: float
    mass_residual_kg: float
    volume_residual_m3: float
    feasible: bool
    boundary_conditions_ok: bool
    phase_convention: str
    checks: Dict[str, bool] = field(default_factory=dict)


@dataclass(frozen=True)
class CaseManifest:
    """Small, hashed, de-identified record of a coupled case (not a case tree).

    Attributes:
        schema_version: Manifest schema version.
        case_id: Coupled case identifier.
        inputs: De-identified scalar inputs (fill, geometry, conduit, roll).
        solver: Solver/time-stepping summary.
        fill_level: VOF partial-fill level (fraction of tank height).
        transfer_volume_m3: Planned half-cycle transfer volume.
        q_peak_m3_s: Peak conduit flow rate.
        phase_convention: Recorded motion/flow phase convention.
        gate_passed: Whether the verification gate passed.
        content_hash: 16-hex digest over the canonical inputs + solver + schema.
    """

    schema_version: str
    case_id: str
    inputs: Dict[str, Any]
    solver: Dict[str, Any]
    fill_level: float
    transfer_volume_m3: float
    q_peak_m3_s: float
    phase_convention: str
    gate_passed: bool
    content_hash: str


@dataclass(frozen=True)
class SyntheticCase:
    """Bundle produced by :func:`emit_synthetic_gravity_exchange_case`."""

    case: OpenFOAMCase
    sweep_case: SweepCase
    spec: CouplingSpec
    verification: VerificationResult
    manifest: CaseManifest


# ============================================================================
# Half-sine flow law (independent, solver-free)
# ============================================================================


def transfer_volume(sweep_case: SweepCase, spec: CouplingSpec) -> float:
    """Planned half-cycle transfer volume V_transfer (m^3).

    The dimensionless ``conduit_capacity`` is a fraction of tank volume per
    half-cycle, so ``V_transfer = conduit_capacity * tank_volume``.
    """
    return sweep_case.conduit_capacity * spec.tank_volume


def peak_flow_rate(v_transfer: float, roll_period_s: float) -> float:
    """Peak conduit flow rate Q_peak = pi * f * V_transfer (m^3/s).

    Derived from the half-sine pulse ``Q(t) = Q_peak sin(omega t)`` whose
    half-cycle integral equals ``V_transfer`` (see module docstring).
    """
    if roll_period_s <= 0.0:
        raise ValueError(f"roll_period_s must be positive, got {roll_period_s}")
    f = 1.0 / roll_period_s
    return math.pi * f * v_transfer


def mean_flow_rate(v_transfer: float, roll_period_s: float) -> float:
    """Mean half-cycle conduit flow rate 2 * f * V_transfer (m^3/s).

    This is the constant volumetric rate that, held over one half-cycle
    ``T/2``, transfers exactly ``V_transfer`` (``q_mean * T/2 = V_transfer``).
    Unlike a face velocity, a volumetric flow rate is independent of the inlet
    patch area, which is why the exchange inlet BC uses it.
    """
    if roll_period_s <= 0.0:
        raise ValueError(f"roll_period_s must be positive, got {roll_period_s}")
    return 2.0 * v_transfer / roll_period_s


# ============================================================================
# Mapper: SweepCase -> OpenFOAMCase
# ============================================================================


def map_sweep_case_to_openfoam_case(
    sweep_case: SweepCase, spec: CouplingSpec
) -> OpenFOAMCase:
    """Map one reduced-order SweepCase (+ spec) to a buildable OpenFOAMCase.

    Sets the VOF fill level from the fill fraction, a ROLL prescribed motion from
    the sweep roll period and the spec amplitude, the domain box from the tank
    dimensions, inlet/outlet boundary conditions for the conduit exchange, and
    metadata carrying the case id, conduit capacity, the flow-law scalars, and an
    explicit motion/flow phase convention.

    Args:
        sweep_case: One deterministic sweep row.
        spec: Dimensional tank/conduit/roll spec for the coupled pair.

    Returns:
        A multiphase ``interFoam`` :class:`OpenFOAMCase` ready to build/verify.

    Raises:
        ValueError: If the spec geometry is inconsistent with the sweep case
            (fill depth mismatch) or the fill fraction is out of range.
    """
    if not (0.0 <= sweep_case.fill_fraction <= 1.0):
        raise ValueError(
            f"fill_fraction must be in [0, 1], got {sweep_case.fill_fraction}"
        )
    expected_depth = sweep_case.fill_fraction * spec.tank_height
    if not math.isclose(expected_depth, sweep_case.fill_depth, rel_tol=1e-6, abs_tol=1e-9):
        raise ValueError(
            f"spec tank_height {spec.tank_height} m is inconsistent with the sweep "
            f"case fill_depth {sweep_case.fill_depth} m at fill_fraction "
            f"{sweep_case.fill_fraction} (expected fill_depth {expected_depth} m); "
            "the spec must describe the same tank the sweep was generated for."
        )

    case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, name=sweep_case.case_id)
    case.fill_level = float(sweep_case.fill_fraction)

    case.domain = DomainConfig(
        min_coords=[0.0, 0.0, 0.0],
        max_coords=[spec.tank_length, spec.tank_width, spec.tank_height],
        n_cells=list(spec.n_cells),
    )

    case.motion = PrescribedMotion(
        motion_type=MotionType.ROLL,
        amplitude=spec.roll_amplitude_deg,
        period=sweep_case.roll_period_s,
        origin=tuple(spec.roll_origin),
    )

    v_transfer = transfer_volume(sweep_case, spec)
    q_peak = peak_flow_rate(v_transfer, sweep_case.roll_period_s)
    q_mean = mean_flow_rate(v_transfer, sweep_case.roll_period_s)
    # Diagnostic only: the peak conduit velocity if the conduit area were the
    # inlet face. The inlet BC itself is specified as a VOLUMETRIC flow rate
    # (area-independent) so the flux is correct regardless of the inlet patch
    # area -- imposing q_peak/A_conduit as a face velocity over-fluxed the tank
    # by the ratio of face area to conduit area (#1528 slice-7 defect fix).
    v_peak = q_peak / spec.conduit.area

    case.boundary_conditions = _exchange_boundary_conditions(q_mean)

    case.metadata = {
        "case_id": sweep_case.case_id,
        "fill_fraction": float(sweep_case.fill_fraction),
        "conduit_capacity": float(sweep_case.conduit_capacity),
        "period_ratio": float(sweep_case.period_ratio),
        "near_resonance": bool(sweep_case.near_resonance),
        "roll_period_s": float(sweep_case.roll_period_s),
        "roll_frequency_hz": float(sweep_case.roll_frequency_hz),
        "transfer_volume_m3": float(v_transfer),
        "q_peak_m3_s": float(q_peak),
        "volumetric_flow_rate_m3_s": float(q_mean),
        "mean_half_cycle_rate_m3_s": float(q_mean),
        "peak_conduit_velocity_m_s": float(v_peak),
        "phase_convention": PHASE_CONVENTION,
    }
    logger.info(
        "Mapped coupled case '{}': fill={:.3f}, T_roll={:.4g}s, cap={:.4g}, "
        "V_transfer={:.4g} m^3, Q_peak={:.4g} m^3/s",
        sweep_case.case_id, sweep_case.fill_fraction, sweep_case.roll_period_s,
        sweep_case.conduit_capacity, v_transfer, q_peak,
    )
    return case


def _exchange_boundary_conditions(flow_rate: float) -> list[BoundaryCondition]:
    """Inlet/outlet boundary conditions for the conduit exchange (interFoam).

    The inlet imposes the conduit VOLUMETRIC flow rate via
    ``flowRateInletVelocity`` (OpenFOAM computes the face velocity as
    ``Q / A_patch`` internally, so the flux is correct regardless of the inlet
    patch area); the outlet is pressure-driven. Fields are the standard
    interFoam set (U, p_rgh, alpha.water).
    """
    return [
        BoundaryCondition(
            "inlet", BoundaryType.FLOW_RATE_INLET_VELOCITY, "U",
            value="uniform (0 0 0)",
            extra={"volumetricFlowRate": f"constant {flow_rate:.10g}"},
        ),
        BoundaryCondition("inlet", BoundaryType.ZERO_GRADIENT, "p_rgh"),
        BoundaryCondition(
            "inlet", BoundaryType.INLET_OUTLET, "alpha.water",
            value="uniform 1", extra={"inletValue": "uniform 1"},
        ),
        BoundaryCondition(
            "outlet", BoundaryType.PRESSURE_INLET_OUTLET_VELOCITY, "U",
            value="uniform (0 0 0)",
        ),
        BoundaryCondition(
            "outlet", BoundaryType.TOTAL_PRESSURE, "p_rgh",
            value="uniform 0", extra={"p0": "uniform 0"},
        ),
        BoundaryCondition(
            "outlet", BoundaryType.INLET_OUTLET, "alpha.water",
            value="uniform 0", extra={"inletValue": "uniform 0"},
        ),
    ]


# ============================================================================
# Verification gate
# ============================================================================


def verify_coupling(
    case: OpenFOAMCase, sweep_case: SweepCase, spec: CouplingSpec
) -> VerificationResult:
    """Pre-run verification gate for a coupled case (no CFD required).

    Runs, in order and BEFORE any CFD: (1) the half-sine flow-pulse volume
    integral identity ``Q_peak*T/pi == V_transfer`` and a cross-check that the
    case metadata's recorded flow law is self-consistent (rejects a tampered /
    non-conserving plan); (2) dry-out / overflow feasibility via
    :func:`gravity_conduit.check_transfer_feasibility`; (3) mass & volume
    conservation of the planned exchange via
    :func:`gravity_conduit.simulate_gravity_exchange`; (4) boundary-condition and
    phase-convention presence/consistency.

    Args:
        case: The mapped OpenFOAM case (source of BCs + recorded flow law).
        sweep_case: The originating sweep row.
        spec: The dimensional tank/conduit/roll spec.

    Returns:
        A :class:`VerificationResult` with ``passed=True`` when every check holds.

    Raises:
        ValueError: On any failing check, with an actionable diagnostic (dry-out,
            overflow, non-conserving plan, missing/inconsistent boundary
            conditions or phase convention).
    """
    checks: Dict[str, bool] = {}

    # --- (1) Flow-pulse volume-integral identity + metadata self-consistency ---
    v_transfer = transfer_volume(sweep_case, spec)
    q_peak = peak_flow_rate(v_transfer, sweep_case.roll_period_s)
    pulse_integral = q_peak * sweep_case.roll_period_s / math.pi
    pulse_residual = abs(pulse_integral - v_transfer)
    checks["pulse_integral_identity"] = pulse_residual <= _PULSE_TOL
    if not checks["pulse_integral_identity"]:  # pragma: no cover - identity is exact
        raise ValueError(
            f"flow-pulse integral Q_peak*T/pi={pulse_integral:.6g} m^3 does not "
            f"equal the requested transfer V_transfer={v_transfer:.6g} m^3 "
            f"(residual {pulse_residual:.3g} m^3); the half-sine plan is not "
            "volume-conserving."
        )

    meta = case.metadata
    for key, truth in (("transfer_volume_m3", v_transfer), ("q_peak_m3_s", q_peak)):
        recorded = meta.get(key)
        if recorded is None:
            raise ValueError(
                f"case metadata is missing '{key}'; cannot verify the flow law. "
                "Re-map the case with map_sweep_case_to_openfoam_case."
            )
        if not math.isclose(float(recorded), truth, rel_tol=1e-6, abs_tol=1e-9):
            raise ValueError(
                f"non-conserving plan: case metadata '{key}'={recorded:.6g} does "
                f"not match the half-sine flow law value {truth:.6g} "
                "(Q_peak=pi*f*V_transfer, V_transfer=Q_peak*T/pi); the recorded "
                "pulse integral is inconsistent with the requested transfer."
            )
    checks["metadata_flow_law_consistent"] = True

    # --- (2) Dry-out / overflow feasibility of the requested transfer ---------
    source = TankState(
        volume=sweep_case.fill_fraction * spec.tank_volume,
        plan_area=spec.plan_area,
        height=spec.tank_height,
    )
    dest = TankState(
        volume=spec.dest_fill_fraction * spec.tank_volume,
        plan_area=spec.plan_area,
        height=spec.tank_height,
    )
    check_transfer_feasibility(source, dest, v_transfer)  # raises on dry-out/overflow
    checks["transfer_feasible"] = True

    # --- (3) Mass & volume conservation of the planned exchange ---------------
    duration = max(2.0 * sweep_case.roll_period_s, sweep_case.roll_period_s)
    dt = sweep_case.roll_period_s / 100.0
    exchange = simulate_gravity_exchange(
        source, dest, spec.conduit,
        duration=duration, dt=dt, density=spec.density, g=spec.gravity,
    )
    v0 = exchange.source_volume[0] + exchange.dest_volume[0]
    v1 = exchange.source_volume[-1] + exchange.dest_volume[-1]
    volume_residual = abs(v1 - v0)
    checks["mass_conserved"] = exchange.mass_residual <= _MASS_TOL
    checks["volume_conserved"] = volume_residual <= _PULSE_TOL
    if not checks["mass_conserved"] or not checks["volume_conserved"]:
        raise ValueError(
            f"non-conserving plan: the planned gravity exchange drifts "
            f"(mass_residual={exchange.mass_residual:.3g} kg, "
            f"volume_residual={volume_residual:.3g} m^3); the coupled transfer is "
            "not mass/volume conserving."
        )

    # --- (4) Boundary conditions + phase convention ---------------------------
    patches = {bc.patch_name for bc in case.boundary_conditions}
    missing = {"inlet", "outlet"} - patches
    if missing:
        raise ValueError(
            f"missing boundary condition(s) for patch(es) {sorted(missing)}; the "
            "coupled exchange needs both an 'inlet' and an 'outlet' patch defined."
        )
    fields_by_patch = {
        p: {bc.field for bc in case.boundary_conditions if bc.patch_name == p}
        for p in ("inlet", "outlet")
    }
    for p in ("inlet", "outlet"):
        if "U" not in fields_by_patch[p]:
            raise ValueError(
                f"boundary patch '{p}' has no velocity (U) condition; inlet and "
                "outlet must both define U for the conduit exchange."
            )
    phase = meta.get("phase_convention")
    if not phase:
        raise ValueError(
            "case metadata is missing a non-empty 'phase_convention'; the "
            "motion/flow phase convention must be recorded before a CFD run."
        )
    checks["boundary_conditions_present"] = True
    checks["phase_convention_recorded"] = True

    passed = all(checks.values())
    logger.info(
        "Verification gate for '{}': {} (V_transfer={:.4g} m^3, Q_peak={:.4g} "
        "m^3/s, mass_residual={:.2g} kg)",
        meta.get("case_id", case.name), "PASS" if passed else "FAIL",
        v_transfer, q_peak, exchange.mass_residual,
    )
    return VerificationResult(
        case_id=str(meta.get("case_id", case.name)),
        passed=passed,
        transfer_volume_m3=v_transfer,
        q_peak_m3_s=q_peak,
        pulse_integral_m3=pulse_integral,
        pulse_integral_residual_m3=pulse_residual,
        mass_residual_kg=exchange.mass_residual,
        volume_residual_m3=volume_residual,
        feasible=True,
        boundary_conditions_ok=True,
        phase_convention=str(phase),
        checks=checks,
    )


# ============================================================================
# Case manifest
# ============================================================================


def build_case_manifest(
    case: OpenFOAMCase,
    sweep_case: SweepCase,
    spec: CouplingSpec,
    verification: VerificationResult,
) -> CaseManifest:
    """Build a small, hashed, de-identified manifest for a coupled case.

    The manifest records the scalar inputs, solver summary and flow-law scalars
    plus a deterministic content hash; it intentionally holds no case directory
    tree and no absolute filesystem paths.
    """
    conduit = spec.conduit
    inputs: Dict[str, Any] = {
        "fill_fraction": round(sweep_case.fill_fraction, _ROUND),
        "dest_fill_fraction": round(spec.dest_fill_fraction, _ROUND),
        "conduit_capacity": round(sweep_case.conduit_capacity, _ROUND),
        "period_ratio": round(sweep_case.period_ratio, _ROUND),
        "near_resonance": bool(sweep_case.near_resonance),
        "tank_length_m": round(spec.tank_length, _ROUND),
        "tank_width_m": round(spec.tank_width, _ROUND),
        "tank_height_m": round(spec.tank_height, _ROUND),
        "roll_amplitude_deg": round(spec.roll_amplitude_deg, _ROUND),
        "roll_period_s": round(sweep_case.roll_period_s, _ROUND),
        "roll_origin_m": [round(float(x), _ROUND) for x in spec.roll_origin],
        "conduit_area_m2": round(conduit.area, _ROUND),
        "conduit_discharge_coefficient": round(conduit.discharge_coefficient, _ROUND),
        "conduit_loss_coefficient": round(conduit.loss_coefficient, _ROUND),
        "density_kg_m3": round(spec.density, _ROUND),
        "n_cells": [int(n) for n in spec.n_cells],
        "gravity_m_s2": round(spec.gravity, _ROUND),
    }
    sc = case.solver_config
    solver: Dict[str, Any] = {
        "solver_name": sc.solver_name,
        "is_multiphase": bool(sc.is_multiphase),
        "end_time_s": sc.end_time,
        "delta_t_s": sc.delta_t,
        "write_interval": sc.write_interval,
    }
    content_hash = _content_hash(
        {"schema": COUPLING_SCHEMA_VERSION, "inputs": inputs, "solver": solver}
    )
    return CaseManifest(
        schema_version=COUPLING_SCHEMA_VERSION,
        case_id=case.name,
        inputs=inputs,
        solver=solver,
        fill_level=float(case.fill_level if case.fill_level is not None else 0.0),
        transfer_volume_m3=verification.transfer_volume_m3,
        q_peak_m3_s=verification.q_peak_m3_s,
        phase_convention=verification.phase_convention,
        gate_passed=verification.passed,
        content_hash=content_hash,
    )


def _content_hash(payload: Dict[str, Any]) -> str:
    """16-hex SHA-256 digest over a canonical (sorted, compact) JSON payload."""
    canonical = json.dumps(payload, sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(canonical.encode("utf-8")).hexdigest()[:16]


# ============================================================================
# Synthetic (de-identified) gravity-exchange case
# ============================================================================


def synthetic_coupling_spec() -> CouplingSpec:
    """A de-identified, feasible synthetic tank/conduit/roll spec.

    Generic prismatic ballast tank (no client geometry or names): a 20 x 6 x 10 m
    tank, 5 deg roll, a lossy 0.5 m^2 conduit, destination tank 30% full.
    """
    return CouplingSpec(
        tank_length=20.0,
        tank_width=6.0,
        tank_height=10.0,
        roll_amplitude_deg=5.0,
        roll_origin=(10.0, 3.0, 0.0),
        conduit=ConduitGeometry(
            area=0.5, discharge_coefficient=0.6, loss_coefficient=1.5
        ),
        dest_fill_fraction=0.30,
        density=1025.0,
        n_cells=(40, 8, 40),
    )


def synthetic_sweep_case() -> SweepCase:
    """A de-identified near-resonance sweep row with a non-zero conduit capacity.

    Half-full source tank, conduit capacity 0.08 of tank volume per half-cycle,
    roll period ratio 1.0 (near resonance). The natural period comes from the
    analytical tanh dispersion via the slice-3 sweep helper for provenance.
    """
    from .sloshing_sweep import first_sloshing_natural_period

    length, height, fill = 20.0, 10.0, 0.50
    nat = first_sloshing_natural_period(length, height, fill)
    roll_period = 1.0 * nat.natural_period_s  # period_ratio = 1.0
    return SweepCase(
        case_id="synthetic-grav-exch-1528s4",
        fill_fraction=fill,
        fill_depth=nat.fill_depth,
        conduit_capacity=0.08,
        period_ratio=1.0,
        natural_frequency_hz=nat.natural_frequency_hz,
        natural_period_s=nat.natural_period_s,
        roll_frequency_hz=1.0 / roll_period,
        roll_period_s=roll_period,
        near_resonance=True,
        approximation=nat.approximation,
    )


def emit_synthetic_gravity_exchange_case() -> SyntheticCase:
    """Produce one buildable, gate-passing synthetic coupled case + manifest.

    Returns:
        A :class:`SyntheticCase` bundling the mapped OpenFOAMCase, its sweep row
        and spec, the passing :class:`VerificationResult`, and a hashed
        :class:`CaseManifest` (no raw case tree, no absolute paths).
    """
    spec = synthetic_coupling_spec()
    sweep_case = synthetic_sweep_case()
    case = map_sweep_case_to_openfoam_case(sweep_case, spec)
    verification = verify_coupling(case, sweep_case, spec)
    manifest = build_case_manifest(case, sweep_case, spec, verification)
    return SyntheticCase(
        case=case,
        sweep_case=sweep_case,
        spec=spec,
        verification=verification,
        manifest=manifest,
    )
