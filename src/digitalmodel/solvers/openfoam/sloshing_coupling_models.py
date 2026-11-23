#!/usr/bin/env python3
"""
ABOUTME: Data contracts for the reduced-order sloshing -> vessel-roll coupling
model (dm#643). Holds one swept CFD case, the first-harmonic moment
coefficients derived from it, and the fill-damping, tuning and coupling-strength
report shapes. Separated from the model itself so callers and the analysis
helpers can depend on the contracts without importing the solver logic.

The harmonic sign convention and the meaning of each coefficient are documented
on the model module.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Dict, List


@dataclass
class SloshingCase:
    """First-harmonic result for a single ``{fill x drive frequency}`` CFD case.

    Mirrors exactly the JSON/CSV row schema the dm#641 sweep harness writes.

    Attributes:
        fill_level: Tank fill fraction (0..1).
        drive_period: Imposed roll period T (s).
        drive_freq_hz: Imposed roll frequency (Hz) = 1/T. Metadata; ``omega`` is
            derived from ``drive_period`` to stay self-consistent.
        roll_amplitude_deg: Imposed roll amplitude A (degrees). Coefficients are
            amplitude-dependent and strictly valid near this value.
        moment_amplitude: First-harmonic moment amplitude |M1| (N.m). Metadata.
        moment_phase_rad: First-harmonic moment phase vs imposed roll (rad).
            Metadata.
        in_phase_coeff: Reactive coefficient [N.m/rad] (in phase with -theta).
        quad_coeff: Dissipative coefficient [N.m/(rad/s)] (in phase with -theta_dot).
    """

    fill_level: float
    drive_period: float
    drive_freq_hz: float = 0.0
    roll_amplitude_deg: float = 0.0
    moment_amplitude: float = 0.0
    moment_phase_rad: float = 0.0
    in_phase_coeff: float = 0.0
    quad_coeff: float = 0.0

    def __post_init__(self) -> None:
        if self.drive_period <= 0.0:
            raise ValueError(
                f"drive_period must be > 0, got {self.drive_period}"
            )
        if not (0.0 <= self.fill_level <= 1.0):
            raise ValueError(
                f"fill_level must be in [0, 1], got {self.fill_level}"
            )

    @property
    def omega(self) -> float:
        """Imposed circular frequency (rad/s), derived from ``drive_period``."""
        return 2.0 * math.pi / self.drive_period

    @property
    def roll_amplitude_rad(self) -> float:
        """Imposed roll amplitude in radians."""
        return math.radians(self.roll_amplitude_deg)

    @classmethod
    def from_row(cls, row: Dict[str, object]) -> "SloshingCase":
        """Build from a manifest dict, tolerating missing optional keys.

        String values (as read from CSV) are coerced to float.
        """

        def _f(key: str, default: float = 0.0) -> float:
            val = row.get(key, default)
            if val is None or val == "":
                return default
            return float(val)  # type: ignore[arg-type]

        period = _f("drive_period")
        freq = _f("drive_freq_hz")
        # Reconcile period/frequency if only one is supplied.
        if period <= 0.0 and freq > 0.0:
            period = 1.0 / freq
        if freq <= 0.0 and period > 0.0:
            freq = 1.0 / period

        return cls(
            fill_level=_f("fill_level"),
            drive_period=period,
            drive_freq_hz=freq,
            roll_amplitude_deg=_f("roll_amplitude_deg"),
            moment_amplitude=_f("moment_amplitude"),
            moment_phase_rad=_f("moment_phase_rad"),
            in_phase_coeff=_f("in_phase_coeff"),
            quad_coeff=_f("quad_coeff"),
        )


# ---------------------------------------------------------------------------
# Interpolated coefficient sample
# ---------------------------------------------------------------------------


@dataclass
class MomentCoefficients:
    """Interpolated sloshing roll-moment coefficients at one (omega, fill).

    Attributes:
        omega: Circular frequency (rad/s) the sample was evaluated at.
        fill_level: Fill fraction the sample was evaluated at.
        in_phase_coeff: Reactive coefficient [N.m/rad].
        quad_coeff: Dissipative (damping) coefficient [N.m/(rad/s)].
        clamped: True if ``omega`` (or fill) was clamped into the swept range.
    """

    omega: float
    fill_level: float
    in_phase_coeff: float
    quad_coeff: float
    clamped: bool = False

    @property
    def added_roll_inertia(self) -> float:
        """Added roll inertia A44 [N.m.s^2/rad] = -in_phase_coeff / omega^2.

        From ``M_reactive = -A44 * theta_ddot`` with ``theta_ddot = -omega^2 theta``.
        A restoring (positive ``in_phase_coeff``) reactive moment therefore reads
        as NEGATIVE added inertia -- expected for a below/above-resonance TLD.
        """
        if self.omega == 0.0:
            return 0.0
        return -self.in_phase_coeff / (self.omega * self.omega)

    @property
    def added_roll_stiffness(self) -> float:
        """Added roll stiffness K44 [N.m/rad] = in_phase_coeff.

        Equivalent reactive representation of the in-phase moment as a spring.
        """
        return self.in_phase_coeff

    @property
    def added_roll_damping(self) -> float:
        """Added roll damping B44 [N.m/(rad/s)] = quad_coeff."""
        return self.quad_coeff

    def moment(self, theta: float, theta_dot: float) -> float:
        """Sloshing roll moment (N.m) for roll ``theta`` (rad) and rate.

        ``M = -in_phase_coeff*theta - quad_coeff*theta_dot`` (positive opposes).
        """
        return -self.in_phase_coeff * theta - self.quad_coeff * theta_dot


# ---------------------------------------------------------------------------
# Tuning / escalation reports
# ---------------------------------------------------------------------------


@dataclass
class FillDampingResult:
    """Anti-roll damping available from one fill level at a target frequency."""

    fill_level: float
    omega: float
    quad_coeff: float          # N.m/(rad/s) -- the anti-roll (damping) coefficient
    in_phase_coeff: float      # N.m/rad
    clamped: bool = False


@dataclass
class TuningReport:
    """Result of the anti-roll fill-tuning search near the roll natural freq."""

    omega_roll: float
    natural_period_s: float
    per_fill: List[FillDampingResult]
    best_fill: float
    best_quad_coeff: float

    def summary(self) -> str:
        rows = ", ".join(
            f"{r.fill_level:.2f}->{r.quad_coeff:.3g}" for r in self.per_fill
        )
        return (
            f"Near T={self.natural_period_s:.1f}s "
            f"(omega={self.omega_roll:.4f} rad/s): best anti-roll fill = "
            f"{self.best_fill:.2f} (quad_coeff={self.best_quad_coeff:.3g} "
            f"N.m/(rad/s)). Per-fill damping: {rows}."
        )


@dataclass
class CouplingStrengthReport:
    """Ratio of the sloshing moment to the vessel roll reference moment.

    Phase A is one-way. If ``ratio`` exceeds ``threshold`` the sloshing moment is
    no longer a small perturbation on the vessel roll balance, and weak two-way
    (tank<->vessel) iteration should be considered.
    """

    slosh_moment_amplitude: float   # N.m
    reference_moment: float         # N.m (restoring or exciting)
    ratio: float
    threshold: float
    escalate: bool

    def summary(self) -> str:
        verdict = (
            "ESCALATE to weak two-way coupling"
            if self.escalate
            else "one-way (Phase A) coupling OK"
        )
        return (
            f"|M_slosh|={self.slosh_moment_amplitude:.3g} N.m vs reference "
            f"{self.reference_moment:.3g} N.m -> ratio={self.ratio:.3f} "
            f"(threshold {self.threshold:.2f}): {verdict}."
        )


# ---------------------------------------------------------------------------
# The reduced-order coupling model
# ---------------------------------------------------------------------------



__all__ = [
    "SloshingCase",
    "MomentCoefficients",
    "FillDampingResult",
    "TuningReport",
    "CouplingStrengthReport",
]


@dataclass(frozen=True, kw_only=True)
class NormalizedRollCoefficients:
    """Explicit normalized tank coefficients for one harmonic roll condition.

    The caller attests to ``normalization='per_roll_angle_and_rate'``:
    ``M_tank = -stiffness*theta - damping*theta_dot``, with ``stiffness_unit``
    exactly ``N.m/rad`` and ``damping_unit`` exactly ``N.m/(rad/s)``. Raw
    harmonic moment components must first be divided by the imposed angle
    amplitude or angular-rate amplitude, respectively, with signs reconciled
    to this convention. No raw-row conversion is provided by this contract.

    ``omega_rad_s`` is the frequency at which these coefficients were evaluated,
    possibly by an external interpolation within ``frequency_support_rad_s``.
    The inclusive support intervals declare applicability, not an interpolation
    law. ``reference_amplitude_rad`` records the imposed roll amplitude used to
    derive the coefficients. ``amplitude_support_rad`` must be justified by the
    caller; a single calibrated amplitude can be represented by equal bounds.
    ``condition`` identifies the tank configuration, fill and coefficient source.
    This contract is validated when passed to ``coupled_roll_response``.
    """

    stiffness: float
    damping: float
    omega_rad_s: float
    reference_amplitude_rad: float
    frequency_support_rad_s: tuple[float, float]
    amplitude_support_rad: tuple[float, float]
    condition: str
    normalization: str
    stiffness_unit: str
    damping_unit: str


@dataclass(frozen=True, kw_only=True)
class CoupledRollResponse:
    """Harmonic roll solution with retained inputs and coefficient provenance.

    Complex responses and their magnitudes are in radians. A positive
    ``reduction_percent`` denotes lower coupled amplitude; a negative value
    denotes amplification. The immutable coefficient record retains condition,
    normalization, units, reference amplitude and declared support. The support
    status refers to the computed coupled amplitude and evaluated frequency.
    The baseline amplitude does not need to lie in tank coefficient support.
    """

    baseline_response_rad: complex
    coupled_response_rad: complex
    baseline_amplitude_rad: float
    coupled_amplitude_rad: float
    reduction_percent: float
    inertia_kg_m2: float
    restoring_nm_per_rad: float
    damping_nm_s_per_rad: float
    omega_rad_s: float
    excitation_moment_nm: complex
    coefficients: NormalizedRollCoefficients
    support_status: str = "within_declared_support"
    amplitude_basis: str = "computed_with_fixed_coefficients"
