#!/usr/bin/env python3
"""
ABOUTME: Reduced-order sloshing -> vessel-roll coupling model for B1546. Ingests
the dm#641 forced-roll CFD sweep (per {fill x drive frequency} first-harmonic
moment coefficients) and returns the ballast-tank sloshing roll-moment as
frequency-dependent added roll inertia + added roll damping, plus a time-domain
moment feed for a vessel roll equation of motion / OrcaWave hand-off.

Phase A is ONE-WAY coupling: the tank reaction moment is treated as an external
contribution added to the vessel roll model, NOT full two-way FSI. A helper
(:meth:`SloshingCouplingModel.coupling_strength`) reports the ratio of the
sloshing moment to the vessel roll restoring/exciting moment so a caller can
detect when the one-way assumption breaks down and weak two-way iteration is
warranted.

Harmonic contract (produced by the dm#641 sweep)
------------------------------------------------
Each swept CFD case imposes a single-frequency roll ``theta(t) = A*sin(omega*t)``
(A = roll amplitude, radians) on a partially filled ballast tank and measures the
tank roll-reaction moment. Its first harmonic is decomposed against the imposed
motion into two coefficients (sign convention: positive OPPOSES the motion):

    M_slosh(t) = -in_phase_coeff * theta(t)  -  quad_coeff * theta_dot(t)

* ``in_phase_coeff`` [N.m/rad]      -- component in phase with ``-theta``
      (reactive: added-inertia / stiffness-like).
* ``quad_coeff``     [N.m/(rad/s)]  -- component in phase with ``-theta_dot``
      (dissipative: damping-like -- this is the anti-roll / TLD action).

Because ``theta_ddot = -omega^2 * theta`` for a sinusoid, the reactive in-phase
part maps to an added roll inertia ``A44(omega) = -in_phase_coeff / omega^2``
(via ``M_reactive = -A44 * theta_ddot``) or, equivalently, an added roll
stiffness ``K44 = in_phase_coeff``. The added roll damping is simply
``B44(omega) = quad_coeff``.

Sloshing is nonlinear, so these coefficients are strictly valid at the swept
roll amplitude; they are interpolated in ``omega`` (linear, per fill) and across
fill level (linear). Out-of-range ``omega`` is clamped to the swept band with a
warning.

Reference: digitalmodel #643 (reduced-order sloshing->roll coupling, ACMA B1546).
"""

from __future__ import annotations

import csv
import json
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Dict, List, Optional, Sequence, Tuple, Union

import numpy as np
from numpy.typing import NDArray

from loguru import logger


# ---------------------------------------------------------------------------
# One swept CFD case (the dm#641 input contract row)
# ---------------------------------------------------------------------------


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


class SloshingCouplingModel:
    """Reduced-order tank-sloshing roll-moment model built from a CFD sweep.

    Consumes the dm#641 forced-roll sweep (a list of :class:`SloshingCase`) and
    provides frequency- and fill-dependent added roll inertia + damping, a
    time-domain moment feed for a vessel roll EOM / OrcaWave hand-off, an
    anti-roll fill-tuning helper, and a one-way-coupling escalation check.

    Interpolation:
        * ``omega``: linear per fill level (``numpy.interp``), clamped to the
          swept band at the ends (a warning is logged on clamp).
        * fill level: linear across the (sorted, unique) swept fills, clamped to
          the nearest swept fill at the ends (warning on clamp).

    Example::

        model = SloshingCouplingModel.from_sweep_manifest("sweep.json")
        coeffs = model.moment_coefficients(omega=0.35, fill_level=0.5)
        B44 = coeffs.added_roll_damping
        Mfn = model.moment_callable(fill_level=0.5)        # M(theta, theta_dot, omega)
        m_t = model.moment_time_series(times, theta, omega=0.35, fill_level=0.5)
        tune = model.best_antiroll_fill(natural_period_s=18.0)
    """

    def __init__(self, cases: Sequence[SloshingCase]) -> None:
        if not cases:
            raise ValueError("SloshingCouplingModel requires at least one case")
        self._cases: List[SloshingCase] = list(cases)
        # Group by fill level -> arrays sorted by omega.
        self._by_fill: Dict[float, Dict[str, NDArray[np.float64]]] = {}
        fills = sorted({round(c.fill_level, 6) for c in self._cases})
        for fl in fills:
            rows = sorted(
                (c for c in self._cases if round(c.fill_level, 6) == fl),
                key=lambda c: c.omega,
            )
            self._by_fill[fl] = {
                "omega": np.array([r.omega for r in rows], dtype=np.float64),
                "in_phase": np.array(
                    [r.in_phase_coeff for r in rows], dtype=np.float64
                ),
                "quad": np.array([r.quad_coeff for r in rows], dtype=np.float64),
            }
        self._fills: NDArray[np.float64] = np.array(fills, dtype=np.float64)

    # ---- constructors ---------------------------------------------------- #

    @classmethod
    def from_cases(cls, cases: Sequence[SloshingCase]) -> "SloshingCouplingModel":
        return cls(cases)

    @classmethod
    def from_rows(
        cls, rows: Sequence[Dict[str, object]]
    ) -> "SloshingCouplingModel":
        """Build from a list of plain manifest dicts."""
        return cls([SloshingCase.from_row(r) for r in rows])

    @classmethod
    def from_sweep_manifest(
        cls, path: Union[str, Path]
    ) -> "SloshingCouplingModel":
        """Load the dm#641 sweep manifest (``.json`` or ``.csv``).

        JSON may be either a top-level list of rows or an object with a
        ``"points"`` (as written by the dm#641 harness), ``"cases"``, or
        ``"rows"`` list. CSV must have a header row whose names match the
        contract keys.

        Args:
            path: Path to the manifest written by the dm#641 harness.

        Returns:
            A ready-to-query :class:`SloshingCouplingModel`.

        Raises:
            FileNotFoundError: If ``path`` does not exist.
            ValueError: If no rows can be parsed.
        """
        path = Path(path)
        if not path.exists():
            raise FileNotFoundError(f"Sweep manifest not found: {path}")

        rows: List[Dict[str, object]]
        if path.suffix.lower() == ".csv":
            with path.open(newline="") as fh:
                rows = list(csv.DictReader(fh))
        else:
            data = json.loads(path.read_text())
            if isinstance(data, dict):
                rows = (
                    data.get("points")
                    or data.get("cases")
                    or data.get("rows")
                    or []
                )
            else:
                rows = data
        if not rows:
            raise ValueError(f"No cases found in sweep manifest: {path}")
        return cls.from_rows(rows)

    # ---- introspection --------------------------------------------------- #

    @property
    def fill_levels(self) -> List[float]:
        """Sorted, unique swept fill levels."""
        return [float(f) for f in self._fills]

    @property
    def n_cases(self) -> int:
        return len(self._cases)

    def omega_range(self, fill_level: Optional[float] = None) -> Tuple[float, float]:
        """(min, max) swept omega -- overall, or for one fill if given."""
        if fill_level is None:
            allw = np.concatenate([d["omega"] for d in self._by_fill.values()])
            return float(allw.min()), float(allw.max())
        fl = self._nearest_fill(fill_level)
        w = self._by_fill[fl]["omega"]
        return float(w.min()), float(w.max())

    # ---- interpolation core --------------------------------------------- #

    def _nearest_fill(self, fill_level: float) -> float:
        idx = int(np.argmin(np.abs(self._fills - fill_level)))
        return float(self._fills[idx])

    def _interp_fill(
        self, fill_omega: Dict[str, NDArray[np.float64]], omega: float
    ) -> Tuple[float, float, bool]:
        """Linear-in-omega interpolation of (in_phase, quad) for one fill."""
        w = fill_omega["omega"]
        clamped = False
        if omega < w[0] or omega > w[-1]:
            clamped = True
        # np.interp clamps to endpoints outside range (desired behaviour).
        ip = float(np.interp(omega, w, fill_omega["in_phase"]))
        qd = float(np.interp(omega, w, fill_omega["quad"]))
        return ip, qd, clamped

    def moment_coefficients(
        self, omega: float, fill_level: float
    ) -> MomentCoefficients:
        """Interpolated ``(in_phase_coeff, quad_coeff)`` at ``(omega, fill)``.

        Linear in ``omega`` per fill, then linear across fill level. Out-of-range
        ``omega`` or fill is clamped to the swept range with a logged warning.
        """
        if omega <= 0.0:
            raise ValueError(f"omega must be > 0, got {omega}")

        clamped = False

        # --- fill clamp / bracket ---
        fills = self._fills
        fl = fill_level
        if fl < fills[0] or fl > fills[-1]:
            clamped = True
            logger.warning(
                f"fill_level {fill_level:.3f} outside swept range "
                f"[{fills[0]:.3f}, {fills[-1]:.3f}]; clamping."
            )
            fl = min(max(fl, float(fills[0])), float(fills[-1]))

        # Per-fill omega interpolation, then linear across fill.
        ip_per: List[float] = []
        qd_per: List[float] = []
        omega_clamped = False
        for f in fills:
            ip, qd, w_clamped = self._interp_fill(self._by_fill[float(f)], omega)
            ip_per.append(ip)
            qd_per.append(qd)
            omega_clamped = omega_clamped or w_clamped

        if omega_clamped:
            clamped = True
            lo, hi = self.omega_range()
            logger.warning(
                f"omega {omega:.4f} rad/s outside swept band "
                f"[{lo:.4f}, {hi:.4f}]; clamping to nearest swept frequency."
            )

        if len(fills) == 1:
            ip_val, qd_val = ip_per[0], qd_per[0]
        else:
            ip_val = float(np.interp(fl, fills, np.array(ip_per)))
            qd_val = float(np.interp(fl, fills, np.array(qd_per)))

        return MomentCoefficients(
            omega=omega,
            fill_level=fill_level,
            in_phase_coeff=ip_val,
            quad_coeff=qd_val,
            clamped=clamped,
        )

    # ---- added inertia / damping convenience ----------------------------- #

    def added_roll_inertia(self, omega: float, fill_level: float) -> float:
        """Added roll inertia A44 [N.m.s^2/rad] at ``(omega, fill)``."""
        return self.moment_coefficients(omega, fill_level).added_roll_inertia

    def added_roll_damping(self, omega: float, fill_level: float) -> float:
        """Added roll damping B44 [N.m/(rad/s)] at ``(omega, fill)``."""
        return self.moment_coefficients(omega, fill_level).added_roll_damping

    # ---- callable moment ------------------------------------------------- #

    def sloshing_moment(
        self, theta: float, theta_dot: float, omega: float, fill_level: float
    ) -> float:
        """Sloshing roll moment (N.m) for ``theta`` (rad), rate, ``omega``, fill.

        ``M = -in_phase_coeff*theta - quad_coeff*theta_dot`` (positive opposes
        the roll). Coefficients are looked up at ``(omega, fill_level)``.
        """
        c = self.moment_coefficients(omega, fill_level)
        return c.moment(theta, theta_dot)

    def moment_callable(
        self, fill_level: float
    ) -> Callable[[float, float, float], float]:
        """Return ``M_slosh(theta, theta_dot, omega)`` bound to a fill level.

        Convenient hand-off to a time-domain roll integrator: it needs only the
        instantaneous roll state and the (dominant) roll frequency to evaluate
        the added-inertia + damping moment.
        """

        def _m(theta: float, theta_dot: float, omega: float) -> float:
            return self.sloshing_moment(theta, theta_dot, omega, fill_level)

        return _m

    # ---- time-domain feed ------------------------------------------------ #

    def moment_time_series(
        self,
        times: NDArray[np.float64],
        theta: NDArray[np.float64],
        omega: float,
        fill_level: float,
        theta_dot: Optional[NDArray[np.float64]] = None,
    ) -> NDArray[np.float64]:
        """Sloshing roll-moment time series to add into a vessel roll EOM.

        Given a roll time history ``theta(t)`` (rad) this returns
        ``M_slosh(t) = -in_phase*theta - quad*theta_dot`` sampled at ``times``,
        using the coefficients interpolated at ``(omega, fill_level)``. If
        ``theta_dot`` is not supplied it is estimated with ``numpy.gradient``.

        This is the Phase-A one-way coupling contract: add the returned series to
        the RHS of the vessel roll equation of motion (or feed it as an external
        roll moment to OrcaWave / the time-domain solver).

        Args:
            times: Time array (s), shape (N,).
            theta: Roll angle history (rad), shape (N,).
            omega: Dominant roll circular frequency (rad/s) for coefficient lookup.
            fill_level: Tank fill fraction.
            theta_dot: Optional roll-rate history (rad/s); derived if omitted.

        Returns:
            Sloshing roll moment (N.m), shape (N,).
        """
        times = np.asarray(times, dtype=np.float64)
        theta = np.asarray(theta, dtype=np.float64)
        if times.shape != theta.shape:
            raise ValueError("times and theta must have the same shape")
        if theta_dot is None:
            theta_dot = np.gradient(theta, times)
        else:
            theta_dot = np.asarray(theta_dot, dtype=np.float64)
        c = self.moment_coefficients(omega, fill_level)
        return -c.in_phase_coeff * theta - c.quad_coeff * theta_dot

    def moment_from_harmonic(
        self,
        amplitude_deg: float,
        omega: float,
        fill_level: float,
        times: NDArray[np.float64],
    ) -> NDArray[np.float64]:
        """Moment series for a synthetic single-frequency roll ``A*sin(omega t)``.

        Builds ``theta(t) = A*sin(omega t)`` (A from ``amplitude_deg``) and its
        analytic rate ``theta_dot = A*omega*cos(omega t)`` then returns the
        sloshing moment. Useful for RAO-style checks and the OrcaWave hand-off.
        """
        times = np.asarray(times, dtype=np.float64)
        a = math.radians(amplitude_deg)
        theta = a * np.sin(omega * times)
        theta_dot = a * omega * np.cos(omega * times)
        return self.moment_time_series(
            times, theta, omega, fill_level, theta_dot=theta_dot
        )

    # ---- anti-roll tuning ----------------------------------------------- #

    def best_antiroll_fill(
        self,
        natural_period_s: Optional[float] = None,
        omega_roll: Optional[float] = None,
    ) -> TuningReport:
        """Which swept fill best damps roll near the roll natural frequency.

        The core design question for the reverse anti-roll tank: at the vessel
        roll natural frequency, the fill with the largest ``quad_coeff`` (added
        damping) provides the most anti-roll action. Supply either the roll
        natural period (s) or ``omega_roll`` (rad/s).

        Returns:
            A :class:`TuningReport` with per-fill damping and the best fill.
        """
        if omega_roll is None:
            if natural_period_s is None or natural_period_s <= 0.0:
                raise ValueError(
                    "Provide natural_period_s > 0 or omega_roll > 0"
                )
            omega_roll = 2.0 * math.pi / natural_period_s
        if natural_period_s is None:
            natural_period_s = 2.0 * math.pi / omega_roll

        per_fill: List[FillDampingResult] = []
        for f in self._fills:
            c = self.moment_coefficients(omega_roll, float(f))
            per_fill.append(
                FillDampingResult(
                    fill_level=float(f),
                    omega=omega_roll,
                    quad_coeff=c.quad_coeff,
                    in_phase_coeff=c.in_phase_coeff,
                    clamped=c.clamped,
                )
            )
        best = max(per_fill, key=lambda r: r.quad_coeff)
        return TuningReport(
            omega_roll=omega_roll,
            natural_period_s=natural_period_s,
            per_fill=per_fill,
            best_fill=best.fill_level,
            best_quad_coeff=best.quad_coeff,
        )

    # ---- one-way-coupling escalation check ------------------------------ #

    def coupling_strength(
        self,
        amplitude_deg: float,
        omega: float,
        fill_level: float,
        reference_moment: Optional[float] = None,
        restoring_stiffness: Optional[float] = None,
        threshold: float = 0.15,
    ) -> CouplingStrengthReport:
        """Ratio of the sloshing moment to a vessel roll reference moment.

        The Phase-A one-way assumption holds only while the sloshing moment is a
        small fraction of the vessel roll balance. This estimates the sloshing
        first-harmonic moment amplitude at ``(amplitude_deg, omega, fill)`` and
        compares it to a reference roll moment. Provide EITHER an explicit
        ``reference_moment`` (N.m, e.g. the wave roll-exciting moment amplitude)
        OR a roll ``restoring_stiffness`` C44 [N.m/rad] -- the reference is then
        ``C44 * A`` (the hydrostatic restoring moment at that roll amplitude).

        Args:
            amplitude_deg: Roll amplitude A (degrees).
            omega: Roll circular frequency (rad/s).
            fill_level: Tank fill fraction.
            reference_moment: Explicit reference roll moment (N.m).
            restoring_stiffness: Roll restoring stiffness C44 [N.m/rad], used to
                build the reference as ``C44 * A`` if ``reference_moment`` is None.
            threshold: Ratio above which two-way iteration is recommended.

        Returns:
            A :class:`CouplingStrengthReport` with the ratio and escalate flag.
        """
        a = math.radians(amplitude_deg)
        c = self.moment_coefficients(omega, fill_level)
        # First-harmonic amplitude of M = -in_phase*A sin - quad*A omega cos.
        m_amp = a * math.hypot(c.in_phase_coeff, c.quad_coeff * omega)

        if reference_moment is None:
            if restoring_stiffness is None or restoring_stiffness <= 0.0:
                raise ValueError(
                    "Provide reference_moment or restoring_stiffness > 0"
                )
            reference_moment = restoring_stiffness * a
        if reference_moment <= 0.0:
            raise ValueError("reference_moment must be > 0")

        ratio = m_amp / reference_moment
        return CouplingStrengthReport(
            slosh_moment_amplitude=m_amp,
            reference_moment=reference_moment,
            ratio=ratio,
            threshold=threshold,
            escalate=ratio > threshold,
        )


__all__ = [
    "SloshingCase",
    "MomentCoefficients",
    "FillDampingResult",
    "TuningReport",
    "CouplingStrengthReport",
    "SloshingCouplingModel",
]
