#!/usr/bin/env python3
"""
ABOUTME: Reduced-order sloshing -> vessel-roll coupling model for ballast-tank
studies. Ingests the dm#641 forced-roll CFD sweep (per {fill x drive
frequency} first-harmonic moment coefficients) and returns the ballast-tank
sloshing roll-moment as frequency-dependent added roll inertia + added roll
damping, plus a time-domain
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

Reference: digitalmodel #643 (reduced-order sloshing->roll coupling).
"""


from __future__ import annotations

import csv
import json
from pathlib import Path
from typing import Callable, Dict, List, Optional, Sequence, Tuple, Union

import numpy as np
from numpy.typing import NDArray

from loguru import logger

from . import sloshing_coupling_analysis as _analysis
from .sloshing_coupling_models import (
    CouplingStrengthReport,
    FillDampingResult,
    MomentCoefficients,
    SloshingCase,
    TuningReport,
)

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


    # ---- derived analyses (implemented in sloshing_coupling_analysis) ----- #

    def moment_time_series(
        self,
        times: NDArray[np.float64],
        theta: NDArray[np.float64],
        omega: float,
        fill_level: float,
        theta_dot: Optional[NDArray[np.float64]] = None,
    ) -> NDArray[np.float64]:
        """Sloshing roll-moment time series to add into a vessel roll EOM."""
        return _analysis.moment_time_series(
            self, times, theta, omega, fill_level, theta_dot
        )

    def moment_from_harmonic(
        self,
        amplitude_deg: float,
        omega: float,
        fill_level: float,
        times: NDArray[np.float64],
    ) -> NDArray[np.float64]:
        """Moment series for a synthetic single-frequency roll."""
        return _analysis.moment_from_harmonic(
            self, amplitude_deg, omega, fill_level, times
        )

    def best_antiroll_fill(
        self,
        natural_period_s: Optional[float] = None,
        omega_roll: Optional[float] = None,
    ) -> TuningReport:
        """Fill level whose sloshing damping best opposes the vessel roll."""
        return _analysis.best_antiroll_fill(self, natural_period_s, omega_roll)

    def coupling_strength(
        self,
        amplitude_deg: float,
        omega: float,
        fill_level: float,
        reference_moment: Optional[float] = None,
        restoring_stiffness: Optional[float] = None,
        threshold: float = 0.15,
    ) -> CouplingStrengthReport:
        """Ratio of the sloshing moment to the vessel roll reference moment."""
        return _analysis.coupling_strength(
            self,
            amplitude_deg,
            omega,
            fill_level,
            reference_moment,
            restoring_stiffness,
            threshold,
        )


__all__ = [
    "SloshingCase",
    "MomentCoefficients",
    "FillDampingResult",
    "TuningReport",
    "CouplingStrengthReport",
    "SloshingCouplingModel",
]
