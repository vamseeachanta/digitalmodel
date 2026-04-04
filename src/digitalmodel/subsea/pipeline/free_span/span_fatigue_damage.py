"""
DNV RP F105 Section 7.3 + DNV-RP-C203 — Fatigue damage accumulation.

Uses the built-in bilinear S-N curve from ``_bilinear_sn`` which provides
the complete DNV-RP-C203 Table 2-1 (14 weld classes × 2 environments).
When ``digitalmodel.structural.fatigue`` is available it is used instead.

DNV-RP-C203 F-class parameters (in air, first slope):
    A1 = 1.73×10¹¹, m1 = 3.0, CAFL = 36.84 MPa
    Second slope: A2 = 6.33×10¹⁴, m2 = 5.0 (N > 10⁷)

Seawater with CP:
    A1 = 8.51×10¹⁰, m1 = 3.0 (reduced)
    A2 = 1.76×10¹⁴, m2 = 5.0
    No fatigue limit (CAFL = 0) per DNV-RP-C203 Sec 2.4.4

Palmgren-Miner rule (F105 Eq 7.3-1):
    D_annual = f_n × T_year / N(Δσ)
    where N(Δσ) from the bilinear S-N curve

Fatigue life = 1 / D_annual  [years]
"""
from __future__ import annotations

import math

from .models import EnvironmentType, PipeSpanInput

# seconds per year (365.25 days)
_SECONDS_PER_YEAR: float = 3.15576e7


class SpanFatigueDamage:
    """Palmgren-Miner fatigue damage per DNV RP F105 Sec 7.3 + DNV-RP-C203.

    Parameters
    ----------
    inp         PipeSpanInput (carries sn_curve_class and environment)
    fn          Governing natural frequency [Hz] (use CF fn for VIV fatigue)
    stress_mpa  Stress range Δσ per cycle [MPa] (= 2 × peak for fully-reversed VIV)
    """

    def __init__(
        self,
        inp: PipeSpanInput,
        fn: float,
        stress_mpa: float,
    ) -> None:
        self._inp = inp
        self._fn = fn
        self._stress_mpa = stress_mpa
        self._curve = self._load_sn_curve()

    # ------------------------------------------------------------------
    # S-N curve loading — built-in bilinear with structural.fatigue fallback
    # ------------------------------------------------------------------

    def _load_sn_curve(self):
        """Load the DNV-RP-C203 S-N curve.

        Strategy:
        1. Try ``digitalmodel.structural.fatigue.sn_curves.get_dnv_curve``
           (returns a PowerLawSNCurve — single slope).
        2. Fall back to the built-in ``_bilinear_sn.get_sn_curve``
           (returns a BilinearSNCurve — two slopes, full DNV-RP-C203 table).

        Both expose ``.get_allowable_cycles(stress_mpa) -> float`` and
        ``.fatigue_limit``, so downstream code is unchanged.
        """
        try:
            from digitalmodel.structural.fatigue.sn_curves import get_dnv_curve
            return get_dnv_curve(self._inp.sn_curve_class)
        except (ImportError, ModuleNotFoundError):
            pass

        # Built-in bilinear fallback
        from ._bilinear_sn import get_sn_curve

        env_str = (
            "seawater_cp"
            if self._inp.environment == EnvironmentType.SEAWATER_CP
            else "air"
        )
        return get_sn_curve(
            curve_class=self._inp.sn_curve_class,
            environment=env_str,
        )

    # ------------------------------------------------------------------
    # Allowable cycles N(S)
    # ------------------------------------------------------------------

    def allowable_cycles(self, stress_mpa: float) -> float:
        """Allowable cycles N from the S-N curve.

        The environment correction is already baked into the curve selection
        (separate "air" and "seawater_cp" parameter sets in DNV-RP-C203).

        Returns ``math.inf`` when *stress_mpa* ≤ CAFL.
        """
        N = self._curve.get_allowable_cycles(stress_mpa)
        if math.isinf(N):
            return math.inf
        return N

    # ------------------------------------------------------------------
    # Annual damage and life
    # ------------------------------------------------------------------

    def damage_per_year(self) -> float:
        """Annual Miner fatigue damage  D = f_n × T_year / N(Δσ).

        Returns 0.0 when stress is below the S-N fatigue limit.
        """
        N = self.allowable_cycles(self._stress_mpa)
        if not math.isfinite(N) or N <= 0.0:
            return 0.0
        cycles_per_year = self._fn * _SECONDS_PER_YEAR
        return cycles_per_year / N

    def fatigue_life_years(self) -> float:
        """Fatigue life = 1 / D_annual  [years].

        Returns math.inf when stress is below the CAFL.
        """
        D = self.damage_per_year()
        if D <= 0.0:
            return math.inf
        return 1.0 / D
