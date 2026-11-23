"""
DNV RP F105 Section 7.3 + DNV-RP-C203 — Fatigue damage accumulation.

Uses the bilinear S-N curve from ``_bilinear_sn``, whose parameters come from
``digitalmodel.fatigue.c203_sn_tables``: DNV-RP-C203 (2011) Tables 2-1 (in air,
knee at 1e7 cycles) and 2-2 (seawater with CP, knee at 1e6 cycles), 14 classes
(#2165).

DNV-RP-C203 F class, for example:
    in air:           log a1 = 11.855, m1 = 3.0; log a2 = 15.091, m2 = 5.0
    seawater with CP: log a1 = 11.455, m1 = 3.0; the same second segment
    fatigue limit at 1e7 cycles: 41.52 MPa (the same in both tables)

Each environment uses its own curve and nothing else (#2165, PR #2195 review
r1 finding 3):

* seawater with CP: the full bilinear curve, m2 = 5 below the 1e6 knee, with
  no cut-off; damage below the in-air limit is counted.
* in air: screening convention for the single constant-amplitude VIV stress
  range: below the air curve's own tabulated fatigue limit (41.52 MPa for F,
  equal to its 1e7 knee stress) the damage is taken as zero. The cut-off is
  the curve's own ``fatigue_limit`` and is never carried to another
  environment.

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
        # Screening cut-off: the selected curve's own fatigue limit (in air
        # the tabulated limit; seawater with CP has none).
        self._fatigue_limit_mpa = self._curve.fatigue_limit

    @property
    def screening_cut_off_mpa(self) -> float:
        """Stress range at or below which the damage is taken as zero [MPa].

        The selected curve's own fatigue limit: the tabulated in-air limit for
        the air curve, 0.0 for seawater with CP (no cut-off).
        """
        return self._fatigue_limit_mpa

    # ------------------------------------------------------------------
    # S-N curve loading
    # ------------------------------------------------------------------

    def _load_sn_curve(self):
        """Load the DNV-RP-C203 S-N curve of the selected environment."""
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

        Returns ``math.inf`` when *stress_mpa* is at or below the screening
        cut-off of the selected curve (in air only).
        """
        if abs(stress_mpa) <= self._fatigue_limit_mpa:
            return math.inf
        N = self._curve.get_allowable_cycles(stress_mpa)
        if math.isinf(N):
            return math.inf
        return N

    # ------------------------------------------------------------------
    # Annual damage and life
    # ------------------------------------------------------------------

    def damage_per_year(self) -> float:
        """Annual Miner fatigue damage  D = f_n × T_year / N(Δσ).

        Returns 0.0 at or below the screening cut-off (in air only).
        """
        N = self.allowable_cycles(self._stress_mpa)
        if not math.isfinite(N) or N <= 0.0:
            return 0.0
        cycles_per_year = self._fn * _SECONDS_PER_YEAR
        return cycles_per_year / N

    def fatigue_life_years(self) -> float:
        """Fatigue life = 1 / D_annual  [years].

        Returns math.inf when the annual damage is zero.
        """
        D = self.damage_per_year()
        if D <= 0.0:
            return math.inf
        return 1.0 / D
