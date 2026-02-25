"""
DNV RP F105 Section 7.3 + DNV-RP-C203 — Fatigue damage accumulation.

Wires to ``digitalmodel.structural.fatigue.sn_curves.get_dnv_curve`` for
the actual S-N curve parameters and allowable-cycles calculation.

DNV-RP-C203 F-class parameters (StandardSNCurves.DNV_CURVES['F']):
    In air:               A = 1.73×10¹¹, m = 3.0, CAFL = 36.84 MPa
    Seawater with CP:     allowable cycles reduced by factor 0.54
                          (DNV-RP-C203 Table 2-1 seawater correction)

Palmgren-Miner rule (F105 Eq 7.3-1):
    D_annual = f_n × T_year / N(Δσ)
    where N(Δσ) = A × Δσ^(-m)  from the S-N curve

Fatigue life = 1 / D_annual  [years]
"""
from __future__ import annotations

import math

from .models import EnvironmentType, PipeSpanInput

# seconds per year (365.25 days)
_SECONDS_PER_YEAR: float = 3.15576e7

# Seawater-with-CP reduction factor on allowable cycles (DNV-RP-C203 §2.3.3).
# N_sw_cp = 0.54 × N_air  →  D_sw_cp = D_air / 0.54
_SEAWATER_CP_CYCLE_FACTOR: float = 0.54


class SpanFatigueDamage:
    """Palmgren-Miner fatigue damage per DNV RP F105 Sec 7.3 + DNV-RP-C203.

    Parameters
    ----------
    inp         PipeSpanInput (carries sn_curve_class and environment)
    fn          Governing natural frequency [Hz] (use CF fn for VIV fatigue)
    stress_mpa  Peak stress range per cycle [MPa]
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
    # S-N curve loading (wired to structural.fatigue module)
    # ------------------------------------------------------------------

    def _load_sn_curve(self):
        """Load the DNV-RP-C203 curve via get_dnv_curve.

        Returns a PowerLawSNCurve with attributes .A, .m, .fatigue_limit
        and method .get_allowable_cycles(stress_mpa).
        """
        from digitalmodel.structural.fatigue.sn_curves import get_dnv_curve
        return get_dnv_curve(self._inp.sn_curve_class)

    # ------------------------------------------------------------------
    # Allowable cycles N(S)
    # ------------------------------------------------------------------

    def allowable_cycles(self, stress_mpa: float) -> float:
        """Allowable cycles N from DNV-RP-C203 curve with environment correction.

        For IN_AIR:      N = curve.get_allowable_cycles(S)
        For SEAWATER_CP: N_sw = 0.54 × N_air  (fewer cycles in seawater)
        Returns inf when S ≤ CAFL (fatigue limit).
        """
        N_air = self._curve.get_allowable_cycles(stress_mpa)
        if math.isinf(N_air):
            return math.inf
        if self._inp.environment == EnvironmentType.SEAWATER_CP:
            return N_air * _SEAWATER_CP_CYCLE_FACTOR
        return N_air

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
