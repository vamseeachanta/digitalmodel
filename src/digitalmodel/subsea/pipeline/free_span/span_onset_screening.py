"""
DNV RP F105 Section 4.3.5 / 4.4.6 — IL and CF VIV onset screening.

Stability parameter (F105 Eq 4.3-5):
    Ks = 4π me ζ_T / (ρ_w D²) / γ_k

IL onset reduced velocity (F105 Sec 4.3.5, piecewise):
    Vr_onset_IL = 1.0           (Ks < 0.4)
                = 0.6 + Ks      (0.4 ≤ Ks ≤ 1.6)
                = 2.2           (Ks > 1.6)
    Apply safety: Vr_onset_IL /= γ_on_IL

CF onset reduced velocity (F105 Sec 4.4.6):
    ψ_prox = 0.2(4 + 1.25 e/D)  (e/D < 0.8)
           = 1.0                 (e/D ≥ 0.8)
    Vr_onset_CF = 3.0 × ψ_prox / γ_on_CF

Total reduced velocity:
    Ur = (Uc + Uw) / (fn × D)
"""
from __future__ import annotations

import math

from .models import OnsetScreeningResult, PipeSpanInput


class SpanOnsetScreening:
    """VIV onset screening per DNV RP F105 Sec 4.3.5 (IL) and 4.4.6 (CF).

    Parameters
    ----------
    inp     PipeSpanInput instance
    fn_IL   In-line natural frequency [Hz]
    fn_CF   Cross-flow natural frequency [Hz]
    m_e     Effective mass per unit length [kg/m]
    """

    def __init__(
        self,
        inp: PipeSpanInput,
        fn_IL: float,
        fn_CF: float,
        m_e: float,
    ) -> None:
        self._inp = inp
        self._fn_IL = fn_IL
        self._fn_CF = fn_CF
        self._m_e = m_e

    # ------------------------------------------------------------------
    # Stability parameter
    # ------------------------------------------------------------------

    def stability_parameter(self) -> float:
        """Ks = 4π me ζ_T / (ρ_w D²) / γ_k  (F105 Eq 4.3-5).

        ζ_T = ζ_structural + ζ_hydrodynamic (total damping ratio).
        Returns Ks with the γ_k safety factor already divided out.
        """
        inp = self._inp
        zeta_T = inp.structural_damping + inp.hydrodynamic_damping
        Ks_char = (
            4.0 * math.pi * self._m_e * zeta_T
            / (inp.water_density_kgm3 * inp.od_m**2)
        )
        return Ks_char / inp.gamma_k

    # ------------------------------------------------------------------
    # IL onset
    # ------------------------------------------------------------------

    def il_onset_velocity(self, Ks: float) -> float:
        """IL onset reduced velocity with γ_on_IL safety factor applied.

        Piecewise per F105 Sec 4.3.5 / Fig 4-2:
            Ks < 0.4  → Vr_onset = 1.0
            0.4–1.6   → Vr_onset = 0.6 + Ks
            Ks > 1.6  → Vr_onset = 2.2
        """
        if Ks < 0.4:
            Vr = 1.0
        elif Ks <= 1.6:
            Vr = 0.6 + Ks
        else:
            Vr = 2.2
        return Vr / self._inp.gamma_on_IL

    # ------------------------------------------------------------------
    # CF onset
    # ------------------------------------------------------------------

    def _seabed_proximity_factor(self) -> float:
        """ψ_prox — seabed proximity correction (F105 Sec 4.4.6)."""
        e_over_D = self._inp.seabed_gap_m / self._inp.od_m
        if e_over_D >= 0.8:
            return 1.0
        return 0.2 * (4.0 + 1.25 * e_over_D)

    def cf_onset_velocity(self) -> float:
        """CF onset reduced velocity with γ_on_CF safety factor applied."""
        psi = self._seabed_proximity_factor()
        return 3.0 * psi / self._inp.gamma_on_CF

    # ------------------------------------------------------------------
    # Reduced velocity
    # ------------------------------------------------------------------

    def _reduced_velocity(self, fn: float) -> float:
        """Ur = (Uc + Uw) / (fn × D)."""
        total_v = self._inp.current_velocity_ms + self._inp.wave_velocity_ms
        return total_v / (fn * self._inp.od_m)

    # ------------------------------------------------------------------
    # Public interface
    # ------------------------------------------------------------------

    def screening_flags(self) -> OnsetScreeningResult:
        """Compute all onset screening flags and return OnsetScreeningResult."""
        Ks = self.stability_parameter()
        Ur_IL = self._reduced_velocity(self._fn_IL)
        Ur_CF = self._reduced_velocity(self._fn_CF)
        Vr_on_IL = self.il_onset_velocity(Ks)
        Vr_on_CF = self.cf_onset_velocity()
        return OnsetScreeningResult(
            Ks=Ks,
            Ur_IL=Ur_IL,
            Ur_CF=Ur_CF,
            Ur_onset_IL=Vr_on_IL,
            Ur_onset_CF=Vr_on_CF,
            il_viv_onset=Ur_IL >= Vr_on_IL,
            cf_viv_onset=Ur_CF >= Vr_on_CF,
        )
