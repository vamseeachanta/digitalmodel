"""
DNV RP F105 Sections 4.3.4 / 4.4.4 — IL and CF VIV amplitude response.

In-line amplitude (F105 Sec 4.3.4, Fig 4-2):
    A2_IL = 0.13 × (1 − Ks/1.8)                          lower plateau
    A1_IL = max(0.18 × (1 − Ks/1.2), A2_IL)              peak
    Vr_end = 3.7  (Ks ≥ 1)  or  4.5 − 0.8 Ks  (Ks < 1)
    Piecewise linear over [Vr_onset, Vr_onset+10A1, Vr_end−2A2, Vr_end]

Cross-flow amplitude (F105 Sec 4.4.4, Fig 4-5):
    Rk = max(1 − 0.15 Ks, 0)                             damping reduction
    A1_CF: 0.9 (current-dominated, α > 0.8)
           wave-dominated: 0.7 (KC<10) / 0.9 (KC>30) / interpolated
    Piecewise from Vr_onset → Vr_peak → Vr_end=16

Stress (simplified beam approximation):
    σ_peak = A/D × (π/L)² × EI × (D/2) / I_section   [MPa]
    Valid for screening; FEA mode shapes give accurate values.
"""
from __future__ import annotations

import math

from .models import PipeSpanInput


class SpanVIVAmplitude:
    """Piecewise VIV amplitude model per DNV RP F105.

    Parameters
    ----------
    inp     PipeSpanInput
    Ks      Stability parameter (with γ_k applied)
    fn_IL   In-line natural frequency [Hz]
    fn_CF   Cross-flow natural frequency [Hz]
    EI      Bending stiffness [N·m²]
    m_e     Effective mass per unit length [kg/m]  (unused here, for future)
    """

    def __init__(
        self,
        inp: PipeSpanInput,
        Ks: float,
        fn_IL: float,
        fn_CF: float,
        EI: float,
        m_e: float,
    ) -> None:
        self._inp = inp
        self._Ks = Ks
        self._fn_IL = fn_IL
        self._fn_CF = fn_CF
        self._EI = EI
        self._m_e = m_e

    # ------------------------------------------------------------------
    # Reduced velocity helpers
    # ------------------------------------------------------------------

    def _Ur(self, fn: float) -> float:
        total_v = self._inp.current_velocity_ms + self._inp.wave_velocity_ms
        return total_v / (fn * self._inp.od_m)

    def _seabed_prox(self) -> float:
        e_D = self._inp.seabed_gap_m / self._inp.od_m
        return 1.0 if e_D >= 0.8 else 0.2 * (4.0 + 1.25 * e_D)

    # ------------------------------------------------------------------
    # In-line amplitude (F105 Sec 4.3.4)
    # ------------------------------------------------------------------

    def il_amplitude_over_D(self) -> float:
        """IL response amplitude A_IL/D (F105 Sec 4.3.4 piecewise model)."""
        Ks = self._Ks
        Ur = self._Ur(self._fn_IL)
        Vr_onset = self._il_onset_Vr(Ks)
        if Ur < Vr_onset:
            return 0.0
        A2 = max(0.13 * (1.0 - Ks / 1.8), 0.0)
        A1 = max(0.18 * (1.0 - Ks / 1.2), A2)
        Vr_end = 3.7 if Ks >= 1.0 else 4.5 - 0.8 * Ks
        Vr1 = Vr_onset + 10.0 * A1
        Vr2 = Vr_end - 2.0 * A2
        return _piecewise_il(Ur, Vr_onset, Vr1, Vr2, Vr_end, A1, A2)

    def _il_onset_Vr(self, Ks: float) -> float:
        """IL onset Vr with γ_on_IL safety factor (same logic as screening)."""
        if Ks < 0.4:
            Vr = 1.0
        elif Ks <= 1.6:
            Vr = 0.6 + Ks
        else:
            Vr = 2.2
        return Vr / self._inp.gamma_on_IL

    # ------------------------------------------------------------------
    # Cross-flow amplitude (F105 Sec 4.4.4)
    # ------------------------------------------------------------------

    def cf_amplitude_over_D(
        self, alpha: float = 1.0, KC: float = 30.0
    ) -> float:
        """CF response amplitude A_CF/D (F105 Sec 4.4.4 piecewise model).

        Parameters
        ----------
        alpha   current / (current + wave) velocity ratio; > 0.8 = current-dominated
        KC      Keulegan-Carpenter number (used in wave-dominated regime)
        """
        Ks = self._Ks
        Ur = self._Ur(self._fn_CF)
        Vr_onset = 3.0 * self._seabed_prox() / self._inp.gamma_on_CF
        if Ur < Vr_onset:
            return 0.0
        Rk = max(1.0 - 0.15 * Ks, 0.0)
        A1 = self._cf_peak_amplitude(alpha, KC) * Rk
        A2 = 0.3 * Rk
        Vr_peak = Vr_onset + 2.5
        Vr_end = 16.0
        return _piecewise_cf(Ur, Vr_onset, Vr_peak, Vr_end, A1, A2)

    @staticmethod
    def _cf_peak_amplitude(alpha: float, KC: float) -> float:
        """Peak CF amplitude factor as a function of α and KC (F105 Sec 4.4.4)."""
        if alpha > 0.8:  # current-dominated
            return 0.9
        # wave-dominated
        if KC < 10.0:
            return 0.7
        if KC > 30.0:
            return 0.9
        return 0.7 + 0.01 * (KC - 10.0)

    # ------------------------------------------------------------------
    # Stress from amplitude (simplified beam formula)
    # ------------------------------------------------------------------

    def stress_from_amplitude(
        self, A_over_D: float, fn: float, mode: str = "IL"
    ) -> float:
        """Peak bending stress from VIV amplitude [MPa] (screening approximation).

        Formula for first bending mode (F105 Sec 6.5 / screening level):
            M_peak = κ_peak × EI × amplitude_m
            σ_peak = M_peak × (D/2) / I_section

        κ_peak = (π/L)² × Cbc  where Cbc accounts for BC-dependent maximum
        curvature location (at ends for fixed supports, midspan for pinned).

        Boundary condition curvature scaling Cbc (ratio to pinned-pinned peak):
            pinned-pinned: 1.0  (sine, max at midspan)
            fixed-pinned:  1.8  (approx, max at fixed end)
            fixed-fixed:   2.3  (approx, max at fixed ends)

        For detailed design, use FEA modal stresses.
        """
        if A_over_D <= 0.0:
            return 0.0
        inp = self._inp
        from .models import BoundaryConditionF105
        _CBC = {
            BoundaryConditionF105.PINNED_PINNED: 1.0,
            BoundaryConditionF105.FIXED_PINNED: 1.8,
            BoundaryConditionF105.FIXED_FIXED: 2.3,
        }
        Cbc = _CBC.get(inp.bc, 1.0)
        amplitude_m = A_over_D * inp.od_m
        k_wave = math.pi / inp.span_length_m       # first-mode wavenumber
        M_peak = (k_wave**2 * Cbc) * self._EI * amplitude_m
        id_m = inp.od_m - 2.0 * inp.wt_m
        I_sec = math.pi / 64.0 * (inp.od_m**4 - id_m**4)
        sigma_pa = M_peak * (inp.od_m / 2.0) / I_sec
        return sigma_pa / 1.0e6  # Pa → MPa


# ---------------------------------------------------------------------------
# Module-level piecewise helpers (pure functions, no side effects)
# ---------------------------------------------------------------------------

def _piecewise_il(
    Ur: float,
    Vr_on: float,
    Vr1: float,
    Vr2: float,
    Vr_end: float,
    A1: float,
    A2: float,
) -> float:
    """Piecewise linear IL amplitude curve (F105 Fig 4-2)."""
    if Ur <= Vr_on:
        return 0.0
    if Ur <= Vr1:
        dV = max(Vr1 - Vr_on, 1.0e-9)
        return A1 * (Ur - Vr_on) / dV
    if Ur <= Vr2:
        dV = max(Vr2 - Vr1, 1.0e-9)
        return A1 + (A2 - A1) * (Ur - Vr1) / dV
    if Ur <= Vr_end:
        dV = max(Vr_end - Vr2, 1.0e-9)
        return A2 * (Vr_end - Ur) / dV
    return 0.0


def _piecewise_cf(
    Ur: float,
    Vr_on: float,
    Vr_peak: float,
    Vr_end: float,
    A1: float,
    A2: float,
) -> float:
    """Piecewise linear CF amplitude curve (F105 Fig 4-5)."""
    if Ur <= Vr_on:
        return 0.0
    if Ur <= Vr_peak:
        dV = max(Vr_peak - Vr_on, 1.0e-9)
        return A1 * (Ur - Vr_on) / dV
    if Ur <= Vr_end:
        dV = max(Vr_end - Vr_peak, 1.0e-9)
        t = (Ur - Vr_peak) / dV
        return A1 + (A2 - A1) * t
    return max(A2, 0.0)
