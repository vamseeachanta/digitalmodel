"""
DNV RP F105 Section 4.2 Level 1 — Allowable free span length.

Two complementary Level-1 checks are applied; the controlling (smaller) limit governs:

1. Static deflection criterion (F105 Sec 4.2 simplified):
       δ_static = W_sub × L⁴ / (384 × EI)   [pinned-pinned, uniform load]
       δ_allow  = 0.0225 × D                  [DNV static limit]
       → L_max_static = (384 × EI × 0.0225 × D / W_sub)^0.25

2. Natural frequency / onset criterion (Uc must not exceed IL onset velocity):
       fn_min = Uc / (Vr_onset_IL × D)        [minimum fn to avoid IL onset]
       fn     = C1 / (2π L²) × √(EI/m_e)
       → L_max_freq = √( C1 × √(EI/m_e) × D × Vr_onset_IL / (2π × Uc) )

Governing limit: min(L_max_static, L_max_freq).
"""
from __future__ import annotations

import math

from .models import BoundaryConditionF105, PipeSpanInput

# C1 factors — same as span_natural_frequency
_C1 = {
    BoundaryConditionF105.PINNED_PINNED: 9.87,
    BoundaryConditionF105.FIXED_FIXED: 22.4,
    BoundaryConditionF105.FIXED_PINNED: 15.42,
}

# Static deflection allowable: δ_allow = _DELTA_FACTOR × D  (F105 Sec 4.2)
_DELTA_FACTOR: float = 0.0225

# IL onset Ks threshold — use Ks=0 (most conservative: Vr_onset=1.0)
_VR_ONSET_IL_DEFAULT: float = 1.0  # conservative; no Ks correction

_TWO_PI = 2.0 * math.pi


class SpanAllowableLength:
    """Level-1 allowable free-span length per F105 Sec 4.2.

    Parameters
    ----------
    inp                  PipeSpanInput
    submerged_weight_N_m Submerged weight of pipe + contents + coating [N/m]
    m_e                  Effective mass [kg/m]; if None it is estimated from inp
    EI                   Bending stiffness [N·m²]; if None computed from inp
    """

    def __init__(
        self,
        inp: PipeSpanInput,
        submerged_weight_N_m: float,
        m_e: float | None = None,
        EI: float | None = None,
    ) -> None:
        self._inp = inp
        self._W_sub = submerged_weight_N_m
        self._EI = EI if EI is not None else self._compute_EI()
        self._m_e = m_e if m_e is not None else self._compute_m_e()

    # ------------------------------------------------------------------
    # Fallback geometry helpers (used when EI/m_e not supplied)
    # ------------------------------------------------------------------

    def _compute_EI(self) -> float:
        inp = self._inp
        id_m = inp.od_m - 2.0 * inp.wt_m
        I = math.pi / 64.0 * (inp.od_m**4 - id_m**4)
        return inp.e_modulus_pa * I

    def _compute_m_e(self) -> float:
        """Effective mass with seabed proximity Ca correction (F105 Sec 6.8.2)."""
        from .span_natural_frequency import _added_mass_coefficient
        inp = self._inp
        id_m = inp.od_m - 2.0 * inp.wt_m
        A_steel = math.pi / 4.0 * (inp.od_m**2 - id_m**2)
        A_bore = math.pi / 4.0 * id_m**2
        m_pipe = inp.steel_density_kgm3 * A_steel
        m_content = inp.content_density_kgm3 * A_bore
        Ca = _added_mass_coefficient(inp.seabed_gap_m / inp.od_m)
        m_added = Ca * inp.water_density_kgm3 * math.pi / 4.0 * inp.od_m**2
        return m_pipe + m_content + m_added

    # ------------------------------------------------------------------
    # Allowable span limits
    # ------------------------------------------------------------------

    def _L_max_static(self) -> float:
        """Level-1 span limit from static deflection criterion [m]."""
        delta_allow = _DELTA_FACTOR * self._inp.od_m
        # pinned-pinned: δ = W L⁴ / (384 EI)  → L = (384 EI δ / W)^0.25
        numerator = 384.0 * self._EI * delta_allow
        return (numerator / self._W_sub) ** 0.25

    def _L_max_freq(self) -> float:
        """Level-1 span limit from IL onset natural frequency criterion [m].

        Ensure fn ≥ fn_min = Uc / (Vr_onset × D) by solving for L.
        Uses the conservative Vr_onset_IL = 1.0 / gamma_on_IL.
        """
        inp = self._inp
        if inp.current_velocity_ms <= 0.0:
            return math.inf
        Vr_onset = _VR_ONSET_IL_DEFAULT / inp.gamma_on_IL
        fn_min = inp.current_velocity_ms / (Vr_onset * inp.od_m)
        C1 = _C1[inp.bc]
        # fn = C1 / (2π L²) × √(EI/m_e)  → L² = C1 / (2π fn_min) × √(EI/m_e)
        freq_factor = C1 / (_TWO_PI * fn_min) * math.sqrt(self._EI / self._m_e)
        return math.sqrt(freq_factor)

    # ------------------------------------------------------------------
    # Public interface
    # ------------------------------------------------------------------

    def max_span_m(self) -> float:
        """Governing Level-1 allowable free span length [m].

        Returns min(L_static, L_freq).
        """
        L_static = self._L_max_static()
        L_freq = self._L_max_freq()
        return min(L_static, L_freq)
