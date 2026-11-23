"""
DNV RP F105 Section 6.8 — Free span natural frequency.

Formula (F105 Eq 6.8-1):
    f_n = C1 / (2π L²) × √(EI / m_e) × √(1 + Ce × (δ/D)²)

C1 boundary-condition factors (angular eigenvalue squared, Table 6-1):
    pinned-pinned  C1 = π²   ≈ 9.87
    fixed-fixed    C1 = 22.4
    fixed-pinned   C1 = 15.42

Ce static-deflection correction (commentary Sec 6.8):
    pinned-pinned  Ce_IL = 0.4,  Ce_CF = 0.2
    fixed-fixed    Ce_IL = 0.2,  Ce_CF = 0.1
    fixed-pinned   Ce_IL = 0.3,  Ce_CF = 0.15
"""
from __future__ import annotations

import math

from .models import BoundaryConditionF105, NaturalFrequencyResult, PipeSpanInput

# C1 factors: ω₁ = C1 × √(EI / m_e L⁴)  →  f₁ = C1 / (2π) × √(EI / m_e L⁴)
_C1 = {
    BoundaryConditionF105.PINNED_PINNED: 9.87,   # = π²
    BoundaryConditionF105.FIXED_FIXED: 22.4,
    BoundaryConditionF105.FIXED_PINNED: 15.42,
}

# Ce IL/CF correction factors (sag effect on effective span stiffness)
_CE_IL = {
    BoundaryConditionF105.PINNED_PINNED: 0.4,
    BoundaryConditionF105.FIXED_FIXED: 0.2,
    BoundaryConditionF105.FIXED_PINNED: 0.3,
}
_CE_CF = {
    BoundaryConditionF105.PINNED_PINNED: 0.2,
    BoundaryConditionF105.FIXED_FIXED: 0.1,
    BoundaryConditionF105.FIXED_PINNED: 0.15,
}

# Added mass coefficient Ca — base value for circular section far from seabed.
# F105 Sec 6.8.2: Ca increases for e/D < 0.8 (seabed proximity effect).
# SpanNaturalFrequency.compute() applies the proximity correction.
_CA_BASE = 1.0


def _added_mass_coefficient(e_over_D: float) -> float:
    """Ca with seabed proximity correction (F105 Sec 6.8.2, Fig 6-3).

    Ca increases as the pipe approaches the seabed (e/D → 0).
    Approximate linear fit over 0 ≤ e/D ≤ 0.8:
        Ca ≈ 1.0 + 0.5 × (1 − e/D / 0.8)   (engineering approximation)
    For e/D ≥ 0.8: Ca = 1.0 (far from seabed).
    """
    if e_over_D >= 0.8:
        return _CA_BASE
    e_over_D = max(0.0, e_over_D)  # clamp: negative gap is physically impossible
    # interpolate from Ca=1.5 at e/D=0 to Ca=1.0 at e/D=0.8
    return 1.0 + 0.5 * (1.0 - e_over_D / 0.8)
_TWO_PI = 2.0 * math.pi


class SpanNaturalFrequency:
    """Calculates in-line and cross-flow natural frequencies per F105 Sec 6.8.

    Usage::

        calc = SpanNaturalFrequency(inp)
        result = calc.compute()   # NaturalFrequencyResult
    """

    def __init__(self, inp: PipeSpanInput) -> None:
        self._inp = inp
        self._id_m = inp.od_m - 2.0 * inp.wt_m
        self._EI = self._bending_stiffness()
        self._m_e = self._effective_mass()

    # ------------------------------------------------------------------
    # Private geometry helpers
    # ------------------------------------------------------------------

    def _bending_stiffness(self) -> float:
        """EI [N·m²] for hollow circular section."""
        I = math.pi / 64.0 * (self._inp.od_m**4 - self._id_m**4)
        return self._inp.e_modulus_pa * I

    def _effective_mass(self) -> float:
        """m_e = m_pipe + m_content + m_added  [kg/m].

        m_added = Ca(e/D) × ρ_w × (π/4) × D²
        Ca includes seabed proximity correction per F105 Sec 6.8.2.
        """
        inp = self._inp
        A_steel = math.pi / 4.0 * (inp.od_m**2 - self._id_m**2)
        A_bore = math.pi / 4.0 * self._id_m**2
        m_pipe = inp.steel_density_kgm3 * A_steel
        m_content = inp.content_density_kgm3 * A_bore
        e_over_D = inp.seabed_gap_m / inp.od_m
        Ca = _added_mass_coefficient(e_over_D)
        m_added = Ca * inp.water_density_kgm3 * math.pi / 4.0 * inp.od_m**2
        return m_pipe + m_content + m_added

    # ------------------------------------------------------------------
    # Core formula
    # ------------------------------------------------------------------

    def _fn(self, ce: float) -> float:
        """f_n [Hz] for a given Ce sag-correction factor.

        F105 Eq 6.8-1 (complete form):
            f_n = C1/(2π L²) × √(EI/m_e) × √(1 + Se/Pcr) × √(1 + Ce(δ/D)²)

        Se = effective axial force (positive = tension, raises fn)
        Pcr = π² × EI / L²   (Euler buckling load for first mode)
        """
        inp = self._inp
        C1 = _C1[inp.bc]
        sag_ratio = inp.sag_m / inp.od_m
        sag_factor = math.sqrt(max(0.0, 1.0 + ce * sag_ratio**2))
        # Axial force correction — F105 Eq 6.8-1 term √(1 + Se/Pcr)
        Pcr = math.pi**2 * self._EI / inp.span_length_m**2  # Euler critical load
        axial_factor = math.sqrt(max(0.0, 1.0 + inp.effective_axial_force_N / Pcr))
        base_freq = (C1 / (_TWO_PI * inp.span_length_m**2)) * math.sqrt(
            self._EI / self._m_e
        )
        return base_freq * sag_factor * axial_factor

    # ------------------------------------------------------------------
    # Public interface
    # ------------------------------------------------------------------

    def fn_IL(self) -> float:
        """In-line natural frequency [Hz] (F105 Sec 6.8.3)."""
        return self._fn(_CE_IL[self._inp.bc])

    def fn_CF(self) -> float:
        """Cross-flow natural frequency [Hz] (F105 Sec 6.8.3).

        CF uses a smaller Ce than IL; for δ=0 fn_CF equals fn_IL.
        """
        return self._fn(_CE_CF[self._inp.bc])

    def compute(self) -> NaturalFrequencyResult:
        """Return NaturalFrequencyResult with fn_IL, fn_CF, m_e, EI."""
        return NaturalFrequencyResult(
            fn_IL_hz=self.fn_IL(),
            fn_CF_hz=self.fn_CF(),
            m_effective_kgm=self._m_e,
            EI_Nm2=self._EI,
        )
