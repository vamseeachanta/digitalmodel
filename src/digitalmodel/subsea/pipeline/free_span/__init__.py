"""
digitalmodel.subsea.pipeline.free_span
=======================================
DNV RP F105 free-spanning pipeline VIV assessment.

Public API
----------
FreespanVIVFatigue   Top-level facade — runs the complete F105 assessment
PipeSpanInput        Input dataclass
SpanVIVResult        Output dataclass
BoundaryConditionF105
EnvironmentType

Quick-start::

    from digitalmodel.subsea.pipeline.free_span import FreespanVIVFatigue
    from digitalmodel.subsea.pipeline.free_span.models import PipeSpanInput

    inp = PipeSpanInput(od_m=0.2731, wt_m=0.0127, span_length_m=45.0)
    result = FreespanVIVFatigue(inp, submerged_weight_N_m=850.0).assess()
    print(f"fn_IL={result.fn_IL_hz:.3f} Hz  fatigue_life={result.fatigue_life_years:.1f} yr")
"""
from __future__ import annotations

import math

from .models import (
    BoundaryConditionF105,
    EnvironmentType,
    NaturalFrequencyResult,
    OnsetScreeningResult,
    PipeSpanInput,
    SpanVIVResult,
)
from .span_allowable_length import SpanAllowableLength
from .span_fatigue_damage import SpanFatigueDamage
from .span_natural_frequency import SpanNaturalFrequency
from .span_onset_screening import SpanOnsetScreening
from .span_viv_response import SpanVIVAmplitude

__all__ = [
    "FreespanVIVFatigue",
    "PipeSpanInput",
    "SpanVIVResult",
    "BoundaryConditionF105",
    "EnvironmentType",
]


class FreespanVIVFatigue:
    """Top-level F105 free-span VIV fatigue assessment facade.

    Orchestrates the five sub-calculators in the correct sequence:

    1. SpanNaturalFrequency  — fn_IL, fn_CF, m_e, EI
    2. SpanOnsetScreening    — Ks, Ur, onset flags
    3. SpanVIVAmplitude      — IL/CF amplitude, stress
    4. SpanFatigueDamage     — annual damage, fatigue life
    5. SpanAllowableLength   — Level-1 allowable span

    Parameters
    ----------
    inp                   PipeSpanInput with all pipeline and site data
    submerged_weight_N_m  Submerged weight W_sub [N/m] for static check
    alpha                 Current / (current+wave) ratio (default 1.0)
    KC                    Keulegan-Carpenter number (default 30)
    """

    def __init__(
        self,
        inp: PipeSpanInput,
        submerged_weight_N_m: float = 850.0,
        alpha: float = 1.0,
        KC: float = 30.0,
    ) -> None:
        self._inp = inp
        self._W_sub = submerged_weight_N_m
        self._alpha = alpha
        self._KC = KC

    def assess(self) -> SpanVIVResult:
        """Run the complete F105 assessment and return SpanVIVResult."""
        inp = self._inp

        # 1 — Natural frequencies
        freq_calc = SpanNaturalFrequency(inp)
        freq = freq_calc.compute()

        # 2 — Onset screening
        scr = SpanOnsetScreening(
            inp, freq.fn_IL_hz, freq.fn_CF_hz, freq.m_effective_kgm
        )
        flags = scr.screening_flags()

        # 3 — Amplitude and stress
        amp = SpanVIVAmplitude(
            inp, flags.Ks, freq.fn_IL_hz, freq.fn_CF_hz,
            freq.EI_Nm2, freq.m_effective_kgm,
        )
        il_A = amp.il_amplitude_over_D()
        cf_A = amp.cf_amplitude_over_D(alpha=self._alpha, KC=self._KC)
        il_sigma = amp.stress_from_amplitude(il_A, freq.fn_IL_hz, mode="IL")
        cf_sigma = amp.stress_from_amplitude(cf_A, freq.fn_CF_hz, mode="CF")

        # 4 — Fatigue: sum IL and CF contributions (F105 Sec 7.3)
        # D_total = D_IL + D_CF  (both modes contribute to Miner sum)
        fat_IL = SpanFatigueDamage(inp, freq.fn_IL_hz, il_sigma)
        fat_CF = SpanFatigueDamage(inp, freq.fn_CF_hz, cf_sigma)
        # Use combined damage object for result; store summed values below
        fat = fat_CF  # kept for life calculation via summed damage

        # 5 — Allowable span
        allow = SpanAllowableLength(
            inp, self._W_sub,
            m_e=freq.m_effective_kgm,
            EI=freq.EI_Nm2,
        )
        L_allow = allow.max_span_m()
        utilization = inp.span_length_m / L_allow if L_allow > 0 else math.inf

        D_IL = fat_IL.damage_per_year()
        D_CF = fat_CF.damage_per_year()
        D_total = D_IL + D_CF
        life_total = 1.0 / D_total if D_total > 0.0 else math.inf

        return SpanVIVResult(
            fn_IL_hz=freq.fn_IL_hz,
            fn_CF_hz=freq.fn_CF_hz,
            Ks=flags.Ks,
            Ur_IL=flags.Ur_IL,
            Ur_CF=flags.Ur_CF,
            Ur_onset_IL=flags.Ur_onset_IL,
            Ur_onset_CF=flags.Ur_onset_CF,
            il_viv_onset=flags.il_viv_onset,
            cf_viv_onset=flags.cf_viv_onset,
            il_A_over_D=il_A,
            cf_A_over_D=cf_A,
            il_stress_mpa=il_sigma,
            cf_stress_mpa=cf_sigma,
            damage_per_year=D_total,
            fatigue_life_years=life_total,
            allowable_span_m=L_allow,
            span_length_m=inp.span_length_m,
            span_utilization=utilization,
        )
