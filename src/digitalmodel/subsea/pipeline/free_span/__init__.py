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
from dataclasses import replace

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
from .wave_velocity import compute_wave_velocity

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
    alpha                 Optional current / (current+wave) ratio override
    KC                    Optional Keulegan-Carpenter number override

    When velocity is omitted and a sea state is supplied, the existing wave
    calculator provides Uw [m/s] and Tu [s] at the pipe centre. The dimensionless
    definitions are KC = Uw*Tu/OD and alpha = Uc/(Uc+Uw), with OD in metres.
    For zero total speed, alpha is defined as 1 (the no-wave convention);
    automatic zero Uw gives KC = 0. Explicit alpha and KC independently override
    their derived values, including zero and the former default values.
    Without automatic velocity resolution, omitted alpha and KC retain the
    legacy defaults 1 and 30 from this facade's existing API.
    """

    def __init__(
        self,
        inp: PipeSpanInput,
        submerged_weight_N_m: float = 850.0,
        alpha: float | None = None,
        KC: float | None = None,
    ) -> None:
        self._inp = inp
        self._W_sub = submerged_weight_N_m
        self._alpha = alpha
        self._KC = KC

    def assess(self) -> SpanVIVResult:
        """Run the complete F105 assessment and return SpanVIVResult."""
        inp, alpha, KC = self._resolve_wave_input()

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
        cf_A = amp.cf_amplitude_over_D(alpha=alpha, KC=KC)
        il_sigma = amp.stress_from_amplitude(il_A, freq.fn_IL_hz, mode="IL")
        cf_sigma = amp.stress_from_amplitude(cf_A, freq.fn_CF_hz, mode="CF")

        # 4 — Fatigue: sum IL and CF contributions (F105 Sec 7.3)
        # D_total = D_IL + D_CF  (both modes contribute to Miner sum)
        fat_IL = SpanFatigueDamage(inp, freq.fn_IL_hz, il_sigma)
        fat_CF = SpanFatigueDamage(inp, freq.fn_CF_hz, cf_sigma)
        # Use combined damage object for result; store summed values below
        # fat = fat_CF  # kept for life calculation via summed damage

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

    def _resolve_wave_input(self) -> tuple[PipeSpanInput, float, float]:
        """Resolve a fresh input copy and response ratios for this assessment."""
        inp = self._inp
        Hs, Tp, depth = inp.Hs_m, inp.Tp_s, inp.water_depth_m
        # Existing API defaults, retained outside automatic sea-state resolution.
        alpha, KC = 1.0, 30.0
        velocity = inp.wave_velocity_ms
        if any(value is not None for value in (Hs, Tp, depth)):
            if Hs is None or Tp is None or depth is None:
                raise ValueError("Hs_m, Tp_s and water_depth_m must be supplied together")
            for name, value in zip(("Hs_m", "Tp_s", "water_depth_m"), (Hs, Tp, depth)):
                if not math.isfinite(value) or value <= 0.0:
                    raise ValueError(f"{name} must be finite and strictly positive")
            if velocity is None:
                wave = compute_wave_velocity(
                    Hs=Hs, Tp=Tp, water_depth_m=depth,
                    pipe_elevation_m=inp.seabed_gap_m, pipe_od_m=inp.od_m,
                )
                velocity = wave.Uw_ms
                KC = velocity * wave.Tu_s / inp.od_m
                total = inp.current_velocity_ms + velocity
                alpha = inp.current_velocity_ms / total if total != 0.0 else 1.0
        if self._alpha is not None:
            alpha = self._alpha
        if self._KC is not None:
            KC = self._KC
        if velocity is None:
            velocity = 0.0
        return replace(inp, wave_velocity_ms=velocity), alpha, KC

    # ------------------------------------------------------------------
    # Multi-current probability-weighted assessment
    # ------------------------------------------------------------------

    def assess_multi_current(
        self,
        current_bins: list[tuple[float, float]],
    ) -> SpanVIVResult:
        """Probability-weighted VIV fatigue over multiple current speed bins.

        Computes damage at each current speed and sums weighted by
        occurrence probability, matching the MATLAB approach:

            D_total = Σ D(Uc_i) × prob_i

        Parameters
        ----------
        current_bins
            List of (current_speed_ms, probability) tuples.
            Probabilities should sum to 1.0.

        Returns
        -------
        SpanVIVResult
            Result with probability-weighted damage.  Frequencies, onset
            screening, amplitudes, and stresses correspond to the current
            speed that produces the maximum (unweighted) damage.
        """
        if not current_bins:
            raise ValueError("current_bins must not be empty")

        weighted_damage = 0.0
        max_damage = -1.0
        worst_result = None

        for speed, prob in current_bins:
            inp_i = replace(self._inp, current_velocity_ms=speed)
            facade_i = FreespanVIVFatigue(
                inp_i, self._W_sub, self._alpha, self._KC,
            )
            result_i = facade_i.assess()
            weighted_damage += result_i.damage_per_year * prob

            if result_i.damage_per_year > max_damage:
                max_damage = result_i.damage_per_year
                worst_result = result_i

        # Build combined result using worst-case details but weighted damage
        assert worst_result is not None
        life = 1.0 / weighted_damage if weighted_damage > 0 else math.inf

        return SpanVIVResult(
            fn_IL_hz=worst_result.fn_IL_hz,
            fn_CF_hz=worst_result.fn_CF_hz,
            Ks=worst_result.Ks,
            Ur_IL=worst_result.Ur_IL,
            Ur_CF=worst_result.Ur_CF,
            Ur_onset_IL=worst_result.Ur_onset_IL,
            Ur_onset_CF=worst_result.Ur_onset_CF,
            il_viv_onset=worst_result.il_viv_onset,
            cf_viv_onset=worst_result.cf_viv_onset,
            il_A_over_D=worst_result.il_A_over_D,
            cf_A_over_D=worst_result.cf_A_over_D,
            il_stress_mpa=worst_result.il_stress_mpa,
            cf_stress_mpa=worst_result.cf_stress_mpa,
            damage_per_year=weighted_damage,
            fatigue_life_years=life,
            allowable_span_m=worst_result.allowable_span_m,
            span_length_m=worst_result.span_length_m,
            span_utilization=worst_result.span_utilization,
        )
