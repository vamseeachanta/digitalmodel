"""
Tests for DNV RP F105 free-spanning pipeline VIV module.

Reference case (all tests unless noted):
  10.75" pipeline: OD=0.2731 m, WT=0.0127 m
  L=34 m (pinned-pinned) → fn_IL ≈ 0.42 Hz
  Uc=0.8 m/s, water depth 120 m, oil content 900 kg/m³
"""
import math
import pytest

from digitalmodel.subsea.pipeline.free_span.models import (
    PipeSpanInput,
    BoundaryConditionF105,
    EnvironmentType,
)
from digitalmodel.subsea.pipeline.free_span.span_natural_frequency import (
    SpanNaturalFrequency,
)
from digitalmodel.subsea.pipeline.free_span.span_onset_screening import (
    SpanOnsetScreening,
)
from digitalmodel.subsea.pipeline.free_span.span_viv_response import (
    SpanVIVAmplitude,
)
from digitalmodel.subsea.pipeline.free_span.span_fatigue_damage import (
    SpanFatigueDamage,
)
from digitalmodel.subsea.pipeline.free_span.span_allowable_length import (
    SpanAllowableLength,
)
from digitalmodel.subsea.pipeline.free_span import FreespanVIVFatigue
from digitalmodel.subsea.pipeline.free_span.span_natural_frequency import (
    _added_mass_coefficient,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def ref_input():
    """10" pipeline reference span — L=34 m gives fn ≈ 0.42 Hz."""
    return PipeSpanInput(
        od_m=0.2731,
        wt_m=0.0127,
        span_length_m=34.0,
        e_modulus_pa=207e9,
        steel_density_kgm3=7850.0,
        content_density_kgm3=900.0,
        water_density_kgm3=1025.0,
        current_velocity_ms=0.8,
        wave_velocity_ms=0.0,
        seabed_gap_m=0.5,
        bc=BoundaryConditionF105.PINNED_PINNED,
        sag_m=0.0,
        structural_damping=0.005,
        hydrodynamic_damping=0.010,
        sn_curve_class="F",
        environment=EnvironmentType.SEAWATER_CP,
        gamma_on_IL=1.1,
        gamma_on_CF=1.3,
        gamma_k=1.15,
    )


@pytest.fixture
def ff_input(ref_input):
    """Fixed-fixed boundary condition variant."""
    from dataclasses import replace
    return replace(ref_input, bc=BoundaryConditionF105.FIXED_FIXED)


@pytest.fixture
def below_onset_input(ref_input):
    """Low velocity — should not trigger onset."""
    from dataclasses import replace
    return replace(ref_input, current_velocity_ms=0.1)


# ---------------------------------------------------------------------------
# Natural Frequency tests (F105 Sec 6.8)
# ---------------------------------------------------------------------------

class TestAddedMassCoefficient:
    """Unit tests for Ca(e/D) seabed proximity function."""

    def test_far_from_seabed_ca_equals_one(self):
        assert _added_mass_coefficient(1.0) == 1.0
        assert _added_mass_coefficient(0.8) == 1.0

    def test_on_seabed_ca_equals_one_point_five(self):
        assert abs(_added_mass_coefficient(0.0) - 1.5) < 1e-9

    def test_midpoint_interpolation(self):
        """Ca at e/D=0.4 should be midway between 1.5 and 1.0."""
        assert abs(_added_mass_coefficient(0.4) - 1.25) < 1e-9

    def test_negative_gap_clamped(self):
        """Negative seabed gap is physically impossible — clamp to e/D=0 (Ca=1.5)."""
        assert _added_mass_coefficient(-0.5) == _added_mass_coefficient(0.0)


class TestSpanNaturalFrequency:
    def test_fn_IL_pinned_pinned_range(self, ref_input):
        """fn_IL for PP span L=34m must be within ±15% of 0.42 Hz."""
        calc = SpanNaturalFrequency(ref_input)
        fn = calc.fn_IL()
        assert 0.36 <= fn <= 0.48, f"fn_IL={fn:.3f} Hz outside expected range"

    def test_fn_CF_less_than_or_equal_IL(self, ref_input):
        """fn_CF ≤ fn_IL (CF uses smaller Ce correction)."""
        calc = SpanNaturalFrequency(ref_input)
        assert calc.fn_CF() <= calc.fn_IL() + 1e-9

    def test_fn_increases_with_shorter_span(self, ref_input):
        """Shorter span → higher natural frequency."""
        from dataclasses import replace
        calc_short = SpanNaturalFrequency(replace(ref_input, span_length_m=20.0))
        calc_long = SpanNaturalFrequency(replace(ref_input, span_length_m=50.0))
        assert calc_short.fn_IL() > calc_long.fn_IL()

    def test_fixed_fixed_higher_than_pinned_pinned(self, ref_input, ff_input):
        """Fixed-fixed fn > pinned-pinned fn (C1_FF > C1_PP)."""
        fn_pp = SpanNaturalFrequency(ref_input).fn_IL()
        fn_ff = SpanNaturalFrequency(ff_input).fn_IL()
        assert fn_ff > fn_pp

    def test_compute_returns_result(self, ref_input):
        calc = SpanNaturalFrequency(ref_input)
        result = calc.compute()
        assert result.fn_IL_hz > 0
        assert result.fn_CF_hz > 0
        assert result.m_effective_kgm > 0
        assert result.EI_Nm2 > 0

    def test_sag_correction_increases_frequency(self, ref_input):
        """Non-zero sag increases fn via Ce correction factor."""
        from dataclasses import replace
        inp_no_sag = ref_input
        inp_sag = replace(ref_input, sag_m=0.1 * ref_input.od_m)
        fn_no = SpanNaturalFrequency(inp_no_sag).fn_IL()
        fn_sag = SpanNaturalFrequency(inp_sag).fn_IL()
        assert fn_sag >= fn_no

    def test_tensile_axial_force_increases_fn(self, ref_input):
        """Positive Se (tension) increases natural frequency per F105 Eq 6.8-1."""
        from dataclasses import replace
        fn_base = SpanNaturalFrequency(ref_input).fn_IL()
        inp_tension = replace(ref_input, effective_axial_force_N=500_000.0)  # 500 kN tension
        fn_tension = SpanNaturalFrequency(inp_tension).fn_IL()
        assert fn_tension > fn_base

    def test_compressive_axial_force_reduces_fn(self, ref_input):
        """Negative Se (compression) reduces natural frequency per F105 Eq 6.8-1."""
        from dataclasses import replace
        fn_base = SpanNaturalFrequency(ref_input).fn_IL()
        inp_compression = replace(ref_input, effective_axial_force_N=-200_000.0)  # 200 kN compression
        fn_compression = SpanNaturalFrequency(inp_compression).fn_IL()
        assert fn_compression < fn_base

    def test_seabed_proximity_increases_added_mass(self, ref_input):
        """Small seabed gap (e/D < 0.8) gives Ca > 1 → higher m_e → lower fn."""
        from dataclasses import replace
        # Far from seabed: e/D >> 0.8 → Ca = 1.0
        inp_far = replace(ref_input, seabed_gap_m=5.0)
        # Close to seabed: e/D = 0.2 → Ca = 1.375
        inp_close = replace(ref_input, seabed_gap_m=0.2 * ref_input.od_m)
        fn_far = SpanNaturalFrequency(inp_far).fn_IL()
        fn_close = SpanNaturalFrequency(inp_close).fn_IL()
        assert fn_close < fn_far, "Seabed proximity should increase m_e and reduce fn"


# ---------------------------------------------------------------------------
# Onset Screening tests (F105 Sec 4.3.5, 4.4.6)
# ---------------------------------------------------------------------------

class TestSpanOnsetScreening:
    def _build(self, inp):
        freq = SpanNaturalFrequency(inp)
        result = freq.compute()
        return SpanOnsetScreening(
            inp, result.fn_IL_hz, result.fn_CF_hz, result.m_effective_kgm
        )

    def test_stability_parameter_positive(self, ref_input):
        scr = self._build(ref_input)
        assert scr.stability_parameter() > 0

    def test_il_onset_detected_at_high_velocity(self, ref_input):
        """High current → IL and CF VIV onset must be detected."""
        scr = self._build(ref_input)
        flags = scr.screening_flags()
        # Ur >> Vr_onset for Uc=0.8 m/s, fn≈0.42 Hz, D=0.2731 m
        assert flags.il_viv_onset is True

    def test_cf_onset_detected_at_high_velocity(self, ref_input):
        scr = self._build(ref_input)
        flags = scr.screening_flags()
        assert flags.cf_viv_onset is True

    def test_no_onset_at_low_velocity(self, below_onset_input):
        """Low velocity (0.1 m/s) must not trigger IL onset."""
        scr = self._build(below_onset_input)
        flags = scr.screening_flags()
        assert flags.il_viv_onset is False

    def test_Ur_positive(self, ref_input):
        scr = self._build(ref_input)
        flags = scr.screening_flags()
        assert flags.Ur_IL > 0
        assert flags.Ur_CF > 0

    def test_IL_onset_piecewise_boundary(self, ref_input):
        """Ks < 0.4 → Vr_onset_IL = 1.0 / gamma_on_IL."""
        from dataclasses import replace
        # Force Ks < 0.4 by reducing damping significantly
        inp = replace(ref_input, structural_damping=0.001, hydrodynamic_damping=0.001)
        freq = SpanNaturalFrequency(inp).compute()
        scr = SpanOnsetScreening(inp, freq.fn_IL_hz, freq.fn_CF_hz,
                                 freq.m_effective_kgm)
        Ks = scr.stability_parameter()
        Vr_on = scr.il_onset_velocity(Ks)
        # Ks is already post-gamma_k; check against 0.4 directly (F105 Sec 4.3.5)
        if Ks < 0.4:
            expected = 1.0 / inp.gamma_on_IL
            assert abs(Vr_on - expected) < 0.01

    def test_seabed_gap_large_gives_full_prox(self, ref_input):
        """Gap > 0.8D → seabed proximity factor = 1.0 (no reduction)."""
        from dataclasses import replace
        inp = replace(ref_input, seabed_gap_m=0.8 * ref_input.od_m + 0.01)
        freq = SpanNaturalFrequency(inp).compute()
        scr = SpanOnsetScreening(inp, freq.fn_IL_hz, freq.fn_CF_hz,
                                 freq.m_effective_kgm)
        Vr_cf = scr.cf_onset_velocity()
        expected = 3.0 / inp.gamma_on_CF
        assert abs(Vr_cf - expected) < 0.01


# ---------------------------------------------------------------------------
# VIV Amplitude tests (F105 Sec 4.3.4, 4.4.4)
# ---------------------------------------------------------------------------

class TestSpanVIVAmplitude:
    def _build(self, inp):
        freq = SpanNaturalFrequency(inp).compute()
        Ks = SpanOnsetScreening(
            inp, freq.fn_IL_hz, freq.fn_CF_hz, freq.m_effective_kgm
        ).stability_parameter()
        return SpanVIVAmplitude(
            inp, Ks, freq.fn_IL_hz, freq.fn_CF_hz,
            freq.EI_Nm2, freq.m_effective_kgm,
        )

    def test_amplitude_zero_below_onset(self, below_onset_input):
        """Below onset velocity → amplitude = 0."""
        amp = self._build(below_onset_input)
        assert amp.il_amplitude_over_D() == 0.0
        assert amp.cf_amplitude_over_D() == 0.0

    def test_cf_amplitude_nonnegative(self, ref_input):
        """CF amplitude/D ≥ 0 in all cases."""
        amp = self._build(ref_input)
        assert amp.cf_amplitude_over_D() >= 0.0

    def test_il_amplitude_nonnegative(self, ref_input):
        """IL amplitude/D ≥ 0 in all cases."""
        amp = self._build(ref_input)
        assert amp.il_amplitude_over_D() >= 0.0

    def test_stress_zero_when_amplitude_zero(self, below_onset_input):
        """No amplitude → no stress."""
        amp = self._build(below_onset_input)
        sigma = amp.stress_from_amplitude(0.0, 0.43)
        assert sigma == 0.0

    def test_stress_positive_when_amplitude_positive(self, ref_input):
        """Non-zero amplitude → positive stress."""
        amp = self._build(ref_input)
        A_over_D = amp.cf_amplitude_over_D()
        freq = SpanNaturalFrequency(ref_input).fn_IL()
        sigma = amp.stress_from_amplitude(A_over_D, freq, mode="CF")
        if A_over_D > 0:
            assert sigma > 0.0

    def test_amplitude_bounded(self, ref_input):
        """F105 amplitude/D is physically bounded: IL < 0.25, CF < 1.5."""
        amp = self._build(ref_input)
        assert amp.il_amplitude_over_D() <= 0.25
        assert amp.cf_amplitude_over_D() <= 1.5


# ---------------------------------------------------------------------------
# Fatigue Damage tests (F105 Sec 7.3 + DNV-RP-C203)
# ---------------------------------------------------------------------------

class TestSpanFatigueDamage:
    def _build_with_stress(self, inp, stress_mpa, fn=0.43):
        return SpanFatigueDamage(inp, fn, stress_mpa)

    def test_zero_damage_below_fatigue_limit(self, ref_input):
        """Stress below S-N fatigue limit → zero annual damage."""
        fat = self._build_with_stress(ref_input, stress_mpa=10.0)
        assert fat.damage_per_year() == 0.0

    def test_infinite_life_below_fatigue_limit(self, ref_input):
        """Stress < fatigue limit → fatigue life = infinity."""
        fat = self._build_with_stress(ref_input, stress_mpa=10.0)
        assert math.isinf(fat.fatigue_life_years())

    def test_finite_damage_above_fatigue_limit(self, ref_input):
        """Stress above fatigue limit → finite positive damage."""
        fat = self._build_with_stress(ref_input, stress_mpa=60.0)
        D = fat.damage_per_year()
        assert D > 0.0
        assert math.isfinite(D)

    def test_fatigue_life_positive(self, ref_input):
        """Finite stress above limit → fatigue life > 0 years."""
        fat = self._build_with_stress(ref_input, stress_mpa=60.0)
        life = fat.fatigue_life_years()
        assert life > 0.0

    def test_seawater_cp_worse_than_air(self, ref_input):
        """Seawater with CP gives more damage (fewer allowable cycles) than in-air."""
        from dataclasses import replace
        inp_sw = ref_input
        inp_air = replace(ref_input, environment=EnvironmentType.IN_AIR)
        D_sw = SpanFatigueDamage(inp_sw, 0.43, 60.0).damage_per_year()
        D_air = SpanFatigueDamage(inp_air, 0.43, 60.0).damage_per_year()
        assert D_sw > D_air

    def test_higher_stress_shorter_life(self, ref_input):
        """Higher stress range → shorter fatigue life."""
        fat_low = self._build_with_stress(ref_input, stress_mpa=50.0)
        fat_high = self._build_with_stress(ref_input, stress_mpa=80.0)
        assert fat_high.fatigue_life_years() < fat_low.fatigue_life_years()

    def test_fixed_fixed_cbc_gives_higher_stress_at_same_amplitude(
        self, ref_input, ff_input
    ):
        """Fixed-fixed stress > pinned-pinned at the same A/D.

        Validates the Cbc boundary-condition curvature factor:
          Cbc_pp=1.0, Cbc_ff=2.3 → σ_ff/σ_pp ≈ 2.3 for equal amplitude.

        Note: for the same span length and current velocity, the higher fixed-fixed
        natural frequency may lower the reduced velocity and thus the actual VIV
        amplitude — but the stress PER UNIT AMPLITUDE is always higher for ff.
        """
        freq_pp = SpanNaturalFrequency(ref_input).compute()
        freq_ff = SpanNaturalFrequency(ff_input).compute()
        Ks_dummy = 0.3  # same for both; we're testing Cbc, not amplitude
        amp_pp = SpanVIVAmplitude(ref_input, Ks_dummy, freq_pp.fn_IL_hz,
                                  freq_pp.fn_CF_hz, freq_pp.EI_Nm2,
                                  freq_pp.m_effective_kgm)
        amp_ff = SpanVIVAmplitude(ff_input, Ks_dummy, freq_ff.fn_IL_hz,
                                  freq_ff.fn_CF_hz, freq_ff.EI_Nm2,
                                  freq_ff.m_effective_kgm)
        A_over_D = 0.5  # controlled equal amplitude
        sigma_pp = amp_pp.stress_from_amplitude(A_over_D, freq_pp.fn_CF_hz, mode="CF")
        sigma_ff = amp_ff.stress_from_amplitude(A_over_D, freq_ff.fn_CF_hz, mode="CF")
        assert sigma_ff > sigma_pp, (
            f"Fixed-fixed stress ({sigma_ff:.1f} MPa) should exceed "
            f"pinned-pinned ({sigma_pp:.1f} MPa) at same A/D"
        )

    def test_dnv_f_class_wired_correctly(self, ref_input):
        """DNV F-class (in air): N = 1.73e11 * S^-3. Verify allowable cycles."""
        from dataclasses import replace
        inp_air = replace(ref_input, environment=EnvironmentType.IN_AIR)
        fat = SpanFatigueDamage(inp_air, 0.43, 100.0)  # 100 MPa, in air
        N = fat.allowable_cycles(100.0)
        # Expected: A * S^(-m) = 1.73e11 * 100^(-3) = 1730
        expected_N = 1.73e11 * 100.0**(-3.0)
        assert abs(N - expected_N) / expected_N < 0.01, (
            f"N={N:.1f} vs expected {expected_N:.1f} — check DNV F-class wiring"
        )


# ---------------------------------------------------------------------------
# Allowable Span Length tests (F105 Sec 4.2 Level 1)
# ---------------------------------------------------------------------------

class TestSpanAllowableLength:
    def test_allowable_span_positive(self, ref_input):
        calc = SpanAllowableLength(ref_input, submerged_weight_N_m=850.0)
        assert calc.max_span_m() > 0.0

    def test_allowable_span_less_than_50m_for_10inch(self, ref_input):
        """Level 1 allowable span < 50 m for 10" pipeline at standard conditions."""
        calc = SpanAllowableLength(ref_input, submerged_weight_N_m=850.0)
        assert calc.max_span_m() < 50.0

    def test_heavier_pipe_shorter_span(self, below_onset_input):
        """Heavier submerged weight → shorter allowable span (static criterion).

        Uses low velocity so the static deflection criterion governs over the
        frequency onset criterion (freq limit is > 30 m at Uc=0.1 m/s).
        """
        calc_light = SpanAllowableLength(below_onset_input, submerged_weight_N_m=500.0)
        calc_heavy = SpanAllowableLength(below_onset_input, submerged_weight_N_m=1500.0)
        assert calc_heavy.max_span_m() < calc_light.max_span_m()

    def test_stiffer_pipe_longer_span(self, ref_input):
        """Higher E → longer allowable span."""
        from dataclasses import replace
        inp_stiff = replace(ref_input, e_modulus_pa=300e9)
        calc_std = SpanAllowableLength(ref_input, submerged_weight_N_m=850.0)
        calc_stiff = SpanAllowableLength(inp_stiff, submerged_weight_N_m=850.0)
        assert calc_stiff.max_span_m() > calc_std.max_span_m()


# ---------------------------------------------------------------------------
# Integration: FreespanVIVFatigue facade
# ---------------------------------------------------------------------------

class TestFreespanVIVFatigue:
    def test_assess_returns_result(self, ref_input):
        facade = FreespanVIVFatigue(ref_input, submerged_weight_N_m=850.0)
        result = facade.assess()
        assert result.fn_IL_hz > 0
        assert result.fn_CF_hz > 0
        assert result.Ks > 0
        assert result.allowable_span_m > 0
        assert result.span_length_m == ref_input.span_length_m

    def test_span_utilization_ratio(self, ref_input):
        """Utilization = span_length / allowable_span."""
        facade = FreespanVIVFatigue(ref_input, submerged_weight_N_m=850.0)
        result = facade.assess()
        expected = result.span_length_m / result.allowable_span_m
        assert abs(result.span_utilization - expected) < 1e-9

    def test_assess_wrk607_example_inputs(self):
        """WRK-607 spec example: 10.75" pipe, L=45 m, Uc=0.8 m/s."""
        inp = PipeSpanInput(
            od_m=0.2731,
            wt_m=0.0127,
            span_length_m=45.0,
            content_density_kgm3=900.0,
            current_velocity_ms=0.8,
            seabed_gap_m=0.5,
        )
        facade = FreespanVIVFatigue(inp, submerged_weight_N_m=850.0)
        result = facade.assess()
        # fn < 0.5 Hz for 45m span (physics check)
        assert result.fn_IL_hz < 0.5
        # span utilization ratio positive
        assert result.span_utilization > 0
