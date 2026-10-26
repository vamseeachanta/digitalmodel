# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic checks of free-span stress multipliers.

Hand calculation (all beam inputs are artificial, not design recommendations):
D = 0.4 m, t = 0.02 m, L = 10 m, E = 100 GPa, pinned ends.
For y(x) = a sin(pi x/L), peak curvature = a (pi/L)**2 and outer
fibre stress = E (D/2) curvature. The fully reversed range is twice
the peak. Thus A/D = 0.25 gives a = 0.1 m and range = 40 pi**2 MPa;
A/D = 0.5 gives a = 0.2 m and range = 80 pi**2 MPa.

The artificial fatigue law is N = 10**12 / S**4, with S in MPa and no
cut-off. Independently, Miner damage D = n/N = n S**4 / 10**12, hence
D(k S)/D(S) = k**4. Test factors 1.5 and 2 give k = 3 and damage ratio
81; factors below unity exercise the strictly positive input domain.
No standard curve constants or solver outputs supply these expectations.
"""

import math
from dataclasses import replace

import pytest

from digitalmodel.subsea.pipeline.free_span import FreespanVIVFatigue
from digitalmodel.subsea.pipeline.free_span.models import (
    BoundaryConditionF105,
    NaturalFrequencyResult,
    PipeSpanInput,
)
from digitalmodel.subsea.pipeline.free_span.span_fatigue_damage import (
    SpanFatigueDamage,
)
from digitalmodel.subsea.pipeline.free_span.span_natural_frequency import (
    SpanNaturalFrequency,
)
from digitalmodel.subsea.pipeline.free_span.span_viv_response import SpanVIVAmplitude


@pytest.fixture
def beam():
    """Return the artificial pinned beam declared in the hand calculation."""
    return PipeSpanInput(
        od_m=0.4,
        wt_m=0.02,
        span_length_m=10.0,
        e_modulus_pa=100e9,
        bc=BoundaryConditionF105.PINNED_PINNED,
    )


def response(inp):
    """Build EI from annular geometry; other dummy inputs do not enter stress."""
    inner_diameter = inp.od_m - 2 * inp.wt_m
    inertia = math.pi * (inp.od_m**4 - inner_diameter**4) / 64
    return SpanVIVAmplitude(
        inp, Ks=0.0, fn_IL=2.0, fn_CF=3.0,
        EI=inp.e_modulus_pa * inertia, m_e=1.0,
    )


@pytest.mark.parametrize(
    "mode,amplitude,coefficient", [("IL", 0.25, 40), ("CF", 0.5, 80)]
)
def test_uncorrected_stress_matches_hand_calculation(
    beam, mode, amplitude, coefficient
):
    assert response(beam).stress_from_amplitude(amplitude, fn=2.0, mode=mode) == (
        pytest.approx(coefficient * math.pi**2)
    )


@pytest.mark.parametrize(
    "mode,amplitude,coefficient", [("IL", 0.25, 40), ("CF", 0.5, 80)]
)
@pytest.mark.parametrize("scf,gamma", [(1.5, 1.0), (1.0, 2.0), (1.5, 2.0), (0.5, 0.5)])
def test_stress_factors_apply_once(beam, mode, amplitude, coefficient, scf, gamma):
    inp = replace(beam, stress_concentration_factor=scf, gamma_s=gamma)
    expected = coefficient * math.pi**2 * scf * gamma
    assert response(inp).stress_from_amplitude(amplitude, fn=2.0, mode=mode) == (
        pytest.approx(expected)
    )


@pytest.mark.parametrize(
    "mode,amplitude,coefficient", [("IL", 0.25, 40), ("CF", 0.5, 80)]
)
def test_identity_factors_preserve_stress(beam, mode, amplitude, coefficient):
    explicit = replace(beam, stress_concentration_factor=1.0, gamma_s=1.0)
    assert beam.stress_concentration_factor == beam.gamma_s == 1.0
    default_stress = response(beam).stress_from_amplitude(amplitude, fn=2.0, mode=mode)
    explicit_stress = response(explicit).stress_from_amplitude(amplitude, fn=2.0, mode=mode)
    assert explicit_stress == default_stress == pytest.approx(coefficient * math.pi**2)


@pytest.mark.parametrize("mode", ["IL", "CF"])
def test_zero_amplitude_preserves_zero_stress(beam, mode):
    inp = replace(beam, stress_concentration_factor=1.5, gamma_s=2.0)
    assert response(inp).stress_from_amplitude(0.0, fn=2.0, mode=mode) == 0.0


@pytest.mark.parametrize("field", ["stress_concentration_factor", "gamma_s"])
@pytest.mark.parametrize(
    "value", [True, False, 0.0, -1.0, math.nan, math.inf, -math.inf, None, "2"]
)
def test_invalid_stress_factors_are_rejected(beam, field, value):
    """Non-numeric, boolean, nonfinite and nonpositive values are invalid."""
    with pytest.raises(ValueError, match=field):
        replace(beam, **{field: value})


class ArtificialCurve:
    """Declared artificial single slope: N = 10**12 / S**4, no cut-off."""

    fatigue_limit = 0.0

    def get_allowable_cycles(self, stress_range):
        return 1e12 / stress_range**4


@pytest.mark.parametrize("scf,gamma", [(1.5, 1.0), (1.0, 2.0), (1.5, 2.0)])
@pytest.mark.parametrize("active_modes", ["IL", "CF", "both"])
def test_fatigue_uses_corrected_range(beam, monkeypatch, scf, gamma, active_modes):
    """Exercise real stress, damage and facade code with controlled response."""
    # Frequencies 2 and 3 Hz are arbitrary cycle rates, unrelated to a standard.
    inertia = math.pi * (0.4**4 - 0.36**4) / 64
    monkeypatch.setattr(
        SpanNaturalFrequency, "compute",
        lambda self: NaturalFrequencyResult(2.0, 3.0, 1.0, 100e9 * inertia),
    )
    il_amplitude = 0.25 if active_modes in ("IL", "both") else 0.0
    cf_amplitude = 0.5 if active_modes in ("CF", "both") else 0.0
    monkeypatch.setattr(
        SpanVIVAmplitude, "il_amplitude_over_D", lambda self: il_amplitude
    )
    monkeypatch.setattr(
        SpanVIVAmplitude, "cf_amplitude_over_D",
        lambda self, **kwargs: cf_amplitude,
    )
    monkeypatch.setattr(
        SpanFatigueDamage, "_load_sn_curve", lambda self: ArtificialCurve()
    )

    baseline = FreespanVIVFatigue(beam).assess()
    corrected = FreespanVIVFatigue(
        replace(beam, stress_concentration_factor=scf, gamma_s=gamma)
    ).assess()

    il_range = 40 * math.pi**2 if il_amplitude else 0.0
    cf_range = 80 * math.pi**2 if cf_amplitude else 0.0
    # Julian year: 365.25 days, 24 hours/day, 60 minutes/hour, 60 seconds/minute.
    seconds_per_year = 365.25 * 24 * 60 * 60
    expected_damage = seconds_per_year * (2 * il_range**4 + 3 * cf_range**4) / 1e12
    multiplier = scf * gamma
    assert baseline.il_stress_mpa == pytest.approx(il_range)
    assert baseline.cf_stress_mpa == pytest.approx(cf_range)
    assert baseline.damage_per_year == pytest.approx(expected_damage)
    assert corrected.il_stress_mpa == pytest.approx(il_range * multiplier)
    assert corrected.cf_stress_mpa == pytest.approx(cf_range * multiplier)
    assert corrected.damage_per_year == pytest.approx(expected_damage * multiplier**4)
    assert corrected.damage_per_year / baseline.damage_per_year == pytest.approx(
        multiplier**4
    )
    assert corrected.fatigue_life_years == pytest.approx(
        1 / (expected_damage * multiplier**4)
    )
    assert corrected.il_A_over_D == baseline.il_A_over_D == il_amplitude
    assert corrected.cf_A_over_D == baseline.cf_A_over_D == cf_amplitude
