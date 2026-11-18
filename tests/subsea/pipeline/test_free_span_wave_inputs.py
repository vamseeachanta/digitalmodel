# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic sea-state wiring; expected ratios are independent hand calculations.

All geometry, sea states and stub velocities below are synthetic test inputs,
not standards constants or measured data. KC=Uw*Tu/OD and alpha=Uc/(Uc+Uw)
are the dimensionless definitions required by the wave-input interface.
"""

from dataclasses import asdict, replace
from itertools import combinations
from types import SimpleNamespace

import pytest

import digitalmodel.subsea.pipeline.free_span as free_span
from digitalmodel.subsea.pipeline.free_span import FreespanVIVFatigue, PipeSpanInput


@pytest.fixture
def sea_input():
    """Synthetic pipe and sea state, in metres, seconds and metres per second."""
    return PipeSpanInput(
        od_m=0.5, wt_m=0.02, span_length_m=50.0,
        current_velocity_ms=0.3, seabed_gap_m=0.25,
        Hs_m=2.0, Tp_s=8.0, water_depth_m=40.0,
    )


@pytest.fixture
def declared_wave(monkeypatch):
    """Declare Uw=0.6 m/s and Tu=5 s independently of the facade."""
    calls = []

    def compute(**kwargs):
        calls.append(kwargs)
        return SimpleNamespace(Uw_ms=0.6, Tu_s=5.0)

    monkeypatch.setattr(free_span, "compute_wave_velocity", compute)
    return calls


@pytest.fixture
def response_parameters(monkeypatch):
    """Observe the response interface while retaining its real calculation."""
    parameters = []
    original = free_span.SpanVIVAmplitude.cf_amplitude_over_D

    def capture(self, alpha, KC):
        parameters.append((alpha, KC))
        return original(self, alpha=alpha, KC=KC)

    monkeypatch.setattr(free_span.SpanVIVAmplitude, "cf_amplitude_over_D", capture)
    return parameters


def test_wave_inputs_resolve_facade(sea_input, declared_wave):
    before = asdict(sea_input)
    result = FreespanVIVFatigue(sea_input).assess()
    # Ur*fn*OD = Uc+Uw = 0.3+0.6 = 0.9 m/s, by definition of Ur.
    assert result.Ur_IL * result.fn_IL_hz * 0.5 == pytest.approx(0.9)
    assert result.Ur_CF * result.fn_CF_hz * 0.5 == pytest.approx(0.9)
    assert declared_wave == [dict(
        Hs=2.0, Tp=8.0, water_depth_m=40.0,
        pipe_elevation_m=0.25, pipe_od_m=0.5,
    )]
    assert asdict(sea_input) == before
    assert sea_input.wave_velocity_ms is None


def test_explicit_zero_velocity_overrides_sea_state(sea_input, declared_wave):
    explicit = replace(sea_input, wave_velocity_ms=0.0)
    no_sea = replace(explicit, Hs_m=None, Tp_s=None, water_depth_m=None)
    assert FreespanVIVFatigue(explicit).assess() == FreespanVIVFatigue(no_sea).assess()
    assert declared_wave == []


def test_wave_derived_kc_and_alpha(sea_input, declared_wave, response_parameters):
    """Hand calculation: KC=0.6*5/0.5=6; alpha=0.3/(0.3+0.6)=1/3."""
    result = FreespanVIVFatigue(sea_input).assess()
    assert response_parameters == [pytest.approx((1 / 3, 6.0))]
    assert result.cf_A_over_D > 0


@pytest.mark.parametrize(
    "overrides, expected",
    [({}, (1 / 3, 6.0)), ({"alpha": 0.0}, (0.0, 6.0)),
     ({"KC": 0.0}, (1 / 3, 0.0)),
     ({"alpha": 1.0, "KC": 30.0}, (1.0, 30.0))],
)
def test_ratio_overrides_are_independent(
    sea_input, declared_wave, response_parameters, overrides, expected,
):
    """Zero and former default values are explicit overrides when supplied."""
    FreespanVIVFatigue(sea_input, **overrides).assess()
    assert response_parameters == [pytest.approx(expected)]


@pytest.mark.parametrize("velocity", [None, 0.0, 0.4])
@pytest.mark.parametrize(
    "present", [fields for count in (1, 2) for fields in
                combinations(("Hs_m", "Tp_s", "water_depth_m"), count)],
)
def test_incomplete_sea_state_rejected_before_assessment(
    sea_input, declared_wave, monkeypatch, velocity, present,
):
    """Each proper nonempty subset is incomplete, even with explicit velocity."""
    def unexpected(*args, **kwargs):
        pytest.fail("assessment started before sea-state validation")

    monkeypatch.setattr(free_span.SpanNaturalFrequency, "compute", unexpected)
    fields = {name: None for name in ("Hs_m", "Tp_s", "water_depth_m")
              if name not in present}
    inp = replace(sea_input, wave_velocity_ms=velocity, **fields)
    with pytest.raises(ValueError, match="Hs_m.*Tp_s.*water_depth_m"):
        FreespanVIVFatigue(inp).assess()
    assert declared_wave == []


@pytest.mark.parametrize("field", ["Hs_m", "Tp_s", "water_depth_m"])
@pytest.mark.parametrize("value", [0.0, -1.0, float("nan"), float("inf"), -float("inf")])
@pytest.mark.parametrize("velocity", [None, 0.0])
def test_invalid_sea_state_rejected_before_assessment(
    sea_input, declared_wave, monkeypatch, field, value, velocity,
):
    """Synthetic invalid scalar cases must fail before any assessment work."""
    def unexpected(*args, **kwargs):
        pytest.fail("assessment started before sea-state validation")

    monkeypatch.setattr(free_span.SpanNaturalFrequency, "compute", unexpected)
    inp = replace(sea_input, wave_velocity_ms=velocity, **{field: value})
    with pytest.raises(ValueError, match=field + ".*finite.*positive"):
        FreespanVIVFatigue(inp).assess()
    assert declared_wave == []


@pytest.mark.parametrize("overrides", [{}, {"alpha": 0.2, "KC": 0.0}])
def test_explicit_velocity_preserves_manual_ratios(sea_input, declared_wave, overrides):
    """A supplied 0.4 m/s velocity retains the manual assessment contract."""
    explicit = replace(sea_input, wave_velocity_ms=0.4)
    no_sea = replace(explicit, Hs_m=None, Tp_s=None, water_depth_m=None)
    assert (FreespanVIVFatigue(explicit, **overrides).assess()
            == FreespanVIVFatigue(no_sea, **overrides).assess())
    assert declared_wave == []


def test_real_calculator_auto_manual_equivalence(sea_input):
    """Use the standalone calculator, never the facade, for manual Uw and Tu."""
    from digitalmodel.subsea.pipeline.free_span.wave_velocity import compute_wave_velocity

    wave = compute_wave_velocity(
        Hs=2.0, Tp=8.0, water_depth_m=40.0,
        pipe_elevation_m=0.25, pipe_od_m=0.5,
    )
    manual_input = replace(sea_input, wave_velocity_ms=wave.Uw_ms)
    # Independent dimensional definitions: [m/s]*[s]/[m], and a speed ratio.
    manual = FreespanVIVFatigue(
        manual_input, KC=wave.Uw_ms * wave.Tu_s / 0.5,
        alpha=0.3 / (0.3 + wave.Uw_ms),
    ).assess()
    automatic = FreespanVIVFatigue(sea_input).assess()
    assert automatic == manual
    assert automatic.cf_A_over_D > 0
    assert automatic.damage_per_year > 0


def test_repeated_assessment_resolves_current_inputs(
    sea_input, declared_wave, response_parameters,
):
    facade = FreespanVIVFatigue(sea_input)
    first = facade.assess()
    sea_input.current_velocity_ms = 0.6
    sea_input.Hs_m, sea_input.Tp_s, sea_input.water_depth_m = 4.0, 12.0, 60.0
    sea_input.seabed_gap_m = 0.5
    second = facade.assess()
    # Updated alpha=0.6/(0.6+0.6)=1/2; the declared wave still gives KC=6.
    assert response_parameters == [pytest.approx((1 / 3, 6)), pytest.approx((0.5, 6))]
    assert second.Ur_CF > first.Ur_CF
    assert declared_wave[-1] == dict(
        Hs=4.0, Tp=12.0, water_depth_m=60.0,
        pipe_elevation_m=0.5, pipe_od_m=0.5,
    )
    assert len(declared_wave) == 2
    assert sea_input.wave_velocity_ms is None


@pytest.mark.parametrize("overrides", [{}, {"alpha": 0.0, "KC": 30.0}])
def test_current_bins_resolve_ratios_independently(
    sea_input, declared_wave, response_parameters, overrides,
):
    before = asdict(sea_input)
    # Synthetic equal-probability bins. Alpha is 0.2/0.8=1/4 and 0.6/1.2=1/2.
    bins = [(0.2, 0.5), (0.6, 0.5)]
    result = FreespanVIVFatigue(sea_input, **overrides).assess_multi_current(bins)
    expected_ratios = [(0.25, 6.0), (0.5, 6.0)] if not overrides else [(0.0, 30.0)] * 2
    assert response_parameters == [pytest.approx(pair) for pair in expected_ratios]
    manual_results = [FreespanVIVFatigue(
        replace(sea_input, current_velocity_ms=speed, wave_velocity_ms=0.6),
        alpha=alpha, KC=KC,
    ).assess() for (speed, _), (alpha, KC) in zip(bins, expected_ratios)]
    # Hand-weighted damage: each of the two independent manual runs contributes half.
    assert result.damage_per_year == pytest.approx(
        (manual_results[0].damage_per_year + manual_results[1].damage_per_year) / 2
    )
    assert asdict(sea_input) == before


def test_zero_total_speed_has_defined_ratios(sea_input, monkeypatch, response_parameters):
    """Zero Uw gives KC=0; the zero-denominator alpha convention is 1."""
    monkeypatch.setattr(free_span, "compute_wave_velocity",
                        lambda **kwargs: SimpleNamespace(Uw_ms=0.0, Tu_s=0.0))
    result = FreespanVIVFatigue(replace(sea_input, current_velocity_ms=0.0)).assess()
    assert response_parameters == [(1.0, 0.0)]
    assert result.Ur_IL == result.Ur_CF == result.damage_per_year == 0.0


def test_omitted_wave_preserves_legacy_subcalculators(sea_input, declared_wave):
    """Direct onset/response consumers retain the legacy zero-wave limit."""
    omitted = replace(sea_input, Hs_m=None, Tp_s=None, water_depth_m=None)
    explicit = replace(omitted, wave_velocity_ms=0.0)
    assert FreespanVIVFatigue(omitted).assess() == FreespanVIVFatigue(explicit).assess()
    frequency = free_span.SpanNaturalFrequency(omitted).compute()
    flags, amplitudes = [], []
    for inp in (omitted, explicit):
        flags.append(free_span.SpanOnsetScreening(
            inp, frequency.fn_IL_hz, frequency.fn_CF_hz, frequency.m_effective_kgm,
        ).screening_flags())
        amplitude = free_span.SpanVIVAmplitude(
            inp, flags[-1].Ks, frequency.fn_IL_hz, frequency.fn_CF_hz,
            frequency.EI_Nm2, frequency.m_effective_kgm,
        )
        amplitudes.append((amplitude.il_amplitude_over_D(), amplitude.cf_amplitude_over_D()))
    assert flags[0] == flags[1]
    assert amplitudes[0] == amplitudes[1]
    assert declared_wave == []
