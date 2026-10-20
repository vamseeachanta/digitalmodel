"""Numerical and invalid-domain regressions for audit sections 10-19."""

import numpy as np
import pytest
from hypothesis import given, settings, strategies as st
from pydantic import ValidationError
from digitalmodel.orcawave.wave_spectrum import (
    SpectrumParameters,
    SpectrumResult,
    generate_spectrum,
    jonswap,
    issc_spectrum,
    compute_spectral_moments,
    spectral_periods,
    torsethaugen,
    heuristic_two_peak,
)
from digitalmodel.orcawave.motion_statistics import (
    ResponseSpectrum,
    short_term_statistics,
    rayleigh_quantile,
)
from digitalmodel.orcawave.rao_processing import RAOTable, compare_raos
from digitalmodel.orcawave.hydro_coefficients import (
    HydroMatrix6x6,
    create_hydrostatic_restoring,
)
from digitalmodel.orcawave.drift_forces import (
    compute_wind_current_drift,
    MeanDriftCoefficients,
)
from digitalmodel.orcawave.vessel_database import (
    list_vessels,
    get_vessel,
    get_vessels_by_type,
    get_representative_raos,
    VesselParameters,
)


@pytest.mark.parametrize("gamma", [0, -1, float("nan"), float("inf")])
def test_10_gamma_validation(gamma):
    with pytest.raises(ValidationError):
        SpectrumParameters(hs=3, tp=10, gamma=gamma)


def test_10_unusual_gamma_warning():
    with pytest.warns(UserWarning, match="1-7"):
        SpectrumParameters(hs=3, tp=10, gamma=0.5)


@given(st.floats(0.1, 12), st.floats(3, 25), st.floats(1, 7))
@settings(max_examples=40, deadline=None)
def test_10_finite_results(hs, tp, gamma):
    result = generate_spectrum(SpectrumParameters(hs=hs, tp=tp, gamma=gamma))
    for field in result.model_dump().values():
        assert np.all(np.isfinite(field))


def test_10_result_rejects_nan():
    with pytest.raises(ValidationError):
        SpectrumResult(
            frequencies=[0.1, 0.2],
            spectral_density=[1, float("nan")],
            hs_computed=1,
            tp_computed=1,
            tz=1,
            t01=1,
        )


@pytest.mark.parametrize("dimension", ["period", "heading"])
def test_11_disjoint_raos(dimension):
    a = RAOTable(
        dof="Heave", periods=[10], headings_deg=[0], amplitudes=[[1]], phases_deg=[[0]]
    )
    b = a.model_copy(
        update={"periods": [20]} if dimension == "period" else {"headings_deg": [90]}
    )
    with pytest.raises(ValueError, match="overlap"):
        compare_raos(a, b)


@pytest.mark.parametrize(
    "probability,sigma",
    [(0, 1), (-1, 1), (1.1, 1), (0.1, 0), (0.1, -1), (float("nan"), 1)],
)
def test_11_invalid_quantile(probability, sigma):
    with pytest.raises(ValueError):
        rayleigh_quantile(probability, sigma)


@pytest.mark.parametrize("moment", ["m0", "m2", "m4"])
@pytest.mark.parametrize("value", [0, -1])
def test_11_invalid_moments(moment, value):
    values = dict(m0=1, m2=1, m4=1)
    values[moment] = value
    with pytest.raises(ValueError):
        short_term_statistics(
            ResponseSpectrum(frequencies=[], spectral_density=[], **values)
        )


@pytest.mark.parametrize(
    "omega",
    [np.array([]), np.array([1, 0.5]), np.array([1, 1]), np.array([0.1, np.nan])],
)
def test_11_bad_grid(omega):
    with pytest.raises(ValueError):
        compute_spectral_moments(omega, np.ones_like(omega))


@pytest.mark.parametrize(
    "kwargs", [dict(omega_min=2, omega_max=1), dict(n_freq=0), dict(omega_min=0)]
)
def test_11_generation_grid(kwargs):
    with pytest.raises(ValueError):
        generate_spectrum(SpectrumParameters(hs=2, tp=10), **kwargs)


def test_12_jonswap_published_ordinates():
    # DNV-RP-C205 (2014) 3.5.5, angular-frequency expression, A_gamma=.6573442515.
    omega = np.linspace(0.01, 20, 200000)
    actual = jonswap(omega, 4, 10, 3.3)
    wp = 2 * np.pi / 10
    for w in [0.5, wp, 0.8, 1.2]:
        i = np.argmin(abs(omega - w))
        w = omega[i]
        sigma = 0.07 if w <= wp else 0.09
        expected = (
            0.6573442515484114
            * (5 / 16)
            * 16
            * wp**4
            / w**5
            * np.exp(-1.25 * (wp / w) ** 4)
            * 3.3 ** np.exp(-0.5 * ((w - wp) / (sigma * wp)) ** 2)
        )
        assert actual[i] == pytest.approx(expected, rel=1e-9)
    # Independent continuous-model variance (A_gamma is an approximation).
    assert np.trapz(actual, omega) == pytest.approx(1.002416202474, rel=0.0001)


def test_12_issc_period_ratio():
    omega = np.linspace(0.01, 20, 200000)
    s = issc_spectrum(omega, 4, 10)
    tz = spectral_periods(compute_spectral_moments(omega, s))["Tz"]
    tp = 2 * np.pi / omega[np.argmax(s)]
    assert tp / tz == pytest.approx(1.40771, rel=0.001)


def test_14_beam_wind_current_values():
    r = compute_wind_current_drift(20, 1, 90)
    assert r.wind_force_sway_kn == pytest.approx(588, rel=1e-9)
    assert r.current_force_sway_kn == pytest.approx(922.5, rel=1e-9)
    assert r.wind_moment_yaw_knm == pytest.approx(14700, rel=1e-9)
    assert r.current_moment_yaw_knm == pytest.approx(23062.5, rel=1e-9)


def test_15_hydrostatic_values_and_assumptions():
    c = np.asarray(create_hydrostatic_restoring(10000, 3000, 2, 100))
    assert c[2, 2] == pytest.approx(30165750, rel=1e-12)
    assert c[3, 3] == pytest.approx(201105000, rel=1e-12)
    assert c[4, 4] == pytest.approx(10055250000, rel=1e-12)
    assert c[2, 4] == c[4, 2] == 0
    assert "centre of flotation" in create_hydrostatic_restoring.__doc__


def test_15_asymmetry_preserved_for_gate():
    m = np.eye(6)
    m[0, 1] = 1
    result = HydroMatrix6x6(frequency_rad_s=1, values=m.tolist())
    assert result.values[0][1] == 1
    assert result.values[1][0] == 0


@pytest.mark.parametrize(
    "key,hand_checked",
    zip(list_vessels(), [160310.0, 79376.0, 92678.04, 13837.5, 115385.48]),
    ids=["entry1", "entry2", "entry3", "entry4", "entry5"],
)
def test_17_database_consistency(key, hand_checked):
    v = get_vessel(key)
    expected = 1.025 * v.length_m * v.beam_m * v.draft_m * v.block_coefficient
    assert v.displacement_te == pytest.approx(hand_checked, rel=1e-12)
    assert v.displacement_te == pytest.approx(expected, rel=1e-9)
    with pytest.raises(ValidationError, match="Displacement"):
        VesselParameters(**(v.model_dump() | {"displacement_te": 2 * expected}))


@pytest.mark.parametrize(
    "kind,roll_damping,pitch_damping",
    [
        ("FPSO", 0.05, 0.08),
        ("SemiSubmersible", 0.03, 0.12),
        ("Drillship", 0.04, 0.08),
        ("Barge", 0.06, 0.10),
        ("LNGC", 0.04, 0.08),
    ],
)
def test_17_rotation_at_25_seconds(kind, roll_damping, pitch_damping):
    result = get_representative_raos(kind)
    natural = get_vessels_by_type(kind)[0].natural_periods
    index = result.periods.index(25)
    slope = (2 * np.pi / 25) ** 2 / 9.81 * 180 / np.pi
    assert result.synthetic
    for mode, damping, values, heading in [
        ("roll_s", roll_damping, result.roll_rao, 2),
        ("pitch_s", pitch_damping, result.pitch_rao, 0),
    ]:
        ratio = natural[mode] / 25
        amplification = 1 / np.hypot(1 - ratio**2, 2 * damping * ratio)
        # T=25 s is still on the short-period side when Tn=50 s.
        excitation = min(slope, 4 * np.pi**2 / (9.81 * natural[mode]**2) * 180 / np.pi)
        assert values[heading][index] == pytest.approx(excitation * amplification, rel=1e-12)


def test_17_roll_resonance_is_not_capped_at_wave_slope():
    result = get_representative_raos("FPSO")
    index = result.periods.index(14)
    slope = (2 * np.pi / 14) ** 2 / 9.81 * 180 / np.pi
    assert result.roll_rao[2][index] == pytest.approx(slope / (2 * 0.05))


@pytest.mark.parametrize(
    "kind", ["FPSO", "SemiSubmersible", "Drillship", "Barge", "LNGC"],
    ids=["entry1", "entry2", "entry3", "entry4", "entry5"],
)
@pytest.mark.parametrize("mode,field,heading", [
    ("roll_s", "roll_rao", 2), ("pitch_s", "pitch_rao", 0),
])
def test_17_short_period_rotations_decay(kind, mode, field, heading):
    result = get_representative_raos(kind)
    natural_period = get_vessels_by_type(kind)[0].natural_periods[mode]
    # At T=3 s all entries are well above the natural frequency. A decaying
    # envelope must be below 30% of the slope at Tn, rather than plateau there.
    slope_at_natural_period = 4 * np.pi**2 / (9.81 * natural_period**2) * 180 / np.pi
    assert getattr(result, field)[heading][0] < 0.3 * slope_at_natural_period


def test_17_pitch_resonance_and_long_wave_limit(monkeypatch):
    vessel = get_vessels_by_type("FPSO")[0].model_copy(deep=True)
    vessel.natural_periods.update(pitch_s=12, roll_s=0.25)
    monkeypatch.setattr(
        "digitalmodel.orcawave.vessel_database.get_vessels_by_type", lambda _: [vessel]
    )
    result = get_representative_raos("FPSO")
    pitch_index = result.periods.index(12)
    pitch_slope = (2 * np.pi / 12) ** 2 / 9.81 * 180 / np.pi
    assert result.pitch_rao[0][pitch_index] == pytest.approx(pitch_slope / (2 * 0.08))
    # T/Tn=100: amplification tends to one, so rotation tends to wave slope.
    slope_25 = (2 * np.pi / 25) ** 2 / 9.81 * 180 / np.pi
    assert result.roll_rao[2][-1] == pytest.approx(slope_25, rel=0.0002)


@pytest.mark.parametrize("tp", [8, 16])
def test_18_heuristic_explicit(tp):
    omega = np.linspace(0.01, 20, 200000)
    with pytest.raises(ValueError, match="Torsethaugen"):
        torsethaugen(omega, 4, tp)
    with pytest.raises(ValueError, match="HeuristicTwoPeak"):
        generate_spectrum(SpectrumParameters(spectrum_type="Torsethaugen", hs=4, tp=tp))
    actual = heuristic_two_peak(omega, 4, tp)
    # Fixed partition's independent closed-form peak ordinate; frozen values below.
    i = np.argmin(abs(omega - 2 * np.pi / tp))
    assert actual[i] == pytest.approx(
        {8: 3.9220232670, 16: 3.6121905777}[tp], rel=0.0001
    )


def test_19_head_wind_frame():
    r = compute_wind_current_drift(20, 0, 0)
    assert r.wind_force_surge_kn == pytest.approx(-735, rel=1e-9)
    assert r.frame == "x_forward_y_port_z_up"
    assert r.heading_convention == "from_clockwise_bow"
    assert compute_wind_current_drift(20, 0, 180).wind_force_surge_kn == pytest.approx(
        735, rel=1e-9
    )


@pytest.mark.parametrize("heading", [-1, 360, float("nan")])
def test_19_heading_domain(heading):
    with pytest.raises(ValueError):
        compute_wind_current_drift(20, 0, heading)


def test_19_imported_frame_required():
    with pytest.raises(ValidationError):
        MeanDriftCoefficients(
            heading_deg=0,
            frequencies_rad_s=[1],
            surge_kn_m2=[1],
            sway_kn_m2=[1],
            yaw_knm_m2=[1],
        )
