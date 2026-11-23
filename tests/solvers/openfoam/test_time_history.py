#!/usr/bin/env python3
"""
ABOUTME: Tests for synchronized CFD time-history extraction (#1528, slice 5).
Truth comes from independently constructed synthetic sloshing signals (known
elevation transects, mass/volume balance, flow integrals, roll/response phase
lag), never from the code under test. Also exercises the per-series units/
provenance envelope and the synchronized-time / mass-balance / dry-out /
overflow / impact validation flags.
"""

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.time_history import (
    ExtractionConfig,
    RawCFDOutputs,
    SynchronizedTimeHistory,
    TimeSeriesChannel,
    ValidationFlags,
    extract_time_history,
    phase_lag_deg,
    validate_synchronized_time,
)


DENSITY = 1025.0  # kg/m^3
TANK_VOLUME = 100.0  # m^3


# ============================================================================
# Independent synthetic-fixture builders (the oracle)
# ============================================================================


def _cumtrap(y: np.ndarray, x: np.ndarray) -> np.ndarray:
    """Independent cumulative trapezoid starting at 0 (oracle, not prod code)."""
    dt = np.diff(x)
    incr = 0.5 * (y[1:] + y[:-1]) * dt
    return np.concatenate([[0.0], np.cumsum(incr)])


def _build_raw(
    *,
    duration: float = 20.0,
    fs: float = 10.0,
    roll_amp: float = 0.15,  # rad
    roll_freq: float = 0.25,  # Hz
    response_lag_deg: float = 30.0,
    volume_consistent: bool = True,
    fill_dip: float = 0.0,
    fill_bump: float = 0.0,
    pressure_spike: float = 0.0,
    case_id: str = "synthetic_tank_case_A",
) -> RawCFDOutputs:
    """Build a fully synthetic, de-identified RawCFDOutputs on a common base."""
    n = int(round(duration * fs))
    t = np.arange(n) / fs

    # --- roll (imposed motion) and a phase-lagged response driver ---
    omega = 2.0 * np.pi * roll_freq
    roll = roll_amp * np.sin(omega * t)
    lag_rad = np.deg2rad(response_lag_deg)

    # --- free-surface elevation transect (above nominal), n_pos probes ---
    x_pos = np.array([0.5, 2.0, 4.0, 6.0, 7.5])
    positions = np.column_stack([x_pos, np.full_like(x_pos, 1.0), np.zeros_like(x_pos)])
    # Standing-wave-like shape: amplitude grows toward the ends, lags roll.
    shape = np.cos(np.pi * (x_pos - x_pos.mean()) / (x_pos.max() - x_pos.min()))
    eta = 0.2 * np.outer(np.sin(omega * t - lag_rad), shape)  # (n, n_pos)

    # --- wall pressure at lower/middle/upper probes (Pa), lags roll ---
    p_locs = np.array(
        [[3.0, 1.0, 0.1], [3.0, 1.0, 2.5], [3.0, 1.0, 4.9]]  # z = low/mid/high
    )
    base_p = np.array([4.0e4, 2.0e4, 5.0e3])  # hydrostatic-ish per elevation
    pressure = base_p[None, :] + 3.0e3 * np.sin(omega * t - lag_rad)[:, None]
    if pressure_spike > 0.0:
        pressure[n // 2, 0] += pressure_spike  # single impact spike, lower probe

    # --- inlet/outlet flows (m^3/s) ---
    q_in = 0.5 + 0.1 * np.sin(0.5 * omega * t)
    q_out = 0.4 + 0.1 * np.cos(0.5 * omega * t)

    # --- liquid volume (m^3): consistent with flows unless told otherwise ---
    v0 = 50.0
    if volume_consistent:
        volume = v0 + _cumtrap(q_in - q_out, t)
    else:
        volume = v0 + 5.0 * t  # unrelated to the flows -> residual explodes

    # inject a dry-out dip and/or overflow bump on the fill fraction
    if fill_dip > 0.0:
        volume = volume.copy()
        volume[n // 3] = fill_dip * TANK_VOLUME
    if fill_bump > 0.0:
        volume = volume.copy()
        volume[2 * n // 3] = fill_bump * TANK_VOLUME

    # --- centre of mass (m) ---
    com = np.column_stack(
        [
            3.0 + 0.3 * np.sin(omega * t - lag_rad),
            np.full(n, 1.0),
            1.2 + 0.05 * np.sin(2.0 * omega * t),
        ]
    )

    return RawCFDOutputs(
        case_id=case_id,
        time=t,
        elevation=eta,
        elevation_positions=positions,
        pressure=pressure,
        pressure_locations=p_locs,
        liquid_volume=volume,
        com=com,
        inlet_flow_rate=q_in,
        outlet_flow_rate=q_out,
        roll_angle=roll,
        density=DENSITY,
        tank_volume=TANK_VOLUME,
    )


# ============================================================================
# TimeSeriesChannel envelope: units / provenance / ragged / unsorted
# ============================================================================


def _ok_channel(**over):
    kw = dict(
        name="demo",
        times=np.array([0.0, 1.0, 2.0]),
        values=np.array([1.0, 2.0, 3.0]),
        units="m",
        case_id="c1",
        source_field="alpha.water",
        provenance="synthetic",
    )
    kw.update(over)
    return TimeSeriesChannel(**kw)


@pytest.mark.parametrize("blank", ["units", "case_id", "source_field", "provenance"])
def test_channel_requires_units_and_provenance(blank):
    with pytest.raises(ValueError):
        _ok_channel(**{blank: ""})


def test_channel_ragged_times_values_raises():
    with pytest.raises(ValueError, match="ragged|length|shape"):
        _ok_channel(values=np.array([1.0, 2.0]))  # 2 vs 3 times


def test_channel_unsorted_time_raises():
    with pytest.raises(ValueError, match="increasing|sorted"):
        _ok_channel(times=np.array([0.0, 2.0, 1.0]))


def test_channel_scalar_stats():
    ch = _ok_channel(values=np.array([-2.0, 5.0, 1.0]))
    assert ch.n_samples == 3
    assert ch.peak == 5.0
    assert ch.trough == -2.0
    assert ch.mean == pytest.approx(4.0 / 3.0)
    assert not ch.is_multichannel


def test_channel_multichannel_spatial_reductions():
    vals = np.array([[1.0, 3.0, 2.0], [-1.0, 0.0, 4.0]])  # (2 times, 3 locs)
    ch = _ok_channel(
        times=np.array([0.0, 1.0]),
        values=vals,
        locations=np.array([[0.0, 0, 0], [1, 0, 0], [2, 0, 0]]),
    )
    assert ch.is_multichannel
    np.testing.assert_allclose(ch.spatial_max, [3.0, 4.0])
    np.testing.assert_allclose(ch.spatial_min, [1.0, -1.0])
    np.testing.assert_allclose(ch.spatial_mean, [2.0, 1.0])


# ============================================================================
# Synchronized-time validation
# ============================================================================


def test_validate_synchronized_time_accepts_uniform():
    validate_synchronized_time(np.array([0.0, 0.1, 0.2, 0.3]))


def test_validate_synchronized_time_rejects_unsorted():
    with pytest.raises(ValueError):
        validate_synchronized_time(np.array([0.0, 0.2, 0.1]))


def test_validate_synchronized_time_rejects_nonfinite():
    with pytest.raises(ValueError):
        validate_synchronized_time(np.array([0.0, np.nan, 0.2]))


def test_from_channels_mismatched_base_raises():
    a = _ok_channel(name="a", times=np.array([0.0, 1.0, 2.0]))
    b = _ok_channel(name="b", times=np.array([0.0, 1.0, 2.5]))  # different base
    with pytest.raises(ValueError, match="synchron|time base|mismatch"):
        SynchronizedTimeHistory.from_channels(
            case_id="c1",
            channels={"a": a, "b": b},
            validation=ValidationFlags(
                synchronized_time=True,
                mass_balance_residual=0.0,
                mass_balance_ok=True,
                dry_out=False,
                overflow=False,
                impact=False,
            ),
        )


def test_from_channels_accepts_shared_base():
    t = np.array([0.0, 1.0, 2.0])
    a = _ok_channel(name="a", times=t)
    b = _ok_channel(name="b", times=t.copy(), values=np.array([9.0, 8.0, 7.0]))
    bundle = SynchronizedTimeHistory.from_channels(
        case_id="c1",
        channels={"a": a, "b": b},
        validation=ValidationFlags(
            synchronized_time=True,
            mass_balance_residual=0.0,
            mass_balance_ok=True,
            dry_out=False,
            overflow=False,
            impact=False,
        ),
    )
    np.testing.assert_allclose(bundle.time, t)
    assert bundle.validation.synchronized_time is True


# ============================================================================
# RawCFDOutputs input validation
# ============================================================================


def test_raw_rejects_ragged_channel():
    raw_kwargs = _build_raw()
    bad_vol = raw_kwargs.liquid_volume[:-1]  # one short
    with pytest.raises(ValueError):
        RawCFDOutputs(
            case_id=raw_kwargs.case_id,
            time=raw_kwargs.time,
            elevation=raw_kwargs.elevation,
            elevation_positions=raw_kwargs.elevation_positions,
            pressure=raw_kwargs.pressure,
            pressure_locations=raw_kwargs.pressure_locations,
            liquid_volume=bad_vol,
            com=raw_kwargs.com,
            inlet_flow_rate=raw_kwargs.inlet_flow_rate,
            outlet_flow_rate=raw_kwargs.outlet_flow_rate,
            roll_angle=raw_kwargs.roll_angle,
            density=DENSITY,
            tank_volume=TANK_VOLUME,
        )


# ============================================================================
# Extraction: elevation channels
# ============================================================================


def test_extract_elevation_spatial_reductions():
    raw = _build_raw()
    bundle = extract_time_history(raw)

    fse = bundle.channels["free_surface_elevation"]
    assert fse.units == "m"
    assert fse.is_multichannel
    np.testing.assert_allclose(fse.values, raw.elevation)
    np.testing.assert_allclose(fse.locations, raw.elevation_positions)

    # Independent spatial reductions.
    np.testing.assert_allclose(
        bundle.channels["elevation_max"].values, raw.elevation.max(axis=1)
    )
    np.testing.assert_allclose(
        bundle.channels["elevation_min"].values, raw.elevation.min(axis=1)
    )
    np.testing.assert_allclose(
        bundle.channels["elevation_mean"].values, raw.elevation.mean(axis=1)
    )
    assert bundle.channels["elevation_max"].units == "m"


# ============================================================================
# Extraction: pressure vs elevation and time
# ============================================================================


def test_extract_pressure_channel_and_profile():
    raw = _build_raw()
    bundle = extract_time_history(raw)

    wp = bundle.channels["wall_pressure"]
    assert wp.units == "Pa"
    np.testing.assert_allclose(wp.values, raw.pressure)
    # locations carry the probe elevations (z) for pressure-vs-elevation.
    np.testing.assert_allclose(wp.locations[:, 2], raw.pressure_locations[:, 2])

    # pressure-vs-elevation at a chosen instant = (z sorted, p aligned)
    idx = 5
    z, p = bundle.pressure_vs_elevation(idx)
    assert np.all(np.diff(z) >= 0)  # sorted ascending by elevation
    # lower probe (z=0.1) must read higher pressure than upper (z=4.9)
    assert p[0] > p[-1]


def test_pressure_tap_statistics_reuse():
    raw = _build_raw()
    bundle = extract_time_history(raw)
    stats = bundle.pressure_tap_statistics()
    # one entry per pressure probe, reusing pressure_taps.compute_tap_statistics
    assert len(stats) == raw.pressure.shape[1]
    for s in stats.values():
        assert s.peak >= s.mean


# ============================================================================
# Extraction: mass, volume, fill fraction, mass-balance residual
# ============================================================================


def test_mass_volume_fill_fraction():
    raw = _build_raw()
    bundle = extract_time_history(raw)

    np.testing.assert_allclose(
        bundle.channels["liquid_volume"].values, raw.liquid_volume
    )
    assert bundle.channels["liquid_volume"].units == "m^3"

    np.testing.assert_allclose(
        bundle.channels["liquid_mass"].values, DENSITY * raw.liquid_volume
    )
    assert bundle.channels["liquid_mass"].units == "kg"

    np.testing.assert_allclose(
        bundle.channels["fill_fraction"].values, raw.liquid_volume / TANK_VOLUME
    )
    assert bundle.channels["fill_fraction"].units == "1"


def test_mass_balance_ok_when_volume_consistent_with_flows():
    raw = _build_raw(volume_consistent=True)
    bundle = extract_time_history(raw)
    assert bundle.validation.mass_balance_ok is True
    assert abs(bundle.validation.mass_balance_residual) < 1e-6


def test_mass_balance_flags_violation():
    raw = _build_raw(volume_consistent=False)
    bundle = extract_time_history(raw)
    assert bundle.validation.mass_balance_ok is False
    assert bundle.validation.mass_balance_residual > 1e-2


# ============================================================================
# Extraction: flow rates + integrated volumes
# ============================================================================


def test_flow_rates_and_integrated_volume():
    raw = _build_raw()
    bundle = extract_time_history(raw)

    np.testing.assert_allclose(
        bundle.channels["inlet_flow_rate"].values, raw.inlet_flow_rate
    )
    assert bundle.channels["inlet_flow_rate"].units == "m^3/s"

    expected_in = _cumtrap(raw.inlet_flow_rate, raw.time)
    np.testing.assert_allclose(
        bundle.channels["inlet_volume"].values, expected_in, rtol=1e-9, atol=1e-9
    )
    assert bundle.channels["inlet_volume"].units == "m^3"


# ============================================================================
# Extraction: centre of mass
# ============================================================================


def test_com_channels():
    raw = _build_raw()
    bundle = extract_time_history(raw)
    np.testing.assert_allclose(bundle.channels["com_x"].values, raw.com[:, 0])
    np.testing.assert_allclose(bundle.channels["com_y"].values, raw.com[:, 1])
    np.testing.assert_allclose(bundle.channels["com_z"].values, raw.com[:, 2])
    for k in ("com_x", "com_y", "com_z"):
        assert bundle.channels[k].units == "m"


# ============================================================================
# Extraction: roll angle / rate / phase and phase lag
# ============================================================================


def test_roll_angle_rate_phase_channels():
    raw = _build_raw(roll_amp=0.15, roll_freq=0.25)
    bundle = extract_time_history(raw)

    np.testing.assert_allclose(bundle.channels["roll_angle"].values, raw.roll_angle)
    assert bundle.channels["roll_angle"].units == "rad"
    assert bundle.channels["roll_rate"].units == "rad/s"

    # analytic rate of A*sin(w t) is A*w*cos(w t) (interior points).
    w = 2.0 * np.pi * 0.25
    analytic_rate = 0.15 * w * np.cos(w * raw.time)
    rate = bundle.channels["roll_rate"].values
    np.testing.assert_allclose(rate[2:-2], analytic_rate[2:-2], atol=0.02)

    assert bundle.channels["roll_phase"].units == "rad"


def test_phase_lag_recovers_known_lag():
    fs = 20.0
    f = 0.25
    n = int(round(40.0 * fs))  # integer periods -> bin aligned
    t = np.arange(n) / fs
    w = 2.0 * np.pi * f
    ref = np.sin(w * t)
    lag = 35.0  # degrees; response lags reference
    resp = np.sin(w * t - np.deg2rad(lag))

    got = phase_lag_deg(ref, resp, times=t)
    assert got == pytest.approx(lag, abs=1.0)


def test_bundle_phase_lag_roll_vs_response():
    raw = _build_raw(response_lag_deg=30.0, duration=40.0, fs=20.0)
    bundle = extract_time_history(raw)
    lag = bundle.phase_lag_deg("elevation_mean")
    assert lag == pytest.approx(30.0, abs=3.0)


# ============================================================================
# Validation flags: dry-out / overflow / impact / synchronized
# ============================================================================


def test_dry_out_flag():
    raw = _build_raw(fill_dip=0.005)  # 0.5% fill -> below default dry-out threshold
    bundle = extract_time_history(raw)
    assert bundle.validation.dry_out is True
    assert bundle.validation.overflow is False


def test_overflow_flag():
    raw = _build_raw(fill_bump=1.05)  # 105% fill -> overflow
    bundle = extract_time_history(raw)
    assert bundle.validation.overflow is True


def test_impact_flag():
    cfg = ExtractionConfig(impact_pressure=1.0e5)
    calm = extract_time_history(_build_raw(), config=cfg)
    assert calm.validation.impact is False

    hit = extract_time_history(_build_raw(pressure_spike=2.0e5), config=cfg)
    assert hit.validation.impact is True


def test_synchronized_flag_true_for_extracted_bundle():
    raw = _build_raw()
    bundle = extract_time_history(raw)
    assert bundle.validation.synchronized_time is True


# ============================================================================
# End-to-end: provenance / units on every series + shared time base
# ============================================================================


def test_every_channel_carries_units_and_provenance_on_shared_base():
    raw = _build_raw()
    bundle = extract_time_history(raw)

    assert bundle.channels  # non-empty
    for name, ch in bundle.channels.items():
        assert ch.units, f"{name} missing units"
        assert ch.case_id == raw.case_id, f"{name} wrong case_id"
        assert ch.source_field, f"{name} missing source_field"
        assert ch.provenance, f"{name} missing provenance"
        # every series shares the common time base
        np.testing.assert_allclose(ch.times, bundle.time)


def test_expected_channel_set_present():
    raw = _build_raw()
    bundle = extract_time_history(raw)
    expected = {
        "free_surface_elevation",
        "elevation_max",
        "elevation_min",
        "elevation_mean",
        "wall_pressure",
        "liquid_volume",
        "fill_fraction",
        "liquid_mass",
        "mass_balance_residual",
        "com_x",
        "com_y",
        "com_z",
        "inlet_flow_rate",
        "outlet_flow_rate",
        "inlet_volume",
        "outlet_volume",
        "roll_angle",
        "roll_rate",
        "roll_phase",
    }
    assert expected.issubset(set(bundle.channels))
