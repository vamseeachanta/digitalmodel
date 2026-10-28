# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent enumerations and hand calculations; no regional calibration.

Weather expectations enumerate hourly blocks. Load expectations prescribe
k=0.1 /m, h=10 m and solve the dispersion relation for period independently.
The test integrates incident linear-wave pressure and velocity analytically;
these are mathematical checks, not published field validation.
"""

import importlib.util
import json
import math
import runpy
from dataclasses import replace
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pytest


def api():
    """Fail explicitly when the requested module has not been implemented."""
    name = "digitalmodel.marine_ops.preload_weather_window"
    assert importlib.util.find_spec(name) is not None, "weather-window API is absent"
    return __import__(name, fromlist=["WaveSample"])


def samples(heights, start=None, periods=None):
    module = api()
    start = start or datetime(2024, 1, 1, tzinfo=timezone.utc)
    periods = periods or [12.0] * len(heights)
    return tuple(
        module.WaveSample(start + timedelta(hours=i), h, t)
        for i, (h, t) in enumerate(zip(heights, periods))
    )


def windows(data, duration=2, limit=None):
    module = api()
    return module.monthly_windows(
        data,
        duration_h=duration,
        cadence_h=1,
        limit=limit or module.SeaStateLimit(1.0, 5.0, 25.0),
    )


def test_persistence_counts_complete_runs_rather_than_independent_samples():
    # Blocks G G B G G G: five 2 h starts, three wholly good: 3/5.
    january = windows(samples([1, 1, 2, 1, 1, 1]))[0]
    assert january.complete_starts == 5
    assert january.workable_starts == 3
    assert january.probability == pytest.approx(3 / 5)
    assert january.censored_starts == 1
    assert january.lower_probability == january.upper_probability == 3 / 5
    assert windows(samples([1, 1, 2, 1, 1, 1]), duration=3)[0].probability == 1 / 4


def test_month_boundary_uses_following_month_and_missing_month_is_unavailable():
    start = datetime(2024, 1, 31, 23, tzinfo=timezone.utc)
    result = windows(samples([1, 1, 2], start))
    assert len(result) == 12
    assert result[0].probability == 1  # Jan 23:00 -> Feb 01:00.
    assert result[1].probability == 0  # Feb 00:00 -> 02:00 contains a bad block.
    assert result[1].censored_starts == 1
    assert result[2].probability is None


@pytest.mark.parametrize("missing", [None, float("nan")])
def test_missing_blocks_are_unknown_with_explicit_probability_bounds(missing):
    # G ? G G: unknown, unknown, good. Conditional 1/1; bounds [1/3, 1].
    result = windows(samples([1, missing, 1, 1]))[0]
    assert result.unknown_starts == 2
    assert result.complete_starts == 1
    assert result.probability == 1
    assert result.lower_probability == pytest.approx(1 / 3)
    assert result.upper_probability == 1
    assert windows(samples([missing, missing]))[0].probability is None


def test_absent_rows_cannot_bridge_a_gap_and_all_gap_starts_are_counted():
    data = samples([1, 1, 1, 1])
    result = windows((data[0], data[2], data[3]))[0]
    assert result.unknown_starts == 2
    assert result.workable_starts == 1
    assert result.lower_probability == pytest.approx(1 / 3)


def test_noninteger_duration_rounds_up_to_complete_observation_blocks():
    data = samples([1, 1, 2, 1])
    assert windows(data, duration=1)[0].probability == 3 / 4
    assert windows(data, duration=1.0000001)[0].probability == 1 / 3
    assert windows(data, duration=8)[0].probability is None
    assert windows(data, duration=8)[0].censored_starts == 4


def test_period_and_height_limits_are_inclusive_and_state_specific():
    module = api()
    data = samples([1, 1, 1, 1.5], periods=[5, 25, 26, 12])
    assert windows(data, 1)[0].probability == 2 / 4
    assert windows(data, 1, module.SeaStateLimit(1.5, 5, 25))[0].probability == 3 / 4


@pytest.mark.parametrize(
    "duration,cadence", [(0, 1), (-1, 1), (1, 0), (math.inf, 1), (1, math.nan)]
)
def test_invalid_time_controls_fail(duration, cadence):
    with pytest.raises(ValueError):
        api().monthly_windows(
            samples([1, 1]),
            duration_h=duration,
            cadence_h=cadence,
            limit=api().SeaStateLimit(1, 5, 25),
        )


def test_invalid_series_and_limits_fail_without_silent_sorting():
    module = api()
    data = samples([1, 1])
    bad_series = [
        (),
        data[::-1],
        (data[0], data[0]),
        (replace(data[0], timestamp=datetime(2024, 1, 1)),),
        (replace(data[0], hs_m=-1),),
        (replace(data[0], tp_s=0),),
        (replace(data[0], hs_m=math.inf),),
        (data[0], replace(data[1], timestamp=data[1].timestamp + timedelta(minutes=1))),
    ]
    for series in bad_series:
        with pytest.raises(ValueError):
            windows(series)
    for limit in [
        module.SeaStateLimit(-1, 5, 25),
        module.SeaStateLimit(1, 25, 5),
        module.SeaStateLimit(1, 0, 25),
    ]:
        with pytest.raises(ValueError):
            windows(data, limit=limit)


def load_case(**kwargs):
    module = api()
    # For k=0.1, h=10, g=10: omega²=tanh(1), T=2*pi/sqrt(tanh(1)).
    inputs = dict(
        wave_height_m=1,
        period_s=2 * math.pi / math.sqrt(math.tanh(1)),
        mean_depth_m=10,
        tide_level_m=0,
        hull=module.HullGeometry(10, 4, 4, -2),
        leg=module.LegGeometry(2, 1, 2),
        density_kg_m3=1000,
        gravity_m_s2=10,
    )
    inputs.update(kwargs)
    return module.swell_loads(**inputs)


def test_leg_force_matches_integrated_hand_calculation_and_phase_combination():
    result = load_case()
    # u_amp(z)=0.5*omega*cosh(0.1*(z+10))/sinh(1).
    velocity_squared_integral = (
        0.25 * math.tanh(1) / math.sinh(1) ** 2 * (5 + math.sinh(2) / 0.4)
    )
    drag_kn = 0.5 * 1000 * 2 * velocity_squared_integral / 1000
    inertia_kn = 1000 * 2 * math.pi * 0.5 * math.tanh(1) / 0.1 / 1000
    assert result.wavenumber_m_inv == pytest.approx(0.1)
    assert result.leg_drag_amplitude_kn == pytest.approx(drag_kn)
    assert result.leg_inertia_amplitude_kn == pytest.approx(inertia_kn)
    assert inertia_kn > 2 * drag_kn
    assert result.leg_peak_kn == pytest.approx(inertia_kn)
    assert result.three_leg_sum_of_peaks_kn == pytest.approx(3 * inertia_kn)
    assert result.leg_peak_kn < drag_kn + inertia_kn
    # Drag-dominated: max(D*cos² + I*sin) = D + I²/(4D).
    dominant = load_case(leg=api().LegGeometry(2, 100, 2))
    assert dominant.leg_peak_kn == pytest.approx(
        100 * drag_kn + inertia_kn**2 / (400 * drag_kn)
    )


def test_fixed_box_incident_pressure_and_buoyancy_match_hand_integrals():
    result = load_case()
    # Opposite end faces are separated by kL=1 rad; integrate z=-2..0.
    face_kn = (
        1000
        * 10
        * 0.5
        * 4
        * (math.sinh(1) - math.sinh(0.8))
        / (0.1 * math.cosh(1))
        / 1000
    )
    bottom_kn = (
        1000
        * 10
        * 0.5
        * math.cosh(0.8)
        / math.cosh(1)
        * 4
        * 2
        * math.sin(0.5)
        / 0.1
        / 1000
    )
    assert result.hull_horizontal_amplitude_kn == pytest.approx(
        2 * face_kn * math.sin(0.5)
    )
    assert result.hull_vertical_amplitude_kn == pytest.approx(bottom_kn)
    assert result.draft_m == 2
    assert result.buoyancy_kn == 800  # 1000 kg/m³ * 10 m/s² * 10*4*2 m³ / 1000.
    assert result.hull_status == "immersed"
    high = load_case(tide_level_m=0.5)
    assert high.water_depth_m == 10.5
    assert high.draft_m == 2.5
    assert high.buoyancy_kn == 1000
    assert high.hull_horizontal_amplitude_kn != result.hull_horizontal_amplitude_kn


def test_elevated_hull_zero_force_requires_clearance_above_crest():
    module = api()
    dry = load_case(hull=module.HullGeometry(10, 4, 4, 1))
    assert dry.hull_status == "dry"
    assert (
        dry.buoyancy_kn
        == dry.hull_horizontal_amplitude_kn
        == dry.hull_vertical_amplitude_kn
        == 0
    )
    contact = load_case(hull=module.HullGeometry(10, 4, 4, 1), tide_level_m=0.5)
    assert contact.hull_status == "intermittent_contact"
    assert contact.hull_horizontal_amplitude_kn is None
    assert contact.hull_vertical_amplitude_kn is None
    assert contact.leg_peak_kn > 0
    shallow = load_case(hull=module.HullGeometry(10, 4, 4, -0.25))
    assert shallow.hull_status == "intermittent_contact"
    flooded = load_case(hull=module.HullGeometry(10, 4, 2, -2))
    assert flooded.hull_status == "deck_wetting"
    assert flooded.hull_horizontal_amplitude_kn is None


def test_calm_water_and_invalid_physical_inputs():
    calm = load_case(wave_height_m=0)
    assert (
        calm.leg_peak_kn
        == calm.hull_horizontal_amplitude_kn
        == calm.hull_vertical_amplitude_kn
        == 0
    )
    for kwargs in [
        {"wave_height_m": -1},
        {"period_s": 0},
        {"mean_depth_m": math.nan},
        {"tide_level_m": -10},
        {"density_kg_m3": math.inf},
        {"leg": api().LegGeometry(0, 1, 2)},
        {"hull": api().HullGeometry(10, 4, 4, -11)},
        {"wave_height_m": 25},
    ]:
        with pytest.raises(ValueError):
            load_case(**kwargs)


def test_example_writes_offline_plotly_json_and_labels_assumptions(tmp_path):
    example = (
        Path(__file__).resolve().parents[2]
        / "examples/domains/geotechnical/jackup_preload_window.py"
    )
    assert example.is_file(), "offline example is absent"
    namespace = runpy.run_path(str(example))
    namespace["run"](tmp_path)
    result = json.loads((tmp_path / "preload_window_results.json").read_text())
    figure = json.loads((tmp_path / "monthly_preload_windows.plotly.json").read_text())
    assert result["basis"]["kind"] == "assumed_scenario"
    assert result["criteria"]["hold_durations_h"][0] == 1  # Inherited hold floor.
    assert result["qualified_for_installation"] is False
    assert len(figure["data"]) == 6  # Two states * three explicitly assumed holds.
    assert all(len(trace["x"]) == len(trace["y"]) == 12 for trace in figure["data"])
    assert "ASSUMED" in figure["layout"]["title"]["text"]
    assert all("source" in item for item in result["source_register"])
    assert len(result["loads"]) == 12  # Two states * two wave cases * three tides.


def test_supplied_series_requires_matching_digest(tmp_path):
    import hashlib

    namespace = runpy.run_path(
        str(
            Path(__file__).resolve().parents[2]
            / "examples/domains/geotechnical/jackup_preload_window.py"
        )
    )
    source = tmp_path / "observations.json"
    payload = {
        "source": "https://example.org/public-observations",
        "version": "1",
        "dataset_id": "mathematical-test-only",
        "samples": [
            {"timestamp": "2024-01-01T00:00:00+00:00", "hs_m": 1, "tp_s": 12},
            {"timestamp": "2024-01-01T01:00:00+00:00", "hs_m": 1, "tp_s": 12},
        ],
    }
    source.write_text(json.dumps(payload))
    with pytest.raises(ValueError, match="digest"):
        namespace["read_observations"](source, "0" * 64)
    data, basis = namespace["read_observations"](
        source, hashlib.sha256(source.read_bytes()).hexdigest()
    )
    assert len(data) == 2
    assert basis["kind"] == "supplied_unqualified_series"


def test_supplied_missing_data_runs_offline_and_retains_source_bytes(
    tmp_path, monkeypatch
):
    import hashlib
    import socket

    def deny_network(*args, **kwargs):
        raise AssertionError("offline example attempted network access")

    monkeypatch.setattr(socket, "create_connection", deny_network)
    namespace = runpy.run_path(
        str(
            Path(__file__).resolve().parents[2]
            / "examples/domains/geotechnical/jackup_preload_window.py"
        )
    )
    source = tmp_path / "source.json"
    payload = {
        "source": "https://example.org/observations",
        "dataset_id": "test-only",
        "version": "1",
        "samples": [
            {"timestamp": "2024-01-01T00:00:00+00:00", "hs_m": 1, "tp_s": 12},
            {"timestamp": "2024-01-01T01:00:00+00:00", "hs_m": None, "tp_s": 12},
            {"timestamp": "2024-01-01T02:00:00+00:00", "hs_m": 1, "tp_s": 12},
        ],
    }
    raw = (json.dumps(payload, indent=3) + "\n").encode()
    source.write_bytes(raw)
    result = namespace["run"](
        tmp_path / "output",
        observations=source,
        expected_sha256=hashlib.sha256(raw).hexdigest(),
    )
    assert result["basis"]["kind"] == "supplied_unqualified_series"
    assert (tmp_path / "output/preload_window_source.json").read_bytes() == raw
    for rows in result["monthly"].values():
        assert rows[0]["probability_pct"] is None  # Missing or terminally censored.


def test_utc_year_boundary_keeps_a_complete_persistent_window():
    # A local January start is still December in UTC: 01:00 +02 = 23:00 UTC.
    start = datetime(2025, 1, 1, 1, tzinfo=timezone(timedelta(hours=2)))
    rows = windows(samples([1, 1], start))
    assert rows[11].probability == 1
    assert rows[0].probability is None
    assert rows[0].censored_starts == 1
