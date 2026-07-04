"""twin A #1373: telemetry ingestion → operating-envelope inputs.

All offline — a synthetic DP-vessel telemetry fixture drives parse -> adapter ->
compute_operating_envelope with no network. Verifies the two channels the envelope
consumes today (measured offset -> offset_pct; measured tension -> tension_n), that
the parked channels (DP state, measured flex-joint angle) do NOT couple into the
engine, the offset-inversion guards, the reused netskip guard, and the fixture
governance rule (provenance + no credentials + no vessel identity).
"""
from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path

import numpy as np
import pytest
import requests
from _pytest.outcomes import Skipped

from digitalmodel.drilling_riser.envelope import (
    EnvelopeCriteria,
    RiserSection,
    SeaState,
    compute_operating_envelope,
)
from digitalmodel.drilling_riser.operability import watch_circle_radius_m
from digitalmodel.drilling_riser.telemetry_inputs import (
    DPState,
    MeasuredResponse,
    TelemetrySnapshot,
    parse_snapshots,
    snapshot_to_offset_pct,
    snapshot_to_tension_n,
    snapshots_to_offset_track,
)
from tests.metocean.netskip import skip_on_network_error

_FIXTURE = Path(__file__).parent / "fixtures" / "telemetry_snapshot_sample.json"


def _fixture():
    return json.loads(_FIXTURE.read_text())


# -- parse ---------------------------------------------------------------------


def test_parse_types_the_fixture_stream():
    snaps = parse_snapshots(_fixture()["records"])
    # 4 records, one has null vessel_offset_m -> 3 typed
    assert len(snaps) == 3
    s = snaps[0]
    assert isinstance(s, TelemetrySnapshot)
    assert isinstance(s.timestamp, datetime)
    assert s.vessel_offset_m == 15.0
    # kN -> N conversion at the parse boundary
    assert s.top_tension_n == 3000.0 * 1000.0
    assert isinstance(s.dp, DPState)
    assert s.dp.total_available_thrust_n == 9000.0 * 1000.0
    assert isinstance(s.measured, MeasuredResponse)
    assert s.measured.flexjoint_angle_lower_deg == 0.9


def test_parse_skips_only_on_missing_offset():
    records = [
        {"vessel_offset_m": None, "top_tension_kn": 3000.0},   # skipped
        {"vessel_offset_m": 20.0},                             # kept; tension/dp optional
        {"vessel_offset_m": 25.0, "vessel_heading_deg": 90.0},  # kept
    ]
    snaps = parse_snapshots(records)
    assert [s.vessel_offset_m for s in snaps] == [20.0, 25.0]
    assert snaps[0].top_tension_n is None and snaps[0].dp is None


def test_dp_station_keeping_margin_is_guarded():
    assert DPState(total_available_thrust_n=9e6, total_commanded_thrust_n=2.7e6).station_keeping_margin == pytest.approx(0.7)
    # all thrusters offline / no capacity -> None, never a ZeroDivisionError
    assert DPState(total_available_thrust_n=0.0, total_commanded_thrust_n=0.0).station_keeping_margin is None
    assert DPState().station_keeping_margin is None


# -- offset inversion ----------------------------------------------------------


def test_offset_pct_is_exact_inverse_of_watch_circle():
    snap = TelemetrySnapshot(vessel_offset_m=30.0)
    pct = snapshot_to_offset_pct(snap, water_depth_m=1500.0)
    assert pct == pytest.approx(2.0)
    # round-trip: pct -> radius -> offset
    assert watch_circle_radius_m(pct, 1500.0) == pytest.approx(30.0)


def test_offset_pct_guards_degenerate_inputs():
    with pytest.raises(ValueError):
        snapshot_to_offset_pct(TelemetrySnapshot(vessel_offset_m=30.0), water_depth_m=0.0)
    with pytest.raises(ValueError):
        snapshot_to_offset_pct(TelemetrySnapshot(vessel_offset_m=-5.0), water_depth_m=1500.0)


def test_offset_track_over_a_window():
    snaps = parse_snapshots(_fixture()["records"])
    track = snapshots_to_offset_track(snaps, water_depth_m=1500.0)
    assert track == [pytest.approx(1.0), pytest.approx(2.0), pytest.approx(15.0)]


# -- offline chain: telemetry -> envelope (non-vacuous) ------------------------


def _section_and_criteria():
    section = RiserSection(outer_diameter_m=0.5334, wall_thickness_m=0.0254)
    # public standard factors (API RP 16Q 2°/4°; API STD 2RD 0.67) — the values the
    # cited getters resolve to; used as literals for offline CI (no wiki), matching
    # the merged metocean test.
    criteria = EnvelopeCriteria(2.0, 4.0, 0.67)
    return section, criteria


def test_measured_offset_drives_operable_to_inoperable_boundary():
    snaps = parse_snapshots(_fixture()["records"])
    wd = 1500.0
    small = snapshot_to_offset_pct(snaps[0], water_depth_m=wd)   # 15 m -> 1%
    large = snapshot_to_offset_pct(snaps[2], water_depth_m=wd)   # 225 m -> 15%
    section, criteria = _section_and_criteria()
    tension = snapshot_to_tension_n(snaps[0])                    # 3.0e6 N
    result = compute_operating_envelope(
        section=section, water_depth_m=wd, length_m=1500.0, tension_n=tension,
        criteria=criteria, offsets_pct=[small, large],
        current_speeds_mps=[0.5], seastates=[SeaState(hs_m=1.5, tp_s=7.0)],
    )
    # the telemetry-derived offset actually discriminates: the boundary is crossed
    small_row = result.allowable_mask[0]
    large_row = result.allowable_mask[1]
    assert small_row.all() and not large_row.any()
    # governing utilisation rises monotonically with the measured offset
    assert result.per_limit_utilisation["flexjoint_angle"][0].max() < \
        result.per_limit_utilisation["flexjoint_angle"][1].max()


def test_measured_tension_feeds_von_mises():
    snaps = parse_snapshots(_fixture()["records"])
    section, criteria = _section_and_criteria()
    kwargs = dict(
        section=section, water_depth_m=1500.0, length_m=1500.0, criteria=criteria,
        offsets_pct=[5.0], current_speeds_mps=[0.5], seastates=[SeaState(1.5, 7.0)],
    )
    measured = compute_operating_envelope(tension_n=snapshot_to_tension_n(snaps[0]), **kwargs)
    lower_tension = compute_operating_envelope(tension_n=2.0e6, **kwargs)
    # the measured tension threads through to the von Mises utilisation surface
    assert not np.array_equal(
        measured.per_limit_utilisation["von_mises"],
        lower_tension.per_limit_utilisation["von_mises"],
    )


# -- coupling guard: parked channels do NOT reach the engine -------------------


def test_parked_channels_do_not_couple_into_the_envelope():
    wd = 1500.0
    section, criteria = _section_and_criteria()
    with_parked = TelemetrySnapshot(
        vessel_offset_m=30.0, top_tension_n=3.0e6,
        dp=DPState(thrusters_online=6, total_available_thrust_n=9e6, total_commanded_thrust_n=2.6e6),
        measured=MeasuredResponse(flexjoint_angle_upper_deg=1.1, flexjoint_angle_lower_deg=1.5),
    )
    without_parked = TelemetrySnapshot(vessel_offset_m=30.0, top_tension_n=3.0e6)
    # the adapter surfaces only offset_pct + tension_n — identical regardless of the
    # parked DP / measured-response fields
    assert snapshot_to_offset_pct(with_parked, water_depth_m=wd) == \
        snapshot_to_offset_pct(without_parked, water_depth_m=wd)
    assert snapshot_to_tension_n(with_parked) == snapshot_to_tension_n(without_parked)

    def run(snap):
        return compute_operating_envelope(
            section=section, water_depth_m=wd, length_m=1500.0,
            tension_n=snapshot_to_tension_n(snap), criteria=criteria,
            offsets_pct=[snapshot_to_offset_pct(snap, water_depth_m=wd)],
            current_speeds_mps=[0.5], seastates=[SeaState(1.5, 7.0)],
        )
    assert np.array_equal(run(with_parked).allowable_mask, run(without_parked).allowable_mask)


# -- reused netskip guard ------------------------------------------------------


def test_netskip_skips_not_fails_on_stream_error():
    """A live telemetry iterator's network error must SKIP, never fail CI.

    Proves the reused ``tests/metocean/netskip`` guard converts a stream
    ``RequestException`` (RetryError on retry exhaustion) into a pytest skip — with
    the iterator consumed INSIDE the guard, per the guard's contract.
    """
    def raising_feed():
        yield {"vessel_offset_m": 10.0}
        raise requests.exceptions.RetryError("simulated retry exhaustion")

    with pytest.raises(Skipped):
        with skip_on_network_error("synthetic telemetry feed"):
            parse_snapshots(list(raising_feed()))  # consume INSIDE the guard


# -- governance: fixture provenance + no credentials + no vessel identity -------


def test_fixture_provenance_no_credentials_no_identity():
    fx = _fixture()
    prov = fx["provenance"]
    assert prov["source"] and prov["licence"]
    raw = _FIXTURE.read_text().lower()
    for forbidden in ("authorization", "x-api-key", "password", "token", "signature=", "aws"):
        assert forbidden not in raw
    # no vessel-identity keys anywhere in the records (fingerprinting surface)
    identity_keys = {"vessel_name", "imo", "mmsi", "callsign", "field", "well", "rig"}
    for rec in fx["records"]:
        assert identity_keys.isdisjoint(rec.keys())
    for tok in ("imo", "mmsi", "callsign", "vessel_name"):
        assert f'"{tok}"' not in raw
