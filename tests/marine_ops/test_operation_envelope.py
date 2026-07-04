"""Vessel envelope by typical operation (digitalmodel #930)."""

from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DOF,
    FrequencyData,
    HeadingData,
    RAOComponent,
    RAOSet,
)
from digitalmodel.marine_ops.operation_envelope import (
    OPERATIONS,
    available_operations,
    envelope_for_operations,
    operation_envelope,
    plot_envelopes,
    resolve_operation,
)

FREQS = np.array([0.2, 0.4, 0.6, 0.8, 1.0, 1.2])
HEADS = np.array([0.0, 45.0, 90.0, 135.0, 180.0])


def _component(dof: DOF, mag: np.ndarray) -> RAOComponent:
    return RAOComponent(
        dof=dof,
        magnitude=mag,
        phase=np.zeros_like(mag),
        frequencies=FrequencyData(
            values=FREQS, periods=None, count=0, min_freq=0.0, max_freq=0.0
        ),
        headings=HeadingData(values=HEADS, count=0, min_heading=0.0, max_heading=0.0),
        unit="",
    )


def _rao_set(*, heave=1.0, roll=0.0, pitch=0.0, heave_mag=None) -> RAOSet:
    nf, nh = len(FREQS), len(HEADS)
    hm = heave_mag if heave_mag is not None else np.full((nf, nh), heave)
    zero = np.zeros((nf, nh))
    return RAOSet(
        vessel_name="TestFPSO",
        analysis_tool="OrcaWave",
        water_depth=1000.0,
        surge=_component(DOF.SURGE, zero.copy()),
        sway=_component(DOF.SWAY, zero.copy()),
        heave=_component(DOF.HEAVE, hm),
        roll=_component(DOF.ROLL, np.full((nf, nh), roll)),
        pitch=_component(DOF.PITCH, np.full((nf, nh), pitch)),
        yaw=_component(DOF.YAW, zero.copy()),
        created_date="2026-06-21",
    )


def test_available_operations_are_the_four_requested():
    assert set(available_operations()) == {
        "transit",
        "dp_station_keeping",
        "heavy_lift",
        "offloading",
    }


def test_unknown_operation_raises():
    with pytest.raises(KeyError):
        resolve_operation("snorkeling")


def test_constant_heave_rao_gives_closed_form_limit():
    # Heave RAO = 1.0 m/m everywhere, no roll/pitch response -> heave governs.
    # significant heave at Hs_ref=1 is 0.5 m, so Hs_limit = 2*alpha*criterion.
    rao = _rao_set(heave=1.0, roll=0.0, pitch=0.0)
    op = OPERATIONS["transit"]  # alpha 0.90, heave limit 3.0 m
    env = operation_envelope(rao, "transit", tp_range_s=[8.0, 12.0, 16.0])

    expected = 2.0 * op.alpha * 3.0  # = 5.4 m
    for point in env.points:
        assert point.governing_dof == "heave"
        assert point.hs_limit_m == pytest.approx(expected, rel=1e-3)


def test_stricter_operation_gives_lower_envelope():
    rao = _rao_set(heave=1.0)
    transit = operation_envelope(rao, "transit", tp_range_s=[12.0])
    heavy = operation_envelope(rao, "heavy_lift", tp_range_s=[12.0])
    assert heavy.points[0].hs_limit_m < transit.points[0].hs_limit_m


def test_limit_scales_linearly_with_criterion_and_alpha():
    rao = _rao_set(heave=1.0)
    base = operation_envelope(rao, "transit", tp_range_s=[12.0]).points[0].hs_limit_m

    from dataclasses import replace

    from digitalmodel.marine_ops.operation_envelope import MotionLimit, Operation

    doubled = Operation(
        key="x",
        label="x",
        limits=(MotionLimit("heave", 6.0, "m"),),  # 2x the heave limit
        alpha=OPERATIONS["transit"].alpha,
        basis="test",
    )
    env2 = operation_envelope(rao, doubled, tp_range_s=[12.0])
    assert env2.points[0].hs_limit_m == pytest.approx(2.0 * base, rel=1e-3)

    halved_alpha = replace(doubled, alpha=doubled.alpha / 2.0)
    env3 = operation_envelope(rao, halved_alpha, tp_range_s=[12.0])
    assert env3.points[0].hs_limit_m == pytest.approx(base, rel=1e-3)


def test_worst_case_heading_governs():
    # Heave RAO larger at heading index 2 (90 deg) -> that heading limits Hs.
    nf, nh = len(FREQS), len(HEADS)
    mag = np.ones((nf, nh))
    mag[:, 2] = 2.0  # 90 deg responds twice as much
    rao = _rao_set(heave_mag=mag)
    env = operation_envelope(rao, "transit", tp_range_s=[12.0])
    point = env.points[0]
    assert point.governing_heading_deg == pytest.approx(90.0)
    # Twice the RAO -> half the Hs limit vs the uniform RAO=1 case (5.4).
    assert point.hs_limit_m == pytest.approx(5.4 / 2.0, rel=1e-3)


def test_envelope_for_all_operations_and_plot(tmp_path):
    rao = _rao_set(heave=1.0, roll=0.5, pitch=0.3)
    results = envelope_for_operations(rao, tp_range_s=[8.0, 12.0, 16.0])
    assert set(results) == {"transit", "dp_station_keeping", "heavy_lift", "offloading"}
    for env in results.values():
        assert len(env.points) == 3
        assert all(p.hs_limit_m > 0 for p in env.points)

    out = plot_envelopes(results, tmp_path / "fpso_envelope.png")
    # matplotlib is a project dep, so the plot should be written.
    assert out is not None and out.is_file()


def test_envelope_to_dict_roundtrips_points():
    rao = _rao_set(heave=1.0)
    env = operation_envelope(rao, "dp_station_keeping", tp_range_s=[10.0, 14.0])
    d = env.to_dict()
    assert d["operation"] == "dp_station_keeping"
    assert len(d["points"]) == 2
    assert env.hs_limit_at(10.0) == d["points"][0]["hs_limit_m"]
