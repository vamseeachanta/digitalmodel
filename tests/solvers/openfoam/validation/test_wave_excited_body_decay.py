"""Tests for the free-decay heave damping analysis (#1332).

ALWAYS (no OpenFOAM): the still-water decay config keeps the verified geometry
while releasing the body from a displacement; the damped-cosine fit recovers a
known ring-down's damping ratio and natural frequency; the reference radiation
damping ratio is physical; and the peak-RAO prediction is monotone in damping
and reproduces the inviscid peak at the radiation value.
"""
from __future__ import annotations

import math

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.validation.wave_excited_body import (
    WaveExcitedBodyConfig,
)
from digitalmodel.solvers.openfoam.validation.wave_excited_body_decay import (
    analyze_ringdown,
    attribution,
    build_decay_config,
    predict_peak_rao,
    reference_damping_ratio,
)


# --------------------------------------------------------------------------- #
#  Still-water decay configuration                                            #
# --------------------------------------------------------------------------- #


def test_decay_config_still_water_and_displaced() -> None:
    cfg = build_decay_config(offset=0.03, end_time=8.0)
    assert cfg.wave_height == 0.0                      # still water
    assert cfg.initial_heave_offset == 0.03
    # body, component and CoM all raised by the offset; water depth unchanged
    base = WaveExcitedBodyConfig(nz=49, nx=250)
    assert cfg.body_extent[2] == pytest.approx(base.body_extent[2] + 0.03)
    assert cfg.comp_extent[2] == pytest.approx(base.comp_extent[2] + 0.03)
    assert cfg.centre_of_mass[2] == pytest.approx(base.centre_of_mass[2] + 0.03)
    assert cfg.depth == base.depth == 0.4
    # component stays inside the tank
    assert cfg.comp_extent[3] < cfg.tank_height


def test_zero_offset_is_a_noop() -> None:
    base = WaveExcitedBodyConfig(nz=49, nx=250)
    zero = build_decay_config(offset=0.0)
    assert zero.body_extent == base.body_extent
    assert zero.centre_of_mass == base.centre_of_mass
    assert zero.comp_extent == base.comp_extent


# --------------------------------------------------------------------------- #
#  Ring-down fit                                                              #
# --------------------------------------------------------------------------- #


@pytest.mark.parametrize("zeta_true", [0.10, 0.20, 0.30])
def test_analyze_decay_recovers_known_ringdown(zeta_true) -> None:
    wn, z_eq, A0 = 7.5, 0.40, 0.03
    wd = wn * math.sqrt(1.0 - zeta_true ** 2)
    t = np.linspace(0.0, 8.0, 1600)
    rng = np.random.default_rng(0)
    z = z_eq + A0 * np.exp(-zeta_true * wn * t) * np.cos(wd * t) \
        + rng.normal(0.0, 3e-5, len(t))
    r = analyze_ringdown(t, z)
    assert r["zeta"] == pytest.approx(zeta_true, abs=0.02)
    assert r["omega_n"] == pytest.approx(wn, rel=0.05)
    assert r["z_eq"] == pytest.approx(z_eq, abs=1e-3)


# --------------------------------------------------------------------------- #
#  Attribution against the potential-flow reference                          #
# --------------------------------------------------------------------------- #


def test_reference_damping_ratio_physical() -> None:
    ref = reference_damping_ratio()
    # light radiation damping for a half-submerged square, resonance ~ 0.84 s
    assert 0.05 < ref["zeta_radiation"] < 0.2
    assert 6.5 < ref["omega_n"] < 8.0
    assert ref["period_n"] == pytest.approx(2 * math.pi / ref["omega_n"])


def test_predict_peak_rao_monotone_and_reproduces_inviscid() -> None:
    ref = reference_damping_ratio()
    # at the radiation damping ratio, the prediction reproduces the frozen peak
    assert predict_peak_rao(ref["zeta_radiation"]) == pytest.approx(2.20, abs=0.05)
    # more damping -> lower peak
    assert predict_peak_rao(0.30) < predict_peak_rao(0.15) < predict_peak_rao(ref["zeta_radiation"])


def test_attribution_light_damping_predicts_resonance() -> None:
    # a near-radiation damping ratio (like the measured CFD ~0.13) must predict
    # a resonance peak far above the forced-sweep near-resonance RAO — i.e. the
    # body would resonate, so the forced reduction is not heavy body damping
    fit = {"zeta": 0.126, "omega_n": 7.45}
    a = attribution(fit, forced_rao_near_resonance=0.744)
    assert a["predicted_peak_rao"] > 1.5
    assert abs(a["omega_n_rel_error"]) < 0.05          # natural frequency matches
    assert a["zeta_excess_ratio"] < 1.5                # only a modest excess
