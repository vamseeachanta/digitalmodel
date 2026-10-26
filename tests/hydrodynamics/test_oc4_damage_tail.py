# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Closed-form moment and damage checks for conditional response envelopes."""

import importlib.util
import json
import math
from pathlib import Path

import numpy as np
import pytest


def helpers():
    path = Path(__file__).with_name("test_oc4_tail_bound.py")
    spec = importlib.util.spec_from_file_location("tail_fixtures", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_constant_rao_closed_form_moments_and_damage_fail_f03(tmp_path):
    fixture = helpers()
    record = fixture.exported(tmp_path, lambda w: np.tile([1, 1, 1, 0, 0, 0], (len(w), 1)))
    report = fixture.load("tail_response_v3").response_bound(
        record, fixture.seas(), band_upper_rad_s=2.44,
        stress_transfer=[1, 0, 0, 0, 0, 0])
    assert report["sn_slope"] == 3
    assert "ASSUMED" in report["sn_slope_status"]
    assert report["wide_band_correction"] == "NOT_IMPLEMENTED"
    for sea, result in zip(fixture.seas(), report["seas"]):
        a = 1.25 * (2 * math.pi / sea["tp_s"])**4
        wave_m0 = sea["hs_m"]**2 / 16
        tail0 = -math.expm1(-a / 2.44**4)
        tail2 = math.sqrt(math.pi * a) * math.erf(math.sqrt(a) / 2.44**2)
        full2 = math.sqrt(math.pi * a)
        expected_damage = 1 - (1 - tail0) * math.sqrt(1 - tail2 / full2)
        for name in ("surge", "sway", "heave", "hangoff_x", "hangoff_y", "hangoff_z", "stress_range_proxy"):
            output = result["outputs"][name]
            scale = 4 if name == "stress_range_proxy" else 1
            assert output["in_band_m2"] == pytest.approx(scale * wave_m0 * (full2 - tail2), rel=1e-12)
            assert output["omitted_m2_upper_bound"] == pytest.approx(scale * wave_m0 * tail2, rel=1e-12)
            assert output["response_variance_tail_fraction"] == pytest.approx(tail0, rel=1e-12)
            assert output["fatigue_damage_tail_fraction"] == pytest.approx(expected_damage, rel=1e-12)
            assert output["fatigue_damage_tail_fraction"] > output["response_variance_tail_fraction"]
            assert output["damage_status"] == "FAIL"
            assert output["threshold_pass"] is False
    assert report["seas"][0]["id"] == "F03"
    assert report["release_eligible"] is False


def test_steep_decay_passes_both_tails(tmp_path):
    fixture = helpers()
    record = fixture.exported(tmp_path, lambda w: np.column_stack([w**-4] * 6), grid_count=1913)
    report = fixture.load("tail_response_v3").response_bound(
        record, fixture.seas(), band_upper_rad_s=2.44,
        stress_transfer=[1, -2, 3, 0, 0, 0])
    for source, sea in zip(fixture.seas(), report["seas"]):
        a = 1.25 * (2 * math.pi / source["tp_s"])**4
        x = a / 2.44**4
        exact0 = source["hs_m"]**2 / 16 * math.exp(-x) * (x*x + 2*x + 2) / a**2
        upper_gamma = (0.75 * math.sqrt(math.pi) * math.erfc(math.sqrt(x))
                       + math.exp(-x) * math.sqrt(x) * (x + 1.5))
        exact2 = source["hs_m"]**2 / 16 * upper_gamma / a**1.5
        assert sea["outputs"]["surge"]["in_band_variance"] == pytest.approx(exact0, rel=1e-4)
        assert sea["outputs"]["surge"]["in_band_m2"] == pytest.approx(exact2, rel=1e-4)
        for output in sea["outputs"].values():
            assert output["response_variance_tail_fraction"] < 0.01
            assert output["fatigue_damage_tail_fraction"] < 0.01
            assert output["variance_status"] == output["damage_status"] == "PASS"
            assert output["threshold_pass"]


def test_boundary_flat_minimum_and_stress_cancellation_envelope(tmp_path):
    fixture = helpers()
    record = fixture.exported(tmp_path, lambda w: np.tile([1, 1, 0, 0, 0, 0], (len(w), 1)))
    report = fixture.load("tail_response_v3").response_bound(
        record, fixture.seas(), band_upper_rad_s=2.44,
        stress_transfer=[1, -1, 0, 0, 0, 0], envelope_multiplier=2)
    assert report["upper_envelope_magnitudes"]["surge"] >= 1
    assert report["upper_envelope_magnitudes"]["stress_range_proxy"] == 8
    stress = report["seas"][0]["outputs"]["stress_range_proxy"]
    assert stress["in_band_variance"] == 0
    assert stress["response_variance_tail_fraction"] == 1
    assert stress["fatigue_damage_tail_fraction"] == 1
    assert not stress["threshold_pass"]


@pytest.mark.parametrize("slope", [2, 3, 5])
def test_slope_uses_damage_moment_formula_and_unrounded_threshold(tmp_path, slope):
    fixture = helpers()
    record = fixture.exported(tmp_path, lambda w: np.ones((len(w), 6)))
    tail = fixture.load("tail_response_v3")
    report = tail.response_bound(record, fixture.seas(), band_upper_rad_s=2.44, sn_slope=slope)
    output = report["seas"][0]["outputs"]["surge"]
    m0, m2 = output["in_band_variance"], output["in_band_m2"]
    total0 = m0 + output["omitted_variance_upper_bound"]
    total2 = m2 + output["omitted_m2_upper_bound"]
    expected = 1 - (m0 / total0)**((slope - 1) / 2) * math.sqrt(m2 / total2)
    assert output["fatigue_damage_tail_fraction"] == pytest.approx(expected)
    for delta, passed in ((-1e-10, False), (1e-10, True)):
        result = tail.response_bound(record, fixture.seas(), band_upper_rad_s=2.44,
                                     sn_slope=slope, threshold=expected + delta)
        assert result["seas"][0]["outputs"]["surge"]["threshold_pass"] is passed


def test_analytic_tail_moments_include_nonzero_lower_tail():
    tail = helpers().load("tail_response_v3")
    a, low, high = 1.25, 0.8, 2.44
    lower, upper = tail.wave_tail_moments(low, high, a)
    assert lower[0] == pytest.approx(math.exp(-a / low**4))
    assert lower[1] == pytest.approx(math.sqrt(math.pi * a) * math.erfc(math.sqrt(a) / low**2))
    assert upper[0] == pytest.approx(-math.expm1(-a / high**4))
    assert upper[1] == pytest.approx(math.sqrt(math.pi * a) * math.erf(math.sqrt(a) / high**2))


def test_synthetic_receipt_is_software_only_and_matches_closed_form():
    fixture = helpers()
    receipt = json.loads((fixture.EXAMPLE / "synthetic-tail-cases-v3.json").read_text())
    assert receipt["status"] == "SYNTHETIC_SOFTWARE_EVIDENCE_NOT_ENGINEERING_RESPONSE"
    for source, sea in zip(fixture.seas(), receipt["cases"]["constant"]["seas"]):
        a = 1.25 * (2 * math.pi / source["tp_s"])**4
        expected0 = -math.expm1(-a / 2.44**4)
        expected2 = math.erf(math.sqrt(a) / 2.44**2)
        output = sea["outputs"]["surge"]
        assert output["response_variance_tail_fraction"] == pytest.approx(expected0)
        assert output["fatigue_damage_tail_fraction"] == pytest.approx(1 - (1 - expected0) * math.sqrt(1 - expected2))
    for case in receipt["cases"].values():
        assert case["release_eligible"] is False
        assert case["stress_transfer"] == [1, 0, 0, 0, 0, 0]
    for sea in receipt["cases"]["steep_decay"]["seas"]:
        assert all(output["threshold_pass"] for output in sea["outputs"].values())
    decaying = receipt["cases"]["variance_only_decay"]["seas"][0]["outputs"]["surge"]
    assert decaying["variance_status"] == "PASS"
    assert decaying["damage_status"] == "FAIL"


@pytest.mark.parametrize("options", [
    {"sn_slope": 0}, {"sn_slope": 0.5}, {"sn_slope": float("nan")},
    {"sn_slope": float("inf")}, {"threshold": 1.1},
    {"stress_transfer": [1, 2]}, {"stress_transfer": [float("nan")] * 6},
    {"stress_transfer": [1j] * 6},
])
def test_damage_rejects_unsupported_parameters(tmp_path, options):
    fixture = helpers()
    record = fixture.exported(tmp_path, lambda w: np.ones((len(w), 6)))
    with pytest.raises(ValueError):
        fixture.load("tail_response_v3").response_bound(record, fixture.seas(), band_upper_rad_s=2.44, **options)
