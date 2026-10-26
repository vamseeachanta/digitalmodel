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


def probe_record(record, magnitudes):
    """Synthetic reload-verified probes preserve the export conventions."""
    import copy
    result = copy.deepcopy(record)
    result["omega_rad_s"] = [2.6, 3.0, 3.5]
    for name in ("added_mass", "damping"):
        result[name] = np.zeros((3, 6, 6)).tolist()
    values = np.zeros((2, 3, 6), complex)
    values[:, :, :3] = np.asarray(magnitudes)[None, :, None] * (0.6 + 0.8j)
    for name in ("motion", "load_haskind", "load_diffraction"):
        result[name] = {"real": values.real.tolist(), "imag": values.imag.tolist()}
    return result


def test_probe_decay_passes_where_last_window_flat_bound_fails(tmp_path):
    fixture = helpers()
    def decay(w):
        amplitude = np.exp(-15 * np.maximum(w - 1.94, 0))
        return np.column_stack([amplitude] * 3 + [np.zeros(len(w))] * 3)
    record = fixture.exported(tmp_path, decay)
    tail = fixture.load("tail_response_v3")
    flat = tail.response_bound(record, fixture.seas(), band_upper_rad_s=2.44)
    probes = {"record": probe_record(record, [0.0002, 0.00005, 0.00001]),
              "mesh_max_span_m": 1.72, "mesh_depth_m": 1500}
    bound = tail.response_bound(record, fixture.seas(), band_upper_rad_s=2.44,
                               probe_envelope=probes)
    assert not flat["seas"][0]["outputs"]["surge"]["threshold_pass"]
    assert all(output["threshold_pass"] for sea in bound["seas"] for output in sea["outputs"].values())
    assert bound["release_eligible"] is False
    receipt = bound["probe_envelope"]
    assert receipt["status"] == "INDICATIVE_BOUND_NOT_RESPONSE"
    assert receipt["mesh_valid_upper_rad_s"] < min(receipt["omega_rad_s"][1:])
    assert all(row["shortfall"] > 0 for row in receipt["resolution"])
    assert all(row["panels_per_wavelength"] < 6 for row in receipt["resolution"])
    assert np.all(np.diff(receipt["motion_upper_magnitudes"], axis=0) <= 0)


def test_rising_probe_raises_the_upper_envelope_and_fails(tmp_path):
    fixture = helpers()
    record = fixture.exported(tmp_path, lambda w: np.tile([1, 1, 1, 0, 0, 0], (len(w), 1)))
    bound = fixture.load("tail_response_v3").response_bound(
        record, fixture.seas(), band_upper_rad_s=2.44,
        probe_envelope={"record": probe_record(record, [0.1, 4, 0.01]),
                        "mesh_max_span_m": 1.72, "mesh_depth_m": 1500})
    envelope = np.asarray(bound["probe_envelope"]["motion_upper_magnitudes"])
    assert np.all(envelope[:3, :3] >= 4)
    assert not bound["seas"][0]["outputs"]["surge"]["threshold_pass"]
    assert bound["seas"][0]["outputs"]["surge"]["fatigue_damage_tail_fraction"] > 0.01


def test_probe_steps_integrate_exact_moments_before_hangoff_transfer(tmp_path):
    fixture = helpers()
    motion = np.array([10j, -20j, 0, 2j, 1j, 0])
    record = fixture.exported(tmp_path, lambda w: np.tile(motion, (len(w), 1)))
    result = fixture.load("tail_response_v3").response_bound(
        record, fixture.seas(), band_upper_rad_s=2.44, stress_transfer=[1, -2, 0, 0, 0, 0],
        probe_envelope={"record": probe_record(record, [0.1, 0.01, 0.001]),
                        "mesh_max_span_m": 1.72, "mesh_depth_m": 1500})
    receipt = result["probe_envelope"]
    w = np.array(receipt["omega_rad_s"])
    anchors = np.array(receipt["motion_anchor_magnitudes"])
    upper = np.array(receipt["motion_upper_magnitudes"])
    # The steps bound both ends, hence every convex complex chord magnitude.
    assert np.all(upper[:-1] >= np.maximum(anchors[:-1], anchors[1:]))
    for source, sea in zip(fixture.seas(), result["seas"]):
        a = 1.25 * (2 * math.pi / source["tp_s"])**4
        tails = np.array([[-math.expm1(-a / value**4),
                           math.sqrt(math.pi * a) * math.erf(math.sqrt(a) / value**2)] for value in w])
        weights = tails - np.vstack([tails[1:], np.zeros(2)])
        for name, bound in (("surge", upper[:, 0]), ("hangoff_x", upper[:, 0] + 10 * upper[:, 4]),
                            ("stress_range_proxy", 2 * (upper[:, 0] + 2 * upper[:, 1]))):
            expected = source["hs_m"]**2 / 16 * (weights.T @ bound**2)
            output = sea["outputs"][name]
            assert output["omitted_variance_upper_bound"] == pytest.approx(expected[0], rel=1e-12)
            assert output["omitted_m2_upper_bound"] == pytest.approx(expected[1], rel=1e-12)
        assert sea["outputs"]["hangoff_x"]["in_band_variance"] == 0
        assert sea["outputs"]["hangoff_x"]["fatigue_damage_tail_fraction"] == 1


@pytest.mark.parametrize("damage", ["below_band", "origin", "nan", "span"])
def test_probe_bound_rejects_inconsistent_evidence(tmp_path, damage):
    fixture = helpers()
    record = fixture.exported(tmp_path, lambda w: np.ones((len(w), 6)))
    probes = {"record": probe_record(record, [1, 0.1, 0.01]),
              "mesh_max_span_m": 1.72, "mesh_depth_m": 1500}
    if damage == "below_band":
        probes["record"]["omega_rad_s"][0] = 2.0
    elif damage == "origin":
        probes["record"]["metadata"]["origin_m"] = [0, 0, -1]
    elif damage == "nan":
        probes["record"]["motion"]["real"][0][0][0] = float("nan")
    else:
        probes["mesh_max_span_m"] = 0
    with pytest.raises(ValueError):
        fixture.load("tail_response_v3").response_bound(
            record, fixture.seas(), band_upper_rad_s=2.44, probe_envelope=probes)


def test_spike_at_irregular_estimate_is_flagged_and_effect_is_reported(tmp_path):
    fixture = helpers()
    # 0.001 rad/s spacing includes the supplied estimate exactly.
    record = fixture.exported(tmp_path, lambda w: np.ones((len(w), 6)), grid_count=2391)
    index = int(np.argmin(abs(np.asarray(record["omega_rad_s"]) - 1.983)))
    record["motion"]["real"][0][index][0] = 100
    tail = fixture.load("tail_response_v3")
    raw = tail.response_bound(record, fixture.seas(), band_upper_rad_s=2.44)
    scan = raw["irregular_frequency_window"]
    assert scan["window_rad_s"] == [1.9, 2.45]
    assert scan["flagged_count"] == 1
    assert scan["flagged_points"][0]["omega_rad_s"] == pytest.approx(1.983)
    assert scan["flagged_points"][0]["components"] == ["surge"]
    change = scan["interpolation_effect"][0]["outputs"]["surge"]
    assert change["in_band_variance_change"] < 0
    assert change["response_variance_tail_fraction_change"] > 0
    assert change["fatigue_damage_tail_fraction_change"] != 0
    interpolated = tail.response_bound(record, fixture.seas(), band_upper_rad_s=2.44,
                                       interpolate_spikes=True)
    before = raw["seas"][0]["outputs"]["surge"]["in_band_variance"]
    after = interpolated["seas"][0]["outputs"]["surge"]["in_band_variance"]
    assert after - before == pytest.approx(change["in_band_variance_change"])
    assert interpolated["irregular_frequency_window"]["interpolation_applied"]
    assert interpolated["release_eligible"] is False


def test_smooth_rao_has_no_spike_and_verified_lid_statement_is_explicit(tmp_path):
    fixture = helpers()
    tail = fixture.load("tail_response_v3")
    record = fixture.exported(tmp_path, lambda w: np.ones((len(w), 6)))
    result = tail.response_bound(record, fixture.seas(), band_upper_rad_s=2.44)
    assert result["irregular_frequency_window"]["flagged_count"] == 0
    assert result["irregular_frequency_window"]["status"] == "SCANNED"
    assert all(v == 0 for sea in result["irregular_frequency_window"]["interpolation_effect"]
               for output in sea["outputs"].values() for v in output.values())
    verified = tail.response_bound(record, fixture.seas(), band_upper_rad_s=2.44,
                                   verified_lid_statement="Native lid geometry read back against reference receipt")
    assert verified["irregular_frequency_window"]["status"] == "CALLER_VERIFIED_LID"
    assert verified["irregular_frequency_window"]["verified_lid_statement"]


def test_spike_scan_preserves_complex_phase_and_respects_window():
    tail = helpers().load("tail_response_v3")
    omega = np.array([1.8, 1.9, 1.983, 2.1, 2.45, 2.5, 2.6])
    rao = np.tile((1 + 2j) * omega[:, None], (1, 6))
    original = rao.copy()
    rao[2, 0] *= 20
    rao[5, 1] *= 20  # Outside the declared window.
    receipt, corrected = tail.scan_irregular_spikes(omega, rao)
    assert receipt["flagged_count"] == 1
    assert corrected[2, 0] == pytest.approx(original[2, 0])
    assert corrected[5, 1] == rao[5, 1]
    for options in ({"jump_factor": 1}, {"window": (2.45, 1.9)}, {"window": (float("nan"), 2.4)}):
        with pytest.raises(ValueError):
            tail.scan_irregular_spikes(omega, rao, **options)
