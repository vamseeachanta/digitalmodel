# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Closed-form response-variance cases through the solver-independent adapter."""

import importlib.util
import json
from pathlib import Path

import numpy as np
import pytest

EXAMPLE = Path(__file__).resolve().parents[2] / "examples/hydrodynamics/oc4-semisub"


def load(name, directory=EXAMPLE):
    spec = importlib.util.spec_from_file_location(name, directory / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def exported(tmp_path, amplitudes, grid_count=479):
    fixtures = load("test_oc4_complex_export", Path(__file__).parent)
    omega = np.linspace(0.05, 2.44, grid_count)
    data = fixtures.sample()
    data.update(omega_rad_s=omega, headings_deg=np.array([0, 180]),
                added_mass=np.zeros((len(omega), 6, 6)), damping=np.zeros((len(omega), 6, 6)))
    motion = np.asarray(amplitudes(omega), complex)
    for key in ("motion", "load_haskind", "load_diffraction"):
        data[key] = np.broadcast_to(motion, (2, len(omega), 6)).copy()
    path = tmp_path / "export.json"
    load("complex_export_v2").export_verified(fixtures.FakeAdapter(data), tmp_path / "fake.native",
                                             path, origin_m=[0, 0, 0])
    return json.loads(path.read_text())


def seas():
    return json.loads((EXAMPLE / "input-specification-v3.json").read_text())["fatigue_seas"]


def test_constant_rao_has_exact_variance_and_no_renormalisation(tmp_path):
    record = exported(tmp_path, lambda w: np.tile([2 + 3j, 1j, 1, 0, 0, 0], (len(w), 1)))
    report = load("tail_response_v3").response_bound(record, seas(), band_upper_rad_s=2.44)
    assert report["renormalised"] is False and report["release_eligible"] is False
    assert report["envelope_status"] == "ASSUMED_NOT_VALIDATED_ABOVE_BAND"
    assert len(report["seas"]) == 3
    for sea, result in zip(seas(), report["seas"]):
        m0 = sea["hs_m"]**2 / 16
        a = 1.25 * (2 * np.pi / sea["tp_s"])**4
        retained = np.exp(-a / 2.44**4) - np.exp(-a / 0.05**4)
        surge = result["outputs"]["surge"]
        assert surge["in_band_variance"] == pytest.approx(13 * m0 * retained, rel=1e-12)
        assert surge["omitted_variance_upper_bound"] == pytest.approx(13 * m0 * (1 - retained))
        assert result["wave_variance_fraction_omitted"] == pytest.approx(1 - retained)
        assert surge["bound_to_in_band_ratio"] == pytest.approx((1 - retained) / retained)
        assert surge["threshold_pass"] is False  # Deliberately fails the ASSUMED 1% criterion.
        assert result["outputs"]["hangoff_x"] == surge
        assert result["annual_fraction"] == sea["annual_fraction"]


def test_decaying_rao_bound_exceeds_closed_form_tail_and_passes_threshold(tmp_path):
    record = exported(tmp_path, lambda w: np.column_stack([w**-2, *[np.zeros(len(w)) for _ in range(5)]]), grid_count=1913)
    report = load("tail_response_v3").response_bound(record, seas(), band_upper_rad_s=2.44)
    for sea, result in zip(seas(), report["seas"]):
        m0 = sea["hs_m"]**2 / 16
        a = 1.25 * (2 * np.pi / sea["tp_s"])**4
        xlo, xhi = a / 0.05**4, a / 2.44**4
        exact_in_band = m0 / a * ((xhi + 1) * np.exp(-xhi) - (xlo + 1) * np.exp(-xlo))
        exact_tail = m0 / a * (1 - (xhi + 1) * np.exp(-xhi))
        surge = result["outputs"]["surge"]
        assert surge["in_band_variance"] == pytest.approx(exact_in_band, rel=5e-5)
        assert surge["omitted_variance_upper_bound"] >= exact_tail
        assert surge["threshold_pass"] is True
    assert report["release_eligible"] is False


def test_complex_point_transform_does_not_credit_cancellation_in_tail(tmp_path):
    record = exported(tmp_path, lambda w: np.tile([10j, -20j, 0, 2j, 1j, 0], (len(w), 1)))
    report = load("tail_response_v3").response_bound(record, seas(), band_upper_rad_s=2.44)
    point = report["seas"][0]["outputs"]
    assert point["hangoff_x"]["in_band_variance"] == 0
    assert point["hangoff_y"]["in_band_variance"] == 0
    assert point["hangoff_x"]["omitted_variance_upper_bound"] > 0
    assert point["hangoff_x"]["bound_to_in_band_ratio"] is None
    assert point["hangoff_x"]["threshold_pass"] is False
    assert report["upper_envelope_magnitudes"]["hangoff_x"] == 20
    assert report["upper_envelope_magnitudes"]["hangoff_y"] == 40


@pytest.mark.parametrize("damage", ["heading", "gamma", "frequency", "units", "phase", "reload",
                                    "frame", "band", "threshold", "multiplier", "duplicate_sea"])
def test_tail_bound_rejects_ambiguous_or_unsupported_inputs(tmp_path, damage):
    record = exported(tmp_path, lambda w: np.ones((len(w), 6), complex))
    selected = seas()
    options = {"band_upper_rad_s": 2.44}
    if damage == "heading":
        options["heading_deg"] = 45
    elif damage == "gamma":
        selected[0]["gamma"] = 3.3
    elif damage == "frequency":
        record["omega_rad_s"][1] = record["omega_rad_s"][0]
    elif damage == "units":
        record["metadata"]["motion_units"][3] = "deg/m"
    elif damage == "phase":
        record["motion"]["imag"][0][0][0] = float("nan")
    elif damage == "reload":
        record["native_reload_verified"] = False
    elif damage == "frame":
        record["metadata"]["coordinate_frame"] = "z down"
    elif damage == "band":
        options["band_upper_rad_s"] = 2.5
    elif damage == "threshold":
        options["threshold"] = -1
    elif damage == "multiplier":
        options["envelope_multiplier"] = 0.9
    else:
        selected.append(selected[0])
    with pytest.raises(ValueError):
        load("tail_response_v3").response_bound(record, selected, **options)


def test_tail_rejects_broadcast_complex_components(tmp_path):
    record = exported(tmp_path, lambda w: np.ones((len(w), 6), complex))
    record["motion"]["imag"] = 0.0
    with pytest.raises(ValueError, match="shape"):
        load("tail_response_v3").response_bound(record, seas(), band_upper_rad_s=2.44)


def test_tail_rejects_sea_without_identity(tmp_path):
    record = exported(tmp_path, lambda w: np.ones((len(w), 6), complex))
    selected = seas()
    del selected[0]["id"]
    with pytest.raises(ValueError, match="sea"):
        load("tail_response_v3").response_bound(record, selected, band_upper_rad_s=2.44)


def test_nonzero_origin_and_heading_preserve_complex_point_phase(tmp_path):
    motion = np.array([1 + 2j, 3 + 4j, 5 + 6j, 0.1j, 0.2 + 0.3j, -0.4j])
    record = exported(tmp_path, lambda w: np.tile(motion, (len(w), 1)))
    # Synthetic metadata change exercises a nonzero reference origin only.
    record["metadata"]["origin_m"] = [2, 3, -4]
    report = load("tail_response_v3").response_bound(record, seas(), band_upper_rad_s=2.44, heading_deg=180)
    point = motion[:3] + np.cross(motion[3:], [-2, -3, -6])
    for sea, result in zip(seas(), report["seas"]):
        a = 1.25 * (2 * np.pi / sea["tp_s"])**4
        m0 = sea["hs_m"]**2 / 16
        retained = np.exp(-a / 2.44**4) - np.exp(-a / 0.05**4)
        for axis, amplitude in zip(("x", "y", "z"), point):
            assert result["outputs"][f"hangoff_{axis}"]["in_band_variance"] == pytest.approx(
                m0 * retained * abs(amplitude)**2, rel=1e-12)
