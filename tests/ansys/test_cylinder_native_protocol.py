"""Composed validator regression tests using SYNTHETIC, non-native protocol data."""
import pytest

from digitalmodel.ansys.cylinder_results_validation import validate_native_evidence
from tests.ansys.cylinder_synthetic_protocol import synthetic_protocol, e24, keyed

REFERENCE_HASH = "a" * 64  # synthetic identity fixture, not independent agreement


@pytest.fixture(scope="module")
def pressure():
    return synthetic_protocol("ocv-t60-p10-n4")


@pytest.fixture(scope="module")
def control():
    return synthetic_protocol("ocv-zero-t60-n16")


@pytest.mark.parametrize("fixture_name", ["pressure", "control"])
def test_composed_synthetic_protocol_complete(request, fixture_name):
    case, artifacts = request.getfixturevalue(fixture_name)
    result = validate_native_evidence(case, artifacts, REFERENCE_HASH, REFERENCE_HASH)
    assert result["status"] == "COMPLETE", result["errors"]
    assert len(result["values"]) == 63
    assert result["rfy_sum"] == 0
    assert result["engineering_qualified"] is False
    assert "unsupported layouts refuse" in result["native_grammar_compatibility"]
    if fixture_name == "control":
        assert all(value == 0 for value in result["values"].values())
    else:
        assert result["values"]["inner_y120", "sigma_r"] == -10
        assert result["values"]["inner_y120", "u_z"] < 0


def test_invalid_reference_identity_refuses_before_numeric_acceptance(pressure):
    case, artifacts = pressure
    result = validate_native_evidence(case, artifacts, REFERENCE_HASH, "b" * 64)
    assert result["status"] == "INCOMPLETE"
    assert result["values"] == {} and result["rfy_sum"] is None


def changed_artifacts(case, original, fault):
    artifacts = dict(original)
    if fault == "load":
        row = f'{case["pressure_faces"][0]["element_id"]:8d}{4:8d}{1:8d}'
        old = (row + e24("10") * 2).encode()
        artifacts["native.out"] = artifacts["native.out"].replace(
            old, (row + e24("9") + e24("10")).encode(), 1)
    elif fault == "state":
        artifacts["state_values.txt"] = artifacts["state_values.txt"].replace(
            keyed(case["case_token"], 0, "0", "0", "NSET", "1"),
            keyed(case["case_token"], 0, "0", "0", "NSET", "2"), 1)
    elif fault == "config":
        artifacts["native.out"] = artifacts["native.out"].replace(b"RESUPREC = 0", b"RESUPREC = 1", 1)
    elif fault == "recovery":
        marker = b"OCV_STRESS_BEGIN\n"
        before, after = artifacts["native.out"].split(marker)
        after = after.replace(e24("-10").encode(), e24("-9").encode(), 1)
        artifacts["native.out"] = before + marker + after
    elif fault == "precision":
        data = artifacts["station_values.txt"]
        artifacts["station_values.txt"] = data[:72] + b"1.0000000E+00".rjust(24) + data[96:]
    elif fault == "load_window":
        artifacts["native.out"] = artifacts["native.out"].replace(
            b"RESUPREC = 0\n", b"RESUPREC = 0\nSFE,1,4,PRES,1,9,9\n", 1)
    return artifacts


@pytest.mark.parametrize("fault", ["load", "state", "config", "recovery", "precision", "load_window"])
def test_composed_single_fault_refuses(pressure, fault):
    case, artifacts = pressure
    mutated = changed_artifacts(case, artifacts, fault)
    assert mutated != artifacts
    result = validate_native_evidence(case, mutated, REFERENCE_HASH, REFERENCE_HASH)
    assert result["status"] == "INCOMPLETE", fault
    assert result["errors"] and result["values"] == {}
    expected = {"load": "pressure sign/magnitude/face", "state": "state count/result set",
                "config": "RESUPREC", "recovery": "Independent readback",
                "precision": "E24.16 precision", "load_window": "status layout"}
    assert expected[fault] in "; ".join(result["errors"])


def test_complete_control_does_not_accept_surface_load_record(control):
    case, original = control
    artifacts = dict(original)
    row = f'{1:8d}{4:8d}{1:8d}' + e24("0") * 2
    artifacts["native.out"] = artifacts["native.out"].replace(
        b"NO SURFACE LOADS", ("ELEMENT FACE KVAL P1 P2\n" + row).encode())
    result = validate_native_evidence(case, artifacts, REFERENCE_HASH, REFERENCE_HASH)
    assert result["status"] == "INCOMPLETE"
