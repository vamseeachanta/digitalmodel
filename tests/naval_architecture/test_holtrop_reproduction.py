"""Diagnostic reproduction preserves operating-point and oracle distinctions."""

import importlib.util
from pathlib import Path

import pytest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/validation/reproduce_holtrop_discrepancy.py"


def load_script():
    spec = importlib.util.spec_from_file_location("holtrop_reproduction", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_fixed_speed_diagnostics_preserve_reference_status_and_components():
    report = load_script().build_report()
    assert report["reference_status"] == "unverified_approximate_fixture_values"
    assert len(report["fixed_speed"]) == 2
    for row in report["fixed_speed"]:
        assert row["speed_ms"] == 7.72
        assert row["ct"] == pytest.approx(row["viscous_ct"] + row["wave_ct"] + row["ca"])
        expected_error = 100 * (row["ct"] / row["ct_approx"] - 1)
        assert row["error_pct"] == pytest.approx(expected_error)
        assert row["floor_ct"] <= row["ct"]


def test_common_froude_comparison_uses_length_specific_speed():
    report = load_script().build_report()
    for pair in report["common_froude"]:
        left, right = pair["rows"]
        assert left["froude_number"] == pytest.approx(pair["froude_number"])
        assert right["froude_number"] == pytest.approx(pair["froude_number"])
        assert right["speed_ms"] > left["speed_ms"]


def test_reordered_fixture_preserves_named_separation(monkeypatch):
    import yaml
    module = load_script()
    original = module.build_report()
    fixture = yaml.safe_load(module.FIXTURE.read_text())
    fixture["test_cases"].reverse()
    monkeypatch.setattr(module.yaml, "safe_load", lambda _: fixture)
    report = module.build_report()
    for left, right in zip(original["common_froude"], report["common_froude"]):
        assert right["tanker_relative_to_series60_pct"] == pytest.approx(left["tanker_relative_to_series60_pct"])
    assert "Holtrop-regression" in report["ct_normalization"]
