# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Diagnostic reproduction preserves operating-point and oracle distinctions."""

import hashlib
import importlib.util
import json
from pathlib import Path
import runpy

import pytest


SCRIPT = (
    Path(__file__).resolve().parents[2]
    / "scripts/validation/reproduce_holtrop_discrepancy.py"
)


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
        assert row["ct"] == pytest.approx(
            row["viscous_ct"] + row["wave_ct"] + row["ca"]
        )
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
        assert right["tanker_relative_to_series60_pct"] == pytest.approx(
            left["tanker_relative_to_series60_pct"]
        )
    assert "Holtrop-regression" in report["ct_normalization"]


def test_report_accounts_for_each_enabled_reference():
    report = load_script().build_report()
    assert report["qualification_status"] == "NOT_EVALUATED"
    assert {row["id"] for row in report["reference_checks"]} == {
        "series60_cb060",
        "tanker_cb080",
        "form_factor_validation",
    }
    for row in report["reference_checks"]:
        assert all(
            check["status"] == "NOT_EVALUATED" for check in row["checks"].values()
        )
    assert any(
        path.endswith("holtrop_diagnostics.py") for path in report["source_sha256"]
    )


@pytest.mark.parametrize(
    "disabled_ids",
    [
        ("series60_cb060",),
        ("tanker_cb080",),
        ("form_factor_validation",),
        ("series60_cb060", "tanker_cb080", "form_factor_validation"),
    ],
)
def test_command_omits_disabled_calculations_and_reports_unavailable_comparisons(
    disabled_ids, monkeypatch, capsys
):
    module = load_script()
    fixture = module.yaml.safe_load(module.FIXTURE.read_text())
    for case in fixture["test_cases"]:
        if case["id"] in disabled_ids:
            case.update(use_as_test=False, inputs={}, outputs={})
    monkeypatch.setattr(module.yaml, "safe_load", lambda _: fixture)

    runpy.run_path(str(SCRIPT), run_name="__main__")
    report = json.loads(capsys.readouterr().out)
    checks = {row["id"]: row for row in report["reference_checks"]}
    for case_id in disabled_ids:
        assert checks[case_id]["status"] == "DISABLED"
        assert checks[case_id]["checks"] == {}
    resistance_ids = {"series60_cb060", "tanker_cb080"}
    enabled_ids = resistance_ids - set(disabled_ids)
    assert {row["id"] for row in report["fixed_speed"]} == enabled_ids
    assert len(report["common_froude"]) == 4
    for comparison in report["common_froude"]:
        assert {row["id"] for row in comparison["rows"]} == enabled_ids
        for row in comparison["rows"]:
            assert row["froude_number"] == pytest.approx(comparison["froude_number"])
        if resistance_ids.intersection(disabled_ids):
            assert comparison["tanker_relative_to_series60_pct"] is None
            assert comparison["comparison_status"] == "UNAVAILABLE"
            assert "disabled" in comparison["reason"]
        else:
            assert comparison["comparison_status"] == "AVAILABLE"
            assert isinstance(comparison["tanker_relative_to_series60_pct"], float)
    factor = report["form_factor_diagnostic"]
    if "form_factor_validation" in disabled_ids:
        assert factor["status"] == "DISABLED"
        assert factor["computed"] is None
        assert factor["reason"] == "use_as_test is false"
        assert "unverified_fixture_range" not in factor
    else:
        assert factor["computed"] == pytest.approx(
            checks["form_factor_validation"]["checks"]["k1_plus_1_min"]["computed"]
        )


def test_source_digests_use_portable_keys_and_normalized_text(tmp_path, monkeypatch):
    module = load_script()
    paths = [
        module.FIXTURE,
        Path(module.hm.__file__),
        Path(module.hc.__file__),
        Path(module.hd.__file__),
        Path(module.__file__),
    ]
    expected = {}
    for path in paths:
        relative = path.relative_to(module.ROOT)
        normalized = path.read_text(encoding="utf-8").encode("utf-8")
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(normalized)
        expected[relative.as_posix()] = hashlib.sha256(normalized).hexdigest()
    monkeypatch.setattr(module, "FIXTURE", tmp_path / paths[0].relative_to(module.ROOT))
    for source, path in zip((module.hm, module.hc, module.hd, module), paths[1:]):
        monkeypatch.setattr(
            source, "__file__", str(tmp_path / path.relative_to(module.ROOT))
        )
    monkeypatch.setattr(module, "ROOT", tmp_path)
    lf_report = module.build_report()
    for path in paths:
        target = tmp_path / path.relative_to(SCRIPT.parents[2])
        target.write_bytes(target.read_bytes().replace(b"\n", b"\r\n"))
    crlf_report = module.build_report()
    assert lf_report["source_sha256"] == crlf_report["source_sha256"] == expected
    assert all("\\" not in key for key in crlf_report["source_sha256"])
    assert (
        crlf_report["source_sha256_basis"]
        == "UTF-8 text with newlines normalized to LF"
    )
