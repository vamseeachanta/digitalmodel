# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Validate retained native starts without treating them as settled results."""

import hashlib
import importlib.util
import json
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[3]


def load_builder():
    spec = importlib.util.spec_from_file_location(
        "study_builder", REPO / "scripts/cfd/build_calm_water_study.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_coefficient_history_preserves_force_sign_and_normalisation():
    builder = load_builder()
    text = "# Time total_x total_y total_z pressure_x pressure_y pressure_z viscous_x viscous_y viscous_z\n1 -10 0 0 -6 0 0 -4 0 0\n"
    assert builder.coefficient_history(text, density=2, area=5, speed=2) == (
        "iteration,ct,cp,cv\n1,0.5,0.3,0.2\n"
    )


def test_malformed_native_history_is_rejected():
    builder = load_builder()
    with pytest.raises(ValueError):
        builder.coefficient_history("1 nan 0 0 -6 0 0 -4 0 0\n", 2, 5, 2)


def test_criterion_digest_and_order_are_checked():
    builder = load_builder()
    data = b"{}\n"
    record = {
        "criterion_sha256": hashlib.sha256(data).hexdigest(),
        "criterion_recorded_at": "2026-10-14T00:00:00+00:00",
        "timed_start_started_at": "2026-10-14T00:00:01+00:00",
    }
    builder.verify_criterion(record, data)
    with pytest.raises(ValueError):
        builder.verify_criterion(record, b"changed")
    record["criterion_recorded_at"] = "2026-10-14T00:00:02+00:00"
    with pytest.raises(ValueError):
        builder.verify_criterion(record, data)


def test_short_starts_cannot_be_reported_as_qualified_results():
    builder = load_builder()
    manifest = retained_manifest()
    page = builder.render_report(manifest)
    assert "not established" in page
    assert "not available" in page
    assert "Revision history" in page
    assert "Qualified resistance results: 0" in page
    assert "<script" not in page


def test_header_whitespace_normalisation_preserves_numerical_bytes():
    builder = load_builder()
    numerical = "1 -10 0 0 -6 0 0 -4 0 0\n"
    assert builder.normalise_force_header("# Force   \n" + numerical) == (
        "# Force\n" + numerical
    )


def test_half_domain_reduction_doubles_force_and_preserves_pressure_thrust():
    builder = load_builder()
    text = "1 -10 0 0 2 0 0 -12 0 0\n"
    assert builder.coefficient_history(text, 2, 5, 2, half_domain=True) == (
        "iteration,ct,cp,cv\n1,1,-0.2,1.2\n"
    )


def test_normalisation_uses_executed_velocity_not_unrounded_request(tmp_path):
    builder = load_builder()
    field = tmp_path / "U"
    field.write_text("Umean 2.19663;\nmUmean -2.19663;\n")
    assert builder.executed_velocity(field) == 2.19663


def test_velocity_readback_rejects_wrong_flow_direction(tmp_path):
    builder = load_builder()
    field = tmp_path / "U"
    field.write_text("Umean 2.19663;\nmUmean 2.19663;\n")
    with pytest.raises(ValueError):
        builder.executed_velocity(field)


def retained_manifest():
    return json.loads((REPO / "data/cfd/calm_water_study/manifest.json").read_text())


def test_retained_evidence_is_verified_and_report_is_reproducible():
    builder = load_builder()
    manifest = retained_manifest()
    builder.verify_retained(manifest, REPO)
    report = REPO / "docs/domains/openfoam/calm-water-resistance-study.html"
    assert report.read_text() == builder.render_report(manifest)
    assert "indicative" in report.read_text().lower()
    assert "unstable" in report.read_text().lower()
    assert "fine-mesh start is not retained" in report.read_text()


@pytest.mark.parametrize("field,value", [
    ("qualified_results", 1), ("readiness_status", "validated"),
])
def test_retained_starts_reject_qualification_claims(field, value):
    builder = load_builder()
    manifest = retained_manifest()
    manifest[field] = value
    with pytest.raises(ValueError):
        builder.verify_retained(manifest, REPO)
    with pytest.raises(ValueError):
        builder.render_report(manifest)


def test_retained_cost_arithmetic_cannot_drift():
    builder = load_builder()
    manifest = retained_manifest()
    manifest["runs"][0]["projected_full_seconds"] *= 2
    with pytest.raises(ValueError, match="cost"):
        builder.verify_retained(manifest, REPO)


def test_retained_history_digest_cannot_drift():
    builder = load_builder()
    manifest = retained_manifest()
    first = next(iter(manifest["files"].values()))
    first["sha256"] = "0" * 64
    with pytest.raises(ValueError, match="digest"):
        builder.verify_retained(manifest, REPO)
