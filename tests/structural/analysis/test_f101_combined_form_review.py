"""Review evidence and fail-closed labels for the F101 screening envelope."""

import hashlib
import json
import math
from pathlib import Path

import pytest

from digitalmodel.structural.analysis import wall_thickness_mt_report as report
from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    PipeGeometry,
    PipeMaterial,
)

ROOT = Path(__file__).resolve().parents[3]
RECORD = ROOT / "docs/domains/structural/reviews/f101-combined-form-review.json"
DOCUMENT = RECORD.with_suffix(".md")
LABEL = "simplified form; not checked against the standard's text"
SCREENING = "Screening envelope from the moment-tension report; not reconciled with the code strategy"


def test_review_record_parses_and_identifies_the_reviewed_form():
    record = json.loads(RECORD.read_text(encoding="utf-8"))
    assert record["schema_version"] == 1
    assert record["code"] == "DNV-ST-F101"
    assert record["form"]["id"] == "f101-root-sum-square"
    strategy = ROOT / record["form"]["implementation"]
    assert hashlib.sha256(strategy.read_bytes()).hexdigest() == record["form"]["sha256"]
    assert record["form"]["expression"] == "sqrt(m**2 + n**2 + q**2)"
    assert record["status"] in {"passed", "failed"}
    assert record["reason"]
    assert type(record["agreement"]) is bool
    assert (record["status"] == "passed") == record["agreement"]
    assert "author" not in record["source"]
    assert record["source"]["publisher"] == "Massachusetts Institute of Technology"
    assert record["source"]["title"] == "Yield and Plastic Flow"
    assert record["source"]["year"] == 2001
    assert record["source"]["pages"] == [5, 6]
    assert record["source"]["access"]["http_status"] == 200
    assert len(record["source"]["access"]["sha256"]) == 64
    points = record["comparison_points"]
    assert {p["id"] for p in points} == {
        "single-load-limits", "mixed-moment-tension", "pressure-coupling", "report-envelope"
    }
    assert all(type(p["agreement"]) is bool and p["reason"] for p in points)
    if record["status"] == "passed":
        assert all(p["agreement"] for p in points)
    text = DOCUMENT.read_text(encoding="utf-8")
    assert LABEL in text
    assert record["source"]["url"] in text
    assert text.index("## Summary") < text.index("## Revision history") < text.index("## Basis")


def test_mixed_load_counterexample_from_independent_ring_equilibrium():
    """Derive capacities by integrating a thin, perfectly plastic circular ring.

    With mean radius R, wall t and yield stress f, dA=t*R*dtheta.
    A tensile arc of angle pi+2*alpha and compressive arc pi-2*alpha
    gives N=4*f*t*R*alpha and Np=2*pi*f*t*R.
    Integrating stress*y*dA gives M=4*f*t*R**2*cos(alpha),
    Mp=4*f*t*R**2. Hence n=2*alpha/pi and m=cos(pi*n/2).
    This is a zero-pressure, unit-factor, no-hardening hand calculation;
    it is not an expected value obtained from the strategy.
    At n=1/2, equilibrium gives m=1/sqrt(2); the root-sum-square
    unity boundary instead gives sqrt(3)/2, a factor sqrt(3/2) larger.
    """
    point = next(
        p for p in json.loads(RECORD.read_text())["comparison_points"]
        if p["id"] == "mixed-moment-tension"
    )
    example = point["hand_calculation"]
    n = example["normalised_tension"]
    assert n == 0.5
    ring_moment = math.cos(math.pi * n / 2)
    rss_moment = math.sqrt(1 - n * n)
    assert ring_moment == pytest.approx(1 / math.sqrt(2))
    assert rss_moment / ring_moment == pytest.approx(math.sqrt(1.5))
    assert round((rss_moment / ring_moment - 1) * 100, 1) == example["excess_percent"]
    assert point["agreement"] is False


@pytest.mark.parametrize("payload", [
    None, [], "passed", {}, {"status": "failed"}, {"status": "pending"},
    {"status": True}, {"status": "PASSED"}, {"status": "passed"},
    {"schema_version": 1, "code": "other", "status": "passed",
     "form": {"id": "f101-root-sum-square"}},
])
def test_label_fails_closed_for_incomplete_or_unpassed_records(tmp_path, monkeypatch, payload):
    path = tmp_path / "review.json"
    path.write_text(json.dumps(payload), encoding="utf-8")
    monkeypatch.setattr(report, "F101_COMBINED_FORM_REVIEW", path)
    assert report._f101_combined_form_label() == LABEL


@pytest.mark.parametrize("content", [None, "{", b"\xff"])
def test_label_fails_closed_for_unreadable_records(tmp_path, monkeypatch, content):
    path = tmp_path / "review.json"
    if isinstance(content, bytes):
        path.write_bytes(content)
    elif content is not None:
        path.write_text(content, encoding="utf-8")
    monkeypatch.setattr(report, "F101_COMBINED_FORM_REVIEW", path)
    assert report._f101_combined_form_label() == LABEL


@pytest.fixture
def passed_review():
    """Synthetic consistent evidence for software tests, not engineering approval."""
    record = json.loads(RECORD.read_text(encoding="utf-8"))
    record["status"] = "passed"
    record["agreement"] = True
    record["reason"] = "Synthetic passing review for label rendering only."
    record["comparison_points"] = [
        {
            "id": point["id"], "agreement": True,
            "reason": "Synthetic passing comparison for label rendering only.",
        }
        for point in record["comparison_points"]
    ]
    record["next_steps"] = ["Synthetic fixture only; no engineering approval."]
    return record


def _label_for_record(tmp_path, monkeypatch, record):
    path = tmp_path / "review.json"
    path.write_text(json.dumps(record), encoding="utf-8")
    monkeypatch.setattr(report, "F101_COMBINED_FORM_REVIEW", path)
    return report._f101_combined_form_label()


def test_minimal_passed_record_retains_label(tmp_path, monkeypatch):
    record = {
        "schema_version": 1, "code": "DNV-ST-F101", "status": "passed",
        "form": {"id": "f101-root-sum-square"},
    }
    assert _label_for_record(tmp_path, monkeypatch, record) == LABEL


@pytest.mark.parametrize("field", [
    ("review_date",), ("reason",), ("agreement",), ("source",),
    ("comparison_points",), ("form", "implementation"), ("form", "symbol"),
    ("form", "reviewed_revision"), ("form", "sha256"), ("form", "expression"),
    ("form", "terms"), ("form", "wall_basis"), ("form", "normalisation"),
    ("form", "domain"), ("source", "title"), ("source", "series"),
    ("source", "publisher"), ("source", "year"), ("source", "pages"),
    ("source", "locator"), ("source", "url"), ("source", "scope"),
    ("source", "access"), ("source", "access", "retrieved_utc"),
    ("source", "access", "http_status"), ("source", "access", "sha256"),
])
def test_passed_record_requires_evidence(tmp_path, monkeypatch, passed_review, field):
    container = passed_review
    for key in field[:-1]:
        container = container[key]
    del container[field[-1]]
    assert _label_for_record(tmp_path, monkeypatch, passed_review) == LABEL


@pytest.mark.parametrize("field,value", [
    (("schema_version",), True), (("review_date",), "invalid"),
    (("reason",), " "), (("agreement",), False), (("agreement",), "true"),
    (("agreement",), 1), (("source",), []), (("source", "title"), " "),
    (("source", "year"), True), (("source", "pages"), []),
    (("source", "pages"), [0]), (("source", "pages"), [True]),
    (("source", "pages"), "5-6"), (("source", "access"), []),
    (("source", "access", "http_status"), 404),
    (("source", "access", "sha256"), "invalid"),
    (("source", "access", "retrieved_utc"), "invalid"),
    (("comparison_points",), []), (("comparison_points",), {}),
    (("form", "terms"), {}), (("form", "terms", "m"), " "),
    (("form", "implementation"), "other.py"),
    (("form", "symbol"), "OtherStrategy._check_combined_loading"),
    (("form", "reviewed_revision"), "invalid"),
    (("form", "sha256"), "0" * 64), (("form", "sha256"), None),
])
def test_invalid_passed_evidence_retains_label(
    tmp_path, monkeypatch, passed_review, field, value,
):
    container = passed_review
    for key in field[:-1]:
        container = container[key]
    container[field[-1]] = value
    assert _label_for_record(tmp_path, monkeypatch, passed_review) == LABEL


def test_changing_only_failed_record_status_retains_label(tmp_path, monkeypatch):
    record = json.loads(RECORD.read_text(encoding="utf-8"))
    record["status"] = "passed"
    assert _label_for_record(tmp_path, monkeypatch, record) == LABEL


@pytest.mark.parametrize("value", [False, "true", 1, None])
def test_passed_record_requires_every_comparison_to_agree(
    tmp_path, monkeypatch, passed_review, value,
):
    passed_review["comparison_points"][1]["agreement"] = value
    assert _label_for_record(tmp_path, monkeypatch, passed_review) == LABEL


@pytest.mark.parametrize("change", ["missing", "duplicate", "reason", "malformed"])
def test_passed_record_requires_complete_comparison_points(
    tmp_path, monkeypatch, passed_review, change,
):
    points = passed_review["comparison_points"]
    if change == "missing":
        points.pop()
    elif change == "duplicate":
        points.append(points[0].copy())
    elif change == "reason":
        points[0]["reason"] = " "
    else:
        points[0] = None
    assert _label_for_record(tmp_path, monkeypatch, passed_review) == LABEL


@pytest.mark.parametrize("readable", [False, True])
def test_unavailable_or_changed_implementation_retains_label(
    tmp_path, monkeypatch, passed_review, readable,
):
    implementation = tmp_path / "strategy.py"
    if readable:
        implementation.write_bytes(
            (ROOT / passed_review["form"]["implementation"]).read_bytes()
            + b"\n# Synthetic change invalidates the review digest.\n"
        )
    monkeypatch.setattr(report, "F101_COMBINED_FORM_IMPLEMENTATION", implementation)
    assert _label_for_record(tmp_path, monkeypatch, passed_review) == LABEL


@pytest.mark.parametrize("status", ["failed", "passed"])
def test_rendered_f101_envelopes_keep_label_unless_review_passed(
    tmp_path, monkeypatch, status, passed_review,
):
    record = passed_review
    record["status"] = status
    assert _label_for_record(tmp_path, monkeypatch, record) == ("" if status == "passed" else LABEL)
    html = report.generate_mt_report(
        PipeGeometry(outer_diameter=0.4, wall_thickness=0.02),
        PipeMaterial(grade="synthetic", smys=400e6, smts=500e6),
        internal_pressure=2e6, external_pressure=1e6,
        code=DesignCode.DNV_ST_F101,
    )
    assert (LABEL in html) == (status != "passed")
    # Each chart fragment carries the warnings if saved separately from the report.
    assert html.count(SCREENING) == 3
    if status != "passed":
        assert html.count(LABEL) == 3
    assert "The DNV-ST-F101 combined check evaluates the Von Mises" not in html


def test_committed_record_controls_the_label():
    record = json.loads(RECORD.read_text(encoding="utf-8"))
    assert (report._f101_combined_form_label() == LABEL) == (record["status"] != "passed")


@pytest.mark.parametrize("status", ["failed", "pending", "PASSED", True, None])
def test_complete_record_without_explicit_pass_retains_label(tmp_path, monkeypatch, status):
    record = json.loads(RECORD.read_text(encoding="utf-8"))
    record["status"] = status
    path = tmp_path / "review.json"
    path.write_text(json.dumps(record), encoding="utf-8")
    monkeypatch.setattr(report, "F101_COMBINED_FORM_REVIEW", path)
    assert report._f101_combined_form_label() == LABEL


def test_other_routes_do_not_gain_f101_review_labels():
    html = report.generate_mt_report(
        PipeGeometry(outer_diameter=0.4, wall_thickness=0.02),
        PipeMaterial(grade="synthetic", smys=400e6, smts=500e6),
        internal_pressure=2e6, external_pressure=1e6,
        code=DesignCode.API_RP_1111,
    )
    assert LABEL not in html
    assert SCREENING not in html
