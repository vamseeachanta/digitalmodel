# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic supplied-result views reject incomplete or conflicting evidence."""
import copy
from hashlib import sha256

import pytest

from digitalmodel.reporting import Provenance, ProvenanceError
from digitalmodel.riser_reporting import canonical_digest, render_fatigue_view
from digitalmodel.riser_reporting import fatigue


def bind(results, inputs, options):
    metadata = {key: options[key] for key in
                ("method", "references", "assumptions", "workflow_version")}
    provenance = Provenance(code_edition="synthetic-v1", validated_path="screening")
    for kind, identifier, payload in (("spec", "input-record", inputs),
                                     ("workflow_results", "result-record", results),
                                     ("metadata", "method-record", metadata)):
        provenance.add(kind, identifier, digest=canonical_digest(payload))
    options["provenance"] = provenance


@pytest.fixture
def case():
    segment = dict(segment_id="segment-a", wave_damage=0.15, viv_damage=0.05,
                   total_damage=0.2, fatigue_life_years=100.0,
                   required_life_years=40.0, dff_margin=2.5, passes_dff=True)
    results = dict(governing_segment="segment-a", governing_dff_margin=2.5,
                   screening_status="pass", segments=[segment])
    inputs = {"design_life_years": 20.0, "dff": 2.0}
    options = dict(method="Synthetic supplied-result method", assumptions=None,
                   references=[dict(identifier="Synthetic reference", edition="test",
                                    locator="test clause", qualification="unverified")],
                   workflow_version="synthetic-v1")
    bind(results, inputs, options)
    return results, inputs, options


@pytest.mark.parametrize("passed", [True, False])
def test_order_values_units_and_readback(case, tmp_path, passed):
    results, inputs, options = case
    if not passed:
        results.update(screening_status="fail", governing_dff_margin=0.5)
        results["segments"][0].update(fatigue_life_years=20.0, dff_margin=0.5, passes_dff=False)
        bind(results, inputs, options)
    before = copy.deepcopy(case)
    html = render_fatigue_view(results, inputs, **options)
    headings = ("Executive summary", "Inputs and assumptions", "Method and standards",
                "Results", "Critical-location findings", "Conclusions", "Data provenance")
    positions = [html.index(f"<h2>{heading}</h2>") for heading in headings]
    assert positions == sorted(positions)
    for text in ("segment-a", "PASS" if passed else "FAIL", "SCREENING",
                 "100.0 years &gt;= 40.0 years" if passed else "20.0 years &lt; 40.0 years",
                 "0.15", "0.05", "0.2", "2.5" if passed else "0.5", "Fatigue life (years)",
                 "Total damage (1)", "unverified", "Assumptions record not supplied",
                 "input-record", "result-record", "method-record"):
        assert text in html
    assert html == render_fatigue_view(results, inputs, **options)
    assert case == before
    assert not any(text in html for text in ("<script", "cdn.", 'src=', 'href="http'))
    output = tmp_path / "report.html"
    assert render_fatigue_view(results, inputs, output_path=output, **options) == output
    assert output.read_text(encoding="utf-8") == html
    assert "@media print" in html and "overflow-wrap" in html


def test_exact_section_reads(case, monkeypatch):
    real = fatigue.assemble_report
    def inspect(*args, **kwargs):
        for section in kwargs["sections"]:
            expected = {"result-record", "method-record"}
            if section.key == "inputs":
                expected = {"input-record", "method-record"}
            if section.key == "method":
                expected = {"method-record"}
            assert set(section.reads) == expected
        return real(*args, **kwargs)
    monkeypatch.setattr(fatigue, "assemble_report", inspect)
    render_fatigue_view(case[0], case[1], **case[2])


def test_all_text_is_escaped_and_assumptions_are_explicit(case):
    results, inputs, options = case
    payload = '<script>alert("x")</script>'
    inputs["markup"] = {"nested": payload}
    options.update(method=payload, assumptions=[payload], workflow_version=payload)
    options["references"][0].update(identifier=payload, locator='javascript:alert("x")')
    results["governing_segment"] = results["segments"][0]["segment_id"] = payload
    bind(results, inputs, options)
    options["provenance"].code_edition = payload
    html = render_fatigue_view(results, inputs, title=payload, **options)
    assert payload not in html and "&lt;script&gt;" in html
    assert 'href="javascript:' not in html and 'id="references"' in html


@pytest.mark.parametrize("kind", ["spec", "workflow_results", "metadata"])
@pytest.mark.parametrize("defect", ["missing", "digest", "duplicate", "changed"])
def test_evidence_fails_before_write(case, tmp_path, kind, defect):
    results, inputs, options = case
    source = next(s for s in options["provenance"].sources if s.kind == kind)
    if defect == "missing":
        options["provenance"].sources.remove(source)
    elif defect == "digest":
        source.digest = "sha256:" + "0" * 64
    elif defect == "duplicate":
        options["provenance"].sources.append(source.model_copy())
    else:
        {"spec": inputs, "workflow_results": results, "metadata": options}[kind]["new"] = 1
        if kind == "metadata":
            options.pop("new")
            options["method"] += " changed"
    output = tmp_path / "absent.html"
    with pytest.raises((ValueError, ProvenanceError)):
        render_fatigue_view(results, inputs, output_path=output, **options)
    assert not output.exists()


@pytest.mark.parametrize("field,value", [
    ("segments", []), ("governing_segment", "absent"),
    ("governing_dff_margin", 7.0), ("screening_status", "fail"),
    ("segments", [{}]), ("governing_dff_margin", None),
    ("screening_status", "unknown"), ("segments", None),
])
def test_invalid_results(case, field, value):
    results, inputs, options = case
    results[field] = value
    bind(results, inputs, options)
    with pytest.raises(ValueError):
        render_fatigue_view(results, inputs, **options)


@pytest.mark.parametrize("field", ["wave_damage", "viv_damage", "total_damage",
                                  "fatigue_life_years", "required_life_years", "dff_margin"])
@pytest.mark.parametrize("value", [None, True, "1,000", -1.0, float("nan"), float("inf")])
def test_bad_segment_numbers(case, tmp_path, field, value):
    results, inputs, options = case
    results["segments"][0][field] = value
    with pytest.raises((ValueError, ProvenanceError)):
        render_fatigue_view(results, inputs, output_path=tmp_path / "bad.html", **options)
    assert not (tmp_path / "bad.html").exists()


@pytest.mark.parametrize("field,value", [("method", " "), ("references", []),
    ("references", [{}]), ("workflow_version", ""), ("assumptions", "unknown")])
def test_invalid_metadata(case, field, value):
    results, inputs, options = case
    options[field] = value
    bind(results, inputs, options)
    with pytest.raises(ValueError):
        render_fatigue_view(results, inputs, **options)


@pytest.mark.parametrize("status", ["validated", None, "screening", "unvalidated"])
def test_provenance_status(case, status):
    results, inputs, options = case
    options["provenance"].validated_path = status
    if status in ("screening", "unvalidated"):
        assert status.upper() in render_fatigue_view(results, inputs, **options)
    else:
        with pytest.raises(ProvenanceError):
            render_fatigue_view(results, inputs, **options)


def test_canonical_digest_contract():
    expected = "sha256:" + sha256('{"a":"é","b":1}'.encode("utf-8")).hexdigest()
    assert canonical_digest({"b": 1, "a": "é"}) == expected
    with pytest.raises(ValueError):
        canonical_digest({"a": float("nan")})
