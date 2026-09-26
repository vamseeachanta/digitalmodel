from io import BytesIO
import hashlib
import json
import re

import pytest
from pypdf import PdfReader

from digitalmodel.workflows.installation_full_report_pdf import render_full_pdf


def inputs():
    cases = []
    source = []
    for i in range(156):
        hs, tp = .25 * (i // 13 + 1), 4 + i % 13
        cases.append(dict(index=i, hs_m=hs, tp_s=tp, status="WITHIN_ASSUMPTIONS",
                          max_utilization=.5, governing_check="Tension", checks=[]))
        source.append(dict(index=i, hs_m=hs, tp_s=tp, status="VERIFIED",
                           settings=dict(duration_s=600), heading_degrees=180,
                           peak_tension_kN=100, minimum_signed_tension_kN=-1,
                           maximum_low_tension_duration_s=20, tension_channels=[]))
    channel = dict(id="wave", label="Simulated wave", units="m",
                   history=dict(times=[240, 360], values=[0, 1]),
                   forecast=dict(times=[361, 480], values=[1, 0]),
                   fit_status="fitted", assumed_limit=None)
    demo = dict(scenarios=[dict(case_index=97, hs_m=2, tp_s=10,
                              source_label="SIMULATED", frames=[dict(now_s=360, channels=[channel])])])
    payload = dict(title="Jumper installation analysis", created_utc="2026-09-18",
                   cases=cases, criteria=[], demo=demo, limitations=[], provenance={})
    summary = dict(cases=source, counts={"VERIFIED": 156}, envelopes=[],
                   campaign_snapshot={"master_sha256": "a" * 64},
                   engineering_acceptance="NOT EVALUATED", matrix_sha256="b" * 64)
    return summary, payload


def source_bytes(summary, payload):
    raw = json.dumps(summary).encode()
    payload["provenance"]["summary"] = dict(sha256=hashlib.sha256(raw).hexdigest())
    return raw


def test_full_report_retains_all_cases_and_embedded_monitoring():
    summary, payload = inputs()
    stream = BytesIO()
    receipt = render_full_pdf(summary, payload, stream, summary_bytes=source_bytes(summary, payload))
    reader = PdfReader(stream)
    text = "\n".join(page.extract_text() for page in reader.pages)
    assert set(re.findall(r"CASE-\d{3}", text)) == {f"CASE-{i:03d}" for i in range(156)}
    for heading in ["Introduction", "Summary and conclusions", "Design data", "Analysis methodology",
                    "Validation status", "References", "Detailed case results", "SIMULATED", "NOW"]:
        assert heading in text
    assert "Engineering acceptance: NOT EVALUATED" in " ".join(text.split())
    assert receipt["cases"] == 156 and receipt["pages"] == len(reader.pages)
    for i, page in enumerate(reader.pages, 1):
        assert f"Page {i} of {len(reader.pages)}" in page.extract_text()


@pytest.mark.parametrize("defect", ["missing", "duplicate", "coordinates"])
def test_inconsistent_source_case_mapping_fails_before_output(defect):
    summary, payload = inputs()
    if defect == "missing":
        summary["cases"].pop()
    elif defect == "duplicate":
        payload["cases"][1]["index"] = 0
    else:
        summary["cases"][0]["hs_m"] = 3
    stream = BytesIO()
    with pytest.raises(ValueError):
        render_full_pdf(summary, payload, stream, summary_bytes=source_bytes(summary, payload))
    assert stream.getvalue() == b""


def test_full_report_represents_current_configuration_without_inventing_approval():
    summary, payload = inputs()
    config = dict(revision="R8", issue_purpose="Technical review",
                  prepared_by="Automated report generation", reviewed_by="Not assigned",
                  approved_by="Not assigned", solver_version="OrcaFlex 11.6c",
                  design_source=dict(path="retained/model.yml", sha256="d" * 64),
                  references=[dict(label="Criteria evidence", path="criteria.html")],
                  revision_history=[dict(revision="R8", date="2026-09-19",
                                         description="Configuration corrected", status="Review")])
    stream = BytesIO()
    render_full_pdf(summary, payload, stream, config, summary_bytes=source_bytes(summary, payload))
    text = " ".join(" ".join(p.extract_text() for p in PdfReader(stream).pages).split())
    for expected in ["R8", "OrcaFlex 11.6c", "retained/model.yml", "d" * 64,
                     "Criteria evidence", "criteria.html", "Configuration corrected",
                     "Automated report generation", "Not assigned"]:
        assert expected in text
    assert "Engineering acceptance: NOT EVALUATED" in text


@pytest.mark.parametrize("status", ["WITHIN_ASSUMPTIONS", "EXCEEDS_ASSUMPTIONS"])
def test_unverified_source_cannot_carry_passing_classification(status):
    summary, payload = inputs()
    summary["cases"][0]["status"] = "MISSING"
    payload["cases"][0]["status"] = status
    with pytest.raises(ValueError, match="verified"):
        render_full_pdf(summary, payload, BytesIO(), summary_bytes=source_bytes(summary, payload))


def test_top_level_seed_is_retained_when_settings_omit_it():
    summary, payload = inputs()
    for case in summary["cases"]:
        case["seed"] = 987654321
    stream = BytesIO()
    render_full_pdf(summary, payload, stream, summary_bytes=source_bytes(summary, payload))
    assert "987654321" in "".join(p.extract_text() for p in PdfReader(stream).pages)


def test_all_distinct_headings_are_reported():
    summary, payload = inputs()
    summary["cases"][1]["heading_degrees"] = 165
    stream = BytesIO()
    render_full_pdf(summary, payload, stream, summary_bytes=source_bytes(summary, payload))
    text = " ".join(" ".join(p.extract_text() for p in PdfReader(stream).pages).split())
    assert "165, 180" in text


@pytest.mark.parametrize("defect", ["missing_bytes", "wrong_digest", "changed_summary"])
def test_claimed_source_digest_must_match_bytes_and_supplied_summary(defect):
    summary, payload = inputs()
    raw = json.dumps(summary).encode()
    digest = hashlib.sha256(raw).hexdigest()
    payload["provenance"]["summary"] = dict(sha256=digest)
    if defect == "missing_bytes":
        raw = None
    elif defect == "wrong_digest":
        payload["provenance"]["summary"]["sha256"] = "0" * 64
    else:
        summary["cases"][0]["peak_tension_kN"] = 999
    stream = BytesIO()
    with pytest.raises(ValueError, match="source|summary"):
        render_full_pdf(summary, payload, stream, summary_bytes=raw)
    assert stream.getvalue() == b""


def test_matching_source_bytes_allow_traceable_report():
    summary, payload = inputs()
    raw = json.dumps(summary).encode()
    payload["provenance"]["summary"] = dict(sha256=hashlib.sha256(raw).hexdigest())
    assert render_full_pdf(summary, payload, BytesIO(), summary_bytes=raw)["cases"] == 156


@pytest.mark.parametrize("digest", [None, "", "not-a-digest", "a" * 63])
def test_missing_or_malformed_source_digest_rejected(digest):
    summary, payload = inputs()
    raw = json.dumps(summary).encode()
    if digest is not None:
        payload["provenance"]["summary"] = dict(sha256=digest)
    with pytest.raises(ValueError, match="digest"):
        render_full_pdf(summary, payload, BytesIO(), summary_bytes=raw)


@pytest.mark.parametrize("case_status", ["NOT_EVALUATED", "WITHIN_ASSUMPTIONS"])
def test_not_evaluated_demand_cannot_govern_headline_or_results(case_status):
    summary, payload = inputs()
    bad = payload["cases"][0]
    bad.update(status=case_status, max_utilization=777)
    bad["checks"] = [dict(id="Missing criterion", utilization=777, status="NOT_EVALUATED")]
    stream = BytesIO()
    render_full_pdf(summary, payload, stream, summary_bytes=source_bytes(summary, payload))
    pages = PdfReader(stream).pages
    body = " ".join(" ".join(p.extract_text() for p in pages[:5]).split())
    assert "largest recorded screening utilization is 0.500" in body
    assert "Missing criterion" not in body


def test_config_disclosures_are_rendered_in_design_basis():
    summary, payload = inputs()
    config = dict(disclosures=["Crane proxy is SWL times a design factor; not response DAF."])
    stream = BytesIO()
    render_full_pdf(summary, payload, stream, config, summary_bytes=source_bytes(summary, payload))
    text = " ".join(" ".join(p.extract_text() for p in PdfReader(stream).pages).split())
    assert config["disclosures"][0] in text
