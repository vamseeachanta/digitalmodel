# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Acceptance records require current evidence, criteria and bounded scope."""

import copy
import hashlib
import importlib.util
import json
from pathlib import Path

import pytest

UTILITY = (
    Path(__file__).resolve().parents[2]
    / "scripts/reporting/knowledge_pack_acceptance.py"
)
SPEC = importlib.util.spec_from_file_location("knowledge_pack_acceptance", UTILITY)
audit = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(audit)


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


@pytest.fixture
def evidence(tmp_path):
    pack = "src/digitalmodel/asset_integrity/docs/agent-pack.md"
    implementation = "src/digitalmodel/asset_integrity/screen.py"
    test = "tests/asset_integrity/test_screen.py"
    sources = {
        pack: f"Bounded source claim. [{implementation}:L1-L2]\n",
        implementation: "def screen(value):\n    return value\n",
        test: "# 1 + 1 = 2\nassert 2 == 2\n# 2 + 1 = 3\nassert 3 == 3\n# 3 + 1 = 4\nassert 4 == 4\n",
    }
    for name, content in sources.items():
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
    record = {
        "id": "source",
        "family": "asset_integrity",
        "pack_path": pack,
        "claim_kind": "source_presence",
        "claim": "The named source and test exist within the inspected scope.",
        "claim_location": f"{pack}:L1-L1",
        "implementation_refs": [f"{implementation}:L1-L2"],
        "test_refs": [f"{test}:L1-L6"],
        "source_digests": {p: digest(tmp_path / p) for p in sources},
        "criterion": "The cited source and test must exist at the recorded digests.",
        "probe_command": None,
        "probe_result": {
            "status": "NOT_RUN",
            "reason": "Static source inspection only.",
        },
        "disposition": "SUPPORTED",
        "missing_evidence": [],
        "observation_time": "2026-10-24T00:00:00Z",
        "observation_basis": "Manual inspection of exactly the cited lines.",
    }
    document = {
        "schema_version": 1,
        "observations": [{"family": "asset_integrity", "findings": [record]}],
        "revision_history": [
            {
                "revision": "0",
                "date": "2026-10-24",
                "change": "Initial synthetic audit.",
            }
        ],
    }
    return tmp_path, document, record


def finding(report):
    return next(row for row in report["findings"] if row["family"] == "asset_integrity")


def run(evidence):
    root, document, _ = evidence
    return audit.build_audit(root, document)


def test_positive_source_presence_is_bounded_and_not_numerical(evidence):
    report = run(evidence)
    assert finding(report)["disposition"] == "SUPPORTED"
    assert report["summary"]["qualified_engineering_results"] == 0
    assert report["scope"]["families"] == list(audit.FAMILIES)
    assert len(report["inventory"]) == 7
    assert report["summary"]["UNESTABLISHED"] == 6


@pytest.mark.parametrize(
    "field",
    [
        "criterion",
        "claim",
        "claim_location",
        "observation_time",
        "observation_basis",
        "implementation_refs",
        "test_refs",
    ],
)
def test_missing_acceptance_fields_refuse_support(evidence, field):
    evidence[2].pop(field)
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_absent_pack_refuses_support(evidence):
    root, _, record = evidence
    (root / record["pack_path"]).unlink()
    with pytest.raises(ValueError, match="stale|missing"):
        run(evidence)


def test_initial_inventory_names_absent_packs(evidence):
    report = audit.build_audit(evidence[0])
    assert all(row["disposition"] == "UNESTABLISHED" for row in report["findings"])
    absent = next(
        row for row in report["inventory"] if row["family"] == "mooring_fatigue"
    )
    assert absent["pack_path"] is None
    assert (
        "pack missing" in " ".join(report["findings"][-1]["missing_evidence"])
        or report["summary"]["SUPPORTED"] == 0
    )


@pytest.mark.parametrize(
    "reference",
    [
        "src/missing.py:L1-L1",
        "src/digitalmodel/asset_integrity/screen.py:L1-L200",
        "src/digitalmodel/asset_integrity/screen.py:L2-L1",
    ],
)
def test_dangling_or_invalid_citations_refuse_support(evidence, reference):
    evidence[2]["implementation_refs"] = [reference]
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_duplicate_family_records_are_not_silently_overwritten(evidence):
    evidence[1]["observations"].append(copy.deepcopy(evidence[1]["observations"][0]))
    with pytest.raises(ValueError, match="duplicate family"):
        run(evidence)


def test_duplicate_claim_ids_are_rejected(evidence):
    evidence[1]["observations"][0]["findings"].append(copy.deepcopy(evidence[2]))
    with pytest.raises(ValueError, match="duplicate claim"):
        run(evidence)


def test_stale_digest_is_fatal(evidence):
    root, _, _ = evidence
    (root / "src/digitalmodel/asset_integrity/screen.py").write_text("changed\n")
    with pytest.raises(ValueError, match="stale"):
        run(evidence)


def test_unhashed_evidence_refuses_support(evidence):
    del evidence[2]["source_digests"]["tests/asset_integrity/test_screen.py"]
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_test_and_implementation_sources_are_distinct_roles(evidence):
    evidence[2]["test_refs"] = evidence[2]["implementation_refs"][:]
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_declared_missing_evidence_prevents_support(evidence):
    evidence[2]["missing_evidence"] = ["Independent evidence absent."]
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_pack_cannot_serve_as_its_own_implementation_evidence(evidence):
    evidence[2]["implementation_refs"] = [evidence[2]["claim_location"]]
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_dangling_pack_citation_is_contradicted(evidence):
    root, _, record = evidence
    pack = root / record["pack_path"]
    pack.write_text(pack.read_text() + "[src/missing.py:L1-L1]\n")
    record["source_digests"][record["pack_path"]] = digest(pack)
    record["claim_kind"] = "citation_integrity"
    assert finding(run(evidence))["disposition"] == "CONTRADICTED"


@pytest.mark.parametrize(
    "reference", ["missing.md", "../outside.py:L1-L1", "src/missing.py:Lbad"]
)
def test_unresolvable_local_citation_formats_cannot_be_silently_ignored(
    evidence, reference
):
    root, _, record = evidence
    pack = root / record["pack_path"]
    pack.write_text(pack.read_text() + f"[{reference}]\n")
    record["source_digests"][record["pack_path"]] = digest(pack)
    record["claim_kind"] = "citation_integrity"
    assert finding(run(evidence))["disposition"] == "CONTRADICTED"


def test_generated_inventory_retains_every_citation_without_duplicate_fields(evidence):
    report = run(evidence)
    item = next(
        row for row in report["inventory"] if row["family"] == "asset_integrity"
    )
    assert item["citations"] == ["src/digitalmodel/asset_integrity/screen.py:L1-L2"]
    assert item["unresolved_citations"] == []


def test_pack_to_pack_citation_does_not_establish_independent_support(evidence):
    root, _, record = evidence
    pack = root / record["pack_path"]
    pack.write_text(f"[{record['pack_path']}:L1-L1]\n")
    record["source_digests"][record["pack_path"]] = digest(pack)
    record["claim_kind"] = "citation_integrity"
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


@pytest.mark.parametrize("kind", ["behavioral", "numerical_report", "gate_coverage"])
def test_test_count_or_pass_word_cannot_support_a_claim(evidence, kind):
    evidence[2].update(
        claim_kind=kind, probe_result={"status": "PASS", "test_count": 100}
    )
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def behavioral(record):
    cases = [
        {
            "arguments": {"value": "NaN"},
            "expected": "reject_with_reason",
            "observed": "reject_with_reason",
            "detail": "ValueError: value must be finite",
        }
    ]
    output = json.dumps(cases, sort_keys=True)
    record.update(
        claim_kind="behavioral",
        probe_command="uv run --no-project python -c '<retained program>'",
        probe_result={
            "status": "RUN",
            "exit_status": 0,
            "interface": "screen(value)",
            "synthetic": True,
            "program": "screen(float('nan'))",
            "cases": cases,
            "output": output,
            "output_sha256": hashlib.sha256(output.encode()).hexdigest(),
        },
    )
    record["probe_result"].update(
        completed=True,
        command=record["probe_command"],
        source_digests=copy.deepcopy(record["source_digests"]),
        program_sha256=hashlib.sha256(
            record["probe_result"]["program"].encode()
        ).hexdigest(),
    )


def test_explicit_probe_can_support_only_its_bounded_claim(evidence):
    behavioral(evidence[2])
    assert finding(run(evidence))["disposition"] == "SUPPORTED"


def test_generated_summary_points_to_unduplicated_observation_evidence(evidence):
    behavioral(evidence[2])
    report = run(evidence)
    derived = finding(report)
    assert "program" not in derived["probe_result"]
    assert derived["evidence_input"] == {
        "family": "asset_integrity",
        "claim_id": "source",
    }
    assert report["observations"][0]["findings"][0]["probe_result"]["program"]


@pytest.mark.parametrize(
    "field",
    [
        "output",
        "output_sha256",
        "cases",
        "program",
        "interface",
        "exit_status",
        "completed",
        "source_digests",
        "program_sha256",
        "command",
    ],
)
def test_absent_probe_evidence_refuses_support(evidence, field):
    behavioral(evidence[2])
    evidence[2]["probe_result"].pop(field)
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_failed_or_mismatching_probe_is_contradicted(evidence):
    behavioral(evidence[2])
    probe = evidence[2]["probe_result"]
    probe["cases"][0]["observed"] = "accepted_nonfinite"
    probe["output"] = json.dumps(probe["cases"], sort_keys=True)
    probe["output_sha256"] = hashlib.sha256(probe["output"].encode()).hexdigest()
    assert finding(run(evidence))["disposition"] == "CONTRADICTED"


def test_probe_output_must_match_structured_cases(evidence):
    behavioral(evidence[2])
    evidence[2]["probe_result"]["cases"][0]["observed"] = "accepted_nonfinite"
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


@pytest.mark.parametrize(
    "field,value",
    [
        ("completed", False),
        ("command", "different command"),
        ("source_digests", {}),
        ("program_sha256", "incorrect"),
    ],
)
def test_mismatched_execution_receipt_refuses_support(evidence, field, value):
    behavioral(evidence[2])
    evidence[2]["probe_result"][field] = value
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_not_run_is_a_named_gap(evidence):
    evidence[2]["claim_kind"] = "behavioral"
    row = finding(run(evidence))
    assert row["disposition"] == "UNESTABLISHED"
    assert any("NOT_RUN" in message for message in row["missing_evidence"])


def test_human_unestablished_judgment_cannot_be_promoted(evidence):
    evidence[2]["disposition"] = "UNESTABLISHED"
    evidence[2]["missing_evidence"] = ["Applicability remains unestablished."]
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_numerical_claim_requires_three_literal_independent_anchors(evidence):
    behavioral(evidence[2])
    record = evidence[2]
    record["claim_kind"] = "numerical_report"
    record["reference_basis"] = "Synthetic arithmetic only; no physical qualification."
    record["anchors"] = [
        {
            "quantity": f"synthetic_{n}",
            "expected": n,
            "actual": n,
            "working": f"{n - 1} + 1 = {n}",
            "tolerance": 0,
            "assertion": f"assert {n} == {n}",
            "independence": "hand-worked",
            "test_ref": f"tests/asset_integrity/test_screen.py:L{2 * n - 3}-L{2 * n - 2}",
        }
        for n in (2, 3, 4)
    ]
    result = record["probe_result"]
    result["cases"] = [
        {
            "quantity": f"synthetic_{n}",
            "arguments": {"value": n},
            "expected": n,
            "observed": n,
            "detail": "Synthetic identity result.",
        }
        for n in (2, 3, 4)
    ]
    result["output"] = json.dumps(result["cases"], sort_keys=True)
    result["output_sha256"] = hashlib.sha256(result["output"].encode()).hexdigest()
    assert finding(run(evidence))["disposition"] == "SUPPORTED"
    record["anchors"].pop()
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_repeated_anchors_do_not_count_as_three_numbers(evidence):
    behavioral(evidence[2])
    record = evidence[2]
    record["claim_kind"] = "numerical_report"
    record["reference_basis"] = "Synthetic arithmetic."
    anchor = {
        "quantity": "only_one",
        "expected": 2,
        "actual": 2,
        "working": "1 + 1 = 2",
        "tolerance": 0,
        "independence": "hand-worked",
        "test_ref": "tests/asset_integrity/test_screen.py:L1-L1",
    }
    record["anchors"] = [anchor] * 3
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


@pytest.mark.parametrize("replacement", [float("nan"), float("inf"), True, "screen(2)"])
def test_nonfinite_or_computed_reference_is_not_a_literal(evidence, replacement):
    behavioral(evidence[2])
    evidence[2]["claim_kind"] = "numerical_report"
    evidence[2]["anchors"] = [{"expected": replacement}] * 3
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_roundtrip_preserves_observations_and_rejects_changed_sources(
    evidence, tmp_path
):
    root, document, record = evidence
    record["disposition"] = "UNESTABLISHED"
    output = tmp_path / "generated"
    audit.generate(root, output, document)
    saved = json.loads((output / audit.JSON_NAME).read_text())
    assert saved["observations"] == document["observations"]
    rebuilt = tmp_path / "rebuilt"
    audit.generate(root, rebuilt, saved)
    assert (output / audit.JSON_NAME).read_bytes() == (
        rebuilt / audit.JSON_NAME
    ).read_bytes()
    assert (output / audit.HTML_NAME).read_bytes() == (
        rebuilt / audit.HTML_NAME
    ).read_bytes()
    (root / "tests/asset_integrity/test_screen.py").write_text("changed\n")
    with pytest.raises(ValueError, match="stale"):
        audit.generate(root, output, saved)
    assert (output / audit.JSON_NAME).read_text() == json.dumps(
        saved, indent=2, sort_keys=True
    ) + "\n"


@pytest.mark.parametrize(
    "reference", ["../outside.py:L1-L1", "C:\\profile\\outside.py:L1-L1"]
)
def test_untrusted_evidence_paths_cannot_escape_root(evidence, reference):
    evidence[2]["implementation_refs"] = [reference]
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_external_symlink_is_not_an_evidence_source(evidence, tmp_path):
    root, _, record = evidence
    outside = tmp_path.parent / (tmp_path.name + "-outside.py")
    outside.write_text("outside\n")
    link = root / "linked.py"
    link.symlink_to(outside)
    record["implementation_refs"] = ["linked.py:L1-L1"]
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_circular_output_citations_do_not_establish_acceptance(evidence):
    root, _, record = evidence
    target = root / "docs/audits" / audit.HTML_NAME
    target.parent.mkdir(parents=True)
    target.write_text("self citation\n")
    record["implementation_refs"] = [f"docs/audits/{audit.HTML_NAME}:L1-L1"]
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_output_cannot_overwrite_sources(evidence):
    root, document, _ = evidence
    before = (root / "src/digitalmodel/asset_integrity/screen.py").read_bytes()
    with pytest.raises(ValueError, match="output"):
        audit.generate(root, root / "src/digitalmodel/asset_integrity", document)
    assert (root / "src/digitalmodel/asset_integrity/screen.py").read_bytes() == before


def test_second_staged_write_failure_preserves_both_outputs(evidence, monkeypatch):
    root, document, _ = evidence
    output = root / "generated"
    audit.generate(root, output, document)
    before = {p.name: p.read_bytes() for p in output.iterdir()}
    original = Path.write_text

    def fail_html(path, *args, **kwargs):
        if path.name.endswith(".html"):
            raise OSError("simulated second output failure")
        return original(path, *args, **kwargs)

    monkeypatch.setattr(Path, "write_text", fail_html)
    with pytest.raises(OSError, match="simulated"):
        audit.generate(root, output, document)
    assert {p.name: p.read_bytes() for p in output.iterdir() if p.is_file()} == before


def test_symlinked_output_is_refused(evidence):
    root, document, _ = evidence
    link = root / "alias"
    link.symlink_to(root / "src", target_is_directory=True)
    with pytest.raises(ValueError, match="output"):
        audit.generate(root, link / "generated", document)


def test_html_escapes_observations_and_has_required_sections(evidence):
    evidence[2]["claim"] = "<script>alert('example')</script>"
    page = audit.render_html(run(evidence))
    assert "<script>" not in page
    assert "&lt;script&gt;" in page
    for title in (
        "Summary",
        "Revision history",
        "Criteria",
        "Findings",
        "Unresolved engineering basis",
    ):
        assert title in page


def test_unknown_schema_family_and_disposition_are_rejected(evidence):
    root, document, record = evidence
    document["schema_version"] = 999
    with pytest.raises(ValueError, match="schema"):
        audit.build_audit(root, document)
    document["schema_version"] = 1
    record["disposition"] = "PASS"
    with pytest.raises(ValueError, match="disposition"):
        audit.build_audit(root, document)
    record["disposition"] = "SUPPORTED"
    document["observations"][0]["family"] = "unknown"
    with pytest.raises(ValueError, match="family"):
        audit.build_audit(root, document)


@pytest.mark.parametrize("value", [None, "wrong", 1, {}])
def test_malformed_family_bundle_is_refused(evidence, value):
    evidence[1]["observations"] = [value]
    with pytest.raises(ValueError, match="family|bundle"):
        run(evidence)


@pytest.mark.parametrize("value", [None, "wrong", 1, []])
def test_malformed_probe_is_unestablished(evidence, value):
    evidence[2].update(claim_kind="behavioral", probe_result=value)
    assert finding(run(evidence))["disposition"] == "UNESTABLISHED"


def test_cli_reads_observations_before_replacing_same_named_output(evidence):
    root, document, _ = evidence
    output = root / "generated"
    audit.generate(root, output, document)
    path = output / audit.JSON_NAME
    before = path.read_bytes()
    assert (
        audit.main(
            ["--root", str(root), "--observations", str(path), "--out-dir", str(output)]
        )
        == 0
    )
    assert path.read_bytes() == before
