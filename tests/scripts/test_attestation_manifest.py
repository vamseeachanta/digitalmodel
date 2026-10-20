"""Offline attestation tests; fixtures are synthetic, never native solver evidence.

Expected counts are enumerated fixture sets. Digests use the independently
published NIST SHA-256 'One Block Message Sample', input 'abc':
https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/SHA256.pdf
Algorithm: FIPS 180-4 (2015), section 6.2; no physics
coefficients or acceptance limits are introduced.
"""

import hashlib
import importlib.abc
import json
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import attestation_manifest as am


@pytest.fixture
def domain(tmp_path):
    root = tmp_path / "domain"
    for name in ("library/templates/a", "library/model_library/b"):
        p = root / name / "spec.yml"
        p.parent.mkdir(parents=True)
        p.write_text("abc")
    return root


def test_inventory_identity_digest_and_planned_output(domain):
    manifest = am.build_manifest(domain, "test-version")
    assert [c["case_id"] for c in manifest["cases"]] == [
        "library/model_library/b/spec.yml",
        "library/templates/a/spec.yml",
    ]
    for case in manifest["cases"]:
        assert case["source_sha256"] == (
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        )
        assert case["master"] == str(Path(case["case_id"]).with_name("master.yml"))
        assert case["generation"]["status"] == "NOT_ATTEMPTED"
        assert case["load"]["status"] == case["statics"]["status"] == "NOT_EVALUATED"
    assert am.dumps(manifest) == am.dumps(am.build_manifest(domain, "test-version"))
    (domain / "library/templates/a/spec.yml").write_text("abcd")
    assert am.build_manifest(domain, "test-version") != manifest


@pytest.mark.parametrize("name", ["/absolute/spec.yml", "../spec.yml"])
def test_reject_unsafe_paths(domain, name):
    with pytest.raises(ValueError):
        am.build_manifest(domain, "test-version", paths=[name])


def test_reject_linked_input(domain, tmp_path):
    outside = tmp_path / "outside"
    outside.write_text("abc")
    link = domain / "linked/spec.yml"
    link.parent.mkdir()
    link.symlink_to(outside)
    with pytest.raises(ValueError):
        am.build_manifest(domain, "test-version", paths=["linked/spec.yml"])


def test_version_required_and_empty_inventory_rejected(tmp_path):
    with pytest.raises(ValueError):
        am.build_manifest(tmp_path, "")
    with pytest.raises(ValueError):
        am.build_manifest(tmp_path, "test-version")


def fake_generate(spec, output):
    output.mkdir(parents=True)
    (output / "includes").mkdir()
    (output / "master.yml").write_text("- includefile: includes/general.yml\n")
    (output / "includes/general.yml").write_text("General: {}\n")


def test_generation_hashes_files_and_retains_independent_failures(domain, tmp_path):
    manifest = am.build_manifest(domain, "test-version")

    def generate(spec, output):
        if "model_library" in spec.parts:
            raise ValueError("synthetic invalid section")
        fake_generate(spec, output)

    result = am.generate_cases(manifest, domain, tmp_path / "outputs", generate)
    first, second = result["cases"]
    assert first["generation"] == {
        "status": "FAIL",
        "error": "ValueError: synthetic invalid section",
    }
    assert second["generation"]["status"] == "PASS"
    assert len(second["files"]) == 2  # master + one include, by fixture construction
    assert second["load"]["status"] == "NOT_EVALUATED"
    assert manifest["cases"][0]["generation"]["status"] == "NOT_ATTEMPTED"


def test_generation_refuses_stale_output_and_input_digest_drift(domain, tmp_path):
    manifest = am.build_manifest(domain, "test-version")
    output = tmp_path / "out"
    output.mkdir()
    with pytest.raises(ValueError, match="exist"):
        am.generate_cases(manifest, domain, output, fake_generate)
    (domain / manifest["cases"][0]["case_id"]).write_text("changed")
    with pytest.raises(ValueError, match="digest"):
        am.generate_cases(manifest, domain, tmp_path / "fresh", fake_generate)
    assert not (tmp_path / "fresh").exists()


@pytest.mark.parametrize(
    "include", ["../../outside.yml", "/outside.yml", "missing.yml"]
)
def test_bad_generated_include_is_failure(domain, tmp_path, include):
    def generate(spec, output):
        output.mkdir(parents=True)
        (output / "master.yml").write_text(f"- includefile: {include}\n")

    result = am.generate_cases(
        am.build_manifest(domain, "v"), domain, tmp_path / "out", generate
    )
    assert all(c["generation"]["status"] == "FAIL" for c in result["cases"])


def test_alternate_schema_retained_without_generator(domain, tmp_path):
    (domain / "library/templates/a/spec.yml").write_text("passing_ship: {}\n")
    result = am.generate_cases(
        am.build_manifest(domain, "v"), domain, tmp_path / "out", fake_generate
    )
    assert result["cases"][1]["generation"]["status"] == "NOT_EVALUATED"
    assert "alternate" in result["cases"][1]["generation"]["error"]


@pytest.fixture
def stored(domain, tmp_path):
    manifest = am.generate_cases(
        am.build_manifest(domain, "test-version"),
        domain,
        tmp_path / "out",
        fake_generate,
    )
    native = tmp_path / "evidence"
    native.mkdir()
    (native / "run.txt").write_bytes(b"abc")
    case = manifest["cases"][0]
    record = {
        "case_id": case["case_id"],
        "source_sha256": case["source_sha256"],
        "files": case["files"],
        "solver_version": "test-version",
        "api_version": "test-api",
        "started_at": "2026-10-01T00:00:00Z",
        "finished_at": "2026-10-01T00:01:00Z",
        "load": {
            "status": "PASS",
            "error": None,
            "failing_object": None,
            "warnings": [],
        },
        "statics": {
            "status": "PASS",
            "converged": True,
            "error": None,
            "failing_object": None,
            "warnings": [],
        },
        "evidence": {"run.txt": hashlib.sha256(b"abc").hexdigest()},
    }
    return manifest, record, native


def test_stored_results_join_missing_not_evaluated_and_no_claim_promotion(stored):
    manifest, record, native = stored
    result = am.reconcile_results(manifest, [record], native)
    assert result["cases"][0]["statics"]["status"] == "PASS"
    assert result["cases"][1]["statics"]["status"] == "NOT_EVALUATED"
    assert result["claim_update"] == "NOT_EVALUATED"
    assert manifest["cases"][0]["load"]["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "field,value",
    [
        ("source_sha256", "wrong"),
        ("solver_version", "wrong"),
        ("files", {}),
        ("evidence", {}),
        ("api_version", ""),
        ("finished_at", "bad"),
        ("case_id", "unknown"),
    ],
)
def test_reject_unbound_evidence(stored, field, value):
    manifest, record, native = stored
    record[field] = value
    with pytest.raises(ValueError):
        am.reconcile_results(manifest, [record], native)


def test_duplicate_and_tampered_evidence_rejected(stored):
    manifest, record, native = stored
    with pytest.raises(ValueError):
        am.reconcile_results(manifest, [record, record], native)
    (native / "run.txt").write_text("tampered")
    with pytest.raises(ValueError, match="digest"):
        am.reconcile_results(manifest, [record], native)


def test_failure_requires_exact_error_and_object_disposition(stored):
    manifest, record, native = stored
    record["load"]["status"] = "FAIL"
    record["statics"]["status"] = "NOT_ATTEMPTED"
    with pytest.raises(ValueError):
        am.reconcile_results(manifest, [record], native)
    record["load"].update(error="Synthetic error", failing_object="LineA")
    result = am.reconcile_results(manifest, [record], native)
    assert result["cases"][0]["load"]["error"] == "Synthetic error"
    record["statics"]["status"] = "PASS"
    with pytest.raises(ValueError):
        am.reconcile_results(manifest, [record], native)


def test_summary_escapes_text_and_separates_recorded_from_claims(stored):
    manifest, record, native = stored
    record["load"]["warnings"] = ["<script>alert('x')</script>"]
    summary = am.render_summary(
        am.reconcile_results(manifest, [record], native), "2026-10-20"
    )
    assert "<script>" not in summary
    assert "&lt;script&gt;" in summary
    assert (
        summary.index("Summary")
        < summary.index("Revision history")
        < summary.index("Results")
    )
    assert "NOT_EVALUATED" in summary


def test_real_generator_never_imports_solver(tmp_path):
    """A minimal schema-only model has General and Environment, not native results."""

    class DenySolver(importlib.abc.MetaPathFinder):
        def find_spec(self, fullname, path=None, target=None):
            if fullname.lower().startswith("orcfxapi"):
                raise AssertionError("licensed solver import attempted")

    guard = DenySolver()
    sys.meta_path.insert(0, guard)
    try:
        spec = tmp_path / "spec.yml"
        # Arbitrary synthetic depth, not a design value or an expected physics result.
        spec.write_text(
            "metadata:\n  name: Synthetic\n  description: Offline test\n  structure: generic\n  operation: in-place\nenvironment:\n  water:\n    depth: 100\n  seabed:\n    stiffness: {}\ngeneric:\n  general: {}\n"
        )
        am.generate_one(spec, tmp_path / "generated")
        assert (tmp_path / "generated/master.yml").is_file()
    finally:
        sys.meta_path.remove(guard)


def test_cli_writes_readable_manifest(domain, tmp_path):
    output = tmp_path / "manifest.json"
    assert (
        am.main(
            [
                "--root",
                str(domain),
                "--solver-version",
                "test-version",
                "--manifest",
                str(output),
            ]
        )
        == 0
    )
    assert json.loads(output.read_text()) == am.build_manifest(domain, "test-version")


def test_later_cli_ingestion_uses_existing_generation_without_overwriting(
    stored, domain, tmp_path
):
    manifest, record, native = stored
    saved = tmp_path / "generated-manifest.json"
    saved.write_text(am.dumps(manifest))
    records = tmp_path / "results.json"
    records.write_text(json.dumps([record]))
    joined = tmp_path / "joined.json"
    assert (
        am.main(
            [
                "--root",
                str(domain),
                "--solver-version",
                "test-version",
                "--input-manifest",
                str(saved),
                "--generated-root",
                str(tmp_path / "out"),
                "--results",
                str(records),
                "--evidence-root",
                str(native),
                "--manifest",
                str(joined),
            ]
        )
        == 0
    )
    assert json.loads(joined.read_text())["cases"][0]["load"]["status"] == "PASS"
    assert json.loads(saved.read_text()) == manifest


def test_verify_generated_rejects_tampered_bundle(stored, domain, tmp_path):
    manifest, _, _ = stored
    master = tmp_path / "out" / manifest["cases"][0]["master"]
    master.write_text("[]\n")
    with pytest.raises(ValueError, match="digest"):
        am.verify_generated(manifest, domain, tmp_path / "out")


@pytest.mark.parametrize("path", ["../run.txt", "/run.txt", "folder\\run.txt"])
def test_native_evidence_paths_cannot_escape(stored, path):
    manifest, record, native = stored
    record["evidence"] = {path: next(iter(record["evidence"].values()))}
    with pytest.raises(ValueError):
        am.reconcile_results(manifest, [record], native)


def test_cli_never_overwrites_existing_evidence(domain, tmp_path):
    output = domain / "library/templates/a/spec.yml"
    before = output.read_bytes()
    with pytest.raises(SystemExit):
        am.main(
            [
                "--root",
                str(domain),
                "--solver-version",
                "v",
                "--manifest",
                str(output),
                "--generate",
                str(tmp_path / "fresh"),
            ]
        )
    assert output.read_bytes() == before
    assert not (tmp_path / "fresh").exists()


def test_report_destinations_must_differ(domain, tmp_path):
    output = tmp_path / "report"
    with pytest.raises(SystemExit):
        am.main(
            [
                "--root",
                str(domain),
                "--solver-version",
                "v",
                "--manifest",
                str(output),
                "--summary",
                str(output),
                "--report-date",
                "2026-10-20",
            ]
        )
    assert not output.exists()


@pytest.mark.parametrize("converged", [False, None, "true", 1])
def test_statics_pass_requires_explicit_convergence(stored, converged):
    manifest, record, native = stored
    record["statics"]["converged"] = converged
    with pytest.raises(ValueError, match="convergence"):
        am.reconcile_results(manifest, [record], native)


def test_cli_does_not_reissue_unverified_native_outcomes(stored, domain, tmp_path):
    manifest, record, native = stored
    prior = am.reconcile_results(manifest, [record], native)
    saved = tmp_path / "prior.json"
    saved.write_text(am.dumps(prior))
    destination = tmp_path / "fresh.json"
    am.main(
        [
            "--root",
            str(domain),
            "--solver-version",
            "test-version",
            "--input-manifest",
            str(saved),
            "--generated-root",
            str(tmp_path / "out"),
            "--manifest",
            str(destination),
        ]
    )
    assert (
        json.loads(destination.read_text())["cases"][0]["load"]["status"]
        == "NOT_EVALUATED"
    )


@pytest.mark.parametrize("object_name,reason", [(42, None), (None, 42), (" ", None)])
def test_failure_object_disposition_requires_text(stored, object_name, reason):
    manifest, record, native = stored
    record["load"].update(
        status="FAIL",
        error="Synthetic error",
        failing_object=object_name,
        object_unavailable_reason=reason,
    )
    record["statics"].update(status="NOT_ATTEMPTED", converged=None)
    with pytest.raises(ValueError, match="object"):
        am.reconcile_results(manifest, [record], native)
