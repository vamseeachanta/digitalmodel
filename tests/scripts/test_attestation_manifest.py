# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

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
import subprocess
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import attestation_manifest as am


@pytest.fixture
def domain(tmp_path):
    root = tmp_path / "domain"
    for folder in am.SPEC_ROOTS:
        (root / folder).mkdir(parents=True, exist_ok=True)
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
    assert [c["master"] for c in manifest["cases"]] == [
        "library/model_library/b/master.yml",
        "library/templates/a/master.yml",
    ]
    for case in manifest["cases"]:
        assert case["source_sha256"] == (
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        )
        assert case["generation"]["status"] == "NOT_ATTEMPTED"
        assert case["load"]["status"] == case["statics"]["status"] == "NOT_EVALUATED"
    assert am.dumps(manifest) == am.dumps(am.build_manifest(domain, "test-version"))
    (domain / "library/templates/a/spec.yml").write_text("abcd")
    assert am.build_manifest(domain, "test-version") != manifest


@pytest.mark.parametrize("replacement", ["missing", "file"])
def test_inventory_rejects_missing_or_non_directory_root(domain, replacement):
    named_root = domain / "training"
    named_root.rename(domain / "archived_training")
    if replacement == "file":
        named_root.write_text("not a directory")
    with pytest.raises(ValueError, match="missing specification root: training"):
        am.build_manifest(domain, "test-version")


def test_inventory_records_every_named_root_count(domain):
    manifest = am.build_manifest(domain, "test-version")
    assert manifest["root_counts"] == {
        "library/model_library": 1,
        "library/templates": 1,
        "library/tier2_fast": 0,
        "installation": 0,
        "jumper": 0,
        "subsea/jumper/installation": 0,
        "pipeline/installation": 0,
        "reference": 0,
        "regional": 0,
        "mooring/reference": 0,
        "passing_ship": 0,
        "training": 0,
    }
    assert sum(manifest["root_counts"].values()) == len(manifest["cases"])


def test_explicit_inventory_counts_only_selected_paths(domain):
    manifest = am.build_manifest(
        domain, "test-version", paths=["library/templates/a/spec.yml"]
    )
    assert manifest["root_counts"]["library/templates"] == 1
    assert manifest["root_counts"]["library/model_library"] == 0
    assert sum(manifest["root_counts"].values()) == 1


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


@pytest.mark.parametrize("failure", ["collision", "runtime"])
def test_generator_exceptions_are_retained_per_case(domain, tmp_path, failure):
    from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
        CollectionCollisionError,
    )

    error = (
        CollectionCollisionError("synthetic generator failure", [])
        if failure == "collision"
        else RuntimeError("synthetic generator failure")
    )

    def generate(spec, output):
        if "model_library" in spec.parts:
            raise error
        fake_generate(spec, output)

    result = am.generate_cases(
        am.build_manifest(domain, "test-version"),
        domain,
        tmp_path / "outputs",
        generate,
    )
    first, second = result["cases"]
    assert first["generation"] == {
        "status": "FAIL",
        "error": f"{type(error).__name__}: synthetic generator failure",
    }
    assert first["files"] == {}
    assert second["generation"]["status"] == "PASS"
    assert len(second["files"]) == 2


@pytest.mark.parametrize(
    "error_type", [ImportError, ModuleNotFoundError, KeyboardInterrupt, SystemExit]
)
def test_dependency_failures_and_interrupts_stop_generation(
    domain, tmp_path, error_type
):
    attempted = []

    def generate(spec, output):
        attempted.append(spec)
        raise error_type("synthetic dependency failure or interrupt")

    with pytest.raises(error_type):
        am.generate_cases(
            am.build_manifest(domain, "test-version"),
            domain,
            tmp_path / "outputs",
            generate,
        )
    assert len(attempted) == 1


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


@pytest.mark.parametrize(
    "field,value",
    [
        ("api_version", None),
        ("api_version", 42),
        ("api_version", []),
        ("api_version", {}),
        ("load", None),
        ("load", []),
        ("load", "PASS"),
        ("load", 42),
        ("statics", None),
        ("statics", []),
        ("statics", "PASS"),
        ("statics", 42),
    ],
)
def test_malformed_record_types_are_rejected_by_cli(
    stored, domain, tmp_path, capsys, field, value
):
    manifest, record, native = stored
    record[field] = value
    with pytest.raises(ValueError):
        am.reconcile_results(manifest, [record], native)
    saved = tmp_path / "generated.json"
    saved.write_text(am.dumps(manifest))
    records = tmp_path / "results.json"
    records.write_text(json.dumps([record]))
    destination = tmp_path / "joined.json"
    with pytest.raises(SystemExit) as failure:
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
                str(destination),
            ]
        )
    assert failure.value.code == 1
    assert capsys.readouterr().err == "Attestation failed: ValueError\n"
    assert not destination.exists()
    assert json.loads(saved.read_text()) == manifest


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


def test_summary_shows_requested_version_and_counts_for_every_status(stored):
    manifest, record, native = stored
    manifest = am.reconcile_results(manifest, [record], native)
    remaining = manifest["cases"][1]
    remaining["generation"] = {"status": "FAIL", "error": "synthetic failure"}
    remaining["load"] = {"status": "NOT_ATTEMPTED"}
    summary = am.render_summary(manifest, "2026-10-20")
    assert "Requested solver version: test-version" in summary
    assert "Pinned solver version" not in summary
    assert (
        "<th>Stage</th><th>PASS</th><th>FAIL</th>"
        "<th>NOT_ATTEMPTED</th><th>NOT_EVALUATED</th>"
    ) in summary
    for stage, counts in (
        ("Generation", (1, 1, 0, 0)),
        ("Load", (1, 0, 1, 0)),
        ("Statics", (1, 0, 0, 1)),
    ):
        assert (
            f"<tr><th>{stage}</th>"
            + "".join(f"<td>{count}</td>" for count in counts)
            + "</tr>"
        ) in summary


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


def test_tampered_cyclic_bundle_is_rejected_before_yaml_parsing(
    stored, domain, tmp_path, monkeypatch
):
    manifest, _, _ = stored
    master = tmp_path / "out" / manifest["cases"][0]["master"]
    master.write_text("cycle: &cycle [*cycle]\n")

    def unexpected_parse(*args, **kwargs):
        pytest.fail("tampered YAML was parsed before its digest was checked")

    monkeypatch.setattr(am.yaml, "safe_load", unexpected_parse)
    with pytest.raises(ValueError, match="digest"):
        am.verify_generated(manifest, domain, tmp_path / "out")


def test_recursive_yaml_alias_traversal_terminates(tmp_path):
    master = tmp_path / "master.yml"
    master.write_text("cycle: &cycle [*cycle]\n")
    probe = subprocess.run(
        [
            sys.executable,
            "-c",
            "import sys; from pathlib import Path; "
            "sys.path.insert(0, sys.argv[1]); "
            "import attestation_manifest as am; "
            "assert am._generated_files(Path(sys.argv[2]), 'master.yml') "
            "== {'master.yml': sys.argv[3]}",
            str(Path(am.__file__).parent),
            str(tmp_path),
            hashlib.sha256(b"cycle: &cycle [*cycle]\n").hexdigest(),
        ],
        capture_output=True,
        text=True,
        timeout=5,
    )
    assert probe.returncode == 0, probe.stderr


@pytest.mark.parametrize("child_name", ["a", "z"])
def test_nested_cases_read_back_in_both_generation_orders(tmp_path, child_name):
    root = tmp_path / "domain"
    for folder in am.SPEC_ROOTS:
        (root / folder).mkdir(parents=True, exist_ok=True)
    names = [
        "library/templates/example/spec.yml",
        f"library/templates/example/{child_name}/spec.yml",
    ]
    for name in names:
        source = root / name
        source.parent.mkdir(parents=True, exist_ok=True)
        source.write_text("abc")

    def generate(spec, output):
        output.mkdir(parents=True, exist_ok=True)
        (output / "master.yml").write_text("General: {}\n")

    manifest = am.generate_cases(
        am.build_manifest(root, "v"), root, tmp_path / "out", generate
    )
    assert all(case["generation"]["status"] == "PASS" for case in manifest["cases"])
    am.verify_generated(manifest, root, tmp_path / "out")


def test_failed_load_requires_statics_not_attempted(stored):
    manifest, record, native = stored
    record["load"].update(
        status="FAIL", error="Synthetic error", failing_object="LineA"
    )
    record["statics"].update(status="NOT_EVALUATED", converged=None)
    with pytest.raises(ValueError, match="NOT_ATTEMPTED"):
        am.reconcile_results(manifest, [record], native)
