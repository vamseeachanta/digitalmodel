# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline controls for issue #3122; all observations are synthetic.

Hand calculation: three rows have magnitudes 2, 4, 8. Selecting entries a and b
flags exactly two rows and preserves 2, 4, 8. A supplied full-body candidate
changes those two magnitudes to 3 and 5; the unrelated magnitude remains 8.
These integers exercise data preservation, not a hydrodynamic prediction or a
half-to-whole scaling law. No physical coefficient or acceptance limit is used.
"""

import hashlib
import json
import subprocess
import sys

import pyarrow as pa
import pyarrow.parquet as pq
import pytest

from scripts.hf import rao_correction as rc

# Required evidence names: issue #3122 (2026), Decision items 2-3.
REQUIRED_GATES = ("buoyancy", "mesh", "solver_agreement", "claims_verifier")


def test_issue_warning_and_gates_are_not_weakened():
    assert rc.WARNING == "under correction: solved from a half-model mesh; do not use"
    assert rc.GATES == REQUIRED_GATES


@pytest.fixture
def case(tmp_path):
    source = tmp_path / "source"
    source.mkdir()
    rows = [
        {
            "vessel_id": name,
            "freq_rad_s": 1.0,
            "magnitude": value,
            "reliability": "prior_flag",
        }
        for name, value in [("a", 2.0), ("b", 4.0), ("c", 8.0)]
    ]
    table = pa.Table.from_pylist(rows).replace_schema_metadata({b"original": b"kept"})
    pq.write_table(table, source / "rao.parquet")
    pq.write_table(table, source / "hydrostatics.parquet")
    (source / "README.md").write_text("---\nlicense: mit\n---\n\nExisting card.\n")
    (source / "other.txt").write_bytes(b"unchanged\n")
    policy = {"installation": ["a"], "parametric": ["b"]}
    return source, tmp_path / "flagged", policy


def stage(case):
    source, target, policy = case
    rc.flag(source, target, policy, "2026-10-21")
    return source, target, policy


def candidate(case, tmp_path):
    source, target, policy = stage(case)
    rows = pq.read_table(source / "rao.parquet").to_pylist()[:2]
    rows[0]["magnitude"], rows[1]["magnitude"] = 3.0, 5.0
    path = tmp_path / "candidate.parquet"
    pq.write_table(pa.Table.from_pylist(rows), path)
    checks = {}
    for gate in REQUIRED_GATES:
        evidence = tmp_path / f"{gate}.txt"
        evidence.write_text("Synthetic PASS evidence for control testing only.\n")
        checks[gate] = {
            "status": "PASS",
            "file": evidence.name,
            "sha256": hashlib.sha256(evidence.read_bytes()).hexdigest(),
        }
    report = tmp_path / "gates.json"
    report.write_text(
        json.dumps({"candidate_sha256": rc.digest(path), "checks": checks})
    )
    return source, target, policy, path, report


def test_flag_preserves_values_rows_prior_flags_and_other_files(case):
    source, target, policy = stage(case)
    for name in ("rao.parquet", "hydrostatics.parquet"):
        before, after = pq.read_table(source / name), pq.read_table(target / name)
        assert after.select(before.column_names).equals(before, check_metadata=False)
        assert after.schema.metadata[b"original"] == b"kept"
        assert b"rao_correction" in after.schema.metadata
        assert after["correction_status"].to_pylist() == [
            "UNDER_CORRECTION",
            "UNDER_CORRECTION",
            None,
        ]
        assert after["correction_warning"].to_pylist() == [rc.WARNING, rc.WARNING, None]
    assert (source / "other.txt").read_bytes() == (target / "other.txt").read_bytes()
    card = (target / "README.md").read_text()
    assert card.startswith((source / "README.md").read_text())
    assert rc.WARNING in card and "Revision history" in card
    assert rc.verify(source, target)["mode"] == "flag"


@pytest.mark.parametrize(
    "policy",
    [
        {},
        {"installation": ["a"]},
        {"installation": ["a"], "parametric": ["a"]},
        {"installation": ["a"], "parametric": ["missing"]},
        {"installation": ["a", "missing"], "parametric": ["b"]},
        {"installation": [], "parametric": ["b"]},
        {"installation": [""], "parametric": ["b"]},
    ],
)
def test_ambiguous_or_incomplete_mapping_refuses_without_output(case, policy):
    source, target, _ = case
    with pytest.raises(ValueError):
        rc.flag(source, target, policy, "2026-10-21")
    assert not target.exists()


def test_destination_must_be_new_and_outside_source(case):
    source, target, policy = stage(case)
    before = (target / "rao.parquet").read_bytes()
    for output in (source, source / "nested", target):
        with pytest.raises(ValueError):
            rc.flag(source, output, policy, "2026-10-21")
    assert (target / "rao.parquet").read_bytes() == before


def test_symlinks_refuse(case, tmp_path):
    source, target, policy = case
    (source / "link").symlink_to(tmp_path / "outside")
    with pytest.raises(ValueError, match="symlink"):
        rc.flag(source, target, policy, "2026-10-21")


@pytest.mark.parametrize(
    "damage", ["card", "table", "extra", "removed", "source", "manifest"]
)
def test_verify_detects_every_artifact_change(case, damage):
    source, target, _ = stage(case)
    paths = {
        "card": target / "README.md",
        "table": target / "rao.parquet",
        "extra": target / "extra.txt",
        "source": source / "other.txt",
        "manifest": target / rc.MANIFEST,
    }
    if damage == "removed":
        (target / "other.txt").rename(target / "renamed.txt")
    else:
        paths[damage].write_text("tampered")
    with pytest.raises(ValueError):
        rc.verify(source, target)


def test_release_needs_claims_verifier_bound_to_exact_bundle(case, tmp_path):
    source, target, _ = stage(case)
    receipt = tmp_path / "claims.json"
    for document in (
        {},
        {"status": "PASS", "bundle_sha256": "stale", "evidence_ref": "review"},
    ):
        receipt.write_text(json.dumps(document))
        with pytest.raises(ValueError):
            rc.release_check(source, target, receipt)
    receipt.write_text(
        json.dumps(
            {
                "status": "PASS",
                "bundle_sha256": rc.digest(target / rc.MANIFEST),
                "evidence_ref": "external-review-reference",
            }
        )
    )
    assert (
        rc.release_check(source, target, receipt)["claims_verifier"] == "PASS_RECORDED"
    )
    (target / "other.txt").write_text("tampered")
    with pytest.raises(ValueError):
        rc.release_check(source, target, receipt)


def test_correct_only_replaces_selected_values_after_all_evidence(case, tmp_path):
    source, flagged, policy, replacement, report = candidate(case, tmp_path)
    output = tmp_path / "corrected"
    rc.correct(
        source,
        flagged,
        output,
        replacement,
        report,
        ["vessel_id", "freq_rad_s"],
        ["magnitude"],
        "2026-10-21",
    )
    rows = pq.read_table(output / "rao.parquet").to_pylist()
    assert [r["magnitude"] for r in rows] == [3.0, 5.0, 8.0]
    assert [r["correction_status"] for r in rows] == ["CORRECTED", "CORRECTED", None]
    assert [r["reliability"] for r in rows] == ["prior_flag"] * 3
    # Hydrostatics are still flagged: replacing RAOs does not qualify other tables.
    assert (
        pq.read_table(output / "hydrostatics.parquet")["correction_status"][0].as_py()
        == "UNDER_CORRECTION"
    )
    assert rc.verify(flagged, output)["mode"] == "correct"
    assert "Full-body RAO replacement" in (output / "README.md").read_text()


@pytest.mark.parametrize("gate", REQUIRED_GATES)
@pytest.mark.parametrize("defect", ["missing", "failed", "not_evaluated", "digest"])
def test_missing_failed_or_unbound_evidence_blocks_correction(
    case, tmp_path, gate, defect
):
    source, flagged, _, replacement, report = candidate(case, tmp_path)
    doc = json.loads(report.read_text())
    if defect == "missing":
        del doc["checks"][gate]
    elif defect == "digest":
        doc["checks"][gate]["sha256"] = "stale"
    else:
        doc["checks"][gate]["status"] = defect.upper()
    report.write_text(json.dumps(doc))
    output = tmp_path / "corrected"
    with pytest.raises(ValueError):
        rc.correct(
            source,
            flagged,
            output,
            replacement,
            report,
            ["vessel_id", "freq_rad_s"],
            ["magnitude"],
            "2026-10-21",
        )
    assert not output.exists()


@pytest.mark.parametrize(
    "defect", ["missing", "duplicate", "extra", "nan", "null", "key", "prior_flag"]
)
def test_candidate_must_cover_exact_keys_with_finite_values(case, tmp_path, defect):
    source, flagged, _, replacement, report = candidate(case, tmp_path)
    rows = pq.read_table(replacement).to_pylist()
    if defect == "missing":
        rows.pop()
    elif defect == "duplicate":
        rows.append(rows[0])
    elif defect == "extra":
        rows.append({**rows[0], "vessel_id": "c"})
    elif defect == "key":
        rows[0]["freq_rad_s"] = 2.0
    elif defect == "prior_flag":
        rows[0]["reliability"] = "silently_cleared"
    else:
        rows[0]["magnitude"] = float("nan") if defect == "nan" else None
    pq.write_table(pa.Table.from_pylist(rows), replacement)
    doc = json.loads(report.read_text())
    doc["candidate_sha256"] = rc.digest(replacement)
    report.write_text(json.dumps(doc))
    with pytest.raises(ValueError):
        rc.correct(
            source,
            flagged,
            tmp_path / "corrected",
            replacement,
            report,
            ["vessel_id", "freq_rad_s"],
            ["magnitude"],
            "2026-10-21",
        )


def test_cli_flag_and_verify(case, tmp_path):
    source, target, policy = case
    config = tmp_path / "affected.json"
    config.write_text(json.dumps(policy))
    for args in (
        [
            "flag",
            str(source),
            str(target),
            "--affected",
            str(config),
            "--date",
            "2026-10-21",
        ],
        ["verify", str(source), str(target)],
    ):
        result = subprocess.run(
            [sys.executable, "-m", "scripts.hf.rao_correction", *args],
            capture_output=True,
            text=True,
            check=False,
        )
        assert result.returncode == 0, result.stderr
        assert "NOT_EVALUATED" in result.stdout


def test_documented_example_preserves_values_and_refuses_missing_evidence(tmp_path):
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "examples.hydrodynamics.rao_correction_example",
            str(tmp_path / "example"),
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == [
        "magnitudes: [2.0, 4.0, 8.0]",
        "statuses: ['UNDER_CORRECTION', 'UNDER_CORRECTION', None]",
        "replacement: NOT_EVALUATED (required gate evidence absent)",
        "publication: NOT_EVALUATED (external claims-verifier required)",
    ]
    assert not (tmp_path / "example" / "corrected").exists()


@pytest.mark.parametrize(
    "defect",
    ["candidate_digest", "checks_type", "gate_type", "missing_file", "path_escape"],
)
def test_invalid_gate_evidence_refuses_before_output(case, tmp_path, defect):
    source, flagged, _, replacement, report = candidate(case, tmp_path)
    doc = json.loads(report.read_text())
    if defect == "candidate_digest":
        doc["candidate_sha256"] = "stale"
    elif defect == "checks_type":
        doc["checks"] = []
    elif defect == "gate_type":
        doc["checks"]["mesh"] = None
    else:
        doc["checks"]["mesh"]["file"] = (
            "../outside" if defect == "path_escape" else "absent.txt"
        )
    report.write_text(json.dumps(doc))
    with pytest.raises(ValueError):
        rc.correct(
            source,
            flagged,
            tmp_path / "corrected",
            replacement,
            report,
            ["vessel_id", "freq_rad_s"],
            ["magnitude"],
            "2026-10-21",
        )
    assert not (tmp_path / "corrected").exists()


@pytest.mark.parametrize(
    "keys,values",
    [
        (["vessel_id"], ["magnitude"]),
        (["vessel_id", "freq_rad_s"], ["freq_rad_s", "magnitude"]),
        (["vessel_id", "freq_rad_s"], ["phase"]),
    ],
)
def test_coordinates_cannot_be_omitted_or_replaced(case, tmp_path, keys, values):
    source, flagged, _, replacement, report = candidate(case, tmp_path)
    with pytest.raises(ValueError):
        rc.correct(
            source,
            flagged,
            tmp_path / "corrected",
            replacement,
            report,
            keys,
            values,
            "2026-10-21",
        )


def test_all_available_motion_values_must_be_replaced(case, tmp_path):
    source, target, policy = case
    table = pq.read_table(source / "rao.parquet").append_column(
        "phase", pa.array([0.0, 0.0, 0.0])
    )
    pq.write_table(table, source / "rao.parquet")
    source, flagged, _, replacement, report = candidate(case, tmp_path)
    with pytest.raises(ValueError, match="all available RAO"):
        rc.correct(
            source,
            flagged,
            tmp_path / "corrected",
            replacement,
            report,
            ["vessel_id", "freq_rad_s"],
            ["magnitude"],
            "2026-10-21",
        )


def test_cli_correction_and_final_claims_check(case, tmp_path):
    """Independent substitution answer: [2, 4, 8] becomes [3, 5, 8].

    Evidence here is synthetic and tests only the software contract. The final
    receipt binds the generated manifest, so it cannot serve a different bundle.
    """
    source, flagged, _, replacement, report = candidate(case, tmp_path)
    output = tmp_path / "corrected"
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "scripts.hf.rao_correction",
            "correct",
            str(source),
            str(flagged),
            "--output",
            str(output),
            "--candidate",
            str(replacement),
            "--gates",
            str(report),
            "--keys",
            "vessel_id",
            "freq_rad_s",
            "--values",
            "magnitude",
            "--date",
            "2026-10-21",
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    table = pq.read_table(output / "rao.parquet")
    assert table["magnitude"].to_pylist() == [3.0, 5.0, 8.0]
    assert table["correction_warning"].to_pylist() == [None, None, None]
    assert json.loads(table.schema.metadata[b"rao_correction"])["warning"] is None
    receipt = tmp_path / "final-claims.json"
    receipt.write_text(
        json.dumps(
            {
                "status": "PASS",
                "bundle_sha256": rc.digest(output / rc.MANIFEST),
                "evidence_ref": "synthetic-review-only",
            }
        )
    )
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "scripts.hf.rao_correction",
            "release-check",
            str(flagged),
            str(output),
            "--claims-report",
            str(receipt),
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert json.loads(result.stdout)["claims_verifier"] == "PASS_RECORDED"
    assert json.loads(result.stdout)["publication"] == "NOT_EVALUATED"


def test_correction_cannot_write_inside_original(case, tmp_path):
    source, flagged, _, replacement, report = candidate(case, tmp_path)
    before = rc._files(source)
    output = source / "nested" / "corrected"
    with pytest.raises(ValueError, match="outside the source"):
        rc.correct(
            source,
            flagged,
            output,
            replacement,
            report,
            ["vessel_id", "freq_rad_s"],
            ["magnitude"],
            "2026-10-21",
        )
    assert not output.parent.exists()
    assert rc._files(source) == before
    rc.verify(source, flagged)


def test_unchanged_auxiliary_nan_is_preserved(case, tmp_path):
    source, _, _ = case
    table = pq.read_table(source / "rao.parquet").append_column(
        "auxiliary", pa.array([float("nan")] * 3)
    )
    pq.write_table(table, source / "rao.parquet")
    source, flagged, _, replacement, report = candidate(case, tmp_path)
    output = tmp_path / "corrected"
    rc.correct(
        source,
        flagged,
        output,
        replacement,
        report,
        ["vessel_id", "freq_rad_s"],
        ["magnitude"],
        "2026-10-21",
    )
    result = pq.read_table(output / "rao.parquet")
    assert result["magnitude"].to_pylist() == [3.0, 5.0, 8.0]
    assert all(value != value for value in result["auxiliary"].to_pylist())


@pytest.mark.parametrize("existing_history", [False, True])
def test_correction_appends_one_revision_history(case, tmp_path, existing_history):
    source, _, _ = case
    if existing_history:
        card = source / "README.md"
        card.write_text(
            card.read_text() + "\n### Revision history\n\n"
            "| Date | Change and reason |\n|---|---|\n"
            "| 2026-10-20 | Initial observations. |\n\n## Notes\n\nRetained.\n"
        )
    source, flagged, _, replacement, report = candidate(case, tmp_path)
    output = tmp_path / "corrected"
    rc.correct(
        source,
        flagged,
        output,
        replacement,
        report,
        ["vessel_id", "freq_rad_s"],
        ["magnitude"],
        "2026-10-22",
    )
    card = (output / "README.md").read_text()
    assert card.count("### Revision history") == 1
    assert card.count("| Date | Change and reason |") == 1
    assert "| 2026-10-21 | Half-model warning; issue #3122. |" in card
    assert "| 2026-10-22 | Full-body RAO replacement; issue #3122. |" in card
    if existing_history:
        assert "| 2026-10-20 | Initial observations. |" in card
        assert "## Notes\n\nRetained." in card
