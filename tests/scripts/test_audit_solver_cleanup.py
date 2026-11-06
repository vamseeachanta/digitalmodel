"""Offline audit cases; expected states follow the issue #2095 evidence rules.

These are logical examples, not solver calculations. Matching source bytes can
establish only freshness of a source review. Changing/removing those bytes must
invalidate that review; neither outcome establishes observed process absence.
"""

import copy
import hashlib
import json
from pathlib import Path
import runpy

import pytest


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/audit_solver_cleanup.py"


@pytest.fixture
def auditor():
    return runpy.run_path(str(SCRIPT))


@pytest.fixture
def sample(tmp_path):
    root = tmp_path / "repo"
    root.mkdir()
    # This would fail if the auditor imported or executed inspected source.
    source = b"raise RuntimeError('inspected source must never execute')\n"
    (root / "wrapper.py").write_bytes(source)
    inventory = {
        "scope": "Synthetic wrapper only; no runtime evidence.",
        "entries": [{
            "id": "synthetic-wrapper",
            "classification": "ROOT_ONLY",
            "sources": [{
                "path": "wrapper.py",
                "sha256": hashlib.sha256(source).hexdigest(),
            }],
            "observations": {
                "termination_requested": "Termination may be requested.",
                "root_exited": "Only root wait is inspected.",
                "whole_owned_job_drained": "No whole-job observation.",
                "cleanup_unknown": "No durable interlock established.",
            },
            "follow_up": "Review the caller before any runtime change.",
        }],
    }
    return root, inventory


def test_matching_source_never_establishes_cleanup_or_reuse(auditor, sample):
    """Hand derivation: source identity is true; all runtime facts are absent."""
    root, inventory = sample
    report = auditor["audit"](root, inventory)
    assert report["source_review_current"] is True
    assert report["solver_reuse"] == "NOT_EVALUATED"
    row, = report["entries"]
    assert row["source_status"] == "SOURCE_MATCH"
    assert row["classification"] == "ROOT_ONLY"
    assert row["runtime_evidence"] == {
        "termination_requested": "NOT_EVALUATED",
        "root_exited": "NOT_EVALUATED",
        "whole_owned_job_drained": "NOT_EVALUATED",
        "cleanup_unknown": "NOT_EVALUATED",
    }


@pytest.mark.parametrize("fault, expected", [
    ("changed", "SOURCE_CHANGED"),
    ("missing", "SOURCE_MISSING"),
    ("directory", "SOURCE_NOT_REGULAR"),
    ("symlink", "SOURCE_SYMLINK_REFUSED"),
])
def test_source_fault_invalidates_review(auditor, sample, tmp_path, fault, expected):
    """Independent oracle: changed, absent or redirected bytes are unreviewed."""
    root, inventory = sample
    path = root / "wrapper.py"
    if fault == "changed":
        path.write_bytes(b"changed\n")
    else:
        path.rename(root / "retained.py")
        if fault == "directory":
            path.mkdir()
        elif fault == "symlink":
            path.symlink_to(root / "retained.py")
    report = auditor["audit"](root, inventory)
    row, = report["entries"]
    assert report["source_review_current"] is False
    assert row["source_status"] == "REVIEW_REQUIRED"
    assert row["sources"][0]["status"] == expected
    assert row["classification"] == "NOT_EVALUATED"
    assert report["solver_reuse"] == "NOT_EVALUATED"


def test_parent_symlink_refused_even_when_bytes_match(auditor, sample):
    root, inventory = sample
    (root / "redirect").symlink_to(root, target_is_directory=True)
    inventory["entries"][0]["sources"][0]["path"] = "redirect/wrapper.py"
    report = auditor["audit"](root, inventory)
    assert report["entries"][0]["sources"][0]["status"] == "SOURCE_SYMLINK_REFUSED"


def test_one_missing_dependency_invalidates_combined_review(auditor, sample):
    root, inventory = sample
    dependency = copy.deepcopy(inventory["entries"][0]["sources"][0])
    dependency["path"] = "caller.py"
    inventory["entries"][0]["sources"].append(dependency)
    report = auditor["audit"](root, inventory)
    assert report["entries"][0]["source_status"] == "REVIEW_REQUIRED"
    assert report["source_review_current"] is False


def test_read_failure_is_unknown_not_observed_absence(auditor, sample, monkeypatch):
    root, inventory = sample

    def denied(path):
        raise PermissionError("synthetic read refusal")

    monkeypatch.setattr(Path, "read_bytes", denied)
    report = auditor["audit"](root, inventory)
    row, = report["entries"]
    assert row["sources"][0]["status"] == "SOURCE_UNREADABLE"
    assert row["observations"] is None
    assert row["runtime_evidence"]["whole_owned_job_drained"] == "NOT_EVALUATED"
    assert report["source_review_current"] is False


@pytest.mark.parametrize("path", [
    "../outside.py", "/absolute.py", "a/../wrapper.py", "./wrapper.py",
    "a//wrapper.py", "a\\wrapper.py", "C:/wrapper.py", "wrapper.py/", "",
])
def test_non_repository_paths_rejected_before_read(auditor, sample, path):
    root, inventory = sample
    inventory["entries"][0]["sources"][0]["path"] = path
    with pytest.raises(ValueError):
        auditor["audit"](root, inventory)


@pytest.mark.parametrize("fault", [
    "empty_entries", "empty_sources", "duplicate_entry", "duplicate_source",
    "bad_digest", "missing_observation", "unknown_classification", "extra_field",
])
def test_incomplete_or_ambiguous_inventory_rejected(auditor, sample, fault):
    root, inventory = sample
    entry = inventory["entries"][0]
    if fault == "empty_entries":
        inventory["entries"] = []
    elif fault == "empty_sources":
        entry["sources"] = []
    elif fault == "duplicate_entry":
        inventory["entries"].append(copy.deepcopy(entry))
    elif fault == "duplicate_source":
        entry["sources"].append(copy.deepcopy(entry["sources"][0]))
    elif fault == "bad_digest":
        entry["sources"][0]["sha256"] = "unknown"
    elif fault == "missing_observation":
        del entry["observations"]["whole_owned_job_drained"]
    elif fault == "unknown_classification":
        entry["classification"] = "CLEANUP_CONFIRMED"
    elif fault == "extra_field":
        entry["solver_reuse"] = True
    with pytest.raises(ValueError):
        auditor["audit"](root, inventory)


def test_duplicate_json_keys_rejected(auditor, tmp_path):
    path = tmp_path / "inventory.json"
    path.write_text('{"scope":"first","scope":"second","entries":[]}', encoding="utf-8")
    with pytest.raises(ValueError):
        auditor["load_inventory"](path)


@pytest.mark.parametrize("changed, expected_code", [(False, 0), (True, 1)])
def test_cli_exit_reports_source_freshness_only(auditor, sample, tmp_path, capsys,
                                              changed, expected_code):
    root, inventory = sample
    path = tmp_path / "inventory.json"
    path.write_text(json.dumps(inventory), encoding="utf-8")
    if changed:
        (root / "wrapper.py").write_bytes(b"changed\n")
    code = auditor["main"](["--repo-root", str(root), "--inventory", str(path)])
    assert code == expected_code
    captured = capsys.readouterr()
    report = json.loads(captured.out)
    assert report["solver_reuse"] == "NOT_EVALUATED"
    assert str(root) not in captured.out + captured.err


def test_cli_invalid_input_has_no_raw_diagnostic(auditor, tmp_path, capsys):
    path = tmp_path / "invalid.json"
    path.write_text("not JSON", encoding="utf-8")
    assert auditor["main"](["--inventory", str(path)]) == 2
    captured = capsys.readouterr()
    assert json.loads(captured.out) == {"error": "INVALID_INVENTORY_OR_ROOT"}
    assert str(tmp_path) not in captured.out + captured.err


def test_cli_missing_root_cannot_pass(auditor, sample, tmp_path, capsys):
    _, inventory = sample
    path = tmp_path / "inventory.json"
    path.write_text(json.dumps(inventory), encoding="utf-8")
    absent = tmp_path / "missing-root"
    assert auditor["main"](["--repo-root", str(absent), "--inventory", str(path)]) == 2
    captured = capsys.readouterr()
    assert json.loads(captured.out) == {"error": "INVALID_INVENTORY_OR_ROOT"}
    assert str(absent) not in captured.out + captured.err


def test_repository_inventory_is_current_and_explicitly_bounded(auditor):
    inventory = auditor["load_inventory"](ROOT / "docs/validation/solver-cleanup-inventory.json")
    report = auditor["audit"](ROOT, inventory)
    assert report["source_review_current"] is True
    assert "bounded" in report["scope"].lower()
    assert report["solver_reuse"] == "NOT_EVALUATED"
    ids = {row["id"] for row in report["entries"]}
    assert {"native-smoke-model", "cylinder-windows", "ghs-sentinel",
            "orcaflex-reproduce", "orcawave-batch", "aqwa-external-force"} <= ids
