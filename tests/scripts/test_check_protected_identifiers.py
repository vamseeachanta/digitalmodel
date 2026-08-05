"""Hostile cases for the protected-identifier scanner (#1961, Stage 1).

Every value used here is SYNTHETIC. No protected identifier appears in this
file, in the scanner, or in the manifest -- the scanner scans its own tree, so a
real value committed anywhere in this repository would make the tool fail on
itself by construction (see ``test_scanner_scans_its_own_implementation``).

These tests are necessary but NOT sufficient. A scanner verified only by tests
that plant what its author thought to plant is validated against the wrong
population. The load-bearing verification is the retrospective corpus in
``scripts/legal/verify_public_surface.sh``, which runs against a leak population
nobody here constructed.
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[2]
SCANNER = REPO_ROOT / "scripts" / "legal" / "check_protected_identifiers.py"
MANIFEST = REPO_ROOT / "scripts" / "legal" / "protected-surface-v1.json"

# Synthetic rule values. Deliberately shaped like the real classes -- an
# alphanumeric code that can be followed by an underscore, and a two-name
# co-occurrence class -- without being any real identifier.
SYNTH_A = "zq7731"
SYNTH_B1 = "orgalpha"
SYNTH_B2 = "projbeta"

EXIT_CLEAN = 0
EXIT_FINDINGS = 1
EXIT_SCHEMA = 2
EXIT_NO_AUTHORITY = 3
EXIT_FAIL_CLOSED = 4


def _git(cwd: Path, *args: str) -> str:
    return subprocess.run(
        ["git", *args],
        cwd=cwd,
        check=True,
        capture_output=True,
        text=True,
    ).stdout


def _init_repo(root: Path) -> None:
    _git(root, "init", "-q", "-b", "main")
    _git(root, "config", "user.email", "t@example.invalid")
    _git(root, "config", "user.name", "t")


def _rules_file(tmp: Path, *, authority: str = "synthetic") -> Path:
    path = tmp / "rules.json"
    path.write_text(
        json.dumps(
            {
                "authority": authority,
                "rules": {
                    "PID-A1": {"class": "A", "values": [SYNTH_A]},
                    "PID-B1": {"class": "B", "values": [SYNTH_B1, SYNTH_B2]},
                },
            }
        ),
        encoding="utf-8",
    )
    return path


def _manifest_dict() -> dict:
    return {
        "schema_version": 1,
        "rules": {
            "PID-A1": {"class": "A", "matcher": "substring_ci"},
            "PID-B1": {"class": "B", "matcher": "co_occurrence_same_file"},
        },
        "classifications": [],
        "default_classification": "scan_text",
        "limits": {"max_file_bytes": 134217728},
        "line_sentinel": "protected-identifier-synthetic",
        "oracle": {"ref": "HEAD", "expected": {}},
    }


def _write_manifest(tmp: Path, data: dict) -> Path:
    path = tmp / "manifest.json"
    path.write_text(json.dumps(data), encoding="utf-8")
    return path


def _run(*args: str, cwd: Path | None = None) -> subprocess.CompletedProcess:
    return subprocess.run(
        [sys.executable, str(SCANNER), *args],
        cwd=str(cwd) if cwd else None,
        capture_output=True,
        text=True,
    )


def _scan(root: Path, tmp: Path, *, manifest: dict | None = None, extra: tuple[str, ...] = ()) -> dict:
    """Run the scanner over ``root`` and return its parsed JSON report."""
    manifest_path = _write_manifest(tmp, manifest if manifest is not None else _manifest_dict())
    proc = _run(
        "--manifest",
        str(manifest_path),
        "--rules",
        str(_rules_file(tmp)),
        "--root",
        str(root),
        "--json",
        *extra,
    )
    report = json.loads(proc.stdout)
    report["_returncode"] = proc.returncode
    report["_stderr"] = proc.stderr
    return report


def _findings_for(report: dict, rule_id: str) -> list[dict]:
    return [f for f in report["findings"] if f["rule_id"] == rule_id]


@pytest.fixture()
def repo(tmp_path: Path) -> Path:
    root = tmp_path / "repo"
    root.mkdir()
    _init_repo(root)
    return root


def _track(root: Path, relative: str, content: str | bytes) -> Path:
    path = root / relative
    path.parent.mkdir(parents=True, exist_ok=True)
    if isinstance(content, bytes):
        path.write_bytes(content)
    else:
        path.write_text(content, encoding="utf-8")
    _git(root, "add", "--", relative)
    return path


# --------------------------------------------------------------------------- #
# D1 -- matcher contract
# --------------------------------------------------------------------------- #


def test_unbounded_matcher_finds_token_followed_by_underscore(repo: Path, tmp_path: Path) -> None:
    """The refuted prescription, pinned. ``\\b`` never fires before ``_``."""
    _track(repo, "pkg/__init__.py", f'__all__ = ["{SYNTH_A}_default_taps"]\n')
    report = _scan(repo, tmp_path)
    assert len(_findings_for(report, "PID-A1")) == 1


def test_word_bounded_rule_is_rejected_by_the_manifest_schema(repo: Path, tmp_path: Path) -> None:
    manifest = _manifest_dict()
    manifest["rules"]["PID-A1"]["matcher"] = "substring_ci_word_bounded"
    report = _scan(repo, tmp_path, manifest=manifest)
    assert report["_returncode"] == EXIT_SCHEMA


def test_boundary_key_anywhere_in_a_rule_is_a_schema_error(repo: Path, tmp_path: Path) -> None:
    manifest = _manifest_dict()
    manifest["rules"]["PID-A1"]["word_boundary"] = True
    report = _scan(repo, tmp_path, manifest=manifest)
    assert report["_returncode"] == EXIT_SCHEMA


def test_content_addressed_field_excluded_by_classification(repo: Path, tmp_path: Path) -> None:
    """A hash-shaped field is suppressed by WHAT THE FIELD IS, never by bounding."""
    _track(
        repo,
        "lock.toml",
        'url = "https://files.pythonhosted.org/packages/54/fd/b207d1c5' + SYNTH_A + '85011f/x.whl"\n',
    )
    manifest = _manifest_dict()
    manifest["classifications"] = [
        {
            "id": "content-addressed-package-index-url",
            "kind": "content_addressed",
            "paths": ["lock.toml"],
            "field_patterns": ["https://files\\.pythonhosted\\.org/[^\"\\s]*"],
            "reason": "package-index URL path segments are content-addressed digests",
        }
    ]
    report = _scan(repo, tmp_path, manifest=manifest)
    assert len(_findings_for(report, "PID-A1")) == 0


def test_content_addressed_classification_does_not_suppress_the_rest_of_the_file(
    repo: Path, tmp_path: Path
) -> None:
    """The exclusion is field-scoped, not file-scoped."""
    _track(
        repo,
        "lock.toml",
        'url = "https://files.pythonhosted.org/packages/54/' + SYNTH_A + '/x.whl"\n'
        f'comment = "{SYNTH_A}_default_taps"\n',
    )
    manifest = _manifest_dict()
    manifest["classifications"] = [
        {
            "id": "content-addressed-package-index-url",
            "kind": "content_addressed",
            "paths": ["lock.toml"],
            "field_patterns": ["https://files\\.pythonhosted\\.org/[^\"\\s]*"],
            "reason": "package-index URL path segments are content-addressed digests",
        }
    ]
    report = _scan(repo, tmp_path, manifest=manifest)
    assert len(_findings_for(report, "PID-A1")) == 1


def test_whole_file_exemption_is_rejected(repo: Path, tmp_path: Path) -> None:
    manifest = _manifest_dict()
    manifest["classifications"] = [
        {
            "id": "blanket",
            "kind": "exempt",
            "paths": ["anything.py"],
            "reason": "a blanket exempt is a backdoor",
        }
    ]
    report = _scan(repo, tmp_path, manifest=manifest)
    assert report["_returncode"] == EXIT_SCHEMA


# --------------------------------------------------------------------------- #
# D2 -- class B, same-file co-occurrence, no window
# --------------------------------------------------------------------------- #


def test_class_b_fires_on_same_file_co_occurrence(repo: Path, tmp_path: Path) -> None:
    _track(repo, "config/a.yml", f"title: {SYNTH_B2} Well, {SYNTH_B1} Rig\n")
    report = _scan(repo, tmp_path)
    assert len(_findings_for(report, "PID-B1")) == 1


def test_class_b_does_not_fire_on_one_name_alone(repo: Path, tmp_path: Path) -> None:
    _track(repo, "config/a.yml", f"title: {SYNTH_B1} Rig\n")
    report = _scan(repo, tmp_path)
    assert len(_findings_for(report, "PID-B1")) == 0


def test_class_b_ignores_distance_between_the_two_names(repo: Path, tmp_path: Path) -> None:
    """No character or line window: any width would be a constant fitted to the data."""
    filler = "x\n" * 5000
    _track(repo, "config/a.yml", f"{SYNTH_B1}\n{filler}{SYNTH_B2}\n")
    report = _scan(repo, tmp_path)
    assert len(_findings_for(report, "PID-B1")) == 1


def test_os_codename_shape_is_not_a_class_b_finding(repo: Path, tmp_path: Path) -> None:
    """The confirmed false positive: one name alone, as an OS release codename."""
    _track(
        repo,
        "scripts/setup/provision.sh",
        f'[[ "${{VERSION_CODENAME:-}}" == "{SYNTH_B1}" ]] || warn "not {SYNTH_B1} (24.04)"\n',
    )
    report = _scan(repo, tmp_path)
    assert len(_findings_for(report, "PID-B1")) == 0


# --------------------------------------------------------------------------- #
# D3 / D6 -- exhaustive enumeration, no symmetric exclusion
# --------------------------------------------------------------------------- #


def test_unclassified_tracked_path_fails_when_no_default_is_declared(repo: Path, tmp_path: Path) -> None:
    _track(repo, "novel.weirdext", "harmless\n")
    manifest = _manifest_dict()
    del manifest["default_classification"]
    report = _scan(repo, tmp_path, manifest=manifest)
    assert report["_returncode"] == EXIT_FAIL_CLOSED


def test_a_skipping_default_classification_is_a_schema_error(repo: Path, tmp_path: Path) -> None:
    """The fail-closed direction is enforced: the default may not be a non-scanning kind."""
    manifest = _manifest_dict()
    manifest["default_classification"] = "content_addressed"
    report = _scan(repo, tmp_path, manifest=manifest)
    assert report["_returncode"] == EXIT_SCHEMA


def test_enumeration_matches_independent_git_ls_files(repo: Path, tmp_path: Path) -> None:
    for name in ("a.py", "b/c.txt", "d/e/f.yml"):
        _track(repo, name, "content\n")
    report = _scan(repo, tmp_path, extra=("--print-enumeration",))
    independent = sorted(
        p
        for p in _git(repo, "ls-files", "-z").split("\0")
        if p
    )
    assert report["enumeration"] == independent


def test_enumeration_count_is_non_zero(repo: Path, tmp_path: Path) -> None:
    _track(repo, "a.py", "content\n")
    report = _scan(repo, tmp_path)
    assert report["enumerated"] == 1


# --------------------------------------------------------------------------- #
# Git metadata surfaces
# --------------------------------------------------------------------------- #


def test_staged_blob_is_scanned_not_the_working_tree(repo: Path, tmp_path: Path) -> None:
    """The index carries the leak; the worktree has been cleaned since."""
    _track(repo, "m.py", f"NAME = '{SYNTH_A}_x'\n")
    (repo / "m.py").write_text("NAME = 'clean'\n", encoding="utf-8")
    report = _scan(repo, tmp_path, extra=("--staged",))
    assert len(_findings_for(report, "PID-A1")) == 1


def test_working_tree_scan_does_not_see_the_staged_only_leak(repo: Path, tmp_path: Path) -> None:
    """Proves the two entry points read different bytes -- not the same reader twice."""
    _track(repo, "m.py", f"NAME = '{SYNTH_A}_x'\n")
    (repo / "m.py").write_text("NAME = 'clean'\n", encoding="utf-8")
    report = _scan(repo, tmp_path)
    assert len(_findings_for(report, "PID-A1")) == 0


def test_commit_message_surface_is_scanned(repo: Path, tmp_path: Path) -> None:
    msg = tmp_path / "COMMIT_EDITMSG"
    msg.write_text(f"fix: drop {SYNTH_A}_default_taps\n", encoding="utf-8")
    report = _scan(repo, tmp_path, extra=("--commit-message-file", str(msg)))
    assert len(_findings_for(report, "PID-A1")) == 1


def test_staged_rename_scans_the_destination_path(repo: Path, tmp_path: Path) -> None:
    _track(repo, "old.py", "clean\n")
    _git(repo, "commit", "-qm", "seed")
    _git(repo, "mv", "old.py", "new.py")
    (repo / "new.py").write_text(f"NAME = '{SYNTH_A}_x'\n", encoding="utf-8")
    _git(repo, "add", "--", "new.py")
    report = _scan(repo, tmp_path, extra=("--staged",))
    assert [f["path"] for f in _findings_for(report, "PID-A1")] == ["new.py"]


def test_staged_deletion_removes_the_path_from_the_staged_enumeration(
    repo: Path, tmp_path: Path
) -> None:
    _track(repo, "gone.py", f"NAME = '{SYNTH_A}_x'\n")
    _git(repo, "commit", "-qm", "seed")
    _git(repo, "rm", "-q", "--", "gone.py")
    report = _scan(repo, tmp_path, extra=("--staged",))
    assert len(_findings_for(report, "PID-A1")) == 0


# --------------------------------------------------------------------------- #
# D6c -- fail closed on hostile artifacts
# --------------------------------------------------------------------------- #


def test_oversize_file_fails_closed_instead_of_being_skipped(repo: Path, tmp_path: Path) -> None:
    _track(repo, "big.bin", "x" * 4096)
    manifest = _manifest_dict()
    manifest["limits"]["max_file_bytes"] = 16
    report = _scan(repo, tmp_path, manifest=manifest)
    assert report["_returncode"] == EXIT_FAIL_CLOSED


def test_undecodable_bytes_are_still_scanned(repo: Path, tmp_path: Path) -> None:
    """Byte-oriented, so invalid UTF-8 cannot hide a match by being undecodable."""
    _track(repo, "blob.bin", b"\xff\xfe\x00" + SYNTH_A.encode() + b"_x\xff")
    report = _scan(repo, tmp_path)
    assert len(_findings_for(report, "PID-A1")) == 1


def test_symlink_target_outside_the_root_is_not_followed(repo: Path, tmp_path: Path) -> None:
    """Never follow: content reachable only through an escaping link is never read."""
    outside = tmp_path / "outside"
    outside.mkdir()
    (outside / "secret.txt").write_text(f"{SYNTH_A}_leak\n", encoding="utf-8")
    os.symlink(str(outside), str(repo / "escape"))
    _git(repo, "add", "--", "escape")
    report = _scan(repo, tmp_path)
    assert len(_findings_for(report, "PID-A1")) == 0


def test_symlink_link_text_is_itself_scanned(repo: Path, tmp_path: Path) -> None:
    """Not followed is not the same as not scanned -- the link text is bytes too."""
    os.symlink(f"../{SYNTH_A}_dir", str(repo / "link"))
    _git(repo, "add", "--", "link")
    report = _scan(repo, tmp_path)
    assert len(_findings_for(report, "PID-A1")) == 1


# --------------------------------------------------------------------------- #
# D9 / D11 -- authority, disclosure, self-coverage, sentinels
# --------------------------------------------------------------------------- #


def test_missing_rules_file_returns_a_distinct_code(repo: Path, tmp_path: Path) -> None:
    manifest_path = _write_manifest(tmp_path, _manifest_dict())
    proc = _run(
        "--manifest",
        str(manifest_path),
        "--rules",
        str(tmp_path / "absent.json"),
        "--root",
        str(repo),
        "--json",
    )
    assert proc.returncode == EXIT_NO_AUTHORITY


def test_synthetic_authority_is_announced_as_unauthenticated(repo: Path, tmp_path: Path) -> None:
    _track(repo, "a.py", "clean\n")
    report = _scan(repo, tmp_path)
    assert report["authority"] == "UNAUTHENTICATED"


def test_diagnostics_never_disclose_the_matched_value(repo: Path, tmp_path: Path) -> None:
    _track(repo, "m.py", f"NAME = '{SYNTH_A}_x'\n")
    manifest_path = _write_manifest(tmp_path, _manifest_dict())
    proc = _run(
        "--manifest",
        str(manifest_path),
        "--rules",
        str(_rules_file(tmp_path)),
        "--root",
        str(repo),
        "--json",
    )
    streams = (proc.stdout + proc.stderr).lower()
    # Paired with the finding count so an empty output cannot pass vacuously.
    found = len(_findings_for(json.loads(proc.stdout), "PID-A1"))
    assert (found, SYNTH_A in streams) == (1, False)


def test_finding_reports_rule_id_path_line_and_byte_offset(repo: Path, tmp_path: Path) -> None:
    _track(repo, "m.py", f"first\nNAME = '{SYNTH_A}_x'\n")
    report = _scan(repo, tmp_path)
    finding = _findings_for(report, "PID-A1")[0]
    assert [finding["path"], finding["line"], finding["byte_offset"], finding["class"]] == [
        "m.py",
        2,
        14,
        "A",
    ]


def test_scanner_scans_its_own_implementation_and_tests(tmp_path: Path) -> None:
    """No self-block hole: the scanner, its manifest and its tests are in the census,
    and each is classified with a kind that reads the bytes."""
    manifest_path = _write_manifest(tmp_path, _manifest_dict())
    proc = _run(
        "--manifest",
        str(manifest_path),
        "--rules",
        str(_rules_file(tmp_path)),
        "--root",
        str(REPO_ROOT),
        "--json",
        "--print-enumeration",
        "--enumerate-only",
    )
    classification = json.loads(proc.stdout)["classification"]
    own = [
        "scripts/legal/check_protected_identifiers.py",
        "scripts/legal/protected_surface_ownership.py",
        "scripts/legal/protected-surface-v1.json",
        "scripts/legal/public_surface_snapshot.py",
        "scripts/legal/verify_public_surface.sh",
        "tests/scripts/test_check_protected_identifiers.py",
        "tests/scripts/test_public_surface_snapshot.py",
    ]
    assert [classification.get(p) for p in own] == ["scan_text"] * len(own)


def test_exact_line_sentinel_is_honoured(repo: Path, tmp_path: Path) -> None:
    _track(repo, "m.py", f"NAME = '{SYNTH_A}_x'  # protected-identifier-synthetic\n")
    report = _scan(repo, tmp_path)
    assert len(_findings_for(report, "PID-A1")) == 0


def test_sentinel_on_another_line_does_not_cover_the_finding(repo: Path, tmp_path: Path) -> None:
    _track(repo, "m.py", f"# protected-identifier-synthetic\nNAME = '{SYNTH_A}_x'\n")
    report = _scan(repo, tmp_path)
    assert len(_findings_for(report, "PID-A1")) == 1


def test_scan_catches_a_planted_token(repo: Path, tmp_path: Path) -> None:
    """Necessary but NOT sufficient -- see the module docstring and D4."""
    _track(repo, "planted.py", f"x = '{SYNTH_A}'\n")
    report = _scan(repo, tmp_path)
    assert len(_findings_for(report, "PID-A1")) == 1


def test_a_clean_tree_exits_zero(repo: Path, tmp_path: Path) -> None:
    _track(repo, "clean.py", "x = 1\n")
    report = _scan(repo, tmp_path)
    assert report["_returncode"] == EXIT_CLEAN


def test_findings_exit_nonzero(repo: Path, tmp_path: Path) -> None:
    _track(repo, "dirty.py", f"x = '{SYNTH_A}'\n")
    report = _scan(repo, tmp_path)
    assert report["_returncode"] == EXIT_FINDINGS


# --------------------------------------------------------------------------- #
# D4 -- the oracle's plumbing (the oracle itself runs outside pytest)
# --------------------------------------------------------------------------- #


def test_historical_ref_mode_reads_the_pinned_tree_not_the_worktree(
    repo: Path, tmp_path: Path
) -> None:
    _track(repo, "m.py", f"NAME = '{SYNTH_A}_x'\n")
    _git(repo, "commit", "-qm", "seed")
    old = _git(repo, "rev-parse", "HEAD").strip()
    (repo / "m.py").write_text("NAME = 'clean'\n", encoding="utf-8")
    _git(repo, "add", "--", "m.py")
    _git(repo, "commit", "-qm", "clean")
    report = _scan(repo, tmp_path, extra=("--ref", old))
    assert len(_findings_for(report, "PID-A1")) == 1
