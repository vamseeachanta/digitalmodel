"""Inventory recorded atlas revisions without modifying manifests or Git refs.

Requires Python, PyYAML and Git. Reachability is established only with complete
history and an available retained ref, using git merge-base --is-ancestor.
"""
from __future__ import annotations

import argparse
from collections import Counter
import json
from pathlib import Path
import re
import subprocess

import yaml

SEARCH_PATTERN = "atlases/*/*/manifest.yaml"
STATUSES = ("REACHABLE", "UNREACHABLE", "UNAVAILABLE")


def _git(root: Path, *args: str) -> subprocess.CompletedProcess:
    return subprocess.run(
        ["git", "-C", str(root), *args], capture_output=True, text=True, check=False,
    )


def _history(root: Path, retained_ref: str) -> tuple[str, str | None, str | None]:
    try:
        top = _git(root, "rev-parse", "--show-toplevel")
    except FileNotFoundError:
        return "GIT_UNAVAILABLE", None, None
    if top.returncode or Path(top.stdout.strip()).resolve() != root.resolve():
        return "SOURCE_ARCHIVE", None, None
    head = _git(root, "rev-parse", "--verify", "HEAD")
    if head.returncode:
        return "HISTORY_UNAVAILABLE", None, None
    revision = head.stdout.strip()
    shallow = _git(root, "rev-parse", "--is-shallow-repository")
    if shallow.returncode:
        return "HISTORY_UNAVAILABLE", revision, None
    if shallow.stdout.strip() == "true":
        return "SHALLOW", revision, None
    retained = _git(root, "rev-parse", "--verify", "--end-of-options", f"{retained_ref}^{{commit}}")
    if retained.returncode:
        return "RETAINED_REF_UNAVAILABLE", revision, None
    return "COMPLETE", revision, retained.stdout.strip()


def inventory_manifests(repo_root: Path, retained_ref: str = "origin/main") -> dict:
    """Return exact manifest locators, original revisions and ancestry outcomes.

    Unknown objects, malformed manifests and incomplete history are UNAVAILABLE,
    never equivalent to a passing ancestry check. No network requests are made.
    """
    root = Path(repo_root)
    state, revision, retained = _history(root, retained_ref)
    rows = []
    for path in sorted(root.glob(SEARCH_PATTERN)):
        row = {"manifest": path.relative_to(root).as_posix(), "code_version": None,
               "status": "UNAVAILABLE", "reason": None}
        try:
            manifest = yaml.safe_load(path.read_text(encoding="utf-8"))
            code_version = manifest.get("provenance", {}).get("code_version")
        except (OSError, UnicodeError, yaml.YAMLError, AttributeError):
            row["reason"] = "manifest_invalid"
        else:
            row["code_version"] = code_version
            if code_version is None:
                row["reason"] = "revision_missing"
            elif not isinstance(code_version, str) or not re.fullmatch(r"[0-9a-fA-F]+", code_version):
                row["reason"] = "revision_invalid"
            elif state != "COMPLETE":
                row["reason"] = "history_shallow" if state == "SHALLOW" else state.lower()
            else:
                commit = _git(root, "rev-parse", "--verify", "--end-of-options", f"{code_version}^{{commit}}")
                if commit.returncode:
                    row["reason"] = "revision_unknown"
                else:
                    ancestor = _git(root, "merge-base", "--is-ancestor", commit.stdout.strip(), retained)
                    # Git documents status 0 for ancestor, 1 for non-ancestor;
                    # any other status is an operational/history failure.
                    if ancestor.returncode == 0:
                        row.update(status="REACHABLE", reason="ancestor_of_retained_ref")
                    elif ancestor.returncode == 1:
                        row.update(status="UNREACHABLE", reason="not_ancestor_of_retained_ref")
                    else:
                        row["reason"] = "ancestry_check_unavailable"
        rows.append(row)
    counts = Counter(row["status"] for row in rows)
    return {
        "search_pattern": SEARCH_PATTERN,
        "inventory_revision": revision,
        "retained_ref": retained_ref,
        "retained_revision": retained,
        "history_state": state,
        "counts": {status: counts[status] for status in STATUSES},
        "manifests": rows,
    }


def manifest_revision_failures(report: dict, exceptions: dict) -> list[str]:
    """Reject new gaps; exceptions bind one path, revision and observed failure.

    Exceptions acknowledge historical debt, never establish ancestry. An absent
    retained ref or other operational failure cannot be allow-listed.
    """
    if report["history_state"] != "COMPLETE":
        return ["complete Git history and retained ref required: " + report["history_state"]]
    if not report["manifests"]:
        return ["no manifests found for " + report["search_pattern"]]
    failures = []
    used = set()
    for row in report["manifests"]:
        path = row["manifest"]
        if row["status"] == "REACHABLE":
            continue
        exception = exceptions.get(path, {})
        if (row["reason"] in {"revision_unknown", "revision_missing", "not_ancestor_of_retained_ref"}
                and exception.get("code_version") == row["code_version"]
                and exception.get("status") == row["status"]
                and exception.get("inventory_reason") == row["reason"]
                and isinstance(exception.get("reason"), str)
                and exception["reason"].strip()):
            used.add(path)
        else:
            failures.append(f"{path}: {row['status']} ({row['reason']})")
    failures.extend("stale or unused exception: " + path for path in sorted(set(exceptions) - used))
    return failures


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo-root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--retained-ref", default="origin/main")
    args = parser.parse_args()
    print(json.dumps(inventory_manifests(args.repo_root, args.retained_ref), indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
