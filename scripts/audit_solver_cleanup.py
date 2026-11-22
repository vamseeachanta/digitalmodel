"""Check freshness of a bounded, manually reviewed process-cleanup inventory.

Inspected files are read as bytes, never imported or executed. This is a source
audit, not a process observer or a solver dispatch gate. Exit success means the
listed source bytes match; it never means cleanup or licence release succeeded.

Evidence basis: issue #2095 and Microsoft, Job Objects (2025), sections Managing
Processes in Jobs and Job Termination; JOBOBJECT_BASIC_ACCOUNTING_INFORMATION
(2024), ActiveProcesses. Source links and review limits accompany the inventory
in docs/validation/solver-cleanup-audit.md. No physical equations or acceptance
limits apply to this source-identity check.
"""

import argparse
import hashlib
import json
from pathlib import Path
import re
import stat


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_INVENTORY = ROOT / "docs/validation/solver-cleanup-inventory.json"
EVIDENCE_FIELDS = (
    "termination_requested", "root_exited", "whole_owned_job_drained", "cleanup_unknown",
)
CLASSIFICATIONS = {
    "QUERY_BEFORE_CLOSE", "ROOT_ONLY", "GROUP_ONLY", "CALLER_CENSUS", "IN_PROCESS",
}


def _shape(value, fields):
    if not isinstance(value, dict) or set(value) != set(fields):
        raise ValueError("Invalid inventory fields")


def _text(value):
    if not isinstance(value, str) or not value.strip():
        raise ValueError("Nonempty inventory text required")


def _relative_path(value):
    _text(value)
    if (value.startswith("/") or any(c in value for c in ("\\", ":"))
            or any(ord(c) < 32 for c in value)
            or any(p in ("", ".", "..") for p in value.split("/"))):
        raise ValueError("Repository-relative POSIX source path required")


def validate_inventory(inventory):
    """Reject incomplete, duplicate or ambiguous source-review records."""
    _shape(inventory, {"scope", "entries"})
    _text(inventory["scope"])
    entries = inventory["entries"]
    if not isinstance(entries, list) or not entries:
        raise ValueError("Nonempty inventory required")
    ids = set()
    for entry in entries:
        _shape(entry, {"id", "classification", "sources", "observations", "follow_up"})
        _text(entry["id"])
        if not re.fullmatch("[a-z][a-z0-9-]*", entry["id"]) or entry["id"] in ids:
            raise ValueError("Unique inventory identifiers required")
        ids.add(entry["id"])
        _text(entry["classification"])
        if entry["classification"] not in CLASSIFICATIONS:
            raise ValueError("Unknown source classification")
        _shape(entry["observations"], EVIDENCE_FIELDS)
        for observation in entry["observations"].values():
            _text(observation)
        _text(entry["follow_up"])
        if not isinstance(entry["sources"], list) or not entry["sources"]:
            raise ValueError("Source dependencies required")
        paths = set()
        for source in entry["sources"]:
            _shape(source, {"path", "sha256"})
            _relative_path(source["path"])
            if source["path"] in paths:
                raise ValueError("Duplicate source dependency")
            paths.add(source["path"])
            _text(source["sha256"])
            # SHA-256 hexadecimal width follows hashlib's digest size.
            width = hashlib.sha256().digest_size * 2
            if not re.fullmatch(f"[0-9a-f]{{{width}}}", source["sha256"]):
                raise ValueError("Invalid source digest")


def _unique(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("Duplicate inventory key")
        result[key] = value
    return result


def load_inventory(path):
    inventory = json.loads(Path(path).read_text(encoding="utf-8"), object_pairs_hook=_unique)
    validate_inventory(inventory)
    return inventory


def _source_status(root, source):
    candidate = root
    try:
        for part in source["path"].split("/"):
            candidate = candidate / part
            if candidate.is_symlink():
                return "SOURCE_SYMLINK_REFUSED"
        if not stat.S_ISREG(candidate.stat().st_mode):
            return "SOURCE_NOT_REGULAR"
        actual = hashlib.sha256(candidate.read_bytes()).hexdigest()
    except FileNotFoundError:
        return "SOURCE_MISSING"
    except OSError:
        return "SOURCE_UNREADABLE"
    return "SOURCE_MATCH" if actual == source["sha256"] else "SOURCE_CHANGED"


def audit(repo_root, inventory):
    """Bind manual findings to source bytes; leave every runtime fact unknown.

    A matching inventory is a bounded snapshot, not discovery of every harness.
    The source read is not an atomic launch transaction and grants no permission.
    """
    validate_inventory(inventory)
    root = Path(repo_root).resolve(strict=True)
    if not root.is_dir():
        raise ValueError("Repository root must be a directory")
    rows = []
    for entry in inventory["entries"]:
        sources = [dict(source, status=_source_status(root, source))
                   for source in entry["sources"]]
        current = all(source["status"] == "SOURCE_MATCH" for source in sources)
        rows.append({
            "id": entry["id"],
            "source_status": "SOURCE_MATCH" if current else "REVIEW_REQUIRED",
            "classification": entry["classification"] if current else "NOT_EVALUATED",
            "observations": dict(entry["observations"]) if current else None,
            "sources": sources,
            "runtime_evidence": dict.fromkeys(EVIDENCE_FIELDS, "NOT_EVALUATED"),
            "follow_up": entry["follow_up"],
        })
    return {
        "scope": inventory["scope"],
        "source_review_current": all(row["source_status"] == "SOURCE_MATCH" for row in rows),
        "solver_reuse": "NOT_EVALUATED",
        "entries": rows,
    }


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo-root", type=Path, default=ROOT)
    parser.add_argument("--inventory", type=Path, default=DEFAULT_INVENTORY)
    args = parser.parse_args(argv)
    try:
        report = audit(args.repo_root, load_inventory(args.inventory))
    except (OSError, ValueError):
        # Do not expose absolute paths or inspected source in public diagnostics.
        print(json.dumps({"error": "INVALID_INVENTORY_OR_ROOT"}))
        return 2
    print(json.dumps(report, indent=2, sort_keys=True))
    return 0 if report["source_review_current"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
