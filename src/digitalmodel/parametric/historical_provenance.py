# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reconstruct historical atlas fingerprints without importing historical code.

The trusted index pins raw source blobs and unchanged atlas artifacts. Its
``source_revision`` is a retained publication snapshot, not a replacement for
the manifest's original ``code_version`` or proof of a numerical execution.
The legacy hash contract is refresh.py::content_fingerprint at the recorded
revision: raw bytes, ordered source paths and sorted-key JSON (no rounding).
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import json
import os
import re
import subprocess
from pathlib import Path, PurePosixPath
from typing import Callable

import yaml

ROOT = Path(__file__).resolve().parents[3]
REGISTRY = "docs/registry/workflows.yaml"
REFRESH = "src/digitalmodel/parametric/refresh.py"
WRAPPER = "src/digitalmodel/parametric/generate.py"
INDEX = "atlases/historical-provenance.json"
MISMATCH = "Stored fingerprint does not match retained publication basis."
Reader = Callable[[str], bytes]


class EvidenceError(ValueError):
    """Required historical evidence is missing, redirected or inconsistent."""


class FingerprintMismatch(EvidenceError):
    """Authenticated source bytes do not reconstruct the recorded fingerprint."""


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _relative(name: str) -> str:
    path = PurePosixPath(name)
    if (not name or path.is_absolute() or "\\" in name
            or ".." in path.parts or path.as_posix() != name):
        raise EvidenceError("Evidence path must be a normalized relative path")
    return name


def archive_reader(root: Path) -> Reader:
    """Read an extracted source archive; reject links and directory junctions."""
    root = Path(root).absolute()

    def read(name: str) -> bytes:
        name = _relative(name)
        try:
            # Resolve parents above the boundary, then require each component
            # to retain its identity, including junctions on older Python.
            expected = root.parent.resolve() / root.name
            if root.is_symlink() or root.resolve() != expected:
                raise EvidenceError(f"Redirected evidence: {name}")
            path = root
            for part in PurePosixPath(name).parts:
                path = path / part
                expected = expected / part
                if path.is_symlink() or path.resolve() != expected:
                    raise EvidenceError(f"Redirected evidence: {name}")
            return path.read_bytes()
        except (OSError, RuntimeError):
            raise EvidenceError(f"Unavailable evidence: {name}") from None

    return read


def _git(root: Path, *args: str, input_bytes: bytes | None = None) -> bytes:
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE",
                          "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES"}}
    env["GIT_NO_REPLACE_OBJECTS"] = "1"
    try:
        return subprocess.check_output(
            ["git", "-C", str(root), *args], input=input_bytes, stderr=subprocess.DEVNULL,
            env=env,
        )
    except (OSError, subprocess.CalledProcessError):
        raise EvidenceError("Unavailable Git evidence or retained history") from None


def git_reader(root: Path, revision: str) -> Reader:
    """Read blobs only; no checkout, imports, subprocess code or solver runs."""
    # Full SHA-1 object names match this repository's retained Git object format.
    if not re.fullmatch(r"[0-9a-f]{40}", revision):
        raise EvidenceError("Source revision must be a full Git object ID")
    _git(root, "cat-file", "-e", f"{revision}^{{commit}}")

    def read(name: str) -> bytes:
        name = _relative(name)
        # git show on a symlink returns the target text, not source bytes.
        entry = _git(root, "ls-tree", revision, "--", name)
        if not entry.startswith(b"100644 ") and not entry.startswith(b"100755 "):
            raise EvidenceError(f"Unavailable regular source blob: {name}")
        return _git(root, "show", f"{revision}:{name}")

    return read


def legacy_fingerprint(spec, response_kwargs, standards, input_bytes, sources) -> str:
    """Legacy refresh.py hash; preserve raw line endings and source list order."""
    source_hash = hashlib.sha256()
    for name, data in sources:
        source_hash.update(name.encode())
        source_hash.update(data)
    basis = {
        "spec": spec,
        "response_kwargs": response_kwargs,
        "standards": standards,
        "input_sha256": sha256(input_bytes),
        "source_sha256": source_hash.hexdigest(),
    }
    return sha256(json.dumps(basis, sort_keys=True).encode())


def _basis(workflow_id: str, read: Reader) -> tuple[dict, list[str]]:
    """Extract literal historical metadata; never eval or exec archived code."""
    try:
        constants = {}
        for node in ast.parse(read(REFRESH)).body:
            if (isinstance(node, ast.AnnAssign)
                    and isinstance(node.target, ast.Name)
                    and node.target.id in {"SOURCE_FILES", "STANDARDS"}):
                constants[node.target.id] = ast.literal_eval(node.value)
        rows = yaml.safe_load(read(REGISTRY))["workflows"]
        row = next(r for r in rows if r["id"] == workflow_id)
        block = row["parametric"]
        basename = row["basename"]
        basis = {
            "spec": {key: block[key] for key in ("physics", "response", "axes")},
            "response_kwargs": block.get("response_kwargs", {}),
            "standards": constants["STANDARDS"].get(basename, []),
            "input": row["input"],
            "response_sources": constants["SOURCE_FILES"].get(basename, []),
        }
    except (KeyError, StopIteration, SyntaxError, TypeError, ValueError, yaml.YAMLError):
        raise EvidenceError("Historical registry or literal source basis unavailable") from None
    paths = [REGISTRY, REFRESH, WRAPPER, basis["input"], *basis["response_sources"]]
    return basis, list(dict.fromkeys(paths))


def _fingerprint(basis: dict, read: Reader) -> str:
    return legacy_fingerprint(
        basis["spec"], basis["response_kwargs"], basis["standards"],
        read(basis["input"]), [(name, read(name)) for name in basis["response_sources"]],
    )


def source_record(workflow_id: str, read: Reader) -> dict:
    """Build a reviewable digest map from an explicitly selected snapshot."""
    basis, paths = _basis(workflow_id, read)
    return {"basis": basis, "sources": {name: sha256(read(name)) for name in paths}}


def verify_sources(record: dict, read: Reader) -> str:
    """Verify a trusted digest map and reconstruct its historical fingerprint.

    Works without Git using archive_reader. All listed bytes, including the
    complete response wrapper module, are checked even though the legacy
    fingerprint did not hash wrappers. An archive must retain those exact bytes.
    """
    if record["algorithm"] != "legacy-raw-v1":
        raise EvidenceError("Unsupported historical fingerprint algorithm")
    blobs = {}
    for name, expected in record["sources"].items():
        data = read(name)
        if sha256(data) != expected:
            raise EvidenceError(f"Source digest mismatch: {name}")
        blobs[name] = data

    def checked(name: str) -> bytes:
        if name not in blobs:
            raise EvidenceError(f"Required source digest missing: {name}")
        return blobs[name]

    basis, paths = _basis(record["workflow_id"], checked)
    if set(paths) != set(blobs) or basis != record["basis"]:
        raise EvidenceError("Historical basis or source inventory mismatch")
    fingerprint = _fingerprint(basis, checked)
    if fingerprint != record["content_fingerprint"]:
        raise FingerprintMismatch(MISMATCH)
    return fingerprint


def verify_artifacts(record: dict, root: Path, *, revision: str | None = None) -> None:
    """Check raw artifact bytes from an archive or a pinned Git revision."""
    manifest = PurePosixPath(_relative(record["manifest"]))
    expected_paths = {
        str(manifest.parent / name)
        for name in ("manifest.yaml", "grid.parquet", "surrogate.json", "validation.json")
    }
    if set(record["artifacts"]) != expected_paths:
        raise EvidenceError("Atlas artifact inventory mismatch")
    read = git_reader(root, revision) if revision is not None else archive_reader(root)
    for name, expected in record["artifacts"].items():
        if sha256(read(name)) != expected:
            raise EvidenceError(f"Artifact digest mismatch: {name}")
    manifest_data = yaml.safe_load(read(record["manifest"]))
    provenance = manifest_data["provenance"]
    if (provenance["content_fingerprint"] != record["content_fingerprint"]
            or provenance["workflow_id"] != record["workflow_id"]
            or provenance["code_version"] != record["recorded_code_version"]):
        raise EvidenceError("Manifest provenance mismatch")


def _working_tree_drift(record: dict, root: Path, revision: str) -> list[str]:
    read = archive_reader(root)
    drift = []
    for name in record["artifacts"]:
        try:
            # Hash validated bytes without writing objects or re-opening a path.
            actual = _git(root, "hash-object", f"--path={name}", "--stdin", input_bytes=read(name))
            expected = _git(root, "rev-parse", f"{revision}:{name}")
            unchanged = actual == expected
        except EvidenceError:
            unchanged = False
        if not unchanged:
            drift.append(name)
    return sorted(drift)


def _commit(root: Path, ref: str) -> str:
    return _git(root, "rev-parse", "--verify", "--end-of-options", f"{ref}^{{commit}}").decode().strip()


def _ancestor(root: Path, revision: str, head: str) -> bool:
    try:
        _git(root, "merge-base", "--is-ancestor", revision, head)
    except EvidenceError:
        return False
    return True


def _inventory_at(root: Path, revision: str) -> dict[str, dict]:
    if not _git(root, "ls-tree", revision, "--", INDEX):
        return {}
    try:
        rows = json.loads(git_reader(root, revision)(INDEX))["atlases"]
        entries = {row["manifest"]: row for row in rows}
        if len(entries) != len(rows):
            raise ValueError("Duplicate manifest")
        return entries
    except (KeyError, TypeError, ValueError):
        raise EvidenceError("Invalid committed inventory") from None


def _ci_head(root: Path, ci_ref: str, retained: str, revision: str) -> str:
    pull_ref = ci_ref.rsplit("/", 1)[0] + "/head"
    event_path = os.environ.get("GITHUB_EVENT_PATH")
    if event_path:
        try:
            event = json.loads(Path(event_path).read_text(encoding="utf-8"))
            head = event["pull_request"]["head"]["sha"]
            if not isinstance(head, str) or not re.fullmatch(r"[0-9a-f]{40}", head):
                raise ValueError("Invalid head object ID")
            return _commit(root, head)
        except (OSError, ValueError, KeyError, TypeError):
            raise EvidenceError("Unavailable or invalid CI pull request head in event payload") from None
    try:
        return _commit(root, pull_ref)
    except EvidenceError:
        parents = _git(root, "rev-list", "--parents", "-n", "1", revision).decode().split()
        if ci_ref.endswith("/merge") and len(parents) == 3 and parents[1] == retained:
            return parents[2]
        raise EvidenceError(
            f"Missing CI pull request head ref {pull_ref}; provide the event head SHA "
            "or a two-parent merge whose first parent is the retained tip"
        ) from None


def _verify_retention(record: dict, root: Path, retained_ref: str,
                      revision: str, pr_head: str | None) -> None:
    retained = _commit(root, retained_ref)
    source = record["source_revision"]
    on_default = _ancestor(root, source, retained)
    retention_hint = (f" Retained ref {retained_ref} ({retained}) does not retain source {source}, "
                      "or the retained ref is stale; fetch it.")
    try:
        branch = _git(root, "symbolic-ref", "--quiet", "HEAD").decode().strip()
    except EvidenceError:
        branch = ""
    default_branch = "refs/heads/" + retained_ref.rsplit("/", 1)[-1]
    if (_ancestor(root, revision, retained)
            or branch == default_branch or os.environ.get("GITHUB_REF") == default_branch):
        if not on_default:
            raise EvidenceError(
                "Source revision is not retained on the default branch; possible squash-merged "
                "orphan. Provenance-entry changes require a merge commit, not a squash merge."
                + retention_hint
            )
        return

    ci_ref = re.fullmatch(r"refs/pull/([0-9]+)/(?:head|merge)", os.environ.get("GITHUB_REF", ""))
    if ci_ref:
        head = _ci_head(root, ci_ref[0], retained, revision)
        if pr_head and _commit(root, pr_head) != head:
            raise EvidenceError("Explicit head does not match the CI pull request head")
    else:
        head = _commit(root, pr_head or "HEAD")
    if not _ancestor(root, head, revision):
        raise EvidenceError("Pull request head is outside the current checkout ancestry")
    base = _git(root, "merge-base", retained, head).decode().strip()
    before = _inventory_at(root, base)
    current = _inventory_at(root, head).get(record["manifest"])
    changed = current is not None and current != before.get(record["manifest"])
    if changed:
        if current != record:
            raise EvidenceError("Entry does not match the committed inventory at the pull request head")
        if not _ancestor(root, source, head):
            raise EvidenceError("Changed entry source revision is not contained in the pull request head")
    elif not on_default:
        raise EvidenceError("An unchanged entry requires source retention on the default branch."
                            + retention_hint)


def verify_git(record: dict, root: Path = ROOT, *, retained_ref: str = "origin/main",
               pr_head: str | None = None) -> dict:
    """Verify committed evidence and report checkout artifact drift separately.

    Only changed, committed inventory entries may use pull-request ancestry.
    Default-branch checks always require ancestry in the retained default ref.
    """
    read = git_reader(root, record["source_revision"])
    revision = _git(root, "rev-parse", "HEAD").decode("ascii").strip()
    _verify_retention(record, root, retained_ref, revision, pr_head)
    verify_artifacts(record, root, revision=revision)
    try:
        fingerprint = verify_sources(record, read)
    except FingerprintMismatch:
        if record["status"] != "NOT_EVALUATED" or record.get("reason") != MISMATCH:
            raise
        result: dict = {"status": "NOT_EVALUATED", "reason": MISMATCH}
    else:
        if record["status"] != "VERIFIED":
            raise EvidenceError("Recorded status contradicts reconstructed evidence")
        result = {"status": "VERIFIED", "fingerprint": fingerprint}
    result["working_tree_drift"] = _working_tree_drift(record, root, revision)
    return result


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--index", type=Path, default=ROOT / "atlases/historical-provenance.json")
    parser.add_argument("--retained-ref", default="origin/main")
    parser.add_argument("--pr-head", help="Pull request head ref contained in the checked revision")
    args = parser.parse_args(argv)
    inventory = json.loads(args.index.read_text(encoding="utf-8"))
    incomplete = False
    for record in inventory["atlases"]:
        try:
            options = {"pr_head": args.pr_head} if args.pr_head else {}
            result = verify_git(record, retained_ref=args.retained_ref, **options)
        except EvidenceError as error:
            result = {"status": "REJECTED", "reason": str(error)}
        print(json.dumps({"manifest": record["manifest"], **result}, sort_keys=True))
        incomplete |= result["status"] != "VERIFIED" or bool(result.get("working_tree_drift"))
    return int(incomplete)


if __name__ == "__main__":
    raise SystemExit(main())
