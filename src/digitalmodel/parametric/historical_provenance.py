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
import re
import subprocess
from pathlib import Path, PurePosixPath
from typing import Callable

import yaml

ROOT = Path(__file__).resolve().parents[3]
REGISTRY = "docs/registry/workflows.yaml"
REFRESH = "src/digitalmodel/parametric/refresh.py"
WRAPPER = "src/digitalmodel/parametric/generate.py"
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
    """Read an extracted source archive; reject links, including its root."""
    root = Path(root).absolute()

    def read(name: str) -> bytes:
        path = root / _relative(name)
        if any(part.is_symlink() for part in (path, *path.parents)):
            raise EvidenceError(f"Redirected evidence: {name}")
        try:
            return path.read_bytes()
        except OSError:
            raise EvidenceError(f"Unavailable evidence: {name}") from None

    return read


def _git(root: Path, *args: str) -> bytes:
    try:
        return subprocess.check_output(
            ["git", "-C", str(root), *args], stderr=subprocess.DEVNULL
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


def verify_artifacts(record: dict, root: Path) -> None:
    """Bind the evidence to the unchanged manifest, grid and validation files."""
    manifest = PurePosixPath(_relative(record["manifest"]))
    expected_paths = {
        str(manifest.parent / name)
        for name in ("manifest.yaml", "grid.parquet", "surrogate.json", "validation.json")
    }
    if set(record["artifacts"]) != expected_paths:
        raise EvidenceError("Atlas artifact inventory mismatch")
    read = archive_reader(root)
    for name, expected in record["artifacts"].items():
        if sha256(read(name)) != expected:
            raise EvidenceError(f"Artifact digest mismatch: {name}")
    manifest_data = yaml.safe_load(read(record["manifest"]))
    provenance = manifest_data["provenance"]
    if (provenance["content_fingerprint"] != record["content_fingerprint"]
            or provenance["workflow_id"] != record["workflow_id"]
            or provenance["code_version"] != record["recorded_code_version"]):
        raise EvidenceError("Manifest provenance mismatch")


def verify_git(record: dict, root: Path = ROOT, *, retained_ref: str = "origin/main") -> dict:
    """Require ancestry, authenticated sources and matching live artifacts."""
    read = git_reader(root, record["source_revision"])
    _git(root, "merge-base", "--is-ancestor", record["source_revision"], retained_ref)
    verify_artifacts(record, root)
    try:
        fingerprint = verify_sources(record, read)
    except FingerprintMismatch:
        if record["status"] != "NOT_EVALUATED" or record.get("reason") != MISMATCH:
            raise
        return {"status": "NOT_EVALUATED", "reason": MISMATCH}
    if record["status"] != "VERIFIED":
        raise EvidenceError("Recorded status contradicts reconstructed evidence")
    return {"status": "VERIFIED", "fingerprint": fingerprint}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--index", type=Path, default=ROOT / "atlases/historical-provenance.json")
    parser.add_argument("--retained-ref", default="origin/main")
    args = parser.parse_args(argv)
    inventory = json.loads(args.index.read_text())
    incomplete = False
    for record in inventory["atlases"]:
        try:
            result = verify_git(record, retained_ref=args.retained_ref)
        except EvidenceError as error:
            result = {"status": "REJECTED", "reason": str(error)}
        print(json.dumps({"manifest": record["manifest"], **result}, sort_keys=True))
        incomplete |= result["status"] != "VERIFIED"
    return int(incomplete)


if __name__ == "__main__":
    raise SystemExit(main())
