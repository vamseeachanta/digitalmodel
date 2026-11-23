"""Read cited regular files across the 2026-11-23 digitalmodel history reset."""

from __future__ import annotations

import hashlib
import json
import os
import re
import subprocess
from functools import lru_cache
from pathlib import Path, PurePosixPath

RESET_ROOT = "5d3f3c9f9380c369a9117f34d1a2ce7082976463"
MANIFEST = Path("atlases/legacy-evidence/manifest.json")
MANIFEST_SHA256 = "1da37044ee67f360cb69632e30b67421c9de8ed5f5bdfacfb177c247ec7ed18e"


def git_output(repo_args: list[str], *args: str) -> bytes:
    """Read local, unreplaced objects even when called from a Git hook."""
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    env.update(GIT_NO_REPLACE_OBJECTS="1", GIT_ALLOW_PROTOCOL="", GIT_NO_LAZY_FETCH="1")
    return subprocess.check_output(
        ["git", *repo_args, "--literal-pathspecs", *args], env=env, stderr=subprocess.PIPE,
    )


@lru_cache(maxsize=1)
def _parse_manifest(raw: bytes) -> dict:
    if hashlib.sha256(raw).hexdigest() != MANIFEST_SHA256:
        raise ValueError("Frozen legacy manifest digest mismatch")
    return json.loads(raw)


def evidence_path(root: Path, relative: Path) -> Path:
    """Keep reads and migration writes inside the evidence directory."""
    if not relative.is_relative_to(MANIFEST.parent):
        raise ValueError(f"Unexpected legacy evidence path: {relative}")
    path = root.resolve() / relative
    if path.resolve() != path:
        raise ValueError(f"Redirected legacy evidence: {relative}")
    return path


def _manifest(root: Path) -> dict:
    try:
        raw = evidence_path(root, MANIFEST).read_bytes()
    except FileNotFoundError:
        return {}
    except (OSError, RuntimeError) as error:
        raise ValueError("Unavailable frozen legacy manifest") from error
    return _parse_manifest(raw)


def legacy_files(root: Path, revision: str, *, require_main: bool = False) -> dict:
    """The path allowlist for an exact old SHA or a recorded abbreviation."""
    manifest = _manifest(root)
    full = manifest.get("aliases", {}).get(revision, revision)
    files = manifest.get("files", {}).get(full, {})
    if require_main:
        return {path: entry for path, entry in files.items() if entry["rule"] == "main-ancestry"}
    return files


def publication_digest(record: dict) -> str:
    return hashlib.sha256(json.dumps(record, sort_keys=True).encode()).hexdigest()


def publication_revision(root: Path, record: dict) -> str | None:
    # A later publication can reuse the same path. Only the original record
    # can substitute its frozen search result for a search of current history.
    entry = _manifest(root).get("publications", {}).get(record["manifest"])
    if entry and entry["record_sha256"] == publication_digest(record):
        return entry["revision"]
    return None


def _tree_blob(repo_args: list[str], revision: str, path: str) -> bytes:
    entry = git_output(repo_args, "ls-tree", "-z", revision, "--", path)
    meta, _, name = entry.rstrip(b"\0").partition(b"\t")
    if name != path.encode() or not meta.startswith((b"100644 blob ", b"100755 blob ")):
        raise ValueError(f"Missing regular file at {revision}:{path}")
    return git_output(repo_args, "cat-file", "blob", meta.split()[2].decode())


def read_legacy(root: Path, revision: str, path: str, *, require_main: bool = False) -> bytes:
    """Verify both content identities before returning frozen evidence bytes."""
    entry = legacy_files(root, revision, require_main=require_main).get(path)
    if not entry:
        raise ValueError(f"No frozen evidence for {revision}:{path}")
    try:
        if entry["storage"] == "same-as-root":
            data = _tree_blob(["-C", str(root)], "HEAD", path)
        else:
            blob = MANIFEST.parent / "blobs" / entry["sha256"]
            data = evidence_path(root, blob).read_bytes()
    except (OSError, RuntimeError, subprocess.CalledProcessError) as error:
        raise ValueError(f"Unavailable legacy evidence for {revision}:{path}") from error
    git_blob = hashlib.sha1(f"blob {len(data)}\0".encode() + data).hexdigest()
    if git_blob != entry["blob"] or hashlib.sha256(data).hexdigest() != entry["sha256"]:
        raise ValueError(f"Legacy evidence digest mismatch for {revision}:{path}")
    return data


def read_evidence(root: Path, revision: str, path: str) -> bytes:
    """Read an exact frozen claim or require main ancestry for a new claim."""
    if (not re.fullmatch(r"[0-9a-f]{4,40}", revision) or not path
            or PurePosixPath(path).is_absolute() or "\\" in path
            or ".." in PurePosixPath(path).parts or PurePosixPath(path).as_posix() != path):
        raise ValueError("Invalid evidence revision or path")
    repo = ["-C", str(root)]
    try:
        objects = git_output(repo, "rev-parse", f"--disambiguate={revision}").splitlines()
        if not objects:
            return read_legacy(root, revision, path)
        if len(objects) != 1:
            raise ValueError(f"Ambiguous evidence revision: {revision}")
        revision = objects[0].decode()
        if git_output(repo, "cat-file", "-t", revision).strip() != b"commit":
            raise ValueError(f"Non-commit evidence revision: {revision}")
        if legacy_files(root, revision).get(path, {}).get("rule") != "exists":
            try:
                main = git_output(repo, "rev-parse", "--verify", "refs/remotes/origin/main")
            except subprocess.CalledProcessError:
                main = git_output(repo, "rev-parse", "--verify", "refs/heads/main")
            git_output(repo, "merge-base", "--is-ancestor", revision, main.decode().strip())
        return _tree_blob(repo, revision, path)
    except (OSError, subprocess.CalledProcessError) as error:
        raise ValueError(f"Unavailable evidence on main for {revision}:{path}") from error
