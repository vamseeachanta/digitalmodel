"""Exact-output publication and transactional replacement of owned portfolios."""
from __future__ import annotations

import hashlib
import json
from pathlib import Path
import shutil
import tempfile
from uuid import uuid4

from cp_portfolio_contract import validate_release

RECEIPT = "portfolio-receipt.json"


def tree_digest(folder: Path) -> dict[str, str]:
    """Include every artifact, catching unlisted data as well as altered values."""
    files = {}
    for path in sorted(folder.rglob("*")):
        if path.is_symlink():
            raise ValueError("symlink not permitted in released portfolio")
        if path.is_file() and path != folder / RECEIPT:
            files[path.relative_to(folder).as_posix()] = hashlib.sha256(path.read_bytes()).hexdigest()
    return files


def publish(candidate: Path, destination: Path, release: dict[str, str],
            *, partial_review: bool = False) -> None:
    """Publish only the exact agent-reviewed bytes; preserve edits and rollback."""
    candidate, destination = candidate.resolve(), destination.resolve()
    if candidate == destination or candidate.is_relative_to(destination):
        raise ValueError("candidate must be independent of destination")
    validate_release(tree_digest(candidate), release)
    coverage = candidate / "coverage.json"
    if coverage.exists() and not partial_review:
        if not json.loads(coverage.read_text("utf-8"))["summary"]["complete"]:
            raise ValueError("incomplete portfolio requires explicit partial-review publication")
    if destination.exists():
        receipt = destination / RECEIPT
        if not receipt.is_file():
            raise ValueError("unmanaged destination; preserve and review before publication")
        if tree_digest(destination) != json.loads(receipt.read_text("utf-8")):
            raise ValueError("existing portfolio modified; preserve reviewer work")
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="cp-publish-", dir=destination.parent) as temp:
        staging = Path(temp).resolve()
        if staging.parent != destination.parent:
            raise ValueError("staging directory outside intended parent")
        incoming = staging / "incoming"
        backup = destination.with_name(f"{destination.name}-previous-{uuid4().hex}")
        shutil.copytree(candidate, incoming)
        validate_release(tree_digest(incoming), release)
        (incoming / RECEIPT).write_text(json.dumps(release, indent=2, sort_keys=True) + "\n",
                                      encoding="utf-8", newline="\n")
        if destination.exists():
            destination.rename(backup)
        try:
            incoming.rename(destination)
        except OSError:
            if backup.exists():
                backup.rename(destination)
            raise
