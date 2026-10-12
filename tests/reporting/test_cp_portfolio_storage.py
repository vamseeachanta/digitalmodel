"""Publication must check reviewed output values and preserve existing work."""
import importlib.util
from pathlib import Path
import sys
from typing import Any

import pytest

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/reporting/cp_portfolio_storage.py"
sys.path.insert(0, str(SCRIPT.parent))
SPEC = importlib.util.spec_from_file_location("cp_portfolio_storage", SCRIPT)
assert SPEC and SPEC.loader
storage = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(storage)


def test_private_output_injection_rejected_before_destination_changes(tmp_path: Path) -> None:
    candidate = tmp_path / "candidate"
    candidate.mkdir()
    (candidate / "results.json").write_text('{"value": 1}')
    release = storage.tree_digest(candidate)
    destination = tmp_path / "published"
    storage.publish(candidate, destination, release)
    before = (destination / "results.json").read_bytes()
    (candidate / "results.json").write_text('{"value": 1, "private_vector": [37.25]}')
    with pytest.raises(ValueError, match="release"):
        storage.publish(candidate, destination, release)
    assert (destination / "results.json").read_bytes() == before


def test_unlisted_file_and_reviewed_sidecar_prevent_overwrite(tmp_path: Path) -> None:
    candidate = tmp_path / "candidate"
    candidate.mkdir()
    (candidate / "report.comments.json").write_text('{"comments": []}')
    release = storage.tree_digest(candidate)
    destination = tmp_path / "published"
    storage.publish(candidate, destination, release)
    (destination / "notes.json").write_text('{"reviewer": "Keep"}')
    with pytest.raises(ValueError, match="modified"):
        storage.publish(candidate, destination, release)
    (destination / "notes.json").unlink()
    (destination / "report.comments.json").write_text('{"comments": [{"text": "Keep"}]}')
    with pytest.raises(ValueError, match="modified"):
        storage.publish(candidate, destination, release)


def test_failed_copy_leaves_existing_portfolio_intact(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    candidate = tmp_path / "candidate"
    candidate.mkdir()
    (candidate / "results.json").write_text('{"value": 1}')
    destination = tmp_path / "published"
    storage.publish(candidate, destination, storage.tree_digest(candidate))
    (candidate / "results.json").write_text('{"value": 2}')
    def fail(*args: Any, **kwargs: Any) -> None:
        raise OSError("injected disk failure")
    monkeypatch.setattr(storage.shutil, "copytree", fail)
    with pytest.raises(OSError):
        storage.publish(candidate, destination, storage.tree_digest(candidate))
    assert (destination / "results.json").read_text() == '{"value": 1}'


def test_partial_portfolio_requires_explicit_draft_publication(tmp_path: Path) -> None:
    candidate = tmp_path / "candidate"
    candidate.mkdir()
    (candidate / "coverage.json").write_text('{"summary": {"complete": false}}')
    release = storage.tree_digest(candidate)
    with pytest.raises(ValueError, match="incomplete"):
        storage.publish(candidate, tmp_path / "final", release)
    storage.publish(candidate, tmp_path / "draft", release, partial_review=True)


def test_previous_generation_remains_preserved(tmp_path: Path) -> None:
    candidate = tmp_path / "candidate"
    candidate.mkdir()
    (candidate / "results.json").write_text('{"value": 1}')
    destination = tmp_path / "published"
    storage.publish(candidate, destination, storage.tree_digest(candidate))
    (candidate / "results.json").write_text('{"value": 2}')
    storage.publish(candidate, destination, storage.tree_digest(candidate))
    backups = list(tmp_path.glob("published-previous-*"))
    assert len(backups) == 1
    assert (backups[0] / "results.json").read_text() == '{"value": 1}'
