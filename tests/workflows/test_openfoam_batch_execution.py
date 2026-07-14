from __future__ import annotations

from pathlib import Path

from digitalmodel.workflows.openfoam_batch_execution import (
    checkpoint_matches,
    make_checkpoint,
)


def test_checkpoint_v2_requires_identity_owner_case_and_completed() -> None:
    checkpoint = make_checkpoint(
        identity_sha256="a" * 64, owner_token="token", case="case-1",
        row={"name": "case-1", "status": "completed"},
    )
    assert checkpoint_matches(checkpoint, "a" * 64, "token", "case-1")
    assert not checkpoint_matches(checkpoint, "b" * 64, "token", "case-1")
    assert not checkpoint_matches(checkpoint, "a" * 64, "foreign", "case-1")
    assert not checkpoint_matches({**checkpoint, "schema": 1}, "a" * 64, "token", "case-1")
    failed = {**checkpoint, "row": {"name": "case-1", "status": "failed"}}
    assert not checkpoint_matches(failed, "a" * 64, "token", "case-1")


def test_checkpoint_contains_no_external_path(tmp_path: Path) -> None:
    checkpoint = make_checkpoint(
        identity_sha256="a" * 64, owner_token="token", case="case-1",
        row={"name": "case-1", "status": "completed", "case_dir": "<external-work>/case-1"},
    )
    assert str(tmp_path) not in str(checkpoint)

