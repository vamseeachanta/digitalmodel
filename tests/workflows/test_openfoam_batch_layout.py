from __future__ import annotations

import json
from pathlib import Path

import pytest

from digitalmodel.workflows.openfoam_batch_config import resolve_batch_paths
from digitalmodel.workflows.openfoam_batch_layout import WorkLayout


def test_hosted_root_is_environment_owned_and_output_remains_local(tmp_path: Path) -> None:
    cfg_dir = tmp_path / "scope" / "case"
    cfg_dir.mkdir(parents=True)
    root = tmp_path / "scratch"
    root.mkdir()
    paths = resolve_batch_paths(
        {"work_root_namespace": "gpu-canary", "work_root": str(tmp_path / "forbidden")},
        cfg_dir,
        env={
            "DIGITALMODEL_EXECUTION_CONTEXT": "hosted-deckhand",
            "DIGITALMODEL_WORK_ROOT": str(root),
        },
    )
    assert paths.operator_root == root.resolve()
    assert paths.output_dir == cfg_dir / "results"
    assert paths.namespace == "gpu-canary"


@pytest.mark.parametrize("namespace", ["../escape", ".", "a//b", "a\\b", "x\x00y"])
def test_namespace_rejects_nonportable_components(tmp_path: Path, namespace: str) -> None:
    root = tmp_path / "root"
    root.mkdir()
    with pytest.raises(ValueError, match="namespace"):
        resolve_batch_paths(
            {"work_root_namespace": namespace}, tmp_path,
            env={"DIGITALMODEL_EXECUTION_CONTEXT": "hosted-deckhand",
                 "DIGITALMODEL_WORK_ROOT": str(root)},
        )


def test_hosted_missing_root_rejects_before_side_effect(tmp_path: Path) -> None:
    before = set(tmp_path.iterdir())
    with pytest.raises(ValueError, match="DIGITALMODEL_WORK_ROOT"):
        resolve_batch_paths({}, tmp_path, env={"DIGITALMODEL_EXECUTION_CONTEXT": "hosted-deckhand"})
    assert set(tmp_path.iterdir()) == before


def test_trusted_local_requires_precreated_absolute_non_git_root(tmp_path: Path) -> None:
    cfg_dir = tmp_path / "checkout" / "case"
    cfg_dir.mkdir(parents=True)
    (tmp_path / "checkout" / ".git").mkdir()
    external = tmp_path / "scratch"
    external.mkdir()
    paths = resolve_batch_paths(
        {"execution_context": "trusted-local", "work_root": str(external)}, cfg_dir, env={}
    )
    assert paths.operator_root == external.resolve()
    with pytest.raises(ValueError, match="Git checkout"):
        resolve_batch_paths(
            {"execution_context": "trusted-local", "work_root": str(cfg_dir)}, cfg_dir, env={}
        )


def test_owned_layout_rejects_foreign_marker_and_cleans_only_case(tmp_path: Path) -> None:
    layout = WorkLayout.create(tmp_path, "ns", "a" * 64)
    case = layout.case_dir("case-001")
    case.mkdir(parents=True)
    (case / "heavy").write_text("x")
    sibling = layout.run_dir / "keep"
    sibling.write_text("safe")
    layout.clean_case("case-001")
    assert not case.exists() and sibling.read_text() == "safe"

    marker = json.loads(layout.marker_path.read_text())
    marker["identity_sha256"] = "b" * 64
    layout.marker_path.write_text(json.dumps(marker))
    with pytest.raises(ValueError, match="owner marker"):
        WorkLayout.create(tmp_path, "ns", "a" * 64)

