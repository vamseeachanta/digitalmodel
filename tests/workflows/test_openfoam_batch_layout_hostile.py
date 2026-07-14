"""Hostile owned-layout tests for external OpenFOAM work roots."""

import json
import os
from pathlib import Path

import pytest

from digitalmodel.workflows import openfoam_batch_layout as layout_module
from digitalmodel.workflows.openfoam_batch_config import ExecutionAuthority


def _identity(value: str = "a" * 64) -> dict:
    return {
        "schema_version": 1,
        "identity_kind": "openfoam-run-v1",
        "identity_sha256": value,
    }


def _authority(tmp_path: Path) -> ExecutionAuthority:
    root = tmp_path / "operator"
    root.mkdir()
    return ExecutionAuthority("trusted-local", root, Path("team/lane"))


def _create(tmp_path: Path):
    authority = _authority(tmp_path)
    return layout_module.WorkLayout.create(authority, _identity(), "cases")


def _run_path(authority: ExecutionAuthority, identity: dict) -> Path:
    return (
        authority.root
        / authority.namespace
        / f"openfoam-run-{identity['identity_sha256']}"
    )


def _snapshot(path: Path) -> list[tuple[str, bytes | None]]:
    if not path.exists():
        return []
    result = []
    for item in sorted(path.rglob("*")):
        result.append((str(item.relative_to(path)), item.read_bytes() if item.is_file() else None))
    return result


def test_new_layout_is_owned_and_binds_root_identity_and_token(tmp_path):
    authority = _authority(tmp_path)
    with layout_module.WorkLayout.create(authority, _identity(), "cases") as layout:
        marker = json.loads((layout.run_path / layout_module.OWNER_FILENAME).read_text())
        root_stat = authority.root.stat()
        assert layout.run_path == _run_path(authority, _identity())
        assert (layout.run_path / ".locks").is_dir()
        assert layout.work_path == layout.run_path / "cases"
        assert marker["schema_version"] == 1
        assert marker["uid"] == os.getuid()
        assert marker["identity"] == _identity()
        assert marker["operator_root_device"] == root_stat.st_dev
        assert marker["operator_root_inode"] == root_stat.st_ino
        assert len(marker["owner_token"]) >= 32


@pytest.mark.parametrize(
    "marker",
    [None, b"{", json.dumps({"schema_version": 1}).encode()],
)
def test_preexisting_unowned_or_corrupt_run_rejects_without_mutation(tmp_path, marker):
    authority = _authority(tmp_path)
    run = _run_path(authority, _identity())
    run.mkdir(parents=True)
    (run / "sentinel").write_bytes(b"keep")
    if marker is not None:
        (run / layout_module.OWNER_FILENAME).write_bytes(marker)
    before = _snapshot(run)
    with pytest.raises(RuntimeError, match="owned run"):
        layout_module.WorkLayout.create(authority, _identity(), "cases")
    assert _snapshot(run) == before


@pytest.mark.parametrize("field,value", [("uid", -1), ("owner_token", "foreign")])
def test_preexisting_foreign_marker_rejects_without_mutation(tmp_path, field, value):
    authority = _authority(tmp_path)
    first = layout_module.WorkLayout.create(authority, _identity(), "cases")
    first.close()
    marker_path = first.run_path / layout_module.OWNER_FILENAME
    marker = json.loads(marker_path.read_text())
    marker[field] = value
    marker_path.write_text(json.dumps(marker))
    before = _snapshot(first.run_path)
    with pytest.raises(RuntimeError, match="owned run"):
        layout_module.WorkLayout.create(authority, _identity(), "cases")
    assert _snapshot(first.run_path) == before


def test_identity_collision_rejects_even_with_valid_foreign_marker(tmp_path):
    authority = _authority(tmp_path)
    first = layout_module.WorkLayout.create(authority, _identity(), "cases")
    first.close()
    marker_path = first.run_path / layout_module.OWNER_FILENAME
    marker = json.loads(marker_path.read_text())
    marker["identity"]["identity_sha256"] = "b" * 64
    marker_path.write_text(json.dumps(marker))
    with pytest.raises(RuntimeError, match="owned run"):
        layout_module.WorkLayout.create(authority, _identity(), "cases")


@pytest.mark.parametrize("collision_kind", ["file", "symlink"])
def test_namespace_collision_rejects_before_run_creation(tmp_path, collision_kind):
    authority = _authority(tmp_path)
    team = authority.root / "team"
    if collision_kind == "file":
        team.write_text("collision")
    else:
        target = tmp_path / "target"
        target.mkdir()
        team.symlink_to(target, target_is_directory=True)
    with pytest.raises(RuntimeError, match="namespace"):
        layout_module.WorkLayout.create(authority, _identity(), "cases")
    assert not _run_path(authority, _identity()).exists()


@pytest.mark.parametrize("case", ["", ".", "..", "a/b", "/abs", "a\\b", ".locks"])
def test_case_names_are_strict_descendants_and_exclude_roots(tmp_path, case):
    with _create(tmp_path) as layout:
        with pytest.raises(ValueError, match="case"):
            layout.case_path(case)


@pytest.mark.parametrize("operation", ["clean", "prune"])
@pytest.mark.parametrize("replacement", ["directory", "symlink"])
def test_substitution_at_destructive_seams_rejects_and_preserves_replacement(
    tmp_path, operation, replacement
):
    with _create(tmp_path) as layout:
        case = layout.case_path("case-a")
        (case / "processor0").mkdir(parents=True)
        (case / "processor0" / "old").write_text("old")
        target = case if operation == "clean" else case / "processor0"
        saved = target.with_name(target.name + ".saved")

        def substitute(_parent_fd, _name):
            target.rename(saved)
            if replacement == "directory":
                target.mkdir()
                (target / "replacement").write_text("keep")
            else:
                destination = tmp_path / f"outside-{operation}"
                destination.mkdir()
                (destination / "replacement").write_text("keep")
                target.symlink_to(destination, target_is_directory=True)

        with pytest.raises(RuntimeError, match="substituted"):
            if operation == "clean":
                layout.clean_case("case-a", mutation_hook=substitute)
            else:
                layout.prune_processors("case-a", mutation_hook=substitute)
        assert target.exists()
        assert (target / "replacement").read_text() == "keep"
        assert (saved / "old").read_text() == "old" if operation == "prune" else saved.exists()


def test_run_rename_or_marker_inode_substitution_rejects_before_mutation(tmp_path):
    layout = _create(tmp_path)
    case = layout.case_path("case-a")
    case.mkdir()
    original = layout.run_path.with_name(layout.run_path.name + ".original")
    layout.run_path.rename(original)
    layout.run_path.mkdir()
    (layout.run_path / "replacement").write_text("keep")
    with pytest.raises(RuntimeError, match="owned run"):
        layout.clean_case("case-a")
    assert (layout.run_path / "replacement").read_text() == "keep"
    layout.close()

