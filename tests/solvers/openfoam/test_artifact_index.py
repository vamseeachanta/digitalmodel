from __future__ import annotations

import json
import os
from hashlib import sha256
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.artifact_index import (
    TREE_DOMAIN,
    ArtifactIndexError,
    FileRecord,
    artifact_id,
    build_index,
    frame,
    host_local_locator,
    is_numeric_time_name,
    select_roots,
    snapshot_tree,
    tree_digest,
    verify_unchanged,
)


def _write_case_tree(case_root: Path) -> None:
    files = {
        "constant/polyMesh/points": b"mesh",
        "0/U": b"u0",
        "0.5/U": b"u1",
        "VTK/case_0.vtk": b"vtk",
        "postProcessing/probes/0/p": b"probe",
        "system/controlDict": b"control",
        "processor0/0/U": b"partition",
        "log.interFoam": b"log",
    }
    for relative_path, content in files.items():
        path = case_root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(content)


def _build_test_index(case_root: Path) -> dict:
    return build_index(
        case_root,
        run_identity_sha256="1" * 64,
        input_sha256="2" * 64,
        source_sha256="3" * 64,
        tool_sha256="4" * 64,
        generation_id="5" * 64,
    )


def test_frame_prepends_uint64_big_endian_length() -> None:
    assert frame(b"x") == b"\x00\x00\x00\x00\x00\x00\x00\x01x"


def test_tree_domain_frame_golden_vector() -> None:
    assert sha256(frame(TREE_DOMAIN)).hexdigest() == (
        "a4474ba5694b4356dc78a265d304584b2de64afcf6136c6cd968025dbd1d6708"
    )


def test_single_record_tree_digest_golden_vector() -> None:
    record = FileRecord(
        path="0/U",
        size_bytes=1,
        sha256=sha256(b"x").hexdigest(),
    )
    assert tree_digest([record]) == (
        "15749b5cd2b684b587d2d7da29accbe736aeb6736d6afa2a0500fe5fd01e6c73"
    )


def test_tree_digest_is_independent_of_input_order() -> None:
    sorted_records = [
        FileRecord("0/U", 1, sha256(b"x").hexdigest()),
        FileRecord("1/U", 1, sha256(b"y").hexdigest()),
    ]
    sorted_order_digest = tree_digest(sorted_records)

    assert tree_digest(list(reversed(sorted_records))) == sorted_order_digest


def test_tree_digest_changes_when_only_size_changes() -> None:
    digest = sha256(b"x").hexdigest()
    original = tree_digest([FileRecord("0/U", 1, digest)])
    changed = tree_digest([FileRecord("0/U", 2, digest)])

    def encode(data: bytes) -> bytes:
        return len(data).to_bytes(8, "big") + data

    expected_stream = b"".join(
        (
            encode(b"dm-artifact-tree-v1"),
            encode(b"file"),
            encode(b"0/U"),
            encode((2).to_bytes(8, "big")),
            encode(bytes.fromhex(digest)),
        )
    )
    assert changed != original
    assert changed == sha256(expected_stream).hexdigest()


def test_artifact_id_is_stable_and_kind_sensitive() -> None:
    inputs = {
        "run_identity_sha256": "1" * 64,
        "generation_id": "2" * 64,
        "kind": "field_tree",
        "selection": "0",
        "size_bytes": 1,
        "file_count": 1,
        "content_sha256": sha256(b"x").hexdigest(),
    }
    first = artifact_id(**inputs)
    second = artifact_id(**inputs)
    mesh = artifact_id(**(inputs | {"kind": "mesh_tree"}))

    assert second == first
    assert mesh != first


def test_host_local_locator_has_exact_opaque_shape() -> None:
    assert host_local_locator("run-id", "generation-id", "artifact-id") == (
        "host-local:///run-id/generation-id/artifact-id"
    )


def test_numeric_time_name_accepts_canonical_nonnegative_values() -> None:
    for name in ("0", "0.5", "1e-3", "10"):
        assert is_numeric_time_name(name) is True


def test_numeric_time_name_rejects_invalid_values() -> None:
    for name in (
        "",
        " 0",
        "+1",
        "-1",
        "inf",
        "nan",
        "..",
        "0/1",
        "constant",
        "system",
    ):
        assert is_numeric_time_name(name) is False


def test_select_roots_returns_disjoint_completed_selections(tmp_path: Path) -> None:
    _write_case_tree(tmp_path)
    selected = select_roots(tmp_path)
    selected_paths = [path for paths in selected.values() for path in paths]
    excluded = {
        path
        for path in selected_paths
        if path.startswith(("processor", "system")) or path == "log.interFoam"
    }

    assert set(selected) == {
        "mesh_tree",
        "field_tree",
        "vtk_tree",
        "postprocessing_tree",
    }
    assert len(selected_paths) == len(set(selected_paths))
    assert excluded == set()


def test_field_tree_contains_only_numeric_time_roots(tmp_path: Path) -> None:
    _write_case_tree(tmp_path)

    assert sorted(select_roots(tmp_path)["field_tree"]) == ["0", "0.5"]


def test_select_roots_rejects_duplicate_numeric_time_spellings(
    tmp_path: Path,
) -> None:
    decimal_case = tmp_path / "decimal"
    (decimal_case / "0.1").mkdir(parents=True)
    (decimal_case / "0.10").mkdir()
    zero_case = tmp_path / "zero"
    (zero_case / "0").mkdir(parents=True)
    (zero_case / "0.0").mkdir()

    with pytest.raises(ArtifactIndexError):
        select_roots(decimal_case)
    with pytest.raises(ArtifactIndexError):
        select_roots(zero_case)


def test_snapshot_tree_rejects_symlink(tmp_path: Path) -> None:
    target = tmp_path / "target"
    target.write_bytes(b"x")
    (tmp_path / "link").symlink_to(target)

    with pytest.raises(ArtifactIndexError):
        snapshot_tree(tmp_path)


@pytest.mark.skipif(not hasattr(os, "mkfifo"), reason="os.mkfifo unavailable")
def test_snapshot_tree_rejects_fifo(tmp_path: Path) -> None:
    os.mkfifo(tmp_path / "pipe")

    with pytest.raises(ArtifactIndexError):
        snapshot_tree(tmp_path)


def test_snapshot_tree_returns_exact_sorted_records(tmp_path: Path) -> None:
    nested = tmp_path / "a"
    nested.mkdir()
    (nested / "x").write_bytes(b"A")
    (tmp_path / "b").write_bytes(b"BC")

    assert snapshot_tree(tmp_path) == [
        FileRecord("a/x", 1, sha256(b"A").hexdigest()),
        FileRecord("b", 2, sha256(b"BC").hexdigest()),
    ]


def test_verify_unchanged_detects_same_size_modification(tmp_path: Path) -> None:
    path = tmp_path / "field"
    path.write_bytes(b"aa")
    expected = snapshot_tree(tmp_path)
    path.write_bytes(b"bb")

    with pytest.raises(ArtifactIndexError):
        verify_unchanged(tmp_path, expected)


def test_verify_unchanged_detects_added_file(tmp_path: Path) -> None:
    (tmp_path / "field").write_bytes(b"x")
    expected = snapshot_tree(tmp_path)
    (tmp_path / "added").write_bytes(b"y")

    with pytest.raises(ArtifactIndexError):
        verify_unchanged(tmp_path, expected)


def test_verify_unchanged_detects_removed_file(tmp_path: Path) -> None:
    path = tmp_path / "field"
    path.write_bytes(b"x")
    expected = snapshot_tree(tmp_path)
    path.unlink()

    with pytest.raises(ArtifactIndexError):
        verify_unchanged(tmp_path, expected)


def test_verify_unchanged_detects_rename(tmp_path: Path) -> None:
    path = tmp_path / "field"
    path.write_bytes(b"x")
    expected = snapshot_tree(tmp_path)
    path.rename(tmp_path / "renamed")

    with pytest.raises(ArtifactIndexError):
        verify_unchanged(tmp_path, expected)


def test_build_index_emits_no_absolute_host_path(tmp_path: Path) -> None:
    _write_case_tree(tmp_path)
    emitted_index = [record.as_dict() for record in _build_test_index(tmp_path).values()]

    assert str(tmp_path) not in json.dumps(emitted_index, sort_keys=True)


def test_completed_index_excludes_processor_and_diagnostic_roots(
    tmp_path: Path,
) -> None:
    _write_case_tree(tmp_path)
    records = _build_test_index(tmp_path)

    assert {record.kind for record in records.values()} == {
        "mesh_tree",
        "field_tree",
        "vtk_tree",
        "postprocessing_tree",
    }
