from __future__ import annotations

from pathlib import Path
import subprocess

import pytest

from digitalmodel.workflows.openfoam_batch_identity import (
    build_run_identity,
    verify_wheel_record,
)


def test_identity_is_stable_and_changes_with_input_tool_and_capacity(tmp_path: Path) -> None:
    source = tmp_path / "input.yml"
    tool = tmp_path / "solver"
    source.write_text("a: 1\n")
    tool.write_bytes(b"solver-v1")
    kwargs = dict(
        effective_config={"run_batch": {"mode": "mpi"}},
        referenced_inputs=[("request", source)],
        selected_executables=[("solver", tool)],
        visible_rank_count=8,
        dispatcher_rank_limit=7,
        package_root=Path(__file__).resolve().parents[2] / "src" / "digitalmodel",
    )
    first = build_run_identity(**kwargs)
    assert first == build_run_identity(**kwargs)
    assert len(first.identity_sha256) == 64
    assert first.selected_executables[0]["basename"] == "solver"
    assert str(tmp_path) not in first.canonical_json()

    source.write_text("a: 2\n")
    changed_input = build_run_identity(**kwargs)
    assert changed_input.identity_sha256 != first.identity_sha256
    source.write_text("a: 1\n")
    tool.write_bytes(b"solver-v2")
    changed_tool = build_run_identity(**kwargs)
    assert changed_tool.identity_sha256 != first.identity_sha256
    kwargs["visible_rank_count"] = 9
    assert build_run_identity(**kwargs).identity_sha256 != changed_tool.identity_sha256


def test_identity_rejects_missing_or_symlinked_input(tmp_path: Path) -> None:
    missing = tmp_path / "missing.yml"
    try:
        build_run_identity(
            effective_config={}, referenced_inputs=[("request", missing)],
            selected_executables=[], visible_rank_count=1,
            dispatcher_rank_limit=1, package_root=tmp_path,
        )
    except ValueError as exc:
        assert "referenced input" in str(exc)
    else:
        raise AssertionError("missing input was accepted")


def test_dirty_git_package_source_is_rejected(tmp_path: Path) -> None:
    package = tmp_path / "pkg"
    package.mkdir()
    source = package / "module.py"
    source.write_text("VALUE = 1\n")
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    subprocess.run(["git", "-C", str(tmp_path), "add", "pkg/module.py"], check=True)
    subprocess.run(["git", "-C", str(tmp_path), "-c", "user.name=Test",
                    "-c", "user.email=test@example.invalid", "commit", "-qm", "init"], check=True)
    source.write_text("VALUE = 2\n")
    with pytest.raises(ValueError, match="dirty"):
        build_run_identity(effective_config={}, referenced_inputs=[], selected_executables=[],
                           visible_rank_count=1, dispatcher_rank_limit=1,
                           package_root=package)


def test_wheel_record_verifies_actual_installed_bytes(tmp_path: Path) -> None:
    import base64
    import hashlib

    package = tmp_path / "digitalmodel"
    dist_info = tmp_path / "digitalmodel-1.0.dist-info"
    package.mkdir()
    dist_info.mkdir()
    module = package / "module.py"
    module.write_bytes(b"VALUE = 1\n")
    encoded = base64.urlsafe_b64encode(hashlib.sha256(module.read_bytes()).digest()).rstrip(b"=").decode()
    record = dist_info / "RECORD"
    record.write_text(f"digitalmodel/module.py,sha256={encoded},{module.stat().st_size}\n"
                      "digitalmodel-1.0.dist-info/RECORD,,\n")
    verified = verify_wheel_record(package, record)
    assert verified["content_sha256"]
    module.write_bytes(b"VALUE = 2\n")
    with pytest.raises(ValueError, match="RECORD"):
        verify_wheel_record(package, record)
