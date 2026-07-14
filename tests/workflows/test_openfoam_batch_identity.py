from __future__ import annotations

from pathlib import Path

from digitalmodel.workflows.openfoam_batch_identity import build_run_identity


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

