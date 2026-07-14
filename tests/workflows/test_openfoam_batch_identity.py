"""Legacy configuration, routing, and public-surface characterization."""

import base64
import csv
import hashlib
import subprocess
from pathlib import Path

import pytest

from digitalmodel.workflows import openfoam_run_batch as ofb
from digitalmodel.workflows.openfoam_batch_config import (
    build_run_identity,
    canonical_json_bytes,
    resolve_execution_authority,
)


def _git_repo(tmp_path: Path) -> tuple[Path, Path, dict[str, Path]]:
    repo = tmp_path / "repo"
    package = repo / "src" / "demo_pkg"
    package.mkdir(parents=True)
    files = {
        "request": repo / "request.yml",
        "matrix": repo / "matrix.csv",
        "case": repo / "case.yml",
    }
    (package / "__init__.py").write_text("VALUE = 1\n")
    for role, path in files.items():
        path.write_text(f"{role}: one\n")
    subprocess.run(["git", "init", "-q", repo], check=True)
    subprocess.run(["git", "-C", repo, "add", "."], check=True)
    subprocess.run(
        [
            "git",
            "-C",
            repo,
            "-c",
            "user.name=Test",
            "-c",
            "user.email=test@example.invalid",
            "commit",
            "-qm",
            "fixture",
        ],
        check=True,
    )
    return repo, package, files


def _identity(tmp_path: Path, **overrides) -> dict:
    repo, package, files = _git_repo(tmp_path)
    tool = repo / "bin" / "solver"
    tool.parent.mkdir()
    tool.write_bytes(b"solver-v1\n")
    values = {
        "config_path": files["request"],
        "package_root": package,
        "package_name": "demo-pkg",
        "package_version": "1.0",
        "effective_config": {"mode": "pool"},
        "referenced_inputs": {"matrix": files["matrix"], "case": files["case"]},
        "selected_executables": {"solver": tool},
        "visible_rank_count": 8,
        "dispatcher_rank_limit": 4,
        "result_policy_version": "result-policy-v1",
        "work_layout_version": "work-layout-v1",
    }
    values.update(overrides)
    return build_run_identity(**values)


def test_canonical_json_is_strict_sorted_ascii_and_lf_terminated():
    assert canonical_json_bytes({"z": "µ", "a": 1}) == b'{"a":1,"z":"\\u00b5"}\n'
    with pytest.raises(ValueError):
        canonical_json_bytes({"bad": float("nan")})


@pytest.mark.parametrize("root_value", [None, "relative", "missing"])
def test_hosted_root_is_operator_authority_and_rejects_invalid_before_side_effects(
    tmp_path, root_value
):
    cfg_dir = tmp_path / "cfg"
    cfg_dir.mkdir()
    root = tmp_path / "operator-root"
    root.mkdir()
    env = {"DIGITALMODEL_EXECUTION_CONTEXT": "hosted-deckhand"}
    if root_value is not None:
        env["DIGITALMODEL_WORK_ROOT"] = str(
            root if root_value == "valid" else root_value
        )
    with pytest.raises(ValueError, match="DIGITALMODEL_WORK_ROOT"):
        resolve_execution_authority({}, cfg_dir, env)
    assert list(cfg_dir.iterdir()) == []


def test_hosted_environment_root_wins_and_yaml_cannot_choose_absolute_root(tmp_path):
    cfg_dir = tmp_path / "cfg"
    root = tmp_path / "operator-root"
    cfg_dir.mkdir()
    root.mkdir()
    env = {
        "DIGITALMODEL_EXECUTION_CONTEXT": "hosted-deckhand",
        "DIGITALMODEL_WORK_ROOT": str(root),
    }
    authority = resolve_execution_authority(
        {"work_root_namespace": "lane/green"}, cfg_dir, env
    )
    assert authority.context == "hosted-deckhand"
    assert authority.root == root
    assert authority.namespace == Path("lane/green")
    with pytest.raises(ValueError, match="hosted YAML"):
        resolve_execution_authority(
            {"work_root": str(tmp_path / "other")}, cfg_dir, env
        )


def test_router_rejects_missing_hosted_root_before_any_directory(tmp_path, monkeypatch):
    monkeypatch.setenv("DIGITALMODEL_EXECUTION_CONTEXT", "hosted-deckhand")
    monkeypatch.delenv("DIGITALMODEL_WORK_ROOT", raising=False)
    cfg = {
        "_config_dir_path": str(tmp_path),
        "openfoam_run_batch": {
            "base": {"case_type": "current_loading"},
            "run_batch": {"mock": True},
        },
    }
    with pytest.raises(ValueError, match="DIGITALMODEL_WORK_ROOT"):
        ofb.router(cfg)
    assert list(tmp_path.iterdir()) == []


@pytest.mark.parametrize(
    "namespace", ["/abs", "../up", "a/./b", "a//b", "a\\b", "a\x00b", "a\nb"]
)
def test_namespace_rejects_nonportable_components(tmp_path, namespace):
    root = tmp_path / "root"
    root.mkdir()
    env = {
        "DIGITALMODEL_EXECUTION_CONTEXT": "hosted-deckhand",
        "DIGITALMODEL_WORK_ROOT": str(root),
    }
    with pytest.raises(ValueError, match="namespace"):
        resolve_execution_authority({"work_root_namespace": namespace}, tmp_path, env)


def test_trusted_local_requires_opt_in_and_rejects_git_or_symlink_roots(tmp_path):
    repo, _, _ = _git_repo(tmp_path)
    external = tmp_path / "external"
    external.mkdir()
    with pytest.raises(ValueError, match="explicit"):
        resolve_execution_authority({"work_root": str(external)}, tmp_path, {})
    with pytest.raises(ValueError, match="Git"):
        resolve_execution_authority(
            {"execution_context": "trusted-local", "work_root": str(repo)},
            tmp_path,
            {},
        )
    link = tmp_path / "link"
    link.symlink_to(external, target_is_directory=True)
    with pytest.raises(ValueError, match="symlink"):
        resolve_execution_authority(
            {"execution_context": "trusted-local", "work_root": str(link)},
            tmp_path,
            {},
        )
    authority = resolve_execution_authority(
        {"execution_context": "trusted-local", "work_root": str(external)},
        tmp_path,
        {},
    )
    assert authority.root == external


def test_source_identity_binds_exact_tracked_bytes_and_rejects_dirty_candidates(
    tmp_path,
):
    identity = _identity(tmp_path)
    assert identity["schema_version"] == 1
    assert identity["identity_kind"] == "openfoam-run-v1"
    assert len(identity["identity_sha256"]) == 64
    assert all(
        not Path(item["safe_relative_path"]).is_absolute()
        for item in identity["referenced_inputs"]
    )
    repo = next(tmp_path.glob("repo"))
    (repo / "src" / "demo_pkg" / "__init__.py").write_text("VALUE = 2\n")
    with pytest.raises(ValueError, match="clean"):
        build_run_identity(
            config_path=repo / "request.yml",
            package_root=repo / "src" / "demo_pkg",
            package_name="demo-pkg",
            package_version="1.0",
            effective_config={"mode": "pool"},
            referenced_inputs={"matrix": repo / "matrix.csv"},
            selected_executables={},
            visible_rank_count=8,
            dispatcher_rank_limit=4,
            result_policy_version="result-policy-v1",
            work_layout_version="work-layout-v1",
        )
    subprocess.run(["git", "-C", repo, "add", "."], check=True)
    subprocess.run(
        [
            "git",
            "-C",
            repo,
            "-c",
            "user.name=T",
            "-c",
            "user.email=t@x.invalid",
            "commit",
            "-qm",
            "package",
        ],
        check=True,
    )
    changed = build_run_identity(
        config_path=repo / "request.yml",
        package_root=repo / "src/demo_pkg",
        package_name="demo-pkg",
        package_version="1.0",
        effective_config={"mode": "pool"},
        referenced_inputs={"matrix": repo / "matrix.csv"},
        selected_executables={},
        visible_rank_count=8,
        dispatcher_rank_limit=4,
        result_policy_version="result-policy-v1",
        work_layout_version="work-layout-v1",
    )
    assert changed["identity_sha256"] != identity["identity_sha256"]


def test_identity_changes_for_config_input_tool_host_policy_and_layout_mutations(
    tmp_path,
):
    baseline = _identity(tmp_path / "base")
    for name, overrides in {
        "host": {"visible_rank_count": 7},
        "ceiling": {"dispatcher_rank_limit": 3},
        "policy": {"result_policy_version": "result-policy-v2"},
        "layout": {"work_layout_version": "work-layout-v2"},
        "config": {"effective_config": {"mode": "mpi"}},
    }.items():
        assert (
            _identity(tmp_path / name, **overrides)["identity_sha256"]
            != baseline["identity_sha256"]
        )
    for role in ("request", "matrix", "case"):
        scope = tmp_path / role
        repo, package, files = _git_repo(scope)
        tool = repo / "solver"
        tool.write_bytes(b"tool")
        files[role].write_text(f"{role}: changed\n")
        subprocess.run(["git", "-C", repo, "add", files[role]], check=True)
        subprocess.run(
            [
                "git",
                "-C",
                repo,
                "-c",
                "user.name=T",
                "-c",
                "user.email=t@x.invalid",
                "commit",
                "-qm",
                "change",
            ],
            check=True,
        )
        changed = build_run_identity(
            config_path=files["request"],
            package_root=package,
            package_name="demo-pkg",
            package_version="1.0",
            effective_config={"mode": "pool"},
            referenced_inputs={"matrix": files["matrix"], "case": files["case"]},
            selected_executables={"solver": tool},
            visible_rank_count=8,
            dispatcher_rank_limit=4,
            result_policy_version="result-policy-v1",
            work_layout_version="work-layout-v1",
        )
        assert changed["identity_sha256"] != baseline["identity_sha256"]


def test_tool_bytes_change_identity_and_host_ceilings_are_validated(tmp_path):
    repo, package, files = _git_repo(tmp_path)
    tool = repo / "solver"
    tool.write_bytes(b"v1")
    values = dict(
        config_path=files["request"],
        package_root=package,
        package_name="demo-pkg",
        package_version="1.0",
        effective_config={"mode": "pool"},
        referenced_inputs={"case": files["case"]},
        selected_executables={"solver": tool},
        visible_rank_count=8,
        dispatcher_rank_limit=4,
        result_policy_version="result-policy-v1",
        work_layout_version="work-layout-v1",
    )
    baseline = build_run_identity(**values)
    tool.write_bytes(b"v2")
    assert (
        build_run_identity(**values)["identity_sha256"] != baseline["identity_sha256"]
    )
    for visible, limit in ((0, 1), (8, 0), (4, 5)):
        with pytest.raises(ValueError, match="rank"):
            build_run_identity(
                **{
                    **values,
                    "visible_rank_count": visible,
                    "dispatcher_rank_limit": limit,
                }
            )


def _record_digest(data: bytes) -> str:
    value = base64.urlsafe_b64encode(hashlib.sha256(data).digest()).rstrip(b"=")
    return "sha256=" + value.decode()


def test_wheel_record_verifies_actual_bytes_size_missing_and_unrecorded(tmp_path):
    site = tmp_path / "site"
    package = site / "demo_pkg"
    dist = site / "demo_pkg-1.0.dist-info"
    package.mkdir(parents=True)
    dist.mkdir()
    module = package / "__init__.py"
    module.write_bytes(b"VALUE = 1\n")
    record = dist / "RECORD"
    with record.open("w", newline="") as stream:
        csv.writer(stream).writerows(
            [
                [
                    "demo_pkg/__init__.py",
                    _record_digest(module.read_bytes()),
                    str(module.stat().st_size),
                ],
                ["demo_pkg-1.0.dist-info/RECORD", "", ""],
            ]
        )
    common = dict(
        config_path=None,
        package_root=package,
        package_name="demo-pkg",
        package_version="1.0",
        effective_config={"mode": "pool"},
        referenced_inputs={},
        selected_executables={},
        visible_rank_count=8,
        dispatcher_rank_limit=4,
        result_policy_version="result-policy-v1",
        work_layout_version="work-layout-v1",
        distribution_root=site,
    )
    assert build_run_identity(**common)["source"]["tracked_tree_clean"] is None
    module.write_bytes(b"VALUE = 2\n")
    with pytest.raises(ValueError, match="RECORD"):
        build_run_identity(**common)
    module.unlink()
    with pytest.raises(ValueError, match="missing"):
        build_run_identity(**common)
    (package / "extra.py").write_text("extra = 1\n")
    with pytest.raises(ValueError, match="unrecorded"):
        build_run_identity(**common)


def test_identity_emits_only_basename_for_executable_and_no_absolute_paths(tmp_path):
    identity = _identity(tmp_path)
    assert identity["selected_executables"][0]["basename"] == "solver"
    serialized = canonical_json_bytes(identity).decode()
    assert str(tmp_path) not in serialized
    assert identity["host_capabilities"] == {
        "visible_rank_count": 8,
        "dispatcher_rank_limit": 4,
    }
