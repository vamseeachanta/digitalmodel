"""Hostile authority and RunIdentity regression tests for issue #1565."""

import base64
import csv
import hashlib
import subprocess
from copy import deepcopy
from pathlib import Path

import pytest

from digitalmodel.workflows import openfoam_run_batch as facade
from digitalmodel.workflows.openfoam_batch_config import build_run_identity


def _git_fixture(tmp_path: Path) -> tuple[Path, Path, dict[str, Path]]:
    repo = tmp_path / "repo"
    package = repo / "src/demo_pkg"
    package.mkdir(parents=True)
    files = {"request": repo / "request.yml", "case": repo / "case.yml"}
    (package / "__init__.py").write_text("VALUE = 1\n")
    for role, path in files.items():
        path.write_text(f"{role}: one\n")
    subprocess.run(["git", "init", "-q", repo], check=True)
    subprocess.run(["git", "-C", repo, "add", "."], check=True)
    subprocess.run(
        ["git", "-C", repo, "-c", "user.name=T", "-c", "user.email=t@x.invalid",
         "commit", "-qm", "fixture"],
        check=True,
    )
    return repo, package, files


def _source_args(tmp_path: Path) -> dict:
    repo, package, files = _git_fixture(tmp_path)
    tool = repo / "solver"
    tool.write_bytes(b"solver\n")
    return {
        "config_path": files["request"], "package_root": package,
        "package_name": "demo-pkg", "package_version": "1.0",
        "effective_config": {"mode": "pool"},
        "referenced_inputs": {"case": files["case"]},
        "selected_executables": {"solver": tool}, "visible_rank_count": 8,
        "dispatcher_rank_limit": 4, "result_policy_version": "result-policy-v1",
        "work_layout_version": "work-layout-v1",
    }


@pytest.mark.parametrize("context", ["hosted-deckhand", "trusted-local"])
def test_external_execution_fails_closed_before_any_mutation(tmp_path, monkeypatch, context):
    cfg_dir = tmp_path / "cfg"
    root = tmp_path / "operator-root"
    cfg_dir.mkdir()
    root.mkdir()
    run_batch = {"mock": True}
    if context == "hosted-deckhand":
        monkeypatch.setenv("DIGITALMODEL_EXECUTION_CONTEXT", context)
        monkeypatch.setenv("DIGITALMODEL_WORK_ROOT", str(root))
    else:
        monkeypatch.delenv("DIGITALMODEL_EXECUTION_CONTEXT", raising=False)
        run_batch.update(execution_context=context, work_root=str(root))
    cfg = {
        "_config_dir_path": str(cfg_dir),
        "openfoam_run_batch": {
            "base": {"case_type": "current_loading"}, "run_batch": run_batch,
        },
    }
    original = deepcopy(cfg)
    with pytest.raises(RuntimeError, match="owned external layout"):
        facade.router(cfg)
    assert cfg == original
    assert list(cfg_dir.iterdir()) == []
    assert list(root.iterdir()) == []


def test_source_identity_rejects_clean_commit_during_byte_reads(tmp_path, monkeypatch):
    args = _source_args(tmp_path)
    package_file = args["package_root"] / "__init__.py"
    original_read = Path.read_bytes
    raced = False

    def commit_after_read(path):
        nonlocal raced
        data = original_read(path)
        if path == package_file and not raced:
            raced = True
            path.write_text("VALUE = 2\n")
            repo = args["package_root"].parents[1]
            subprocess.run(["git", "-C", repo, "add", "."], check=True)
            subprocess.run(
                ["git", "-C", repo, "-c", "user.name=T", "-c",
                 "user.email=t@x.invalid", "commit", "-qm", "race"], check=True,
            )
        return data

    monkeypatch.setattr(Path, "read_bytes", commit_after_read)
    with pytest.raises(ValueError, match="HEAD changed"):
        build_run_identity(**args)


def test_top_level_request_role_cannot_be_shadowed(tmp_path):
    args = _source_args(tmp_path)
    args["referenced_inputs"]["request"] = args["referenced_inputs"]["case"]
    with pytest.raises(ValueError, match="reserved.*request"):
        build_run_identity(**args)


def _record_digest(data: bytes) -> str:
    value = base64.urlsafe_b64encode(hashlib.sha256(data).digest()).rstrip(b"=")
    return "sha256=" + value.decode()


def _wheel_args(tmp_path: Path) -> tuple[dict, Path]:
    site = tmp_path / "site"
    package = site / "demo_pkg"
    dist = site / "demo_pkg-1.0.dist-info"
    package.mkdir(parents=True)
    dist.mkdir()
    module = package / "__init__.py"
    module.write_bytes(b"VALUE = 1\n")
    record = dist / "RECORD"
    with record.open("w", newline="") as stream:
        csv.writer(stream).writerows([
            ["demo_pkg/__init__.py", _record_digest(module.read_bytes()),
             str(module.stat().st_size)],
            ["demo_pkg-1.0.dist-info/RECORD", "", ""],
        ])
    args = {
        "config_path": None, "package_root": package, "package_name": "demo-pkg",
        "package_version": "1.0", "effective_config": {"mode": "pool"},
        "referenced_inputs": {}, "selected_executables": {},
        "visible_rank_count": 8, "dispatcher_rank_limit": 4,
        "result_policy_version": "result-policy-v1",
        "work_layout_version": "work-layout-v1", "distribution_root": site,
    }
    return args, record


def test_wheel_distribution_matches_declared_name_version_and_record_bytes(tmp_path):
    args, record = _wheel_args(tmp_path)
    baseline = build_run_identity(**args)
    for field, bad in (("package_name", "other"), ("package_version", "9.9")):
        with pytest.raises(ValueError, match="distribution"):
            build_run_identity(**{**args, field: bad})
    record.write_bytes(record.read_bytes().replace(b"\r\n", b"\n"))
    assert build_run_identity(**args)["identity_sha256"] != baseline["identity_sha256"]


@pytest.mark.parametrize(
    "namespace",
    ["C:/abs", "a:b", "con", "CON.txt", "snowman-☃", "a" * 64, "a/" + "b" * 63],
)
def test_namespace_uses_closed_portable_ascii_grammar(tmp_path, monkeypatch, namespace):
    root = tmp_path / "root"
    root.mkdir()
    monkeypatch.setenv("DIGITALMODEL_EXECUTION_CONTEXT", "hosted-deckhand")
    monkeypatch.setenv("DIGITALMODEL_WORK_ROOT", str(root))
    cfg = {"work_root_namespace": namespace}
    with pytest.raises(ValueError, match="namespace"):
        facade._resolve_execution_authority_impl(cfg, tmp_path)


@pytest.mark.parametrize(
    ("field", "value"),
    [("package_name", "/private/name"), ("package_version", "v/1"),
     ("result_policy_version", "policy:1"), ("work_layout_version", "layout/☃")],
)
def test_identity_metadata_labels_and_versions_are_portable(tmp_path, field, value):
    args = _source_args(tmp_path)
    args[field] = value
    with pytest.raises(ValueError, match="portable"):
        build_run_identity(**args)


@pytest.mark.parametrize("collection", ["referenced_inputs", "selected_executables"])
def test_identity_roles_are_portable_and_path_free(tmp_path, collection):
    args = _source_args(tmp_path)
    value = next(iter(args[collection].values()))
    args[collection] = {"/private/role": value}
    with pytest.raises(ValueError, match="portable"):
        build_run_identity(**args)


@pytest.mark.parametrize("visible,limit", [(True, 1), (8.9, 4), (8, False), (8, 4.1)])
def test_rank_ceilings_require_exact_positive_non_bool_integers(tmp_path, visible, limit):
    args = _source_args(tmp_path)
    args.update(visible_rank_count=visible, dispatcher_rank_limit=limit)
    with pytest.raises(ValueError, match="exact positive integers"):
        build_run_identity(**args)
