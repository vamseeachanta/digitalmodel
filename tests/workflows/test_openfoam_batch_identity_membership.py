"""Package content and membership race regressions for issue #1565."""

import base64
import csv
import hashlib
import subprocess
from pathlib import Path

import pytest

from digitalmodel.workflows import openfoam_batch_config as config_module
from digitalmodel.workflows.openfoam_batch_config import build_run_identity


def _commit(repo: Path, message: str) -> None:
    subprocess.run(["git", "-C", repo, "add", "."], check=True)
    subprocess.run(
        ["git", "-C", repo, "-c", "user.name=T", "-c",
         "user.email=t@x.invalid", "commit", "-qm", message], check=True,
    )


def _source_args(tmp_path: Path) -> dict:
    repo = tmp_path / "repo"
    package = repo / "src/demo_pkg"
    package.mkdir(parents=True)
    (package / "__init__.py").write_bytes(b"VALUE = 1\n")
    request = repo / "request.yml"
    request.write_bytes(b"request: one\n")
    subprocess.run(["git", "init", "-q", repo], check=True)
    _commit(repo, "fixture")
    return {
        "config_path": request, "package_root": package,
        "package_name": "demo-pkg", "package_version": "1.0",
        "effective_config": {"mode": "pool"}, "referenced_inputs": {},
        "selected_executables": {}, "visible_rank_count": 8,
        "dispatcher_rank_limit": 4, "result_policy_version": "result-policy-v1",
        "work_layout_version": "work-layout-v1",
    }


def _digest(data: bytes) -> str:
    encoded = base64.urlsafe_b64encode(hashlib.sha256(data).digest()).rstrip(b"=")
    return "sha256=" + encoded.decode()


def _wheel_args(tmp_path: Path) -> tuple[dict, Path, Path]:
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
            ["demo_pkg/__init__.py", _digest(module.read_bytes()),
             str(module.stat().st_size)],
            ["demo_pkg-1.0.dist-info/RECORD", "", ""],
        ])
    return ({
        "config_path": None, "package_root": package,
        "package_name": "demo-pkg", "package_version": "1.0",
        "effective_config": {"mode": "pool"}, "referenced_inputs": {},
        "selected_executables": {}, "visible_rank_count": 8,
        "dispatcher_rank_limit": 4, "result_policy_version": "result-policy-v1",
        "work_layout_version": "work-layout-v1", "distribution_root": site,
    }, module, record)


def test_source_package_rejects_transient_content_view(tmp_path, monkeypatch):
    args = _source_args(tmp_path)
    module = args["package_root"] / "__init__.py"
    original_read = Path.read_bytes
    raced = False

    def transient_read(path):
        nonlocal raced
        if path != module or raced:
            return original_read(path)
        raced = True
        original = original_read(path)
        path.write_bytes(b"TRANSIENT = 1\n")
        observed = original_read(path)
        path.write_bytes(original)
        return observed

    monkeypatch.setattr(Path, "read_bytes", transient_read)
    with pytest.raises(ValueError, match="changed"):
        build_run_identity(**args)


def test_source_package_rejects_transient_tracked_membership(tmp_path, monkeypatch):
    args = _source_args(tmp_path)
    repo = args["package_root"].parents[1]
    extra = args["package_root"] / "extra.py"
    original_git_output = config_module._git_output
    original_read = Path.read_bytes
    inserted = False

    def insert_after_head(repo_path, *git_args):
        nonlocal inserted
        output = original_git_output(repo_path, *git_args)
        if git_args == ("rev-parse", "HEAD") and not inserted:
            inserted = True
            extra.write_bytes(b"EXTRA = 1\n")
            subprocess.run(["git", "-C", repo, "add", str(extra)], check=True)
        return output

    def remove_after_read(path):
        data = original_read(path)
        if path == extra:
            subprocess.run(["git", "-C", repo, "rm", "--cached", "-q", str(extra)],
                           check=True)
            extra.unlink()
        return data

    monkeypatch.setattr(config_module, "_git_output", insert_after_head)
    monkeypatch.setattr(Path, "read_bytes", remove_after_read)
    with pytest.raises(ValueError, match="membership|changed"):
        build_run_identity(**args)


@pytest.mark.parametrize("mutation", ["add", "remove", "rename"])
def test_wheel_rejects_package_membership_change(tmp_path, monkeypatch, mutation):
    args, module, record = _wheel_args(tmp_path)
    original_read = Path.read_bytes
    record_reads = 0

    def mutate_after_inventory(path):
        nonlocal record_reads
        data = original_read(path)
        if path == record:
            record_reads += 1
            if record_reads == 2:
                if mutation == "add":
                    (module.parent / "extra.py").write_bytes(b"EXTRA = 1\n")
                elif mutation == "remove":
                    module.unlink()
                else:
                    module.rename(module.with_name("renamed.py"))
        return data

    monkeypatch.setattr(Path, "read_bytes", mutate_after_inventory)
    with pytest.raises(ValueError, match="membership|changed|missing"):
        build_run_identity(**args)


def test_wheel_rejects_matching_record_membership_change(tmp_path, monkeypatch):
    args, _, record = _wheel_args(tmp_path)
    original_read = Path.read_bytes
    record_reads = 0

    def add_ambiguous_record(path):
        nonlocal record_reads
        data = original_read(path)
        if path == record:
            record_reads += 1
            if record_reads == 2:
                other = args["distribution_root"] / "demo.pkg-1.0.dist-info"
                other.mkdir()
                (other / "RECORD").write_bytes(data)
        return data

    monkeypatch.setattr(Path, "read_bytes", add_ambiguous_record)
    with pytest.raises(ValueError, match="distribution|membership"):
        build_run_identity(**args)
