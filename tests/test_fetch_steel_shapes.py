"""Fetch guards are exercised offline in disposable repositories."""

import hashlib
import importlib.util
import os
import subprocess
from pathlib import Path

import pytest


@pytest.fixture
def fetch_module():
    source = (
        Path(__file__).resolve().parents[1]
        / "scripts/integrations/fetch_steel_shapes.py"
    )
    spec = importlib.util.spec_from_file_location("fetch_shapes", source)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize(
    "url", ["http://www.aisc.org/shapes.xlsx", "https://example.invalid/shapes.xlsx"]
)
def test_reject_nonpublisher_urls(fetch_module, url):
    with pytest.raises(ValueError, match="publisher HTTPS"):
        fetch_module.publisher_url(url)


def test_publisher_redirect_guard(fetch_module):
    request = fetch_module.urllib.request.Request("https://www.aisc.org/shapes.xlsx")
    with pytest.raises(ValueError, match="publisher HTTPS"):
        fetch_module.PublisherRedirect().redirect_request(
            request, None, 302, "redirect", {}, "https://example.invalid/shapes.xlsx"
        )


def _git_fixture(root):
    root.mkdir()
    environment = {
        key: value
        for key, value in os.environ.items()
        if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}
    }
    subprocess.run(["git", "init", "-q", str(root)], env=environment, check=True)
    (root / ".gitignore").write_text("/data/steel_shapes_local/\n")
    return root


def test_output_guard_ignores_inherited_git_bindings(
    fetch_module, tmp_path, monkeypatch
):
    caller = _git_fixture(tmp_path / "caller")
    job = _git_fixture(tmp_path / "job")
    sentinel = caller / "preserved.txt"
    sentinel.write_text("fixture shall remain unchanged")
    before = {
        path: hashlib.sha256(path.read_bytes()).hexdigest()
        for path in [sentinel, caller / ".git/HEAD", caller / ".git/config"]
    }
    monkeypatch.setenv("GIT_DIR", str(caller / ".git"))
    monkeypatch.setenv("GIT_WORK_TREE", str(caller))
    monkeypatch.setenv("GIT_COMMON_DIR", str(caller / ".git"))
    monkeypatch.setattr(
        fetch_module,
        "__file__",
        str(job / "scripts/integrations/fetch_steel_shapes.py"),
    )
    assert fetch_module.output_directory() == job / "data/steel_shapes_local"
    after = {path: hashlib.sha256(path.read_bytes()).hexdigest() for path in before}
    assert after == before


def test_output_guard_preserves_existing_data(fetch_module, tmp_path, monkeypatch):
    job = _git_fixture(tmp_path / "job")
    monkeypatch.setattr(
        fetch_module,
        "__file__",
        str(job / "scripts/integrations/fetch_steel_shapes.py"),
    )
    output = job / "data/steel_shapes_local"
    output.mkdir(parents=True)
    sentinel = output / "source.xlsx"
    sentinel.write_bytes(b"preserved fixture")
    digest = hashlib.sha256(sentinel.read_bytes()).hexdigest()
    with pytest.raises(FileExistsError, match="preserved"):
        fetch_module.output_directory()
    assert hashlib.sha256(sentinel.read_bytes()).hexdigest() == digest


def test_output_guard_requires_ignore_rule(fetch_module, tmp_path, monkeypatch):
    job = _git_fixture(tmp_path / "job")
    (job / ".gitignore").write_text("# no catalogue ignore rule\n")
    monkeypatch.setattr(
        fetch_module,
        "__file__",
        str(job / "scripts/integrations/fetch_steel_shapes.py"),
    )
    with pytest.raises(ValueError, match="git-ignored"):
        fetch_module.output_directory()
    assert not (job / "data/steel_shapes_local").exists()
