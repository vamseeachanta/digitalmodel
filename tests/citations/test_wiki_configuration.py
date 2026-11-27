"""Private checkout configuration, exercised with disposable source trees."""

import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

from digitalmodel.citations import CitationResolutionError, resolver
from digitalmodel.drilling_riser.calc_contracts import REGISTRY_REL, resolve_wiki_root


@pytest.fixture
def checkout(tmp_path, monkeypatch):
    monkeypatch.delenv("LLM_WIKI_PATH", raising=False)
    monkeypatch.delenv("DIGITALMODEL_REPO_ROOT", raising=False)
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path / "config"))
    monkeypatch.setattr(resolver, "_KNOWN_LOCAL_CLONES", ())
    repo = tmp_path / "application"
    repo.mkdir()
    (repo / ".git").write_text("gitdir: worktree", encoding="utf-8")
    monkeypatch.setattr(resolver, "__file__", str(repo / "src" / "resolver.py"))
    return tmp_path


def _clone(path):
    page = path / REGISTRY_REL
    page.parent.mkdir(parents=True)
    page.write_text("synthetic registry", encoding="utf-8")
    return path


def _configure(root, **values):
    config = root / "config" / "digitalmodel" / "paths.json"
    config.parent.mkdir(parents=True)
    config.write_text(json.dumps(values), encoding="utf-8")


def test_environment_precedes_configured_path_and_sibling(checkout, monkeypatch):
    env_root = _clone(checkout / "environment")
    absolute_root = _clone(checkout / "absolute")
    sibling = _clone(checkout / "reference")
    _configure(checkout, wiki_path=str(absolute_root), wiki_sibling=sibling.name)
    monkeypatch.setenv("LLM_WIKI_PATH", str(env_root))
    monkeypatch.setenv("DIGITALMODEL_REPO_ROOT", str(_clone(checkout / "legacy")))
    assert resolver.resolve_wiki_base() == env_root
    assert resolve_wiki_root() == env_root

    monkeypatch.delenv("LLM_WIKI_PATH")
    assert resolver.resolve_wiki_base() == absolute_root
    assert resolve_wiki_root() == absolute_root


def test_stale_configured_path_uses_checkout_sibling(checkout, monkeypatch):
    sibling = _clone(checkout / "reference")
    monkeypatch.setenv("DIGITALMODEL_REPO_ROOT", str(_clone(checkout / "legacy")))
    _configure(checkout, wiki_path=str(checkout / "missing"), wiki_sibling=sibling.name)
    assert resolver.resolve_wiki_base() == sibling
    assert resolve_wiki_root() == sibling


def test_configured_overlay_uses_nested_registry(checkout):
    root = checkout / "overlay"
    _clone(root / "knowledge")
    _configure(checkout, wiki_path=str(root))
    assert resolver.resolve_wiki_base() == root
    assert resolve_wiki_root() == root / "knowledge"


@pytest.mark.parametrize("name", ["../external", "nested/reference", r"..\external", "."])
def test_sibling_name_must_be_a_single_directory(checkout, name):
    _configure(checkout, wiki_sibling=name)
    with pytest.raises(CitationResolutionError, match="wiki_config_invalid"):
        resolver.resolve_wiki_base()


@pytest.mark.parametrize("remote", [None, "https://example.invalid/synthetic.git"])
def test_exported_remote_reflects_configuration(remote):
    env = os.environ.copy()
    env.pop("DIGITALMODEL_WIKI_URL", None)
    if remote:
        env["DIGITALMODEL_WIKI_URL"] = remote
    code = (
        "import json; from digitalmodel.citations import WIKI_REPO_URL; "
        "print(json.dumps(WIKI_REPO_URL))"
    )
    value = subprocess.check_output([sys.executable, "-c", code], text=True, env=env)
    assert json.loads(value) == remote
