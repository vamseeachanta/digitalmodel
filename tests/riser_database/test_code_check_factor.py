# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Test legacy factor parity and citation metadata, not standard equations."""

from __future__ import annotations

import hashlib
import os
import subprocess
import sys
from pathlib import Path

import pytest

from digitalmodel.citations import CitationResolutionError, resolver
from digitalmodel.riser_database import get_dnv_safety_class_factor


FIXTURE_ROOT = Path(__file__).resolve().parents[1] / "citations" / "fixtures"
PAGE = Path("wikis/engineering-standards/wiki/standards/dnv-os-f201.md")


@pytest.fixture(autouse=True)
def isolated_resolution(tmp_path, monkeypatch):
    """Keep resolution offline and independent of local wiki discovery."""
    resolver._RESOLUTION_CACHE.clear()
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path / "no-config"))
    monkeypatch.delenv("LLM_WIKI_PATH", raising=False)
    monkeypatch.delenv("DIGITALMODEL_REPO_ROOT", raising=False)
    monkeypatch.setattr(resolver, "_KNOWN_LOCAL_CLONES", ())
    deep = tmp_path.joinpath(*(["nested"] * 20))
    deep.mkdir(parents=True)
    monkeypatch.setattr(resolver, "__file__", str(deep / "resolver.py"))
    yield
    resolver._RESOLUTION_CACHE.clear()


@pytest.mark.parametrize(
    "safety_class, expected", [("low", 1.04), ("normal", 1.14), ("high", 1.26)]
)
def test_defined_classes_match_engine_and_citation(safety_class, expected):
    """Frontmatter establishes identity only; engine parity is a regression."""
    from digitalmodel.orcaflex.code_check_engine import DNVOSF201Input, SafetyClass

    cited = get_dnv_safety_class_factor(safety_class, repo_root=FIXTURE_ROOT)
    engine = DNVOSF201Input(safety_class=SafetyClass(safety_class))
    assert cited.value == engine.safety_class_factor() == expected
    assert cited.units == "dimensionless"
    assert cited.citation.code_id == "dnv-os-f201"
    assert cited.citation.publisher == "DNV"
    assert cited.citation.revision == "2010"
    assert cited.citation.source_sibling == "generic"
    assert cited.citation.wiki_path == PAGE.as_posix()
    assert cited.citation.section == "Sec.5 (safety class resistance factors)"
    assert safety_class in cited.citation.note
    assert "legacy implementation" in cited.citation.note
    assert "not independently validated" in cited.citation.note


def test_default_selects_normal_class():
    assert get_dnv_safety_class_factor(repo_root=FIXTURE_ROOT).value == 1.14


@pytest.mark.parametrize(
    "safety_class, default", [("low", 1.04), ("normal", 1.14), ("high", 1.26)]
)
def test_override_wins_and_retains_legacy_basis(safety_class, default):
    cited = get_dnv_safety_class_factor(
        safety_class, override=1.5, repo_root=FIXTURE_ROOT
    )
    assert cited.value == 1.5
    assert "user override 1.5" in cited.citation.note
    assert str(default) in cited.citation.note
    assert "legacy implementation" in cited.citation.note
    assert "not independently validated" in cited.citation.note


@pytest.mark.parametrize("safety_class", ["unknown", "NORMAL", "", None])
def test_unknown_selector_raises_even_with_override(safety_class):
    with pytest.raises(ValueError, match="low.*normal.*high"):
        get_dnv_safety_class_factor(safety_class, override=1.5, repo_root=FIXTURE_ROOT)


@pytest.mark.parametrize(
    "override",
    [0, -1, float("nan"), float("inf"), -float("inf"), "invalid", "1,25", "1,000", 10**400],
)
def test_invalid_override_is_rejected(override):
    with pytest.raises(ValueError, match="finite and positive"):
        get_dnv_safety_class_factor(override=override, repo_root=FIXTURE_ROOT)


@pytest.mark.parametrize("route", ["explicit", "environment"])
@pytest.mark.parametrize(
    "failure",
    ["page_missing", "revision_mismatch", "publisher_mismatch", "code_id_missing"],
)
@pytest.mark.parametrize("override", [None, 1.5])
def test_invalid_metadata_fails_closed(tmp_path, monkeypatch, route, failure, override):
    """Mutate a temporary copy; retain the original fixture bytes."""
    original = FIXTURE_ROOT / "knowledge" / PAGE
    before = hashlib.sha256(original.read_bytes()).hexdigest()
    root = tmp_path / "wiki"
    page = root / "knowledge" / PAGE
    page.parent.mkdir(parents=True)
    text = original.read_text(encoding="utf-8")
    if failure == "revision_mismatch":
        text = text.replace('revision: "2010"', 'revision: "stale-test-revision"')
    elif failure == "publisher_mismatch":
        text = text.replace("publisher: DNV", "publisher: synthetic")
    elif failure == "code_id_missing":
        text = text.replace("code_id: dnv-os-f201\n", "")
    if failure != "page_missing":
        page.write_text(text, encoding="utf-8")
    if route == "environment":
        monkeypatch.setenv("LLM_WIKI_PATH", str(root))
    with pytest.raises(CitationResolutionError) as exc:
        get_dnv_safety_class_factor(
            override=override, repo_root=root if route == "explicit" else None
        )
    assert exc.value.code_id == "dnv-os-f201"
    expected = {
        "page_missing": "page_missing",
        "revision_mismatch": "frontmatter_mismatch:revision:",
        "publisher_mismatch": "frontmatter_mismatch:publisher:",
        "code_id_missing": "frontmatter_missing:code_id",
    }
    assert exc.value.reason.startswith(expected[failure])
    assert hashlib.sha256(original.read_bytes()).hexdigest() == before


def test_unconfigured_resolution_raises():
    with pytest.raises(CitationResolutionError):
        get_dnv_safety_class_factor()


def test_export_imports_without_engine_or_native_bindings():
    program = """
import sys
from importlib.abc import MetaPathFinder
class RejectSolver(MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname == 'OrcFxAPI' or fullname.startswith('digitalmodel.orcaflex'):
            raise AssertionError('Getter import must not load solver modules')
sys.meta_path.insert(0, RejectSolver())
from digitalmodel.riser_database import get_dnv_safety_class_factor
assert callable(get_dnv_safety_class_factor)
"""
    env = dict(os.environ, PYTHONPATH=os.pathsep.join(sys.path))
    completed = subprocess.run(
        [sys.executable, "-c", program],
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    assert completed.returncode == 0, completed.stderr
