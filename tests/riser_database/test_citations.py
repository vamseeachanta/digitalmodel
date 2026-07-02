"""Suite 5 (#1245): the 4 citation defenses for the riser getters, adapted from
tests/orcaflex/test_mooring_design_citations.py:

  (a) fail-closed on a missing wiki page (CitationResolutionError carries code_id)
  (b) user-override-wins (note records the override and the standard value)
  (c) standalone-graceful one-shot RuntimeWarning from the consumer helper
  (d) bounded-walk sentinel (degraded resolution terminates fast)

plus the explicit `pip install digitalmodel` no-wiki path: with every
resolution route disabled, direct getter calls FAIL CLOSED and only the
consumer helper degrades.
"""
from __future__ import annotations

import time
import warnings
from pathlib import Path

import pytest

from digitalmodel.citations import CitationResolutionError
from digitalmodel.riser_database import getters


def _fixture_repo_root() -> Path:
    """Vendored wiki fixture root (frontmatter-only pages, see
    tests/citations/fixtures/**/FIXTURE_PROVENANCE.md)."""
    return Path(__file__).resolve().parent.parent / "citations" / "fixtures"


@pytest.fixture(autouse=True)
def _reset_caches(monkeypatch):
    from digitalmodel.citations import resolver as _resolver

    _resolver._RESOLUTION_CACHE.clear()
    monkeypatch.setattr(getters, "_CITATION_WARNED", False)
    yield
    _resolver._RESOLUTION_CACHE.clear()


@pytest.fixture(autouse=True)
def _unset_env(monkeypatch):
    monkeypatch.delenv("DIGITALMODEL_REPO_ROOT", raising=False)
    monkeypatch.delenv("LLM_WIKI_PATH", raising=False)


@pytest.fixture(autouse=True)
def _disable_known_clones(monkeypatch):
    from digitalmodel.citations import resolver as _resolver

    monkeypatch.setattr(_resolver, "_KNOWN_LOCAL_CLONES", ())


@pytest.fixture()
def _no_wiki_anywhere(tmp_path, monkeypatch):
    """Simulate `pip install digitalmodel` with no wiki: env unset, known
    clones disabled (autouse), and the resolver's parent walk re-rooted into a
    deep tree that contains no clone."""
    from digitalmodel.citations import resolver as _resolver

    deep = tmp_path
    for i in range(20):
        deep = deep / f"d{i}"
    deep.mkdir(parents=True)
    monkeypatch.setattr(_resolver, "__file__", str(deep / "resolver.py"))


# -- behavior against the vendored fixture --------------------------------------


def test_get_riser_dff_emits_f201_citation():
    cv = getters.get_riser_dff(repo_root=_fixture_repo_root())
    assert cv.value == 10.0
    assert cv.citation.code_id == "dnv-os-f201"
    assert cv.citation.publisher == "DNV"
    assert cv.citation.revision == "2010"
    assert cv.citation.source_sibling == "generic"
    assert cv.units == "dimensionless"


def test_get_riser_scf_emits_c203_citation():
    cv = getters.get_riser_scf(repo_root=_fixture_repo_root())
    assert cv.value == 1.0
    assert cv.citation.code_id == "dnv-rp-c203"
    assert cv.citation.revision == "2024-10"
    assert cv.citation.source_sibling == "generic"
    # The standard sources the METHODOLOGY; 1.0 is the neutral default.
    assert "methodology" in cv.citation.note.lower()


# -- defense (a): fail-closed ----------------------------------------------------


def test_fail_closed_on_missing_wiki_page(tmp_path, monkeypatch):
    (tmp_path / "knowledge" / "wikis").mkdir(parents=True)
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path))
    with pytest.raises(CitationResolutionError) as exc:
        getters.get_riser_dff()
    assert exc.value.code_id == "dnv-os-f201"
    assert exc.value.reason == "page_missing"


# -- defense (b): user override wins ---------------------------------------------


def test_user_override_wins_for_dff():
    cv = getters.get_riser_dff(override=3.0, repo_root=_fixture_repo_root())
    assert cv.value == 3.0
    assert "user override" in cv.citation.note
    assert "10.0" in cv.citation.note  # standard value still cited


def test_user_override_wins_for_scf():
    cv = getters.get_riser_scf(override=1.2, repo_root=_fixture_repo_root())
    assert cv.value == 1.2
    assert "user override" in cv.citation.note
    assert "1.0" in cv.citation.note


# -- defenses (c)+(d) + pip-no-wiki path ------------------------------------------


def test_pip_no_wiki_direct_getter_fails_closed(_no_wiki_anywhere):
    with pytest.raises(CitationResolutionError):
        getters.get_riser_dff()


def test_pip_no_wiki_helper_warns_once_and_degrades(_no_wiki_anywhere):
    t0 = time.perf_counter()
    with pytest.warns(RuntimeWarning, match="standalone mode"):
        assert getters.riser_citations() == {}
    elapsed = time.perf_counter() - t0
    # defense (d): degraded resolution is bounded (no unbounded parent walk)
    assert elapsed < 0.5
    # one-shot: the second call is silent
    with warnings.catch_warnings():
        warnings.simplefilter("error", RuntimeWarning)
        assert getters.riser_citations() == {}


def test_helper_returns_cited_values_in_context():
    cited = getters.riser_citations(repo_root=_fixture_repo_root())
    assert set(cited) == {"dff", "scf"}
    assert cited["dff"].value == 10.0
    assert cited["scf"].value == 1.0
