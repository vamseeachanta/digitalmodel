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


def test_get_tension_weight_factor_emits_16q_citation():
    cv = getters.get_tension_weight_factor(repo_root=_fixture_repo_root())
    assert cv.value == 1.05
    assert cv.citation.code_id == "api-rp-16q"
    assert cv.citation.publisher == "API"
    assert cv.citation.revision == "1993"  # 1st Ed. 1993, not 2017
    assert cv.citation.source_sibling == "generic"
    assert "3.3" in cv.citation.section
    assert cv.units == "dimensionless"


def test_get_buoyancy_tension_factor_emits_16q_citation():
    cv = getters.get_buoyancy_tension_factor(repo_root=_fixture_repo_root())
    assert cv.value == 0.96
    assert cv.citation.code_id == "api-rp-16q"
    assert cv.citation.publisher == "API"
    assert cv.citation.revision == "1993"
    assert cv.citation.source_sibling == "generic"
    assert "3.3" in cv.citation.section


def test_16q_getters_are_in_parity_with_stackup_literals():
    """The getters must return exactly the drilling_riser.stackup literals —
    the calc still uses the module constants; the getters cite the same values.
    """
    from digitalmodel.drilling_riser import stackup

    f_wt = getters.get_tension_weight_factor(repo_root=_fixture_repo_root())
    f_bt = getters.get_buoyancy_tension_factor(repo_root=_fixture_repo_root())
    assert f_wt.value == stackup.F_WT_DEFAULT == 1.05
    assert f_bt.value == stackup.F_BT_DEFAULT == 0.96
    # And no getter exists for the 1.25 top-tension factor (no 16Q provision).
    assert not hasattr(getters, "get_tension_safety_factor")


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


def test_user_override_wins_for_f_wt():
    cv = getters.get_tension_weight_factor(override=1.1, repo_root=_fixture_repo_root())
    assert cv.value == 1.1
    assert "user override" in cv.citation.note
    assert "1.05" in cv.citation.note  # standard value still cited


def test_user_override_wins_for_f_bt():
    cv = getters.get_buoyancy_tension_factor(override=0.9, repo_root=_fixture_repo_root())
    assert cv.value == 0.9
    assert "user override" in cv.citation.note
    assert "0.96" in cv.citation.note


# -- #1281a envelope criteria getters --------------------------------------------


def test_get_von_mises_design_factor_emits_2rd_citation():
    cv = getters.get_von_mises_design_factor(repo_root=_fixture_repo_root())
    assert cv.value == 0.67
    assert cv.citation.code_id == "api-std-2rd"
    assert cv.citation.publisher == "API"
    assert cv.citation.revision == "3e-2025"
    assert cv.citation.source_sibling == "generic"
    assert cv.units == "dimensionless"


def test_get_flexjoint_angle_limit_mean_and_max():
    mean = getters.get_flexjoint_angle_limit("mean", repo_root=_fixture_repo_root())
    mx = getters.get_flexjoint_angle_limit("max", repo_root=_fixture_repo_root())
    assert mean.value == 2.0 and mx.value == 4.0
    assert mean.citation.code_id == mx.citation.code_id == "api-rp-16q"
    assert mean.citation.revision == "1993"
    assert mean.units == "degrees"


def test_get_flexjoint_angle_limit_rejects_unknown_kind():
    with pytest.raises(ValueError):
        getters.get_flexjoint_angle_limit("median", repo_root=_fixture_repo_root())


def test_von_mises_getter_matches_code_check_engine_default():
    """Parity: the getter value equals the public APIRP2RDInput.design_factor."""
    from digitalmodel.orcaflex.code_check_engine import APIRP2RDInput

    cv = getters.get_von_mises_design_factor(repo_root=_fixture_repo_root())
    assert cv.value == APIRP2RDInput().design_factor == 0.67


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
    assert set(cited) == {"dff", "scf", "f_wt", "f_bt", "von_mises_df"}
    assert cited["dff"].value == 10.0
    assert cited["scf"].value == 1.0
    assert cited["f_wt"].value == 1.05
    assert cited["f_bt"].value == 0.96
    assert cited["von_mises_df"].value == 0.67
