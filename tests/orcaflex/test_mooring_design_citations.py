"""Integration tests for #2685 — DNV-OS-E301 mooring citation pilot.

Plan: docs/plans/2026-05-13-issue-2685-citation-pilot-option-a-plan.md
Covers AC2 (behavior of get_intact/damaged_safety_factor, check_mbl_with_safety_factor)
plus the user-override-wins, standalone-graceful, bounded-walk-sentinel, and
fail-closed defenses.
"""
from __future__ import annotations

import warnings
from pathlib import Path
from unittest.mock import patch

import pytest

from digitalmodel.citations import CitationResolutionError
from digitalmodel.citations.schema import Citation, validate_citation
from digitalmodel.orcaflex import mooring_design as md
from digitalmodel.orcaflex.mooring_design import MooringLineDesign


def _fixture_repo_root() -> Path:
    """Workspace-hub fixture root for citation resolution (vendored per #2580)."""
    return Path(__file__).resolve().parent.parent / "citations" / "fixtures"


@pytest.fixture(autouse=True)
def _reset_warn_cache():
    """Each test starts with empty standalone-warning + resolver caches so the
    one-shot warning is observable in test_standalone_no_repo_root_graceful_warn
    and the resolver's DeprecationWarning cache is reset per-test."""
    from digitalmodel.citations import resolver as _resolver
    md._REPO_ROOT_RESOLUTION_CACHE.clear()
    _resolver._RESOLUTION_CACHE.clear()
    yield
    md._REPO_ROOT_RESOLUTION_CACHE.clear()
    _resolver._RESOLUTION_CACHE.clear()


@pytest.fixture(autouse=True)
def _unset_env_var(monkeypatch):
    monkeypatch.delenv("DIGITALMODEL_REPO_ROOT", raising=False)
    monkeypatch.delenv("LLM_WIKI_PATH", raising=False)


@pytest.fixture(autouse=True)
def _disable_known_clones_fallback(monkeypatch):
    """Disable real-filesystem known-clone fallbacks so tests in this module
    can exercise fail-closed paths deterministically without false-positive
    resolution via /mnt/local-analysis/llm-wiki etc."""
    from digitalmodel.citations import resolver as _resolver
    monkeypatch.setattr(_resolver, "_KNOWN_LOCAL_CLONES", ())


# -- AC2: behavior of get_intact / get_damaged ----------------------------------


def test_mooring_design_emits_intact_citation():
    cv = MooringLineDesign().get_intact_safety_factor(repo_root=_fixture_repo_root())
    assert cv.value == 1.67
    assert cv.citation.code_id == "DNV-OS-E301"
    assert cv.citation.publisher == "DNV"
    assert cv.citation.revision == "2021-07"
    assert "Section 2.2.3" in cv.citation.section
    assert cv.units == "dimensionless"


def test_mooring_design_emits_damaged_citation():
    cv = MooringLineDesign().get_damaged_safety_factor(repo_root=_fixture_repo_root())
    assert cv.value == 1.25
    assert cv.citation.code_id == "DNV-OS-E301"


# -- AC2: behavior of check_mbl_with_safety_factor ----------------------------


def test_check_mbl_with_sf_applies_sf():
    ml = MooringLineDesign()
    r = ml.check_mbl_with_safety_factor(
        3000.0, condition="intact", repo_root=_fixture_repo_root()
    )
    assert r["safety_factor"] == 1.67
    assert r["condition"] == "intact"
    assert len(r["citations"]) == 1
    assert r["citations"][0].code_id == "DNV-OS-E301"

    # Behavioral check: utilisation_with_sf = (max_tension * 1.67) / mbl per segment.
    for seg in ml.segments:
        from digitalmodel.orcaflex.mooring_design import MOORING_MATERIAL_LIBRARY
        mbl = MOORING_MATERIAL_LIBRARY[seg.material_key].mbl
        expected_with = round((3000.0 * 1.67) / mbl, 4)
        expected_no = round(3000.0 / mbl, 4)
        assert r["results"][seg.material_key]["utilisation_with_sf"] == expected_with
        assert r["results"][seg.material_key]["utilisation_no_sf"] == expected_no


def test_check_mbl_with_sf_fail_closed_on_missing_wiki(tmp_path, monkeypatch):
    # tmp_path has knowledge/wikis/ shape so _default_repo_root accepts it,
    # but the cited page doesn't exist -> registry should raise.
    (tmp_path / "knowledge" / "wikis").mkdir(parents=True)
    monkeypatch.setenv("DIGITALMODEL_REPO_ROOT", str(tmp_path))
    with pytest.raises(CitationResolutionError) as exc:
        MooringLineDesign().check_mbl_with_safety_factor(3000.0, condition="intact")
    assert exc.value.code_id == "DNV-OS-E301"
    assert exc.value.reason == "page_missing"


# -- AC2 fix #6: user override wins -------------------------------------------


def test_user_override_wins_for_intact_sf():
    ml = MooringLineDesign(safety_factor_intact=2.5)
    r = ml.check_mbl_with_safety_factor(
        3000.0, condition="intact", repo_root=_fixture_repo_root()
    )
    assert r["safety_factor"] == 2.5
    assert len(r["citations"]) == 1
    note = r["citations"][0].note
    assert "user override" in note
    assert "1.67" in note  # registry reference value cited


def test_user_override_wins_for_damaged_sf():
    ml = MooringLineDesign(safety_factor_damaged=1.5)
    r = ml.check_mbl_with_safety_factor(
        3000.0, condition="damaged", repo_root=_fixture_repo_root()
    )
    assert r["safety_factor"] == 1.5
    note = r["citations"][0].note
    assert "user override" in note
    assert "1.25" in note


# -- AC2 fix #7: standalone graceful no-op ------------------------------------


def test_standalone_no_repo_root_graceful_warn():
    ml = MooringLineDesign()
    with patch.object(md, "_default_repo_root", return_value=None):
        with pytest.warns(RuntimeWarning, match="standalone mode"):
            r = ml.check_mbl_with_safety_factor(3000.0, condition="intact")
    assert r["safety_factor"] == 1.67  # field default
    assert r["citations"] == []
    # utilisation still produced even without citation
    assert all(
        "utilisation_with_sf" in seg_result for seg_result in r["results"].values()
    )


# -- AC2 fix #5: bounded parent walk + env var resolution ---------------------


def test_repo_root_walk_bounded_sentinel_via_delegation(tmp_path, monkeypatch):
    """Integration test: mooring_design._default_repo_root delegates to
    resolve_wiki_base, and the bounded walk in the resolver terminates within
    ~ms returning None for standalone (no env, no clone, no overlay).
    Bounded-walk sentinel is verified per-resolver-test at
    tests/citations/test_resolver.py::test_resolver_parent_walk_has_sentinel."""
    from digitalmodel.citations import resolver as _resolver
    deep = tmp_path
    for i in range(20):
        deep = deep / f"d{i}"
    deep.mkdir(parents=True)
    fake_file = deep / "fake.py"
    fake_file.write_text("# fake")
    monkeypatch.setattr(_resolver, "__file__", str(fake_file))
    import time
    start = time.monotonic()
    result = md._default_repo_root()
    elapsed = time.monotonic() - start
    assert result is None, f"expected None (standalone fail-closed), got {result!r}"
    assert elapsed < 0.5, f"walk took {elapsed:.3f}s — expected bounded"


def test_repo_root_env_var_overrides_walk(monkeypatch):
    """Legacy DIGITALMODEL_REPO_ROOT still works (with DeprecationWarning)."""
    fixture = _fixture_repo_root()
    monkeypatch.setenv("DIGITALMODEL_REPO_ROOT", str(fixture))
    with pytest.warns(DeprecationWarning, match="DIGITALMODEL_REPO_ROOT"):
        assert md._default_repo_root() == fixture


def test_repo_root_invalid_env_var_raises(monkeypatch, tmp_path):
    """Invalid DIGITALMODEL_REPO_ROOT raises CitationResolutionError (symmetric
    with LLM_WIKI_PATH behavior). Reason names the env var so the user knows
    what to fix."""
    bad = tmp_path / "nonexistent-root"
    monkeypatch.setenv("DIGITALMODEL_REPO_ROOT", str(bad))
    with pytest.raises(CitationResolutionError) as exc:
        md._default_repo_root()
    assert "DIGITALMODEL_REPO_ROOT" in exc.value.reason
    assert exc.value.reason.startswith("digitalmodel_repo_root_invalid:")


def test_repo_root_llm_wiki_path_env_var_supported(monkeypatch):
    """LLM_WIKI_PATH env var (new, per #617) is honored by _default_repo_root
    via delegation to the resolver."""
    fixture = _fixture_repo_root()
    monkeypatch.setenv("LLM_WIKI_PATH", str(fixture))
    assert md._default_repo_root() == fixture


def test_repo_root_llm_wiki_path_takes_precedence_over_legacy(monkeypatch, tmp_path):
    """When both env vars are set, LLM_WIKI_PATH wins per #617 precedence chain.
    Legacy DeprecationWarning does NOT fire because the new var took precedence."""
    fixture = _fixture_repo_root()
    monkeypatch.setenv("LLM_WIKI_PATH", str(fixture))
    monkeypatch.setenv("DIGITALMODEL_REPO_ROOT", str(tmp_path / "garbage"))
    with warnings.catch_warnings():
        warnings.simplefilter("error", DeprecationWarning)  # any DeprecationWarning fails the test
        assert md._default_repo_root() == fixture


# -- Codex finding: invalid condition string ----------------------------------


def test_invalid_condition_string_raises():
    ml = MooringLineDesign()
    with pytest.raises(ValueError, match="must be 'intact' or 'damaged'"):
        ml.check_mbl_with_safety_factor(3000.0, condition="damagd")


# -- AC6 smoke: production wiki page frontmatter matches template -------------


def test_wiki_page_frontmatter_matches_template():
    """If this test fails, the production wiki page at
    knowledge/wikis/engineering/wiki/standards/dnv-os-e301.md has drifted from
    the citation template — fail-closed at calc time would catch it but this
    test gives a clear localized signal."""
    # Walk from this file up to workspace-hub (must find knowledge/wikis/)
    here = Path(__file__).resolve()
    for parent in [here, *here.parents][:8]:
        if (parent / "knowledge" / "wikis").is_dir():
            workspace_root = parent
            break
    else:
        pytest.skip("not running inside workspace-hub overlay; AC6 covered by AC1")

    template = Citation(
        code_id="DNV-OS-E301",
        publisher="DNV",
        revision="2021-07",
        section="Section 2.2.3 (intact, quasi-static)",
        wiki_path="wikis/engineering/wiki/standards/dnv-os-e301.md",
    )
    validate_citation(template, repo_root=workspace_root)  # raises on drift
