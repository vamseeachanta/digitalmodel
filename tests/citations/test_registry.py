"""Registry integration tests — pilot: mooring safety factors from DNV-OS-E301.

Per #2481 D1: mooring load-factor is the pilot calc target.
"""
from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.citations import CitationResolutionError
from digitalmodel.citations.registry import (
    MooringCondition,
    get_mooring_safety_factor,
)
from digitalmodel.citations import registry


def _repo_root() -> Path:
    # Option B (workspace-hub #2580): vendor wiki fixtures into the test tree so
    # the citation resolver works regardless of how digitalmodel is checked out
    # (workspace-hub overlay vs. standalone CI clone). The fixture tree mirrors
    # the resolver's expected `knowledge/wikis/...` subpath bit-for-bit.
    return Path(__file__).resolve().parent / "fixtures"


def test_intact_quasi_static_factor_emits_cited_value():
    cv = get_mooring_safety_factor(MooringCondition.INTACT_QUASI_STATIC, repo_root=_repo_root())
    assert cv.value == 1.67
    assert cv.citation.code_id == "DNV-OS-E301"
    assert cv.citation.publisher == "DNV"
    assert cv.citation.revision == "2021-07"
    assert "Section 2.2.3" in cv.citation.section
    assert cv.citation.wiki_path.endswith("dnv-os-e301.md")
    assert cv.units == "dimensionless"


def test_damaged_quasi_static_factor_emits_cited_value():
    cv = get_mooring_safety_factor(MooringCondition.DAMAGED_QUASI_STATIC, repo_root=_repo_root())
    assert cv.value == 1.25
    assert cv.citation.code_id == "DNV-OS-E301"


def test_registry_unknown_condition_raises():
    with pytest.raises(ValueError):
        get_mooring_safety_factor("nonexistent-condition", repo_root=_repo_root())  # type: ignore[arg-type]


def test_registry_with_broken_repo_root_fails_closed(tmp_path):
    # If the "repo root" does not actually contain the cited wiki page,
    # the registry must raise CitationResolutionError rather than returning a value.
    with pytest.raises(CitationResolutionError) as exc:
        get_mooring_safety_factor(MooringCondition.INTACT_QUASI_STATIC, repo_root=tmp_path)
    assert exc.value.code_id == "DNV-OS-E301"
    assert exc.value.reason == "page_missing"


def test_registry_repo_root_none_defers_to_resolver(monkeypatch):
    """#617: repo_root=None defers to LLM_WIKI_PATH resolver chain."""
    monkeypatch.setenv("LLM_WIKI_PATH", str(_repo_root()))
    cv = get_mooring_safety_factor(MooringCondition.INTACT_QUASI_STATIC, repo_root=None)
    assert cv.value == 1.67
    assert cv.citation.code_id == "DNV-OS-E301"


def test_registry_canonical_wiki_path_form_is_wikis_no_prefix():
    """#617: canonical citation wiki_path form is `wikis/...` (no `knowledge/` prefix)."""
    cv = get_mooring_safety_factor(MooringCondition.INTACT_QUASI_STATIC, repo_root=_repo_root())
    assert cv.citation.wiki_path.startswith("wikis/"), (
        f"Expected canonical wiki_path to start with 'wikis/', got {cv.citation.wiki_path!r}"
    )
    assert not cv.citation.wiki_path.startswith("knowledge/"), (
        f"Canonical wiki_path should not have 'knowledge/' prefix, got {cv.citation.wiki_path!r}"
    )


def test_dnv_f103_reference_emits_cited_value():
    cv = registry.get_dnv_f103_reference(
        "Table 5-1 and Annex 1",
        note="Current density and coating breakdown basis",
        repo_root=_repo_root(),
    )
    assert cv.value == 1.0
    assert cv.citation.code_id == "dnv-rp-f103"
    assert cv.citation.publisher == "DNV"
    assert cv.citation.revision == "2010"
    assert cv.citation.section == "Table 5-1 and Annex 1"
    assert cv.citation.wiki_path == "wikis/engineering-standards/wiki/standards/dnv-rp-f103.md"
    assert cv.citation.note == "Current density and coating breakdown basis"
    assert cv.units == "reference"


def test_dnv_f103_reference_with_broken_repo_root_fails_closed(tmp_path):
    with pytest.raises(CitationResolutionError) as exc:
        registry.get_dnv_f103_reference("Table 5-1", repo_root=tmp_path)
    assert exc.value.code_id == "dnv-rp-f103"
    assert exc.value.reason == "page_missing"
