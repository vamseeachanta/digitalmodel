"""Citation-emission tests for naval_architecture seakeeping.

Wires the EN400 (USNA Principles of Ship Performance, Summer 2020) citation
target to the `natural_heave_period` calc, mirroring the fundamentals pilot
(tests/naval_architecture/test_na_fundamentals_citations.py) and the
DNV-OS-E301 mooring pilot.
"""
from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.citations import CitationResolutionError
from digitalmodel.citations.schema import CitationResolutionError as _CRE
from digitalmodel.naval_architecture.seakeeping import (
    natural_heave_period,
    natural_heave_period_cited,
)


def _repo_root() -> Path:
    # Reuse the vendored citation fixtures tree (shared with tests/citations/).
    return Path(__file__).resolve().parent.parent / "citations" / "fixtures"


def test_natural_heave_period_cited_value_matches_plain():
    """The cited variant must return the same physical value as the legacy fn."""
    result = natural_heave_period_cited(10000, 2000, repo_root=_repo_root())
    assert result["value"] == natural_heave_period(10000, 2000)
    assert result["units"] == "s"


def test_natural_heave_period_cited_emits_en400_citation():
    result = natural_heave_period_cited(10000, 2000, repo_root=_repo_root())
    assert len(result["citations"]) == 1
    c = result["citations"][0]
    assert c.code_id == "EN400"
    assert c.publisher == "USNA"
    assert c.revision == "Summer 2020"
    assert c.wiki_path.endswith("en400.md")
    assert "Chapter 8" in c.section


def test_natural_heave_period_cited_fail_closed_on_missing_page(tmp_path):
    """Configured repo_root without the page must fail closed, not silently drop."""
    with pytest.raises(CitationResolutionError) as exc:
        natural_heave_period_cited(10000, 2000, repo_root=tmp_path)
    assert exc.value.code_id == "EN400"
    assert exc.value.reason == "page_missing"


def test_natural_heave_period_cited_standalone_graceful(monkeypatch):
    """Standalone mode (resolver unconfigured) warns once and emits no citation,
    while still returning the correct value."""
    from digitalmodel.naval_architecture import seakeeping as sk

    def _unconfigured(*_a, **_k):
        raise _CRE(code_id="EN400", wiki_path="wikis/...", reason="resolver_unconfigured:test")

    monkeypatch.setattr(sk, "get_en400_reference", _unconfigured, raising=False)
    with pytest.warns(RuntimeWarning, match="standalone"):
        result = natural_heave_period_cited(10000, 2000)
    assert result["value"] == natural_heave_period(10000, 2000)
    assert result["citations"] == []
