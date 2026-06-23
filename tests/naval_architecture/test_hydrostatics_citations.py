"""Citation-emission tests for naval_architecture hydrostatics.

Wires the EN400 (USNA Principles of Ship Performance, Summer 2020) citation
target to the `buoyant_force` calc, mirroring the fundamentals pilot
(tests/naval_architecture/test_na_fundamentals_citations.py) and the DNV-OS-E301
mooring pilot (tests/citations/test_registry.py).
"""
from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.citations import CitationResolutionError
from digitalmodel.citations.schema import CitationResolutionError as _CRE
from digitalmodel.naval_architecture.hydrostatics import (
    buoyant_force,
    buoyant_force_cited,
)


def _repo_root() -> Path:
    # Reuse the vendored citation fixtures tree (shared with tests/citations/).
    return Path(__file__).resolve().parent.parent / "citations" / "fixtures"


def test_buoyant_force_cited_value_matches_plain():
    """The cited variant must return the same physical value as the legacy fn."""
    result = buoyant_force_cited(100.0, repo_root=_repo_root())
    assert result["value"] == buoyant_force(100.0)
    assert result["units"] == "lb"


def test_buoyant_force_cited_emits_en400_citation():
    result = buoyant_force_cited(100.0, repo_root=_repo_root())
    assert len(result["citations"]) == 1
    c = result["citations"][0]
    assert c.code_id == "EN400"
    assert c.publisher == "USNA"
    assert c.revision == "Summer 2020"
    assert c.wiki_path.endswith("en400.md")
    assert "Chapter 2" in c.section


def test_buoyant_force_cited_fail_closed_on_missing_page(tmp_path):
    """Configured repo_root without the page must fail closed, not silently drop."""
    with pytest.raises(CitationResolutionError) as exc:
        buoyant_force_cited(100.0, repo_root=tmp_path)
    assert exc.value.code_id == "EN400"
    assert exc.value.reason == "page_missing"


def test_buoyant_force_cited_standalone_graceful(monkeypatch):
    """Standalone mode (resolver unconfigured) warns once and emits no citation,
    while still returning the correct value."""
    from digitalmodel.naval_architecture import hydrostatics as hydro

    def _unconfigured(*_a, **_k):
        raise _CRE(code_id="EN400", wiki_path="wikis/...", reason="resolver_unconfigured:test")

    monkeypatch.setattr(hydro, "get_en400_reference", _unconfigured, raising=False)
    with pytest.warns(RuntimeWarning, match="standalone"):
        result = buoyant_force_cited(100.0)
    assert result["value"] == buoyant_force(100.0)
    assert result["citations"] == []
