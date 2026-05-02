"""Schema + resolution tests for digitalmodel.citations.

Covers #2481 TDD list items: schema validation, wiki-path security,
fail-closed resolution with code_id in the error message.
"""
from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.citations import (
    Citation,
    CitedValue,
    CitationResolutionError,
    validate_citation,
)
from digitalmodel.citations.schema import CitationValidationError


REAL_DNV_PAGE = "knowledge/wikis/engineering/wiki/standards/dnv-os-e301.md"


def _repo_root() -> Path:
    # Option B (workspace-hub #2580): vendor wiki fixtures into the test tree so
    # the citation resolver works regardless of how digitalmodel is checked out
    # (workspace-hub overlay vs. standalone CI clone). The fixture tree mirrors
    # the resolver's expected `knowledge/wikis/...` subpath bit-for-bit.
    return Path(__file__).resolve().parent / "fixtures"


def _valid_kwargs(**overrides):
    base = dict(
        code_id="DNV-OS-E301",
        publisher="DNV",
        revision="2021-07",
        section="Section 2.2.3",
        wiki_path=REAL_DNV_PAGE,
        note="",
    )
    base.update(overrides)
    return base


def test_schema_accepts_well_formed_citation():
    c = Citation(**_valid_kwargs())
    assert c.code_id == "DNV-OS-E301"


def test_schema_rejects_missing_code_id():
    with pytest.raises(CitationValidationError):
        Citation(**_valid_kwargs(code_id=""))


def test_schema_rejects_missing_publisher():
    with pytest.raises(CitationValidationError):
        Citation(**_valid_kwargs(publisher=""))


def test_schema_rejects_wiki_path_outside_wiki_tree():
    with pytest.raises(CitationValidationError):
        Citation(**_valid_kwargs(wiki_path="../../etc/passwd"))


def test_schema_rejects_wiki_path_absolute():
    with pytest.raises(CitationValidationError):
        Citation(**_valid_kwargs(wiki_path="/etc/passwd"))


def test_schema_rejects_wiki_path_with_backslash():
    with pytest.raises(CitationValidationError):
        Citation(**_valid_kwargs(wiki_path="knowledge\\wikis\\engineering.md"))


def test_resolution_passes_for_real_page():
    c = Citation(**_valid_kwargs())
    validate_citation(c, repo_root=_repo_root())


def test_resolution_fails_on_missing_page():
    c = Citation(**_valid_kwargs(wiki_path="knowledge/wikis/engineering/wiki/standards/does-not-exist.md"))
    with pytest.raises(CitationResolutionError) as exc:
        validate_citation(c, repo_root=_repo_root())
    assert "DNV-OS-E301" in str(exc.value)
    assert exc.value.code_id == "DNV-OS-E301"
    assert exc.value.reason == "page_missing"


def test_resolution_fails_on_frontmatter_mismatch_revision():
    c = Citation(**_valid_kwargs(revision="2099-99"))
    with pytest.raises(CitationResolutionError) as exc:
        validate_citation(c, repo_root=_repo_root())
    assert "DNV-OS-E301" in str(exc.value)
    assert "frontmatter_mismatch:revision" in exc.value.reason


def test_cited_value_preserves_numeric_interface():
    c = Citation(**_valid_kwargs())
    v = CitedValue(value=1.67, citation=c, units="dimensionless")
    assert v.value == 1.67
    assert v.units == "dimensionless"
