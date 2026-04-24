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


def _repo_root() -> Path:
    here = Path(__file__).resolve()
    for parent in here.parents:
        if (parent / "knowledge" / "wikis").is_dir():
            return parent
    raise RuntimeError("could not locate workspace-hub root above test file")


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
