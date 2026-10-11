"""Offline report citation regressions for the edition-routing defect.

Inputs and numerical values are reused from workflow_inputs/jacket.yml and
workflow_inputs/riser_base_phased.yml. Edition metadata is pinned by the
existing citations/fixtures frontmatter and its FIXTURE_PROVENANCE.md;
inspected 2026-10-11. No licensed standard content is reproduced here.
"""

from __future__ import annotations

import warnings
from copy import deepcopy
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.cathodic_protection.report_adapters import anode_design_report
from digitalmodel.citations import Citation, validate_citation


TESTS = Path(__file__).resolve().parents[1]
INPUTS = TESTS / "fixtures/cathodic_protection/workflow_inputs"
CITATION_FIXTURES = TESTS / "citations/fixtures"
PAGE = "wikis/engineering-standards/wiki/standards/dnv-rp-b401-2021.md"


def _run(name: str) -> dict[str, Any]:
    cfg = yaml.safe_load((INPUTS / f"{name}.yml").read_text(encoding="utf-8"))
    cfg["citation_repo_root"] = str(CITATION_FIXTURES)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", DeprecationWarning)
        return run_cathodic_protection(cfg)


def _frontmatter() -> dict[str, str]:
    text = (CITATION_FIXTURES / "knowledge" / PAGE).read_text(encoding="utf-8")
    return yaml.safe_load(text.split("---", 2)[1])


def _assert_edition_citations(cfg: dict[str, Any]) -> None:
    before = deepcopy(cfg)
    spec = anode_design_report(cfg)
    assert cfg == before, "report construction must preserve caller results"
    metadata = _frontmatter()
    assert spec.citations, "the report must retain calculation citations"
    for record in spec.citations:
        citation = Citation(**record)
        validate_citation(citation, repo_root=CITATION_FIXTURES)
        assert citation.wiki_path == PAGE
        for field in ("code_id", "publisher", "revision"):
            assert getattr(citation, field) == metadata[field]
    original_sections = {
        label.split(" ", 2)[2] for label in cfg["results"]["citations"]
    }
    assert {record["section"] for record in spec.citations} == original_sections
    assert len(spec.citations) == len(original_sections)


def test_jacket_report_citations_match_edition_frontmatter() -> None:
    _assert_edition_citations(_run("jacket"))


def test_phased_report_citations_match_edition_frontmatter() -> None:
    _assert_edition_citations(_run("riser_base_phased"))


def test_short_and_canonical_phase_labels_emit_one_citation_per_section() -> None:
    cfg = _run("riser_base_phased")
    short_revision = cfg["inputs"]["design_data"]["edition"]
    short_label = next(
        label
        for label in cfg["results"]["citations"]
        if label.split(" ", 2)[1] == short_revision
    )
    code, _, section = short_label.split(" ", 2)
    cfg["results"]["citations"].append(f"{code} {_frontmatter()['revision']} {section}")
    _assert_edition_citations(cfg)
