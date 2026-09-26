# ABOUTME: Contract tests for the standard report spec (#2212).
# ABOUTME: Document numbering, block shape rules, citations, manifest, provenance.
"""Tests for :mod:`digitalmodel.reporting.spec`."""

from __future__ import annotations

import pytest
from pydantic import ValidationError

from digitalmodel.citations.schema import Citation
from digitalmodel.reporting import (
    DOC_NUMBER_RE,
    MANIFEST_REQUIRED_FIELDS,
    DocumentMeta,
    FigureBlock,
    Provenance,
    ReportSpec,
    Section,
    StandardLabel,
    StatusBlock,
    TableBlock,
    TextBlock,
)
from digitalmodel.report_pack import workflow as report_pack_workflow

WIKI = "wikis/marine-engineering/wiki/standards/dnv-rp-b401.md"


def _doc(**overrides):
    base = dict(
        number="B0000-RPT-001-00",
        revision="00",
        title="Test report",
        project="B0000",
        client="Example Client",
        prepared_by="AP",
        checked_by="CP",
        approved_by="XP",
    )
    base.update(overrides)
    return DocumentMeta(**base)


def _section(key="s1", blocks=None):
    return Section(key=key, title="Section", blocks=blocks or [TextBlock(markdown="x")])


def _spec(**overrides):
    base = dict(
        document=_doc(),
        sections=[_section()],
        provenance=Provenance().add("file", "inputs.yml"),
    )
    base.update(overrides)
    return ReportSpec(**base)


# --- document control ------------------------------------------------------


def test_document_number_regex_is_shared_with_report_pack():
    assert report_pack_workflow.DOC_NUMBER_RE is DOC_NUMBER_RE
    assert report_pack_workflow.MANIFEST_REQUIRED_FIELDS is MANIFEST_REQUIRED_FIELDS


@pytest.mark.parametrize("number", ["B0000-RPT-001-00", "B0000-001-00", "A1234-X-999-07"])
def test_document_number_accepted(number):
    rev = number[-2:]
    assert _doc(number=number, revision=rev).number == number


@pytest.mark.parametrize("number", ["b0000-rpt-001-00", "B0000-RPT-1-00", "REPORT-1", ""])
def test_document_number_rejected(number):
    with pytest.raises(ValidationError, match="document.number"):
        _doc(number=number)


def test_document_revision_must_match_number_suffix():
    with pytest.raises(ValidationError, match="revision suffix must match"):
        _doc(number="B0000-RPT-001-01", revision="00")


def test_document_default_revision_history_from_meta():
    doc = _doc(date="2026-09-25")
    assert [r.model_dump() for r in doc.revision_history] == [
        {
            "rev": "00",
            "date": "2026-09-25",
            "description": "Issued",
            "prepared": "AP",
            "checked": "CP",
            "approved": "XP",
        }
    ]


def test_document_rejects_unknown_fields():
    with pytest.raises(ValidationError):
        _doc(generated_at="now")


# --- blocks ----------------------------------------------------------------


def test_status_fail_requires_governing_case():
    with pytest.raises(ValidationError, match="FAIL requires governing_case"):
        StatusBlock(label="Mass", status="FAIL")
    ok = StatusBlock(label="Mass", status="FAIL", governing_case="Final, zone 2")
    assert ok.governing_case == "Final, zone 2"
    assert StatusBlock(label="Mass", status="PASS").governing_case is None


def test_status_only_pass_or_fail():
    with pytest.raises(ValidationError):
        StatusBlock(label="Mass", status="WARN")


def test_table_units_and_rows_match_columns():
    with pytest.raises(ValidationError, match="units has 1 entries for 2 columns"):
        TableBlock(title="t", columns=["a", "b"], units=["-"], rows=[])
    with pytest.raises(ValidationError, match="row 0 has 3 cells"):
        TableBlock(title="t", columns=["a", "b"], rows=[[1, 2, 3]])
    table = TableBlock(title="t", columns=["a", "b"], units=["m", "s"], rows=[[1, 2]])
    assert table.kind == "table"


def test_figure_needs_a_payload_and_html_safe_id():
    with pytest.raises(ValidationError, match="needs one of plotly, svg or image_path"):
        FigureBlock(title="f", figure_id="fig-1")
    with pytest.raises(ValidationError, match="figure_id must be an HTML id"):
        FigureBlock(title="f", figure_id="1 fig", svg="<svg/>")
    fig = FigureBlock(title="f", figure_id="fig_1", plotly={"data": [], "layout": {}})
    assert fig.kind == "figure"


def test_section_requires_blocks_and_safe_key():
    with pytest.raises(ValidationError, match="blocks must not be empty"):
        Section(key="s1", title="S", blocks=[])
    with pytest.raises(ValidationError, match="Section.key"):
        Section(key="1 s", title="S", blocks=[TextBlock(markdown="x")])


def test_blocks_round_trip_through_dicts_by_kind():
    spec = ReportSpec.model_validate(
        {
            "document": _doc().model_dump(),
            "sections": [
                {
                    "key": "s1",
                    "title": "S",
                    "blocks": [
                        {"kind": "text", "markdown": "hi"},
                        {"kind": "status", "label": "L", "status": "PASS"},
                    ],
                }
            ],
        }
    )
    kinds = [type(b).__name__ for b in spec.sections[0].blocks]
    assert kinds == ["TextBlock", "StatusBlock"]


# --- spec-level rules -------------------------------------------------------


def test_citations_accept_dataclass_and_store_dicts():
    citation = Citation(
        code_id="DNV-RP-B401",
        publisher="DNV",
        revision="2021",
        section="Table 10-1",
        wiki_path=WIKI,
        note="coating breakdown",
    )
    spec = _spec(citations=[citation, {"code_id": "ISO 15589-2", "publisher": "ISO",
                                       "revision": "2024", "section": "7", "wiki_path": WIKI}])
    assert all(isinstance(c, dict) for c in spec.citations)
    assert spec.citations[0]["code_id"] == "DNV-RP-B401"
    assert spec.citations[0]["source_sibling"] == "generic"
    assert spec.citations[1]["note"] == ""


def test_citations_validated_through_citation_schema():
    with pytest.raises(ValidationError, match=r"citations\[0\] invalid"):
        _spec(citations=[{"code_id": "X", "publisher": "P", "revision": "1",
                          "section": "s", "wiki_path": "not/under/wikis.md"}])


def test_manifest_required_fields_enforced_when_supplied():
    assert _spec(manifest={}).manifest == {}
    full = {
        "issue": "https://github.com/example/repo/issues/1",
        "project": "B0000",
        "artifact_class": "test",
        "privacy_classification": "test",
        "publishability_decision": "test only",
        "input_source_ids": ["SRC-1"],
        "raw_output_path": "repo:tmp/raw",
        "final_output_path": "repo:tmp/final",
    }
    assert _spec(manifest=full).manifest == full
    partial = {k: v for k, v in full.items() if k != "issue"}
    with pytest.raises(ValidationError, match="missing required field.*issue"):
        _spec(manifest=partial)
    with pytest.raises(ValidationError, match="input_source_ids"):
        _spec(manifest={**full, "input_source_ids": [""]})


def test_duplicate_section_keys_and_figure_ids_rejected():
    with pytest.raises(ValidationError, match="duplicate section keys: s1"):
        _spec(sections=[_section("s1"), _section("s1")])
    fig = FigureBlock(title="f", figure_id="fig-1", svg="<svg/>")
    with pytest.raises(ValidationError, match="duplicate figure_ids: fig-1"):
        _spec(sections=[_section("s1", [fig])], appendices=[_section("a", [fig])])


def test_has_plotly_and_figure_blocks():
    svg = FigureBlock(title="f", figure_id="fig-svg", svg="<svg/>")
    plotly = FigureBlock(title="g", figure_id="fig-p", plotly={"data": []})
    assert _spec(sections=[_section("s1", [svg])]).has_plotly() is False
    spec = _spec(sections=[_section("s1", [svg])], appendices=[_section("a", [plotly])])
    assert spec.has_plotly() is True
    assert [f.figure_id for f in spec.figure_blocks()] == ["fig-svg", "fig-p"]


def test_standard_label_fields_non_empty():
    with pytest.raises(ValidationError):
        StandardLabel(code_id="DNV-RP-B401", edition=" ", provenance="wiki")


def test_spec_never_reads_the_clock():
    """Two specs built from the same inputs dump identically (no timestamps)."""
    assert _spec().model_dump() == _spec().model_dump()
