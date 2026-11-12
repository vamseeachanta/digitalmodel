"""Tests for the standard HSE and safety report appendix."""

from __future__ import annotations

import json
from html.parser import HTMLParser

import pytest

from digitalmodel.reporting.hse_section import (
    HSE_BLOCKS,
    check_consistency,
    check_sidecar,
    hse_content,
    hse_section_spec,
    load_hse_appendix,
    markdown_to_html,
    with_hse_section,
)
from digitalmodel.reporting.skeleton import ReportSkeleton

HEADINGS = [h for _, h in HSE_BLOCKS.values()]


def _markdown(skip: int = 0) -> str:
    parts = ["## Appendix F HSE and safety", "", "Opening sentence.", ""]
    for n, heading in enumerate(HEADINGS, start=1):
        if n == skip:
            continue
        parts += [f"### F.{n} {heading}", ""]
        if n == 1:
            parts += ["| Source | Records |", "|---|---|", "| Synthetic set | 7 |", ""]
        elif n == 6:
            parts += [
                "- [H1] Synthetic set. https://example.org/a.zip (read 2026-01-31).",
                "",
            ]
        else:
            parts += [f"Text of **part {n}** with <angle> brackets.", ""]
    return "\n".join(parts)


def _sidecar(**over) -> dict:
    base = {
        "equipment_class": "mooring_line",
        "sources": [
            {
                "source_id": "synthetic",
                "url": "https://example.org/a.zip",
                "retrieved": "2026-01-31",
            }
        ],
        "references": ["Synthetic set. https://example.org/a.zip (read 2026-01-31)."],
        "lessons": [{"key": "case#1", "addressed_in": "Section 6.3"}],
        "open_items": [],
    }
    base.update(over)
    return base


def _skeleton() -> ReportSkeleton:
    return ReportSkeleton.model_validate(
        {
            "title": "Mooring analysis",
            "require_provenance": False,
            "sections": [
                {
                    "key": "summary",
                    "label": "Summary",
                    "blocks": [{"key": "scope", "label": "Scope"}],
                }
            ],
        }
    )


def test_section_has_six_required_slots_with_fixed_headings() -> None:
    spec = hse_section_spec()
    assert spec.label == "Appendix F HSE and safety"
    assert [b.label for b in spec.blocks] == [
        f"F.{n} {h}" for n, h in enumerate(HEADINGS, start=1)
    ]
    assert all(b.required for b in spec.blocks)
    assert hse_section_spec("G").blocks[0].label == "G.1 Data used"


def test_with_hse_section_appends_once_and_report_is_incomplete_without_it() -> None:
    skeleton = with_hse_section(_skeleton())
    assert skeleton.sections[-1].key == "hse_safety"
    assert with_hse_section(skeleton) is skeleton
    status = skeleton.completeness({"scope": "<p>x</p>"})
    assert not status.complete and len(status.missing_blocks) == 6
    content = {"scope": "<p>x</p>", **hse_content(_markdown())}
    assert skeleton.completeness(content).complete
    assert "Appendix F HSE and safety" in skeleton.build_html(content)


def test_content_is_split_by_subsection_and_escaped() -> None:
    content = hse_content(_markdown())
    assert list(content) == list(HSE_BLOCKS)
    assert content["hse_data_used"].startswith("<h3>F.1 Data used</h3>")
    assert content["hse_references"].startswith(
        "<h3>F.6 References for this appendix</h3>"
    )
    assert "<td>Synthetic set</td><td>7</td>" in content["hse_data_used"]
    assert (
        "<strong>part 2</strong> with &lt;angle&gt; brackets"
        in content["hse_incident_history"]
    )
    assert '<a href="https://example.org/a.zip">' in content["hse_references"]


def test_missing_or_renamed_subsection_is_refused() -> None:
    with pytest.raises(ValueError, match="subsection 4"):
        hse_content(_markdown(skip=4))
    with pytest.raises(ValueError, match="not 'Incident history'"):
        hse_content(_markdown().replace("F.2 Incident history", "F.2 History"))


@pytest.mark.parametrize(
    "change,expected",
    [
        ({"sources": []}, "no data source listed"),
        (
            {
                "sources": [
                    {"source_id": "s", "url": "http://x", "retrieved": "2026-01-31"}
                ]
            },
            "no public link",
        ),
        (
            {
                "sources": [
                    {"source_id": "s", "url": "https://x", "retrieved": "Jan 2026"}
                ]
            },
            "no date read",
        ),
        ({"references": []}, "no references"),
        ({"lessons": [{"key": "c#1", "addressed_in": ""}]}, "no design response"),
        ({"open_items": ["lesson c#1 has no design response"]}, "open item"),
        ({"equipment_class": ""}, "no equipment class"),
    ],
)
def test_sidecar_checks(change, expected) -> None:
    assert any(expected in p for p in check_sidecar(_sidecar(**change)))


def test_ready_sidecar_has_no_problems_and_open_items_can_be_allowed() -> None:
    assert check_sidecar(_sidecar()) == []
    assert check_sidecar(_sidecar(open_items=["x"]), allow_open=True) == []


def test_load_appendix_reads_the_pair_of_files(tmp_path) -> None:
    md = tmp_path / "appendix.md"
    md.write_text(_markdown(), encoding="utf-8")
    md.with_suffix(".json").write_text(json.dumps(_sidecar()), encoding="utf-8")
    assert set(load_hse_appendix(md)) == set(HSE_BLOCKS)
    md.with_suffix(".json").write_text(
        json.dumps(_sidecar(open_items=["x"])), encoding="utf-8"
    )
    with pytest.raises(ValueError, match="not ready"):
        load_hse_appendix(md)
    assert load_hse_appendix(md, allow_open=True)


def test_markdown_subset() -> None:
    assert markdown_to_html("- a\n- b") == "<ul><li>a</li><li>b</li></ul>"
    assert markdown_to_html("<script>") == "<p>&lt;script&gt;</p>"


class _Anchors(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.attrs: list[dict] = []

    def handle_starttag(self, tag, attrs):
        if tag == "a":
            self.attrs.append(dict(attrs))


@pytest.mark.parametrize(
    "text",
    [
        'https://example.org/a"onmouseover="alert(1) x',
        "https://example.org/a'onmouseover='alert(1) x",
        'see https://example.org/a"><script>alert(1)</script>',
        "https://example.org/a<b onmouseover=x>",
    ],
)
def test_link_text_cannot_add_attributes_or_tags(text) -> None:
    out = markdown_to_html(text)
    parser = _Anchors()
    parser.feed(out)
    assert parser.attrs == [{"href": "https://example.org/a"}]
    assert "<script" not in out and "<b " not in out


def test_link_keeps_query_and_drops_trailing_full_stop() -> None:
    out = markdown_to_html("Read https://example.org/a?x=1&y=2 (read 2026-01-31).")
    parser = _Anchors()
    parser.feed(out)
    assert parser.attrs == [{"href": "https://example.org/a?x=1&y=2"}]
    assert out.endswith("(read 2026-01-31).</p>")


@pytest.mark.parametrize(
    "response", ["", "   ", None, 7, "not yet assigned", " Not Yet Assigned "]
)
def test_unanswered_lesson_blocks_issue_but_not_a_draft(response) -> None:
    sidecar = _sidecar(lessons=[{"key": "c#1", "addressed_in": response}])
    assert check_sidecar(sidecar) == ["lesson c#1: no design response"]
    assert check_sidecar(sidecar, allow_open=True) == []


@pytest.mark.parametrize(
    "value", ["2026-02-30", "2026-13-01", "2026-1-31", "20260131", None, 20260131]
)
def test_impossible_or_malformed_date_read_is_refused(value) -> None:
    src = {"source_id": "s", "url": "https://example.org/a.zip", "retrieved": value}
    assert check_sidecar(_sidecar(sources=[src])) == ["source s: no date read"]


def test_text_must_print_each_source_and_reference() -> None:
    assert check_consistency(_markdown(), _sidecar()) == []
    stripped = _markdown().replace("(read 2026-01-31)", "")
    problems = check_consistency(stripped, _sidecar())
    assert "source synthetic: its link and date read are not in the text" in problems
    assert "a reference in the sidecar is not in the text" in problems


def test_load_refuses_text_that_lost_its_citation(tmp_path) -> None:
    md = tmp_path / "appendix.md"
    md.write_text(
        _markdown().replace("https://example.org/a.zip", "the data set"),
        encoding="utf-8",
    )
    md.with_suffix(".json").write_text(json.dumps(_sidecar()), encoding="utf-8")
    with pytest.raises(ValueError, match="not in the text"):
        load_hse_appendix(md)
