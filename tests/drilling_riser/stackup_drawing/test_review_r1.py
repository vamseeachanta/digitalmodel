"""Codex review r1 of PR #2159 (#2158): one counterexample per finding.

Each test is a drawing or spec that is WRONG but passed the #2158 gate, and
asserts the check (or ``validate()`` rule) that must now reject it.
"""

from __future__ import annotations

import re
import xml.etree.ElementTree as ET

import pytest

from digitalmodel.drilling_riser.stackup_drawing import reconcile, render

NS = "{http://www.w3.org/2000/svg}"


def _assert_reason(report: dict, check: str, reason: str) -> None:
    statuses = {k: v["status"] for k, v in report["checks"].items()}
    assert report["result"] == "fail", f"result {report['result']!r}; {statuses}"
    failures = report["checks"][check]["failures"]
    assert any(
        reason in f for f in failures
    ), f"{check} lacks reason {reason!r}; failures: {failures[:8]}; {statuses}"


def _item(spec, item_id):
    return next(i for i in spec.design_data if i.id == item_id)


# -- finding 1: per-row register items must carry verifiable values ---------------------


def test_f1_composite_item_value_change_is_detected(spec):
    # D-12 backs the diverter OD; changing the field must disagree with the
    # item's explicit per-field value
    spec.component("c01-diverter").od_in = 22.0
    _assert_reason(reconcile(spec, render(spec)), "f_design_data", "D-12")


def test_f1_composite_item_values_are_per_field_with_units(spec):
    item = _item(spec, "D-12")
    entry = item.values["components.c01-diverter.od_in"]
    assert entry == {"value": 21.0, "unit": "in"}
    assert spec.validate() == []


def test_f1_item_without_a_verifiable_value_is_not_established(spec):
    item = _item(spec, "D-13")
    item.values = {}
    report = reconcile(spec, render(spec))
    f = report["checks"]["f_design_data"]
    assert f["status"] == "not_established", f
    assert any("D-13" in o and "no verifiable" in o for o in f["open_items"])
    assert report["result"] == "pass_with_open_items"


def test_f1_values_must_name_fields_the_item_backs(spec):
    _item(spec, "D-12").values["components.c09-riser-joint-buoyant.od_in"] = {
        "value": 21.0,
        "unit": "in",
    }
    assert any("D-12" in p and "does not back" in p for p in spec.validate())


# -- finding 2: a public item needs a source citation -------------------------------------


def test_f2_public_item_citing_only_context_fails_validation(spec):
    _item(spec, "D-06").reference_ids = ["R-4"]  # R-4 is a context reference
    assert any("D-06" in p and "source" in p for p in spec.validate())


def test_f2_citation_role_is_set_per_citation(spec):
    item = _item(spec, "D-06")
    item.reference_ids = [{"id": "R-1", "role": "context"}]
    assert any("D-06" in p and "source" in p for p in spec.validate())
    item.reference_ids = [{"id": "R-1", "role": "source"}, "R-4"]
    assert spec.validate() == []


def test_f2_assumed_item_citing_a_source_role_fails(spec):
    _item(spec, "D-19").reference_ids = [{"id": "R-4", "role": "source"}]
    assert any("D-19" in p and "context" in p for p in spec.validate())


# -- finding 3: reference URL and date are parsed ------------------------------------------


@pytest.mark.parametrize(
    "url",
    [
        "https://#",
        "https://localhost/x",
        "ftp://example.com/x",
        "https:///path",
        "example.com",
    ],
)
def test_f3_unusable_url_fails_validation(spec, url):
    spec.references[0].url = url
    assert any("R-1" in p and "url" in p for p in spec.validate())


@pytest.mark.parametrize("date", ["2026-99-99", "2026-02-30", "2026-9-24", "yesterday"])
def test_f3_impossible_retrieval_date_fails_validation(spec, date):
    spec.references[0].retrieved = date
    assert any("R-1" in p and "retrieved" in p for p in spec.validate())


def test_f3_retrieval_after_the_spec_date_fails_validation(spec):
    spec.as_of = "2026-01-01"
    assert any("R-1" in p and "after" in p for p in spec.validate())


# -- finding 4: review entries target a real row ------------------------------------------


def test_f4_entry_for_a_missing_tensioner_row_fails(spec):
    spec.tensioner_system = None
    spec.gaps.append(
        {
            "component_id": "tensioner_system",
            "item": "count",
            "status": "open",
            "detail": "x",
        }
    )
    assert any("tensioner_system" in p for p in spec.validate())
    _assert_reason(reconcile(spec, render(spec)), "d_totals", "tensioner_system")


def test_f4_drawing_level_entry_has_no_row_mark(spec, svg):
    spec.gaps.append(
        {"component_id": None, "item": "general", "status": "open", "detail": "x"}
    )
    drawn = render(spec)
    assert reconcile(spec, drawn)["result"] == "pass"
    root = ET.fromstring(drawn)
    marks = [
        el.get("data-review")
        for g in root
        if g.get("data-role") == "table-row"
        for el in g
        if el.get("class") == "warn"
    ]
    assert "gaps[1]" not in " ".join(marks)


# -- finding 5: review list classification and warning attachment ------------------------


def test_f5_swapped_classification_fails(spec, svg):
    old = "<tspan>Conflict</tspan>"
    assert svg.count(old) == 1
    tampered = svg.replace(old, "<tspan>Gap</tspan>")
    _assert_reason(reconcile(spec, tampered), "a_mapping", "classification")


def test_f5_warning_mark_moved_off_its_entry_fails(spec, svg):
    m = re.search(
        r'(<path d="M22,)([\d.]+)( l5\.5,-10 l5\.5,10 z" class="warn" '
        r'data-part="warn" data-review="gaps\[0\]"/>)',
        svg,
    )
    assert m, "review warn mark for gaps[0]"
    moved = f"{m.group(1)}{float(m.group(2)) - 15:.2f}{m.group(3)}"
    _assert_reason(
        reconcile(spec, svg.replace(m.group(0), moved)), "b_positions", "warning"
    )


def test_f5_entry_text_must_equal_the_formatted_entry(spec, svg):
    tampered = svg.replace("<tspan>: </tspan>", "<tspan> — </tspan>", 1)
    assert tampered != svg
    _assert_reason(reconcile(spec, tampered), "a_mapping", "entry text")


# -- finding 6: archive rule -----------------------------------------------------------------


@pytest.mark.parametrize(
    "text",
    [
        "cal-0123",
        "Cal-77 rev B",
        "'Input Data'!B12",
        "Data!B12",
        "Sheet1!$A$1",
        "Data!A1:C7",
        "see 'Stack up'!$B$4:$D$9",
    ],
)
def test_f6_archive_citations_fail(spec, text):
    spec.gaps[0]["detail"] = f"value from {text} to check"
    _assert_reason(reconcile(spec, render(spec)), "f_design_data", "archive")


@pytest.mark.parametrize(
    "text", ["Confirm!", "Note! check the wall", "Data! later", "calibrate-1"]
)
def test_f6_ordinary_punctuation_passes(spec, text):
    spec.gaps[0]["detail"] = text
    assert reconcile(spec, render(spec))["result"] == "pass"
