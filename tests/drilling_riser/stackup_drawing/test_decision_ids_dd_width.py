"""Two-letter owner-decision IDs and the Design data column width (owner decision G08).

1. An owner-decision ID is one or two capital letters plus two digits
   (``K06``, ``DD04``, ``G07``). The provenance basis
   ``owner decision <ID> (<YYYY-MM-DD>)`` and the owner_decision note check
   accept both; three letters, one digit and the hyphenated design-data IDs
   (``D-04``) are not decision IDs.
2. A row with many design-data IDs stays inside the table: the Design data
   column is sized from its widest cell (never narrower than the original
   240 px) and the sheet width follows. The fixture row
   ``c10-riser-joint-buoyant`` carries eight IDs.
"""

from __future__ import annotations

import re
import xml.etree.ElementTree as ET

import pytest

from digitalmodel.drilling_riser.stackup_drawing import Provenance, reconcile

from .conftest import row_cell

NS = "{http://www.w3.org/2000/svg}"
EIGHT_ID_ROW = "c10-riser-joint-buoyant"

# -- 1. decision IDs ------------------------------------------------------------------


@pytest.mark.parametrize(
    "basis",
    [
        "owner decision DD04 (2026-09-24)",
        "owner decision G07 (2026-09-24)",
        "owner decision K06 (2026-09-24)",
    ],
)
def test_one_or_two_letter_decision_basis_is_accepted(basis):
    assert Provenance("somewhere", basis).basis == basis


@pytest.mark.parametrize(
    "basis",
    [
        "owner decision DDD04 (2026-09-24)",
        "owner decision D4 (2026-09-24)",
        "owner decision DD4 (2026-09-24)",
        "owner decision D-04 (2026-09-24)",
        "owner decision Dd04 (2026-09-24)",
        "owner decision DD004 (2026-09-24)",
    ],
)
def test_malformed_decision_id_in_basis_is_rejected(basis):
    with pytest.raises(ValueError, match="basis"):
        Provenance("somewhere", basis)


def _decision_problems(spec, note: str) -> list[str]:
    item = next(i for i in spec.design_data if i.id == "D-10")
    assert item.source_class == "owner_decision"
    item.note = note
    return [p for p in spec.validate() if "D-10" in p and "decision" in p]


@pytest.mark.parametrize(
    "note",
    [
        "Owner decision DD04 (2026-09-24): rating class.",
        "Follows owner decision G07.",
        "Owner decision K06: rating class.",
    ],
)
def test_owner_decision_note_naming_a_one_or_two_letter_id_passes(spec, note):
    assert _decision_problems(spec, note) == []


@pytest.mark.parametrize(
    "note",
    [
        "Follows owner decision DDD04.",
        "Follows owner decision D4.",
        "Rating as stated in design data item D-04.",
        "See D-04 and D-10 in the register.",
        "Chosen by the OWNER at the DESIGN review.",
    ],
)
def test_owner_decision_note_without_a_decision_id_fails(spec, note):
    assert _decision_problems(spec, note), note


# -- 2. Design data column width -------------------------------------------------------

#: Advance widths measured in headless Edge (system-ui -> Segoe UI) for the
#: drawing's stylesheet: ``.tdd`` 11 px and the class flags 8.5 px bold.
#: Independent of the renderer's own metric; 10 % margin added below.
_ADV_11 = {
    "D": 7.72,
    "-": 4.40,
    ",": 2.43,
    " ": 3.10,
    "n": 6.22,
    "/": 4.37,
    "a": 5.61,
    **{d: 5.98 for d in "0123456789"},
}
_ADV_FLAG = {"P": 5.22, "D": 6.26, "A": 6.02}
_MARGIN = 1.10


def _span_width(sp: ET.Element) -> float:
    text = sp.text or ""
    table = _ADV_FLAG if (sp.get("class") or "").startswith("ddc-") else _ADV_11
    return _MARGIN * sum(table.get(ch, 8.0) for ch in text)


def _table_right(root: ET.Element) -> float:
    header = next(g for g in root if g.get("data-role") == "table-header")
    rect = next(el for el in header if el.tag == NS + "rect")
    return float(rect.get("x")) + float(rect.get("width"))


def _dd_texts(root: ET.Element):
    for g in root:
        if g.get("data-role") != "table-row":
            continue
        for t in g.iter(NS + "text"):
            if t.get("data-col") == "dd":
                yield g, t


def test_fixture_row_carries_eight_design_data_ids(svg):
    line = row_cell(svg, EIGHT_ID_ROW, "dd")
    assert len(re.findall(r'<tspan data-dd="D-\d+">', line)) == 8


def test_every_design_data_tspan_is_inside_the_table_and_the_viewbox(spec, svg):
    root = ET.fromstring(svg)
    right = _table_right(root)
    vb_w = float(root.get("viewBox").split()[2])
    seen = 0
    for g, t in _dd_texts(root):
        x = float(t.get("x"))
        for sp in t.iter(NS + "tspan"):
            x += _span_width(sp)
            key = g.get("data-component-id") or g.get("data-datum") or g.get("data-row")
            assert x <= right, (key, sp.text, round(x, 1), right)
            assert x <= vb_w, (key, sp.text, round(x, 1), vb_w)
        seen += 1
    assert seen == len(spec.components) + 2
    assert reconcile(spec, svg)["result"] == "pass"


def test_design_data_column_grows_with_the_widest_cell(svg):
    root = ET.fromstring(svg)
    right = _table_right(root)
    # the eight-ID cell does not fit the original 240 px column (right edge 1510)
    assert right > 516.0 + 994.0
    assert float(root.get("width")) == pytest.approx(right + 14.0)


def test_design_data_column_keeps_its_minimum_width(spec):
    from digitalmodel.drilling_riser.stackup_drawing import render

    # every row back to at most six IDs: the column is the original 240 px
    c10 = spec.component(EIGHT_ID_ROW)
    c10.provenance["top_el_m"].design_data_id = "D-04"
    c10.provenance["bottom_el_m"].design_data_id = "D-04"
    spec.design_data = [i for i in spec.design_data if i.id not in ("D-29", "D-30")]
    d04 = next(i for i in spec.design_data if i.id == "D-04")
    for f, v in (("top_el_m", c10.top_el_m), ("bottom_el_m", c10.bottom_el_m)):
        d04.values[f"components.{EIGHT_ID_ROW}.{f}"] = {"value": v, "unit": "m"}
    svg = render(spec)
    root = ET.fromstring(svg)
    assert _table_right(root) == pytest.approx(516.0 + 994.0)
    assert reconcile(spec, svg)["result"] == "pass"


def test_design_data_text_pushed_past_the_right_edge_fails(spec, svg):
    line = row_cell(svg, EIGHT_ID_ROW, "dd")
    x = float(re.search(r' x="([\d.]+)"', line).group(1))
    new = line.replace(f' x="{x:g}"', f' x="{x + 120:g}"', 1)
    assert new != line
    report = reconcile(spec, svg.replace(line, new))
    assert report["result"] == "fail"
    failures = report["checks"]["b_positions"]["failures"]
    assert any(
        EIGHT_ID_ROW in f and "runs past the table's right edge" in f for f in failures
    ), failures[:8]
