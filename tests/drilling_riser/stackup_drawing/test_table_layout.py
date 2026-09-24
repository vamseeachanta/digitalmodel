"""Tabular callouts, review list, nesting and design-data references (#2158).

Owner decisions: L10 (value provenance on the drawing, round-ft axis
ticks), L11 = A (aligned table rows are the only callout layout), L12
(review list keyed by ``component_id``), K10/K05 (``nested_in`` overlay and
landing, provenance bases and space-out reference totals), and the
2026-09-24 instruction that every drawn value references the report's
design-data table (D-IDs with a P/D/A class flag), which replaces the
report-table † and owner-decision ‡ markers.

Every tamper test changes one thing in a correct drawing and asserts the
check that fails AND its reason text.
"""

from __future__ import annotations

import re
import xml.etree.ElementTree as ET

import pytest

from digitalmodel.drilling_riser.stackup_drawing import (
    Provenance,
    ReferenceValue,
    StackupDrawingSpec,
    from_json,
    reconcile,
    render,
    to_json,
)

NS = "{http://www.w3.org/2000/svg}"
CHECKS = (
    "a_mapping",
    "b_positions",
    "c_numbers",
    "d_totals",
    "e_not_found",
    "f_design_data",
)


def _lines(svg: str) -> list[str]:
    return svg.split("\n")


def _statuses(report: dict) -> dict:
    return {k: v["status"] for k, v in report["checks"].items()}


def _assert_reason(report: dict, check: str, reason: str) -> None:
    statuses = _statuses(report)
    assert report["result"] == "fail", f"result {report['result']!r}; {statuses}"
    failures = report["checks"][check]["failures"]
    assert any(reason in f for f in failures), (
        f"{check} lacks reason {reason!r}; {check} failures: {failures[:8]}; "
        f"statuses: {statuses}"
    )


def _row_head(cid: str) -> str:
    return f'<g data-role="table-row" data-component-id="{cid}"'


def _row_span(lines: list[str], head: str) -> tuple[int, int]:
    start = next(i for i, ln in enumerate(lines) if ln.startswith(head))
    end = next(i for i in range(start, len(lines)) if lines[i] == "</g>")
    return start, end


def _row_line(svg: str, cid: str, needle: str) -> str:
    lines = _lines(svg)
    start, end = _row_span(lines, _row_head(cid))
    hits = [ln for ln in lines[start:end] if needle in ln]
    assert len(hits) == 1, (cid, needle, hits)
    return hits[0]


def _cell(svg: str, cid: str, col: str) -> str:
    """The cell text of column ``col`` in row ``cid``."""
    lines = _lines(svg)
    start, end = _row_span(lines, _row_head(cid))
    hits = [ln for ln in lines[start:end] if f'data-col="{col}"' in ln]
    assert len(hits) == 1, (cid, col, hits)
    return hits[0]


def _drop_line(svg: str, line: str) -> str:
    lines = _lines(svg)
    assert lines.count(line) == 1, line
    lines.remove(line)
    return "\n".join(lines)


def _swap(svg: str, old: str, new: str) -> str:
    assert svg.count(old) == 1, (old, svg.count(old))
    return svg.replace(old, new)


# -- positive: the table is the only callout layout ---------------------------------------


def test_synthetic_fixture_renders_as_table_and_reconciles_pass(spec, svg):
    report = reconcile(spec, svg)

    assert report["result"] == "pass", {
        k: v["failures"][:5] for k, v in report["checks"].items()
    }
    assert tuple(report["checks"]) == CHECKS
    for check in CHECKS:
        assert report["checks"][check]["status"] == "pass", check
    root = ET.fromstring(svg)
    roles = [g.get("data-role") for g in root if g.tag == NS + "g"]
    assert "callout" not in roles and "datum-label" not in roles
    rows = [g for g in root if g.get("data-role") == "table-row"]
    keys = sorted(
        g.get("data-component-id") or g.get("data-datum") or g.get("data-row")
        for g in rows
    )
    expected = sorted(
        [c.id for c in spec.components] + ["drill_floor_el_m", "tensioner_system"]
    )
    assert keys == expected
    assert roles.count("table-header") == 1
    assert roles.count("table-legend") == 1
    assert roles.count("review-list") == 1


def test_sheet_width_is_derived_from_the_table_columns(svg):
    root = ET.fromstring(svg)
    header = next(g for g in root if g.get("data-role") == "table-header")
    rect = next(el for el in header if el.tag == NS + "rect")
    right = float(rect.get("x")) + float(rect.get("width"))
    assert float(root.get("width")) == pytest.approx(right + 14.0)
    assert root.get("viewBox").split()[2] == root.get("width")


def test_numbers_in_cells_use_the_header_units(spec, svg):
    header = next(
        ln for ln in _lines(svg) if ln.startswith('<g data-role="table-header"')
    )
    assert header
    for unit in ("no. × ft", "m (ft)", "OD in / rating ft"):
        assert f">{unit}<" in svg
    top = _cell(svg, "c06-pup-joint", "top")
    assert 'text-anchor="end"' in top
    assert 'data-unit="m"' in top


# -- cell consistency tampers -------------------------------------------------------------


def test_cell_changed_value_fails_numbers(spec, svg):
    line = _cell(svg, "c06-pup-joint", "top")
    tampered = svg.replace(line, _swap(line, ">−12.46<", ">−12.56<"))
    _assert_reason(reconcile(spec, tampered), "c_numbers", "top_el_m")


def test_cell_na_swapped_for_not_applicable_dash_fails(spec, svg):
    line = _cell(svg, "c02-upper-flex-joint", "od")
    new = _swap(
        line,
        '<tspan class="na" data-field="od_in">n/a</tspan>',
        '<tspan class="nap">–</tspan>',
    )
    _assert_reason(reconcile(spec, svg.replace(line, new)), "e_not_found", "cell")


def test_cell_dash_swapped_for_na_fails(spec, svg):
    line = _cell(svg, "c06-pup-joint", "buoy")
    new = _swap(
        line,
        '<tspan class="nap">–</tspan>',
        '<tspan class="na" data-field="buoyancy_od_in">n/a</tspan>',
    )
    _assert_reason(reconcile(spec, svg.replace(line, new)), "e_not_found", "cell")


def test_cell_dropped_design_data_id_fails(spec, svg):
    line = _cell(svg, "c09-riser-joint-buoyant", "dd")
    new = re.sub(
        r'<tspan>, </tspan><tspan data-dd="D-10">D-10</tspan><tspan class="[^"]+">D</tspan>',
        "",
        line,
    )
    assert new != line
    _assert_reason(reconcile(spec, svg.replace(line, new)), "f_design_data", "D-10")


def test_cell_missing_warning_mark_fails(spec, svg):
    line = _row_line(svg, "c02-upper-flex-joint", 'class="warn"')
    _assert_reason(reconcile(spec, _drop_line(svg, line)), "a_mapping", "warning mark")


def test_cell_swapped_data_field_fails(spec, svg):
    # the top cell shows the bottom elevation under its own field name
    line = _cell(svg, "c06-pup-joint", "top")
    new = re.sub(
        r'data-field="top_el_m"([^>]*)>−12\.46<',
        r'data-field="bottom_el_m"\g<1>>−21.60<',
        line,
    )
    assert new != line
    _assert_reason(reconcile(spec, svg.replace(line, new)), "c_numbers", "cell")


def _shift_row(svg: str, cid: str, dy: float) -> str:
    """Move a whole row by ``dy`` px: background, rule, texts, warn and leader end."""
    lines = _lines(svg)
    start, end = _row_span(lines, _row_head(cid))

    def sh(m):
        return f'{m.group(1)}="{float(m.group(2)) + dy:.2f}"'

    for i in range(start + 1, end):
        ln = lines[i]
        if ln.startswith("<polyline"):
            pts = re.search(r'points="([^"]*)"', ln).group(1).split()
            moved = pts[:2] + [
                f"{p.split(',')[0]},{float(p.split(',')[1]) + dy:.2f}" for p in pts[2:]
            ]
            lines[i] = ln.replace(" ".join(pts), " ".join(moved))
        elif ln.startswith("<circle"):
            continue  # the anchor dot stays on the component
        elif ln.startswith("<path"):
            lines[i] = re.sub(
                r'd="M([\d.]+),([\d.]+)',
                lambda m: f'd="M{m.group(1)},{float(m.group(2)) + dy:.2f}',
                ln,
            )
        else:
            lines[i] = re.sub(r"\b(y|y1|y2)=\"([\d.]+)\"", sh, ln)
    return "\n".join(lines)


def test_row_moved_far_from_its_anchor_fails(spec, svg):
    tampered = _shift_row(svg, "c18-conductor", 400.0)
    _assert_reason(reconcile(spec, tampered), "b_positions", "row offset")


def test_leader_not_ending_at_the_row_fails(spec, svg):
    line = _row_line(svg, "c06-pup-joint", "<polyline")
    pts = re.search(r'points="([^"]*)"', line).group(1).split()
    x, y = pts[-1].split(",")
    pts[-1] = f"{x},{float(y) + 6:.2f}"
    new = re.sub(r'points="[^"]*"', f'points="{" ".join(pts)}"', line)
    _assert_reason(reconcile(spec, svg.replace(line, new)), "b_positions", "leader")


# -- design data (owner instruction 2026-09-24) and round-ft ticks ------------------------


def _dd_ids(line: str) -> list[str]:
    return re.findall(r'<tspan data-dd="(D-\d+)">', line)


def test_design_data_column_lists_the_row_ids_with_class_flags(spec, svg):
    line = _cell(svg, "c09-riser-joint-buoyant", "dd")
    assert _dd_ids(line) == ["D-04", "D-05", "D-06", "D-07", "D-08", "D-10"]
    assert re.findall(r'<tspan class="ddc-([pda])">', line) == [
        "d",
        "p",
        "p",
        "p",
        "p",
        "d",
    ]
    lines = _lines(svg)
    start, end = _row_span(lines, '<g data-role="table-row" data-row="tensioner_system"')
    tens = next(ln for ln in lines[start:end] if 'data-col="dd"' in ln)
    assert _dd_ids(tens) == ["D-16", "D-17", "D-18"]
    assert 'class="ddc-a">A<' in tens
    # the legend refers the IDs to the report's design-data table
    assert "Design data" in svg


def test_drawing_never_prints_provenance_source_or_basis(spec, svg):
    assert "synthetic fixture" not in svg
    assert "data-source=" not in svg
    assert "owner decision" not in svg
    assert "report table" not in svg


def test_printed_design_data_id_that_does_not_exist_fails(spec, svg):
    line = _cell(svg, "c09-riser-joint-buoyant", "dd")
    new = line.replace('data-dd="D-08">D-08<', 'data-dd="D-98">D-98<')
    assert new != line
    _assert_reason(reconcile(spec, svg.replace(line, new)), "f_design_data", "D-98")


def test_wrong_design_data_class_flag_fails(spec, svg):
    line = _cell(svg, "c09-riser-joint-buoyant", "dd")
    new = line.replace(
        '<tspan data-dd="D-10">D-10</tspan><tspan class="ddc-d">D</tspan>',
        '<tspan data-dd="D-10">D-10</tspan><tspan class="ddc-p">P</tspan>',
    )
    assert new != line
    _assert_reason(reconcile(spec, svg.replace(line, new)), "f_design_data", "class")


def test_register_value_disagreeing_with_its_field_fails(spec):
    item = next(i for i in spec.design_data if i.id == "D-06")
    item.value = 22.0  # the riser main tube OD fields are 21 in
    _assert_reason(reconcile(spec, render(spec)), "f_design_data", "D-06")


def test_register_value_within_the_formatting_tolerance_passes(spec):
    item = next(i for i in spec.design_data if i.id == "D-05")
    item.value = 75.004  # joint length prints to 0.1 ft at worst
    assert reconcile(spec, render(spec))["result"] == "pass"


def test_printed_value_without_design_data_id_fails(spec):
    spec.component("c06-pup-joint").provenance["od_in"].design_data_id = None
    _assert_reason(reconcile(spec, render(spec)), "f_design_data", "design_data_id")


def test_spec_without_a_register_is_not_established(spec):
    spec.design_data = []
    spec.references = []
    for comp in spec.components:
        for p in comp.provenance.values():
            p.design_data_id = None
    for p in list(spec.datums.provenance.values()) + list(
        spec.tensioner_system.provenance.values()
    ):
        p.design_data_id = None
    for ref in spec.reference_totals.values():
        ref.design_data_id = None
    report = reconcile(spec, render(spec))
    assert report["checks"]["f_design_data"]["status"] == "not_established"
    assert report["result"] == "pass_with_open_items"


@pytest.mark.parametrize(
    ("text", "where"),
    [
        ("CAL-0123", "report_document_no"),
        ("CAL-0123-A", "title"),
        ("see Data!B12", "gap"),
        ("see Data!", "gap"),
    ],
)
def test_archive_citation_text_on_the_drawing_fails(spec, text, where):
    if where == "report_document_no":
        spec.title_block.report_document_no = text
    elif where == "title":
        spec.title_block.title = f"Riser stack-up {text}"
    else:
        spec.gaps[0]["detail"] = text
    _assert_reason(reconcile(spec, render(spec)), "f_design_data", "archive")


def test_title_block_prints_the_report_document_number(spec, svg):
    assert spec.title_block.report_document_no == "RPT-SYN-001"
    assert ">DOCUMENT<" in svg and ">SOURCE<" not in svg
    assert 'data-field="title_block.report_document_no"' in svg
    assert ">RPT-SYN-001<" in svg
    assert reconcile(spec, svg)["result"] == "pass"


def test_archive_document_ref_is_never_printed(spec):
    spec.title_block.document_ref = "CAL-0456-B"
    svg = render(spec)
    assert "CAL-0456-B" not in svg
    assert reconcile(spec, svg)["result"] == "pass"


def test_missing_report_document_number_prints_na(spec):
    spec.title_block.report_document_no = None
    svg = render(spec)
    assert '<tspan class="na" data-field="title_block.report_document_no">n/a</tspan>' in svg
    assert reconcile(spec, svg)["result"] == "pass"


def test_public_item_without_references_fails_validation(spec):
    item = next(i for i in spec.design_data if i.id == "D-06")
    item.reference_ids = []
    assert any("D-06" in p and "public" in p for p in spec.validate())


def test_unresolved_reference_id_fails_validation(spec):
    item = next(i for i in spec.design_data if i.id == "D-06")
    item.reference_ids = ["R-9"]
    assert any("R-9" in p for p in spec.validate())


def test_assumed_item_must_say_why_no_public_data(spec):
    item = next(i for i in spec.design_data if i.id == "D-18")
    item.note = "sheave radius from a sketch"
    assert any("D-18" in p and "no public data" in p for p in spec.validate())


def test_assumed_item_may_cite_only_context_references(spec):
    item = next(i for i in spec.design_data if i.id == "D-18")
    item.reference_ids = ["R-1"]
    assert any("D-18" in p and "context" in p for p in spec.validate())


def test_owner_decision_item_must_name_the_decision(spec):
    item = next(i for i in spec.design_data if i.id == "D-10")
    item.note = "chosen by the owner"
    assert any("D-10" in p and "decision" in p for p in spec.validate())


def test_design_data_id_must_resolve(spec):
    spec.component("c06-pup-joint").provenance["od_in"].design_data_id = "D-97"
    assert any("D-97" in p for p in spec.validate())


@pytest.mark.parametrize("bad", ["D-1", "D12", "X-12", "D-1234"])
def test_design_data_item_id_pattern(spec, bad):
    spec.design_data[0].id = bad
    assert any(bad in p for p in spec.validate())


def test_register_round_trips_through_json(spec):
    again = from_json(to_json(spec))
    assert again.design_data == spec.design_data
    assert again.references == spec.references
    assert (
        again.component("c09-riser-joint-buoyant")
        .provenance["buoyancy_depth_rating_ft"]
        .design_data_id
        == "D-10"
    )


def test_non_round_ft_tick_fails(spec, svg):
    root = ET.fromstring(svg)
    axis = [g for g in root if g.get("data-role") == "axis" and len(g)]
    label = next(
        sp
        for g in axis
        for sp in g.iter(NS + "tspan")
        if sp.get("data-unit") == "ft" and sp.text == "50"
    )
    z = label.get("data-tick-el-m")
    z_new = f"{float(z) + 0.05:.9f}"  # 50.16 ft: prints as "50" but is not round
    tampered = svg.replace(f'data-tick-el-m="{z}"', f'data-tick-el-m="{z_new}"')
    assert tampered != svg
    _assert_reason(reconcile(spec, tampered), "f_design_data", "round")


# -- nested_in: overlay and landing ---------------------------------------------------------


def test_overlay_is_excluded_from_the_chain_and_passes(spec, svg):
    lfj = spec.component("c14-lower-flex-joint")
    assert lfj.nested_in == "c15-lmrp"
    assert spec.is_overlay(lfj)
    assert lfj not in spec.stacked()
    report = reconcile(spec, svg)
    assert report["checks"]["d_totals"]["status"] == "pass"
    assert any("overlay" in n for n in report["checks"]["d_totals"]["notes"])
    # drawn after its host
    order = [
        g.get("data-component-id")
        for g in ET.fromstring(svg)
        if g.get("data-role") == "component"
    ]
    assert order.index("c14-lower-flex-joint") > order.index("c15-lmrp")
    assert 'data-nested-in="c15-lmrp"' in svg


def test_unknown_nested_in_host_fails_validation(spec):
    spec.component("c14-lower-flex-joint").nested_in = "c99-missing"
    assert any("nested_in" in p for p in spec.validate())
    report = reconcile(spec, "<svg/>")
    _assert_reason(report, "d_totals", "nested_in")


def test_landing_overlap_passes_with_explicit_nested_in(spec, svg):
    wh = spec.component("c17-wellhead")
    cond = spec.component("c18-conductor")
    assert wh.nested_in == "c18-conductor"
    assert float(wh.bottom_el_m) < float(cond.top_el_m)  # a real overlap
    assert wh in spec.stacked()
    report = reconcile(spec, svg)
    assert report["checks"]["d_totals"]["status"] == "pass"
    assert any("lands" in n for n in report["checks"]["d_totals"]["notes"])


def test_landing_overlap_without_nested_in_fails_totals(spec):
    spec.component("c17-wellhead").nested_in = None
    spec.component("c17-wellhead").provenance.pop("nested_in")
    _assert_reason(reconcile(spec, render(spec)), "d_totals", "gap/overlap")


def test_nested_component_neither_overlay_nor_chained_fails(spec):
    # casing is outside the chain by type; naming a host it does not fit in is wrong
    spec.component("c20-casing").nested_in = "c18-conductor"
    _assert_reason(
        reconcile(spec, render(spec)), "d_totals", "neither an overlay nor in"
    )


def test_nested_in_attribute_must_match_the_spec(spec, svg):
    tampered = _swap(svg, ' data-nested-in="c15-lmrp"', "")
    _assert_reason(reconcile(spec, tampered), "a_mapping", "data-nested-in")


# -- provenance bases, keys and reference totals ---------------------------------------------


@pytest.mark.parametrize(
    "basis",
    ["owner decision K05 (2026-09-24)", "assumed", "report table", "synthetic"],
)
def test_accepted_provenance_bases(basis):
    assert Provenance("somewhere", basis).basis == basis


@pytest.mark.parametrize(
    "basis",
    [
        "owner decision K5 (2026-09-24)",
        "owner decision k05 (2026-09-24)",
        "owner decision K05 (26-09-24)",
        "owner decision K05",
        "owner decision K05 (2026-09-24) extra",
        "Owner decision K05 (2026-09-24)",
    ],
)
def test_malformed_owner_decision_basis_is_rejected(basis):
    with pytest.raises(ValueError, match="basis"):
        Provenance("somewhere", basis)


def test_reference_value_basis_is_validated():
    with pytest.raises(ValueError, match="basis"):
        ReferenceValue(1.0, "somewhere", "guess")


def test_pivot_and_nested_in_provenance_keys_are_accepted(spec):
    comp = spec.component("c14-lower-flex-joint")
    assert {"pivot_el_m", "nested_in"} <= set(comp.provenance)
    assert spec.validate() == []


def test_unknown_provenance_key_is_rejected(spec):
    spec.component("c06-pup-joint").provenance["colour"] = Provenance("x", "assumed")
    assert any("provenance key 'colour'" in p for p in spec.validate())


@pytest.mark.parametrize(
    "key",
    [
        "closure_residual_geometric_m",
        "static_stretch_m",
        "tj_ib_geometric_m",
        "tj_ib_tensioned_m",
        "tj_ib_mid_stroke_m",
        "tj_offset_from_mid_m",
        "ufj_pivot_el_m",
        "lfj_pivot_el_m",
        "static_stretch_m_workbook",
        "stackup_length_m_workbook",
    ],
)
def test_space_out_reference_keys_are_accepted(spec, key):
    spec.reference_totals[key] = ReferenceValue(1.0, "synthetic fixture", "synthetic")
    assert not any("reference_totals" in p for p in spec.validate())


def test_unknown_reference_key_is_rejected(spec):
    spec.reference_totals["made_up_m"] = ReferenceValue(1.0, "x", "synthetic")
    assert any("reference_totals key 'made_up_m'" in p for p in spec.validate())


def test_pivot_symbol_sits_at_the_pivot_elevation(spec, svg):
    root = ET.fromstring(svg)
    pivots = [
        el
        for g in root
        if g.get("data-role") == "component"
        for el in g.iter(NS + "circle")
        if el.get("data-part") == "pivot"
    ]
    assert len(pivots) == 2
    assert sorted(float(p.get("data-pivot-el-m")) for p in pivots) == [-1479.0, 21.5]
    pv = next(p for p in pivots if p.get("data-pivot-el-m") == "21.5")
    old = f'cx="{pv.get("cx")}" cy="{pv.get("cy")}"'
    new = f'cx="{pv.get("cx")}" cy="{float(pv.get("cy")) + 5:.2f}"'
    _assert_reason(reconcile(spec, _swap(svg, old, new)), "b_positions", "pivot")


# -- L12: review list --------------------------------------------------------------------------


def _review_keys(svg: str) -> list[str]:
    root = ET.fromstring(svg)
    band = next(g for g in root if g.get("data-role") == "review-list")
    return sorted(
        {
            t.get("data-review")
            for t in band.iter(NS + "text")
            if t.get("data-review") is not None
        }
    )


def test_review_list_has_one_entry_per_open_item_keyed_by_id(spec, svg):
    assert _review_keys(svg) == ["data_conflicts[0]", "gaps[0]"]
    # warning marks on the rows named by component_id, and nowhere else
    marked = sorted(
        g.get("data-component-id")
        for g in ET.fromstring(svg)
        if g.get("data-role") == "table-row"
        and any(el.get("class") == "warn" for el in g)
    )
    assert marked == ["c02-upper-flex-joint", "c19-conductor"]
    assert 'data-field="gaps.0.component_id"' in svg


def test_closed_items_are_not_listed(spec):
    spec.gaps.append(
        {
            "component_id": "c06-pup-joint",
            "item": "od_in",
            "status": "resolved",
            "detail": "closed",
        }
    )
    svg = render(spec)
    assert _review_keys(svg) == ["data_conflicts[0]", "gaps[0]"]
    assert reconcile(spec, svg)["result"] == "pass"


def test_review_entry_dropped_from_the_list_fails(spec, svg):
    lines = _lines(svg)
    start, end = _row_span(lines, '<g data-role="review-list"')
    kept = [
        ln for ln in lines[start:end] if 'data-review="data_conflicts[0]"' not in ln
    ]
    assert len(kept) < end - start
    tampered = "\n".join(lines[:start] + kept + lines[end:])
    _assert_reason(reconcile(spec, tampered), "a_mapping", "review")


def test_review_entry_text_must_equal_the_spec(spec, svg):
    tampered = _swap(svg, ">two synthetic sources disagree", ">two synthetic sources agree")
    _assert_reason(reconcile(spec, tampered), "c_numbers", "data_conflicts.0.detail")


def test_review_entry_with_unknown_component_id_fails_validation(spec):
    spec.gaps[0]["component_id"] = "c99-missing"
    assert any("component_id" in p for p in spec.validate())


def test_entries_without_component_id_are_read_compatibly(spec_text):
    import json

    data = json.loads(spec_text)
    for entry in data["gaps"] + data["data_conflicts"]:
        entry.pop("component_id")
    data["gaps"][0]["item"] = "c02-upper-flex-joint od_in"
    data["data_conflicts"][0]["item"] = "free text naming nothing"
    spec = StackupDrawingSpec.from_dict(data)
    assert spec.gaps[0]["component_id"] == "c02-upper-flex-joint"
    assert spec.data_conflicts[0]["component_id"] is None
    assert spec.validate() == []
    assert reconcile(spec, render(spec))["result"] == "pass"


# -- determinism ---------------------------------------------------------------------------------


def test_table_render_is_deterministic(spec):
    again = from_json(to_json(spec))
    assert render(spec).encode("utf-8") == render(again).encode("utf-8")
