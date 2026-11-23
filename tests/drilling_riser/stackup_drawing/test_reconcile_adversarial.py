"""Adversarial reconcile tests (#2152, review r1).

Each test builds one reviewer counterexample - a tampered SVG or an edited
spec that is WRONG - and asserts that reconcile rejects it on the check meant
to catch it. Finding numbers refer to the r1 review.
"""

from __future__ import annotations

import html
import json
import re

import pytest

from digitalmodel.drilling_riser.stackup_drawing import (
    from_json,
    from_schedule_assembly,
    reconcile,
    render,
    to_json,
)

from .conftest import row_cell
from .test_schema_adapters import FT, _demo_assembly, _record


def _checks(report: dict) -> dict:
    return {k: v["status"] for k, v in report["checks"].items()}


def _assert_fails(report: dict, check: str) -> None:
    assert report["result"] == "fail", _checks(report)
    assert report["checks"][check]["status"] == "fail", _checks(report)


def _lines(svg: str) -> list[str]:
    return svg.split("\n")


def _group_span(lines: list[str], head: str) -> tuple[int, int]:
    start = next(i for i, ln in enumerate(lines) if ln.startswith(head))
    end = next(i for i in range(start, len(lines)) if lines[i] == "</g>")
    return start, end


def _comp_head(cid: str) -> str:
    return f'<g data-role="component" data-component-id="{cid}"'


def _row_head(cid: str) -> str:
    # #2158: the callout is a table row (design A); the r1 classes are
    # re-expressed against the row and its cells
    return f'<g data-role="table-row" data-component-id="{cid}"'


def _replace_once(text: str, old: str, new: str) -> str:
    assert text.count(old) == 1, (old, text.count(old))
    return text.replace(old, new)


# -- 1. transforms and visibility ---------------------------------------------------


def test_f1_transform_on_component_is_rejected(spec, svg):
    tampered = _replace_once(
        svg,
        _comp_head("c06-pup-joint"),
        _comp_head("c06-pup-joint") + ' transform="translate(0,100)"',
    )
    _assert_fails(reconcile(spec, tampered), "b_positions")


def test_f1_display_none_on_component_is_rejected(spec, svg):
    tampered = _replace_once(
        svg,
        _comp_head("c06-pup-joint"),
        _comp_head("c06-pup-joint") + ' display="none"',
    )
    _assert_fails(reconcile(spec, tampered), "a_mapping")


def test_f1_style_opacity_zero_on_callout_is_rejected(spec, svg):
    cell = row_cell(svg, "c06-pup-joint", "top")
    hidden = _replace_once(cell, ' class="tc"', ' class="tc" style="opacity:0"')
    tampered = _replace_once(svg, cell, hidden)
    _assert_fails(reconcile(spec, tampered), "a_mapping")


def test_f1_stylesheet_hiding_rule_is_rejected(spec, svg):
    tampered = _replace_once(svg, "</style>", ".tc{display:none}</style>")
    _assert_fails(reconcile(spec, tampered), "a_mapping")


# -- 2. body pieces -------------------------------------------------------------------


def test_f2_duplicate_body_piece_is_rejected(spec, svg):
    lines = _lines(svg)
    start, end = _group_span(lines, _comp_head("c06-pup-joint"))
    body = next(i for i in range(start, end) if 'data-part="body"' in lines[i])
    lines.insert(body + 1, lines[body])
    _assert_fails(reconcile(spec, "\n".join(lines)), "b_positions")


def test_f2_gap_inside_multi_zone_body_is_rejected(spec, svg):
    # c08 crosses the top zone and the condensed zone: two body pieces. Halving
    # the first piece leaves a gap but keeps the overall top and bottom.
    lines = _lines(svg)
    start, end = _group_span(lines, _comp_head("c08-riser-joint-bare"))
    body = next(i for i in range(start, end) if 'data-part="body"' in lines[i])
    lines[body] = re.sub(
        r' height="([\d.]+)"',
        lambda m: f' height="{float(m.group(1)) / 2:.2f}"',
        lines[body],
        count=1,
    )
    _assert_fails(reconcile(spec, "\n".join(lines)), "b_positions")


# -- 3. zone table ----------------------------------------------------------------------


def test_f3_non_finite_zone_scale_is_rejected(spec, svg):
    match = re.search(r'data-zones="([^"]*)"', svg)
    zones = json.loads(html.unescape(match.group(1)))
    for zone in zones:
        zone["px_per_m"] = float("nan")
    tampered = svg.replace(
        match.group(0), f'data-zones="{html.escape(json.dumps(zones), quote=True)}"'
    )
    # and move a component 2 px, which the NaN scale would otherwise hide
    lines = _lines(tampered)
    start, end = _group_span(lines, _comp_head("c06-pup-joint"))
    body = next(i for i in range(start, end) if 'data-part="body"' in lines[i])
    lines[body] = re.sub(
        r' y="([\d.]+)"',
        lambda m: f' y="{float(m.group(1)) + 2:.2f}"',
        lines[body],
        count=1,
    )
    _assert_fails(reconcile(spec, "\n".join(lines)), "b_positions")


# -- 4. callouts ------------------------------------------------------------------------


def test_f4_empty_callout_text_is_rejected(spec, svg):
    lines = _lines(svg)
    start, end = _group_span(lines, _row_head("c06-pup-joint"))
    kept = [ln for ln in lines[start:end] if not ln.startswith("<text ")]
    kept.append('<text data-col="top"/>')
    tampered = "\n".join(lines[:start] + kept + lines[end:])
    _assert_fails(reconcile(spec, tampered), "a_mapping")


def test_f4_detached_leader_is_rejected(spec, svg):
    lines = _lines(svg)
    start, end = _group_span(lines, _row_head("c06-pup-joint"))
    leader = next(i for i in range(start, end) if 'class="leader"' in lines[i])
    lines[leader] = re.sub(
        r'points="([\d.]+),',
        lambda m: f'points="{float(m.group(1)) + 150:.2f},',
        lines[leader],
        count=1,
    )
    _assert_fails(reconcile(spec, "\n".join(lines)), "b_positions")


# -- 5. n/a literal -----------------------------------------------------------------------


def test_f5_number_inside_na_class_is_rejected(spec, svg):
    tampered = _replace_once(
        svg,
        '<tspan class="na" data-field="od_in">n/a</tspan>',
        '<tspan class="na" data-field="od_in">21</tspan>',
    )
    _assert_fails(reconcile(spec, tampered), "e_not_found")


# -- 6. complete number text ------------------------------------------------------------------


def _c06_co2(svg: str) -> str:
    """The OD cell of the pup-joint row (the r1 second callout line)."""
    return row_cell(svg, "c06-pup-joint", "od")


def test_f6_extra_token_after_number_is_rejected(spec, svg):
    cell = row_cell(svg, "c08-riser-joint-bare", "qty")
    pattern = re.compile(r'(<tspan data-field="count"[^>]*>)3(</tspan>)')
    new, n = pattern.subn(r"\g<1>3 999\g<2>", cell)
    assert n == 1
    tampered = _replace_once(svg, cell, new)
    _assert_fails(reconcile(spec, tampered), "c_numbers")


def test_f6_negative_precision_cannot_widen_tolerance(spec, svg):
    line = _c06_co2(svg)
    old = '<tspan data-field="od_in" data-unit="in" data-decimals="0">21</tspan>'
    new = '<tspan data-field="od_in" data-unit="in" data-decimals="-3">521</tspan>'
    tampered = svg.replace(line, _replace_once(line, old, new))
    _assert_fails(reconcile(spec, tampered), "c_numbers")


@pytest.mark.parametrize("where", ["tail", "direct"])
def test_f6_number_outside_tspans_is_rejected(spec, svg, where):
    line = _c06_co2(svg)
    if where == "tail":
        new = line.replace("</tspan></text>", "</tspan>99</text>")
    else:
        new = line.replace('data-col="od">', 'data-col="od">99', 1)
    assert new != line
    _assert_fails(reconcile(spec, svg.replace(line, new)), "c_numbers")


def test_f6_numeric_field_without_digits_is_rejected(spec, svg):
    line = row_cell(svg, "c06-pup-joint", "top")
    new = re.sub(
        r'(<tspan data-field="top_el_m" data-unit="m" data-decimals="2">)[^<]*(</tspan>)',
        r"\g<1>see note\g<2>",
        line,
        count=1,
    )
    assert new != line
    _assert_fails(reconcile(spec, svg.replace(line, new)), "c_numbers")


# -- 7. per-component closure -------------------------------------------------------------------


def test_f7_opposing_length_errors_do_not_cancel(spec):
    spec.component("c06-pup-joint").joint_length_m += 0.5
    spec.component("c07-pup-joint").joint_length_m -= 0.5
    _assert_fails(reconcile(spec, render(spec)), "d_totals")


# -- 8. closure exemption -------------------------------------------------------------------------


def test_f8_adapter_residual_is_not_established():
    spec = from_schedule_assembly(_demo_assembly(), ordered_top_down=True)
    report = reconcile(spec, render(spec))

    assert report["checks"]["d_totals"]["status"] == "not_established", _checks(report)
    assert report["result"] == "pass_with_open_items"


def test_f8_string_above_drill_floor_fails():
    assembly = _demo_assembly()
    records = [dict(item.component) for item in assembly.model.items]
    records.insert(
        4, _record("buoyancy-joint-extra", "riser_joint_buoyant", 30, 75 * FT)
    )
    model = type(assembly.model).from_records(records, rsu_id="DEMO")
    long_string = type(assembly)(
        rsu_id="DEMO",
        model=model,
        geometry=assembly.geometry,
        assumptions=assembly.assumptions,
    )
    spec = from_schedule_assembly(long_string, ordered_top_down=True)
    assert float(spec.components[0].top_el_m) > float(spec.datums.drill_floor_el_m)

    _assert_fails(reconcile(spec, render(spec)), "d_totals")


# -- 9. datums ---------------------------------------------------------------------------------------


def test_f9_water_depth_and_air_gap_must_match_datum_elevations(spec):
    spec.datums.water_depth_m = float(spec.datums.water_depth_m) - 100.0
    spec.datums.air_gap_m = float(spec.datums.air_gap_m) + 100.0
    _assert_fails(reconcile(spec, render(spec)), "d_totals")


def test_f9_missing_mudline_datum_is_rejected(spec, svg):
    lines = [
        ln
        for ln in _lines(svg)
        if not ln.startswith('<g data-role="datum" data-datum="mudline_el_m"')
    ]
    assert len(lines) == len(_lines(svg)) - 1
    _assert_fails(reconcile(spec, "\n".join(lines)), "b_positions")


# -- the untampered drawings still pass ---------------------------------------------------------------


def test_untampered_fixture_still_passes(spec, svg):
    assert reconcile(spec, svg)["result"] == "pass"
    again = from_json(to_json(spec))
    assert reconcile(again, render(again))["result"] == "pass"
