"""Closed-grammar reconcile tests (#2152, review r2).

One test per round-2 counterexample, plus a generic unknown-element and
unknown-attribute test. Each asserts the check that fails AND the reason text.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.drilling_riser.stackup_drawing import reconcile


def _lines(svg: str) -> list[str]:
    return svg.split("\n")


def _group_span(lines: list[str], head: str) -> tuple[int, int]:
    start = next(i for i, ln in enumerate(lines) if ln.startswith(head))
    end = next(i for i in range(start, len(lines)) if lines[i] == "</g>")
    return start, end


C06 = '<g data-role="component" data-component-id="c06-pup-joint"'


def _assert_reason(report: dict, check: str, reason: str) -> None:
    statuses = {k: v["status"] for k, v in report["checks"].items()}
    assert report["result"] == "fail", f"result {report['result']!r}; {statuses}"
    failures = report["checks"][check]["failures"]
    assert any(reason in f for f in failures), (
        f"{check} lacks reason {reason!r}; {check} failures: {failures}; "
        f"statuses: {statuses}"
    )


def _replace_once(text: str, old: str, new: str) -> str:
    assert text.count(old) == 1, (old, text.count(old))
    return text.replace(old, new)


def _c06_line(svg: str, cls: str) -> str:
    return next(
        ln
        for ln in _lines(svg)
        if f'class="{cls}" data-component-id="c06-pup-joint"' in ln
    )


# -- r2-1: definitions are not rendered instances ------------------------------------


def test_r2_1_component_wrapped_in_defs_is_rejected(spec, svg):
    lines = _lines(svg)
    start, end = _group_span(lines, C06)
    block = lines[start : end + 1]
    rest = lines[:start] + lines[end + 1 :]
    defs_end = next(i for i, ln in enumerate(rest) if "</defs>" in ln)
    rest[defs_end] = rest[defs_end].replace("</defs>", "\n".join(block) + "</defs>")
    _assert_reason(
        reconcile(spec, "\n".join(rest)),
        "a_mapping",
        "grammar: <g> not allowed under <defs>",
    )


# -- r2-2: inline hiding the r1 scan missed ------------------------------------------------


def test_r2_2_display_none_important_is_rejected(spec, svg):
    tampered = _replace_once(svg, C06, C06 + ' style="display:none!important"')
    _assert_reason(
        reconcile(spec, tampered),
        "a_mapping",
        "grammar: attribute 'style' not allowed on <g>",
    )


def test_r2_2_font_size_zero_on_callout_is_rejected(spec, svg):
    tampered = _replace_once(
        svg,
        'class="co1" data-component-id="c06-pup-joint"',
        'class="co1" font-size="0" data-component-id="c06-pup-joint"',
    )
    _assert_reason(
        reconcile(spec, tampered),
        "a_mapping",
        "grammar: attribute 'font-size' not allowed on <text>",
    )


# -- r2-3: viewport -----------------------------------------------------------------------------


def test_r2_3_shifted_viewbox_is_rejected(spec, svg):
    head = _lines(svg)[0]
    shifted = head.replace('viewBox="0 0 ', 'viewBox="100000 100000 ', 1)
    assert shifted != head
    _assert_reason(
        reconcile(spec, svg.replace(head, shifted, 1)), "b_positions", "root viewBox"
    )


def test_r2_3_nested_svg_is_rejected(spec, svg):
    lines = _lines(svg)
    start, end = _group_span(lines, C06)
    lines[start] = '<svg x="0" y="100">' + lines[start]
    lines[end] = lines[end] + "</svg>"
    _assert_reason(
        reconcile(spec, "\n".join(lines)),
        "a_mapping",
        "grammar: <svg> not allowed under <svg>",
    )


# -- r2-4: text positioning on tspans ----------------------------------------------------------


def test_r2_4_tspan_x_offset_is_rejected(spec, svg):
    line = _c06_line(svg, "co1")
    moved = line.replace("><tspan ", '><tspan x="100000" ', 1)
    assert moved != line
    _assert_reason(
        reconcile(spec, svg.replace(line, moved)),
        "b_positions",
        "grammar: attribute 'x' not allowed on <tspan>",
    )


# -- r2-5: tick metadata on a field span ----------------------------------------------------------


def test_r2_5_tick_metadata_on_field_span_is_rejected(spec, svg):
    line = _c06_line(svg, "co2")
    old = '<tspan data-field="od_in" data-unit="in" data-decimals="0">21</tspan>'
    new = (
        '<tspan data-field="od_in" data-tick-el-m="999" data-unit="m" '
        'data-decimals="0">999</tspan>'
    )
    tampered = svg.replace(line, _replace_once(line, old, new))
    _assert_reason(
        reconcile(spec, tampered),
        "c_numbers",
        "mixes data-field with tick metadata",
    )


# -- r2-6: sign in a sibling span -------------------------------------------------------------------


def test_r2_6_sibling_minus_before_od_is_rejected(spec, svg):
    line = _c06_line(svg, "co2")
    old = '<tspan data-field="od_in"'
    tampered = svg.replace(line, _replace_once(line, old, "<tspan>−</tspan>" + old))
    _assert_reason(reconcile(spec, tampered), "c_numbers", "sign outside its field")


# -- r2-7: non-finite spec values ---------------------------------------------------------------------


def test_r2_7_nan_water_depth_in_spec_is_rejected(spec, svg):
    spec.datums.water_depth_m = math.nan
    _assert_reason(
        reconcile(spec, svg), "d_totals", "spec invalid: datums.water_depth_m"
    )


@pytest.mark.parametrize("value", [math.inf, -math.inf])
def test_r2_7_validate_rejects_infinite_values(spec, value):
    spec.component("c06-pup-joint").od_in = value
    assert any(
        "components[c06-pup-joint].od_in" in p and "not finite" in p
        for p in spec.validate()
    )


# -- generic closed grammar ------------------------------------------------------------------------------


def test_unknown_element_is_rejected(spec, svg):
    tampered = svg.replace(
        '<g data-role="decor">',
        '<g data-role="decor"><foreignObject x="0" y="0" width="10" height="10"/>',
        1,
    )
    assert tampered != svg
    _assert_reason(
        reconcile(spec, tampered),
        "a_mapping",
        "grammar: element <foreignObject> not allowed",
    )


def test_unknown_attribute_is_rejected(spec, svg):
    lines = _lines(svg)
    start, end = _group_span(lines, C06)
    body = next(i for i in range(start, end) if 'data-part="body"' in lines[i])
    lines[body] = lines[body].replace("<rect ", '<rect data-evil="1" ', 1)
    _assert_reason(
        reconcile(spec, "\n".join(lines)),
        "a_mapping",
        "grammar: attribute 'data-evil' not allowed on <rect>",
    )


def test_non_ascii_digits_in_field_are_rejected(spec, svg):
    # Full-width "２１" parses numerically as 21 in Python but is not the canonical form.
    line = _c06_line(svg, "co2")
    old = '<tspan data-field="od_in" data-unit="in" data-decimals="0">21</tspan>'
    new = '<tspan data-field="od_in" data-unit="in" data-decimals="0">２１</tspan>'
    tampered = svg.replace(line, _replace_once(line, old, new))
    _assert_reason(reconcile(spec, tampered), "c_numbers", "non-ASCII digit")


def test_non_ascii_digits_outside_fields_are_rejected(spec, svg):
    # Arabic-Indic digits in an unannotated span must count as an untraceable number.
    line = _c06_line(svg, "co2")
    old = '<tspan data-field="od_in"'
    tampered = svg.replace(line, _replace_once(line, old, "<tspan>٣</tspan>" + old))
    _assert_reason(reconcile(spec, tampered), "c_numbers", "non-ASCII digit")


def test_document_reference_with_digits_is_verbatim_text(spec):
    from digitalmodel.drilling_riser.stackup_drawing import render

    spec.title_block.document_ref = "DOC-100-A"
    svg = render(spec)
    assert reconcile(spec, svg)["result"] == "pass"
    tampered = svg.replace(">DOC-100-A<", ">DOC-101-A<")
    _assert_reason(reconcile(spec, tampered), "c_numbers", "title_block.document_ref")
