"""Paint order, required profiles, path grammar and geometry (#2152, review r3).

Each test builds a drawing that renders wrong (covered, invisible, empty or
degenerate) while every r2 check still passes, and asserts the failing check
and its reason text.
"""

from __future__ import annotations

import re

from digitalmodel.drilling_riser.stackup_drawing import reconcile

C06 = '<g data-role="component" data-component-id="c06-pup-joint"'


def _lines(svg: str) -> list[str]:
    return svg.split("\n")


def _group_span(lines: list[str], head: str) -> tuple[int, int]:
    start = next(i for i, ln in enumerate(lines) if ln.startswith(head))
    end = next(i for i in range(start, len(lines)) if lines[i] == "</g>")
    return start, end


def _assert_reason(report: dict, check: str, reason: str) -> None:
    statuses = {k: v["status"] for k, v in report["checks"].items()}
    assert report["result"] == "fail", f"result {report['result']!r}; {statuses}"
    failures = report["checks"][check]["failures"]
    assert any(reason in f for f in failures), (
        f"{check} lacks reason {reason!r}; {check} failures: {failures[:6]}; "
        f"statuses: {statuses}"
    )


def _sub_once(pattern: str, repl, text: str) -> str:
    out, n = re.subn(pattern, repl, text, count=1)
    assert n == 1, pattern
    return out


# -- r3-1: layer and paint order ------------------------------------------------------


def test_r3_1_background_painted_last_is_rejected(spec, svg):
    lines = _lines(svg)
    bg = next(i for i, ln in enumerate(lines) if 'class="bg"' in ln)
    moved = lines.pop(bg)
    end = next(i for i, ln in enumerate(lines) if ln == "</svg>")
    lines.insert(end, moved)
    _assert_reason(reconcile(spec, "\n".join(lines)), "a_mapping", "layer order")


def test_r3_1_title_block_moved_over_the_drawing_is_rejected(spec, svg):
    tampered = _sub_once(
        r'(<rect x="[\d.]+" y=")[\d.]+(" [^>]*class="tb")', r"\g<1>20\g<2>", svg
    )
    _assert_reason(reconcile(spec, tampered), "b_positions", "title block")


# -- r3-2: required attribute profiles ------------------------------------------------------


def test_r3_2_datum_lines_without_class_are_rejected(spec, svg):
    tampered, n = re.subn(
        r' class="(?:datum|datum-msl|datum-ml)"( data-part="datum-line")', r"\g<1>", svg
    )
    assert n >= 3
    _assert_reason(
        reconcile(spec, tampered), "a_mapping", "lacks required attribute 'class'"
    )


def test_r3_2_class_from_another_role_is_rejected(spec, svg):
    tampered = svg.replace(
        'class="datum-ml" data-part="datum-line"', 'class="rig" data-part="datum-line"'
    )
    assert tampered != svg
    _assert_reason(
        reconcile(spec, tampered), "a_mapping", "class 'rig' not allowed on <line>"
    )


# -- r3-3: path grammar ------------------------------------------------------------------------


def test_r3_3_path_reduced_to_a_move_is_rejected(spec, svg):
    tampered = svg.replace('d="M134,232.5 l6,9 l6,-9 z"', 'd="M134,232.5"')
    assert tampered != svg
    _assert_reason(reconcile(spec, tampered), "b_positions", "path d")


def test_r3_3_path_segment_off_the_sheet_is_rejected(spec, svg):
    tampered = svg.replace(
        'd="M134,232.5 l6,9 l6,-9 z"', 'd="M134,232.5 l6,9 l100000,-9 z"'
    )
    assert tampered != svg
    _assert_reason(reconcile(spec, tampered), "b_positions", "outside the viewBox")


# -- r3-4: degenerate geometry -------------------------------------------------------------------


def test_r3_4_zero_radius_sheave_is_rejected(spec, svg):
    tampered, n = re.subn(r'r="7"( class="sheave")', r'r="0"\g<1>', svg)
    assert n == 2
    _assert_reason(reconcile(spec, tampered), "b_positions", "radius")


def test_r3_4_sheave_off_its_spec_radius_is_rejected(spec, svg):
    tampered = svg.replace('<circle cx="219.76"', '<circle cx="249.76"')
    assert tampered != svg
    _assert_reason(reconcile(spec, tampered), "b_positions", "sheave radius")


def test_r3_4_zero_width_symbol_rect_is_rejected(spec, svg):
    lines = _lines(svg)
    start, end = _group_span(lines, C06)
    k = next(
        i for i in range(start, end) if 'class="flange" data-part="symbol"' in lines[i]
    )
    lines[k] = _sub_once(r'width="[\d.]+"', 'width="0"', lines[k])
    _assert_reason(reconcile(spec, "\n".join(lines)), "b_positions", "not positive")


# -- r3-5: numeric attribute syntax ----------------------------------------------------------------


def test_r3_5_nan_viewbox_is_rejected(spec, svg):
    tampered = _sub_once(r'viewBox="[^"]*"', 'viewBox="nan nan nan nan"', svg)
    _assert_reason(reconcile(spec, tampered), "b_positions", "root viewBox")


def test_r3_5_non_plain_number_syntax_is_rejected(spec, svg):
    tampered = svg.replace(
        '<rect x="0" y="0" width="860"', '<rect x="0e0" y="0" width="860"'
    )
    assert tampered != svg
    _assert_reason(
        reconcile(spec, tampered), "b_positions", "not a plain finite SVG number"
    )
