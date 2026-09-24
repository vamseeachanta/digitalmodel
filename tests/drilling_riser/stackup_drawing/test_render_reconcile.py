"""Render + reconcile tests for the riser stack-up drawing (#2152).

Every negative test tampers with one thing and asserts that the check meant
to catch it fails:

  (a) mapping    - dropped component, orphan element
  (b) positions  - component moved 2 px
  (c) numbers    - callout value changed
  (d) totals     - closure broken by editing the spec
  (e) NOT_FOUND  - a NOT_FOUND value drawn as a number

Golden snapshot
---------------
``fixtures/synthetic.svg`` is the committed render of
``fixtures/synthetic_spec.json``. When the renderer changes on purpose,
regenerate it deliberately and review the diff before committing::

    $env:STACKUP_DRAWING_REGEN_GOLDEN = "1"   # PowerShell; bash: export ...=1
    python -m pytest tests/drilling_riser/stackup_drawing -k golden
    Remove-Item Env:STACKUP_DRAWING_REGEN_GOLDEN

The regeneration run rewrites the file and skips the comparison; the next
plain run must pass. ``.gitattributes`` marks the file ``-text`` so the
comparison is byte-exact on every platform.
"""

from __future__ import annotations

import os
import re

import pytest

from digitalmodel.drilling_riser.stackup_drawing import (
    from_json,
    reconcile,
    render,
    to_json,
)

from .conftest import GOLDEN_SVG

CHECKS = ("a_mapping", "b_positions", "c_numbers", "d_totals", "e_not_found")


def _status(report: dict, check: str) -> str:
    return report["checks"][check]["status"]


def _component_block(svg: str, cid: str) -> tuple[int, int]:
    """Line span [start, end] of the component group ``cid`` in ``svg``."""
    lines = svg.split("\n")
    head = f'<g data-role="component" data-component-id="{cid}"'
    start = next(i for i, ln in enumerate(lines) if ln.startswith(head))
    end = next(i for i in range(start, len(lines)) if lines[i] == "</g>")
    return start, end


def _assert_fails(report: dict, check: str) -> None:
    assert report["result"] == "fail"
    assert _status(report, check) == "fail", (
        f"{check} did not fail; checks: "
        + str({k: v["status"] for k, v in report["checks"].items()})
    )


# -- positive -------------------------------------------------------------------


def test_synthetic_fixture_renders_and_reconciles_clean(spec, svg):
    report = reconcile(spec, svg)

    assert svg.startswith("<svg ")
    assert report["result"] == "pass", {
        k: v["failures"] for k, v in report["checks"].items()
    }
    for check in CHECKS:
        assert _status(report, check) == "pass"
        assert report["checks"][check]["failures"] == []
    # the fixture carries one deliberate NOT_FOUND (upper flex joint OD)
    assert report["not_found"] == ["components[c02-upper-flex-joint].od_in"]


def test_every_component_is_drawn_with_data_attributes(spec, svg):
    for comp in spec.components:
        assert f'data-component-id="{comp.id}"' in svg
    assert 'id="elevation-transform"' in svg


# -- negative: (a) mapping -----------------------------------------------------------


def test_dropped_component_fails_mapping(spec, svg):
    start, end = _component_block(svg, "c06-pup-joint")
    lines = svg.split("\n")
    tampered = "\n".join(lines[:start] + lines[end + 1 :])

    report = reconcile(spec, tampered)

    _assert_fails(report, "a_mapping")
    assert any("c06-pup-joint" in f for f in report["checks"]["a_mapping"]["failures"])


def test_orphan_element_fails_mapping(spec, svg):
    orphan = (
        '<g data-role="component" data-component-id="c99-ghost" data-type="other">'
        '<rect x="300" y="100" width="20" height="20" data-part="body"/></g>\n'
    )
    tampered = svg.replace("</svg>", orphan + "</svg>")

    report = reconcile(spec, tampered)

    _assert_fails(report, "a_mapping")
    assert any("c99-ghost" in f for f in report["checks"]["a_mapping"]["failures"])


def test_unlabelled_group_fails_mapping(spec, svg):
    tampered = svg.replace("</svg>", '<g><rect x="1" y="1" width="2" height="2"/></g>\n</svg>')

    _assert_fails(reconcile(spec, tampered), "a_mapping")


# -- negative: (b) positions ------------------------------------------------------------


def test_component_moved_two_px_fails_positions(spec, svg):
    start, end = _component_block(svg, "c06-pup-joint")
    lines = svg.split("\n")
    for i in range(start, end + 1):
        if 'data-part="body"' in lines[i]:
            lines[i] = re.sub(
                r' y="([\d.]+)"',
                lambda m: f' y="{float(m.group(1)) + 2.0:.2f}"',
                lines[i],
                count=1,
            )
            break
    else:  # pragma: no cover - fixture guard
        pytest.fail("pup joint body not found")
    tampered = "\n".join(lines)
    assert tampered != svg

    report = reconcile(spec, tampered)

    _assert_fails(report, "b_positions")
    assert any("c06-pup-joint" in f for f in report["checks"]["b_positions"]["failures"])


# -- negative: (c) numbers -------------------------------------------------------------------


def test_callout_number_changed_fails_numbers(spec, svg):
    pattern = re.compile(
        r'(<text [^>]*data-component-id="c08-riser-joint-bare"[^>]*>'
        r'<tspan data-field="count"[^>]*>)3(</tspan>)'
    )
    tampered, n = pattern.subn(r"\g<1>4\g<2>", svg)
    assert n == 1

    report = reconcile(spec, tampered)

    _assert_fails(report, "c_numbers")
    assert any("count" in f for f in report["checks"]["c_numbers"]["failures"])


# -- negative: (d) totals ------------------------------------------------------------------------


def test_closure_broken_in_spec_fails_totals(spec):
    pup = spec.component("c06-pup-joint")
    pup.joint_length_m = float(pup.joint_length_m) + 0.5  # elevations left as-is

    report = reconcile(spec, render(spec))

    _assert_fails(report, "d_totals")
    # the drawing itself is still a faithful picture of the (inconsistent) data
    assert _status(report, "c_numbers") == "pass"


# -- negative: (e) NOT_FOUND ----------------------------------------------------------------------


def test_not_found_drawn_as_number_fails_not_found(spec, svg):
    na = '<tspan class="na" data-field="od_in">n/a</tspan>'
    assert svg.count(na) == 1
    tampered = svg.replace(
        na, '<tspan data-field="od_in" data-unit="in" data-decimals="0">21</tspan>'
    )

    report = reconcile(spec, tampered)

    _assert_fails(report, "e_not_found")
    assert any("od_in" in f for f in report["checks"]["e_not_found"]["failures"])


# -- determinism + golden ---------------------------------------------------------------------------


def test_render_is_deterministic(spec):
    first = render(spec)
    second = render(spec)
    via_json = render(from_json(to_json(spec)))

    assert first.encode("utf-8") == second.encode("utf-8")
    assert first.encode("utf-8") == via_json.encode("utf-8")


def test_golden_snapshot(svg):
    fresh = svg.encode("utf-8")
    if os.environ.get("STACKUP_DRAWING_REGEN_GOLDEN") == "1":
        GOLDEN_SVG.write_bytes(fresh)
        pytest.skip(f"golden regenerated: {GOLDEN_SVG}")
    assert GOLDEN_SVG.exists(), "golden missing; see module docstring to regenerate"
    assert GOLDEN_SVG.read_bytes() == fresh
