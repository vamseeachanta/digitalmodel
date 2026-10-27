# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Static accessibility contract for the authored forecast exemplar.

Basis: docs/api/_assets/LAYOUT.md, Accessibility baseline, and the controls in
docs/api/hydro/short-horizon-motion-forecast.html. This checks authored markup
and style/script declarations, without executing JavaScript or using a browser.
"""

from dataclasses import dataclass
from html.parser import HTMLParser
from pathlib import Path
import re

import pytest


PAGE = (
    Path(__file__).resolve().parents[2]
    / "docs/api/hydro/short-horizon-motion-forecast.html"
)
# Authored element identities prevent deletion from making universal checks pass.
CANVASES = {"cvWave", "cvMotion", "cvGov", "cvTimeline"}
RANGES = {"hs", "tp", "scrub"}
# The physical controls speak these units. The scrubber is a normalized timeline
# position, not a physical time value; its existing name is "Scrub time".
SPOKEN_UNITS = {"hs": "metres", "tp": "seconds"}
FOCUS_SELECTORS = (
    ".theme-btn", ".play", ".seg button", ".segrow button",
    ".speed button", "input[type=range]",
)


@dataclass
class Element:
    tag: str
    attrs: dict
    parent: "Element | None"
    text: str = ""


class ForecastHTML(HTMLParser):
    """Retain elements, ancestry and text from the real HTML document."""

    def __init__(self, html):
        super().__init__(convert_charrefs=True)
        self.elements = []
        self.stack = []
        self.feed(html)
        self.close()

    def handle_starttag(self, tag, attrs):
        element = Element(tag, dict(attrs), self.stack[-1] if self.stack else None)
        self.elements.append(element)
        if tag not in {
            "area", "base", "br", "col", "embed", "hr", "img", "input",
            "link", "meta", "param", "source", "track", "wbr",
        }:
            self.stack.append(element)

    def handle_endtag(self, tag):
        while self.stack:
            if self.stack.pop().tag == tag:
                break

    def handle_data(self, data):
        for element in self.stack:
            element.text += data

    def with_id(self, identity):
        matches = [e for e in self.elements if e.attrs.get("id") == identity]
        assert len(matches) == 1, f"expected unique element: {identity}"
        return matches[0]


def _assert_visible_outline(selector, declarations):
    """Check the exemplar's authored syntax, not browser-computed styles.

    Supported colours are opaque hex colours and the existing ink/accent brand
    tokens. Other colour expressions fail closed and require explicit coverage.
    """
    properties = ("outline-width", "outline-style", "outline-color")
    effective = {name: ("", False) for name in properties}
    for declaration in declarations.split(";"):
        name, _, value = declaration.partition(":")
        name, value = name.strip().casefold(), value.strip().casefold()
        important = bool(re.search(r"!\s*important$", value))
        value = re.sub(r"\s*!\s*important$", "", value).strip()
        if name == "outline":
            parts = value.split(maxsplit=2)
            updates = dict(zip(properties, parts if len(parts) == 3 else ("",) * 3))
        elif name == "all":
            updates = dict.fromkeys(properties, "")
        elif name in properties:
            updates = {name: value}
        else:
            continue
        for property_name, property_value in updates.items():
            if important or not effective[property_name][1]:
                effective[property_name] = (property_value, important)
    width, style, colour = (effective[name][0] for name in properties)
    positive_width = re.fullmatch(r"(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)px", width)
    opaque_colour = re.fullmatch(
        r"(?:#[0-9a-f]{3}|#[0-9a-f]{6}|var\(--(?:accent|ink)\))", colour,
    )
    assert (
        positive_width and float(width[:-2]) > 0
        and style == "solid" and opaque_colour
    ), f"{selector}: missing visible outline"


def _assert_accessibility_contract(html):
    document = ForecastHTML(html)
    canvases = [e for e in document.elements if e.tag == "canvas"]
    ranges = [e for e in document.elements
              if e.tag == "input" and e.attrs.get("type") == "range"]
    assert {e.attrs.get("id") for e in canvases} == CANVASES
    assert {e.attrs.get("id") for e in ranges} == RANGES
    for element in canvases + ranges:
        identity = element.attrs["id"]
        document.with_id(identity)
        # These elements use aria-label in the authored DOM, not adjacent text.
        name = element.attrs.get("aria-label", "").strip()
        assert name, f"{identity}: missing accessible name"
        if element.tag == "canvas":
            assert element.attrs.get("role") == "img", f"{identity}: image role"
        if identity in SPOKEN_UNITS:
            assert SPOKEN_UNITS[identity] in name.casefold().split(), (
                f"{identity}: missing spoken units"
            )

    # Tie selectors to real buttons and the containers populated with buttons by
    # the page, including controls whose all:unset declaration removes the ring.
    focus_targets = {
        "themeBtn": ("theme-btn", ".theme-btn"),
        "playBtn": ("play", ".play"),
        "assetSeg": ("seg", ".seg button"),
        "opSeg": ("seg", ".seg button"),
        "seaSeg": ("segrow", ".segrow button"),
        "speedSeg": ("speed", ".speed button"),
    }
    selectors = {"input[type=range]"}
    for identity, (class_name, selector) in focus_targets.items():
        assert class_name in document.with_id(identity).attrs.get("class", "").split()
        selectors.add(selector)
    css = "\n".join(e.text for e in document.elements if e.tag == "style")
    css = re.sub(r"/\*.*?\*/", "", css, flags=re.DOTALL)
    rules = {}
    for selector_list, declarations in re.findall(r"([^{}]+)\{([^{}]*)\}", css):
        for selector in selector_list.split(","):
            key = selector.strip()
            rules[key] = rules.get(key, "") + ";" + declarations
    for selector in sorted(selectors):
        focus = selector + ":focus-visible"
        assert focus in rules, f"{selector}: missing focus-visible style"
        _assert_visible_outline(selector, rules[focus])

    # Colour is supplemented by a textual verdict. Check both the real target
    # in the status banner and its authored text update, not a string in CSS.
    verdict = document.with_id("verdictWord")
    assert "verdict-word" in verdict.attrs.get("class", "").split()
    ancestor = verdict.parent
    while ancestor and ancestor.attrs.get("id") != "statusCard":
        ancestor = ancestor.parent
    assert ancestor is not None, "verdict must be in the status banner"
    script = "\n".join(e.text for e in document.elements if e.tag == "script")
    script = re.sub(r"/\*.*?\*/|//[^\n]*", "", script, flags=re.DOTALL)
    assert re.search(
        r'el\(["\']verdictWord["\']\)\s*\.textContent\s*=\s*V\.word\s*;',
        script,
    ), "status: missing textual verdict update"
    # These are the exemplar's distinct text states, not numerical thresholds.
    assert set(re.findall(r'\bword\s*=\s*["\']([^"\']+)["\']', script)) == {
        "GO", "HOLD", "CAUTION", "NO-GO",
    }, "status: missing distinct text states"


@pytest.fixture
def forecast_html():
    return PAGE.read_text(encoding="utf-8")


def test_forecast_page_canvas_and_controls_are_named(forecast_html):
    _assert_accessibility_contract(forecast_html)


@pytest.mark.parametrize("identity", sorted(CANVASES | RANGES))
@pytest.mark.parametrize("replacement", ["", ' aria-label="   "'],
                         ids=["removed", "blank"])
def test_accessibility_contract_rejects_missing_names(
    forecast_html, identity, replacement,
):
    element = ForecastHTML(forecast_html).with_id(identity)
    original = f' aria-label="{element.attrs["aria-label"]}"'
    assert original in forecast_html
    mutated = forecast_html.replace(original, replacement)
    with pytest.raises(AssertionError, match=f"{identity}: missing accessible name"):
        _assert_accessibility_contract(mutated)


@pytest.mark.parametrize("selector", FOCUS_SELECTORS)
def test_accessibility_contract_rejects_missing_focus_styles(forecast_html, selector):
    original = selector + ":focus-visible"
    assert original in forecast_html
    mutated = forecast_html.replace(original, selector + ":hover")
    with pytest.raises(AssertionError, match="missing focus-visible style"):
        _assert_accessibility_contract(mutated)


def test_accessibility_contract_rejects_colour_only_status(forecast_html):
    original = 'el("verdictWord").textContent=V.word;'
    assert original in forecast_html
    with pytest.raises(AssertionError, match="missing textual verdict update"):
        _assert_accessibility_contract(forecast_html.replace(original, ""))


def _add_focus_declarations(html, selector, declarations, placement):
    focus = selector + ":focus-visible"
    if placement == "later-rule":
        return html.replace("</style>", f"{focus}{{{declarations}}}</style>")
    changed = 0

    def add_to_target(match):
        nonlocal changed
        selectors = [value.strip() for value in match[1].split(",")]
        if focus not in selectors:
            return match[0]
        changed += 1
        others = [value for value in selectors if value != focus]
        unchanged = ",".join(others) + "{" + match[2] + "}" if others else ""
        return unchanged + focus + "{" + match[2] + ";" + declarations + "}"

    style = re.search(r"<style>(.*?)</style>", html, flags=re.DOTALL)
    assert style is not None
    mutated_css = re.sub(r"([^{}]+)\{([^{}]*)\}", add_to_target, style[1])
    assert changed == 1
    return html[:style.start(1)] + mutated_css + html[style.end(1):]


@pytest.mark.parametrize("selector", FOCUS_SELECTORS)
@pytest.mark.parametrize("placement", ["same-rule", "later-rule"])
@pytest.mark.parametrize("declarations", [
    "outline:0 solid transparent",
    "outline:0px solid var(--accent)",
    "outline:2px solid transparent",
    "outline:none",
    "outline-width:0",
    "outline-style:none",
    "outline-color:transparent",
    "all:unset",
    "outline:0 solid transparent!important;outline:2px solid var(--accent)",
])
def test_accessibility_contract_rejects_invisible_focus_outlines(
    forecast_html, selector, placement, declarations,
):
    mutated = _add_focus_declarations(
        forecast_html, selector, declarations, placement,
    )
    with pytest.raises(
        AssertionError, match=re.escape(f"{selector}: missing visible outline"),
    ):
        _assert_accessibility_contract(mutated)


@pytest.mark.parametrize("selector", FOCUS_SELECTORS)
@pytest.mark.parametrize("placement", ["same-rule", "later-rule"])
@pytest.mark.parametrize("declarations", [
    "outline:3px solid #123456",
    "outline-width:3px",
    "outline:2px solid var(--accent)!important;outline:none",
    "outline:none;outline:2px solid var(--accent)",
])
def test_accessibility_contract_accepts_effective_visible_focus_outlines(
    forecast_html, selector, placement, declarations,
):
    mutated = _add_focus_declarations(
        forecast_html, selector, declarations, placement,
    )
    _assert_accessibility_contract(mutated)
