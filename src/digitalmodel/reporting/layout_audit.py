# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Assess composed CalcReport HTML without changing or executing the report.

The markup contract is CalcReport._section/_toc, MethodBlock.render and the
``numbered L1 / L2 sections`` rules in assets/calc_report_format.html. Section
and subsection numbers are document ordinals, not engineering coefficients.
This audit checks HTML labels and static SVG text structure. Expected-label
attribution excludes nested SVG and foreignObject subtrees. CSS cascade,
generated counter content, script changes and browser/PDF visibility require
a separate rendered check.
"""

from collections import Counter
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from typing import Literal
from urllib.parse import unquote

import html5lib
from bs4 import BeautifulSoup, Tag


_SVG_NAMESPACE = "http://www.w3.org/2000/svg"
_SVG_BOUNDARIES = (
    f"{{{_SVG_NAMESPACE}}}svg",
    f"{{{_SVG_NAMESPACE}}}foreignObject",
)


class _SVGParser(html5lib.HTMLParser):
    """Retain SVG recovery evidence before HTML5 parsing repairs the tree."""

    def __init__(self):
        super().__init__(tree=html5lib.getTreeBuilder("etree"))
        self.svg_recoveries: list[tuple[int, int]] = []

    def parseError(self, errorcode, datavars=None):
        if (
            errorcode == "unexpected-html-element-in-foreign-content"
            and self.tree.openElements[-1].namespace == _SVG_NAMESPACE
        ):
            self.svg_recoveries.append(self.tokenizer.stream.position())
        super().parseError(errorcode, datavars)


def _text_content(node) -> str:
    """Read text and tails, excluding comments and other graphic subtrees."""
    parts = []
    pending = [node]
    while pending:
        current = pending.pop()
        if isinstance(current, str):
            parts.append(current)
            continue
        parts.append(current.text or "")
        for child in reversed(current):
            pending.append(child.tail or "")
            if isinstance(child.tag, str) and child.tag not in _SVG_BOUNDARIES:
                pending.append(child)
    return "".join(parts)


def _graphic_texts(node):
    """Yield text elements without entering nested SVG or foreignObject."""
    pending = list(reversed(node))
    while pending:
        child = pending.pop()
        if child.tag in _SVG_BOUNDARIES:
            continue
        if child.tag == f"{{{_SVG_NAMESPACE}}}text":
            yield child
        pending.extend(reversed(child))


@dataclass(frozen=True)
class LayoutCheck:
    """One bounded check and the evidence or limitation supporting its status."""

    status: Literal["PASS", "FAIL", "NOT_EVALUATED"]
    details: tuple[str, ...] = ()


def audit_svg_text(
    document: str, *, expected_labels: Mapping[str, Sequence[str]] | None = None
) -> LayoutCheck:
    """Check static SVG text namespaces and optional labels by SVG element ID.

    HTML5 parsing supplies the namespaces used by a browser, including when
    inline SVG omits xmlns. Parser recovery that ejects HTML from SVG fails the
    check, even if the recovered tree has no offending descendants left.
    Expected labels are complete text element contents (entities decoded,
    outer whitespace stripped), with exact multiplicity in the named graphic.
    Nested SVG and foreignObject subtrees are excluded from that graphic's
    labels and text content. Nested SVG IDs can be checked independently;
    unnamed nested SVG never supplies an ancestor's labels. Only IDs and labels
    supplied by the caller impose expected-label requirements.
    Coordinates, CSS, scripts, visibility and calculation accuracy are outside
    this check; no expected values are inferred from the document itself.
    """
    parser = _SVGParser()
    root = parser.parse(document)
    svg_ns = f"{{{_SVG_NAMESPACE}}}"
    failures = [
        f"HTML element displaced from SVG at line {position[0]}, column {position[1]}."
        for position in parser.svg_recoveries
    ]
    texts = list(root.iter(f"{svg_ns}text"))
    for text in texts:
        if any(
            not child.tag.startswith(svg_ns)
            for child in text.iter() if isinstance(child.tag, str)
        ):
            failures.append("SVG text contains an element outside the SVG namespace.")
    for figure_id, labels in (expected_labels or {}).items():
        figures = [
            node
            for node in root.iter(f"{svg_ns}svg")
            if node.get("id") == figure_id
        ]
        # An ID collision with HTML is also ambiguous to report consumers.
        id_count = sum(node.get("id") == figure_id for node in root.iter())
        if len(figures) != 1 or id_count != 1:
            failures.append(f"Expected SVG ID {figure_id!r} is missing or not unique.")
            continue
        actual = Counter(
            _text_content(text).strip()
            for text in _graphic_texts(figures[0])
        )
        for label, count in Counter(labels).items():
            if actual[label] != count:
                failures.append(
                    f"SVG {figure_id!r} label {label!r}: expected {count}, found {actual[label]}."
                )
    if failures:
        return LayoutCheck("FAIL", tuple(failures))
    if not texts:
        return LayoutCheck("NOT_EVALUATED", ("No static SVG text found.",))
    return LayoutCheck("PASS")


def _anchors(soup: BeautifulSoup) -> LayoutCheck:
    ids = [tag["id"] for tag in soup.select("[id]")]
    if not ids:
        return LayoutCheck("NOT_EVALUATED", ("No HTML IDs found.",))
    failures = tuple(
        f"Empty or duplicate ID: {key!r} ({count} occurrences)."
        for key, count in Counter(ids).items()
        if not key or count > 1
    )
    return LayoutCheck("FAIL" if failures else "PASS", failures)


def _contents_order(soup: BeautifulSoup, sections: list[Tag]) -> LayoutCheck:
    links = soup.select("nav.toc a")
    if not links or not sections:
        return LayoutCheck("NOT_EVALUATED", ("Contents or report sections absent.",))
    ids = [section.get("id") for section in sections]
    all_ids = Counter(tag["id"] for tag in soup.select("[id]"))
    positions, failures = [], []
    for link in links:
        href = link.get("href", "")
        target = unquote(href[1:]) if href.startswith("#") else None
        if not target or ids.count(target) != 1 or all_ids[target] != 1:
            failures.append(f"Contents target is not a unique section: {href!r}.")
        else:
            positions.append(ids.index(target))
    if positions != sorted(set(positions)):
        failures.append("Contents targets repeat or differ from document order.")
    return LayoutCheck("FAIL" if failures else "PASS", tuple(failures))


def _subsection_labels(sections: list[Tag]) -> LayoutCheck:
    failures, unresolved = [], []
    found = False
    # Ordinals start at one by the cited house CSS counter-increment contract.
    for section_number, section in enumerate(sections, start=1):
        for index, subsection in enumerate(section.select(".l2"), start=1):
            found = True
            expected = f"{section_number}.{index}"
            header = subsection.find(class_="l2head", recursive=False)
            number = header.select_one(":scope > .n") if header else None
            if number is None:
                failures.append(f"Subsection {expected} has no number element.")
            elif not number.get_text(strip=True):
                unresolved.append(
                    f"Subsection {expected} requires CSS counter rendering."
                )
            elif number.get_text(strip=True) != expected:
                failures.append(
                    f"Subsection {expected} has label {number.get_text()!r}."
                )
    if failures:
        return LayoutCheck("FAIL", tuple(failures + unresolved))
    if unresolved or not found:
        return LayoutCheck(
            "NOT_EVALUATED", tuple(unresolved) or ("No subsections found.",)
        )
    return LayoutCheck("PASS")


def audit_calc_report_html(document: str) -> dict[str, LayoutCheck]:
    """Check IDs, contents order, label text and SVG namespaces in a document.

    PASS applies only to the named structural check. It never certifies visible
    numbering: explicit text can be hidden and empty spans can be numbered by
    CSS. Missing evidence is NOT_EVALUATED. Contents order checks links present,
    not completeness: CalcReport._toc omits the optional revision section.
    No files, external stylesheets, scripts or solver processes are accessed.
    """
    soup = BeautifulSoup(document, "html.parser")
    sections = soup.select("main.main > section")
    return {
        "anchors": _anchors(soup),
        "contents_order": _contents_order(soup, sections),
        "subsection_labels": _subsection_labels(sections),
        "svg_text": audit_svg_text(document),
        "visible_numbering": LayoutCheck(
            "NOT_EVALUATED", ("Browser-rendered visibility has not been checked.",)
        ),
    }
