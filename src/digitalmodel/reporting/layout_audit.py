"""Assess composed CalcReport HTML without changing or executing the report.

The markup contract is CalcReport._section/_toc, MethodBlock.render and the
``numbered L1 / L2 sections`` rules in assets/calc_report_format.html. Section
and subsection numbers are document ordinals, not engineering coefficients.
This audit checks HTML labels only. CSS cascade, generated counter content,
script changes and browser/PDF visibility require a separate rendered check.
"""

from collections import Counter
from dataclasses import dataclass
from typing import Literal
from urllib.parse import unquote

from bs4 import BeautifulSoup, Tag


@dataclass(frozen=True)
class LayoutCheck:
    """One bounded check and the evidence or limitation supporting its status."""

    status: Literal["PASS", "FAIL", "NOT_EVALUATED"]
    details: tuple[str, ...] = ()


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
    """Check IDs, contents order and label text in an already rendered document.

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
        "visible_numbering": LayoutCheck(
            "NOT_EVALUATED", ("Browser-rendered visibility has not been checked.",)
        ),
    }
