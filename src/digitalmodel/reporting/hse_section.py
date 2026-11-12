#!/usr/bin/env python3
"""Standard "HSE and safety" report appendix.

ABOUTME: Every engineering report carries an HSE and safety appendix: the public
incident history of the equipment it analyses, the recorded failure modes and
causes, and the lessons for the design. This module adds that appendix to a
report skeleton as six required slots and fills them from the files written by
the worldenergydata equipment failure table
(``python -m worldenergydata.safety_analysis.equipment section``).

The appendix is generated, not written by hand. This module adds no words of its
own to the incident text: it checks the generated section and places it. A
cause appears only where the source data codes it or the cited investigation
states it; that rule, and the correctness of every count and case statement,
is enforced where the section is generated. The checks here are structural:
:func:`check_sidecar` refuses a section with open items, an unanswered lesson
or a data source without a public link and a real date read, and
:func:`check_consistency` refuses text that does not print each source and
reference listed in the sidecar.
"""

from __future__ import annotations

import datetime as dt
import html
import json
import re
from pathlib import Path
from typing import Dict, List, Optional

from digitalmodel.reporting.skeleton import BlockSpec, ReportSkeleton, SectionSpec

HSE_SECTION_KEY = "hse_safety"
UNASSIGNED = "not yet assigned"  # placeholder the generator writes

# Slot key -> (subsection number, heading). The headings are fixed.
HSE_BLOCKS: Dict[str, tuple] = {
    "hse_data_used": (1, "Data used"),
    "hse_incident_history": (2, "Incident history"),
    "hse_failure_modes": (3, "Failure modes and stated causes"),
    "hse_public_cases": (4, "Illustrative public cases"),
    "hse_lessons": (5, "Lessons for this design"),
    "hse_references": (6, "References for this appendix"),
}


def hse_section_spec(letter: str = "F") -> SectionSpec:
    """The appendix as a skeleton section; every slot is required."""
    return SectionSpec(
        key=HSE_SECTION_KEY,
        label=f"Appendix {letter} HSE and safety",
        blocks=[
            BlockSpec(key=key, label=f"{letter}.{n} {heading}", required=True)
            for key, (n, heading) in HSE_BLOCKS.items()
        ],
    )


def with_hse_section(skeleton: ReportSkeleton, letter: str = "F") -> ReportSkeleton:
    """Return ``skeleton`` with the HSE appendix as its last section."""
    if any(s.key == HSE_SECTION_KEY for s in skeleton.sections):
        return skeleton
    return skeleton.model_copy(
        update={"sections": [*skeleton.sections, hse_section_spec(letter)]}
    )


def check_sidecar(sidecar: dict, *, allow_open: bool = False) -> List[str]:
    """Problems that stop the appendix going into a report (empty when ready)."""
    problems: List[str] = []
    if not sidecar.get("equipment_class"):
        problems.append("no equipment class")
    sources = sidecar.get("sources") or []
    if not sources:
        problems.append("no data source listed")
    for src in sources:
        name = src.get("source_id", "?")
        if not str(src.get("url", "")).startswith("https://"):
            problems.append(f"source {name}: no public link")
        if not _is_date(src.get("retrieved")):
            problems.append(f"source {name}: no date read")
    if not sidecar.get("references"):
        problems.append("no references")
    if not allow_open:
        for lesson in sidecar.get("lessons") or []:
            if not _answered(lesson.get("addressed_in")):
                problems.append(f"lesson {lesson.get('key', '?')}: no design response")
        problems.extend(f"open item: {x}" for x in sidecar.get("open_items") or [])
    return problems


def _is_date(value: object) -> bool:
    """True for a real calendar date written as YYYY-MM-DD."""
    if not isinstance(value, str):
        return False
    try:
        return dt.date.fromisoformat(value).isoformat() == value
    except ValueError:
        return False


def _answered(value: object) -> bool:
    """True when a lesson carries a design response the analyst wrote."""
    return (
        isinstance(value, str)
        and bool(value.strip())
        and value.strip().lower() != UNASSIGNED
    )


def check_consistency(markdown: str, sidecar: dict) -> List[str]:
    """Structural agreement between the appendix text and its sidecar.

    Each data source in the sidecar must appear in the text with its link and
    the date it was read, and each reference must be printed. This does not
    prove any number: the counts and case statements are checked where the
    section is generated.
    """
    problems: List[str] = []
    for src in sidecar.get("sources") or []:
        name = src.get("source_id", "?")
        url, read = str(src.get("url", "")), str(src.get("retrieved", ""))
        if url and f"{url} (read {read})" not in markdown:
            problems.append(
                f"source {name}: its link and date read are not in the text"
            )
    for ref in sidecar.get("references") or []:
        if str(ref) not in markdown:
            problems.append("a reference in the sidecar is not in the text")
    return problems


_BOLD = re.compile(r"\*\*(.+?)\*\*")
# A link is plain https with URL-safe characters only: no quotes, brackets or
# spaces, so nothing in the text can add an attribute to the anchor.
_LINK = re.compile(r"https://[A-Za-z0-9\-._~:/?#@!$&*+,;=%]+")


def _escape(text: str) -> str:
    out = html.escape(text, quote=True)
    return _BOLD.sub(r"<strong>\1</strong>", out)


def _inline(text: str) -> str:
    """Escape ``text``; turn bold marks and plain https links into markup."""
    parts: List[str] = []
    last = 0
    for match in _LINK.finditer(text):
        url = match.group(0).rstrip(".,;:")
        end = match.start() + len(url)
        parts.append(_escape(text[last : match.start()]))
        safe = html.escape(url, quote=True)
        parts.append(f'<a href="{safe}">{safe}</a>')
        last = end
    parts.append(_escape(text[last:]))
    return "".join(parts)


def _cells(line: str) -> List[str]:
    return [c.strip() for c in line.strip().strip("|").split("|")]


def markdown_to_html(text: str) -> str:
    """Convert the small Markdown subset the generator writes.

    Paragraphs, ``-`` bullets, pipe tables and ``**bold**``. All text is
    escaped; nothing is passed through as raw HTML.
    """
    out: List[str] = []
    lines = text.strip().split("\n")
    i = 0
    while i < len(lines):
        line = lines[i].rstrip()
        if not line:
            i += 1
        elif line.startswith("|"):
            rows = []
            while i < len(lines) and lines[i].startswith("|"):
                rows.append(lines[i])
                i += 1
            head, body = rows[0], [r for r in rows[1:] if not set(r) <= set("|-: ")]
            out.append("<table><thead><tr>")
            out += [f"<th>{_inline(c)}</th>" for c in _cells(head)]
            out.append("</tr></thead><tbody>")
            for row in body:
                out.append(
                    "<tr>"
                    + "".join(f"<td>{_inline(c)}</td>" for c in _cells(row))
                    + "</tr>"
                )
            out.append("</tbody></table>")
        elif line.startswith("- "):
            out.append("<ul>")
            while i < len(lines) and lines[i].startswith("- "):
                out.append(f"<li>{_inline(lines[i][2:].strip())}</li>")
                i += 1
            out.append("</ul>")
        else:
            out.append(f"<p>{_inline(line)}</p>")
            i += 1
    return "".join(out)


_HEADING = re.compile(r"^### ([A-Z])\.(\d) (.+)$", re.MULTILINE)


def hse_content(markdown: str) -> Dict[str, str]:
    """Split the generated appendix into the six slots, as HTML.

    Raises ``ValueError`` if a subsection is missing or its heading differs
    from the fixed one.
    """
    marks = list(_HEADING.finditer(markdown))
    found = {
        int(m.group(2)): (m.group(3).strip(), m.end(), i, m.group(1))
        for i, m in enumerate(marks)
    }
    content: Dict[str, str] = {}
    for key, (n, heading) in HSE_BLOCKS.items():
        if n not in found:
            raise ValueError(f"HSE appendix: subsection {n} ({heading}) is missing")
        got, start, idx, letter = found[n]
        if got != heading:
            raise ValueError(
                f"HSE appendix: subsection {n} is {got!r}, not {heading!r}"
            )
        end = marks[idx + 1].start() if idx + 1 < len(marks) else len(markdown)
        body = markdown[start:end].strip()
        if not body:
            raise ValueError(f"HSE appendix: subsection {n} ({heading}) is empty")
        content[key] = f"<h3>{letter}.{n} {html.escape(got)}</h3>" + markdown_to_html(
            body
        )
    return content


def load_hse_appendix(
    markdown_path: str | Path,
    sidecar_path: Optional[str | Path] = None,
    *,
    allow_open: bool = False,
) -> Dict[str, str]:
    """Read the generated appendix and return slot content for a skeleton.

    ``sidecar_path`` defaults to the ``.json`` file beside the Markdown file.
    """
    markdown_path = Path(markdown_path)
    sidecar_path = (
        Path(sidecar_path) if sidecar_path else markdown_path.with_suffix(".json")
    )
    sidecar = json.loads(sidecar_path.read_text(encoding="utf-8"))
    markdown = markdown_path.read_text(encoding="utf-8")
    problems = check_sidecar(sidecar, allow_open=allow_open)
    problems += check_consistency(markdown, sidecar)
    if problems:
        raise ValueError("HSE appendix is not ready: " + "; ".join(problems))
    return hse_content(markdown)


__all__ = [
    "HSE_BLOCKS",
    "HSE_SECTION_KEY",
    "check_consistency",
    "check_sidecar",
    "hse_content",
    "hse_section_spec",
    "load_hse_appendix",
    "markdown_to_html",
    "with_hse_section",
]
